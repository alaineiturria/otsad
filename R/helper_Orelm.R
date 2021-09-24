Orelm <- R6::R6Class("Orelm", cloneable = FALSE,

  public = list(

    initialize = function(inputs, outputs, numHiddenNeurons, orth = TRUE,
                          inputWeightForgettingFactor = 0.999, outputWeightForgettingFactor = 0.999)
    {

      stopifnot(is.numeric(inputs), inputs >= 1)
      stopifnot(is.numeric(outputs), outputs >= 1)
      stopifnot(is.numeric(numHiddenNeurons), numHiddenNeurons >= 1)
      stopifnot(is.logical(orth))
      stopifnot(is.numeric(inputWeightForgettingFactor), inputWeightForgettingFactor <= 1,
                inputWeightForgettingFactor > 0)
      stopifnot(is.numeric(outputWeightForgettingFactor), outputWeightForgettingFactor <= 1,
                outputWeightForgettingFactor > 0)

      private$inputs <- inputs
      private$outputs <- outputs
      private$numHiddenNeurons <- numHiddenNeurons

      # input to hidden weights
      private$inputWeights  <- sapply(1:inputs, function(i) {runif(numHiddenNeurons, 0, 1)})

      # hidden layer to hidden layer wieghts
      private$hiddenWeights <- sapply(1:numHiddenNeurons, function(i) {
        runif(numHiddenNeurons, 0, 1)
      })

      # initial hidden layer activation
      private$initialH <- t(as.matrix(runif(numHiddenNeurons, -1, 1)))
      private$h <- private$initialH
      private$orth <- orth

      # bias of hidden units
      private$bias <- t(as.matrix(runif(numHiddenNeurons, -1, 1)))

      # hidden to output layer connection
      private$beta <- sapply(1:outputs, function(i) {runif(numHiddenNeurons, 0, 1)})

      # auxiliary matrix used for sequential learning
      private$m <- pracma::inv(0.00001 * diag(numHiddenNeurons))
      private$forgettingFactor <- outputWeightForgettingFactor
      private$inputForgettingFactor <- inputWeightForgettingFactor

      private$inputAE <- Foselm$new(
        inputs = inputs,
        outputs = inputs,
        numHiddenNeurons = numHiddenNeurons,
        forgettingFactor = inputWeightForgettingFactor,
        orth = orth
      )

      private$hiddenAE <- Foselm$new(
        inputs = numHiddenNeurons,
        outputs = numHiddenNeurons,
        numHiddenNeurons = numHiddenNeurons,
        orth = orth
      )

    },

    initializePhase = function(lamb = 0.0001) {

      private$bias <- t(as.matrix(runif(private$numHiddenNeurons, -1, 1)))
      private$m <- pracma::inv(lamb * diag(private$numHiddenNeurons))
      private$beta <- sapply(1:private$outputs, function(i) rep(0, private$numHiddenNeurons))

      # randomly initialize the input->hidden connections
      private$inputWeights <- sapply(1:private$inputs, function(i) {runif(private$numHiddenNeurons, -1, 1)})

      private$inputAE$initializePhase(lamb = 0.00001)
      private$hiddenAE$initializePhase(lamb = 0.00001)

      invisible(self)
    },

    train = function(features, targets, resetting = FALSE) {

      ## Step 2: Sequential learning phase
      ## param features: feature matrix with dimension (numSamples, numInputs)
      ## param targets: target matrix with dimension (numSamples, numOutputs)

      if (!is.matrix(targets)) {
        targets <- as.matrix(targets)
      }

      if (!is.matrix(features)) {
        features <- matrix(features, nrow = 1, ncol = length(features))
      }

      numSamples <- nrow(targets)
      if (nrow(features) != numSamples) {
        stop("Number of columns of fetaures and weights differ.")
      }

      h <- private$calculateHiddenLayerActivation(features)
      ht <- t(h)

      tryCatch({
        scale <- 1 / (private$forgettingFactor)
        aux <- scale * private$m
        m <- aux - (aux %*% (ht %*% ((pracma::pinv(diag(numSamples) + (h %*% (aux %*% ht)))) %*%
                                       (h %*% aux))))
        aux <- private$forgettingFactor * private$beta
        beta <- aux + (m %*% (ht %*% (targets - (h %*% aux))))
        private$m <- m
        private$beta <- beta
      },
      error = function(error_message) {
        message("SVD not converge, ignore the current training cycle")
      })

      invisible(self)

    },

    predict = function(features) {

      ## Make prediction with feature matrix
      ## param features: feature matrix with dimension (numSamples, numInputs)
      ## return: predictions with dimension (numSamples, numOutputs)

      if (!is.matrix(features)) {
        features <- matrix(features, nrow = 1, ncol = length(features))
      }
      h <- private$calculateHiddenLayerActivation(features)
      prediction <- h %*% private$beta

      return(prediction)
    }

  ),
  private = list(
    inputs = NULL,
    outputs = NULL,
    numHiddenNeurons = NULL,
    inputWeights  = NULL,
    hiddenWeights = NULL,
    initialH = NULL,
    h = NULL,
    orth = NULL,
    bias = NULL,
    beta = NULL,
    m = NULL,
    forgettingFactor = NULL,
    inputForgettingFactor = NULL,
    inputAE = NULL,
    hiddenAE = NULL,

    calculateInputWeightsUsingAE = function(features) {
      private$inputAE$train(features = features, targets = features)
      return(private$inputAE$get("beta"))
    },

    calculateHiddenWeightsUsingAE = function(features) {
      private$hiddenAE$train(features = features, targets = features)
      return(private$hiddenAE$get("beta"))
    },

    calculateHiddenLayerActivation = function(features) {

      ## Calculate activation level of the hidden layer
      ## param features: feature matrix with dimension (numSamples, numInputs)
      ## return: activation level (numSamples, numHiddenNeurons)

      private$inputWeights <- private$calculateInputWeightsUsingAE(features)
      private$hiddenWeights <- private$calculateHiddenWeightsUsingAE(private$h)

      v <- private$linearRecurrent(
        features = features,
        inputW = private$inputWeights,
        hiddenW = private$hiddenWeights,
        hiddenA = private$h,
        bias = private$bias
      )

      v <- private$layerNormalization(v)
      private$h <- private$sigmoidActFunc(v)

      return(private$h)
    },

    linearRecurrent = function(features, inputW, hiddenW, hiddenA, bias) {
      return((features %*% t(inputW)) + (hiddenA %*% hiddenW) + bias)
    },

    sigmoidActFunc = function(v) {
      return(1 / (1 + exp(-v)))
    },

    layerNormalization = function(h, scaleFactor = 1, biasFactor = 0) {

      variance <- sum((h - mean(h)) ^ 2) / length(h)
      hNormalized <- (h - mean(h)) / (sqrt(variance + 0.0001))
      hNormalized <- scaleFactor * hNormalized + biasFactor

      return(hNormalized)
    }

  )
)

