Foselm <- R6::R6Class("Foselm", cloneable = FALSE,

  public = list(

    initialize = function(inputs, outputs, numHiddenNeurons, forgettingFactor = 0.999,
                          orth = FALSE)
    {

      private$inputs <- inputs
      private$outputs <- outputs
      private$numHiddenNeurons <- numHiddenNeurons
      # input to hidden weights
      private$inputWeights <- sapply(1:inputs, function(i) {runif(numHiddenNeurons, 0, 1)})
      private$orth <- orth
      # bias of hidden units
      private$bias <- t(as.matrix(runif(numHiddenNeurons, -1, 1)))
      # hidden to output layer connection
      private$beta <- sapply(1:outputs, function(i) {runif(numHiddenNeurons, 0, 1)})
      # auxiliary matrix used for sequential learning
      private$forgettingFactor <- forgettingFactor
    },

    initializePhase = function(lamb = 0.0001) {

      # randomly initialize the input->hidden connections
      private$inputWeights <- sapply(1:private$inputs, function(i) {runif(private$numHiddenNeurons, -1, 1)})

      if (private$orth) {
        if (private$numHiddenNeurons > private$inputs) {
          private$inputWeights <- private$orthogonalization(private$inputWeights)
        } else {
          private$inputWeights <- private$orthogonalization(t(private$inputWeights))
          private$inputWeights <- t(private$inputWeights)
        }
      }

      private$bias <- t(as.matrix(runif(private$numHiddenNeurons, -1, 1)))
      private$m <- solve(lamb * diag(private$numHiddenNeurons))
      private$beta <- sapply(1:private$outputs, function(i) rep(0, private$numHiddenNeurons))

    },

    train = function(features, targets) {

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
      numOutputs <- ncol(targets)
      if (nrow(features) != numSamples)
        stop("Unknown activation function type.")

      h <- private$calculateHiddenLayerActivation(features)
      ht <- t(h)

      tryCatch({
        aux <- (1 / private$forgettingFactor) * private$m
        m <- aux - (aux %*% (ht %*% (pracma::pinv(diag(numSamples) + (h %*% (aux %*% ht))) %*% (h %*% aux))))
        beta <- private$beta + (m %*% (ht %*% (targets - (h %*% private$beta))))
        private$m <- m
        private$beta <- beta
      },
      error = function(error_message) {
        message("SVD not converge, ignore the current training cycle")
      })


      return(NULL)

    },

    predict = function(features) {

      ## Make prediction with feature matrix
      ## param features: feature matrix with dimension (numSamples, numInputs)
      ## return: predictions with dimension (numSamples, numOutputs)

      h <- private$calculateHiddenLayerActivation(features)
      prediction <- h %*% private$beta
      return(prediction)
    },

    get = function(param) {
      return(private[[param]])
    }

  ),
  private = list(
    inputs = NULL,
    outputs = NULL,
    numHiddenNeurons = NULL,
    inputWeights = NULL, # input to hidden weights
    orth = NULL,
    bias = NULL, # bias of hidden units
    beta = NULL, # hidden to output layer connection
    m = NULL, # auxiliary matrix used for sequential learning
    forgettingFactor = NULL,

    calculateHiddenLayerActivation = function(features) {

      v <- private$linear(features, private$inputWeights, private$bias)
      v <- private$layerNormalization(v)
      h <- private$sigmoidActFunc(v)

      return(h)
    },

    orthogonalization = function(arr) {

      nRow <- nrow(arr)

      res <- svd(arr, nu = nRow, nv = nRow)
      q <- res$u
      s <- res$d
      tol = max(dim(arr)) * pracma::eps(max(s))
      r = sum(s > tol)
      q = q[, 1:r]

      return(q)
    },

    linear = function(features, weights, bias) {

      if (ncol(features) != ncol(weights))
        stop("number of columns of fetaures and weights differ")

      v = features %*% t(weights) + bias

      return(v)
    },

    sigmoidActFunc = function(v) {

      h = 1 / (1 + exp(-v))

      return(h)
    },

    layerNormalization = function(h, scaleFactor = 1, biasFactor = 0) {

      variance <- sum((h - mean(h)) ^ 2) / length(h)
      hNormalized <- (h - mean(h)) / (sqrt(variance + 0.0001))
      hNormalized <- scaleFactor * hNormalized + biasFactor

      return(hNormalized)
    }

  )
)

