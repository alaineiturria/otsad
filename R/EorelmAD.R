#' Ensemble based online recurrent extream learning machine anomaly detector (EORELM-AD)
#'
#' @description
#' R6 class that implements our ensemble-based online recurrent extreme learning machine anomaly
#' detector (EORELM-AD). EORELM-AD is an online univariate time series anomaly detector based on an
#' online recurrent extreme learning machine (OR-ELM) prediction model proposed by J. M. Park and
#' J. H. Kim (2017) <doi:10.1109/IJCNN.2017.7966094>. ORELM is an online learning algorithm that
#' trains single-hidden layer feedforward neural networks (SLFNs), able to learn data one-by-one
#' or chunk-by-chunk with fixed or varying data size.
#'
#' @details
#' EORELM-AD implements several online normalization techniques needed to train ORLEM. Then, based
#' on a window of normalized previous values it predicts the value of the current data point using
#' an ensemble of OR-ELMs initialized with different parameters. Finally, the anomaly score and
#' the anomaly label are computed based on past prediction errors. For this aim, several scoring
#' methods are available.
#'
#' @references
#' J. M. Park and J. H. Kim, Online recurrent extreme learning machine and its application to
#' time-series prediction, in Proceedings of the International Joint Conference on Neural Networks,
#' 2017, pp. 1983–1990.
#'
#' @examples
#'
#' ## Generate data
#' set.seed(100)
#' n <- 350
#' x <- sample(1:100, n, replace = TRUE)
#' x[70:90] <- sample(110:115, 21, replace = TRUE)
#' x[25] <- 200
#' x[320] <- 170
#' df <- data.frame(timestamp = 1:n, value = x)
#'
#'
#' ## Calculate anomalies
#' detector <- EorelmAD$new(
#'   n.train = 20,
#'   numLags = 10,
#'   numHiddenNeurons = c(20),
#'   normMethod = NULL,
#'   outlierMethod = "DSS",
#'   threshold = 0.5
#' )
#' result <- detector$predict(df$value)
#'
#' ## Plot results
#' res <- cbind(df, result)
#' PlotDetections(res, title = "EORELM-AD ANOMALY DETECTOR", return.ggplot = TRUE)
#'
#' @export

EorelmAD <- R6::R6Class("EorelmAD", cloneable = FALSE,

  public = list(

  #' @description Create a new EorelmAD object.
  #' @param n.train Number of points of the dataset that correspond to the training set.
  #' \code{n.train} must be greater or equal to \code{numLags} + 2.
  #' @param numLags Number of past values needed to make the prediction. \code{numLags} must be
  #' lower or equal to \code{n.train} - 2.
  #' @param ncomb Number of base learners to initialize during the training stage.
  #' @param k Numer of best base learners to be used during the test stage.
  #' @param numHiddenNeurons Number or vector of possible values of hidden neurons.
  #' @param outputWeightForgettingFactor Value or vector of possible values of the output
  #' weight forgetting factor. All values must be between 0 and 1. If it is 1, it does not
  #' forget anything.
  #' @param inputWeightForgettingFactor Value or vector of possible values of the input
  #' weight forgetting factor. All values must be between 0 and 1. If it is 1, it does not
  #' forget anything.
  #' @param normMethod The normalization method to be used. Possible values are: NULL, "WN",
  #' "DN", "OAN", and "OAMN". Note that if the value of \code{normMethod} is NULL, the input data
  #'  must be previously normalized or standardized.
  #' @param normParams A list with the additional parameters to configure selected
  #' \code{normMethod}; by default NULL. Please for more details see,
  #' \code{\link{WindowNormalizer}} (WN), \code{\link{DynamicNormalizer}} (DN),
  #' \code{\link{AdaptiveNormalizer}} (OAN), \code{\link{AdaptiveNormalizer2}} (OAMN).
  #' @param outlierMethod The outlier method to be used to compute the anomaly score
  #' based on historical prediction errors. Possible values are: "AL", "DT", "SS" and "DSS".
  #' @param outlierParams A list with the additional parameters to configure selected
  #' \code{outlierMethod}; by default empty list. Please for more details see,
  #' \code{\link{AnomalyLikelihoodScorer}}(AL), \code{\link{DynamicThresholdScorer}} (DT),
  #' \code{\link{SigmaScorer}} (SS) and \code{\link{DynamicSigmaScorer}} (DSS).
  #' @param threshold Anomaly threshold. A integer value between [0,1].
  #' @param reduceFP If TRUE reduces false positives.
  #' @return A new EorelmAD object.
  initialize = function(n.train, numLags = 100, ncomb = 30, k = 6,
                       numHiddenNeurons = c(20, 25, 30, 35, 40, 45, 50),
                       outputWeightForgettingFactor = seq(0.9, 1, 0.01),
                       inputWeightForgettingFactor = 1, normMethod = "DN", normParams = NULL,
                       outlierMethod = "DS", outlierParams = list(), threshold = 0.5,
                       reduceFP = FALSE)
  {

    # validate parameters
    if (!is.numeric(n.train) | n.train <= 0 | numLags + 2 > n.train) {
      stop("n.train argument must be a positive numeric value and it
           must be greater than or equal to numLags + 2.")
    }
    if (!is.numeric(numLags) | numLags <= 0) {
      stop("numLags must be a positive value and it must be lower or equal to n.train - 2")
    }
    if (!is.numeric(ncomb) | ncomb <= 0) {
      stop("ncomb must be an integer greater than 0.")
    }
    if (!is.numeric(k) | k <= 0) {
      stop("k must be an integer greater than 0.")
    }
    if (!is.numeric(numHiddenNeurons) | sum(numHiddenNeurons <= 0) > 0) {
      stop("numHiddenNeurons must be a value or vector of possitve integer values major than 0.")
    }
    if (!is.numeric(outputWeightForgettingFactor) | sum(outputWeightForgettingFactor < 0) > 0
        | sum(outputWeightForgettingFactor > 1) > 0) {
      stop("outputWeightForgettingFactor must be a value or vector between 0 and 1.")
    }
    if (!is.numeric(inputWeightForgettingFactor) | sum(inputWeightForgettingFactor < 0) > 0
        | sum(inputWeightForgettingFactor > 1) > 0) {
      stop("inputWeightForgettingFactor must be a value or vector between 0 and 1.")
    }
    if (!is.null(normMethod)){
      if (!(normMethod %in% c("WN", "DN", "OAN", "OAMN"))) {
        stop("normMethod must be one of these values: NULL, 'WN', 'DN', 'OAN', 'OAMN'")
      }
    }
    if (!is.null(normParams)){
      if (!is.list(normParams)) {
        stop("normParams must be a list")
      }
    }
    if (!(outlierMethod %in% c("AL", "DT", "SS", "DSS"))) {
      stop("outlierMethod must be one of these values: 'AL', 'DT', 'SS' and 'DSS'")
    }
    if (!is.list(outlierParams)) {
      stop("outlierParams must be a list")
    }
    if (!is.numeric(threshold) | threshold < 0 |  threshold > 1) {
      stop("threshold argument must be a numeric value in (0,1] range.")
    }
    if (!is.logical(reduceFP)) {
      stop("reduceFP argument must be a logical value.")
    }

    # EORELM variable params
    private$n.train <- n.train
    private$numLags <- numLags

    # Normalization method
    private$normalizer <- OrelmNormalizer$new(private$numLags + 2, normMethod , normParams)

    # Ensemble params
    private$outputWeightForgettingFactor <- outputWeightForgettingFactor
    private$numHiddenNeurons <- numHiddenNeurons
    private$inputWeightForgettingFactor <- inputWeightForgettingFactor
    private$ncomb <- ncomb
    private$k <- k
    private$netList <- private$initializeNetList()

    # Scorer
    # private$scorer <- OrelmScorer$new(
    #   method = outlierMethod,
    #   n.train = n.train,
    #   params = outlierParams
    # )
    private$scorer <- private$initializeScorer(
      method = outlierMethod,
      n.train = n.train,
      params = outlierParams
    )

    private$threshold <- threshold
    private$reduceFP <- reduceFP

  },

  #' @description It determines whether or not the current value is an anomaly and its degree of
  #' abnormality.
  #' @param x New data value to be evaluated.
  #' @return Dataset conformed by the following columns:
  #'
  #' \tabular{ll}{
  #'   \code{predictedInput} \tab Standardized predicted value for the current input \code{x}. \cr
  #'   \code{normalizedInput} \tab Normalized value of the current input \code{x}. \cr
  #'   \code{error} \tab Prediction error.\cr
  #'   \code{is.anomaly} \tab 1 if the value is anomalous, 0 otherwise.\cr
  #'   \code{anomaly.score} \tab Probability of anomaly.\cr
  #' }
  #'
  #'
  #'
  predict = function(x) {
    res <- lapply(1:length(x), function(i) {
     y <- private$test(x[i])
     return(y)
    })
    res <- rlist::list.rbind(res)
    return(res)
  }

  ),
  private = list(

    # Params
    i = 0,
    n.train = NULL,
    numLags = NULL,
    normalizer = NULL,
    ncomb = NULL,
    k = NULL,
    combinations = NULL,
    netList = NULL,
    scorer = NULL,
    threshold = NULL,
    outputWeightForgettingFactor = NULL,
    numHiddenNeurons = NULL,
    inputWeightForgettingFactor = NULL,
    reduceFP = FALSE,
    red.res = NULL,

    # Initialize Random ncomb OR-ELM NNs
    initializeNetList = function() {

      private$combinations <- expand.grid(
        outputWeightForgettingFactor = private$outputWeightForgettingFactor,
        numHiddenNeurons = private$numHiddenNeurons,
        inputWeightForgettingFactor = private$inputWeightForgettingFactor,
        stringsAsFactors = FALSE
      )
      private$ncomb <- min(private$ncomb, nrow(private$combinations))

      combSin <- which(private$combinations$outputWeightForgettingFactor != 1)
      combCon <- which(private$combinations$outputWeightForgettingFactor == 1)
      newcomb1 <- NULL
      newcomb2 <- NULL
      if (length(combSin) != 0)
        newcomb1 <- sample(combSin, min(private$ncomb, length(combSin)))
      if (length(combCon) != 0)
        newcomb2 <- sample(combCon, min(5, length(combCon)))

      newcomb <- c(newcomb1, newcomb2)

      netList <- lapply(1:length(newcomb), function(i) {

        netParams <- as.list(c(inputs = private$numLags, outputs = 1, private$combinations[i,]))
        obj <- get("Orelm")
        net <- do.call(obj$new, netParams)
        net$initializePhase(lamb = 0.0001)

        return(c(list(
          net = net, predictedInput = NULL, cumMape = 0, cumRmse = 0, mape = 0, nrmse = 0,
          error = 0, histPred = Buffer$new(private$n.train),
          histRaw = Buffer$new(private$n.train)), netParams
        ))

      })

      return(netList)
    },

    train = function(traindf, netList, n.train, k) {

      # Train each NN
      toDelete <- NULL

      sapply(1:length(netList), function(item) {

        # Train and Test
        netList[[item]]$net$train(traindf[1, 1:private$numLags], traindf[1, (private$numLags + 1)])
        netList[[item]]$predictedInput <<- netList[[item]]$net$predict(traindf[2, 1:private$numLags])

        # Cumulative error calculation: NRMSE and MAPE
        netList[[item]]$histPred$add(netList[[item]]$predictedInput)
        netList[[item]]$histRaw$add(traindf[2, (private$numLags + 1)])
        histPred <- netList[[item]]$histPred$get()
        histRaw <- netList[[item]]$histRaw$get()
        netList[[item]]$error <<- (netList[[item]]$predictedInput - traindf[2, (private$numLags + 1)]) ^ 2
        netList[[item]]$mape <<- 100 * mean(abs(((histRaw + 0.00001) - histPred) /
                                                  (histRaw + 0.00001)))
        sdRaw <- ifelse(length(histRaw) == 1 | is.na(sd(histRaw)), 0.0003, sd(histRaw))
        netList[[item]]$nrmse <<- 100 * sqrt(mean((histRaw - histPred) ^ 2)) / sdRaw

        # NN Deviation check
        if (netList[[item]]$predictedInput > 10000)
          toDelete <<- c(toDelete, item)
      })
      # Delete deviated NNs
      if(!is.null(toDelete))
        netList <- netList[-toDelete]

      # Select Best k NNs
      if (private$i == n.train) {
        totals <- lapply(1:length(netList), function(item) {
          return(data.frame(total = (netList[[item]]$mape + netList[[item]]$nrmse) / 2,
                            outputWeightForgettingFactor = netList[[item]]$outputWeightForgettingFactor,
                            numHiddenNeurons = netList[[item]]$numHiddenNeurons))
        })
        totals <- rlist::list.rbind(totals)

        con <- which(totals$outputWeightForgettingFactor == 1)
        sin <- which(totals$outputWeightForgettingFactor != 1)
        select <- NULL
        # if (length(con) != 0)
        #   select <- con[order(totals[con, "total"])[1:min(1,k)]]
        # if (k > 1 & length(sin) != 0) {
        #   selSin <- sin[order(totals[sin, "total"])[1:(k-1)]]
        #   select <- c(select, selSin)
        # }
        if (length(sin) != 0)
          select <- sin[order(totals[sin, "total"])[1:(k-1)]]
        if (k > 1 & length(con) != 0) {
          selCon <- con[order(totals[con, "total"])[1]]
          select <- c(select, selCon)
        }

        netList <- netList[select]

      }

      return(netList)

    },

    test = function(x) {

      private$i <- private$i + 1
      predictedInput <- NA
      normalizedInput <- NA
      error <- 0
      anomaly.score <- 0
      is.anomaly <- 0

      if (private$i >= (private$numLags + 2) & private$i <= private$n.train) { # Training Phase

        df <- private$normalizer$normalize(x)
        normalizedInput <- df[2, (private$numLags + 1)]
        private$netList <- private$train(df, private$netList, private$n.train, private$k)

        # Anomaly detection
        error <- mean(sort(unlist(rlist::list.select(private$netList, error)))
                      [1:min(3, length(private$netList))])
        anomaly.score <- private$scorer$computeScore(error, value = x)

      }
      else if (private$i > private$n.train) { # Testing Phase

        df <- private$normalizer$normalize(x)
        private$netList <- private$train(df, private$netList, private$n.train, private$k)

        # Calculate aggregated prediction
        res <- rlist::list.stack(rlist::list.select(private$netList, predictedInput, error))
        best <- res[order(res$error)[1:min(3, nrow(res))],]
        predictedInput <- mean(best$predictedInput)
        normalizedInput <- df[2, (private$numLags + 1)]
        error <- (predictedInput - normalizedInput) ^ 2

        # Add new NN if anyone has diverged
        nNew <- private$k - length(private$netList)
        if(nNew != 0) {
          print(paste0("Deleted: ", nNew))
          newNetL <- private$initializeNetList()
          newNetL <- private$train(df, newNetL, private$i, k = nNew)
          private$netList <- c(private$netList, newNetL)
        }

        # Anomaly Detection
        anomaly.score <- private$scorer$computeScore(error, value = x)
        is.anomaly <- anomaly.score >= private$threshold

      }
      else {
        private$normalizer$normalize(x)
        private$scorer$computeScore(error, value = x)
      }

      if (private$reduceFP) {
        private$red.res <- ReduceAnomalies(is.anomaly, windowLength = round(private$n.train / 2.5),
                                           incremental = TRUE, last.res = private$red.res$last.res)
        is.anomaly <- private$red.res$result
      }



      return(data.frame(predictedInput, normalizedInput, error, anomaly.score, is.anomaly,
                        stringsAsFactors = F))

    },

    initializeScorer = function(method, n.train, params = list()) {

      switch(method,
             "DT" = {
               if(is.null(params$wnMin))
                 params$wnMin = min(n.train, 100)
               obj <- get("DynamicThresholdScorer")
               scorer <- do.call(obj$new, params)
             },
             "AL" = {
               if(is.null(params$estimationSamples))
                 params$estimationSamples <- min(n.train, 100)
               if(is.null(params$historicWindowSize))
                 params$historicWindowSize = 2000
               obj <- get("AnomalyLikelihoodScorer")
               scorer <- do.call(obj$new, params)
             },
             "SS" = {
               if(is.null(params$wnMin))
                 params$wnMin = min(n.train, 100)
               obj <- get("SigmaScorer")
               scorer <- do.call(obj$new, params)
             },
             "DSS" = {
               obj <- get("DynamicSigmaScorer")
               scorer <- do.call(obj$new, params)
             },
             stop("Wrong anomaly scoring method")
      )

      return(scorer)
    }

  )
)


# df <- otsad::TravelTime_387
# values <- (df$value - mean(df$value)) / sd(df$value)
# n.train <- otsad::GetNumTrainingValues(length(values))
#
# RNGkind(sample.kind = "Rounding")
# set.seed(1)
# detector <- otsad::EorelmAD$new(n.train, normMethod = NULL, outlierMethod = "SS", threshold = 0.5)
# res <- detector$predict(values)


