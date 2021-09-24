#' Anomaly Likelihood Scorer
#'
#' @description
#' R6 class that implements the anomaly likelihood introduced by Ahmad et al. The original
#' source code is available at
#' https://github.com/numenta/NAB/blob/master/nab/detectors/numenta/numenta_detector.py. This class
#' analyzes and estimates the distribution of averaged anomaly scores from a given model. Given a
#' new anomaly score \code{s}, estimates \code{P(score >= s)}. The number \code{P(score >= s)}
#' represents the likelihood of the current state of predictability. For example, a likelihood of
#' 0.01 or 1% means we see this much predictability about one out of every 100 records. The number
#' is not as unusual as it seems. For records that arrive every minute, this means once every hour
#' and 40 minutes. A likelihood of 0.0001 or 0.01% means we see it once out of 10,000 records, or
#' about once every 7 days.
#'
#' @references
#' S. Ahmad, A. Lavin, S. Purdy, Z. Agha, Unsupervised real-time anomaly detection for streaming
#' data, Neurocomputing 262 (2017) 134-147.
#'
#' @examples
#' a <- rnorm(500)
#'
#' scorer <- AnomalyLikelihoodScorer$new(
#'    learningPeriod = 10,
#'    estimationSamples = 10,
#'    historicWindowSize = 20,
#'    reestimationPeriod = 10
#' )
#'
#' sapply(a, function(x) {scorer$computeScore(x, x)})
#'
#' @export

AnomalyLikelihoodScorer <- R6::R6Class("AnomalyLikelihoodScorer", cloneable = FALSE,

  public = list(

    #' @description Create a new AnomalyLikelihoodScorer object.
    #' @param learningPeriod Number of iterations required for the algorithm to learn the basic
    #' patterns in the dataset and for the anomaly score to 'settle down'. The default is based on
    #' empirical observations but in reality this could be larger for more complex domains. The
    #' downside if this is too large is that real anomalies might get ignored and not flagged.
    #' @param estimationSamples Number of reasonable anomaly scores required for the initial
    #' estimate of the Gaussian. The default of 100 records is reasonable - we just need sufficient
    #' samples to get a decent estimate for the Gaussian. It's unlikely you will need to tune this
    #' since the Gaussian is re-estimated every 10 iterations by default.
    #' @param historicWindowSize Size of sliding window of historical data points to maintain for
    #' periodic reestimation of the Gaussian. Note: the default of 8640 is based on a month's worth
    #' of history at 5-minute intervals.
    #' @param reestimationPeriod How often we re-estimate the Gaussian distribution. The ideal is to
    #' re-estimate every iteration but this is a performance hit. In general the system is not very
    #' sensitive to this number as long as it is small relative to the total number of records
    #' processed.
    #' @param computeLogLikelihood If \code{TRUE} compute a log scale representation of the
    #' likelihood value. Since thelikelihood computations return low probabilities that often go
    #' into four 9's or five 9's, a log value is more useful for visualization, thresholding, etc.
    initialize = function(learningPeriod = 288, estimationSamples = 100, historicWindowSize = 8640,
                          reestimationPeriod = 100, computeLogLikelihood = F)
    {
      if (!is.numeric(learningPeriod) | learningPeriod < 1) {
        stop("learningPeriod must be an integer greater than 0.")
      }
      if (!is.numeric(estimationSamples) | estimationSamples < 1 | estimationSamples > historicWindowSize) {
        stop("estimationSamples must be an integer greater than 1 and less than or equal to historicWindowSize.")
      }
      if (!is.numeric(historicWindowSize) | historicWindowSize < 1) {
        stop("historicWindowSize must be an integer greater than 1 and greater than or equal to estimationSamples.")
      }
      if (!is.numeric(reestimationPeriod) | reestimationPeriod < 0) {
        stop("reestimationPeriod must be an integer greater than 0.")
      }
      if (!is.logical(computeLogLikelihood)) {
        stop("computeLogLikelihood must be logical.")
      }

      private$learningPeriod <- learningPeriod
      private$estimationSamples <- estimationSamples
      private$histoWindowSize <- historicWindowSize
      private$reestimationPeriod <- reestimationPeriod
      private$nTrain <- learningPeriod + estimationSamples
      private$computeLogLikelihood <- computeLogLikelihood
    },

    #' @description Compute the probability that the current value plus anomaly score represents
    #' an anomaly given the historical distribution of anomaly scores. The closer the number is
    #' to 1, the higher the chance it is an anomaly.
    #' @param x The current raw anomaly score.
    #' @param value The current ("raw") input value.
    #' @return The anomalyLikelihood for this record.
    computeScore = function(x, value) {

      private$i <- private$i + 1
      dataPoint <- data.frame(i = private$i, value = value, anomalyScore = x)

      # Ignore the first nTrain values
      if (private$i <= private$nTrain) {
        likelihood <- 0.5
      } else {
        if (is.null(private$distribution) | ((private$i - 1) %% private$reestimationPeriod == 0)) {

          numSkipRecords <- private$calcSkipRecords(
            numIngested = (private$i - 1),
            windowSize = private$histoWindowSize,
            learningPeriod = private$learningPeriod
          )

          private$estimateAnomalyLikelihoods(
            private$historicalScores,
            skipRecords = numSkipRecords
          )

        }

        likelihoods <- private$updateAnomalyLikelihoods(dataPoint)$likelihoods
        likelihood <- 1 - likelihoods[1]

      }

      private$historicalScores <- rbind(private$historicalScores, dataPoint)
      if (nrow(private$historicalScores) > private$histoWindowSize)
        private$historicalScores <- private$historicalScores[-1,]

      if(private$computeLogLikelihood) {
        likelihood <- log(1.0000000001 - likelihood) / -23.02585084720009
      }

      return(likelihood)
    }

  ),
  private = list(
    i = 0,
    historicalScores = NULL,
    distribution = NULL,
    learningPeriod = NULL,
    estimationSamples = NULL,
    histoWindowSize = NULL,
    reestimationPeriod = NULL,
    nTrain = NULL,
    historicalLikelihoods = NULL,
    computeLogLikelihood = NULL,

    calcSkipRecords = function(numIngested, windowSize, learningPeriod) {
      numShiftedOut <- max(0, numIngested - windowSize)
      return(min(numIngested, max(0, learningPeriod - numShiftedOut)))
    },

    anomalyScoreMovingAverage = function(anomalyScores, windowSize = 10) {

      slidingWindow <- NULL
      total <- 0.0
      averagedRecordList <- data.frame()

      # Aggregated records
      sapply(1:nrow(anomalyScores), function(i) {

        newVal <- anomalyScores[i, 3]

        if (length(slidingWindow) == windowSize) {
          total <<- total - slidingWindow[1]
          slidingWindow <<- slidingWindow[-1]
        }

        slidingWindow <<- c(slidingWindow, newVal)
        total <<- total + newVal
        avg <- total / length(slidingWindow)

        averagedRecordList <<- rbind(averagedRecordList,
                                     c(i = anomalyScores[i, 1], value = anomalyScores[i, 2],
                                       anomalyScore = avg))
      })

      return(list(
        averagedRecordList = averagedRecordList,
        historicalValues = slidingWindow,
        total = total
      ))

    },

    estimateNormal = function(sampleData, performLowerBoundCheck = T) {

      params = list(
        name = "normal",
        mean = mean(sampleData),
        variance = sum((sampleData - mean(sampleData)) ^ 2) / length(sampleData),
        stdev = 0
      )

      if (performLowerBoundCheck) {

        if (params$mean < 0.03)
          params$mean <- 0.03

        if (params$variance < 0.0003)
          params$variance <- 0.0003
      }

      # Compute standard deviation
      if (params$variance > 0)
        params$stdev <- sqrt(params$variance)

      return(params)
    },

    nullDistribution = function() {
      return(list(
        name = "normal",
        mean = 0.5,
        variance = 1e6,
        stdev = 1e3
      ))
    },

    tailProbability = function(x, distributionParams) {

      if (x < distributionParams$mean) {
        xp <- 2 * distributionParams$mean - x
        return(private$tailProbability(xp, distributionParams))
      }

      z <- (x - distributionParams$mean) / distributionParams$stdev
      return(0.5 * pracma::erfc(z/1.4142))
    },

    filterLikelihoods = function(likelihoods, redThreshold = 0.99999, yellowThreshold = 0.999) {

      redThreshold <- 1 - redThreshold
      yellowThreshold <- 1 - yellowThreshold

      # The first value is untouched
      filteredLikelihoods <- likelihoods[1]
      sapply(2:length(likelihoods), function(i) {

        if (likelihoods[i] <= redThreshold) {

          # Value is in the redzone
          if (likelihoods[i] > redThreshold) {
            # Previous value is not in redzone, so leave as-is
            filteredLikelihoods <<- c(filteredLikelihoods, likelihoods[i])
          } else {
            filteredLikelihoods <<- c(filteredLikelihoods, yellowThreshold)
          }

        } else {
          # Value is below the redzone, so leave as-is
          filteredLikelihoods <<- c(filteredLikelihoods, likelihoods[i])
        }
      })

      return(filteredLikelihoods)

    },

    estimateAnomalyLikelihoods = function(anomalyScores, averagingWindow = 10, skipRecords=0) {

      if (length(anomalyScores) == 0)
        stop("Must have at least one anomalyScore")

      # Compute averaged anomaly scores
      mvRes <- private$anomalyScoreMovingAverage(anomalyScores, windowSize = averagingWindow)
      dataValues <- mvRes$averagedRecordList[,3]

      # Estimate the distribution of anomaly scores based on aggregated records
      if (nrow(mvRes$averagedRecordList) <= skipRecords)
        distributionParams <- private$nullDistribution()
      else
        distributionParams <- private$estimateNormal(
          dataValues[(skipRecords + 1):length(dataValues)]
        )

      metricValues <- mvRes$averagedRecordList[,2]
      metricDistribution <- private$estimateNormal(
        metricValues[(skipRecords + 1):length(metricValues)],
        performLowerBoundCheck = F
      )

      if (metricDistribution$variance < 1.5e-5)
        distributionParams <- private$nullDistribution()

      # Estimate likelihoods based on this distribution
      likelihoods <- dataValues
      likelihoods <- sapply(1:length(dataValues), function(i) {
        private$tailProbability(dataValues[i], distributionParams)
      })

      # Filter likelihood values
      filteredLikelihoods <- private$filterLikelihoods(likelihoods)

      nLikelihoods <- length(likelihoods)
      private$distribution <- list(
        distribution = distributionParams,
        movingAverage = list(
          historicalValues = mvRes$historicalValues,
          total = mvRes$total,
          windowSize = averagingWindow
        ),
        historicalLikelihoods = likelihoods[
          (nLikelihoods-min(averagingWindow, nLikelihoods) + 1):nLikelihoods
        ]
      )

      return(list(
        likelihoods = filteredLikelihoods,
        aggRecordList = mvRes$averagedRecordList
      ))

    },

    updateAnomalyLikelihoods = function(anomalyScores) {

      if (length(anomalyScores) == 0)
        stop("Must have at least one anomalyScore")

      if (is.null(private$historicalLikelihoods))
        private$historicalLikelihoods <- 1.0

      # Compute moving averages of these new scores using the previous values
      # as well as likelihood for these scores using the old estimator
      slidingWindow <- private$distribution$movingAverage$historicalValues
      total <- private$distribution$movingAverage$total
      windowSize <- private$distribution$movingAverage$windowSize

      aggRecordList <- rep(0, nrow(anomalyScores))
      likelihoods <- rep(0, nrow(anomalyScores))
      sapply(1:nrow(anomalyScores), function(i) {

        newVal <- anomalyScores[i, 3]

        if (length(slidingWindow) == windowSize) {
          total <<- total - slidingWindow[1]
          slidingWindow <<- slidingWindow[-1]
        }

        slidingWindow <<- c(slidingWindow, newVal)
        total <<- total + newVal
        avg <- total / length(slidingWindow)

        aggRecordList[i] <<- avg
        likelihoods[i] <<- private$tailProbability(avg, private$distribution$distribution)

      })

      # Filter the likelihood values. First we prepend the historical likelihoods
      # to the current set. Then we filter the values.  We peel off the likelihoods
      # to return and the last windowSize values to store for later.
      likelihoods2 <- c(private$distribution$historicalLikelihoods, likelihoods)
      filteredLikelihoods <- private$filterLikelihoods(likelihoods2)
      likelihoods <- filteredLikelihoods[
        (length(filteredLikelihoods) - length(likelihoods) + 1):length(filteredLikelihoods)
      ]
      historicalLikelihoods <- likelihoods2[
        (length(likelihoods2) - min(windowSize, length(likelihoods2)) + 1):length(likelihoods2)
      ]

      # Update the estimator
      private$distribution <- list(
        distribution = private$distribution$distribution,
        movingAverage = list(
          historicalValues = slidingWindow,
          total = total,
          windowSize = windowSize
        ),
        historicalLikelihoods = historicalLikelihoods
      )

      return(list(
        likelihoods = unlist(likelihoods),
        aggRecordList = aggRecordList
      ))
    }

  )
)

