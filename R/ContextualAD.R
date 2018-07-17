#' Contextual Anomaly Detector - Open Source (CAD)
#'
#' \code{ContextualAnomalyDetector} calculates the anomaly score of a
#' dataset using the notion of contexts conformed by facts and provides
#' probabilistic abnormality scores.
#'
#' @param data Numerical vector with training and test dataset.
#' @param rest.period Training period after an anomaly.
#' @param max.left.semicontexts Number of semicontexts that should be maintained in memory.
#' @param max.active.neurons Number of neurons of the model.
#' @param num.norm.value.bits Granularity of the transformation into discrete values
#' @param base.threshold Threshold to be considered an anomaly.
#'
#' @details \code{data} must be a numerical vector without NA values.
#' \code{threshold} must be a numeric value between 0 and 1. If the anomaly
#' score obtained for an observation is greater than the \code{threshold}, the
#' observation will be considered abnormal.
#'
#' @return Anomaly score of \code{data}.
#'
#' @references Smirnov, M. (2018). CAD: Contextual Anomaly
#'     Detector. https://github.com/smirmik/CAD
#'
#' @export
ContextualAnomalyDetector <- function(data,
                                     rest.period = max(min(150, round(nrow(data) * 0.03), 1)),
                                     max.left.semicontexts = 7,
                                     max.active.neurons = 15,
                                     num.norm.value.bits = 3,
                                     base.threshold = 0.75){
    
                                        # Operational parameters
    min.value <- min(data, na.rm = T)
    max.value <- max(data, na.rm = T)
    
    max.bin.value <- 2^num.norm.value.bits - 1
    full.value.range <- ifelse(max.value == min.value,
                              max.bin.value,
                              max.value - min.value)
    
    min.value.step <- full.value.range / max.bin.value
    
                                        # Operational memory
    left.fact.group <-  list()
    potential.new.context <- list()
    last.predicted.facts <- list()
    result.values.history <- 1.0
    n <- nrow(data)
    
    
                                        # Creation of Context operator
    context.operator <-  new.env()
    assign("max.left.semicontexts", max.left.semicontexts, envir = context.operator)
    assign("fact.dict", list(left = new.env(), right = new.env()), 
           envir = context.operator)
    assign("sctxt.dict", list(left = new.env(), right = new.env()), 
           envir = context.operator)
    assign("sctxt.value.list", list(left = list(), right = list()), 
           envir = context.operator)
    assign("crossed.sctxt.list", list(left = list(), right = list()), 
           envir = context.operator)
    assign("ctxt.value.list", list, envir = context.operator)
    assign("new.ctxt.id", FALSE, envir = context.operator)
    
    
                                        # Adjust data globally
    adjusted.data <- sapply(data, function(x){
        round((x - min.value) / min.value.step)
    })
    bits <- lapply(adjusted.data, IntToBinarySens, num.norm.value.bits)
    
                                        # Create environment for iterated anomaly score computation
    local.environment <- new.env()
    assign("value", bits[[1]], envir = local.environment)
    assign("i", 1, envir = local.environment)
    assign("rest.period", rest.period, envir = local.environment)
    assign("result.values.history", result.values.history, 
           envir = local.environment)
    assign("max.active.neurons.num", max.active.neurons, 
           envir = local.environment)
    assign("context.operator", context.operator, 
           envir = local.environment)
    assign("potential.new.ctxt", list(), 
           envir = local.environment)
    assign("left.facts", list(), 
           envir = local.environment)
    assign("base.threshold", base.threshold,
           envir = local.environment)
    
    results <- lapply(bits, GetAnomalyScore.CAD,
                     local.environment = local.environment)
    return(unlist(results))
}
