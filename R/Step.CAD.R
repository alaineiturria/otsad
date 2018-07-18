#' Step operator for Contextual Anomaly Detector (ContextualAD)
#'
#' \code{Step.CAD} is an auxiliar method that makes the necessary
#' changes in the local environment of the anomaly detector
#'
#' @param datum New datum in form of numeric vector corresponding to
#'     the bits that have been activated.
#' @param local.environment Local environment with the variables of
#'     the Contextual Anomaly Detector.
#'
#' @details \code{datum} must be a numeric vector.
#'
#' @return If \code{left.or.right}, number of new contex added tho the
#'     \code{context.operator}. Otherwise, the output from
#'     \code{UpdateContextsAndGetActive.CAD}.
#' @references Smirnov, M. (2018). CAD: Contextual Anomaly
#'     Detector. https://github.com/smirmik/CAD
Step.CAD <- function(datum, local.environment){
    if(length(local.environment$left.facts) > 0 && length(datum) > 0) {
        pot.new.zero.level.context <- list(left=list(local.environment$left.facts),
                                          right=list(datum))
        new.context.flag <- GetContextByFacts.CAD(pot.new.zero.level.context,
                                                 zero.level = 1, local.environment$context.operator)
    }
    else {
        pot.new.zero.level.context <- FALSE
        new.context.flag <- FALSE
    }
    
    result.context.crosser <- ContextCrosser.CAD(left.or.right = 1,
                                                fact.list = datum, 
                                                new.ctxt.flag = new.context.flag,
                                                context.operator = local.environment$context.operator)
    active.ctxt <- result.context.crosser[[1]]
    potential.new.ctxt.list <- result.context.crosser[[3]]
    
    num.uniq.pot.new.context <- length(potential.new.ctxt.list) +
        ifelse(is.null(pot.new.zero.level.context), 0, length(pot.new.zero.level.context))
    perc.selected.context.active <- ifelse(result.context.crosser[[2]] > 0,
                                          length(active.ctxt) / result.context.crosser[[2]],
                                          0)
                                        # active.ctxt <- active.ctxt[order(active.ctxt)]
    active.neurons <- active.ctxt #[local.environment$context.operator:1]
    if (length(active.neurons) > 0) {
        local.environment$left.facts <- c(datum, 2^31 + active.neurons)
    }
    else{
        local.environment$left.facts <- datum
    }
    
    num.new.contexts <- ContextCrosser.CAD(left.or.right = 0,
                                          fact.list = local.environment$left.facts,
                                          potential.new.contexts = potential.new.ctxt.list,
                                          context.operator = local.environment$context.operator)
    num.new.contexts <- num.new.contexts + ifelse(new.context.flag, 1, 0)
    
    perc.added.context.to.uniq.pot.new <- ifelse(num.uniq.pot.new.context > 0,
                                                num.new.contexts / num.uniq.pot.new.context,
                                                0)
    
    return(c(perc.selected.context.active,
             perc.added.context.to.uniq.pot.new))
    
}
