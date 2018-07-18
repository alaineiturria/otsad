#' Context Crosser for Contextual Anomaly Detector (ContextualAD)
#'
#' \code{ContextCrosser.CAD} is an auxiliar method that makes the necessary
#' changes in the \code{context.operator} environment, including in
#' the \code{fact.list} and the corresponding dictionary of facts the
#' new facts.
#'
#' @param left.or.right Situation from which the method have been
#'     called. It says if the new facts have been seen or if they are
#'     completely new.
#' @param fact.list New fact list.
#' @param new.ctxt.flag Flag that indicates if a new context is required.
#' @param potential.new.contexts Potential new contexts that might be added.
#' @param context.operator Environment with the current status for the
#'     context operator.
#'
#' @details \code{left.or.right} must be 0 or 1 to indicate left or
#'     right respectively.
#'
#' @return If \code{left.or.right}, number of new contex added tho the
#'     \code{context.operator}. Otherwise, the output from
#'     \code{UpdateContextsAndGetActive.CAD}.
#' @references Smirnov, M. (2018). CAD: Contextual Anomaly
#'     Detector. https://github.com/smirmik/CAD
ContextCrosser.CAD <- function(left.or.right, 
                              fact.list,
                              new.ctxt.flag = FALSE,
                              potential.new.contexts = list(),
                              context.operator){
                                        # Get current number of new contexts
    if (left.or.right == 0){
        num.new.contexts <- ifelse(length(potential.new.contexts) > 0,
                                  GetContextByFacts.CAD(potential.new.contexts, 
                                                        context.operator),
                                  0)
    }
    
    if (left.or.right == 0) {
        selected.crossed.sctxt <- context.operator$crossed.sctxt.list$left
    } else {
        selected.crossed.sctxt <- context.operator$crossed.sctxt.list$right
    }
    
    for(semicontext.values in selected.crossed.sctxt){
        semicontext.values[[1]] <- list()
        semicontext.values[[3]] <- 0
    }
    
    if (left.or.right == 0) {
        selected.dict <- context.operator$fact.dict$left
    } else {
        selected.dict <- context.operator$fact.dict$right
    }
    
    for(fact in fact.list){
        if (fact %in% names(selected.dict)) {
            selected.dict.fact <- get(fact, envir = selected.dict)
        } else {
            selected.dict.fact <- list()
        }
        
        for(semicontext.values in selected.dict.fact){
            semicontext.values[[1]] <- append(semicontext.values[[1]], fact)
        }
    }
    
    new.crossed.values <- list()
    
    for(semicontext.values in context.operator$sctxt.value.list[[left.or.right+1]]){
        len.semicontext <- length(semicontext.values[[1]])
        semicontext.values[[3]] <- len.semicontext
        if (len.semicontext > 0){
            new.crossed.values <- append(new.crossed.values, semicontext.values)
        }
    }
    
    if (left.or.right) {
        return(UpdateContextsAndGetActive.CAD(new.ctxt.flag, context.operator))
    } else {
        return(num.new.contexts)
    }
}
