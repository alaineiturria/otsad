#' Get Context By Facts for Contextual Anomaly Detector
#'
#' \code{GetContextByFacts.CAD} is an auxiliary method that determine
#' by the complete facts list whether the context is already saved to
#' the memory. If the context is not found the function immediately
#' creates such. To optimize speed and volume of the occupied memory
#' the contexts are divided into semi-contexts as several contexts can
#' contain the same facts set in its left and right parts.
#' 
#' @param context.list List of potentially new contexts
#' @param context.operator Environment with the current status for the
#'     context operator.
#' @param zero.level Flag indicating the context type in transmitted
#'     list
#' 
#' 
#' @return Depending on the type of  potentially new context transmitted as
#' an input parameters the function returns either:
#'   a) flag indicating that the transmitted zero-level context is
#' a new/existing one;
#' or:
#'   b) number of the really new contexts that have been saved to the
#' context memory.
#' @references Smirnov, M. (2018). CAD: Contextual Anomaly
#'     Detector. https://github.com/smirmik/CAD
GetContextByFacts.CAD <- function(context.list, context.operator, zero.level = 0){
    num.added.context <- 0
    for(i in 1:length(context.list$left)){
        
        left.facts <- context.list$left[[i]]
        right.facts  <- context.list$right[[i]]
        
        left.hash <- digest::sha1(left.facts)
        right.hash <- digest::sha1(right.facts)
        
        # Add left hash
        next.left.sctxt.number <- length(context.operator$sctxt.dict$left) + 1
        if(!(left.hash %in% names(context.operator$sctxt.dict$left))) {
            assign(left.hash, next.left.sctxt.number, 
                   envir = context.operator$sctxt.dict$left)
            left.sctxt.id <- next.left.sctxt.number
        } else {
            left.sctxt.id <- get(left.hash, envir = context.operator$sctxt.dict$left)
        }
                                        # Add facts in left
        if (left.sctxt.id == next.left.sctxt.number) {
            left.sctxt.values <- list(list(), length(left.facts), 0, new.env())
            context.operator$sctxt.value.list$left <- append(context.operator$sctxt.value.list$left, 
                                                            list(left.sctxt.values))
            
            for(fact in left.facts){
                fact <- paste("x", fact, sep = "")
                if(!(fact %in% names(context.operator$fact.dict$left))) {
                    assign(fact, list(), envir = context.operator$fact.dict$left)
                    sctxt.list <- list()
                } else{
                    sctxt.list <- get(fact, envir = context.operator$fact.dict$left)
                }
                
                sctxt.list <- append(sctxt.list, left.sctxt.values)
                assign(fact, sctxt.list, envir = context.operator$fact.dict$left)
            }
        }
        
        
                                        # Add right hash
        next.right.sctxt.number <- length(context.operator$sctxt.dict$right)
        if(!(right.hash %in% names(context.operator$sctxt.dict$right))) {
            assign(right.hash, next.right.sctxt.number, envir = context.operator$sctxt.dict$right)
            right.sctxt.id <- paste("x", next.right.sctxt.number, sep = "")
        } else {
            right.sctxt.id <- get(right.hash, envir = context.operator$sctxt.dict$right)
        }
                                        # Add facts in right
        if (right.sctxt.id == next.right.sctxt.number) {
            right.sctxt.values <- list(list(), length(right.facts), 0)
            context.operator$sctxt.value.list$right <- append(context.operator$sctxt.value.list$right, 
                                                             list(right.sctxt.values))
            
            for(fact in right.facts){ 
                fact <- paste("x", fact, sep = "")
                if(!(fact %in% names(context.operator$fact.dict$right))) {
                    assign(fact, list(), envir = context.operator$fact.dict$right)
                    sctxt.list <- list()
                } else{
                    sctxt.list <- get(fact, envir = context.operator$fact.dict$right)
                }
                
                sctxt.list <- append(sctxt.list, left.sctxt.values)
                assign(fact, sctxt.list, envir = context.operator$fact.dict$right)
            }
        }
        
                                        # Count new context
        next.free.context.id.number <- length(context.operator$ctxt.value.list)
        selected.ctxt.dict <- context.operator$sctxt.value.list$left[[left.sctxt.id]][[4]]
        if(right.sctxt.id %in% names(selected.ctxt.dict)) {
            context.id <- get(right.sctxt.id, envir = selected.ctxt.dict)
        } else {
            context.id <- next.free.context.id.number
            key <-  paste("x",right.sctxt.id,sep = "")
            assign(key, context.id, envir = selected.ctxt.dict)
        }
        
        if (context.id == next.free.context.id.number) {
            num.added.context <- num.added.context + 1
            context.values <- list(0, zero.level, left.hash, right.hash)
            
            context.operator$ctxt.value.list <- append(context.operator$ctxt.value.list,
                                                      list(context.values))
            
            if (zero.level){
                new.context.id <- context.id
                return(TRUE)
            }
        } else {
            context.values <- context.operator$ctxt.value.list[[context.id]]
            
            if (zero.level){
                context.values[[1]] <- 1
                return(FALSE)
            }
        }
    }
    
    return(num.added.context)
}
