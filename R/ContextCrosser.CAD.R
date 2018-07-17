ContextCrosser.CAD <- function(left.or.right, 
                              fact.list,
                              new.ctxt.flag = FALSE,
                              potential.new.contexts = list(),
                              context.operator){
                                        # Get current number of new contexts
    if (left.or.right == 0){
        num.new.contexts <- ifelse(length(potential.new.contexts) > 0,
                                  getContextByFacts(potential.new.contexts, context.operator),
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
        return(UpdateContextsAndGetActive.CAD(new.context.flag, context.operator))
    } else {
        return(num.new.contexts)
    }
}
