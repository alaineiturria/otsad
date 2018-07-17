#' This function reviews the list of previously selected left semi-contexts,
#' creates the list of potentially new contexts resulted from intersection
#' between zero-level contexts, determines the contexts that coincide with
#' the input data and require activation.
#' @param newContextFlag:         flag indicating that a new zero-level
#' context is not recorded at the current
#' step, which means that all contexts
#' already exist and there is no need to
#' create new ones.
#' 
#' @return activeContexts:         list of identifiers of the contexts which
#' completely coincide with the input stream,
#' should be considered active and be 
#' recorded to the input stream of “neurons”
#' @return potentialNewContextList:  list of contexts based on intersection
#' between the left and the right zero-level
#' semi-contexts, which are potentially new
#' contexts requiring saving to the context
#' memory

UpdateContextsAndGetActive.CAD <- function(new.ctxt.flag, context.operator){
  active.ctxt <- list()
  num.selected.context <- 0
  potential.new.context.list <- list()
  crossed.sctxt.list <- get("crossed.sctxt.list", 
                            envir = context.operator)
  new.ctxt.id <- get("new.ctxt.id", envir = context.operator)

  for(left.semicontext.values in crossed.sctxt.list$left){
    for(element in left.semicontext.values[[3]]){
      right.semicontext.value <- element[[1]]
      context.id <- 
      if (new.context.id != context.id) {
        context.values <- context.values.list[context.id]
        results.sc.values <- semi.context.values.list[[1]][right.semicontext.id]

        if (left.semicontext.values[[1]] == left.semicontext.values[[2]]) {
          num.selected.context <- num.selected.context + 1

          if (results.sc.values[3] > 0){
            if (results.sc.values[2] == results.sc.values[3]){
              context.values[1] <- context.values[1] + 1
              active.ctxt <- append(active.ctxt,
                                       list(context.id,
                                            context.values[1],
                                            context.values[3],
                                            context.values[4]))
            }
            else if (context.values[2] && new.ctxt.flag && 
                     left.semicontext.values[3] <= max.left.semicontexts) {
              potential.new.context.list <- append(potential.new.context.list,
                                                   list(list(left.semicontext.values[1]), 
                                                        list(results.sc.values[1])))
            }
          }
        }
        else if (context.values[2] &&
                 new.context.flag &&
                 results.sc.values[3] > 0 &&
                 left.semicontext.values[3] <= max.left.semicontexts.length){
          potential.new.context.list <- append(potential.new.context.list,
                                               left.semicontext.values,
                                               results.sc.values[1])
        }
      }
    }
  }

  new.context.id <- FALSE
  return(list(active.ctxt,
              num.selected.context,
              potential.new.context.list))
  
}


