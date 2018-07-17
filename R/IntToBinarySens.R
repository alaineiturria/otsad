#' Int To Binary Sens
#'
#' @description \code{IntToBinarySens} Auxiliar function that transforms
#' integer to binary bins. 
#'
#' @param x input data
#' @param num.norm.value.bits Expected number of binary bins
#'
#' @details 
#'
#' @return 
#'
#' @example examples/IntToBinarySens_example.R
IntToBinarySens <- function(x, num.norm.value.bits){
  bin.input.value <- as.numeric(intToBits(x))[1:num.norm.value.bits]
  out.sens <- 0:(num.norm.value.bits - 1) * 2 + bin.input.value
  
  return(out.sens)
}
