#' Install python module bencode
#'
#' \code{install_bencode} installs bencode module.
#'
#' @param method Argument for reticulate::py_install. Installation method. By default, it finds a method for the local environment.
#' @param conda Argument for reticulate::py_install. Path to conda executable.
#'
#' @details 
#' On Linux and OS X the "virtualenv" method will be used by default ("conda" will be used if virtualenv isn't available). On Windows, the "conda" method is always used.
#'
#' @export
install_bencode <- function(method = "auto", conda = "auto") {
  reticulate::py_install("bencode", method = method, conda = conda)
}
