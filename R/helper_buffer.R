Buffer <- R6::R6Class("Buffer", list(

  buffer = NULL,
  len = NULL,

  initialize = function(len) {
    stopifnot(is.numeric(len), len > 1)
    self$len <- len
  },

  add = function(x) {

    self$buffer <- c(self$buffer, x)

    if (length(self$buffer) > self$len)
      self$buffer <- self$buffer[-(1:(length(self$buffer)-self$len))]

    invisible(self)

  },

  get = function(last = NULL) {

    n <- length(self$buffer)
    if (is.null(last)) last <- n

    return(self$buffer[(n - last + 1):n])
  },

  length = function() {
    return(length(self$buffer))
  },

 full = function() {
    return(length(self$buffer) == self$len)
  }

))

# buffer <- Buffer(3)
# buffer$add(1:3)
# buffer$get()
# buffer$add(4:5)
# buffer$get(3)
# buffer$length()
