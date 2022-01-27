OrelmNormalizer <- R6::R6Class("OrelmNormalizer", cloneable = FALSE,

  public = list(

    initialize = function(wl, method, others = NULL) {

      stopifnot(is.numeric(wl), wl > 1)
      stopifnot(is.null(method) |  method %in% c("DN", "WN", "OAN", "OAMN"))

      private$normalized_window <- Buffer$new(wl)
      private$method <- method

      if (!is.null(private$method)) {

        switch(private$method,
          # dinamyc_normalizalizer
          DN = private$normalizer <- DynamicNormalizer$new(),
          # window_normalizer
          WN = {
            obj <- get("WindowNormalizer")
            private$normalizer <- do.call(obj$new, c(list(wl = wl), others))
          },
          # adaptive_normalizer
          OAN = {
            obj <- get("AdaptiveNormalizer")
            private$normalizer <- do.call(obj$new, c(list(wl = wl - 1), others))
          },
          # adaptive_normalizer2
          OAMN = {
            obj <- get("AdaptiveNormalizer2")
            private$normalizer <- do.call(obj$new, c(list(wl = wl), others))
          }
        )
      }
    },

    normalize = function(x) {
      if (!is.null(private$method))
        y <- private$normalizer$normalize(x)
      else
        y <- x
      if (!is.null(nrow(y))) {
        df <- y
      }
      else {
        private$normalized_window$add(y)
        len <- private$normalized_window$length()
        window <- private$normalized_window$get()
        df <- rbind(window[1:(len - 1)], window[2:len])
      }
      return(df)
    }

  ),
  private = list(
    normalized_window = NULL,
    normalizer = NULL,
    method = NULL
  )
)

# normalizer <- OrelmNormalizer$new(wl = 3, method = "DN")
#
# normalizer$normalize(10)
# normalizer$normalize(15)
# normalizer$normalize(20)
# normalizer$normalize(10)
# normalizer$normalize(30)
# normalizer$normalize(15)
