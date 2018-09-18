CAD_OSE <- NULL
.onLoad <- function(libname, pkgname){
  CAD_OSE <<- reticulate::import_from_path("CAD_OSE", 
                                           system.file("python", "CAD", 
                                                       package = utils::packageName(), 
                                                       mustWork = TRUE))
}
