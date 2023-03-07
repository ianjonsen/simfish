#' @title Pre-simulation setup
#'
#' @description load required rasters, receiver locations
#'
#'
#' @author Ian Jonsen \email{ian.jonsen@mq.edu.au}
#'
#' @param config - path to config.R script containing file.paths for required & optional data
#' @export
#'
sim_setup <-
  function(config = config) {

    suppressWarnings(source(config, local = TRUE, echo=FALSE))
    if(is.null(prj)) prj <- "+proj=merc +datum=WGS84 +units=km"

    out <- list(
      land = land,
      grad = grad,
      recLocs = recs,
      recPoly = recPoly,
      prj = prj
    )

    return(out)
  }
