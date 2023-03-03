#' @title Pre-simulation setup
#'
#' @description load required rasters, receiver locations
#'
#'
#' @author Ian Jonsen \email{ian.jonsen@mq.edu.au}
#'
#' @param config - path to config.R script containing file.paths for required & optional data
#' @importFrom raster raster stack brick projectRaster extract
#' @importFrom sp coordinates<- proj4string<- CRS spTransform SpatialPointsDataFrame spsample
#' @importFrom sf st_as_sf st_sample st_coordinates st_distance
#' @importFrom dplyr select filter rename bind_cols %>% tibble distinct
#' @export
#'
sim_setup <-
  function(config = config) {

    suppressWarnings(source(config, local = TRUE, echo=FALSE))
    if(is.null(prj)) prj <- "+proj=merc +datum=WGS84 +units=km"

    out <- list(
      land = suppressWarnings(raster(land)),
      grad = suppressWarnings(stack(grad))
    )

    out[["recLocs"]] <- recs
    out[["recPoly"]] <- recPoly_sf
    out[["prj"]] <- prj

    return(out)
  }
