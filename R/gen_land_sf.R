##' @title utility function - generate sf land
##'
##' @description generates an sf data.frame of land from user-specified extents
##'
##' @param ext longitude and latitude extents as a 4-element vector:
##' (xmin, ymin, xmax, ymax)
##' @param dist buffer distance (km) to add to land. This can be used when
##' generating the gradient rasters to keep tracks a specified distance from land.
##' The default is 0 km, no buffer.
##'
##' @return an sf data.frame defining the simulation environment (i.e., ocean with
##' land barriers).
##'
##' @importFrom sf st_crop st_make_valid st_buffer
##' @importFrom dplyr "%>%" summarise
##' @importFrom rnaturalearth ne_countries
##'
##' @keywords internal
gen_land_sf <- function(ext,
                        dist = 0) {
  if (is.null(ext))
    stop("Extents in long,lat must be provided.")

  if (requireNamespace("rnaturalearthhires", quietly = TRUE)) {
    env <- ne_countries(scale = 10, returnclass = "sf") %>%
      st_make_valid
  } else {
    message(
      "using medium resolution data; install 'rnaturalearthhires' pkg for highest resolution data"
    )
    env <- ne_countries(scale = 50, returnclass = "sf") %>%
      st_make_valid
  }

env.sf <- suppressWarnings(env %>%
                             st_crop(., y = c(
                               xmin = ext[1],
                               ymin = ext[2],
                               xmax = ext[3],
                               ymax = ext[4]
                             )) %>%
                             summarise(do_union = TRUE))

  if (dist > 0) {
    env.sf <- env.sf %>%
      st_buffer(dist = dist)
  }
  env.sf <- env.sf %>%
    st_make_valid()

  return(env.sf)
}
