##' @title create environmental raster
##'
##' @description create environmental raster to constraint simulated tracks
##'
##' @param ext longitude and latitude extents as a 4-element vector:
##' (xmin, ymin, xmax, ymax)
##' @param res resolution of output raster in x,y km (default is 1 km).
##' Smaller values yield higher resolution but longer computation times.
##' @param prj a crs string or EPSG defining the projection for the raster with
##' units in km. The default projection is a Mercator grid:
##' `prj = "+prj=merc +datum=WGS84 +units=km"`.
##' @param grad (logical) generate gradient rasters.
##' The gradient rasters (gradient of distance from water in x and y directions)
##' are required by the potential function used to simulate tracks.
##' @param dist buffer distance (km) to add to land. This can be used when
##' generating the gradient rasters to keep tracks a specified distance from land.
##' The default is 0 km, no buffer.
##'
##' @details the `terra` package is used to generate the land and gradient
##' rasters as SpatRaster objects. This allows higher-resolution rasters to be
##' generated faster than with the `raster` package. The SpatRaster objects are
##' converted to RasterLayer (land barrier) and RasterStack (gradients) objects
##' for use in `sim_fish`.
##'
##' @return a raster defining the simulation environment (i.e., ocean with
##' land barriers). Optionally, the gradient rasters, required for the simulation
##' potential function, can also be generated. Only one of the environment raster
##' or the gradient rasters can be returned.
##'
##' @importFrom terra rast ext rasterize project
##' @importFrom raster raster stack
##' @importFrom sf st_crs
##' @importFrom dplyr "%>%" summarise
##' @importFrom rnaturalearth ne_countries
##'
##' @examples
##' x <- generate_env(ext = c(-70,43,-52,53), res = c(5,5))
##'
##' raster::plot(x)
##'
##' @export
##' @md
generate_env <- function(ext = NULL,
                         prj = "+proj=merc +datum=WGS84 +units=km",
                         res = c(1,1),
                         grad = FALSE,
                         dist = 0) {

  env.sf <- gen_land_sf(ext = ext,
                        dist = dist)

  crs.from <- st_crs(env.sf)$proj4string

  ## rasterize
  y <- rast(crs = crs.from,
            vals = 1,
            resolution = c(0.0025, 0.0025),
            extent = ext(env.sf))

  x <- rasterize(env.sf, y, fun = "min")

  ## reproject to Mercator grid in km
  ## in principle, any projection will work as long as the units are in km
  env <- project(x, y = prj, res = res)
  env[env < 1] <- 1
  env[env > 1] <- 1

  if(grad) {
    ## convert from terra rast back to raster / stack for sim use
    return(list(land = raster(env), grad = stack(generate_grad(env))))
  } else {
    return(raster(env))
  }


}
