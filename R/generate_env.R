##' @title create environmental raster
##'
##' @description create environmental raster to constraint simulated tracks
##'
##' @author Ian Jonsen \email{ian.jonsen@mq.edu.au}
##'
##' @param ext longitude and latitude extents as a 4-element vector:
##' (xmin, ymin, xmax, ymax)
##' @param res resolution of raster in degrees long and lat. Smaller values
##' yield higher resolution but longer computation times.
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
##' @return a raster defining the simulation environment (i.e., ocean with
##' land barriers). Optionally, the gradient rasters, required for the simulation
##' potential function, can also be generated. Only one of the environment raster
##' or the gradient rasters can be returned.
##'
##' @importFrom raster raster extent rasterize projectExtent projectRaster crs
##' @importFrom sf st_transform st_crop st_make_valid st_buffer
##' @importFrom methods as
##' @importFrom dplyr "%>%" summarise
##' @importFrom rnaturalearth ne_countries
##'
##' @examples
##' x <- generate_env(ext = c(-70,43,-52,53), res = c(0.04,0.04))
##'
##' raster::plot(x)
##'
##' @export
##' @md
generate_env <- function(ext = NULL,
                         res = c(0.05, 0.05),
                         prj = "+proj=merc +datum=WGS84 +units=km",
                         grad = FALSE,
                         dist = 0) {

  env.sf <- gen_land_sf(ext = ext,
                        dist = dist)

  ## rasterise
  env <- as(env.sf, "Spatial")

  ## rasterize at a high resolution for a pretty map
  ##  a lower resolution will run faster
  env <- raster(crs = crs(env),
                         vals = 1,
                         resolution = res,
                         ext = extent(c(ext[1], ext[3], ext[2], ext[4]))) %>%
    rasterize(env, .)

  ## reproject to Mercator grid in km
  ## in principle, any projection will work as long as the units are in km
  ext <- projectExtent(env, crs = prj)
  env <- projectRaster(env, ext)
  env[env < 1] <- 1
  env[env > 1] <- 1

  if(grad) {
    return(list(land = env, grad = generate_grad(env)))
  } else {
    return(env)
  }


}
