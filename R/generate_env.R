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
##' `prj = "+prj=merc +datum=WGS84 +units=km"`
##' @param ... additional arguments to be ignored
##'
##' @return a raster defining the simulation environment
##'
##' @importFrom raster raster extent rasterize projectExtent projectRaster crs
##' @importFrom sf as_Spatial st_transform st_crop st_make_valid
##' @importFrom dplyr "%>%"
##' @importFrom rnaturalearth ne_countries
##'
##' @examples
##' x <- generate_env(ext = c(-70,43,-52,53), res = c(0.04,0.04))
##'
##' raster::plot(x, zlim = c(0,1))
##'
##' @export
##' @md
generate_env <- function(ext = NULL,
                         res = c(0.05, 0.05),
                         prj = "+proj=merc +datum=WGS84 +units=km") {

  if(is.null(ext)) stop("Extents in long,lat must be provided.")

  env <- suppressWarnings(rnaturalearth::ne_countries(scale = 10, returnclass = "sf") %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_crop(xmin=ext[1], ymin=ext[2], xmax=ext[3], ymax=ext[4]) %>%
    sf::st_make_valid())

  ## rasterise
  env <- as(env, "Spatial")

  ## rasterize at a high resolution for a pretty map
  ##  a lower resolution will run faster
  env <- raster::raster(crs = crs(env),
                         vals = 1,
                         resolution = res,
                         ext = extent(c(ext[1], ext[3], ext[2], ext[4]))) %>%
    raster::rasterize(env, .)

  ## reproject to Mercator grid in km
  ## in principle, any projection will work as long as the units are in km
  ext <- raster::projectExtent(env, crs = prj)
  env <- raster::projectRaster(env, ext)
  env[env > 1] <- 1

  return(env)

}
