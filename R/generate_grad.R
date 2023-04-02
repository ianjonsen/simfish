##' @title create gradient rasters for potential function
##'
##' @description create gradient rasters for potential function that constrains
##' simulated to stay off land. Uses the `terra` package instead of `raster` for
##' enhanced computational speed
##'
##'
##' @param x input environmental raster
##'
##' @return two gradient SpatRasters defining gradients in distance from water
##'
##' @importFrom terra distance terrain
##'
##' @keywords internal

generate_grad <- function(x) {

  # set land to NA, water to 1
  x[is.na(x)] <- -1
  x[x == 1] <- NA
  x[x == -1] <- 1

  ## calculate gradient rasters - these are needed to keep fish off land
  dist <- distance(x)
  x <- terrain(dist, v = "slope", unit = "radians")
  y <- terrain(dist, v = "aspect", unit = "radians")
  grad.x <- -1 * x * cos(0.5 * pi - y)
  grad.y <- -1 * x * sin(0.5 * pi - y)
  grad <- c(grad.x, grad.y)

  return(grad)
}
