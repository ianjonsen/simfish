##' @title create gradient rasters for potential function
##'
##' @description create gradient rasters for potential function that constrains
##' simulated to stay off land
##'
##' @author Ian Jonsen \email{ian.jonsen@mq.edu.au}
##'
##' @param x input environmental raster
##'
##' @return two gradient rasters defining gradients in distance from water
##'
##' @importFrom raster distance stack
##' @importFrom ctmcmove rast.grad
##'
##' @examples
##' x <- generate_env(ext = c(-70,43,-52,53), res = c(0.04,0.04))
##' g <- generate_grad(x)
##'
##' raster::plot(g)
##'
##' @export
##' @md
generate_grad <- function(x) {

  # set land to NA, water to 1
  x[is.na(x)] <- -1
  x[x == 1] <- NA
  x[x == -1] <- 1

  ## calculate gradient rasters - these are needed to keep fish off land
  dist <- distance(x)
  grad <- rast.grad(dist)
  grad <- stack(grad$rast.grad.x, grad$rast.grad.y)

  return(grad)
}
