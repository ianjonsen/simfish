#' @title random walk movement kernel for Miramichi smolts
#'
#' @description utility function not to be called by user
#'
#' @importFrom CircStats rwrpcauchy
#' @importFrom stats rweibull
#' @importFrom raster extract xyFromCell
#' @export
#'
move_kernel <- function(data, xy = NULL, mpar, s, pv) {

  ## biased random walk toward a Center of Attraction
  if(!is.na(mpar$coa)) {
    delta <- c(mpar$coa[1] - xy[1], mpar$coa[2] - xy[2])
    mu <- atan2(delta[1], delta[2])
  } else {
    mu <- 0
  }

  ## fixed rho gives strength of bias to the CoA
  phi <- rwrpcauchy(1, mu, mpar$rho)

  new.xy <- cbind(xy[1] + sin(phi) * s, xy[2] + cos(phi) * s) + pv * mpar$beta

  cbind(new.xy[1], new.xy[2])

}
