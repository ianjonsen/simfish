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
  if(all(!is.na(mpar$coa))) {
    delta <- c(mpar$coa[1] - xy[1], mpar$coa[2] - xy[2])
    mu <- atan2(delta[1], delta[2])
  } else {
    mu <- 45/180*pi
  }

  ## fixed rho gives strength of bias to the CoA
  phi <- rwrpcauchy(1, mu, mpar$rho)

  new.xy.tmp <- cbind(xy[1] + sin(phi) * s, xy[2] + cos(phi) * s)

  ## calculate potential fn values
  pv <- c(extract(data$grad[[1]], new.xy.tmp)[1],
          extract(data$grad[[2]], new.xy.tmp)[1])

  new.xy <- new.xy.tmp + pv * mpar$beta

  if(extract(data$land, rbind(new.xy)) == 1 & !is.na(extract(data$land, rbind(new.xy)))) {
    pv <- c(extract(data$grad[[1]], new.xy)[1],
            extract(data$grad[[2]], new.xy)[1])
    new.xy <- new.xy.tmp + pv * (mpar$beta * 3)
    ## if still on land then move to closest point in water
    if(extract(data$land, new.xy) == 1 & !is.na(extract(data$land, new.xy))) {
      ## find all nearby cells within 0.25 km & select the one farthest from land
      cells <- extract(data$land, rbind(new.xy.tmp), buffer = 0.5, cellnumbers = TRUE, df = TRUE)
      idx <- which(is.na(cells[,3]))[1]
      cell.water <- cells[idx,2]
      new.xy <- xyFromCell(data$land, cell.water) %>% rbind()
    }
  }

  cbind(new.xy[1], new.xy[2])

}
