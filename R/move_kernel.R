#' @title random walk movement kernel for Miramichi smolts
#'
#' @description utility function not to be called by user
#'
#' @importFrom CircStats rwrpcauchy
#' @importFrom stats rweibull
#' @importFrom raster extract xyFromCell
#' @keywords internal
#'
move_kernel <- function(data, xy = NULL, mpar, s) {

  ## biased random walk toward a Center of Attraction
  if(all(!is.na(mpar$coa))) {
    delta <- c(mpar$coa[1] - xy[1], mpar$coa[2] - xy[2])
    psi <- atan2(delta[1], delta[2])

    phi <- atan2(sin(xy[3]) + mpar$nu * sin(psi), cos(xy[3]) + mpar$nu * cos(psi))

  } else {
    phi <- atan2(sin(xy[3]), cos(xy[3]))
  }

  ## fixed rho gives strength of bias to the CoA
  mu <- rwrpcauchy(1, phi, mpar$rho)

  new.xy <- cbind(xy[1] + sin(mu) * s, xy[2] + cos(mu) * s)

  if (!is.null(data$land)) {
    ## calculate potential fn values
    pv <- c(extract(data$grad[[1]], new.xy)[1],
            extract(data$grad[[2]], new.xy)[1])

    new2.xy <- new.xy + pv * mpar$beta

    ## if provision new.xy is on land then try again
    if (!is.na(extract(data$land, rbind(new2.xy)))) {
      pv <- c(extract(data$grad[[1]], new2.xy)[1],
              extract(data$grad[[2]], new2.xy)[1])
      new3.xy <- new.xy + pv * (mpar$beta * 3)
      ## if still on land then move back to water
      if (!is.na(extract(data$land, new3.xy))) {
        ## find all nearby cells within mpar$buffer km & select the first one in water
        cells <-
          extract(
            data$land,
            rbind(new.xy),
            buffer = mpar$buffer,
            cellnumbers = TRUE,
            df = TRUE
          )
        idx <- which(is.na(cells[, 3]))[1]
        cell.water <- cells[idx, 2]
        new.xy <- xyFromCell(data$land, cell.water) %>% rbind()
      } else {
        new.xy <- new3.xy
      }
    } else {
      new.xy <- new2.xy
    }
  }

  cbind(new.xy[1], new.xy[2], mu)

}
