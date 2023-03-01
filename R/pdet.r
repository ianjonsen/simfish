#' @title Utility fn to calculate distances b/w transmitter and reciever
#'
#' @description assumes detection is a logistic fn of distance in 3-D
#'
#' @author Ian Jonsen \email{ian.jonsen@mq.edu.au}
#'
#' @param trs - a tibble of transmission locations (in m) with: id, x, y, et and line id
#' @param rec - a data.frame of receiver locations (in m) with: x, y, z
#' @param b - a vector of two parameters (intercept & slope) for logistic detection range function
#' @param noise - range 0 - 1; simulate effect of noisy environment. Reduces detection prob w dist
#' by specified proportion; default = 1, no reduction
#' @export
#'
pdet <- function(trs = NULL, rec = NULL, b = NULL, noise = 1){

#3-D distance between gth receiver and each transmission
# assume transmissions at surface...
  ## calculate all pairwise distances in 3-D
  xd2 <- outer(trs$x, rec$x, FUN = "-")^2
  yd2 <- outer(trs$y, rec$y, FUN = "-")^2
  zd2 <- outer(0, rec$z, FUN = "-")^2

  dist <- sqrt(xd2 + yd2 + c(zd2))
  dimnames(dist) <- list(NULL, recv_id = rec$id)

  ## calculate probability of detection
  pr.det <- apply(dist, 2, function(.) plogis(b[1] + b[2] * .) * noise)

  ## simulate detection
  det <- apply(pr.det, 2, function(x) rbinom(length(x),1,x))

  idx <- which(det == 1, arr.ind = TRUE)

  out <- data.frame(trs[idx[, 1], ], rec[idx[, 2], ], dist = dist[det == 1]) %>%
    rename(
      trns_id = id,
      date = date,
      recv_id = id.1,
      recv_array = locality,
      trns_x = x,
      trns_y = y,
      recv_x = x.1,
      recv_y = y.1,
      recv_z = z
    ) %>%
    mutate(
      trns_x = trns_x / 1000,
      trns_y = trns_y / 1000,
      recv_x = recv_x / 1000,
      recv_y = recv_y / 1000,
      dist = dist/1000
    ) %>%
    select(trns_id,
           recv_id,
           recv_array,
           date,
           trns_x,
           trns_y,
           recv_x,
           recv_y,
           recv_z,
           dist) %>%
    arrange(recv_id, recv_array, date)

out

}
