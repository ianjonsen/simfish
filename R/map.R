##' @title map
##'
##' @description a map of single or multiple simulated tracks
##'
##' @author Ian Jonsen \email{ian.jonsen@mq.edu.au}
##'
##' @param x a simfish simulation object
##' @param env ...
##' @param by.id logical (default: FALSE); if x is a multi-track object then colour tracks by id
##' @param ... additional arguments to be ignored
##'
##' @return a ggplot object
##'
##' @importFrom ggplot2 ggplot geom_path geom_point aes theme_dark coord_fixed
##' @importFrom dplyr %>% bind_rows
##' @importFrom stars st_as_stars geom_stars
##'
##' @examples
##' my.par <- sim_par(N = 1440, time.step = 5, start = c(0, 0), coa = c(0,30))
##'
##' out <- sim_fish(id = 1, mpar = my.par)
##' map(out)
##'
##' @export
##' @md

map <- function(x,
                env = NULL,
                by.id = FALSE,
                ...) {
  if(is.null(env)) stop("a raster defining the simulation environment must be supplied")

  if("land" %in% names(env)) {
    land <- st_as_stars(env$land)
  }

  if(!is.null(nrow(x))) {
    sims <- lapply(x$rep, function(.) .$sim) %>%
      bind_rows()
    dets <- lapply(x$rep, function(.) .$detect) %>%
      bind_rows()
    coa <- lapply(x$rep, function(.) if(.$params$nu != 0) {
      data.frame(x = .$params$coa[1], y = .$params$coa[2])
    } else {
      data.frame(x = NA, y = NA)
    }) %>%
      bind_rows()
  }

  m <- ggplot() + geom_stars(data = land)

  if("recLocs" %in% names(env)) {
    m <- m + geom_point(data = env$recLocs,
                        aes(x, y),
                        shape = 19,
                        colour = "firebrick",
                        alpha = 0.6,
                        size = 0.5)
  }

  if(is.null(nrow(x))) {
    m <- m + geom_point(data = out$sim,
                        aes(x, y),
                        size = 0.1,
                        colour = "orange") +
      geom_point(data = out$detect,
                 aes(recv_x, recv_y),
                 shape = 19,
                 size = 3,
                 colour = "hotpink") +
      geom_point(data = with(out$params, data.frame(x = coa[1], y = coa[2])),
                 aes(x, y),
                 shape = 17,
                 size = 2,
                 colour = "dodgerblue")

  } else {
    if(by.id) {
      m <- m + geom_point(data = sims,
                          aes(x, y, colour = id),
                          size = 0.1) +
        scale_colour_brewer(palette = "Pastel1", guide = "none")
    } else {
      m <- m + geom_point(data = sims,
                          aes(x, y),
                          size = 0.1,
                          colour = "orange")
    }
      m <- m + geom_point(data = dets,
                 aes(recv_x, recv_y),
                 shape = 19,
                 size = 3,
                 colour = "hotpink") +
      geom_point(data = coa,
                 aes(x, y),
                 shape = 17,
                 size = 2,
                 colour = "dodgerblue")
  }
    m <- m + theme_minimal() +
    theme(legend.position = "none") +
    labs(x = element_blank(),
         y = element_blank()) +
    coord_sf(expand = FALSE)

    return(m)

}
