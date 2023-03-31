##' @title map
##'
##' @description a map of single or multiple simulated tracks
##'
##' @author Ian Jonsen \email{ian.jonsen@mq.edu.au}
##'
##' @param x a simfish simulation object
##' @param env ...
##' @param by.id logical (default: FALSE); if x is a multi-track object then
##' colour tracks by id
##' @param coas logical (default: FALSE); should CoA's be displayed on map
##' @param term.pts logical (default: TRUE); should terminal (start, end) points
##' be displayed on map
##' @param zoom logical (default: FALSE); should map be zoomed to extent of
##' simulated track, otherwise map uses extents from the land raster in `env`
##' @param ... graphical parameters. Arguments that can be passed to
##' `ggplot2::coord_sf()`, such as `xlim = c(20,40)`.
##'
##' @return a ggplot object
##'
##' @importFrom ggplot2 ggplot geom_path geom_point aes theme_minimal coord_sf
##' @importFrom ggplot2 labs theme scale_colour_brewer
##' @importFrom dplyr "%>%" bind_rows
##' @importFrom stars st_as_stars geom_stars
##'
##'
##' @export
##' @md

map <- function(x,
                env = NULL,
                by.id = FALSE,
                coas = FALSE,
                term.pts = TRUE,
                zoom = FALSE,
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
    m <- m + geom_path(data = x$sim,
                       aes(x, y),
                       linewidth = 0.1,
                       colour = "orange") +
      geom_point(data = x$sim,
                        aes(x, y),
                        size = 0.1,
                        colour = "orange")

    if("recLocs" %in% names(env)) {
      m <- m + geom_point(data = x$detect,
                 aes(recv_x, recv_y),
                 shape = 19,
                 size = 3,
                 colour = "hotpink")
    }

    if (coas) {
      if (is.null(dim(x$params$coa))) {
        m <-
          m + geom_point(
            data = with(x$params, data.frame(x = coa[1], y = coa[2])),
            aes(x, y),
            shape = 17,
            size = 2,
            colour = "dodgerblue"
          )
      } else {
        xx <- as.data.frame(x$params$coa)
        names(xx) <- c("x", "y")
        m <- m + geom_point(
          data = xx,
          aes(x, y),
          shape = 17,
          size = 2,
          colour = "dodgerblue"
        )
      }
    }
    if(term.pts) {
      m <-
        m + geom_point(
          data = with(x$params, data.frame(x = start[1], y = start[2])),
          aes(x, y),
          shape = 17,
          size = 2,
          colour = "dodgerblue"
        ) +
        geom_point(
          data = x$sim[nrow(x$sim), ],
          aes(x, y),
          shape = 15,
          size = 2,
          colour = "firebrick"
        )
    }

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
    if("recLocs" %in% names(env)) {
      m <- m + geom_point(data = dets,
                 aes(recv_x, recv_y),
                 shape = 19,
                 size = 3,
                 colour = "hotpink")
    }
      m <- m + geom_point(data = coa,
                 aes(x, y),
                 shape = 17,
                 size = 2,
                 colour = "dodgerblue")
  }
    m <- m + theme_minimal() +
    theme(legend.position = "none") +
    labs(x = element_blank(),
         y = element_blank())

    if(zoom) {
      m <- m + coord_sf(
        expand = FALSE,
        xlim = extendrange(x$sim$x, f= 0.2),
        ylim = extendrange(x$sim$y, f= 0.2),
        ...
      )
    } else {
      m <- m + coord_sf(expand = FALSE, ...)
    }

    return(m)

}
