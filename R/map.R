##' @title map
##'
##' @description a map of single or multiple simulated tracks
##'
##' @author Ian Jonsen \email{ian.jonsen@mq.edu.au}
##'
##' @param x a simfish simulation object
##' @param env ...
##' @param land.poly logical (default: TRUE); should land be displayed using
##' rnaturalearth spatial polygon data, or using the raster used by the simulation.
##' The former will look smoother in higher resolution images.
##' @param by.id logical (default: FALSE); if x is a multi-track object then
##' colour tracks by id
##' @param coas logical (default: FALSE); should CoA's be displayed on map
##' @param term.pts logical (default: TRUE); should terminal (start, end) points
##' be displayed on map
##' @param zoom logical (default: FALSE); should map be zoomed to extent of
##' simulated track, otherwise map uses extents from the land raster in `env`
##' @param ... graphical parameters. Arguments that can be passed to
##' `ggplot2::coord_sf()`, such as `xlim = c(20,40)`.
##' @param downsample speed up map rendering by down-sampling the land raster,
##' e.g., `downsample = 1` removes every second pixel. Default is 0 or no
##' down-sampling. Ignored if `land.poly = TRUE`
##'
##' @return a ggplot object
##'
##' @importFrom ggplot2 ggplot geom_path geom_point aes theme_minimal coord_sf
##' @importFrom ggplot2 labs theme scale_colour_brewer element_blank element_rect
##' @importFrom rnaturalearth ne_countries
##' @importFrom raster projectExtent extent
##' @importFrom dplyr "%>%" bind_rows summarise
##' @importFrom stars st_as_stars geom_stars
##' @importFrom sf st_crop st_make_valid st_transform
##' @importFrom grDevices extendrange
##'
##'
##' @export
##' @md

map <- function(x,
                env = NULL,
                land.poly = TRUE,
                by.id = FALSE,
                coas = FALSE,
                term.pts = TRUE,
                zoom = FALSE,
                downsample = 0,
                ...) {
  if(is.null(env)) stop("a raster defining the simulation environment must be supplied")

  if("land" %in% names(env) & !land.poly) {
    land <- st_as_stars(env$land)

  } else if (land.poly) {
    ## get correction project from land raster if not already given in env
    if (length(grep("prj", names(env))) == 0) {
      env$prj <- crs(env$land, asText = TRUE)
    }

    wm <- ne_countries(scale = 10, returnclass = "sf")
    ext.ll <- env$land %>%
      projectExtent(., crs = 4326) %>%
      extent()
    land <- suppressWarnings(
      wm %>%
        st_crop(
          xmin = ext.ll[1],
          ymin = ext.ll[3],
          xmax = ext.ll[2],
          ymax = ext.ll[4]
        ) %>%
        st_make_valid()
    ) %>%
      summarise(do_union = TRUE) %>%
      st_transform(crs = env$prj)

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

  m <- ggplot()

  if(land.poly) {
    m <- m + geom_sf(data = land,
                     col = NA,
                     fill = "steelblue4")
  } else {
    m <- m + geom_stars(data = land, downsample = downsample)
  }


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
    m <- m +
    theme(legend.position = "none") +
    labs(x = element_blank(),
         y = element_blank())

    if(land.poly) {
      m <- m + theme(
        panel.background = element_rect(fill = grey(0.5)),
        panel.grid = element_blank())
    }

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
