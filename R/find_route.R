##' @title find_route
##'
##' @description check for barriers between start and end (CoA) points & find route
##' around barriers via intermediate CoA's to be used by `sim_fish()`. Can be
##' called prior to `sim_fish()` or within `sim_fish()`.
##'
##' @param data - a list of required data. If missing then simulation runs on a
##' Cartesian grid (a featureless environment).
##' @param mpar - simulation control parameters supplied as a list using `sim_par()`
##' @param coas - number of Centres of Attraction to calculate
##' @param npts - number of route-finding points to use. A larger number of points
##' can produce smoother routes that do not jump across land, a smaller number of
##' points will produce a less complex route, faster.
##' @param land.buff - distance (km) to buffer around land. A small buffer
##' (e.g., 1 - 2 km) can reduce coastline complexity and make route-finding simpler.
##' @param buffer - `pathroutr` argument that sets the typical distance from
##' coastline of the visibility graph.
##' @param centroids - `pathroutr` argument that turns on/off (default: FALSE)
##' centroids in calculating the visibility graph
##' @param keep.coas - index of calculated CoA's to use in the simulation. Default
##' is NULL (use all calculated CoA's).
##'
##' @return a list including a matrix of calculated CoA's and a `ggplot2` object
##' visualizing the CoA's. The visualization is useful for determining whether to
##' use all the CoA's or subset them, and for helping to diagnose route-finding
##' failures.
##' @importFrom raster projectExtent extent crs
##' @importFrom sf st_transform st_bbox st_make_valid st_buffer st_crop st_as_sf
##' @importFrom sf st_intersects st_coordinates st_crs
##' @importFrom dplyr "%>%" bind_rows summarise rename
##' @importFrom pathroutr prt_visgraph prt_reroute
##' @importFrom ggplot2 ggplot geom_sf geom_point geom_path aes coord_sf
##' @importFrom grDevices extendrange grey
##' @importFrom rnaturalearth ne_countries
##'
##' @export
##' @md

find_route <- function(data,
                    mpar,
                    coas = 12,
                    npts = 40,
                    land.buff = 2,
                    buffer = 5,
                    centroids = FALSE,
                    keep.coas = NULL
                    ) {

  if(length(grep("prj", names(data))) == 0) {
    data$prj <- crs(data$land)
  }

  cat("finding route around land barriers...\n")

  if(inherits(mpar$coa, "matrix")) {
    warning("Multiple CoA's detected in mpar, using the last CoA. All others will be overwritten",
            immediate. = TRUE)
    mpar$coa <- mpar$coa[nrow(mpar$coa), ]
    }

  ## required for pathroutr fn's
  # default vals
  detach.dplyr.on.end <- FALSE
  detach.sf.on.end <- FALSE

  if(!"package:dplyr" %in% search()) {
    detach.dplyr.on.end <- TRUE
    suppressMessages(attachNamespace("dplyr"))
  }
  if(!"package:sf" %in% search()) {
    detach.sf.on.end <- TRUE
    suppressMessages(attachNamespace("sf"))
  }


  wm <- ne_countries(scale = 10, returnclass = "sf")
  ext.ll <- data$land %>%
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
    st_transform(crs = data$prj)

  land <- land %>%
    st_buffer(dist = land.buff) %>%
    summarise(do_union = TRUE) %>%
    st_make_valid()

  xs <- approx(c(mpar$start[1], mpar$coa[1]), n = npts)$y
  ys <- approx(c(mpar$start[2], mpar$coa[2]), n = npts)$y

  xy.sf <- data.frame(x = xs, y = ys) %>%
    st_as_sf(coords = c("x", "y"), crs = data$prj)

  ## test if any point falls on land, if not then nothing to do
  idx <- as.vector(st_intersects(xy.sf, land, sparse = FALSE))
  if(sum(idx) != 0) {
    ## set start and end points to TRUE so they are included
    idx[c(1, npts)] <- TRUE
    xy.sf <- xy.sf[idx,]

    vg <-
      suppressWarnings(prt_visgraph(land, buffer = buffer, centroids = centroids))
    out <- prt_reroute(xy.sf, land, vg, blend = TRUE) %>%
      st_as_sf()

    coas.p <- st_coordinates(out) %>%
      as.data.frame() %>%
      rename(x = X, y = Y)

    coas.df <- data.frame(x = approx(coas.p$x, n = coas)$y,
                          y = approx(coas.p$y, n = coas)$y)

    if (!is.null(keep.coas))
      coas.df <- coas.df[keep.coas,]

    coas.m <- bind_rows(coas.df,
                        data.frame(x = mpar$coa[1],
                                   y = mpar$coa[2])) %>%
      as.matrix()
    rownames(coas.m) <- 1:nrow(coas.m)

    ## plot rerouting on top of visibility network
    m <- ggplot() +
      geom_sf(
        data = st_as_sf(vg, "edges"),
        col = "lightblue",
        alpha = 0.5
      ) +
      geom_sf(
        data = st_as_sf(vg, "nodes"),
        col = "lightblue",
        alpha = 0.5
      ) +
      geom_sf(data = land,
              col = NA,
              fill = grey(0.4)) +
      geom_sf(data = xy.sf,
              col = "red",
              size = 0.8) +
      geom_path(data = coas.p,
                aes(x, y),
                col = 'blue3',
                linewidth = 0.5) +
      geom_point(data = coas.p,
                 aes(x, y),
                 col = 'blue3',
                 size = 0.7) +
      geom_point(
        data = with(mpar, data.frame(x = start[1], y = start[2])),
        aes(x, y),
        col = "blue",
        shape = 17,
        size = 5
      ) +
      geom_point(data = as.data.frame(coas.m),
                 aes(x, y),
                 col = "red",
                 size = 2) +
      geom_point(
        data = as.data.frame(coas.m)[nrow(coas.m),],
        aes(x, y),
        col = "firebrick",
        size = 5,
        shape = 15
      ) +
      coord_sf(
        datum = st_crs(data$prj),
        xlim = extendrange(coas.p$x, f = 0.2),
        ylim = extendrange(coas.p$y, f = 0.2),
        expand = FALSE
      )
    if (detach.dplyr.on.end)
      detach(package:dplyr)
    if (detach.sf.on.end)
      detach(package:sf)

    return(list(coas.m, m))
  } else {
    cat("no land barriers detected, using original CoA...\n")
    return(list(mpar$coa, NULL))
  }
}

