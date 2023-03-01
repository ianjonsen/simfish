#' @title simulate acoustic transmissions & detection, using \code{simulate} & \code{sim_setup} output
#'
#' @description simulates transmissions & detections along simulated track segments within a defined range of acoustic array(s)
#'
#' @author Ian Jonsen \email{ian.jonsen@mq.edu.au}
#'
#' @param s - a deadsmolt class list containing output from sim_setup and sim_smolt/sim_kelt
#' @param delay - min & max time intervals (s) between transmissions
#' @param burst - duration of each transmission (s)
#' @param noise - range 0 - 1; simulate effect of noisy environment. Reduces detection prob w dist
#' by specified proportion; default = 1, no reduction
#' @importFrom sp Polygon Polygons SpatialPolygons CRS
#' @importFrom sf st_as_sf st_contains
#' @importFrom raster buffer
#' @importFrom prevR point.in.SpatialPolygons
#' @importFrom dplyr %>% bind_rows mutate arrange desc
#' @importFrom stats plogis
#' @export
#'
sim_detect <-
  function(s, data, delay = c(50,130), burst = 5.0, noise = 1){

    ## simulate tag transmissions along track but only within +/-10 km of avg receiver location
    ##  otherwise trap() output is far too big to generate along full track
    ##    - convert locs from km to m grid; vel in m/s

    if(!exists("recLocs", data)) stop("no receiver locations present in data")

    recLocs <- data$recLocs
    trans <- tmp.tr <- dt <- tmp.dt <- NULL
    b <- s$params$pars$pdrf

    if(exists("rec", data)) {
      if (data$rec == "lines") {
        yrec <- recLocs$y %>% unique()

        in.rng <- lapply(1:length(yrec), function(i) {
          which(abs(yrec[i] - s$sim[, "y"]) <= 1.5)
        })
        ## drop rec lines that smolt did not cross
        in.rng <- in.rng[which(sapply(in.rng, length) > 0)]

        ## simulate transmissions
        trans <- lapply(1:length(in.rng), function(i) {
          path <- s$sim[in.rng[[i]], c("id", "date", "x", "y")]
          path[, c("x", "y")] <- path[, c("x", "y")] * 1000
          sim_transmit(path, delayRng = delay, burstDur = burst) #%>%
          #          mutate(line = rep(paste0("l", i), nrow(.)))
        }) %>%
          do.call(rbind, .)

      } else if (data$rec != "lines") {
        sim_sf <- st_as_sf(s$sim, coords = c("x", "y"), crs = data$prj)
        in.rng <- st_contains(data$recPoly, sim_sf)[[1]]
        path <- s$sim[in.rng, c("id", "date", "x", "y")]
        path[, c("x", "y")] <- path[, c("x", "y")] * 1000
        if (length(in.rng >= 1)) {
          trans <- sim_transmit(path, delayRng = delay, burstDur = burst)
        } else {
          trans <- NULL
        }
    }
    } else if(!exists("rec", data)) {
      if(!is.null(data$recPoly)) {
        sim_sf <- st_as_sf(s$sim, coords = c("x", "y"), crs = data$prj)
        in.rng <- st_contains(data$recPoly, sim_sf)[[1]]
      } else {
        in.rng <- rep(TRUE, nrow(s$sim))
      }
      path <- s$sim[in.rng, c("id","date","x","y")]
      path[, c("x","y")] <- path[, c("x","y")] * 1000
      if(length(in.rng) >= 1) {
        trans <- sim_transmit(path, delayRng = delay, burstDur = burst)
      } else {
        trans <- NULL
      }
    }

    ## define logistic detection range (m) function
    ## parameterised from analysis of SoBI sentinel tag detections
    ## in July 2009 & July 2010 (see ~/Dropbox/collab/otn/fred/r/fn/sentinel.r)

    ## simulate detections given receiver locations & simulated transmission along track
      recLocs <- recLocs %>%
        mutate(x = x * 1000, y = y * 1000)

      if(!is.null(trans)) {
      detect <- trans %>%
        pdet(trs = ., rec = recLocs[, c("id","locality","x","y","z")], b = b, noise = noise)
      } else {
        detect <- NULL
      }

#      s$trans <- trans %>%
#        select(id, date, x, y) %>%
#        arrange(date)

      if(!is.null(detect)) {
      s$detect <- detect %>%
        arrange(date, recv_id, trns_id)
      } else {
        s$detect <- detect
      }

    return(s)
  }
