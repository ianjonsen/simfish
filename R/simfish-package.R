##' \pkg{simfish}
##'
##' simulate fish tracks & acoustic detections
##'
##' @name simfish-package
##' @aliases simfish simfish-package
##' @docType package
##' @author
##' **Maintainer:** Ian Jonsen <ian.jonsen@mq.edu.au> [ORCID](https://orcid.org/0000-0001-5423-6076)
##'
##'
##' @seealso Useful Links:
##' * [https://github.com/ianjonsen/simfish/](https://github.com/ianjonsen/simfish/)
##' * Report bugs/issues at [https://github.com/ianjonsen/simfish/issues/](https://github.com/ianjonsen/simfish/issues/)
##'
##'
##' @keywords simfish
##' @importFrom dplyr "%>%" arrange as_tibble bind_rows desc everything filter
##' @importFrom dplyr lag mutate rename select
##' @importFrom sf st_as_sf st_contains st_crop st_make_valid st_transform
##' @importFrom sp CRS Polygon Polygons SpatialPolygons
##' @importFrom stars st_as_stars geom_stars
##' @importFrom raster buffer crs distance extent extract nlayers projectExtent
##' @importFrom raster projectRaster raster rasterize stack xyFromCell
##' @importFrom ggplot2 ggplot geom_point geom_path aes coord_fixed coord_sf
##' @importFrom ggplot2 theme theme_dark theme_minimal element_blank geom_sf
##' @importFrom ggplot2 xlim ylim labs scale_colour_brewer
##' @importFrom CircStats rwrpcauchy
##' @importFrom ctmcmove rast.grad
##' @importFrom grDevices hcl.colors
##' @importFrom graphics abline
##' @importFrom lubridate week yday
##' @importFrom methods as
##' @importFrom utils setTxtProgressBar txtProgressBar
##' @importFrom stats approx plogis qlogis rbinom runif rweibull
##' @importFrom stringr str_split
##' @importFrom tibble as_tibble
##' @importFrom rnaturalearth ne_countries
NULL

## stop R CMD check generating NOTES about global variables
globalVariables(c(".", "x", "y", "x.1", "y.1", "out", "recv_x", "recv_y", "id",
                  "id.1", "z", "trns_x", "trns_y", "trns_id", "recv_id",
                  "recv_z"))
