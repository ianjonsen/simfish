
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simfish

simulate fish tracks with/without acoustic detections in featureless and
semi-realistic environments

<!-- badges: start -->

[![Project Status: WIP – Initial development is in
progress.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
<!-- badges: end -->

{simfish} is an R package that facilitates simulation of fish tracks in
semi-realistic environments (coastal oceans, fjords, and lakes). A
user-supplied raster defining the water body - land boundaries is used
to constrain simulated tracks. Fish movements are simulated (currently)
as either a correlated random walk or a biased & correlated random walk,
with the bias toward a defined Centre-of-Attraction. A potential
function is used to ensure the tracks avoid land. The package can also
be used to simulate detections of acoustically-tagged fish by acoustic
receivers at user-defined locations.

# Installation

You can install simfish from
[GitHub](https://github.com/ianjonsen/simfish) with:

``` r
# install.packages("remotes")
remotes::install_github("ianjonsen/simfish")
```

# Example 1 - simulate a fish track in a featureless environment

``` r
require(simfish, quietly = TRUE)
require(tidyverse, quietly = TRUE)
```

## Set up the simulation

``` r
## define simulation parameters

my.par <- sim_par(
  N = 250,  # number of simulation time steps
  time.step = 60, # time step in minutes
  coa = c(15, 30), # location of centre-of-attraction for biased correlated random walk (can be NA)
  nu = 1, # strength of attraction to CoA (range: 0 - infinity)
  rho = 0.6 # angular dispersion param for move directions (range 0 - 1)
)
```

## Simulate a single track as a biased & correlated random walk

``` r
## simulate a single track
out <- sim_fish(id = 1, 
                data = NULL, 
                mpar = my.par)
```

``` r
plot(out)
```

<img src="man/figures/README-ex1 plot simulated track-1.png" width="100%" />

# Example 2 - simulate fish tracks in a semi-realistic environment

``` r
require(sf, quietly = TRUE)
```

## Create an environment to simulate fish movements

``` r
## create raster using rnaturalearth hires polygon data
land <- generate_env(ext = c(-70,43,-52,53), 
                     res = c(0.03,0.03)) 

grad <- generate_grad(land)
```

## Set up the simulation

``` r
## first create a list with the required data
x <- list(land = land, 
          grad = grad, 
          prj = "+proj=merc +datum=WGS84 +units=km")

## then parameterize the simulation
my.par <- sim_par(
  N = 90*24,  # number of simulation time steps (60 days x 24 h)
  start = c(-7260, 5930), # start location for simulated fish
  time.step = 60, # time step in minutes
  bl = 3,
  coa = c(-6250, 6000), # location of centre-of-attraction for biased correlated random walk (can be NA)
  nu = 0.7, # strength of attraction to CoA (range: 0 - infinity)
  rho = 0.65, # angular dispersion param for move directions (range 0 - 1)
  beta = c(-10,-10) # potential function parameters for x and y directions to 
  # keep fish off land. Larger -ve values result in stronger land avoidance but can
  # introduce unrealistic jumps (possibly across narrow land features) in the track.
)
```

## Simulate a single track as a biased & correlated random walk

``` r
## simulate a single track
out <- sim_fish(id = 1, 
                data = x, 
                mpar = my.par)
```

## Now simulate acoustic detections on a receiver array - we’ll use the Ocean Tracking Network’s Cabot Strait receiver line

``` r
tmp <- tempfile(fileext = ".csv")
download.file("https://members.oceantrack.org//geoserver/wfs?request=getfeature&service=wfs&outputFormat=CSV&typename=otn:stations_receivers&cql_filter=collectioncode+EQ+%27CBS%27",
              destfile = tmp)
recLocs <- read_csv(tmp) %>%
  filter(deploy_date >= "2020-01-01")

xy <- recLocs %>%
  st_as_sf(., coords = c("stn_long", "stn_lat"), crs = 4326) %>%
  st_transform(crs = "+proj=merc +datum=WGS84 +units=km") %>%
  st_coordinates() %>%
  as.data.frame()
recLocs <- bind_cols(recLocs, xy) %>%
  dplyr::select(id = station_name, x = X, y = Y) %>%
  mutate(z = 1)

## add receiver locations to data
x$recLocs <- recLocs

## simulate detections of a V13 acoustic tag
##   parameters (intercept, slope) from a logistic regression of distance on 
##   detection rate
## use calc_pdrf to calculate slope for 50% detection rate at 500 m with an
##  intercept of 5
my.par$pdrf <- calc_pdrf(pdet = 0.5, dist = 500, int = 5)
out <- out %>%
  sim_detect(data = x)
```

## Visualise simulated track & detections

``` r
simfish::map(out, env = x)
```

<img src="man/figures/README-map results-1.png" width="100%" />
