##' @title calculate pdrf parameters
##'
##' @description A helper function to calculate the logisitc regression
##' intercept and/or slope parameters used to simulate acoustic detection
##' probability as a function of distance between transmitter and receiver
##'
##'
##' @author Ian Jonsen \email{ian.jonsen@mq.edu.au}
##'
##' @param pdet probability of detection
##' @param dist distance (in m) between transmitter and receiver
##' @param int intercept parameter from a logistic regression of distance on
##' p(detection). Default value is 5.
##' @param slope slope parameter from a logistic regression of distance on
##' p(detection). Default is NULL.
##' @param plot logical (default = FALSE); plot the relationship.
##' @param dmax maximum distance to be plotted. Ignored if plot = FALSE.
##' @param dint plotting interval between distances. Ignored if plot = FALSE.
##'
##' @return the intercept and slope parameters, given `pdet`, `dist`, and
##' one of `int` or `slope`.
##'
##' @details The `int` default value (5) is based on empirical data collected from
##' sentinel tags and receivers on the Ocean Tracking Network's Halifax line
##' (https://members.oceantrack.org/OTN/project?ccode=HFX).
##'
##' @importFrom stats qlogis
##' @importFrom graphics abline
##'
##' @examples
##' # calculate slope
##' calc_pdrf(pdet = 0.5, dist = 250, int = 5)
##'
##' # calculate intercept
##' calc_pdrf(pdet = 0.5, dist = 250, slope = -0.02, int = NULL)
##'
##' # calculate slope and plot relationship
##' calc_pdrf(pdet = 0.5, dist = 250, int = 5, plot = TRUE)
##'
##' @export
##' @md
calc_pdrf <- function(pdet = NULL,
                      dist = NULL,
                      int = 5,
                      slope = NULL,
                      plot = FALSE,
                      dmax = 1000,
                      dint = 20) {

  if(all(is.null(int), is.null(slope))) stop("Must specify one of either int or slope.")
  if(any(is.null(pdet), is.null(dist))) stop("pdet and dist must be specified.")
  #message("intercept & slope parameters: ")
  if(all(!is.null(pdet), !is.null(dist), !is.null(int), is.null(slope))) {
    pars <- c(int, (qlogis(pdet) - int) / dist)

    } else if(all(!is.null(pdet), !is.null(dist), !is.null(slope), is.null(int))) {
    pars <- c(dist * -slope + qlogis(pdet), slope)

    } else if(all(!is.null(pdet), !is.null(dist), !is.null(int), !is.null(slope))) {
      stop("both parameters specified, nothing to do...")
    }

  if(plot) {
    d <- seq(0, dmax, by = dint)
    plot(d, plogis(pars[1] + pars[2] * d),
         ylim = c(0,1),
         xlim = c(0, dmax),
         ylab = "p(Detect)",
         xlab = "Distance (m)",
         las = 1)
    abline(v = dist, col = 'red')
    abline(h = pdet, col = "red")
  }
  return(pars)
}
