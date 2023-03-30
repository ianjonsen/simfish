##' @title plot
##'
##' @description a simple visualization of single or multiple simulated tracks
##'
##' @author Ian Jonsen \email{ian.jonsen@mq.edu.au}
##'
##' @param x a simfish simulation object
##' @param by.id logical (default: FALSE); if x is a multi-track object then colour tracks by id
##' @param ... additional arguments to be ignored
##'
##' @return a ggplot object
##'
##' @importFrom ggplot2 ggplot geom_path geom_point aes theme_dark coord_fixed
##' @importFrom ggplot2 xlab ylab element_blank scale_colour_brewer
##' @importFrom dplyr "%>%" bind_rows
##' @importFrom grDevices hcl.colors
##' @method plot simfish
##'
##' @examples
##' my.par <- sim_par(N = 1440, time.step = 5, start = c(0, 0), coa = c(0,30))
##'
##' z <- sim_fish(id = 1, mpar = my.par)
##' plot(z)
##'
##' @export
##' @md

plot.simfish <- function(x,
                         by.id = FALSE,
                         ...) {

  if(is.null(nrow(x))) {
    p <- ggplot(x$sim) +
      geom_path(aes(x,y),
                linewidth = 0.1,
                colour = "orange") +
      geom_point(aes(x,y),
                 size = 0.75,
                 colour = "orange")
    if(x$params$nu != 0 & is.null(dim(x$params$coa))) {
      p <- p + geom_point(data = with(x$params, data.frame(x=coa[1], y=coa[2])),
                     aes(x,y),
                     shape = 17,
                     size = 2,
                     colour = "dodgerblue")
    } else if (x$params$nu != 0 & !is.null(dim(x$params$coa))) {
      xx <- as.data.frame(x$params$coa)
      names(xx) <- c("x", "y")
      p <- p + geom_point(data = xx,
                          aes(x, y),
                          shape = 17,
                          size = 2,
                          colour = "dodgerblue")
    }
      p <- p + theme_dark() +
      coord_fixed() +
      xlab(element_blank()) +
      ylab(element_blank())

  } else {
    sims <- lapply(x$rep, function(.) .$sim) %>%
      bind_rows()
    coa <- lapply(x$rep, function(.) if(.$params$nu != 0) {
      data.frame(x = .$params$coa[1], y = .$params$coa[2])
      } else {
        data.frame(x = NA, y = NA)
      }) %>%
      bind_rows()

   p <- ggplot(data = sims)

   if(by.id) {
     p <- p + geom_path(aes(x,y, group = id, colour = id),
                linewidth = 0.1) +
       geom_point(aes(x,y, group = id, colour = id),
                  size = 0.75) +
       scale_colour_brewer(palette = "Pastel1", guide = "none")
   } else {
     p <- p + geom_path(aes(x,y, group = id),
                    linewidth = 0.1,
                    colour = "orange") +
       geom_point(aes(x,y, group = id),
                  size = 0.75,
                  colour = "orange")
   }

   p <- p + geom_point(data = coa,
                 aes(x,y),
                 shape = 17,
                 size = 2,
                 colour = "dodgerblue") +
      theme_dark() +
      coord_fixed() +
      xlab(element_blank()) +
      ylab(element_blank())
  }

return(p)

}
