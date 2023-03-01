##' @importFrom dplyr %>% summarise group_by n n_distinct
##' @importFrom sf st_coordinates
##' @method summary simsmolt
##' @export
summary.simsmolt <- function(x, data = NULL, ...) {
  if (length(list(...)) > 0) {
    warning("additional arguments ignored")
  }
  
  if(length(class(x)) == 1) {
    stop("summary not yet implemented for single simulation runs...\n")
  }
  
  else if(class(x)[2] == "rowwise_df" || class(x)[2] == "grouped_df") {
    compl <- sapply(x$rep, function(.) !inherits(., "try-error"))
    cat(sprintf("dropping %i failed runs\n\n", sum(!compl)))
    x <- x[compl, ]
  
  ## summarise multiple replicates
  if(names(x)[2] != "rep") stop("expecting simulation output objects to be named 'rep'")
    
    ## count smolts passing each receiver line/array (whether or not detected)
    r <- data$recLoc
    yrec <- unique(r$y)
    maxy <- sapply(x$rep, function(.) nrow(.$sim)) %>% max()
    na.fill <- function(., maxy) {
      y <- .$sim$y
      ly <- length(y)
      if(ly < maxy) {
        y <- c(y, rep(NA, maxy - ly))
      }
      y
    }
    sm.y <- lapply(x$rep, na.fill, maxy) %>% do.call(cbind, .)
    
    fn <- function(x) sapply(1:length(yrec), function(i) sum(sum((yrec[i] - x) < 0, na.rm = TRUE) > 0))
    num.cross <- apply(sm.y, 2, fn) %>% apply(., 1, sum)

    all.dt <- lapply(x$rep, function(.) .$detect) %>% do.call(rbind, .)
    dt.by.line <- all.dt %>% group_by(line) %>% summarise(n = n())
    dt.by.recline <- all.dt %>% group_by(line,recv_id) %>% summarise(n = n()) %>% group_by(line) %>% summarise(n = n())
  
    ## count individual smolts detected at each line
    num.ind.smolt.dt.by.line <- all.dt %>% group_by(line, trns_id) %>% summarise(n = n())
    num.smolt.dt.by.line <-  num.ind.smolt.dt.by.line %>% summarise(n = n())
    
    ## count mortalities between each line
    dead <- lapply(x$rep, function(.) .$sim[nrow(.$sim), c("id","y","s")]) %>% 
      do.call(rbind, .) %>%
      filter(s == 0)
    
    which.line <- lapply(1:5, function(i) {
      if(i==1) which(dead$y < yrec[1])
      else if(i>1 & i<5) { which(dead$y >= yrec[i-1] & dead$y < yrec[i])}
      else if(i==5) { which(dead$y >= yrec[i-1])}
    })
    
    num.dead.by.line <- sapply(which.line, length)

    ## record how many smolts get detected on 1,2,3, or 4 lines
    tmp <- all.dt %>% group_by(trns_id) %>% summarise(n=n_distinct(line))
    num.smolt.lines <- table(tmp$n)
    whsm <- tmp$trns_id[which(tmp$n > 1)] 
    
    lines <- paste0("l",1:length(yrec))
    if(nrow(dt.by.line != length(yrec))) {
      ex <- which(!lines %in% dt.by.line$line)
      dt.by.line <- rbind(dt.by.line, data.frame(line = lines[ex], n = ex*0)) %>%
        arrange(line)
    }
    if(nrow(dt.by.recline != length(yrec))) {
      ex <- which(!lines %in% dt.by.recline$line)
      dt.by.recline <- rbind(dt.by.recline, data.frame(line = lines[ex], n = ex*0)) %>%
        arrange(line)
    }
    if(nrow(num.smolt.dt.by.line) != length(yrec)) {
      ex <- which(!lines %in% num.smolt.dt.by.line$line)
      num.smolt.dt.by.line <- rbind(num.smolt.dt.by.line, data.frame(line = lines[ex], n = ex*0)) %>%
        arrange(line)
    }
    if(length(num.smolt.lines) != length(yrec)) {
      ex <- which(!lines %in% paste0("l", names(num.smolt.lines)))
      num.smolt.lines <- as.numeric(c(num.smolt.lines, c(ex*0)))
    }

  n <- c(num.cross, nrow(x))
  ndt <- c(num.smolt.dt.by.line$n, length(unique(num.ind.smolt.dt.by.line$trns_id)))
  nsl <- c(num.smolt.lines, sum(num.smolt.lines[-1]))

  return(structure(list(
    n = n,
    num.dead = c(num.dead.by.line[-5], sum(num.dead.by.line)),
    ndt = ndt,
    nsl = nsl,
    whsm = whsm,
    dt.num = c(dt.by.line$n, nrow(all.dt)),
    dt.by.recline = c(dt.by.recline$n, sum(dt.by.recline$n)),
    p.smolt = ndt/n
  ),
  class = "summary.simsmolt"))

  }
}

##' @method print summary.simsmolt
##' @export
print.summary.simsmolt <- function(x)
{

  xx <- t(cbind(c(NA, x$n), 
                c(NA, x$num.dead),
                c(NA, x$ndt),
                c(NA, ".", x$nsl[-1]),
                   c(NA, x$dt.num), 
                   c(NA, x$dt.by.recline),
                   c(NA, round(x$p.smolt, 4))
                   ))
  
  xx <- cbind(c("smolts", "mortalities","smolts detected", "smolts detected on > 1 line", "detections", "receivers with detections", "p(smolts detected)"),
              xx)

  dimnames(xx) <- list(rep("", 7), 
                       c("", " ", paste0("line.", 1:(length(x$n) - 1)), "total")
                       )
  
  print(xx, justify="right", right=TRUE, quote=FALSE, na.print="")

  invisible(xx)
  
} 
