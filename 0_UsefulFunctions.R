axis.intervals <- function(side=1, ticks = axTicks(side), atLabels = NULL, labels = 1:length(atLabels), ...)
# side : integer between 1 and 4, the side at which to draw the interval.  The axis is placed as follows: 1=below, 2=left, 3=above and 4=right ;
# ticks : position of the ticks delimiting the intervals. If a single number is provided, this indicates the number of intervals; if a vector is provided, this indicates the position of each ticks ; 
# atLabels : the position of the labels for each interval. If not of length nIntervals - 1, the vector is either recycled, either cut. The default is to the center of the interval ;
# labels : the labels of each interval ; 
# ... : other graphical parameters to pass to the 'axis' function
{
    stopifnot((side <- as.integer(side)) %in% 1:4)
    if (length(ticks) == 1){
       is.x <- side%%2 == 1
       usr <- par("usr")[1:2 + 2*!is.x]
       XY <- function(ch) paste0(if (is.x) "x" else "y", ch)
       axs <- par(XY("axs"))
       if (axs == "r"){
          per4 <- 4*diff(usr)/108
          usr[1] <- usr[1] + per4
          usr[2] <- usr[2] - per4
       }
       nIntervals <- floor(ticks)
       ticks <- seq(usr[1],usr[2],length.out = nIntervals + 1)
    } else {
       nIntervals <- length(ticks) - 1
    }    
    axis(side, at = ticks, labels = FALSE, ...)
    if (is.null(atLabels)){
       atLabels <- (ticks[-1] + ticks[1:nIntervals]) / 2
    } else {
       if (length(atLabels) != nIntervals) atLabels <- rep_len(atLabels,nIntervals)
    }
    if (length(labels) != nIntervals) labels <- rep_len(labels,nIntervals)
    axis(side, at = atLabels, labels = labels, tick = FALSE, ...)
}
