#' Calculated growth rates for given data
#'
#' @description
#' Uses two supplied dataframes (or one dataframe and one vector) to calculate growth rate information.
#'
#' @param d A dataframe of the OD values including a column t for time as the first column.
#' @param wells A dataframe of the well information including strain type and optionally environment.
#' @param plotRaw A logical value indicating whether you would like to plot your raw data.
#' @param strainName A string value indicating the strain name.
#'
#' @example \dontrun{growthRates(read.csv("GrowthRates_forLGR.csv"), read.csv("GrowthRates_forLGR_wells.csv"))}
#'
#' @returns A dataframe containing well, lgr, and maxOD information. Additionally, plots of the raw data curves can be created.
#' @import graphics
#' @export
growthRates <- function(d, wells, plotRaw = TRUE, strainName = "Name"){

names(d) <- c("t", paste("C", seq(length=ncol(d)-1), sep=""))
d <- d[complete.cases(d$t),]

lgr <- spline.slope.dat(d)
maxOD <- apply(d[,2:length(d)], 2, function(x) max(x))
data <- data.frame(wells, lgr = round(lgr, 3), maxOD = round(maxOD, 3))

if(plotRaw == TRUE){
  par(mfrow = c(12, round(nrow(wells)/12, 0)), mar=c(1, 1, 1, 1))
  for(i in 1:nrow(wells)) {
    plot(d[,1], d[,i+1], ylim= c(0, max(d[,2:ncol(d)])*1.1), xlab="", ylab="", yaxt="n", xaxt="n")
    mtext(wells[i, strainName], side=3, adj=0.01)
  }
}

return(data)
}

nderiv <- function(fit, x, eps=1e-5)
  (predict(fit, x + eps) - predict(fit, x - eps))/(2 * eps)

spline.slope <- function(x, y, n=101, eps=1e-5)
  max(nderiv(loess(log(y) ~ x, degree=1, span=0.1),
             seq(min(x), max(x), length=n)), na.rm=TRUE)

spline.slope.dat <- function(d, ...)
  sapply(d[-1], spline.slope, x=d$t, ...)

se <- function(x, na.rm=TRUE) sqrt(var(x, na.rm=TRUE)/(length(x) - 1))
