#' Calculated growth rates for given data
#'
#' @description
#' Uses two supplied dataframes (or one dataframe and one vector) to calculate growth information.
#'
#' @param d A dataframe of the OD values including a column t for time as the first column.
#' @param wells A dataframe of the well information including strain type and optionally environment.
#' @param plot A logical value indicating whether you would also like to plot your values.
#' @param aov A logical value indicated whether you would like to print the anova tests associated.
#'
#' @example \dontrun{growthRates(read.csv("GrowthRates_forLGR.csv"), read.csv("GrowthRates_forLGR_wells.csv"))}
#'
#' @returns A dataframe containing well, lgr, and maxOD information. Additionally, two plots can be created and anova created when the plot and aov is set to true.
#' @import graphics
#' @export
growthRates <- function(d, wells){

names(d) <- c("t", paste("C", seq(length=ncol(d)-1), sep=""))
d <- d[complete.cases(d$t),]

lgr <- spline.slope.dat(d)
maxOD <- apply(d[,2:length(d)], 2, function(x) max(x))
data <- data.frame(wells, lgr, maxOD)
ddn <- split(data, data$Environment)

par(mfrow=c(3, 2))
for(i in c(6, 5, 4, 2, 3, 1)){
  print(names(ddn)[i])
  plot(as.numeric(as.factor(ddn[[i]]$Strain)), ddn[[i]]$lgr, main= names(ddn)[i], ylim=c(0,0.3))
  test <- aov(ddn[[i]]$lgr~ddn[[i]]$Strain)
  print(summary(aov(test)))
  print(TukeyHSD(test))
  }

data.ag <- aggregate(data[c("lgr","maxOD")], data[c("Strain", "Environment")], mean)
data.se <- aggregate(data[c("lgr","maxOD")], data[c("Strain", "Environment")], se)

data_ag <- cbind(data.ag, data.se[,3:4 ])
names(data_ag)[5:6] <- c("lgr.se", "maxOD.se")
data_ag$numEnviro <- rep(c(6, 5, 4, 2, 3, 1), each=3)
data_ag$col <- rep(c("black", "goldenrod", "purple"))

ddn_ag <- split(data_ag, data_ag$Environment)

par(oma=c(3, 1, 1, 1))
plot(data_ag$numEnviro, data_ag$lgr, col=data_ag$col, pch=19, yaxt="n", xaxt="n", ylab="Growth rate", xlab="", xlim=c(0.7, 6.3), cex=1.3, ylim=c(0, 0.3))
axis(2, las=2, at=c(0, 0.1, 0.2, 0.3))
axis(1, at=1:6, labels=FALSE)
text(1:6,  -0.03, c("LB", "NMM(19/0/30)", "NMM(19/30/1)", "NMM(13/30/0)", "NMM(0/30/0)", "NMM(0/100/0)"), srt = -45, xpd=NA, adj=0, cex=0.8)
arrows(data_ag$numEnviro, data_ag$lgr - data_ag$lgr.se, data_ag$numEnviro, data_ag$lgr + data_ag$lgr.se, length=0)
legend("topright", col=c("black", "goldenrod", "purple"), pch=19, legend=c("TUB0", "TUB85", "TUB170"))
mtext("Environment", line=4, side=1)
text(data_ag$numEnviro+0.2, data_ag$lgr, c("B", "B", "A", "B", "B", "A", "C", "B", "A", "B", "B", "A", "B", "B", "A", "B", "A", "B"), cex=0.5)

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
