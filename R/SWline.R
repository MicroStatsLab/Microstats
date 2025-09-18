#' Coverage Depth Function for SWline
#'
#' @description
#' What it does
#'
#' @param line the line of interest, output from bin function
#' @param strain_name used for plotting defaults to the name of the line
#'
#' @examples
#' #SWline(SRR8068050)
#'
#' @returns what it outputs
#' @export

SWline <- function(line, strain_name = NULL) {
  # Use strain_name if provided; otherwise fall back to object name
  nm <- if (!is.null(strain_name)) strain_name else deparse(substitute(line))

  x0 <- length(line[[1]])
  stand <- median(unlist(line[[1]]))

  plot(1:x0, unlist(line[[1]]) / stand, type = "l", col = "red",
       xlim = c(0, length(unlist(line))), xaxt = "n", yaxt = "n",
       xlab = "", ylab = "", ylim = c(0, 5))

  for (i in 2:13) {
    x1 <- length(line[[i]]) + x0 - 1
    points(x0:x1, unlist(line[[i]] / stand), type = "l", col = col[i])
    x0 <- x1
  }

  mtext(nm, side = 3, adj = 0.01, cex = 0.7)
  abline(h = 1:4, lty = 2)
  axis(1, at = c(49,148,254,375,509,671,862,1067,1282,1511,1761,2037,2323), labels = FALSE)
}
