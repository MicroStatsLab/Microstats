#' Coverage Depth Function for bin
#'
#' @description
#' Calculates coverage across the genome in bins of specified size (default 5 kb) from the output of the samtools depth function.
#'
#' @param ddn a list that contains information about each strain.
#' @param line the numeric position in the list for the line of interest.
#' @param len the bin size in bp.
#'
#' @examples
#' #ERR1938053 <- bin(cov.ddn, 11)
#'
#' @returns what it outputs
#' @export
binDepth <- function(ddn, line, len=5000){
  #divide the large list into a list that contains read #s for only one line (based on column number), chr, locus
  line.ddn <-  lapply(ddn, "[",  c(1, 2, line))
  #set up an empty list
  bin_cov <- list()
  # for each of the 13 chromosomes (note there are 13 elements/dataframes stored in the list)
  # loop over all chromosomes
  for (i in 1:13){
    # set up another empty list
    blanksliding <- list()
    bin1000 <- rep(seq(1,length(line.ddn[[i]]$locus),len), each=len)
    #how many extra 'positions' did we create from the above sequence
    extra <- length(bin1000)-length(line.ddn[[i]]$locus)
    #correct for the extra bins by subtracting off the extra, so we have the same number as actual positions
    bins <- bin1000[1:(length(bin1000)-extra)]
    # add as a column to the dataframe that contains data for one line and one chromosome
    line.ddn[[i]]$bin <- bins
    # now split into a new list for the focal line and focal chromosome by bin
    blanksliding <- split(line.ddn[[i]][,3], line.ddn[[i]]$bin)
    # now calculate the coverage of each bin/sliding window as the sum divided by the length and store it in a list; this contains the average # of reads for each of our bins, whicgh contains 5000 positions for the focal line and focal chromsoome
    bin_cov[[i]] <- sapply(blanksliding, function(x) sum(x)/length(x))
  }
  names(bin_cov) <- names(ddn)
  return(bin_cov)
}
