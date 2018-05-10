#' The BLANT function
#'
#' This function will return a table containing the sampling results\cr
#' Note:  1. Nodes must be integers numbered 0 through n-1, inclusive.\cr
#' 2. Duplicates and self-loops should be removed before calling BLANT.
#' @param mode --output Mode: o (ODV, the default); i (indexGraphlets); g (GDV); f (graphletFrequency)
#' @param n -- Sampling number
#' @param k --The number of nodes in graphlets to be sampled
#' @param filename --Graph must be in one of the following formats with its extension name:
#' GML (.gml) GraphML (.xml) LGF(.lgf) CSV(.csv) LEDA(.leda) Edgelist (.el) .
#' @param sMethod -- Sampling Mehtods. These are the valid Sampling Methods: \cr
#' 1.   SAMPLE_UNBIASED/SU -- makes things REALLY REALLY slow.  Like 10-100 samples per second rather than a million.\cr
#' 2.   SAMPLE_NODE_EXPANSION/SNE -- sample using uniform node expansion; about 100,000 samples per second.\cr
#' 3.   SAMPLE_EDGE_EXPANSION/SEE -- Fastest, up to a million samples per second.\cr
#' 4.   SAMPLE_RESERVOIR -- Lu Bressan's reservoir sampler, reasonably but not entirely unbiased.\cr
#' @keywords blant
#' @export
blant <- function(mode, n, k, filename, sMethod, threads = -1) {
  blantHelper <- function(mode, n, k, filename, sMethod, threads = -1) {
    mode <- paste("-m", mode, sep="")
    if(sMethod=="SAMPLE_UNBIASED" || sMethod=="SU") {
      cmd <- paste("./blantSU", mode, "-s", format(n, scientific = F), "-k", toString(k), filename)
    } else if(sMethod=="SAMPLE_NODE_EXPANSION" || sMethod=="SNE") {
      cmd <- paste("./blantSNE", mode, "-s", format(n, scientific = F), "-k", toString(k), filename)
    } else if(sMethod=="SAMPLE_EDGE_EXPANSION" || sMethod=="SEE") {
      cmd <- paste("./blantSEE", mode, "-s", format(n, scientific = F), "-k", toString(k), filename)
    } else if(sMethod=="SAMPLE_RESERVOIRN" || sMethod=="SR") {
      cmd <- paste("./blantSR", mode, "-s", format(n, scientific = F), "-k", toString(k), filename)
    }
    sprintf(cmd)
    system(cmd, intern=TRUE)
  }
  path = getwd()
  newpath = paste(installed.packages()["BLANT", "LibPath"],"/BLANT/blantsrc", sep="")
  setwd(newpath)
  a = blantHelper(mode, n, k, filename, sMethod, threads)
  number = as.numeric(lapply(strsplit(a, " "), function(x) x[1]))
  can = as.numeric(lapply(strsplit(a, " "), function(x) x[2]))
  df = data.frame(v1=can, v2=number)
  names(df) = c("Graphlet Number", "Count")
  setwd(path)
  return(df)
}

#blant("f", 10000, 5, "syeast.el","SNE")
