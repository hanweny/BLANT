#' The blant function
#'
#' This function will return a table containing the sampling results\cr
#' Note:  1. Nodes must be integers numbered 0 through n-1, inclusive.\cr
#' 2. Duplicates and self-loops should be removed before calling BLANT.
#' @param mode --output Mode: o (ODV, the default); i (indexGraphlets); g (GDV); f (graphletFrequency)
#' @param n -- Sampling number
#' @param k --The number of nodes in graphlets to be sampled
#' @param filename --Graph must be in one of the following formats with its extension name:
#' GML (.gml) GraphML (.xml) LGF(.lgf) CSV(.csv) LEDA(.leda) Edgelist (.el) .
#' @keywords blant
#' @export
blant <- function(mode, n, k, filename, threads = -1) {
  blantHelper <- function(mode, n, k, filename, threads = -1) {
    mode <- paste("-m", mode, sep="")
    cmd <- paste("./blant", mode, "-s", format(n, scientific = F), "-k", toString(k), filename)
    sprintf(cmd)
    system(cmd, intern=TRUE)
  }
  path = getwd()
  newpath = paste(.Library, "/BLANT/data", sep = "")
  setwd(newpath)
  a = blantHelper(mode, n, k, filename, threads)
  number = as.numeric(lapply(strsplit(a, " "), function(x) x[1]))
  can = as.numeric(lapply(strsplit(a, " "), function(x) x[2]))
  df = data.frame(v1=can, v2=number)
  names(df) = c("NC#", "Count")
  setwd(path)
  return(df)
}
