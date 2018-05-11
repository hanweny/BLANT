#' The BLANT function
#'
#'This function will return a table containing the sampling results\cr\cr\cr
#' NOTE: \cr\cr
#' Before using this package, MAKE SURE you have a working Blant function installed\cr\cr
#' 1. Nodes must be integers numbered 0 through n-1, inclusive.\cr\cr
#' 2. Duplicates and self-loops should be removed before calling BLANT.
#' @param filepath: The path to your BLANT function
#' @param mode --output Mode: o (ODV, the default); i (indexGraphlets); g (GDV); f (graphletFrequency)
#' @param n -- Sampling number
#' @param k --The number of nodes in graphlets to be sampled
#' @param filename --Graph must be in one of the following formats with its extension name:
#' GML (.gml) GraphML (.xml) LGF(.lgf) CSV(.csv) LEDA(.leda) Edgelist (.el) .
#' @param connected -- if it is TRUE/T, blant function will return a table ONLY contains connected graphlets.
#' Otherwise, it can contain both connected and disconnected graphlets.
#' @keywords blant
#' @examples
#' blant(filepath=..., mode="f", n=10000, k=5, filename="syeast.el", connected=T)
#' @export
blant <- function(filepath,mode, n, k, filename, connected, threads = -1) {
  path = getwd()
  setwd(filepath)
  a = blantHelper(mode, n, k, filename, threads)
  number = as.numeric(lapply(strsplit(a, " "), function(x) x[1]))
  can = as.numeric(lapply(strsplit(a, " "), function(x) x[2]))
  df = data.frame(v1=can, v2=number)
  names(df) = c("Graphlet Number", "Count")
  if(connected) {df=ConnectedHelper(k,df)}
  setwd(path)
  return(df)
}

blantHelper <- function(mode, n, k, filename, threads = -1) {
  mode <- paste("-m", mode, sep="")
  cmd <- paste("./blant", mode, "-s", format(n, scientific = F), "-k", toString(k), filename)
  sprintf(cmd)
  system(cmd, intern=TRUE)
}

ConnectedHelper<-function(k,a) {
  path=getwd()
  setwd(paste(installed.packages()["BLANT", "LibPath"],"/BLANT/blantsrc", sep=""))
  maps <-read.table(paste("UpperToLower",toString(k),".txt",sep=""))
  maps <- maps[order(maps$V5),]
  rownames(maps) <- seq(length=nrow(maps))
  toDelete=c()
  for(i in seq(length=nrow(maps))) {
    if(maps[i,"V1"] == 0) {
      toDelete <- c(toDelete,i)
    }
  }
  a = a[-toDelete,]
  rownames(a) <- seq(length=nrow(a))
  setwd(path)
  return(a)
}


#blant("~/research/wayne/BLANT","f", 10000, 5, "syeast.el",T)




