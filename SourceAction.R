
# Get max dir
UseMaxDist <- function(dVec,speed) {
  maxDist <- max(dVec)
  posNr <- c(1:length(dVec))[dVec==maxDist][1]
  dirChange <- seq(-0.5,0.5,1/(length(dVec)-1))[posNr]
  return(list(dirChange=dirChange,speed=speed))
}


