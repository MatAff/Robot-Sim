
# Sense line 
SenseLine <- function(senseLine, draw=FALSE) {
  # Assumed global: spatialLines
  if(draw) { lines(senseLine,type="l") }
  l2 <- Line(senseLine)
  sl2 <- SpatialLines(list(Lines(list(l2),ID="1")))
  int.pts <- gIntersection(spacialLines,sl2,byid = TRUE)
  if(is.null(int.pts)) {
    d <- 999
  } else {
    cord <- int.pts@coords
    dVec <- sapply(1:nrow(cord), function(e) sqrt((cord[e,1] - x) ^ 2 + (cord[e,2] - y) ^ 2))
    d <- min(dVec)
    
    # Plot intersect
    if(draw) { points(int.pts@coords[dVec==d,"x"],int.pts@coords[dVec==d,"y"]) } 
  }
  return(d)
}

# Function to get distance vector 
SenseAndPlot <- function(nrSteps=20, draw=FALSE) {
  
  dVec <- c()
  
  # Loop (default 20 step per rotation)
  for(senseDir in seq(-0.5,0.5,2/nrSteps)) {
  
    # Determing overly long line
    endX <- x+sin((senseDir + dir) * pi) * 100
    endY <- y+cos((senseDir + dir) * pi) * 100
    sLine <- matrix(c(x,y,endX,endY),ncol=2, byrow=TRUE)
    if (senseDir>0.5) { draw <- FALSE }
    
    # Get intersect distance and add to vector
    dist <- SenseLine(sLine,FALSE)
    dVec <- c(dVec,dist)
    
    # Draw with updated distance
    if(draw) {
      endX <- x+sin((senseDir + dir) * pi) * (dist +1)
      endY <- y+cos((senseDir + dir) * pi) * (dist + 1)
      sLine <- matrix(c(x,y,endX,endY),ncol=2, byrow=TRUE)
      SenseLine(sLine,draw)
    }

  }
  
  return(dVec)
  
  #return(dVec[1:(length(dVec)/2+1)])
  
}

PlotSenseLine <- function(dVec) {
  lData <- data.frame()
  for(eNr in 1:length(dVec)) {
    lData <- rbind(lData,c(eNr * 10, dVec[eNr] + 110))
    lines(lData,type="l")
    points(eNr * 10, 110)
  } 
}

Move <- function(nrSteps) {
  
  # Move
  dir <- dir + dirChange / nrSteps * speed / 10 *0.05
  x <- x + sin(dir*pi) * speed / nrSteps / 10
  y <- y + cos(dir*pi) * speed / nrSteps / 10
 
  # Assign values to global environment
  assign("dir",dir,envir = .GlobalEnv)
  assign("x",x,envir = .GlobalEnv)
  assign("y",y,envir = .GlobalEnv)

}


  



