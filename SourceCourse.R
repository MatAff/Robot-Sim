
# Function to generate a (partial) circle
GenerateCircle <- function(cx, cy, radius, nrSteps, start=NULL, end=NULL) {
  lData <- data.frame()
  if(is.null(start)) { start <- 0 }
  if(is.null(end)) { end <- 2 }
  seqNrs <- seq(start, end, 2/nrSteps)
  for(s in seqNrs) {
    x <- cx + round(sin(s*pi) * radius,5)
    y <- cy + round(cos(s*pi) * radius,5)
    lData <- rbind(lData, data.frame(x,y))
  }
  return(lData)
}

# Function to generate V shape (horizontal/vertical) to fill in gaps
GenerateV <- function(x1,y1,x2,y2) {
  if(x1==x2) {
    lData <- matrix(c(x1,y1,x1+(y2-y1)/2,mean(c(y1,y2)),x2,y2), ncol=2, byrow=TRUE)
  } 
  if(y1==y2) {
    lData <- matrix(c(x1,y1,mean(c(x1,x2)),y1+(x2-x1)/2,x2,y2), ncol=2, byrow=TRUE)
  }
  return(lData)
}

# Add error to points in line list
AddError <- function(lineList,sdVal=0) {
  for(lNr in 1:length(lineList)) {
    for(rNr in 2:(nrow(lineList[[lNr]])-1)) {
      lineList[[lNr]][rNr,1] <- lineList[[lNr]][rNr,1] + rnorm(1,0,sdVal)
      lineList[[lNr]][rNr,2] <- lineList[[lNr]][rNr,2] + rnorm(1,0,sdVal)
    }
  }
  return(lineList)
}

# Generate course
GenerateCourse <- function(errorSD=0) {
  leftOuter <- GenerateCircle(50,50, 40, 16, 0.75, 2.25)
  rightOuter <- GenerateCircle(140,50, 40, 16, 1.75, 3.25)
  leftInner <- GenerateCircle(50,50, 24, 16, 0.75, 2.25)
  rightInner <- GenerateCircle(140,50, 24, 16, 1.75, 3.25)
  leftV <- GenerateV(leftInner[1,1],leftInner[1,2],leftInner[nrow(leftInner),1],leftInner[nrow(leftInner),2])
  rightV <- GenerateV(rightInner[1,1],rightInner[1,2],rightInner[nrow(rightInner),1],rightInner[nrow(rightInner),2])
  topV <- GenerateV(rightOuter[1,1],rightOuter[1,2],leftOuter[nrow(leftOuter),1],leftOuter[nrow(leftOuter),2])
  bottomV <- GenerateV(leftOuter[1,1],leftOuter[1,2],rightOuter[nrow(rightOuter),1],rightOuter[nrow(rightOuter),2])
  
  # Creat line list
  lineList <- list(leftOuter, rightOuter, leftInner, rightInner, leftV, rightV, topV, bottomV)
  
  # Add error
  lineList <- AddError(lineList, errorSD)
  
  # Return
  return(lineList)
}

# Function to draw course
DrawCourse <- function(lineList, lim=200) {
  plot.new()
  par(mar=c(0,0,0,0))
  plot.window( xlim=c(0,lim), ylim=c(0,lim), asp = 1)
  lapply(lineList,lines,type="l")
}

# Function to convert lines to spacial lines for detection
GetSpacialLines <- function(lineList) {
  lineList <- lapply(lineList,Line)
  return(SpatialLines(list(Lines(lineList,ID="1"))))
}



