
###########################
### MAX DISTANCE METHOD ###
###########################

# Get max dir
UseMaxDist <- function(dVec,speed) {
  maxDist <- max(dVec)
  posNr <- c(1:length(dVec))[dVec==maxDist][1]
  dirChange <- seq(-0.5,0.5,1/(length(dVec)-1))[posNr]
  return(list(dirChange=dirChange,speed=speed))
}

###############################
### FOLLOW THE GAP FUNCTION ###
###############################

# Function that follows the gap
UseGaps <- function(dVec,speed) {
  gaps <- FindGaps(dVec, 30)
  bestGap <- BestGap(gaps)
  #print(gaps)
  #print(bestGap)
  #print(bestGap$heading)
  return(list(dirChange=bestGap$heading,speed=speed))
}

# Function to find gaps at a certain distance
FindGaps <- function(dVec, dist) {

  gaps <- data.frame()
  lastIndex <- 1
  
  # Index to dir function
  IndexToDir <- function(index, dVec) {
    seq(-0.5,0.5,1/(length(dVec)-1))[index]
  }
  
  # Loop and find gaps
  for(i in 1:length(dVec)) {
    if(dVec[i] < dist) {
      if(i - lastIndex > 1) {
        heading <- (IndexToDir(i,dVec) + IndexToDir(lastIndex,dVec)) / 2
        width <- IndexToDir(i,dVec) - IndexToDir(lastIndex,dVec)
        gaps <- rbind(gaps,c(heading,width))
      }
      lastIndex <- i 
    }
  }
  
  # Add final gap if present
  if(i - lastIndex > 1) {
    heading <- (IndexToDir(i,dVec) + IndexToDir(lastIndex,dVec)) / 2
    width <- IndexToDir(i,dVec) - IndexToDir(lastIndex,dVec)
    gaps <- rbind(gaps,c(heading,width))
  }
  
  # Return
  names(gaps) <- c("heading","width")
  return(gaps)
}

# Test data
# gaps <- FindGaps(c(5,5,5,5,15,15,15,6,6,6,12,12,5,20,20),10); gaps

# Function to pick best gap
BestGap <- function(gaps) {
  
  if(nrow(gaps)==1) { 
    return(gaps[1,])
  } else {
    gaps$abs <- abs(gaps[,1])
    gaps$score <- gaps$width - gaps$abs
    return(gaps[which.max(gaps$score),])
  }
  
}



