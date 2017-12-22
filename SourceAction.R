
# Statement relating to space left, right
PosStatement <- function(dVec) {
  return(paste("Left:",round(dVec[1],2),"Right:",round(dVec[length(dVec)],2)))
}

# Statement relating beta values
BetaPosStatement <- function(dVec, leftBeta, rightBeta) {
  beta <- paste("LeftBeta:",round(leftBeta,2),"RightBet",round(rightBeta,2))
  return(paste(PosStatement(dVec),beta))
}

# Statement relating to direction change


# Get max dir
GetMaxDir <- function(dVec) {
  maxDir <- seq(-0.5,0.5,0.1)[dVec==max(dVec)]; print(maxDir)
  return(maxDir[1])
}

# Compute beta
ComputeBeta <- function(a,b,alpha) {
  b1 <- cos(alpha*pi) * b
  b2 <- sin(alpha*pi) * b
  beta <- atan((a-b1)/b2) / pi
  return(beta)
}

# Action
getActionOuter <- function(dVec, speed) {
  midVal <- length(dVec) / 2 + 0.5
  reqDist <- 4
  leftBeta <- ComputeBeta(dVec[1],dVec[4],0.3)
  rightBeta <- ComputeBeta(dVec[11],dVec[8],-0.3)
  #print(BetaPosStatement(dVec,leftBeta,rightBeta))
  dirChange <- 0
  margin <- 0.05
  if(dVec[1]<reqDist) { dirChange <- max(dirChange,leftBeta + margin) }
  if(dVec[11]<reqDist) { dirChange <- min(dirChange,rightBeta - margin) }
  return(list(dirChange=dirChange,speed=speed))
}

GetActionLineFollow <- function(dVec, speed) {
  return(list(dirChange=dVec[3]/15,speed=speed))
}

rlData <- as.data.frame(matrix(999,nrow=500,ncol=2))
names(rlData) <- c("pi", "R")
ppi <- 0.1
c <- 0

GetActionLineFollowRL <- function(dVec, speed) {
  c <- c + 1
  reward <- GetRewardLineFollow(dVec)
  if(c<50) {
    ppi <- ppi + rnorm(1,0,0.02)
  } else {
    ppi <- rlData[rlData[,"R"]==min(rlData[,"R"]),"pi"] + rnorm(1,0,0.01)
  }
  if(c==50) { print(rlData) }
  print(paste(round(c(ppi,reward),2)))
  rlData[c,"R"] <- reward
  rlData[c+5,"pi"] <- ppi
  assign("pi",pi,envir=.GlobalEnv)
  assign("c",c,envir=.GlobalEnv)
  assign("rlData",rlData,envir=.GlobalEnv)
  return(list(dirChange=dVec[3]*ppi,speed=speed))
}

GetRewardLineFollow <- function(dVec) {
  reward <- abs(dVec[1]) + 4 * abs(dVec[2]-dVec[1])
  return(reward)
}


# # Action
# getActionBase <- function(dVec, speed) {
#   midVal <- length(dVec) / 2 + 0.5
#   if(dVec[midVal+1]>dVec[midVal] || dVec[midVal-1]>dVec[midVal]) { 
#     if(dVec[midVal+1] > dVec[midVal-1]) {
#       dirChange <- 0.1
#     } else {
#       dirChange <- -0.1
#     }
#   } else {
#     dirChange <- 0
#   }
#   return(list(dirChange=dirChange,speed=speed))
# }
# 
# 
# # Action
# getActionBasePoint <- function(dVec, speed) {
#   midVal <- length(dVec) / 2 + 0.5
#   reqVal <- 8
#   if(dVec[1+1]<reqVal) { dVec[1:midVal] <- 0 }
#   if(dVec[length(dVec)-1]<reqVal) { dVec[midVal:length(dVec)] <- 0 }
#   if(dVec[midVal+1]>dVec[midVal] || dVec[midVal-1]>dVec[midVal]) { 
#     if(dVec[midVal+1] > dVec[midVal-1]) { 
#       dirChange <- 0.1
#     } else {
#       dirChange <- -0.1
#     }
#   } else {
#     dirChange <- 0
#   }
#   return(list(dirChange=dirChange,speed=speed))
# }
# 
# 
# # Action
# getActionBeta <- function(dVec, speed) {
#   midVal <- length(dVec) / 2 + 0.5
#   reqDist <- 4
#   leftBeta <- ComputeBeta(dVec[1],dVec[2],0.1)
#   rightBeta <- ComputeBeta(dVec[11],dVec[10],-0.1)
#   print(BetaPosStatement(dVec,leftBeta,rightBeta))
#   if(dVec[midVal+1]>dVec[midVal] || dVec[midVal-1]>dVec[midVal]) { 
#     if(dVec[midVal+1] > dVec[midVal-1]) { 
#       dirChange <- 0.1
#     } else {
#       dirChange <- -0.1
#     }
#   } else {
#     dirChange <- 0
#   }
#   if(dVec[1]<reqDist) { dirChange <- max(dirChange,leftBeta) }
#   if(dVec[11]<reqDist) { dirChange <- min(dirChange,rightBeta) }
#   return(list(dirChange=dirChange,speed=speed))
# }
# 
# # Action
# getActionBetaMaxDir <- function(dVec, speed) {
#   midVal <- length(dVec) / 2 + 0.5
#   reqDist <- 4
#   leftBeta <- ComputeBeta(dVec[1],dVec[2],0.1)
#   rightBeta <- ComputeBeta(dVec[11],dVec[10],-0.1)
#   print(BetaPosStatement(dVec,leftBeta,rightBeta))
#   dirChange <- GetMaxDir(dVec)
#   if(dVec[1]<reqDist) { dirChange <- max(dirChange,leftBeta) }
#   if(dVec[11]<reqDist) { dirChange <- min(dirChange,rightBeta) }
#   return(list(dirChange=dirChange,speed=speed))
# }
