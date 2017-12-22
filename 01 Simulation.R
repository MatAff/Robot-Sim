
# Load source and packages
library(sp)
library(rgeos)
source("SourceCourse.R")
source("SourceSenseMove.R")
source("SourceAction.R")

# Generate course, get spatial lines for sensing and draw
lineList <- GenerateCourse(0.75)
spacialLines <- GetSpacialLines(lineList)
DrawCourse(lineList)

# Set start position and draw
x <- 18
y <- 50
dir <- 0 # 0/2 are up
points(x,y)

# Start movement
dirChange <- 0 # change/meter (2 = full circle)
speed <- 4 # meter/sec 

# Sense
dVec <- SenseAndPlot(20,TRUE); print(dVec)
PlotSenseLine(dVec)

# Loop
for(i in 1:500) {
  
  # Plot course and position
  DrawCourse(lineList)
  points(x,y)
  
  # Sense
  dVec <- SenseAndPlot(20,TRUE); 
  
  # Move
  Move(1)
  
  # Action
  action <- getActionOuter(dVec, speed)
    dirChange <- action$dirChange
    speed <- action$speed
  
  # Pause (seconds)
  Sys.sleep(0.05)
  
}
