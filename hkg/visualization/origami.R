# Libraries
library(ggplot2)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(pracma)

# Parameters
# Session -> set working directory -> source file location

filePath <- "folds/"
fileName <- "10.txt"
fullPath <- paste(filePath, fileName, sep = "")

# Read in raw data
rawData <- read_lines(file = fullPath)
polys <- rawData[1] %>% as.integer()
numVertex <- rawData[2] %>% as.integer()
# Raw vertices
verticesRaw <- rawData[1:numVertex + 2] %>% 
  as.list() %>% 
  map(~str_split(.x, ",")) %>% 
  flatten() %>%
  map(~str_split(.x, "/")) %>% 
  flatten() %>%
  map(~as.integer(.x))
numSegs <- rawData[numVertex + 3] %>% as.integer()
# Raw line segments
s <- numVertex + 4
e <- numVertex + 4 + numSegs - 1
segmentsRaw <- rawData[s:e] %>% 
  as.list() %>% 
  map(~str_split(.x, " ")) %>% 
  flatten() %>% 
  map(~str_split(.x, ",")) %>% 
  flatten() %>% 
  map(~str_split(.x, "/")) %>% 
  flatten() %>% 
  map(~as.integer(.x))

# Process as fractions
vertNum <- verticesRaw %>% 
  map(~.x[1]) %>% 
  unlist() %>% 
  as.vector()
vertDenom <- verticesRaw %>% map(~.x[2]) %>% 
  map(~ifelse(is.na(.x), 1, .x)) %>% 
  map(~as.integer(.x))
segNum <- segmentsRaw %>% 
  map(~.x[1]) %>% 
  unlist() %>% 
  as.vector()
segDenom <- segmentsRaw %>% map(~.x[2]) %>% 
  map(~ifelse(is.na(.x), 1, .x)) %>% 
  map(~as.integer(.x))

# Find least common multiple
lcmVert <- vertDenom %>% reduce(Lcm)
lcmSeg <- segDenom %>% reduce(Lcm)
lcm <- Lcm(lcmVert, lcmSeg)

# Create scales LCM by denominators
vertScales <- vertDenom %>% 
  map(~lcm / .x) %>% 
  unlist() %>% 
  as.vector()
segScales <- segDenom %>% 
  map(~lcm / .x) %>% 
  unlist() %>% 
  as.vector()

# Scaled vertices and segments
scaledVert <- vertScales * vertNum
scaledSeg <- segScales * segNum

# Find max and min for scaling plot
maxVert <- scaledVert %>% max()
maxSeg <- scaledSeg %>% max()
maxAll <- max(maxVert, maxSeg)
minVert <- scaledVert %>% min()
minSeg <- scaledSeg %>% min()
minAll <- min(minVert, minSeg) %>% min(0)

# Convert to a vertices data frame
xVert <- scaledVert[seq(1, length(scaledVert), 2)]
yVert <- scaledVert[seq(2, length(scaledVert), 2)]
vert_df <- data.frame(xVert = xVert, yVert = yVert)

# Convert to a line segment data frame
x1 <- scaledSeg[seq(1, length(scaledSeg), 4)]
y1 <- scaledSeg[seq(2, length(scaledSeg), 4)]
x2 <- scaledSeg[seq(3, length(scaledSeg), 4)]
y2 <- scaledSeg[seq(4, length(scaledSeg), 4)]
seg_df <- data.frame(x1 = x1, y1 = y1, x2 = x2, y2 = y2)

# Plot
plot <- vert_df %>% 
  ggplot() + 
  geom_point(mapping = aes(x = xVert, y = yVert, color = "red")) + 
  geom_segment(mapping = aes(x = x1, y = y1, xend = x2, yend = y2, color = "blue"), 
               data = seg_df) + 
  scale_x_continuous(limits = c(minAll, maxAll)) + 
  scale_y_continuous(limits = c(minAll, maxAll))

# not scaled plot
unscaledVert <- vertNum / (vertDenom %>% unlist() %>% as.vector())
unscaledSeg <- segNum / (segDenom %>% unlist() %>% as.vector())

maxVertUS <- unscaledVert %>% max()
maxSegUS <- unscaledSeg %>% max()
maxAllUS <- max(maxVertUS, maxSegUS) %>% max(1)
minVertUS <- unscaledVert %>% min()
minSegUS <- unscaledSeg %>% min()
minAllUS <- min(minVertUS, minSegUS) %>% min(0)

# Convert to a vertices data frame
xVertUS <- unscaledVert[seq(1, length(unscaledVert), 2)]
yVertUS <- unscaledVert[seq(2, length(unscaledVert), 2)]
vertUS_df <- data.frame(xVertUS = xVertUS, yVertUS = yVertUS)

# Convert to a line segment data frame
x1US <- unscaledSeg[seq(1, length(unscaledSeg), 4)]
y1US <- unscaledSeg[seq(2, length(unscaledSeg), 4)]
x2US <- unscaledSeg[seq(3, length(unscaledSeg), 4)]
y2US <- unscaledSeg[seq(4, length(unscaledSeg), 4)]
segUS_df <- data.frame(x1US = x1US, y1US = y1US, x2US = x2US, y2US = y2US)

# Plot
plotUS <- vertUS_df %>% 
  ggplot() + 
  geom_point(mapping = aes(x = xVertUS, y = yVertUS, color = "red")) + 
  geom_segment(mapping = aes(x = x1US, y = y1US, xend = x2US, yend = y2US, color = "blue"), 
               data = segUS_df) + 
  scale_x_continuous(limits = c(minAllUS, maxAllUS)) + 
  scale_y_continuous(limits = c(minAllUS, maxAllUS))

# calculate shift to normalized position

maxVertX <- vertUS_df %>% max(xVertUS)
maxSegX <- segUS_df %>% max(x1US, x2US)
maxVertY <- vertUS_df %>% max(yVertUS)
maxSegY <- segUS_df %>% max(y1US, y2US)

shiftX <- 1 - max(maxVertX, maxSegX)
shiftY <- 1 - max(maxVertY, maxSegY)

# Plot
plotShift <- vertUS_df %>% 
  ggplot() + 
  geom_point(mapping = aes(x = xVertUS + shiftX, 
                           y = yVertUS + shiftY, 
                           color = "red")) + 
  geom_segment(mapping = aes(x = x1US + shiftX, 
                             y = y1US + shiftY, 
                             xend = x2US + shiftX, 
                             yend = y2US + shiftY, 
                             color = "blue"), 
               data = segUS_df) + 
  scale_x_continuous(limits = c(minAllUS, maxAllUS)) + 
  scale_y_continuous(limits = c(minAllUS, maxAllUS))

#Show plots
plot
plotUS
plotShift