#Clear Environment
rm(list=ls())

############################################ PROGRAM DESCRIPTION
# The purpose of this program is to determine flow speed from the drifter's GPS coordinates

#-------------------------------------------------------------------------------------------------------------------------------------
# Front Matter - PACKAGES
# geosphere for haversine formula
library(geosphere)

#-------------------------------------------------------------------------------------------------------------------------------------
# Session Tool Information: 
options(width = 100)
devtools::session_info()

#-------------------------------------------------------------------------------------------------------------------------------------
# Front Matter - PATHS
setwd("C:/Users/julia/Documents/Mitchell's lab CGCS internship/Flow_data")

#-------------------------------------------------------------------------------------------------------------------------------------
# Front Matter - FUNCTIONS
# haversine formula used to calculate distance between two points on a sphere (Earth)

#-------------------------------------------------------------------------------------------------------------------------------------
# Front Matter - DATA
# In path: 

#May27, 2021 field trip
data <- read.csv("Manitouwabing_River_Inlet_May27_2021.csv")

#-------------------------------------------------------------------------------------------------------------------------------------
######################################################################################################################################
#-------------------------------------------------------------------------------------------------------------------------------------
# Begin Program Area
#-------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------

#extract coordinates from data file
coord <- data[,c(6,5)]

#convert time into xx:xx:xx format
time <- data[,4]
i<- 1
for (i in 1:length(time)){
t <- sub("\\s+$", "", gsub('(.{2})', '\\1 ', time))
t <- gsub(" ", ":", t)
}

#convert date into yy-mm-da format
date <- data[,3]
j <- 1
for (j in 1:length(date)){
  d <- sub("\\s+$", "", gsub('(.{2})', '\\1 ', date))
  d <- gsub(" ", "-", d)
}

k <- 1
v <- c()
for (k in 1:(nrow(coord)-1)){
  #determine time difference
  t1 <- paste(d[k], t[(k)], sep = " ")
  t2 <- paste(d[k+1], t[(k+1)], sep= " ")
  timediff <- difftime(t2,t1, units = "secs")
  timediff <- as.numeric(timediff)
  
  #determine distance between points 
  dist <- distHaversine(coord[k,], coord[(k+1),])
  
  v[k] <- dist/timediff
  
}

average_flow <- mean(v)




