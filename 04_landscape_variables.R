#04_landscape_variables.R
#This script was written by J. Atkins 2015, 2016 (jeffatkins@virginia.edu or @atkinsjeff)
#https://github.com/atkinsjeff
#
#This script combines elevation DEM data and distance-from-stream data to calcualte relationships
#with these landscape variables and
require(plyr)
require(dplyr)
require(magrittr)

#importing data
elev_classes <- read.csv(file = "data/elevation_classes.txt", sep=",", skip = 0, colClasses=c("NULL", "NULL", NA, NA, NA))
colnames(elev_classes) <- c("x", "y", "elev")

#
table(elev_classes$elev)
     


elev_classes$elev <- as.character(elev_classes$elev)

elev_classes$elev[elev_classes$elev == "1"] <- "LOW"
elev_classes$elev[elev_classes$elev == "2"] <- "MID"
elev_classes$elev[elev_classes$elev == "3"] <- "HIGH"
elev_classes$elev[elev_classes$elev == "0"] <- NA

#raw elevation data in meters
elev_raw <- read.csv(file = "data/elevation_raw.txt", sep=",", skip = 0, colClasses=c("NULL", "NULL", NA, NA, NA))
colnames(elev_raw) <- c("x", "y", "elev.raw")


############# Weimer Stream Distances

stream_distance <- read.csv(file = "data/weimer_distance2.txt", sep =",", skip = 0, colClasses=c("NULL", "NULL", NA, NA, NA))
colnames(stream_distance) <- c("x", "y", "stream_dist")
# 
# stream_distance$x <- stream_distance$x - 5.7
# stream_distance$y <- stream_distance$y - 15


######Importing aspect as this is probably important if you look at where the changes are occuring.

weimer_aspect <- read.csv(file = "data/weimer_aspect.txt",, sep=",", skip = 0, colClasses=c("NULL", "NULL", NA, NA, NA))
colnames(weimer_aspect) <- c("x", "y", "aspect") 


#classifying the aspect by direction for statistics later
  weimer_aspect$aspect.class[weimer_aspect$aspect ==-1] <- "FLAT"
  weimer_aspect$aspect.class[weimer_aspect$aspect >= 0 & weimer_aspect$aspect  <= 22.5] <- "North"
  weimer_aspect$aspect.class[weimer_aspect$aspect >= 22.51 & weimer_aspect$aspect  <= 67.5] <- "Northeast"
  weimer_aspect$aspect.class[weimer_aspect$aspect >= 67.51 & weimer_aspect$aspect  <= 112.5] <- "East"
  weimer_aspect$aspect.class[weimer_aspect$aspect >= 112.51 & weimer_aspect$aspect  <= 157.5] <- "Southeast"
  weimer_aspect$aspect.class[weimer_aspect$aspect >= 157.51 & weimer_aspect$aspect  <= 202.5] <- "South"
  weimer_aspect$aspect.class[weimer_aspect$aspect >= 202.51 & weimer_aspect$aspect  <= 247.5] <- "Southwest"
  weimer_aspect$aspect.class[weimer_aspect$aspect >= 247.51 & weimer_aspect$aspect  <= 292.5] <- "West"
  weimer_aspect$aspect.class[weimer_aspect$aspect >= 292.51 & weimer_aspect$aspect  <= 337.5] <- "Northwest"
  weimer_aspect$aspect.class[weimer_aspect$aspect >= 337.51 & weimer_aspect$aspect  <= 360] <- "North"

weimer_aspect$aspect.class <-as.factor(weimer_aspect$aspect.class)  

#Making NDVI merge file with classes and stream distance
class.ndvi.86.11 <- merge(df.86.11, stream_distance, by = c("x", "y"))
class.ndvi.86.11 <- merge(class.ndvi.86.11, elev_classes, by = c("x", "y"))
class.ndvi.86.11 <- merge(class.ndvi.86.11, weimer_aspect, by = c("x", "y"))
class.ndvi.86.11 <- merge(class.ndvi.86.11, elev_raw, by = c("x", "y"))

class.ndvi.86.11$elev <-as.factor(class.ndvi.86.11$elev)
class_ndvi <- class.ndvi.86.11

class_ndvi$elev.raw[class_ndvi$elev.raw == 0] <- NA


#colnames(class_ndvi) <- c("x", "y", "ndvi.86", "ndvi.11", "diff", "stream_dist", "elev", "aspect", "aspect.class", "elev.raw")


# #reordering elevation classes 
#class_ndvi$elev <- factor(class.ndvi.86.11$elev, levels = c("low", "mid","high"), labels = c("LOW", "MID", "HIGH"))

