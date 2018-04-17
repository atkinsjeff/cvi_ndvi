require(rgdal)
require(RStoolbox)
require(raster)

dummy <- raster("./data/training_data.tif")


dummy[1,1] <- 0
dummy[1,2] <- 0.1
dummy[1,3] <- 0.2
dummy[1,4] <- 0.3
dummy[1,5] <- 0.4
dummy[1,6] <- 0.5
dummy[1,7] <- 0.6
dummy[1,8] <- 0.7
dummy[1,9] <- 0.8

dummy$training_data <- ifelse(dummy$training_data>0.6, NA)

writeRaster(dummy, filename = "dummy.tif")

r <- raster(ncol = 4, nrow = 2)
r[1,1] <- 0
r[1,2] <- 0.1
r[1,3] <- 0.2
r[1,4] <- 0.3
r[2,1] <- 0.4
r[2,2] <- 0.5
r[2,3] <- 0.6
r[2,4] <- 0.7

dummy <- r
writeRaster(dummy, filename = "dummy2.tif", overwrite = TRUE)