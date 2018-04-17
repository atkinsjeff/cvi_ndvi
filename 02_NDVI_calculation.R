# Atkins redo of CVI paper on NDVI change using R
# 
# Band 1 Visible (0.45 - 0.52 µm) 30 m.  BLUE
# Band 2 Visible (0.52 - 0.60 µm) 30 m.  GREEN
# Band 3 Visible (0.63 - 0.69 µm) 30 m.  RED
# Band 4 Near-Infrared (0.76 - 0.90 µm) 30 m. NIR
# Band 5 Near-Infrared (1.55 - 1.75 µm) 30 m. SWIR
# Band 6 Thermal (10.40 - 12.50 µm) 120 m. THERMAL IR
# Band 7 SWIR (2.09 - 2.35) 30 m 
# https://landsat.usgs.gov/what-are-best-spectral-bands-use-my-study
library(maptools)  ## For wrld_simpl
library(raster)
library(rgdal)


list.86 <- list.files(path = "data/", pattern = "1986.*\\.tif$", full.names  = TRUE)
list.90 <- list.files(path = "data/", pattern = "1990.*\\.tif$", full.names  = TRUE)
list.95 <- list.files(path = "data/", pattern = "1995.*\\.tif$", full.names  = TRUE)
list.05 <- list.files(path = "data/", pattern = "2005.*\\.tif$", full.names  = TRUE)
list.09 <- list.files(path = "data/", pattern = "2009.*\\.tif$", full.names  = TRUE)
list.11 <- list.files(path = "data/", pattern = "2011.*\\.tif$", full.names  = TRUE)


plot(raster(list.86[1]))

stack.86 <- stack(list.86) 
stack.90 <- stack(list.90)
stack.95 <- stack(list.95)
stack.05 <- stack(list.05)
stack.09 <- stack(list.09)
stack.11 <- stack(list.11)

plotRGB(stack.11, r = 3, g = 2, b = 1, axes = TRUE, stretch = 'lin')

stack.86
     

# NDVI uses band 4 of the data (NIR) and band 3 (the red)
ndvi.calc <- function(x) {
     (x[[4]] - x[[3]]) / (x[[4]] + x[[3]])
}

ndvi.86 <- ndvi.calc(stack.86)
ndvi.90 <- ndvi.calc(stack.90)
ndvi.95 <- ndvi.calc(stack.95)
ndvi.05 <- ndvi.calc(stack.05)
ndvi.09 <- ndvi.calc(stack.09)
ndvi.11 <- ndvi.calc(stack.11)




# Coefficents
# 
# 

# http://ceholden.github.io/open-geo-tutorial/R/chapter_3_visualization.html
# from Crist 1985
#brightness
bright.calc <- function(x) {
     (x[[1]] * 0.2043) + (x[[2]] * 0.4158) + (x[[3]] * 0.5524) + 
          (x[[4]] * 0.5741) + (x[[5]] + 0.3124) + (x[[6]] * 0.2330 )
}

bright.86 <- bright.calc(stack.86)
bright.90 <- bright.calc(stack.90)
bright.95 <- bright.calc(stack.95)
bright.05 <- bright.calc(stack.05)
bright.09 <- bright.calc(stack.09)
bright.11 <- bright.calc(stack.11)

plot(bright.86)
plot(bright.11)

#greenness
green.calc <- function(x) {
     (x[[1]] * -0.1603) + (x[[2]] * -0.2189) + (x[[3]] * -0.4934) + 
          (x[[4]] * 0.7940) + (x[[5]] + -0.0002) + (x[[6]] * -0.1446 )
}

green.86 <- green.calc(stack.86)
green.90 <- green.calc(stack.90)
green.95 <- green.calc(stack.95)
green.05 <- green.calc(stack.05)
green.09 <- green.calc(stack.09)
green.11 <- green.calc(stack.11)

plot(green.86)
plot(green.11)

#wetnness
wet.calc <- function(x) {
     (x[[1]] * 0.0315) + (x[[2]] * 0.2021) + (x[[3]] * 0.3102) + 
          (x[[4]] * 0.1954) + (x[[5]] + -0.6806) + (x[[6]] * -0.6109 )
}

wet.86 <- wet.calc(stack.86)
wet.90 <- wet.calc(stack.90)
wet.95 <- wet.calc(stack.95)
wet.05 <- wet.calc(stack.05)
wet.09 <- wet.calc(stack.09)
wet.11 <- wet.calc(stack.11)

plot(wet.86)
plot(wet.11)


#write them rasters
writeRaster(evi.86, filename = "evi_1986.tif", format = "GTiff", overwrite = TRUE)
writeRaster(evi.11, filename = "evi_2011.tif", format = "GTiff", overwrite = TRUE)
writeRaster(savi.86, filename = "savi_1986.tif", format = "GTiff", overwrite = TRUE)
writeRaster(savi.11, filename = "savi_2011.tif", format = "GTiff", overwrite = TRUE)
writeRaster(green.86, filename = "green_1986.tif", format = "GTiff", overwrite = TRUE)
writeRaster(green.11, filename = "green_2011.tif", format = "GTiff", overwrite = TRUE)
writeRaster(wet.86, filename = "wet_1986.tif", format = "GTiff", overwrite = TRUE)
writeRaster(wet.11, filename = "wet_2011.tif", format = "GTiff", overwrite = TRUE)
writeRaster(bright.86, filename = "bright_1986.tif", format = "GTiff", overwrite = TRUE)
writeRaster(bright.11, filename = "bright_2011.tif", format = "GTiff", overwrite = TRUE)