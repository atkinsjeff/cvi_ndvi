# Atkins redo of CVI paper on NDVI change using R
# 
# 
library(maptools)  ## For wrld_simpl
library(raster)
library(rgdal)

## Example SpatialPolygonsDataFrame
data(wrld_simpl)
SPDF <- subset(wrld_simpl, NAME=="Brazil")

## Example RasterLayer
r <- raster(nrow=1e3, ncol=1e3, crs=proj4string(SPDF))
r[] <- 1:length(r)

## crop and mask
r2 <- crop(r, extent(SPDF))
r3 <- mask(r2, SPDF)

## Check that it worked
plot(r3)
plot(SPDF, add=TRUE, lwd=2)




###
# list.files('~/GIS/', pattern='\\.sh$')
# basin <- shapefile("~/GIS/wymerbasin")

basin <- shapefile("C:/github/cvi_ndvi_redux/GIS/wymerbasin")


r1 <- raster("data/LT50170331986083XXX04_sr_band1.tif")

r2 <- crop(r1, extent(basin))
r23 <- mask(r2, basin)
#Looking at files in data folder

list.files("data", pattern='\\.tif')
