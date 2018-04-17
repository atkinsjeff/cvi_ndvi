###########

require(raster)
require(maptool)
require(rgdal)
require(plyr)
require(dplyr)

#r <- stack("./data/1986083_band2.tif", "./data/1986083_band3.tif", "./data/1986083_band4.tif")
 s <- stack("./data/b2011_band1.tif", "./data/b2011_band2.tif", "./data/b2011_band3.tif", "./data/b2011_band4.tif", "./data/b2011_band5.tif", "./data/b2011_band7.tif")
s <- brick(s)
# 

 r <- addLayer(s, wet.11, bright.11, green.11)
 
 r <- calc(r, fun = function(x) x /10000)
pairs(r)

covs.11 <- addLayer(r, ndvi.11)

#rename training data

names(covs.11) <- c("band1", "band2", "band3", "band4", "band5", "band7", "wetness", "greenness", "brightness", "ndvi")

plot(covs.11)

train <- shapefile("./GIS/trainingpolyutm6.shp")

plot(ndvi.11)
plot(train, add = TRUE)

train@data$Name <- as.factor(train@data$Name)
train@data$code <- as.numeric(train@data$Name)

## Assign 'Code' values to raster cells (where they overlap)
classes.11 <- rasterize(train, ndvi.11, field='code')

## Plotting
# Define a colour scale for the classes (as above)
# corresponding to: cropland, forest, wetland
cols <- c("orange", "dark green", "light blue", "grey", "blue")
## Plot without a legend
plot(classes.11, col=cols, legend=FALSE)
## Add a customized legend
legend("topright", legend=c( "forest", "grass", "rock", "shrub", "spruce"), fill=cols, bg="white")

covmasked.11 <- raster::mask(covs.11, classes.11)
plot(covmasked.11)

## Combine this new brick with the classes layer to make our input training dataset
names(classes.11) <- "class"
trainingbrick.11<- addLayer(covmasked.11, classes.11)
plot(trainingbrick.11)

## Extract all values into a matrix
valuetable.11 <- getValues(trainingbrick.11)

valuetable.11 <- na.omit(valuetable.11)

valuetable.11 <- as.data.frame(valuetable.11)
head(valuetable.11, n = 10)
tail(valuetable.11, n = 10)

valuetable.11$class <- factor(valuetable.11$class, levels = c(1:5))

val_forest <- subset(valuetable.11, class == 1)
val_grass <- subset(valuetable.11, class == 2)
val_rock <- subset(valuetable.11, class == 3)
val_shrub <- subset(valuetable.11, class == 4)
val_spruce <- subset(valuetable.11, class == 5)

## 1. NDVI
par(mfrow = c(5, 1))
hist(val_forest$ndvi, main = "forest", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 1), col = "orange")
hist(val_grass$ndvi, main = "grass", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 1), col = "dark green")
hist(val_rock$ndvi, main = "rock", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "light blue")
hist(val_shrub$ndvi, main = "shrub", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "grey")
hist(val_spruce$ndvi, main = "spruce", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "blue")

# ## 3. Bands 3 and 4 (scatterplots)
# plot(b2011_band4 ~ b2011_band3, data = val_shrub)  , pch = ".", col = "grey")
# points(band4 ~ band3, data = val_forest, pch = ".", col = "dark green")
# points(band4 ~ band3, data = val_wetland, pch = ".", col = "light blue")
# legend("topright", legend=c("cropland", "forest", "wetland"), fill=c("orange", "dark green", "light blue"), bg="white")


## Construct a random forest model
# Covariates (x) are found in columns 1 to 5 of valuetable
# Training classes (y) are found in the 'class' column of valuetable
## Caution: this step takes fairly long!
# but can be shortened by setting importance=FALSE
require(randomForest)
modelRF.11 <- randomForest(x=valuetable.11[ ,c(1:10)], y=valuetable.11$class,
                        importance = TRUE, ntree = 2000)


## Inspect the structure and element names of the resulting model
modelRF.11
class(modelRF.11)
str(modelRF.11)
names(modelRF.11)
## Inspect the confusion matrix of the OOB error assessment
modelRF.11$confusion
# to make the confusion matrix more readable
colnames(modelRF.11$confusion) <- c("forest", "grass", "rock", "shrub", "spruce", "class.error")
rownames(modelRF.11$confusion) <- c("forest", "grass", "rock", "shrub", "spruce")
modelRF.11$confusion

varImpPlot(modelRF.11)

## Double-check layer and column names to make sure they match
names(covs.11)
names(valuetable.11)

## Predict land cover using the RF model
pred11 <- raster::predict(covs.11, model=modelRF.11, na.rm=TRUE)

## Plot the results
# recall: 1 = cropland, 2 = forest, 3 = wetland
cols <- c("#EAB674","#B4F087", "#C1C5D9",  "#437D0F", "#1A2407")
plot(pred11, col= cols, legend=FALSE)
legend("bottomleft", 
       legend=c( "forest", "grass", "rock", "shrub", "spruce"), 
       fill= cols, bg="white",
       title = "2011")

#### 1986
q <- stack( "./data/1986083_band1.tif","./data/1986083_band2.tif", "./data/1986083_band3.tif", "./data/1986083_band4.tif", "./data/1986083_band5.tif", "./data/1986083_band7.tif")
q <- brick(q)

q <- addLayer(q, wet.86, bright.86, green.86)

q <- calc(q, fun = function(x) x /10000)
pairs(q)


covs.86 <- addLayer(q, ndvi.86)
names(covs.86) <- c("band1","band2", "band3", "band4", "band5", "band7", "wetness", "brightness", "greenness", "ndvi")

plot(covs.86)

covmasked.86 <- mask(covs.86, classes)
plot(covmasked.86)

##
##plot(ndvi.11)
plot(train, add = TRUE)

## Assign 'Code' values to raster cells (where they overlap)
classes.86 <- rasterize(train, ndvi.86, field='code')

## Plotting
# Define a colour scale for the classes (as above)
# corresponding to: cropland, forest, wetland
cols <- c("orange", "dark green", "light blue", "grey", "blue")
## Plot without a legend
plot(classes.86, col=cols, legend=FALSE)
## Add a customized legend
legend("topright", legend=c("forest", "grass", "rock", "shrub", "spruce"), fill=cols, bg="white")

covmasked.86 <- mask(covs.86, classes.86)
plot(covmasked.86)

## Combine this new brick with the classes layer to make our input training dataset
names(classes.86) <- "class"
trainingbrick.86 <- addLayer(covmasked.86, classes.86)
plot(trainingbrick.86)

## Extract all values into a matrix
valuetable.86 <- getValues(trainingbrick.86)

valuetable.86 <- na.omit(valuetable.86)

valuetable.86 <- as.data.frame(valuetable.86)
head(valuetable.86, n = 10)
tail(valuetable.86, n = 10)

valuetable.86$class <- factor(valuetable.86$class, levels = c(1:5))

val_forest <- subset(valuetable.86, class == 1)
val_grass <- subset(valuetable.86, class == 2)
val_rock <- subset(valuetable.86, class == 3)
val_shrub <- subset(valuetable.86, class == 4)
val_spruce <- subset(valuetable.86, class == 5)

## 1. NDVI
par(mfrow = c(5, 1))
hist(val_forest$ndvi, main = "forest", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 1), col = "orange")
hist(val_grass$ndvi, main = "grass", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 1), col = "dark green")
hist(val_rock$ndvi, main = "rock", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "light blue")
hist(val_shrub$ndvi, main = "shrub", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "grey")
hist(val_spruce$ndvi, main = "spruce", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "blue")

# ## 3. Bands 3 and 4 (scatterplots)
# plot(b2011_band4 ~ b2011_band3, data = val_shrub)  , pch = ".", col = "grey")
# points(band4 ~ band3, data = val_forest, pch = ".", col = "dark green")
# points(band4 ~ band3, data = val_wetland, pch = ".", col = "light blue")
# legend("topright", legend=c("cropland", "forest", "wetland"), fill=c("orange", "dark green", "light blue"), bg="white")


## Construct a random forest model
# Covariates (x) are found in columns 1 to 5 of valuetable
# Training classes (y) are found in the 'class' column of valuetable
## Caution: this step takes fairly long!
# but can be shortened by setting importance=FALSE

set.seed(400)
require(randomForest)
modelRF.86 <- randomForest(x=valuetable.86[ ,c(1:10)], y=valuetable.86$class,
                        importance = TRUE, ntree = 2000)

## Inspect the structure and element names of the resulting model
modelRF.86
class(modelRF.86)
str(modelRF.86)
names(modelRF.86)
## Inspect the confusion matrix of the OOB error assessment
modelRF.86$confusion
# to make the confusion matrix more readable
colnames(modelRF.86$confusion) <- c( "forest", "grass", "rock", "shrub", "spruce", "class.error")
rownames(modelRF.86$confusion) <- c("forest", "grass", "rock", "shrub", "spruce")
modelRF.86$confusion

varImpPlot(modelRF.86)

## Double-check layer and column names to make sure they match
names(covs.86)
names(valuetable.86)


pred86 <- raster::predict(covs.86, model=modelRF.86, na.rm=TRUE)

y <- valuetable.86$class
caret::confusionMatrix(pred86, covs.86)

#plotting 1986
x11()
cols <- c("#EAB674","#B4F087", "#C1C5D9",  "#437D0F", "#1A2407")
plot(pred86, col=cols, legend=FALSE)
legend("bottomleft", 
       legend=c("forest","grass", "rock", "shrub", "spruce"), 
       fill=cols, bg="white",
       title = "1986")

###############################

#### 1990

#r <- stack("./data/1986083_band2.tif", "./data/1986083_band3.tif", "./data/1986083_band4.tif")
s <- stack("./data/b1990_band1.tif", "./data/b1990_band2.tif", "./data/b1990_band3.tif", "./data/b1990_band4.tif", "./data/b1990_band5.tif", "./data/b1990_band7.tif")
s <- brick(s)
# 

r <- addLayer(s, wet.90, bright.90, green.90)

r <- calc(r, fun = function(x) x /10000)
pairs(r)

covs.90 <- addLayer(r, ndvi.90)

#rename trainging data

# names(covs.90) <- c("band2", "band3", "band4", "ndvi")
names(covs.90) <- c("band1", "band2", "band3", "band4", "band5", "band7", "wetness", "greenness", "brightness", "ndvi")

plot(covs.90)

train <- shapefile("./GIS/trainingpolyutm6.shp")

train@data$Name <- as.factor(train@data$Name)
train@data$code <- as.numeric(train@data$Name)

## Assign 'Code' values to raster cells (where they overlap)
classes.90 <- rasterize(train, ndvi.90, field='code')

## Plotting
# Define a colour scale for the classes.90 (as above)
# corresponding to: cropland, forest, wetland
cols <- c("orange", "dark green", "light blue", "grey", "blue")
## Plot without a legend
plot(classes.90, col=cols, legend=FALSE)
## Add a customized legend
legend("topright", legend=c( "forest", "grass", "rock", "shrub", "spruce"), fill=cols, bg="white")

covmasked.90 <- mask(covs.90, classes.90)
plot(covmasked.90)

## Combine this new brick with the classes.90 layer to make our input training dataset
names(classes.90) <- "class"
trainingbrick <- addLayer(covmasked, classes.90)
plot(trainingbrick)

## Extract all values into a matrix
valuetable.90 <- getValues(trainingbrick)

valuetable.90 <- na.omit(valuetable.90.90)

valuetable.90.90 <- as.data.frame(valuetable.90.90)
head(valuetable.90.90, n = 10)
tail(valuetable.90.90, n = 10)

valuetable.90$class <- factor(valuetable.90$class, levels = c(1:5))

val_forest <- subset(valuetable.90, class == 1)
val_grass <- subset(valuetable.90, class == 2)
val_rock <- subset(valuetable.90, class == 3)
val_shrub <- subset(valuetable.90, class == 4)
val_spruce <- subset(valuetable.90, class == 5)

## 1. NDVI
par(mfrow = c(5, 1))
hist(val_forest$ndvi, main = "forest", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 1), col = "orange")
hist(val_grass$ndvi, main = "grass", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 1), col = "dark green")
hist(val_rock$ndvi, main = "rock", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "light blue")
hist(val_shrub$ndvi, main = "shrub", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "grey")
hist(val_spruce$ndvi, main = "spruce", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "blue")

# ## 3. Bands 3 and 4 (scatterplots)
# plot(b2011_band4 ~ b2011_band3, data = val_shrub)  , pch = ".", col = "grey")
# points(band4 ~ band3, data = val_forest, pch = ".", col = "dark green")
# points(band4 ~ band3, data = val_wetland, pch = ".", col = "light blue")
# legend("topright", legend=c("cropland", "forest", "wetland"), fill=c("orange", "dark green", "light blue"), bg="white")


## Construct a random forest model
# Covariates (x) are found in columns 1 to 5 of valuetable.90
# Training classes.90 (y) are found in the 'class' column of valuetable.90
## Caution: this step takes fairly long!
# but can be shortened by setting importance=FALSE
require(randomForest)
modelRF.90 <- randomForest(x=valuetable.90[ ,c(1:10)], y=valuetable.90$class,
                        importance = TRUE, ntree = 2000)


## Inspect the structure and element names of the resulting model
modelRF.90
class(modelRF.90)
str(modelRF.90)
names(modelRF.90)
## Inspect the confusion matrix of the OOB error assessment
modelRF.90$confusion
# to make the confusion matrix more readable
colnames(modelRF.90$confusion) <- c("forest", "grass", "rock", "shrub", "spruce", "class.error")
rownames(modelRF.90$confusion) <- c("forest", "grass", "rock", "shrub", "spruce")
modelRF.90$confusion

varImpPlot(modelRF.90)

## Double-check layer and column names to make sure they match
names(covs.90)
names(valuetable.90)

## Predict land cover using the RF model
pred.90 <- raster::predict(covs.90, model=modelRF.90, na.rm=TRUE)

## Plot the results
# recall: 1 = cropland, 2 = forest, 3 = wetland
cols <- c("#EAB674","#B4F087", "#C1C5D9",  "#437D0F", "#1A2407")
plot(pred.90, col=cols, legend=FALSE)
legend("bottomleft", 
       legend=c( "forest", "grass", "rock", "shrub", "spruce"), 
       fill=cols, bg="white",
       title = "1990")

#######
#1995

#r <- stack("./data/1986083_band2.tif", "./data/1986083_band3.tif", "./data/1986083_band4.tif")
s <- stack("./data/b1995_band1.tif", "./data/b1995_band2.tif", "./data/b1995_band3.tif", "./data/b1995_band4.tif", "./data/b1995_band5.tif", "./data/b1995_band7.tif")
s <- brick(s)
# 

r <- addLayer(s, wet.95, bright.95, green.95)

r <- calc(r, fun = function(x) x /10000)
pairs(r)

covs.95 <- addLayer(r, ndvi.95)

#rename trainging data

# names(covs.95) <- c("band2", "band3", "band4", "ndvi")
names(covs.95) <- c("band1", "band2", "band3", "band4", "band5", "band7", "wetness", "greenness", "brightness", "ndvi")

plot(covs.95)

train <- shapefile("./GIS/trainingpolyutm6.shp")

train@data$Name <- as.factor(train@data$Name)
train@data$code <- as.numeric(train@data$Name)

## Assign 'Code' values to raster cells (where they overlap)
classes.95 <- rasterize(train, ndvi.95, field='code')

## Plotting
# Define a colour scale for the classes.95 (as above)
# corresponding to: cropland, forest, wetland
cols <- c("orange", "dark green", "light blue", "grey", "blue")
## Plot without a legend
plot(classes.95, col=cols, legend=FALSE)
## Add a customized legend
legend("topright", legend=c( "forest", "grass", "rock", "shrub", "spruce"), fill=cols, bg="white")

covmasked.95 <- raster::mask(covs.95, classes.95)
plot(covmasked.95)

## Combine this new brick with the classes.95 layer to make our input training dataset
names(classes.95) <- "class"
trainingbrick <- addLayer(covmasked.95, classes.95)
plot(trainingbrick)

## Extract all values into a matrix
valuetable.95 <- getValues(trainingbrick)

valuetable.95 <- na.omit(valuetable.95)

valuetable.95 <- as.data.frame(valuetable.95)
head(valuetable.95, n = 10)
tail(valuetable.95, n = 10)

valuetable.95$class <- factor(valuetable.95$class, levels = c(1:5))

val_forest <- subset(valuetable.95, class == 1)
val_grass <- subset(valuetable.95, class == 2)
val_rock <- subset(valuetable.95, class == 3)
val_shrub <- subset(valuetable.95, class == 4)
val_spruce <- subset(valuetable.95, class == 5)

## 1. NDVI
par(mfrow = c(5, 1))
hist(val_forest$ndvi, main = "forest", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 1), col = "orange")
hist(val_grass$ndvi, main = "grass", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 1), col = "dark green")
hist(val_rock$ndvi, main = "rock", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "light blue")
hist(val_shrub$ndvi, main = "shrub", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "grey")
hist(val_spruce$ndvi, main = "spruce", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "blue")

# ## 3. Bands 3 and 4 (scatterplots)
# plot(b2011_band4 ~ b2011_band3, data = val_shrub)  , pch = ".", col = "grey")
# points(band4 ~ band3, data = val_forest, pch = ".", col = "dark green")
# points(band4 ~ band3, data = val_wetland, pch = ".", col = "light blue")
# legend("topright", legend=c("cropland", "forest", "wetland"), fill=c("orange", "dark green", "light blue"), bg="white")


## Construct a random forest model
# Covariates (x) are found in columns 1 to 5 of valuetable.95
# Training classes.95 (y) are found in the 'class' column of valuetable.95
## Caution: this step takes fairly long!
# but can be shortened by setting importance=FALSE
modelRF.95 <- randomForest(x=valuetable.95[ ,c(1:10)], y=valuetable.95$class,
                           importance = TRUE, ntree = 2000)


## Inspect the structure and element names of the resulting model
modelRF.95
class(modelRF.95)
str(modelRF.95)
names(modelRF.95)
## Inspect the confusion matrix of the OOB error assessment
modelRF.95$confusion
# to make the confusion matrix more readable
colnames(modelRF.95$confusion) <- c("forest", "grass", "rock", "shrub", "spruce", "class.error")
rownames(modelRF.95$confusion) <- c("forest", "grass", "rock", "shrub", "spruce")
modelRF.95$confusion

varImpPlot(modelRF.95)

## Double-check layer and column names to make sure they match
names(covs.95)
names(valuetable.95)

## Predict land cover using the RF model
pred.95 <- raster::predict(covs.95, model=modelRF.95, na.rm=TRUE)

## Plot the results
# recall: 1 = cropland, 2 = forest, 3 = wetland
cols <- c("#EAB674","#B4F087", "#C1C5D9",  "#437D0F", "#1A2407")
plot(pred.95, col=cols, legend=FALSE)
legend("bottomleft", 
       legend=c( "forest", "grass", "rock", "shrub", "spruce"), 
       fill=cols, bg="white",
       title = "1995")


###############3
#2005

#r <- stack("./data/1986083_band2.tif", "./data/1986083_band3.tif", "./data/1986083_band4.tif")
s <- stack("./data/b2005_band1.tif", "./data/b2005_band2.tif", "./data/b2005_band3.tif", "./data/b2005_band4.tif", "./data/b2005_band5.tif", "./data/b2005_band7.tif")
s <- brick(s)
# 

r <- addLayer(s, wet.05, bright.05, green.05)

r <- calc(r, fun = function(x) x /10000)
pairs(r)

covs.05 <- addLayer(r, ndvi.05)

#rename trainging data

# names(covs.05) <- c("band2", "band3", "band4", "ndvi")
names(covs.05) <- c("band1", "band2", "band3", "band4", "band5", "band7", "wetness", "greenness", "brightness", "ndvi")

plot(covs.05)


## Assign 'Code' values to raster cells (where they overlap)
classes.05 <- rasterize(train, ndvi.05, field='code')

## Plotting
# Define a colour scale for the classes (as above)
# corresponding to: cropland, forest, wetland
cols <- c("orange", "dark green", "light blue", "grey", "blue")
## Plot without a legend
plot(classes.05, col=cols, legend=FALSE)
## Add a customized legend
legend("topright", legend=c( "forest", "grass", "rock", "shrub", "spruce"), fill=cols, bg="white")

covmasked.05 <- raster::mask(covs.05, classes.05)
plot(covmasked.05)

## Combine this new brick with the classes layer to make our input training dataset
names(classes.05) <- "class"
trainingbrick <- addLayer(covmasked.05, classes.05)
plot(trainingbrick)

## Extract all values into a matrix
valuetable.05 <- getValues(trainingbrick)

valuetable.05 <- na.omit(valuetable.05)

valuetable.05 <- as.data.frame(valuetable.05)
head(valuetable.05, n = 10)
tail(valuetable.05, n = 10)

valuetable.05$class <- factor(valuetable.05$class, levels = c(1:5))

val_forest <- subset(valuetable.05, class == 1)
val_grass <- subset(valuetable.05, class == 2)
val_rock <- subset(valuetable.05, class == 3)
val_shrub <- subset(valuetable.05, class == 4)
val_spruce <- subset(valuetable.05, class == 5)

## 1. NDVI
par(mfrow = c(5, 1))
hist(val_forest$ndvi, main = "forest", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 1), col = "orange")
hist(val_grass$ndvi, main = "grass", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 1), col = "dark green")
hist(val_rock$ndvi, main = "rock", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "light blue")
hist(val_shrub$ndvi, main = "shrub", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "grey")
hist(val_spruce$ndvi, main = "spruce", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "blue")

# ## 3. Bands 3 and 4 (scatterplots)
# plot(b2011_band4 ~ b2011_band3, data = val_shrub)  , pch = ".", col = "grey")
# points(band4 ~ band3, data = val_forest, pch = ".", col = "dark green")
# points(band4 ~ band3, data = val_wetland, pch = ".", col = "light blue")
# legend("topright", legend=c("cropland", "forest", "wetland"), fill=c("orange", "dark green", "light blue"), bg="white")


## Construct a random forest model
# Covariates (x) are found in columns 1 to 5 of valuetable.05
# Training classes (y) are found in the 'class' column of valuetable.05
## Caution: this step takes fairly long!
# but can be shortened by setting importance=FALSE
modelRF.05 <- randomForest(x=valuetable.05[ ,c(1:10)], y=valuetable.05$class,
                           importance = TRUE, ntree = 2000)


## Inspect the structure and element names of the resulting model
modelRF.05
class(modelRF.05)
str(modelRF.05)
names(modelRF.05)
## Inspect the confusion matrix of the OOB error assessment
modelRF.05$confusion
# to make the confusion matrix more readable
colnames(modelRF.05$confusion) <- c("forest", "grass", "rock", "shrub", "spruce", "class.error")
rownames(modelRF.05$confusion) <- c("forest", "grass", "rock", "shrub", "spruce")
modelRF.05$confusion

varImpPlot(modelRF.05)

## Double-check layer and column names to make sure they match
names(covs.05)
names(valuetable.05)

## Predict land cover using the RF model
pred.05 <- raster::predict(covs.05, model=modelRF.05, na.rm=TRUE)

## Plot the results
# recall: 1 = cropland, 2 = forest, 3 = wetland
cols <- c("#EAB674","#B4F087", "#C1C5D9",  "#437D0F", "#1A2407")
plot(pred.05, col=cols, legend=FALSE)
legend("bottomleft", 
       legend=c( "forest", "grass", "rock", "shrub", "spruce"), 
       fill=cols, bg="white",
       title = "2005")


##################
# 2009
#r <- stack("./data/1986083_band2.tif", "./data/1986083_band3.tif", "./data/1986083_band4.tif")
s <- stack("./data/b2009_band1.tif", "./data/b2009_band2.tif", "./data/b2009_band3.tif", "./data/b2009_band4.tif", "./data/b2009_band5.tif", "./data/b2009_band7.tif")
s <- brick(s)
# 

r <- addLayer(s, wet.09, bright.09, green.09)

r <- calc(r, fun = function(x) x /10000)
pairs(r)

covs.09 <- addLayer(r, ndvi.09)

#rename trainging data

# names(covs.09) <- c("band2", "band3", "band4", "ndvi")
names(covs.09) <- c("band1", "band2", "band3", "band4", "band5", "band7", "wetness", "greenness", "brightness", "ndvi")

plot(covs.09)

train <- shapefile("./GIS/trainingpolyutm6.shp")

train@data$Name <- as.factor(train@data$Name)
train@data$code <- as.numeric(train@data$Name)

## Assign 'Code' values to raster cells (where they overlap)
classes.09 <- rasterize(train, ndvi.09, field='code')

## Plotting
# Define a colour scale for the classes.09 (as above)
# corresponding to: cropland, forest, wetland
cols <- c("orange", "dark green", "light blue", "grey", "blue")
## Plot without a legend
plot(classes.09, col=cols, legend=FALSE)
## Add a customized legend
legend("topright", legend=c( "forest", "grass", "rock", "shrub", "spruce"), fill=cols, bg="white")

covmasked.09 <- raster::mask(covs.09, classes.09)
plot(covmasked.09)

## Combine this new brick with the classes.09 layer to make our input training dataset
names(classes.09) <- "class"
trainingbrick <- addLayer(covmasked.09, classes.09)
plot(trainingbrick)

## Extract all values into a matrix
valuetable.09 <- getValues(trainingbrick)

valuetable.09 <- na.omit(valuetable.09)

valuetable.09 <- as.data.frame(valuetable.09)
head(valuetable.09, n = 10)
tail(valuetable.09, n = 10)

valuetable.09$class <- factor(valuetable.09$class, levels = c(1:5))

val_forest <- subset(valuetable.09, class == 1)
val_grass <- subset(valuetable.09, class == 2)
val_rock <- subset(valuetable.09, class == 3)
val_shrub <- subset(valuetable.09, class == 4)
val_spruce <- subset(valuetable.09, class == 5)

## 1. NDVI
par(mfrow = c(5, 1))
hist(val_forest$ndvi, main = "forest", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 1), col = "orange")
hist(val_grass$ndvi, main = "grass", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 1), col = "dark green")
hist(val_rock$ndvi, main = "rock", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "light blue")
hist(val_shrub$ndvi, main = "shrub", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "grey")
hist(val_spruce$ndvi, main = "spruce", xlab = "NDVI", xlim = c(0, 1), ylim = c(0,1 ), col = "blue")

# ## 3. Bands 3 and 4 (scatterplots)
# plot(b2011_band4 ~ b2011_band3, data = val_shrub)  , pch = ".", col = "grey")
# points(band4 ~ band3, data = val_forest, pch = ".", col = "dark green")
# points(band4 ~ band3, data = val_wetland, pch = ".", col = "light blue")
# legend("topright", legend=c("cropland", "forest", "wetland"), fill=c("orange", "dark green", "light blue"), bg="white")


## Construct a random forest model
# Covariates (x) are found in columns 1 to 5 of valuetable.09
# Training classes.09 (y) are found in the 'class' column of valuetable.09
## Caution: this step takes fairly long!
# but can be shortened by setting importance=FALSE
modelRF.09 <- randomForest(x=valuetable.09[ ,c(1:10)], y=valuetable.09$class,
                           importance = TRUE, ntree = 2000)


## Inspect the structure and element names of the resulting model
modelRF.09
class(modelRF.09)
str(modelRF.09)
names(modelRF.09)
## Inspect the confusion matrix of the OOB error assessment
modelRF.09$confusion
# to make the confusion matrix more readable
colnames(modelRF.09$confusion) <- c("forest", "grass", "rock", "shrub", "spruce", "class.error")
rownames(modelRF.09$confusion) <- c("forest", "grass", "rock", "shrub", "spruce")
modelRF.09$confusion

varImpPlot(modelRF.09)

## Double-check layer and column names to make sure they match
names(covs.09)
names(valuetable.09)

## Predict land cover using the RF model
pred.09 <- raster::predict(covs.09, model=modelRF.09, na.rm=TRUE)

## Plot the results
# recall: 1 = cropland, 2 = forest, 3 = wetland
cols <- c("#EAB674","#B4F087", "#C1C5D9",  "#437D0F", "#1A2407")
plot(pred.09, col=cols, legend=FALSE)
legend("bottomleft", 
       legend=c( "forest", "grass", "rock", "shrub", "spruce"), 
       fill=cols, bg="white",
       title = "2009")

rf.86 <- as.data.frame(pred86)
table(rf.86)

rf.90 <- as.data.frame(pred.90)
table(rf.90)

rf.95 <- as.data.frame(pred.95)
table(rf.95)

rf.05 <-as.data.frame(pred.05)
table(rf.05)

rf.09 <- as.data.frame(pred.09)
table(rf.09)

rf.11 <- as.data.frame(predLC)
table(rf.11)



#### plot trees
tree <- getTree(modelRF.11, k=1, labelVar=TRUE)
dend <- as.dendrogram(tree)

require(tree)
plot(tree)


to.dendrogram <- function(dfrep,rownum=1,height.increment=0.1){
     
     if(dfrep[rownum,'status'] == -1){
          rval <- list()
          
          attr(rval,"members") <- 1
          attr(rval,"height") <- 0.0
          attr(rval,"label") <- dfrep[rownum,'prediction']
          attr(rval,"leaf") <- TRUE
          
     }else{##note the change "to.dendrogram" and not "to.dendogram"
          left <- to.dendrogram(dfrep,dfrep[rownum,'left daughter'],height.increment)
          right <- to.dendrogram(dfrep,dfrep[rownum,'right daughter'],height.increment)
          rval <- list(left,right)
          
          attr(rval,"members") <- attr(left,"members") + attr(right,"members")
          attr(rval,"height") <- max(attr(left,"height"),attr(right,"height")) + height.increment
          attr(rval,"leaf") <- FALSE
          attr(rval,"edgetext") <- dfrep[rownum,'split var']
          #To add Split Point in Dendrogram
          #attr(rval,"edgetext") <- paste(dfrep[rownum,'split var'],"\n<",round(dfrep[rownum,'split point'], digits = 2),"=>", sep = " ")
     }
     
     class(rval) <- "dendrogram"
     
     return(rval)
}

mod <- randomForest(Species ~ .,data=iris)
tree <- getTree(modelRF.11, 1,labelVar=TRUE)

d <- to.dendrogram(tree)
str(d)
plot(d,center=TRUE,leaflab='',edgePar=list(t.cex=1,p.col=NA,p.lty=0))
