#Trying to do all of it in R

#### ras class

require(rasclass)

# For this example, create artificial data
mysample <- c(rep(rep(c(1,2), each = 25), 25), rep(rep(c(3,4), each = 25), 25))
mysample <- mysample + sample(c(0, NA), 2500, replace = TRUE, prob = c(1, 50))
myvar1 <- rep(1:50, each = 50) + rnorm(2500, 0, 5)
myvar2 <- rep(rep(1:50), 50) + rnorm(2500, 0, 5)
newdata <- data.frame(mysample, myvar1, myvar2)
# Prepare a rasclass object using the dataframe and specifying raster properties
object <- new('rasclass')
object <- setRasclassData(newdata, ncols = 50, nrows = 50,
                          xllcorner = 0, yllcorner = 0, cellsize = 1, NAvalue = -9999,
                          samplename = 'mysample')

# Classify using each algorithm once
outlist <- list()
outlist[['maximumLikelihood']] <- classifyRasclass(object, method = 'maximumLikelihood')
summary(outlist[['maximumLikelihood']])
outlist[['logit']] <- classifyRasclass(object, method = 'logit')
summary(outlist[['logit']])
outlist[['neuralNetwork']] <- classifyRasclass(object, method = 'neuralNetwork')
summary(outlist[['neuralNetwork']])
outlist[['randomForest']] <- classifyRasclass(object, method = 'randomForest')
summary(outlist[['randomForest']])
outlist[['supportVector']] <- classifyRasclass(object, method = 'supportVector')
summary(outlist[['supportVector']])
# Store sample data as a rasclassRaster for display purposes
mysample.ras <- new('rasclassRaster')
mysample.ras@grid <- mysample
mysample.ras@nrows <- 50
mysample.ras@ncols <- 50
mysample.ras@xllcorner <- 0
mysample.ras@yllcorner <- 0
mysample.ras@cellsize <- 1
mysample.ras@NAvalue <- -9999
# Plot results of each classifier
opar <- par(mfrow = c(2, 3))
image(mysample.ras)
title('Sample data')
for(i in 1:length(outlist)) {
     image(outlist[[i]]@predictedGrid)
     title(names(outlist)[[i]])
}
par(opar) 

#####

ras.86 <- extract(ndvi.86)
o.86 <- new('rasclass')
o.86 <- setRasclassData(df.86, ncols = 85, nrows = 85,
                          xllcorner = 633885, yllcorner = 4329615, cellsize = 30, NAvalue = -9999,
                          samplename = 'ndvi1986')

# kmeans

km.86 <- kmeans(values(ndvi.86), centers = 2, iter.max = 500,
                nstart = 3, algorithm = "Lloyd")

                # analysis 1986
sub.86 <- subset(stack.86, 2:4)

covs.86 <- addLayer(sub.86, ndvi.86)
valuetable <- getValues(covs.86)
head(valuetable)

km <- kmeans(na.omit(valuetable), centers = 2, iter.max = 100, nstart = 10)

head(km$cluster)

unique(km$cluster)                

## Create a blank raster with default values of 0
rNA <- setValues(raster(covs.86), 0)
## Loop through layers of covs
## Assign a 1 to rNA wherever an NA is enountered in covs
for(i in 1:nlayers(covs.86)){
     rNA[is.na(covs.86[[i]])] <- 1
}
## Convert rNA to an integer vector
rNA <- getValues(rNA)


## Convert valuetable to a data.frame
valuetable <- as.data.frame(valuetable)
## If rNA is a 0, assign the cluster value at that position
valuetable$class[rNA==0] <- km$cluster
## If rNA is a 1, assign an NA at that position
valuetable$class[rNA==1] <- NA

## Create a blank raster
classes <- raster(covs.86)
## Assign values from the 'class' column of valuetable
classes <- setValues(classes, valuetable$class)
plot(classes, legend=FALSE, col=c("dark green", "orange", "light blue"))

                
                
#spatial autocorrelation
require(spdep)
moran.test(ndvi.86, ndvi.11)

                
                
                ######

####
####NLCD
####
nlcd <- raster('./GIS/nlcd_2011_weimer.TIF')

plot(nlcd)
