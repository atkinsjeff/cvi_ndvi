# Working with the iso cluster analysis from ArcGIS

ndvi.86.cluster <- read.csv("./data/ndvi1986cluster.txt", colClasses=c("NULL", "NULL", NA, NA, NA ))
ndvi.11.cluster <- read.csv("./data/ndvi2011cluster.txt", colClasses=c("NULL", "NULL", NA, NA, NA ))
savi.86.cluster <- read.csv("./data/savi1986cluster.txt", colClasses=c("NULL", "NULL", NA, NA, NA ))
savi.11.cluster <- read.csv("./data/savi2011cluster.txt", colClasses=c("NULL", "NULL", NA, NA, NA ))
evi.86.cluster <- read.csv("./data/evi1986cluster.txt", colClasses=c("NULL", "NULL", NA, NA, NA ))
evi.11.cluster <- read.csv("./data/evi2011cluster.txt", colClasses=c("NULL", "NULL", NA, NA, NA ))

table(ndvi.86.cluster$iso1986_ndvi)


cluster86 <- merge( ndvi.86.cluster, savi.86.cluster, by = c("X", "Y"))
cluster86 <- merge( cluster86, evi.86.cluster, by = c("X", "Y"))



#additively combining scenes to create model classification
attach(cluster86)
cluster86$combo <- (iso1986_ndvi + isocluster2_1 + evi1986cluster_1) - 3   #Subtracting 4 b/c all ones would be non-shrubs and we want just the no. of models that predict shrub cover
detach(cluster86)


# 2011
cluster11 <- merge( ndvi.11.cluster, savi.11.cluster, by = c("X", "Y"))
cluster11 <- merge( cluster11, evi.11.cluster, by = c("X", "Y"))

#additively combining scenes to create model classification
attach(cluster11)
cluster11$combo <- (iso2011_ndvi + savi2011cluster_1 + evi2011cluster_1) - 3   #Subtracting 4 b/c all ones would be non-shrubs and we want just the no. of models that predict shrub cover
detach(cluster11)



# lets look at this jenk
table(cluster86$combo)
table(cluster11$combo)


#ndvi
table(cluster86$iso1986_ndvi)
table(cluster86$evi1986cluster_1)

table(cluster11$evi2011cluster_1)

table(cluster11$combo)
summary(cluster11)

# write.csv(cluster86, "isomodel_1986.csv")
# write.csv(cluster11, "isomodel_2011.csv")