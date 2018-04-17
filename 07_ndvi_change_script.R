# This script uses the NDVI change data from Iso-Clusted Unsupervised Classification

n1986 <- read.csv("./data/iso1986_class_data.csv")
n2011 <- read.csv("./data/iso2011_class_data.csv")


# Time to make class comparisons for NDVI 1986 to 2011
# a 1 is NO SHRUB and 2 is SHRUB
ndvi.comp <- merge(n1986, n2011, by = c("X", "Y"))
ndvi.comp <- ndvi.comp[,c(1,2,5,8)]
colnames(ndvi.comp) <- c("x", "y", "ndvi86", "ndvi11")


##this function assesses change in classification from 1986 - 2011
# In raw file, 2 is a shrub, 1 is not

# 1 - Stays nothing
# 2 - Becomes shrub
# 3 - was shrub, not shrub anymore
# 4 - Was shrub, stays shrub

shrubify <- function(x, y, ...) {
     if(x == 1 && y == 1) {class = 1}
     else if(x == 1 && y == 2) {class = 2}
     else if(x == 2 && y == 1) {class = 3}
     else if(x == 2 && y == 2) {class = 4}
     
}

ndvi.comp$class <-mapply( shrubify, ndvi.comp$ndvi86, ndvi.comp$ndvi11)
write.csv(ndvi.comp, file="ndvi_comp_86_11.csv")      #this writes the file for import to ArcGIS

summary(ndvi.comp)
table(ndvi.comp$class)


# Now to bring in raw NDVI data

2013/4153
781/4153
122/4153
1237/4153

r.86 <- raster("./data/ndvi1986.tif")
r.90 <- raster("./data/b1990_ndvi.tif")
r.95 <- raster("./data/b1995_ndvi.tif")
r.05 <- raster("./data/b2005_ndvi.tif")
r.09 <- raster("./data/b2009_ndvi.tif")
r.11 <- raster("./data/b2011_ndvi.tif")

df.86.stats <- data.frame(df.86.mean=cellStats(df.86, "mean"))

cellStats(df.86, "mean")
cellStats(df.11, "mean")

df.86 <- rasterToPoints(r.86)
df.90 <- rasterToPoints(r.90)
df.95 <- rasterToPoints(r.95)
df.05 <- rasterToPoints(r.05)
df.09 <- rasterToPoints(r.09)
df.11 <- rasterToPoints(r.11)

df.86 <- data.frame(df.86)
df.90 <- data.frame(df.90)
df.95 <- data.frame(df.95)
df.05 <- data.frame(df.05)
df.09 <- data.frame(df.09)
df.11 <- data.frame(df.11)

df.86.11 <- merge(df.86, df.11)

df.86.11$diff <- df.86.11$b2011_ndvi - df.86.11$ndvi1986

# Change the column names
colnames(df.86.11) <- c("x","y", "ndvi.86", "ndvi.11", "diff")



density(df.86.11$diff)
table(sign(df.86.11$diff))


#filled NDVI density plots by year 


#Making density plots of NDVI through time
d.1986 <- density(r.86)http://www.environmentalbiophysics.org/environmental-biophysics-lectures/
d.1990 <- density(r.90)
d.1995 <- density(r.95)
d.2005 <- density(r.05)
d.2009 <- density(r.09)
d.2011 <- density(r.11)

#this makes the joy division plot
par(mfrow = c(6,1), mar=c(2,4,0,2))
plot(d.1986, main="", xaxt='n', cex.axis = 1.5, xlim = c(0,1), ylim = c(0,10))+
     polygon(d.1986, col="black", border="black")+
     title("1986", line = -1.5, adj=0.8, cex.main = 2)
plot(d.1990, main="", xaxt='n',cex.axis = 1.5, xlim = c(0,1), ylim = c(0,10))+
     polygon(d.1990, col="black", border="black")+
     title("1990", line = -1.5, adj=0.8, cex.main = 2)
plot(d.1995, main="", xaxt='n',cex.axis = 1.5,xlim = c(0,1), ylim = c(0,10))+
     polygon(d.1995, col="black", border="black")+
     title("1995", line = -1.5, adj=0.8, cex.main = 2)
plot(d.2005, main="", xaxt='n',cex.axis = 1.5,xlim = c(0,1), ylim = c(0,10))+
     polygon(d.2005, col="black", border="black")+
     title("2005", line = -1.5, adj=0.8, cex.main = 2)
plot(d.2009, main="", xaxt='n',cex.axis = 1.5,xlim = c(0,1), ylim = c(0,10))+
     polygon(d.2009, col="black", border="black")+
     title("2009", line = -1.5, adj=0.8, cex.main = 2)
plot(d.2011, main="", cex.axis = 1.5, xlim = c(0,1), ylim = c(0,10))+
     polygon(d.2011, col="black", border="black")+
     title("2011", line = -1.5, adj=0.8, cex.main = 2)
par(mar = c(0, 0, 0, 0))

#reset to default
dev.off()

# OK let's try this with ggjoy
df.all <- bind_cols(df.86, df.90, df.95, df.05, df.09, df.11)

colnames(df.all) <- c("x", "y", "1986", "1990", "1995", "2005", "2009", "2011")
require(tidyr)

df.all %>% gather(year, ndvi, c(`1986`:`2011`)) -> df.long


require(ggjoy)

ggplot(df.long, aes(x = ndvi, y = year))+
     geom_joy(scale = 2, size = 1, color = "black", fill = "forestgreen")+
     theme_joy(grid = FALSE) +
     scale_y_discrete(expand = c(0.5, 0.5)) +   # will generally have to set the `expand` option
     scale_x_continuous(expand = c(0, 0.5)) +
     xlab("NDVI")+
     ylab("")+
     xlim(c(0.2, 0.8))
     
     


