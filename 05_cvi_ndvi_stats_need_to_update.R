#05_cvi_ndvi_stats.R
#This script was written by J. Atkins 2015, 2016 (jeffatkins@virginia.edu or @atkinsjeff)
#https://github.com/atkinsjeff
#
#This script includes the linear regression model and all statistics from the manuscript
#I have left a lot of old code in here because honestly I was learning as I went along and I just kept it here.
##################
#LANDSAT linear regression change model
#each cell is recalculated to a change in time.

require(gplots)
library(broom)
library(magrittr)
require(plyr)
library(dplyr)
require(tidyr)
require(Hmisc)
require(psych)
####NEED TO ORDER SCRIPT BY PAPER
summary(ndvi.86)
cellStats(ndvi.86, stat = "mean")
summary(ndvi.90)
cellStats(ndvi.90, stat = "mean")
summary(ndvi.95)
cellStats(ndvi.95, stat = "mean")
summary(ndvi.05)
cellStats(ndvi.05, stat = "mean")
summary(ndvi.09)
cellStats(ndvi.09, stat = "mean")
summary(ndvi.11)
cellStats(ndvi.11, stat = "mean")


#########  Linear regression scripts

#### NDVI
ndvi.stack <- bind_rows(c(df.86, df.90, df.95, df.05, df.09, df.11))
ndvi.stack <- data.frame(ndvi.stack)

stack <- ndvi.stack 
raw.stack <- stack

stack <- gather(stack, "rawyear", "ndvi", 3:8)

library(stringr)
stack$year <- as.numeric(unlist(str_extract_all(stack$rawyear, '\\d+')))


#ndvi

head(df.86.11)
ndvi.86.11.positive <- subset(df.86.11, diff > 0)
ndvi.86.11.negative <- subset(df.86.11, diff <= 0)

# evi
head(e.86.11)
summary(e.86.11)
evi.86.11.positive <- subset(e.86.11, diff > 0)
evi.86.11.negative <- subset(e.86.11, diff <= 0)

head(s.86.11)
summary(s.86.11)
savi.86.11.positive <- subset(s.86.11, diff > 0)
savi.86.11.negative <- subset(s.86.11, diff <= 0)

########## Linear regressions with pipes

library(broom)
library(magrittr)
library(dplyr)
require(tidyr)

#NDVI
lm.ndvi.pipe = 
  stack %>% 
  group_by(x,y) %>%
  do({model = lm(ndvi~year, data=.)    # create your model
  data.frame(tidy(model),              # get coefficient info
             glance(model))})          # get model info

lm.ndvi.pipe %>%
  select(x, y, term, estimate, p.value)

lm.ndvi.pipe %>%
  select(x, y, term, estimate, adj.r.squared, p.value) %>%
  spread(term, estimate) 

lm.ndvi.pipe.clean <- subset(lm.ndvi.pipe[,c(1,2,3,4,7,9)])
lm.ndvi.pipe.clean <- subset(lm.ndvi.pipe.clean, term == "year")


plot(lm.ndvi.pipe.clean$p.value)

###3 Trying raster p values


temp.ndvi <- lm.ndvi.pipe.clean[,c(1,2,4,5)]
temp.ndvi$change <- temp.ndvi$estimate * 25

temp.ras <- lm.ndvi.pipe.clean[,c(1,2,5)]
rasterNDVI.p <- rasterFromXYZ(temp.ras)

#plotting p values
breakpoints <- c(0, 0.01, 0.05,1)
colors <- c("dark red","red","light grey")

plot(rasterNDVI.p,breaks=breakpoints,col=colors )+
     mtext("P value")

#change
temp.ras.change <- temp.ndvi[,c(1,2, 5)]
rasterNDVI.change <- rasterFromXYZ(temp.ras.change)

colfunc <- colorRampPalette(c("brown", "white", "dark green"))
colfunc(12)

breakpoints.change <- c(-0.6, -0.5, -0.4, -0.3, -0.2, 0, 0.2, 0.4, 0.6)
plot(rasterNDVI.change, col = colfunc(10), breaks = seq(-0.5, 0.5, by = 0.1))+
     mtext("NDVI change")

lm.ndvi.ras <- raster(temp.ndvi, "p.value")
#putting in all the landscape variables elevation class and distance to stream and aspect
lm.ndvi.elev <- merge(temp.ndvi, elev_classes, by = c("x","y"))
lm.ndvi.elev <- merge(lm.ndvi.elev, stream_distance, by = c("x","y"))
lm.ndvi.elev$elev <- as.factor(lm.ndvi.elev$elev)
lm.ndvi.elev <- merge(lm.ndvi.elev, weimer_aspect, by = c("x", "y"))
lm.ndvi.elev <- merge(lm.ndvi.elev, elev_raw, by = c("x", "y"))

# let's look at some stats

# How  many p value less than 0.05
(sum(temp.ndvi$p.value <= 0.05) / 4153)
(sum(temp.ndvi$p.value <= 0.01) / 4153)

#adding a factor column based on p.value

lm.ndvi.elev = within(lm.ndvi.elev, {
  p.class = ifelse(p.value <= 0.05, "SIGNIFICANT", "NOT SIGNIFICANT")
  
})

lm.ndvi.elev$p.class <- as.factor(lm.ndvi.elev$p.class)

summary(lm.ndvi.elev)
#write.csv(lm.ndvi.elev, file="lm_ndvi_output_may_2017.csv")

ndvi.positive <- subset(lm.ndvi.elev, change > 0)
ndvi.negative <- subset(lm.ndvi.elev, change <= 0)


###################
# LANDESCAPE ANALYSIS setion

ndvi.df <- ndvi.elev

colnames(ndvi.df)[colnames(ndvi.df) == "ndvi1986"] <- "ndvi.86"
colnames(ndvi.df)[colnames(ndvi.df) == "b1990_ndvi"] <- "ndvi.90"
colnames(ndvi.df)[colnames(ndvi.df) == "b1995_ndvi"] <- "ndvi.95"
colnames(ndvi.df)[colnames(ndvi.df) == "b2005_ndvi"] <- "ndvi.05"
colnames(ndvi.df)[colnames(ndvi.df) == "b2009_ndvi"] <- "ndvi.09"
colnames(ndvi.df)[colnames(ndvi.df) == "b2011_ndvi"] <- "ndvi.11"

x <- merge(ndvi.df, stream_distance, by = c("x", "y"))
x3 <- merge(x, weimer_aspect, by = c("x", "y"))
x4 <- merge(x3, elev_raw, by = c("x", "y"))


x4$elev <-as.factor(x4$elev)
class_ndvi <- x4

class_ndvi$elev.raw[class_ndvi$elev.raw == 0] <- NA
class_ndvi <- na.omit(class_ndvi)
###### Elevation 
###!!!!!!!!! this section dependent on elevation_time.R script

####ELEVATION ANALYSIS ONE using the output from the linear regression, lm.pipes model
lm.ndvi.high <- subset(lm.ndvi.elev, elev == "HIGH")
lm.ndvi.mid <- subset(lm.ndvi.elev, elev == "MID")
lm.ndvi.low <- subset(lm.ndvi.elev, elev == "LOW")


# Uses previous function to summarize elevation linear regression data
sapply(lm.ndvi.high[,ind], my.summary)
sapply(lm.ndvi.mid[,ind], my.summary)
sapply(lm.ndvi.low[,ind], my.summary)

# #############  NDVI
# #merging w/ elev classes to 
ndvi.elev <- merge(ndvi.stack, elev_classes, by = c("x", "y"))

aggregate(ndvi ~ elev + year, data = ndvi.elev, FUN=mean)

ndvi.high <- subset(ndvi.elev, elev == "HIGH")
ndvi.mid <- subset(ndvi.elev, elev == "MID")
ndvi.low <- subset(ndvi.elev, elev == "LOW")

sapply(ndvi.high[,ind], my.summary)
sapply(ndvi.mid[,ind], my.summary)
sapply(ndvi.low[,ind], my.summary)
# 
# ############  G+W



####Analysis of variance script

####################

#first test normality assumptions (they are good)
shapiro.test(class_ndvi$ndvi.86)
shapiro.test(class_ndvi$ndvi.11)

#ANOVA of NDVI (1986 and 2011) to aspect.class, with no covariant#ANOVA for raw differences in elevation and aspect
lm.elev.fit.86 <- aov(ndvi.86 ~ elev + aspect.class + elev * aspect.class, data = class_ndvi, na.action = na.omit)
lm.elev.fit.90 <- aov(ndvi.90 ~ elev + aspect.class + elev * aspect.class, data = class_ndvi, na.action = na.omit)
lm.elev.fit.95 <- aov(ndvi.95 ~ elev + aspect.class + elev * aspect.class, data = class_ndvi, na.action = na.omit)
lm.elev.fit.05 <- aov(ndvi.05 ~ elev + aspect.class + elev * aspect.class, data = class_ndvi, na.action = na.omit)
lm.elev.fit.09 <- aov(ndvi.09 ~ elev + aspect.class + elev * aspect.class, data = class_ndvi, na.action = na.omit)
lm.elev.fit.11 <- aov(ndvi.11 ~ elevelev + aspect.class + elev * aspect.class, data = class_ndvi, na.action = na.omit)
sum(is.na(class_ndvi$elev))

summary(lm.elev.fit.86)
summary(lm.elev.fit.11)

TukeyHSD(lm.elev.fit.86)
TukeyHSD(lm.elev.fit.11)

require(psych)
#Using the pscyh packate to describe.by elev
describeBy(class_ndvi, class_ndvi$elev)

#Using the pscyh packate to describe.by aspect.class
describeBy(class_ndvi, class_ndvi$aspect.class)

#ANOVA on NDVI change by elevation and aspect.class from trend analysis
shapiro.test(lm.ndvi.elev$change)
lm.trend.ndvi.fit <- aov(change ~ elev * aspect.class, data = lm.ndvi.elev)

summary(lm.trend.ndvi.fit)
TukeyHSD(lm.trend.ndvi.fit)

#looking at means from each 1986, and 2011 and trend
aggregate(ndvi.86 ~ elev + aspect.class, data = class_ndvi, FUN = mean)
aggregate(ndvi.86 ~ aspect.class, data = class_ndvi, FUN = sd)

aggregate(ndvi.11 ~ elev + aspect.class, data = class_ndvi, FUN = mean)
aggregate(ndvi.11 ~ aspect.class, data = class_ndvi, FUN = mean)

aggregate(change ~ elev + aspect.class, data = lm.ndvi.elev, FUN = mean)
aggregate(change ~ aspect.class, data = lm.ndvi.elev, FUN = mean)

#looking at means by elevation
aggregate(ndvi.86 ~ elev, data = class_ndvi, FUN = mean)
aggregate(ndvi.11 ~ elev, data = class_ndvi, FUN = mean)

aggregate(diff ~ elev, data = class_ndvi, FUN = mean)
aggregate(ndvi.11 ~ elev, data = class_ndvi, FUN = mean)
#####let's do a further analysis on our change data

#this does a count of the number of cells that have a sign change (either that are positive or negative)
table(sign(lm.ndvi.elev$change))

ndvi.positive <- subset(lm.ndvi.elev, change > 0)
ndvi.negative <- subset(lm.ndvi.elev, change <= 0)

#creating output table showing stastics on change

my.summary <- function(x, na.rm=TRUE) {
  result <- c(Mean = as.numeric(format(mean(x, na.rm=na.rm), digits=3)),
              SD = format(sd(x, na.rm=na.rm), digits=3),
              Median = format(median(x, na.rm=na.rm), digits=3),
              Min = format(min(x, na.rm=na.rm), digits = 3),
              Max = format(max(x, na.rm=na.rm), digits = 3),
              N = length(x),
              SE = sd(x) / sqrt(length(x)),
              CV = abs(mean(x)) / abs(sd(x)))
}

ind3 <- sapply(ndvi.positive, is.numeric)
ind4 <- sapply(ndvi.negative, is.numeric)

#prints the two tales contrasting postitive and negative values
sapply(ndvi.positive[,ind3], my.summary)
sapply(ndvi.negative[,ind4], my.summary)

#boxplot for distribution of ndvi
ndvi.box <- boxplot(ndvi~year, data=stack, main="Distribution of NDVI values by year", ylab="NDVI")






#ANCOVA testing differences in NDVI by elevation by stream distance



######
#####
#####
# HERE!!!!!!!!!
contrasts(class_ndvi$elev)=contr.poly(3)
model.1=aov(ndvi.11 ~ stream_dist + elev, data = class_ndvi)
Anova(model.1, type = "III")

summary.lm(model.1)
posth=glht(model.1, linfct=mcp(elev="Tukey"))  ##gives the post-hoc Tukey analysis of elevation, which is the factorvariable
summary(posth) ##shows the output in a nice format.
interact.model=aov(ndvi.11 ~ stream_dist + elev + stream_dist:elev, data=class_ndvi)
summary(interact.model)
#Vist these sites:   http://goo.gl/yxUZ1R 
# http://stats.stackexchange.com/questions/51780/how-i-make-an-ancova-in-rR


#ANCOVA  elevation and stream distance on 86, 11, and change

#1986
results.86 = lm(ndvi.86 ~ stream_dist * elev, data = class_ndvi)
aov.86 <-aov(results.86)
summary(aov.86)
TukeyHSD(aov.86, "elev")
qqnorm(results.86$residuals)

plot(results.86$fitted,results$res,xlab="Fitted",ylab="Residuals")

#2011
results.11 = lm(ndvi.11 ~ stream_dist * elev , data = class_ndvi)
aov.11 <- aov(results.11)
summary(aov.11)
TukeyHSD(aov.11, "elev")
qqnorm(results.11$residuals)

head(ndvi.86)

plot(results.11$fitted,results$res,xlab="Fitted",ylab="Residuals")

#ndvi difference from endmembers
results.change = lm(diff ~ stream_dist * elev , data = class_ndvi)
aov.change <-aov(results.change)
summary(aov.change)
TukeyHSD(aov.change, "elev")
qqnorm(results.change$residuals)

plot(results.change$fitted,results$res,xlab="Fitted",ylab="Residuals")

p.stream.elev <- ggplot(class_ndvi, aes(x=stream_dist, y=ndvi.11, color=elev))+
  geom_point(size=3)



##### Piecewise regression
require(segmented)

# piecewise regression on class_ndvi 

#2011
x11()
plot(class_ndvi$stream_dist, class_ndvi$ndvi.11)
lm.ndvi.stream.11 <- lm(ndvi.11 ~ stream_dist, data = class_ndvi)

seg.ndvi.stream.11 <- segmented(lm.ndvi.stream.11, seg.Z = ~stream_dist, psi=100,
                                control = seg.control(display=TRUE))
plot(seg.ndvi.stream.11, conf.level = 0.95, shade = TRUE)

#1986
plot(class_ndvi$stream_dist, class_ndvi$ndvi.86)
lm.ndvi.stream.86 <- lm(ndvi.86 ~ stream_dist, data = class_ndvi)

seg.ndvi.stream.86 <- segmented(lm.ndvi.stream.86, seg.Z = ~stream_dist, psi=100,
                                control = seg.control(display=TRUE))
plot(seg.ndvi.stream.86, conf.level = 0.95, shade = TRUE)

#Estimating breakpoints
summary(seg.ndvi.stream.86)
summary(seg.ndvi.stream.11)

#identifies the changepoint as 142.4 m +/- 4.468

##RIPARIAN
#Now to classify as riparian or upland based on stream_dist threshold
class_ndvi$stream_class[class_ndvi$stream_dist <= 150] <- "Riparian"
class_ndvi$stream_class[class_ndvi$stream_dist > 150] <- "Upland"

#OK, now let's check that jam with some linear regression action
riparian.ndvi <- subset(class_ndvi, class_ndvi$stream_class == "Riparian")
upland.ndvi <- subset(class_ndvi, class_ndvi$stream_class == "Upland")

#Linear regression using the RIPARIAN data
lm.riparian.11 <- lm(ndvi.11 ~ stream_dist, data = riparian.ndvi)
lm.riparian.86 <- lm(ndvi.86 ~ stream_dist, data = riparian.ndvi)
lm.riparian.change  <-lm(diff  ~ stream_dist, data = riparian.ndvi)

#pearson's correlation
cor.riparian.11 <- cor(riparian.ndvi$stream_dist, riparian.ndvi$ndvi.11, use = "everything", method = "pearson")
cor.riparian.86 <- cor(riparian.ndvi$stream_dist, riparian.ndvi$ndvi.86, use = "everything", method = "pearson")
cor.riparian.change <- cor(riparian.ndvi$stream_dist, riparian.ndvi$change, use = "everything", method = "pearson")

summary(lm.riparian.86)
summary(lm.riparian.11)
summary(lm.riparian.change)

##UPLAND
lm.upland.86 <- lm(ndvi.86 ~ stream_dist, data = upland.ndvi)
lm.upland.11 <- lm(ndvi.11 ~ stream_dist, data = upland.ndvi)
lm.upland.change <- lm(change ~ stream_dist, data = upland.ndvi)

#pearson's correlation
cor.upland.11 <- cor(upland.ndvi$stream_dist, upland.ndvi$ndvi.11, use = "everything", method = "pearson")
cor.upland.86 <- cor(upland.ndvi$stream_dist, upland.ndvi$ndvi.86, use = "everything", method = "pearson")
cor.upland.change <- cor(upland.ndvi$stream_dist, upland.ndvi$change, use = "everything", method = "pearson")

summary(lm.upland.86)
summary(lm.upland.11)
summary(lm.upland.change)



plot(upland.ndvi$stream_dist, upland.ndvi$ndvi.11)
plot(riparian.ndvi$stream_dist, riparian.ndvi$ndvi.86)





# # Now we want to recombine the data to look at the mean increase in NDVI for each 
# This will combing the ndvi.comp df which tells us the change in classification based on the endmembmers of 1986 and 2011 NDVI
# with the linear regression function results
class_ndvi_comp <- merge(class_ndvi, ndvi.comp, by = c("x","y"))

#CHANGE
aggregate.ndvi.comp <- aggregate(change ~ class, data = class_ndvi_comp, FUN =  function(x) c(  = sd(x), AVG= mean(x)))
print(aggregate.ndvi.comp)

aggregate.ndvi.elev.comp <- aggregate(change ~ class*elev, data = class_ndvi_comp, mean, na.rm=TRUE)
print(aggregate.ndvi.elev.comp)

aggregate.ndvi.elev.stream.comp <- aggregate(change ~ class*elev*stream_class, data = class_ndvi_comp, mean, na.rm=TRUE)
print(aggregate.ndvi.elev.stream.comp)

aggregate.ndvi.stream.comp <- aggregate(change ~ class*stream_class, data = class_ndvi_comp, mean, na.rm=TRUE)
print(aggregate.ndvi.stream.comp)

#NDVI
aggregate.ndvi <- aggregate(ndvi.11 ~ class, data = class_ndvi_comp, mean, na.rm=TRUE)
print(aggregate.ndvi)

aggregate.ndvi.elev <- aggregate(ndvi.11 ~ class*elev, data = class_ndvi_comp, mean, na.rm=TRUE)
print(aggregate.ndvi.elev)

aggregate.ndvi.elev.stream <- aggregate(ndvi.11 ~ class*elev*stream_class, data = class_ndvi_comp, mean, na.rm=TRUE)
print(aggregate.ndvi.elev.stream)
                                             
aggregate.ndvi.stream <- aggregate(ndvi.11 ~ class*stream_class, data = class_ndvi_comp, mean, na.rm=TRUE)
print(aggregate.ndvi.stream)



as.table(aggregate.ndvi.stream)
