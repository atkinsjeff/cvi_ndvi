#06_cvi_ndvi_plots.R
#This script was written by J. Atkins 2015, 2016 (jeffatkins@virginia.edu or @atkinsjeff)
#https://github.com/atkinsjeff
#
#This script makes all of the plots in the manuscript and some bonus stuff!
#requires

require(ggplot2)
require(wesanderson)
library(raster)

#### Import section
r.86 <- raster("./data/ndvi1986.tif")
r.90 <- raster("./data/b1990_ndvi.tif")
r.95 <- raster("./data/b1995_ndvi.tif")
r.05 <- raster("./data/b2005_ndvi.tif")
r.09 <- raster("./data/b2009_ndvi.tif")
r.11 <- raster("./data/b2011_ndvi.tif")

#
plotRGB(r.86)
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

#pulls in other df.

###COMPARING to 1:1 lines
# 
# #comparing ndvi endpoints to a 1:1 line
# ndvi.86.11 <- cbind(x1986, x2011)
# ndvi.86.11 <- ndvi.86.11[,c(1,2,3,8)]
# colnames(ndvi.86.11) <- c("x", "y", "ndvi.86", "ndvi.11")

p.ndvi <- ggplot(df.86.11, aes(x=ndvi.86, y=ndvi.11))+
  geom_point(size=2, alpha=0.5)+
  theme_bw()+
  geom_abline(intercept=0, slope=1)+
  xlim(0.25,0.75)+
  ylim(0.25,0.75)+
  xlab("NDVI (1986)")+
  ylab("NDVI (2011)")+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x = -Inf, y = Inf, hjust = -0.25, vjust = 1.25, label = "A", size = 12)
x11()
p.ndvi

#SAVI

s.86 <- rasterToPoints(savi.86)
s.90 <- rasterToPoints(savi.90)
s.95 <- rasterToPoints(savi.95)
s.05 <- rasterToPoints(savi.05)
s.09 <- rasterToPoints(savi.09)
s.11 <- rasterToPoints(savi.11)

s.86 <- data.frame(s.86)
s.90 <- data.frame(s.90)
s.95 <- data.frame(s.95)
s.05 <- data.frame(s.05)
s.09 <- data.frame(s.09)
s.11 <- data.frame(s.11)

colnames(s.86)[3] <- "savi.86"
colnames(s.11)[3] <- "savi.11"

s.86.11 <- merge(s.86, s.11)

s.86.11$diff <- s.86.11$savi.11 - s.86.11$savi.86


#comparing savi

p.savi<- ggplot(s.86.11, aes(x=savi.86, y=savi.11))+
  geom_point(size=2, alpha=0.5)+
  theme_bw()+
  geom_abline(intercept=0, slope=1)+
   xlim(0.2, 1.25)+
   ylim(0.2, 1.25)+
  xlab("SAVI (1986)")+
  ylab("SAVI (2011)")+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x = -Inf, y = Inf, hjust = -0.25, vjust = 1.25, label = "B", size = 12)

p.savi
# EVI
e.86 <- rasterToPoints(evi.86)
e.90 <- rasterToPoints(evi.90)
e.95 <- rasterToPoints(evi.95)
e.05 <- rasterToPoints(evi.05)
e.09 <- rasterToPoints(evi.09)
e.11 <- rasterToPoints(evi.11)

e.86 <- data.frame(e.86)
e.90 <- data.frame(e.90)
e.95 <- data.frame(e.95)
e.05 <- data.frame(e.05)
e.09 <- data.frame(e.09)
e.11 <- data.frame(e.11)

colnames(e.86)[3] <- "evi.86"
colnames(e.11)[3] <- "evi.11"

e.86.11 <- merge(e.86, e.11)

e.86.11$diff <- e.86.11$evi.11 - e.86.11$evi.86


#comparing savi

p.evi<- ggplot(e.86.11, aes(x=evi.86, y=evi.11))+
     geom_point(size=2, alpha=0.5)+
     theme_bw()+
     geom_abline(intercept=0, slope=1)+
        xlim(0.5, 3)+
        ylim(0.5, 3)+
     xlab("EVI (1986)")+
     ylab("EVI (2011)")+
     theme(axis.title.x = element_text(size=20),
           axis.text.x  = element_text(vjust=0.5, size=16),
           axis.title.y = element_text(size=20),
           axis.text.y  = element_text(size=16),
           panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
     annotate("text", x = -Inf, y = Inf, hjust = -0.25, vjust = 1.25, label = "C", size = 12)

p.evi
# #comparing greenness endpoints to a 1:1 line

g.86 <- rasterToPoints(green.86)
g.11 <- rasterToPoints(green.11)

g.86 <- data.frame(g.86)
g.11 <- data.frame(g.11)

colnames(g.86)[3] <- "green.86"
colnames(g.11)[3] <- "green.11"

g.86.11 <- merge(g.86, g.11)

#e.86.11$diff <- e.86.11$evi.11 - e.86.11$evi.86


p.green <- ggplot(g.86.11, aes(x=green.86, y=green.11))+
  geom_point(size=2, alpha=0.5)+
  theme_bw()+
  geom_abline(intercept=0, slope=1)+
   xlim(1000, 4000)+
   ylim(1000, 4000)+
  xlab("Greenness (1986)")+
  ylab("Greenness (2011)")+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x = -Inf, y = Inf, hjust = -0.25, vjust = 1.25, label = "D", size = 12)
p.green

# #comparing brightness endpoints to a 1:1 line
b.86 <- rasterToPoints(bright.86)
b.11 <- rasterToPoints(bright.11)

b.86 <- data.frame(b.86)
b.11 <- data.frame(b.11)

colnames(b.86)[3] <- "bright.86"
colnames(b.11)[3] <- "bright.11"

b.86.11 <- merge(b.86, b.11)

p.bright <- ggplot(b.86.11, aes(x=bright.86, y=bright.11))+
  geom_point(size=2, alpha=0.5)+
  theme_bw()+
  geom_abline(intercept=0, slope=1)+
   xlim(2000, 6000)+
   ylim(2000, 6000)+
  xlab("Brightness (1986)")+
  ylab("Brightness (2011)")+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x = -Inf, y = Inf, hjust = -0.25, vjust = 1.25, label = "E", size = 12)
p.bright
# 
# #comparing brightness endpoints to a 1:1 line
w.86 <- rasterToPoints(wet.86)
w.11 <- rasterToPoints(wet.11)

w.86 <- data.frame(w.86)
w.11 <- data.frame(w.11)

colnames(w.86)[3] <- "wet.86"
colnames(w.11)[3] <- "wet.11"

w.86.11 <- merge(w.86, w.11)
# colnames(wet.86.11) <- c("x", "y", "w.86", "w.11")

p.wet <- ggplot(w.86.11, aes(x=wet.86, y=wet.11))+
  geom_point(size=2, alpha=0.5)+
  theme_bw()+
  geom_abline(intercept=0, slope=1)+
  xlim(500, 3000)+
  ylim(500, 3000)+
  xlab("Wetness (1986)")+
  ylab("Wetness (2011)")+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  annotate("text", x = -Inf, y = Inf, hjust = -0.25, vjust = 1.25, label = "F", size = 12)
p.wet
# 
# #plotting gw
# p.gw <- ggplot(gw.86.11, aes(x=gw.86, y=gw.11))+
#   geom_point(size=2, alpha=0.5)+
#   theme_bw()+
#   geom_abline(intercept=0, slope=1)+
#   xlim(-0.2,0.0)+
#   ylim(-0.2,0.0)+
#   xlab("G + W (1986)")+
#   ylab("G + W (2011)")+
#   theme(axis.title.x = element_text(size=20),
#         axis.text.x  = element_text(vjust=0.5, size=16),
#         axis.title.y = element_text(size=20),
#         axis.text.y  = element_text(size=16),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   annotate("text", x = -Inf, y = Inf, hjust = -0.25, vjust = 1.25, label = "F", size = 12)
# p.gw
# 
# 
# plot((1-DI2011$DI), s2011$savi)
# plot((1-DI1986$DI), x1986$ndvi)

# 
# #trying another approach
# library(gtable)
# library(gridExtra)
# 
# # Get the widths
# gA <- ggplot_gtable(ggplot_build(p.ndvi))
# gB <- ggplot_gtable(ggplot_build(p.di))
# gC <- ggplot_gtable(ggplot_build(p.green))
# gD <- ggplot_gtable(ggplot_build(p.gw))
# gX <- ggplot_gtable(ggplot_build(p.bright))
# gZ <- ggplot_gtable(ggplot_build(p.wet))
# 
# 
# maxWidth = unit.pmax(gA$widths[2:3], gB$widths[2:3], 
#                      gC$widths[2:3], gD$widths[2:3])
# 
# # Set the widths
# gA$widths[2:3] <- maxWidth
# gB$widths[2:3] <- maxWidth
# gC$widths[2:3] <- maxWidth
# gD$widths[2:3] <- maxWidth
# gX$widths[2:3] <- maxWidth
# gZ$widths[2:3] <- maxWidth
# 
# 
# # Arrange the four charts
# grid.arrange(gA, gC, gX, gZ, gB, gD, nrow=3)


#####NDVI and streamdistance
pal <- wes_palette("Zissou", 100, type = "continuous")
image(volcano, col = pal)
#modified rushmore
Rushmore2 = c( "#0B775E", "#35274A" ,"#F2300F")

p.stream.ndvi.86 <- ggplot(class_ndvi, aes(x = ndvi.86, y = ndvi.11, color= stream_dist))+
  geom_point(size=3, alpha=0.5)+
  theme_bw()+
  scale_colour_gradientn(colours = pal, "Distance\nfrom stream") + 
  geom_abline(intercept=0, slope=1)+
  xlim(0.25,0.75)+
  ylim(0.25,0.75)+
  xlab("NDVI (1986)")+
  ylab("NDVI (2011)")+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        # theme(legend.justification=c(1,0), legend.position=c(1,0))
        theme(legend.position="none")
x11()
p.stream.ndvi.86

p.stream.ndvi.11 <- ggplot(class_ndvi, aes(x = stream_dist, y = ndvi.11))+
  geom_point(size=4, alpha=0.5)+
  theme_bw()+
  # geom_abline(intercept=0, slope=1)+
  # xlim(0.25,0.75)+
  ylim(0.25,0.85)+
  xlab("Distance from Stream (m)")+
  ylab("NDVI (2011)")+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_smooth(colour="darkgoldenrod1", size=1.5, method="loess", span=0.1)+
  # theme(legend.justification=c(1,0), legend.position=c(1,0))
  theme(legend.position="none")+
  geom_vline(xintercept=143.9, size = 2, color = "#BD3039", linetype = "dashed")
p.stream.ndvi.11

p.stream.ndvi.86 <- ggplot(class_ndvi, aes(x = stream_dist, y = ndvi.86))+
  geom_point(size=4, alpha=0.5)+
  theme_bw()+
  # geom_abline(intercept=0, slope=1)+
  ylim(0.25,0.85)+
  xlab("Distance from Stream (m)")+
  ylab("NDVI (1986)")+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  stat_smooth(colour="darkgoldenrod1", size=1.5, method="loess", span=0.1)+
  # theme(legend.justification=c(1,0), legend.position=c(1,0))
  theme(legend.position="none")+
  geom_vline(xintercept=138.5, size = 2, color = "#BD3039", linetype = "dashed")
p.stream.ndvi.86

p.elev.ndvi <- ggplot(class_ndvi, aes(x = ndvi.86, y = ndvi.11, color= elev))+
  geom_point(size=3, alpha=0.5)+
  theme_bw()+
  scale_color_manual(values = Rushmore2, "Elevation\nclass")+
  geom_abline(intercept=0, slope=1)+
  xlim(0.25,0.75)+
  ylim(0.25,0.75)+
  xlab("NDVI (1986)")+
  ylab("NDVI (2011)")+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    #   theme(legend.justification=c(1,0), legend.position=c(1,0))
        theme(legend.position="none")
p.elev.ndvi
# 
# ##############GW model data
# p.stream.gw <- ggplot(class_gw, aes(x = gw.86, y = gw.11, color= stream_dist))+
#   geom_point(size=3, alpha=0.5)+
#   theme_bw()+
#   scale_colour_gradientn(colours = pal, "Distance\nfrom stream") + 
#   geom_abline(intercept=0, slope=1)+
#   xlim(-0.2,0.0)+
#   ylim(-0.2,0.0)+
#   xlab("G + W (1986)")+
#   ylab("G + W (2011)")+
#   theme(axis.title.x = element_text(size=20),
#         axis.text.x  = element_text(vjust=0.5, size=16),
#         axis.title.y = element_text(size=20),
#         axis.text.y  = element_text(size=16),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   theme(legend.justification=c(1,0), legend.position=c(1,0))
# p.stream.gw
# 
# 
# p.elev.gw <- ggplot(class_gw, aes(x = gw.86, y = gw.11, color= elev))+
#   geom_point(size=3, alpha=0.5)+
#   theme_bw()+
#   scale_color_manual(values = Rushmore2, "Elevation\nClass")+
#   geom_abline(intercept=0, slope=1)+
#   xlim(-0.2,0.0)+
#   ylim(-0.2,0.0)+
#   xlab("G + W (1986)")+
#   ylab("G + W (2011)")+
#   theme(axis.title.x = element_text(size=20),
#         axis.text.x  = element_text(vjust=0.5, size=16),
#         axis.title.y = element_text(size=20),
#         axis.text.y  = element_text(size=16),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#         theme(legend.justification=c(1,0), legend.position=c(1,0))
# p.elev.gw
# 

# 
# ###GRID-ING THOSE PLOTS
# 
# # Get the widths
# gE <- ggplot_gtable(ggplot_build(p.stream.ndvi))
# gF <- ggplot_gtable(ggplot_build(p.elev.ndvi))
# gG <- ggplot_gtable(ggplot_build(p.stream.gw))
# gH <- ggplot_gtable(ggplot_build(p.elev.gw))
# 
# 
# maxWidth = unit.pmax(gE$widths[2:3], gF$widths[2:3], 
#                      gG$widths[2:3], gH$widths[2:3])
# 
# # Set the widths
# gE$widths[2:3] <- maxWidth
# gF$widths[2:3] <- maxWidth
# gG$widths[2:3] <- maxWidth
# gH$widths[2:3] <- maxWidth
# 
# 
# # Arrange the four charts
# grid.arrange(gE, gF, gG, gH, nrow=2)




####################################
# Comparisons of static variables. No more year to year, but I am looking at 2011 shrub versus stream distance


p.stream.ndvi.class <- ggplot(class_ndvi, aes(x = stream_dist, y = ndvi.11))+
  geom_point(size=4, alpha=0.5)+
  #geom_point(data = upland.ndvi, size=4, alpha=0.5 )+
  theme_bw()+
  # scale_colour_gradientn(colours = pal, "Distance\nfrom stream") + 
     xlim(0, 800)+
     ylim(0.25,0.75)+
  xlab("Distance from Stream (m)")+
  ylab("NDVI (2011)")+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  # theme(legend.justification=c(1,0), legend.position=c(1,0))
  geom_smooth(colour="dodgerblue", size=1.5, method="lm", degree = 0, span=0.1, SE = FALSE, data = class_ndvi)+
  theme(legend.position="none")
p.stream.ndvi.class







##### More landscape plots

#looking at aspect and change
plot(lm.ndvi.elev$aspect, lm.ndvi.elev$p.value)
plot(lm.ndvi.elev$aspect, lm.ndvi.elev$change)
plot(lm.ndvi.elev$stream_dist, lm.ndvi.elev$change)
plot(lm.ndvi.elev$stream_dist, lm.ndvi.elev$p.value)

p.stream.p.value <- ggplot(lm.ndvi.elev, aes(x = stream_dist, y = p.value, color = p.class))+
  geom_point()

p.stream.p.value

p.aspect.p.value <- ggplot(lm.ndvi.elev, aes(x = aspect, y = p.value, color = p.class))+
  geom_point()

p.aspect.p.value

p.elev.raw.p.value <- ggplot(lm.ndvi.elev, aes(x = elev.raw, y = p.value, color = p.class))+
  geom_point()+
  xlim(950, 1200)

p.elev.raw.p.value

ndvi.aspect <- aggregate(lm.ndvi.elev$change, by=list(lm.ndvi.elev$aspect.class), FUN=mean)
ndvi.aspect.sd <- aggregate(lm.ndvi.elev$change, by=list(lm.ndvi.elev$aspect.class), FUN=sd)
ndvi.aspect.n <- aggregate(lm.ndvi.elev$change, by=list(lm.ndvi.elev$aspect.class), FUN=length)

ndvi.aspect.change <- merge(ndvi.aspect, ndvi.aspect.sd, by ="Group.1")
ndvi.aspect.change <- merge(ndvi.aspect.change, ndvi.aspect.n, by ="Group.1")
ndvi.aspect.change <- rename(ndvi.aspect.change, c("Group.1" = "aspect.class", "x.x" = "mean_change","x.y" = "sd","x" =  "n" ))
ndvi.aspect.change$SE <- ndvi.aspect.change$sd / (sqrt(ndvi.aspect.change$n))

lm.ndvi.elev$aspect.class = factor(lm.ndvi.elev$aspect.class, c("Northwest", "North", "Northeast", "East", "Southeast","South","Southwest","West","FLAT"))
boxplot(change ~ aspect.class, data = lm.ndvi.elev, ylab = "NDVI change (1986-2011)") +abline(h=0)





