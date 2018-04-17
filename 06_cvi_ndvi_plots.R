#06_cvi_ndvi_plots.R
#This script was written by J. Atkins 2015, 2016 (jeffatkins@virginia.edu or @atkinsjeff)
#https://github.com/atkinsjeff
#
#This script makes all of the plots in the manuscript and some bonus stuff!
#requires

require(ggplot2)
require(wesanderson)

plot(ndvi.86,  col=rev( terrain.colors(10)), breaks = seq(0, 1, by = 0.1) )+
     mtext("1986")
plot(ndvi.90,  col=rev( terrain.colors(10)), breaks = seq(0, 1, by = 0.1) )+
     mtext("1990")
plot(ndvi.95,  col=rev( terrain.colors(10)), breaks = seq(0, 1, by = 0.1) )+
     mtext("1995")
plot(ndvi.05,  col=rev( terrain.colors(10)), breaks = seq(0, 1, by = 0.1) )+
     mtext("2005")
plot(ndvi.09,  col=rev( terrain.colors(10)), breaks = seq(0, 1, by = 0.1) )+
     mtext("2009")
plot(ndvi.11,  col=rev( terrain.colors(10)), breaks = seq(0, 1, by = 0.1) )+
     mtext("2011")

#trying another approach
library(gtable)
library(gridExtra)

# Get the widths
gA <- ggplot_gtable(ggplot_build(p.ndvi))
gB <- ggplot_gtable(ggplot_build(p.di))
gC <- ggplot_gtable(ggplot_build(p.green))
gD <- ggplot_gtable(ggplot_build(p.gw))
gX <- ggplot_gtable(ggplot_build(p.bright))
gZ <- ggplot_gtable(ggplot_build(p.wet))


maxWidth = unit.pmax(gA$widths[2:3], gB$widths[2:3], 
                     gC$widths[2:3], gD$widths[2:3])

# Set the widths
gA$widths[2:3] <- maxWidth
gB$widths[2:3] <- maxWidth
gC$widths[2:3] <- maxWidth
gD$widths[2:3] <- maxWidth
gX$widths[2:3] <- maxWidth
gZ$widths[2:3] <- maxWidth


# Arrange the four charts
grid.arrange(gA, gC, gX, gZ, gB, gD, nrow=3)


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
p.stream.ndvi

p.stream.ndvi.11 <- ggplot(class_ndvi, aes(x = stream_dist, y = ndvi.11))+
  geom_point(size=4, alpha=0.5)+
  theme_bw()+
  # geom_abline(intercept=0, slope=1)+
  # xlim(0.25,0.75)+
  ylim(0.25,0.75)+
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
  geom_vline(xintercept=142.4, size = 2, color = "#BD3039", linetype = "dashed")
p.stream.ndvi.11

p.stream.ndvi.86 <- ggplot(class_ndvi, aes(x = stream_dist, y = ndvi.86))+
  geom_point(size=4, alpha=0.5)+
  theme_bw()+
  # geom_abline(intercept=0, slope=1)+
  ylim(0.25,0.75)+
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
  geom_vline(xintercept=136.7, size = 2, color = "#BD3039", linetype = "dashed")
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

##############GW model data
p.stream.gw <- ggplot(class_gw, aes(x = gw.86, y = gw.11, color= stream_dist))+
  geom_point(size=3, alpha=0.5)+
  theme_bw()+
  scale_colour_gradientn(colours = pal, "Distance\nfrom stream") + 
  geom_abline(intercept=0, slope=1)+
  xlim(-0.2,0.0)+
  ylim(-0.2,0.0)+
  xlab("G + W (1986)")+
  ylab("G + W (2011)")+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.justification=c(1,0), legend.position=c(1,0))
p.stream.gw


p.elev.gw <- ggplot(class_gw, aes(x = gw.86, y = gw.11, color= elev))+
  geom_point(size=3, alpha=0.5)+
  theme_bw()+
  scale_color_manual(values = Rushmore2, "Elevation\nClass")+
  geom_abline(intercept=0, slope=1)+
  xlim(-0.2,0.0)+
  ylim(-0.2,0.0)+
  xlab("G + W (1986)")+
  ylab("G + W (2011)")+
  theme(axis.title.x = element_text(size=20),
        axis.text.x  = element_text(vjust=0.5, size=16),
        axis.title.y = element_text(size=20),
        axis.text.y  = element_text(size=16),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(legend.justification=c(1,0), legend.position=c(1,0))
p.elev.gw



###GRID-ING THOSE PLOTS

# Get the widths
gE <- ggplot_gtable(ggplot_build(p.stream.ndvi))
gF <- ggplot_gtable(ggplot_build(p.elev.ndvi))
gG <- ggplot_gtable(ggplot_build(p.stream.gw))
gH <- ggplot_gtable(ggplot_build(p.elev.gw))


maxWidth = unit.pmax(gE$widths[2:3], gF$widths[2:3], 
                     gG$widths[2:3], gH$widths[2:3])

# Set the widths
gE$widths[2:3] <- maxWidth
gF$widths[2:3] <- maxWidth
gG$widths[2:3] <- maxWidth
gH$widths[2:3] <- maxWidth


# Arrange the four charts
grid.arrange(gE, gF, gG, gH, nrow=2)




####################################
# Comparisons of static variables. No more year to year, but I am looking at 2011 shrub versus stream distance


p.stream.ndvi.class <- ggplot(riparian.ndvi, aes(x = stream_dist, y = ndvi.11))+
  geom_point(size=4, alpha=0.5)+
  geom_point(data = upland.ndvi, size=4, alpha=0.5 )+
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
  geom_smooth(colour="dodgerblue", size=1.5, method="lm", degree = 0, span=0.1, SE = FALSE, data = riparian.ndvi)+
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





#filled NDVI density plots by year 
d.gw.pos <- density(gw.lm.stack$change)

plot(d.gw.pos, main = "kernel density of G + W (postive)", xlim=c(-0.3,0.3) )+
  
  polygon(d.gw.pos, col="blue", border="black")
abline(v=0)

d.ndvi<- density(ndvi.stack$change)

plot(d.ndvi, main = "kernel density of NDVI", xlim=c(-0.3,0.3))
polygon(d.ndvi, col="red", border="black")
abline(v=0)

d.di <- density(DI.86.11$di.86)
plot(d.di, main = "kernel density of delta DI")
polygon(d.di, col="red", border="black")
abline(v=0)
xlim=c(-5,5)

#Making density plots of NDVI through time
d.1986 <- density(x1986$ndvi)
d.1990 <- density(x1990$ndvi)
d.1995 <- density(x1995$ndvi)
d.2005 <- density(x2005$ndvi)
d.2009 <- density(x2009$ndvi)
d.2011 <- density(x2011$ndvi)

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


