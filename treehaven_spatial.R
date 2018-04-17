# TREEHAVEN TEST
# doing some more work on talladega
require(plyr)
require(dplyr)
require(tidyr)
require(raster)
require(rgdal)
require(maps)
require(ggplot2)
require(cluster)
require(wesanderson)
### bring in the csc
neon.csc <- read.csv("./data/laserquest_pcl_master_transects_2018_clean.csv")


neon.csc %>%
  group_by(plotID, siteID) %>%
  summarise_all(funs(mean)) -> csc.means

csc.means %>%
  filter(siteID == "TREE") -> TREE.csc

TREE.csc <- data.frame(TREE.csc)

#adds that 0
require(stringi)
stri_sub(TREE.csc$plotID, 6, 5) <- 0

# bring in spatial data
#points <- read.csv("c:/github/laserquestR/pointID_spatialData_towerPlots.csv")
points <- read.csv("c:/github/laserquestR/NEON_spatial_data_tower_distributed_EAGER_project.csv")

# this filters down to just the TOWER points
points %>%
  #filter(pointID == 41) -> center.points
  filter(plotType == "tower") -> center.points


TREE <- merge(TREE.csc, center.points, all.x = TRUE)

#rugosity bt elevation
p <- ggplot(TALL, aes(x = elevation, y = rugosity, color = nlcdClass))+
  geom_point(size = 4)+
  #scale_color_manual(values = wes_palette("Darjeeling"))+
  scale_color_manual(values=c("#FF0000", "#00A08A", "#F2AD00"), 
                     name="",
                     breaks=c("evergreenForest", "mixedForest", "deciduousForest"),
                     labels=c("Evergreen", "Mixed", "Deciduous"))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )+
  theme(axis.title = element_text(face="bold", colour="#FFFFFF", size=20),
        axis.text  = element_text(color = "#FFFFFF", size = 16),
        axis.line.x=element_line(color = "#FFFFFF"),
        axis.line.y=element_line(color = "#FFFFFF"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="#FFFFFF", size=10, 
                                   face="bold"),
        legend.position = "bottom")+
  ylab("Rugosity")+
  xlab("Elevation (m)")


ggsave("elev_rugo.png", p, width = 6, height = 4, units = "in", bg = "transparent")

#vai by elevation
q <- ggplot(TALL, aes(x = elevation, y = mean.vai, color = nlcdClass))+
  geom_point(size = 4)+
  #scale_color_manual(values = wes_palette("Darjeeling"))+
  scale_color_manual(values=c("#FF0000", "#00A08A", "#F2AD00"), 
                     name="",
                     breaks=c("evergreenForest", "mixedForest", "deciduousForest"),
                     labels=c("Evergreen", "Mixed", "Deciduous"))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )+
  theme(axis.title = element_text(face="bold", colour="#FFFFFF", size=20),
        axis.text  = element_text(color = "#FFFFFF", size = 16),
        axis.line.x=element_line(color = "#FFFFFF"),
        axis.line.y=element_line(color = "#FFFFFF"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="#FFFFFF", size=10, 
                                   face="bold"),
        legend.position = "bottom")+
  ylab("VAI")+
  xlab("Elevation (m)")


ggsave("elev_vai.png", q, width = 6, height = 4, units = "in", bg = "transparent")

# meanheight by elevation
r <- ggplot(TALL, aes(x = elevation, y = mean.height, color = nlcdClass))+
  geom_point(size = 4)+
  #scale_color_manual(values = wes_palette("Darjeeling"))+
  scale_color_manual(values=c("#FF0000", "#00A08A", "#F2AD00"), 
                     name="",
                     breaks=c("evergreenForest", "mixedForest", "deciduousForest"),
                     labels=c("Evergreen", "Mixed", "Deciduous"))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )+
  theme(axis.title = element_text(face="bold", colour="#FFFFFF", size=20),
        axis.text  = element_text(color = "#FFFFFF", size = 16),
        axis.line.x=element_line(color = "#FFFFFF"),
        axis.line.y=element_line(color = "#FFFFFF"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="#FFFFFF", size=10, 
                                   face="bold"),
        legend.position = "bottom")+
  ylab("Mean Leaf Height")+
  xlab("Elevation (m)")


ggsave("elev_height.png", r, width = 6, height = 4, units = "in", bg = "transparent")

# enl vs. elevation
# meanheight by elevation
t <- ggplot(TREE, aes(x = elevation, y = mean.max.ht, color = nlcdClass))+
  geom_point(size = 4)+
  #scale_color_manual(values = wes_palette("Darjeeling"))+
  scale_color_manual(values=c( "#00A08A", "#F2AD00"), 
                     name="",
                     breaks=c("mixedForest", "deciduousForest"),
                     labels=c("Mixed", "Deciduous"))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )+
  theme(axis.title = element_text(face="bold", colour="#FFFFFF", size=20),
        axis.text  = element_text(color = "#FFFFFF", size = 16),
        axis.line.x=element_line(color = "#FFFFFF"),
        axis.line.y=element_line(color = "#FFFFFF"),
        legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(colour="#FFFFFF", size=10, 
                                   face="bold"),
        legend.position = "bottom")+
  ylab("MOCH")+
  xlab("Elevation (m)")


ggsave("elev_moch_tree.png", t, width = 6, height = 4, units = "in", bg = "transparent")
######################


#### classes
#  6 = mixedforest
#  3 = deciduous
#### CLASSIFCATION

require(randomForest)
tree.csc <- TREE
tree.csc$nlcdClass <- unclass(tree.csc$nlcdClass)
set.seed(415)

r.fit.tree <- randomForest(as.factor(nlcdClass) ~ mean.height + mean.max.ht + mean.vai + deep.gap.fraction +
                        porosity + rugosity + sky.fraction + clumping.index + enl +
                        elevation,
                      data = tree.csc,
                      importance = TRUE,
                      ntree = 2000)
# View the forest results.
print(r.fit.tree) 

# Importance of each predictor.
print(importance(r.fit.tree)) 

varImpPlot(r.fit.tree)

### with just structure
r.fit.csc.tree <- randomForest(as.factor(nlcdClass) ~ mean.height + mean.max.ht + mean.vai + deep.gap.fraction +
                            porosity + rugosity + sky.fraction + clumping.index + enl,
                          data = tree.csc,
                          importance = TRUE,
                          ntree = 2000)
# View the forest results.
print(r.fit.csc.tree) 

# Importance of each predictor.
print(importance(r.fit.csc.tree)) 

varImpPlot(r.fit.csc.tree)










