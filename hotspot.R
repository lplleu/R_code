# kernel density estimate with contour
# 28-12-2016
# jedenfalls

#install.packages('rgdal', type = "source", dependencies=TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("ggspatial", dependencies = TRUE)
#install.packages("ggrepel", dependencies = TRUE)

library(sf)   
require(rgdal)  #enables reading of shapefiles
library(ggplot2) #enables charting application
library(ggspatial)
library(OpenStreetMap)
library(ggrepel) 
library(ggmap)  
####import 'settlements' layer####

settlements <- readOGR(dsn="/cloud/project/",layer="African_Elephant-4089_2020-11-16_14_22_43-point") #botswana_settlements
settlements_fortified <- data.frame(settlements)

elephant4089 <- readOGR(dsn="/cloud/project/",layer="African_Elephant-4089_2020-11-16_14_22_43-point") #botswana_settlements

elephant_fortified4089 <- data.frame(elephant4089)
elephantSubset = subset(elephant_fortified4089, coords.x1!="0.000000°")

elephant <- readOGR(dsn="/cloud/project/",layer="African_Elephant-4088_2021-03-27_12_19_31-line")
elephant_fortified <- data.frame(elephant)
elephantSubset = subset(elephant_fortified, coords.x1!="0.000000°")

####get basemap first ####



myLocation<-c(left = 24, bottom = -20.5, right = 26, top = -19.5)
basemapEle <- ggmap(get_map(location = myLocation, source = "stamen", maptype = "toner", crop = T))

####plot the 'settlements' layer#####
mapElephant <- 
  #ggmap(get_map(location = myLocation, source = "stamen", maptype = "toner", crop = T))+
  ggplot(elephantSubset, aes(x = coords.x1, y = coords.x2))+ 
  geom_point(data = elephantSubset, mapping = aes(x = coords.x1, y = coords.x2), color = '#000000', size = .1, alpha = 0.5) + 
  geom_density_2d(aes(colour = ..level..), bins=7,size=.4) +
  #geom_bin2d(aes(colour = ..level..), bins=7,size=.4) +
  #scale_color_gradient(low='green', high='red') +
  xlab("longitude, E")+
  ylab("latitude, N")+
  coord_map(xlim = c(23.5, 26),ylim = c(-19.5, -20.5))+
  theme_bw()  
mapElephant

####^^^^https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2.html#####

mapElephantWithContour <- mapElephant + 
  geom_density_2d(aes(colour = ..level..), bins=7,size=.4) +
  scale_color_gradient(low='green', high='red')

print(mapElephantWithContour)

base_map_sat <- get_map(location = "Varese", source = "naver", maptype = "satellite")


mmap <- get_navermap(center = c(lon = 25, lat = -20), zoom = 4,baselayer = c("satellite"), color = c("color"))


metse  <- data.frame(x = c(24.5352, 24.6495, 25.2552, 25.826389, 25.438675406074562), y = c(-20.2008, -20.2276, -20.2095, -20.164722, -20.101616808367538), maina = c("Phuduhudu","Nxai Turn-off","Gweta", "Zoroga", "Tsokatshaa"))


mmape <-get_map(location = myLocation, source = "stamen", maptype = "toner", crop = T)

obs_map <- get_map(ice = obs, plotting = TRUE, reg_info,
                   main = "Observed Mapping \n February 1985")

install.packages("leaflet",dependencies=TRUE)
library(leaflet)


map.city <- leaflet() %>% addProviderTiles('Esri.WorldImagery') %>% 
  setView(24, -19, zoom = 11)
map.city %>% addProviderTiles("CartoDB.PositronOnlyLabels")

ggmap(map.city)


devtools::install_github('oswaldosantos/ggsn')
library(ggsn)
my_scalebar <- edit(scalebar)




####start 4088 here####
#read in the shapefile
#elephant4088 <- readOGR(dsn="/cloud/project/",layer="African_Elephant-4088_2021-03-27_12_19_31-point")
elephant4088 <- readOGR(dsn="/cloud/project/",layer="4088")

#fortify to enable reading/manipulation in R
elephant_fortified4088 <- data.frame(elephant4088)

#load basemap
myLocation2<-c(left = 24, bottom = -22.5, right = 27, top = -20)
mmape2 <-get_map(location = myLocation2, source = "stamen", maptype = "toner", crop = T)

#identify relevant villages
metse2  <- data.frame(
  x = c(25.2552, 24.50736261664093,  24.40917,  26.43917, 26.08667, 26.83, 24.86833),
  y = c( -20.2095, -20.47249365368973,  -21.03694, -21.86750, -21.85806, -22.22, -21.20194), 
  maina = c("Gweta","Kumaga","Rakops", "Mmashoro", "Malatswai", "Mabeleapudi", "Mopipi")
)

protectedAreas <- readOGR(dsn="/cloud/project/BotswanaLandUse/",layer="PA") #botswana_settlements
PAs <- fortify(protectedAreas)

mpofuMap2 <- ggmap(mmape2, extent = "device") +
  geom_path(data = PAs, aes(x =long , y = lat, group = group))+ #redo the coordinate system
  geom_point(data = elephant_fortified4088, aes(x = coords.x1, y = coords.x2), size = 0.3) +
  geom_density2d(data = elephant_fortified4088, aes(x = coords.x1, y = coords.x2), size = 0.3) +
  stat_density2d(data = elephant_fortified4088,
                 aes(x = coords.x1, y = coords.x2, fill = ..level.., alpha = ..level..), size = 0.005,
                 bins = 25, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0, 0.3), guide = FALSE)+
  geom_point(data = metse2, aes(x = x, y = y), shape = 21, colour = "black", fill = "white", size = 1, stroke = 1) +
  geom_label_repel(aes(x=x, y=y, label=maina), data=metse2)+ #
  xlab("Longitude, E")+
  ylab("Latitude, N")+ 
  coord_map(xlim = c(24, 27),ylim = c(-20, -22.5))+
  labs(subtitle = "as of 2021-03-27 at 12:19")+
  theme_bw()  +  
  theme(plot.title =  element_text(hjust = 0.5),
        plot.subtitle =  element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")
  ) 


mpofuMap2
#####end 4088 here####

####start 4089 here####

#http://zonums.com/online/kml2shp.php


#read in the shapefile
elephant4089 <- readOGR(dsn="/cloud/project/",layer="African_Elephant-4089_2020-11-16_14_22_43-point")
elephant4089 <- readOGR(dsn="/cloud/project/",layer="4089")

#fortify to enable reading/manipulation in R
elephant_fortified4089 <- data.frame(elephant4089)

#load basemap
myLocation<-c(left = 24, bottom = -20.5, right = 26, top = -19.5)
mmape <-get_map(location = myLocation, source = "stamen", maptype = "toner", crop = T)

#identify relevant villages
metse  <- data.frame(
  x = c(24.5352, 24.6495, 25.2552, 25.826389, 25.438675406074562), 
  y = c(-20.2008, -20.2276, -20.2095, -20.164722, -20.101616808367538), 
  maina = c("Phuduhudu","Nxai Turn-off","Gweta", "Zoroga", "Tsokatshaa")
  )

mpofuMap4089 <- ggmap(mmape, extent = "device") +
  geom_path(data = shapefile_df, aes(x =long , y = lat, group = group))+ #redo the coordinate system
  #geom_polygon(data = protectedAreas_fortified, aes(x = coords.x1, y = coords.x2)) + 
  geom_point(data = elephant_fortified4089, aes(x = coords.x1, y = coords.x2), size = 0.3) +
  geom_density2d(data = elephant_fortified4089, aes(x = coords.x1, y = coords.x2), size = 0.3) +
  stat_density2d(data = elephant_fortified4089,
                 aes(x = coords.x1, y = coords.x2, fill = ..level.., alpha = ..level..), size = 0.005,
                 bins = 25, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0, 0.3), guide = FALSE)+
  geom_point(data = metse, aes(x = x, y = y), color = "black") +
  geom_label_repel(aes(x=x, y=y, label=maina), data=metse)+
  #geom_text(aes(label=maina),hjust=0, vjust=0)+
  xlab("Longitude, E")+
  ylab("Latitude, N")+
  coord_map(xlim = c(24, 26),ylim = c(-19.5, -20.5))+
  #ggtitle("Elephant-4089 by 2020-11-16 at 14:22")+   +  
  labs(subtitle = "as of 2021-03-27 at 12:19")+
  #coord_sf(crs = st_crs(nz), datum = NA)+
  #coord_sf(crs = "+proj=tmerc +lat_0=0 +lon_0=27 +k=1 +x_0=0 +y_0=0 +axis=enu +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") +
  #coord_sf(datum = sf::st_crs(8225))+
  
  #coord_sf(crs = "+proj=ortho +lat_0=-20.5 +lon_0=24") + #focus on this
  #
  #coord_sf(crs = "+proj=utm +zone=36 +south")+
  theme_bw()  +  
  theme(plot.title =  element_text(hjust = 0.5),
        plot.subtitle =  element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")
        ) 


mpofuMap4089
#####end here####




####start 4090 here####
#read in the shapefile
elephant4090 <- readOGR(dsn="/cloud/project/kml2shp/xfiles",layer="points4")

#fortify to enable reading/manipulation in R
elephant_fortified4090 <- data.frame(elephant4090)

#load basemap
myLocation3<-c(left = 23.5, bottom = -20.6, right = 25, top = -19.8)
mmape3 <-get_map(location = myLocation3, source = "stamen", maptype = "toner", crop = T)

#identify relevant villages
metse3  <- data.frame(
  x = c(24.5352, 24.6495, 25.2552, 24.50736261664093, 24.254444), 
  y = c(-20.2008, -20.2276, -20.2095, -20.47249365368973, -20.238611), 
  maina = c("Phuduhudu","Nxai Turn-off","Gweta","Kumaga","Moreomaoto")
)

mpofuMap3 <- ggmap(mmape3, extent = "device") +
  geom_path(data = PAs, aes(x =long , y = lat, group = group))+ #redo the coordinate system
  geom_point(data = elephant_fortified4090, aes(x = coords.x1, y = coords.x2), size = 0.3) +
  geom_density2d(data = elephant_fortified4090, aes(x = coords.x1, y = coords.x2), size = 0.3) +
  stat_density2d(data = elephant_fortified4090,
                 aes(x = coords.x1, y = coords.x2, fill = ..level.., alpha = ..level..), size = 0.005,
                 bins = 25, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0, 0.3), guide = FALSE)+
  geom_point(data = metse3, aes(x = x, y = y), color = "black") +
  geom_label_repel(aes(x=x, y=y, label=maina), data=metse3)+
  xlab("Longitude, E")+
  ylab("Latitude, N")+
  coord_map(xlim = c(23.5, 25),ylim = c(-19.8, -20.6))+
  labs(subtitle = "as of 2021-03-27 at 12:19")+
  theme_bw()  +  
  theme(plot.title =  element_text(hjust = 0.5),
        plot.subtitle =  element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")
  ) 


mpofuMap3
#####end 2 here####




####start 4091 here####
#read in the shapefile
elephant4091 <- readOGR(dsn="/cloud/project/",layer="points2")

#fortify to enable reading/manipulation in R
elephant_fortified4091 <- data.frame(elephant4091)

#load basemap
myLocation4<-c(left = 24, bottom = -21, right = 25.5, top = -20)
mmape4 <-get_map(location = myLocation4, source = "stamen", maptype = "toner", crop = T)

#identify relevant villages
metse4  <- data.frame(
  x = c(24.5352, 24.6495, 25.2552, 24.50736261664093, 24.254444), 
  y = c(-20.2008, -20.2276, -20.2095, -20.47249365368973, -20.238611), 
  maina = c("Phuduhudu","Nxai Turn-off","Gweta","Kumaga","Moreomaoto")
)


mpofuMap4 <- ggmap(mmape4, extent = "device") +
  geom_path(data = PAs, aes(x =long , y = lat, group = group))+ #redo the coordinate system
  geom_point(data = elephant_fortified4091, aes(x = coords.x1, y = coords.x2), size = 0.3) +
  geom_density2d(data = elephant_fortified4091, aes(x = coords.x1, y = coords.x2), size = 0.3) +
  stat_density2d(data = elephant_fortified4091,
                 aes(x = coords.x1, y = coords.x2, fill = ..level.., alpha = ..level..), size = 0.005,
                 bins = 25, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0, 0.3), guide = FALSE)+
  geom_point(data = metse4, aes(x = x, y = y), color = "black") +
  geom_label_repel(aes(x=x, y=y, label=maina), data=metse4)+
  xlab("Longitude, E")+
  ylab("Latitude, N")+
  coord_map(xlim = c(24, 25.5),ylim = c(-20, -21))+
  labs(subtitle = "as of 2021-03-27 at 12:19")+
  theme_bw()  +  
  theme(plot.title =  element_text(hjust = 0.5),
        plot.subtitle =  element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")
  ) 


mpofuMap4
#####end 4091 here####


####start merged here####
#read in the shapefile
elephant_fortified4088$animal <- 4088
elephant_fortified4089$animal <- 4089  
elephant_fortified4090$animal <- 4090
elephant_fortified4091$animal <- 4091

elephantMerged <- rbind(elephant_fortified4088, elephant_fortified4089, elephant_fortified4090, elephant_fortified4091) 
elephantMerged$animal <- as.character(elephantMerged$animal)
#fortify to enable reading/manipulation in R
elephant_fortifiedMerged <- data.frame(elephantMerged)

#load basemap
myLocationMerged<-c(left = 23.75, bottom = -22.5, right = 27, top = -19.5)
mmapeMerged <-get_map(location = myLocationMerged, source = "stamen", maptype = "toner", crop = T)

mpofuMapMerged <- ggmap(mmapeMerged, extent = "device") +
  geom_path(data = PAs, aes(x =long , y = lat, group = group))+ #redo the coordinate system
  
  geom_point(data = elephantMerged, aes(x = coords.x1, y = coords.x2, colour = factor(animal), shape = factor(animal)), size = 0.3) +
  scale_alpha(range = c(0, 0.7), guide = FALSE)+
  
  geom_density2d(data = elephant_fortified4091, aes(x = coords.x1, y = coords.x2), size = 0.3) +
  stat_density2d(data = elephant_fortified4091,
                 aes(x = coords.x1, y = coords.x2, fill = ..level.., alpha = ..level..), size = 0.005,
                 bins = 25, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
  
  xlab("Longitude, E")+
  ylab("Latitude, N")+
  coord_map(xlim = c(23.75, 27),ylim = c(-19.5, -22.5))+
  labs(subtitle = "as of 2021-03-27 at 12:19")+
  theme_bw()  +  
  theme(plot.title =  element_text(hjust = 0.5),
        plot.subtitle =  element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")
  ) 


mpofuMapMerged

mpofuMapMerged2 <- ggmap(mmapeMerged, extent = "device") +
  geom_path(data = PAs, aes(x =long , y = lat, group = group))+ #redo the coordinate system
  
  geom_point(data = elephantMerged, aes(x = coords.x1, y = coords.x2, colour = factor(animal), shape = factor(animal)), size = 0.3) +
  scale_alpha(range = c(0, 0.7), guide = FALSE)+
  
  geom_density2d(data = elephantMerged, aes(x = coords.x1, y = coords.x2), size = 0.3) +
  stat_density2d(data = elephantMerged,
                 aes(x = coords.x1, y = coords.x2, fill = ..level.., alpha = ..level..), size = 0.005,
                 bins = 25, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
  
  xlab("Longitude, E")+
  ylab("Latitude, N")+
  coord_map(xlim = c(23.75, 27),ylim = c(-19.5, -22.5))+
  labs(subtitle = "as of 2021-03-27 at 12:19")+
  theme_bw()  +  
  theme(plot.title =  element_text(hjust = 0.5),
        plot.subtitle =  element_text(hjust = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")
  ) 

mpofuMapMerged2
#####end merged here####



install.packages("cowplot",dependencies=TRUE)
library(cowplot)

plot_grid(mpofuMap2, mpofuMap4089, mpofuMap3, mpofuMap4, labels = c('Elephant-4088', 'Elephant-4089', 'Elephant-4090', 'Elephant-4091'), label_size = 12, align="hv")

####import 'settlements' layer####



'''
leaflet(philly_WGS84) %>%
  addPolygons(
    stroke = FALSE, 
    fillColor = ~pal_fun(homic_rate),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = p_popup,
    group = "philly") %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Carto") %>%
  addLegend("bottomright", 
            colors = brewer.pal(7, "YlOrRd"), 
            labels = paste0("up to ", format(breaks_qt$brks[-1], digits = 2)),
            title = 'Philadelphia homicide density per sqkm') %>%
  addLayersControl(baseGroups = c("OSM", "Carto"), 
                   overlayGroups = c("philly"))  
'''


protectedAreas <- readOGR(dsn="/cloud/project/BotswanaLandUse/",layer="PA") #botswana_settlements
protectedAreas_fortified <- data.frame(protectedAreas)

shapefile_df <- fortify(protectedAreas)
ggplot()+geom_path(data = shapefile_df, aes(x =long , y = lat, group = group)) #redo the coordinate system

ggplot()+
geom_path(data = shapefile_df, aes(x =long , y = lat, group = group))+ #redo the coordinate system
  coord_map()
#coord_map(xlim = c(20, 29),ylim = c(-25, -17))

#elephantSubset = subset(elephant_fortified, coords.x1!="0.000000°")


#####add-satelite-image#####
#install.packages("ggmap",dependencies=TRUE)

#remove.packages(rlang)
#install.packages("rlang")
#update.packages("rlang")


library(ggmap)

myLocation<-c(left = 23, bottom = -20.5, right = 26, top = -19)

#myMap<-get_stamenmap(myLocation, zoom = 5, maptype = "toner-lite")
#ggmap(myMap)
#lnd <- readOGR(dsn="/cloud/project/",layer="African_Elephant-4089_2020-11-16_14_22_43-point") #botswana_settlements
#b <- bbox(lnd)
#b
basemapEle <- ggmap(get_map(location = myLocation, source = "stamen", maptype = "toner", crop = T))

mapElephant <- basemapEle+ 
  geom_point(color = 'purple', size = .2)+ 
  theme_bw() +
  #color = '#000000'+
  xlab("longitude, E")+
  ylab("latitude, N") 
#scale_y_continuous(labels = settlements_fortified$coords.x2)

#window()
print(mapElephant)



#myMap2<-get_map(location=myLocation, source="osm",  color="bw")

#myMap3<-get_openstreetmap(bbox = c(left = 22, bottom = -20, right = 24, top = -18.5), 
#                  format = c("jpeg"), messaging = FALSE, urlonly = FALSE,
#                  filename = NULL, color = c("color"))
#help("Defunct")
#ggmap(myMap3)


#summary(mapElephant)
#headers(mapElephant)

####import 'settlements' layer####

settlements <- readOGR(dsn="/cloud/project/",layer="African_Elephant-4089_2020-11-16_14_22_43-point")
settlements <- readOGR(dsn="/cloud/project/",layer="wattled_crane_2001")
settlements_fortified <- data.frame(settlements)

####plot the 'settlements' layer#####
mapSettlements <- ggplot(settlements_fortified, 
                         aes(x = coords.x1, y = coords.x2))+ 
  geom_point(color = 'purple', size = .2)+ 
  theme_bw() +
  #color = '#000000'+
  xlab("longitude, E")+
  ylab("latitude, N") +
  coord_map(xlim = c(22.0, 24),ylim = c(-18.5, -20))
  #scale_y_continuous(labels = settlements_fortified$coords.x2)

print(mapSettlements)

####add projection to 'settlements' layer####
mapSettlements <- mapElephant + coord_map()
mapSettlements <- mapSettlements + coord_map()

####add contours to 'settlements' layer####
mapSettlementsWithContour <- mapSettlements + 
  geom_density_2d(aes(colour = ..level..),
                  bins=7,size=.4
                  )+
  scale_color_gradient(low='green', high='red')#+
  #coord_map(xlim = c(22.0, 24),ylim = c(-18.5, -20))

  #or palegreen and red

print(mapSettlementsWithContour)

####load border shapefile####
border <- readOGR(dsn="/cloud/project/",layer="Ngamiland_CHAs")
border_fortified <- fortify(border)


####border###
border <- readOGR(dsn="/cloud/project/",layer="cha")
border_fortified <- fortify(border)

Nhabe <- ggplot(border_fortified) +
  #theme_bw() +
  geom_sf(aes(fill = group))+#library("sf")
  geom_sf_text(aes(label = piece), colour = "black")
  #geom_polygon(data = border_fortified, aes(group=group), colour='black', fill=NA) +
  #geom_path(data = border_fortified,aes(x = long, y = lat, group = group),color = '#000000', size = .2)+
  #geom_text(data=border_fortified, aes(x = long, y = lat, label = piece), size=3) + #add labels at centroids
  #xlab("longitude, E")+
  #ylab("latitude, N")
#geom_point()


####addProjection####
Nhabe_projected <- Nhabe +
  coord_map()

print(Nhabe_projected)


####add projections to 'border' layer####


####final map with border####
bwSettlementsWithContour <- mapSettlementsWithContour+
  geom_path(data = border_fortified, 
            aes(x = long, y = lat, group = group),
            color = '#000000', size = .2)

print(bwSettlementsWithContour)

#...id rather have the contours vary in colour or size [use c() for custom values??]

####add heatmap####
bwSettlementsWithContour + 
  stat_density_2d(aes(fill = stat(level)), geom = "polygon", colour="black") +
  scale_fill_distiller() #settlements go up to 18

####convert to polygon to enable labelling##

polygon <- as(border, "SpatialLinesDataFrame")
polygon <- as(polygon, "SpatialLinesDataFrame")
writeOGR(polygon, dsn="/cloud/project/", layer="pathCHA", driver="ESRI Shapefile", overwrite_layer=TRUE)




centroid <- aggregate(cbind(long,lat) ~ id, data=borderCHA_fortified, FUN=mean)
testtext <- ggplot() +
  geom_polygon(data = borderCHA_fortified, mapping = aes(x=long, y=lat, group = group, fill=id)) +
  geom_text(data = centroid, mapping = aes(x=long, y=lat, label=id)) +
  geom_path(color = "white") +
  scale_fill_hue(l=40) +
  coord_equal() +
  theme(legend.position = "none", title = element_blank(), axis.text = element_blank())

testtext
#https://stackoverflow.com/questions/42225987/geom-text-finding-centroids-and-adding-text-in-a-polygon-using-ggplot2

centroid$name <- c("NG/13","NG/14","NG/11","NG/10","NG/15","NG/7","NG/1","NG/10","NG/16","NG/12","NG/6","NG/20","NG/18","NG/2","NG/22","NG/24","NG/23","NG/28","U","NG/41","NG/42","NG/21","NG/3","NG/19","NG/25","NG/8","NG/26","NG/34","NG/43","NG/31","NG/27A","NG/33","NG/17","NG/38","NG/27B","NG/30","NG/52","NG/48","NG/29","NG/35","NG/4","U0","NG/45","NG/36","NG/9","NG/5","NG/45","NG/51","NG/37","NG/52","U1","NG/39","U2","NG/38x","U3","U4")
centroid$name <- c("CH/3","NG/13","NG/12","NG/22","NG/5","NG/18","NG/2","NG/6","NG/24","NG/23","NG/28","D","NG/14","NG/41","NG/42","NG/21","NG/3","NG/19","NG/25","NG/8","NG/26","NG/0yyyyy","NG/43","NG/11","NG/31","NG/27A","NG/34","NG/17","NG/32","NG/27B","NG/30","NG/47","NG/48","NG/29","NG/10","NG/35","NG/4D","Dy","NG/45","NG/36","NG/9","NG/5","NG/49","NG/51","NG/37","NG/15","NG/49","Dw","NG/39","toDel_NG/4","NG/38","Dz","NG/7","NG/1","Dv","NG/16")

centroid <- subset(centroid, name!="Dv")
centroid <- subset(centroid, name!="Dz")
centroid <- subset(centroid, name!="D")
centroid <- subset(centroid, name!="Dy")
centroid <- subset(centroid, name!="Dw")
centroid <- subset(centroid, name!="toDel_NG/4")
centroid <- subset(centroid, name!="NG/0yyyyy")








#library("sf")

#borderCHA <- readOGR(dsn="/cloud/project/",layer="pathCHA")
#borderCHA

#borderCHA_fortified <- fortify(borderCHA)
#check
polyNhabe <- ggplot(borderCHA_fortified) +
  theme_bw() +
  geom_polygon(data = borderCHA_fortified, aes(x = long, y = lat, group=group), colour='gray', fill=NA) +  
  geom_text(data = centroid, mapping = aes(x=long, y=lat, label=name), size=4) 
  #geom_text(data=borderCHA_fortified, aes(x = long, y = lat, label = order), size=3)  #add labels at centroids

####addProjection####
polyNhabe_projected <- polyNhabe +
  coord_map()

print(polyNhabe_projected)


