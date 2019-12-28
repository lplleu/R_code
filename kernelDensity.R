# kernel density estimate with contour
# 28-12-2016
# jedenfalls

#install.packages('rgdal', type = "source", dependencies=TRUE)
#install.packages("ggplot2", dependencies = TRUE)

require(rgdal)  #enables reading of shapefiles
library(ggplot2) #enables charting application

####import 'settlements' layer####

settlements <- readOGR(dsn="/cloud/project/",layer="botswana_settlements")
settlements_fortified <- data.frame(settlements)

####plot the 'settlements' layer#####
mapSettlements <- ggplot(settlements_fortified, 
                         aes(x = coords.x1, y = coords.x2)) +
                         theme_bw() +
                         #color = '#000000'+
                         geom_point(color = '#000000', size = .1) +
                         xlab("longitude, E")+
                         ylab("latitude, N")

print(mapSettlements)

####add projection to 'settlements' layer####
mapSettlements <- mapSettlements + coord_map()

####add contours to 'settlements' layer####
mapSettlementsWithContour <- mapSettlements + 
  geom_density_2d(aes(colour = ..level..),
                  bins=7,size=.4
                  )+
  scale_color_gradient(low='white', high='black')

  #or palegreen and red

print(mapSettlementsWithContour)

####load border shapefile####
border <- readOGR(dsn="/cloud/project/",layer="BOT_outline")
border_fortified <- fortify(border)

####add projections to 'border' layer####

####final map with border####
bwSettlementsWithContour <- mapSettlementsWithContour+
  geom_path(data = border_fortified, 
            aes(x = long, y = lat, group = group),
            color = '#000000', size = .2)

print(bwSettlementsWithContour)
