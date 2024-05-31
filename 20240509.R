#### metadata ####
# Above-ground biomass calculation
# 20240509
# Magobadi and Gasebatho 

#### setup ####

#install packages

#install.packages("ggplot2",dependencies=TRUE)
#install.packages("units",dependencies=TRUE)
#install.packages("sf",dependencies=TRUE)
#install.packages("rgdal",dependencies=TRUE)
#install.packages("sf_1.0-16.tar.gz", dependencies=TRUE, repos = NULL, type = "source")
##install.packages("raster", dependencies=TRUE)
#install.packages("terra_1.7-71.tar", dependencies=TRUE, repos = NULL, type = "source")
#install.packages(multicompView", dependencies=TRUE)
#install.packages("cowplot", dependencies=TRUE)

#### load libraries ####
require(rgdal)
library(Rcpp)
library(sf)
library(sp)
library(ggplot2)
library(terra)
library(plyr)
library(readxl)
library(multcompView)
library(cowplot)
library(dplyr)

#setup working directory
getwd()
setwd("C:/Users/UTF_5/Documents/EA 055")

#### import files ####

#load single dataset using dialog box
dataset2 <- read_excel(file.choose())

#or load entire folder #####
#Get the path of filenames
filenames <- list.files("C:/Users/UTF_5/Documents/EA 055", full.names = TRUE)
list_data <- lapply(filenames, read.table)
names(list_data) <- paste('df', seq_along(filenames), sep = '_')
list2env(list_data, .GlobalEnv)

#for now, i am just specifying in the datasheet manually
dataset0 <- read_excel("NFI_35X448Y7577.xlsx")
dataset1 <- read_excel("NFI_35X448Y7577.xlsx")

#### merge datasets ####
dataset<-rbind(dataset0,dataset1)

#### preview files ####
summary(dataset)

#### preview files ####
heads(dataset)

#clear console
cat("\014")  

#### data cleaning ####
#removes empty rows
dataset_final = subset(dataset, !is.na(`Survey date`))

#exploring dataset (vol per stand)
dataset_volByStand <- aggregate(`T_Vol (m3)` ~ Plot_No + `UNIT ID` + `UNIT ID X` + `UNIT ID Y`, data=dataset_final, sum)
View(dataset_volByStand)

dataset_volByStand$`UNIT ID Y` <- dataset_volByStand$`UNIT ID Y`*1000
dataset_volByStand$`UNIT ID X` <- dataset_volByStand$`UNIT ID X`*1000

#adding new columns that depict the centre point for all 3 sampling plots
dataset_final$E <- (max(dataset_final$`UNIT ID Y`)-min(dataset_final$`UNIT ID Y`))/2
dataset_final$N <- (max(dataset_final$`UNIT ID X`)-min(dataset_final$`UNIT ID X`))/2

#### working with shapefiles ####

#districts
dikgaolo <- st_read("000/AdminA_BP 263_V2022.shp")

#visualise districts
ggplot() +
  geom_sf(data = dikgaolo, size = 1.5, color = "black", fill = "cyan1") +
  coord_sf()+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 15, color="#000000"),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(color="#000000", face="bold", size=22),
    axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  ) +
  ylab("latitude")+
  xlab("longitude")

#reserves
forests_outline <- st_read("000/Forest_Reserves.shp")

ggplot() +
  geom_sf(data = forests_outline, size = 1.5, color = "black", fill = "cyan1") +
  coord_sf()+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 15, color="#000000"),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(color="#000000", face="bold", size=22),
    axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  ) +
  ylab("latitude")+
  xlab("longitude")

#dataset as polygon
ggplot(dataset_final, aes(x = `UTM Ave. E.`, y = `UTM Ave. N.`))+
  geom_polygon(aes(fill=`*DBH^X`,group=Plot_No))+
  #theme_minimal() +
  theme(
    legend.background = element_rect(fill = "transparent",color = "transparent"),
    legend.text=element_text(size=17),
    axis.text.x = element_text(size = 15, color="#000000"),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(color="#000000", face="bold", size=22),
    axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
    ) +
  ylab("latitude")+
  xlab("longitude")

# as point
ggplot(dataset_final, aes(x = "UTM Ave. E.", y = "UTM Ave. N."))+
  geom_point(data = dataset_final, mapping = aes(x = `E`, y = `N`), color = '#000000', size = .1, alpha = 0.5)+
  xlab("longitude")+
  ylab("latitude")+
  theme_bw()  

####extract district from tabular data####
dataset_final2 <- dataset_final[c("UTM Ave. E.","UTM Ave. N.")]

dataset_final2 %>% 
  mutate(id = row_number())

datapull <- data.frame(dataset_final2)

#using over function from package sp
test <- data.frame(xx=over(dikgaolo, dataset_final))
combined <- cbind(test, datapull)
combined <- na.omit(combined)
combined

####tabular data####

#tree height distribution
distribution_height <- dataset_final$`Height (m)`
hist(distribution_height,
     main="",
     xlab="height (m)",
     freq=FALSE)

#tree volume distribution
distribution_volume <- dataset_final$`DBH (cm)`
hist(distribution_volume,
     main="",
     xlab=expression("Volume (m" ^ 3*")"),
     freq=FALSE)#,breaks=4,

#dbh distribution
distribution_DBH <- dataset_final$`DBH (cm)`
hist(distribution_DBH,
     main="",
     xlab="dbh (cm)",
     freq=FALSE)

#### same charts using ggplot #####
# histogram with median
p<-ggplot(dataset_final, aes(x=`DBH (cm)`)) + 
  geom_histogram(color="black", fill="white")+ 
  geom_vline(aes(xintercept=mean(`DBH (cm)`)),color="blue", linetype="dashed", linewidth=1)+
  theme_minimal()+
  theme(
    legend.background = element_rect(fill = "transparent",color = "transparent"),
    legend.text=element_text(size=17),
    axis.text.x = element_text(size = 15, color="#000000"),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(color="#000000", face="bold", size=22),
    axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  ) +
  ylab("count")+
  xlab("dbh (cm)")

p


# density plot
p2<-ggplot(dataset_final, aes(y=..density..,x=`DBH (cm)`)) + 
  geom_histogram(color="black", fill="white")+ 
  geom_density(alpha=.2, fill="#FF6666") +
  theme_minimal()+
  theme(
    legend.background = element_rect(fill = "transparent",color = "transparent"),
    legend.text=element_text(size=17),
    axis.text.x = element_text(size = 15, color="#000000"),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(color="#000000", face="bold", size=22),
    axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )+ 
  ylab("density")+
  xlab("dbh (cm)")

p2

# combined plots
plot_grid(p,p2, nrow=1, labels = "auto", label_size = 25, rel_widths = c(2,2))



