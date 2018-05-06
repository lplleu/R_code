#jedenfalls
#updated nesting numbers R script
#06-May-2018

####load libraries####
library(dplyr) ; library(tidyr)
library(reshape2)
library(ggplot2) #visually responsive charts
library(ggpubr)
library(lsmeans)
library(lme4)
library(plyr)
library(Rmisc) #summarySE
library(cowplot) #numbering charts
library(multcompView) #several charts in 1
library(scales) #enable integers in axis

####load csv####
#the following data still misses 2016, but that is because in 2016 most nesting species were "unknown"
performance <- read.csv("E:/2018/Apr/26/raptorsBW.csv", sep=";")

####restructure for Linyanti####
#subset for Linyanti, because AA said not to aggregate, it seems comparable surveys for LNY were in the midseason
activeLNYmid <- subset(performance, location=="Linyanti"  & mid==1) #& Year!=2006 & Year!=2008 & Year!=2016

#in case I have to use early
activeLNYearly <- subset(performance, location=="Linyanti" & early==1) #Year!=2006 & Year!=2008 & Year!=2016 & 

#what to add to the midseason
addLNY   <- subset(performance, location=="Linyanti"  & add2m==1) #& Year!=2006 & Year!=2008 & Year!=2016

DataLNY  = summarySE(data=activeLNYmid,"mid", groupvars="Year", conf.interval = 0.95) 

DataLNY2 = summarySE(data=addLNY,"add2m", groupvars="Year", conf.interval = 0.95) 
DataLNY2

#rename "add2m" to "mid" so I can collate the subsets
DataLNY2 <- dplyr::rename(DataLNY2, mid = add2m)  #  df <- rename(df, new_name = old_name)

DataLNY["stage"]<-"og"
DataLNY2["stage"]<-"ad"

#merging the 2 subsets
DataLNY3<-rbind(DataLNY,DataLNY2)

#for later, add a column called Linyanti
DataLNY3["location"]<-"Linyanti"
DataLNY3

###///////from preliminary assessment, add seems more, so I might have to use "early"
####end Linyanti structuring####

#offset.v = -1     # offsets for data labels
#offset.h = 0.5

p1<-ggplot(data=DataLNY3, aes(x=as.factor(Year), y=N, fill="LNY", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("gray")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
 # coord_cartesian(ylim=c(8,100))+ 
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "mid-season nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .95),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=17),
        axis.text=element_text(size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=19,vjust= 1.8),
        axis.title.x = element_text(size=19,vjust= -0.5),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )



#####and now, KHW#####
#select for those with early surveys
activeKHW <- subset(performance, location=="Khwai" & early==1) #& Year!=2006 & Year!=2008 & Year!=2016
#select for those with nests extra to early ones
addKHW  <- subset(performance, location=="Khwai"  & add2e==1) #& Year!=2006 & Year!=2008 & Year!=2016

#select for those with midseason nests
activeKHW2 <- subset(performance, location=="Khwai" & mid==1) #& Year!=2006 & Year!=2008 & Year!=2016
#select for nests that are extra to midseason
addKHW2    <- subset(performance, location=="Khwai"  & add2m==1) #& Year!=2006 & Year!=2008 & Year!=2016

#summarise early nests
DataKHW = summarySE(data=activeKHW,"early", groupvars="Year", conf.interval = 0.95) 
#summarise additional nests
DataKHW2 = summarySE(data=addKHW,"add2e", groupvars="Year", conf.interval = 0.95) 

DataKHW2 <- dplyr::rename(DataKHW2, early = add2e)  #  df <- rename(df, new_name = old_name)

DataKHW["stage"]<-"og"
DataKHW2["stage"]<-"ad"

#DataKHW <- dplyr::rename(DataKHW, early = add2e)

DataKHW3<-rbind(DataKHW,DataKHW2)
DataKHW3["location"]<-"Khwai"

####end of khwai restructuring####
#there seems to be nothing in early though^^

offset.v = -1.5     # offsets for data labels
offset.h = 0.5

p2<-ggplot(data=DataKHW3, aes(x=as.factor(Year), y=N, fill="KHW", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
 # coord_cartesian(ylim=c(8,100))+ 
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "mid-season nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .89),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=17),
        axis.text=element_text(size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=19,vjust= 1.8),
        axis.title.x = element_text(size=19,vjust= -0.5),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )

####structure data for AFE @ KHW in midseason####
AFEdataKHW2 <- subset(activeKHW2, species=="AFE" & location=="Khwai")
#summarised
dataAFE_KHW = summarySE(data=AFEdataKHW2,"mid", groupvars="Year", conf.interval = 0.95) 
#^^^^NaNs produced due to there being only a single record for 2017, hence no variation to give SD/SE/CI
#mark the orginals so that the bars can be differenciated for calculating minimum nesting number later
dataAFE_KHW["stage"]<-"og"

####structure data for AFE @ KHW in additional(mid) nests####
AFEdataKHW2a = summarySE(data=addKHW,"add2m", groupvars="Year", conf.interval = 0.95) 
#rename to enable merging
dataAFE_KHWa <- dplyr::rename(AFEdataKHW2a, mid = add2m)   #  df <- rename(df, new_name = old_name)
#`add2m` contains NAs so can't be renamed. replace NAs with 0s?
#hold off for now
#dataAFE_KHWa$add2m[is.na(dataAFE_KHWa$add2m)] <- 0

#mark the additionals
dataAFE_KHWa["stage"]<-"ad"

#merge the 2 datasets
DataAFEKHW3<-rbind(dataAFE_KHW,dataAFE_KHWa)

#not sure why I'm doing this for now
DataAFEKHW3["location"]<-"KHW"
####end of restructuring for midseason####

#####now, do the same thing  for AFE  @ KHW in early season####
AFEdataKHW2b <- subset(activeKHW2, species=="AFE" & location=="Khwai")
dataAFE_KHW2b = summarySE(data=AFEdataKHW2b,"early", groupvars="Year", conf.interval = 0.95)
#AFE_KHW2a <- dplyr::rename(dataAFE_KHW2b, mid = add2e)
dataAFE_KHW2b["stage"]<-"og"

####structure data for AFE @ KHW in additional(early) nests####
AFEdataKHW2c <- subset(addKHW2, species=="AFE" & location=="Khwai")
dataAFE_KHW2c = summarySE(data=AFEdataKHW2c,"add2e", groupvars="Year", conf.interval = 0.95)
dataAFE_KHW2c <- dplyr::rename(dataAFE_KHW2c, early = add2e) #sometimes throws error the first time around
dataAFE_KHW2c["stage"]<-"ad"
AFE_KHW2d<-rbind(dataAFE_KHW2b,dataAFE_KHW2c)
AFE_KHW2d["location"]<-"KHW"

####end of early AFE KHW restructuring#####

#the problem here is that the midseason and late season are perculilarly similar, 
#also the minimums should be the same just distributed differently, however, DataAFEKHW3 goes up to the late 20s while AFE_KHW2d only goes to less than 10

SBSdataKHW2 <- subset(activeKHW2, species=="SBS" & location=="Khwai")
dataSBS_KHW2 = summarySE(data=SBSdataKHW2,"mid", groupvars="Year", conf.interval = 0.95) 
#AHE_KHW <- dplyr::rename(dataAHE_KHW2, mid = mid)
SBSdataKHW2 <- subset(addKHW, species=="SBS" & location=="Khwai")
dataSBS_KHW2 = summarySE(data=SBSdataKHW2,"add2e", groupvars="Year", conf.interval = 0.95)
SBS_KHW2 <- dplyr::rename(dataSBS_KHW2, mid = add2e)
SBS_KHW["stage"]<-"og"
SBS_KHW2["stage"]<-"ad"
SBS_KHW2["location"]<-"Khwai"
SBS_KHW3<-rbind(SBS_KHW,SBS_KHW2)


AHEdataKHW2 <- subset(activeKHW2, species=="AHE" & location=="Khwai")
dataAHE_KHW2 = summarySE(data=AHEdataKHW2,"mid", groupvars="Year", conf.interval = 0.95) 
#AHE_KHW <- dplyr::rename(dataAHE_KHW2, mid = mid)
AHEdataKHW2 <- subset(addKHW, species=="AHE" & location=="Khwai")
dataAHE_KHW2 = summarySE(data=AHEdataKHW2,"add2e", groupvars="Year", conf.interval = 0.95)
AHE_KHW2 <- dplyr::rename(dataAHE_KHW2, mid = add2m)
AHE_KHW["stage"]<-"og"
AHE_KHW2["stage"]<-"ad"
AHE_KHW2["location"]<-"Khwai"
AHE_KHW3<-rbind(AHE_KHW,AHE_KHW2)

MEdataKHW2<- subset(activeKHW2, species=="ME" & location=="Khwai")
dataME_KHW = summarySE(data=MEdataKHW2,"mid", groupvars="Year", conf.interval = 0.95) 
#ME_KHW <- dplyr::rename(dataME_KHW, active_early = mid)
MEdataKHW2 <- subset(addKHW, species=="ME" & location=="Khwai")
dataME_KHW2 = summarySE(data=MEdataKHW2,"add2e", groupvars="Year", conf.interval = 0.95)
ME_KHW2 <- dplyr::rename(dataME_KHW2, mid = add2e)
dataME_KHW["stage"]<-"og"
ME_KHW2["stage"]<-"ad"
ME_KHW2["location"]<-"Khwai"
dataME_KHW["location"]<-"Khwai"
ME_KHW3<-rbind(dataME_KHW,ME_KHW2)

TEdataKHW2 <- subset(activeKHW2, species=="TE" & location=="Khwai")
dataTE_KHW = summarySE(data=TEdataKHW2,"mid", groupvars="Year", conf.interval = 0.95) 
#TE_KHW <- dplyr::rename(dataTE_KHW,   mid = active_early)
TEdataKHW2 <- subset(addKHW, species=="TE" & location=="Khwai")
dataTE_KHW2 = summarySE(data=TEdataKHW2,"add2m", groupvars="Year", conf.interval = 0.95)
TE_KHW2 <- dplyr::rename(dataTE_KHW2, mid = add2m)
TE_KHW["stage"]<-"og"
TE_KHW2["stage"]<-"ad"
TE_KHW2["location"]<-"Khwai"
#TE_KHW["location"]<-"Khwai"
TE_KHW3<-rbind(TE_KHW,TE_KHW2)

AFE_KHW["location"]<-"Khwai"
AHE_KHW["location"]<-"Khwai"
ME_KHW["location"]<-"Khwai"
TE_KHW["location"]<-"Khwai"
AFEdataKHW2 <- subset(activeKHWearly2, species=="AFE" & location=="Khwai")
dataAFE_KHW2 = summarySE(data=AFEdataKHW2,"add2e", groupvars="Year", conf.interval = 0.95)
AFE_KHW2 <- dplyr::rename(dataAFE_KHW2, active_early = add2e)
AFE_KHW["stage"]<-"og"
AFE_KHW2["stage"]<-"ad"
#AFE_KHW2["location"]<-"Khwai"
AFE_KHW3<-rbind(AFE_KHW,AFE_KHW2)

##

SBSdataLNY <- subset(activeLNYearly, species=="SBS" & location=="Linyanti")
dataSBS_LNY = summarySE(data=SBSdataLNY,"early", groupvars="Year", conf.interval = 0.95)
SBS_LNY <- dplyr::rename(dataSBS_LNY, active_early = early)

SBSdataLNY2 <- subset(activeLNYearly2, species=="SBS" & location=="Linyanti")
dataSBS_LNY2 = summarySE(data=SBSdataLNY2,"add2e", groupvars="Year", conf.interval = 0.95)
SBS_LNY2 <- dplyr::rename(dataSBS_LNY2, active_early = add2e)
SBS_LNY["stage"]<-"og"
SBS_LNY2["stage"]<-"ad"
#SBS_LNY2["location"]<-"Linyanti"
SBS_LNY3<-rbind(SBS_LNY,SBS_LNY2)

AFEdataLNY <- subset(activeLNYearly, species=="AFE" & location=="Linyanti")
dataAFE_LNY = summarySE(data=AFEdataLNY,"early", groupvars="Year", conf.interval = 0.95)
AFE_LNY <- dplyr::rename(dataAFE_LNY, active_early = early)

AFEdataLNY2 <- subset(activeLNYearly2, species=="AFE" & location=="Linyanti")
dataAFE_LNY2 = summarySE(data=AFEdataLNY2,"add2e", groupvars="Year", conf.interval = 0.95)
AFE_LNY2 <- dplyr::rename(dataAFE_LNY2, active_early = add2e)
AFE_LNY["stage"]<-"og"
AFE_LNY2["stage"]<-"ad"
#AFE_LNY2["location"]<-"Linyanti"
AFE_LNY3<-rbind(AFE_LNY,AFE_LNY2)

AHEdataLNY <- subset(activeLNYearly, species=="AHE" & location=="Linyanti")
dataAHE_LNY = summarySE(data=AHEdataLNY,"early", groupvars="Year", conf.interval = 0.95) 
AHE_LNY <- dplyr::rename(dataAHE_LNY, active_early = early)
AHEdataLNY2 <- subset(activeLNYearly2, species=="AHE" & location=="Linyanti")
dataAHE_LNY2 = summarySE(data=AHEdataLNY2,"add2e", groupvars="Year", conf.interval = 0.95) 
AHE_LNY2 <- dplyr::rename(dataAHE_LNY2, active_early = add2e)
AHE_LNY["stage"]<-"og"
AHE_LNY2["stage"]<-"ad"
AHE_LNY2["location"]<-"Linyanti"
AHE_LNY["location"]<-"Linyanti"
AHE_LNY3<-rbind(AHE_LNY,AHE_LNY2)


MEdataLNY <- subset(activeLNYearly, species=="ME" & location=="Linyanti")
dataME_LNY = summarySE(data=MEdataLNY,"early", groupvars="Year", conf.interval = 0.95) 
ME_LNY <- dplyr::rename(dataME_LNY, active_early = early)
ME_LNY[nrow(ME2) + 1,] = c(2017,0,"","","","","")
MEdataLNY2 <- subset(activeLNYearly2, species=="ME" & location=="Linyanti")
dataME_LNY2 = summarySE(data=MEdataLNY2,"early", groupvars="Year", conf.interval = 0.95) 
ME_LNY <- dplyr::rename(dataME_LNY2, active_early = early)

TEdataLNY <- subset(activeLNYearly, species=="TE" & location=="Linyanti")
dataTE_LNY = summarySE(data=TEdataLNY,"early", groupvars="Year", conf.interval = 0.95) 
TE_LNY <- dplyr::rename(dataTE_LNY, active_early = early)
TEdataLNY2 <- subset(activeLNYearly2, species=="TE" & location=="Linyanti")
dataTE_LNY2 = summarySE(data=TEdataLNY2,"early", groupvars="Year", conf.interval = 0.95) 
TE_LNY2 <- dplyr::rename(dataTE_LNY2, active_early = early)
TE_LNY["stage"]<-"og"
TE_LNY2["stage"]<-"ad"
TE_LNY2["location"]<-"Linyanti"
TE_LNY["location"]<-"Linyanti"
TE_LNY3<-rbind(TE_LNY,TE_LNY2)

AFE_LNY3["location"]<-"Linyanti"
AHE_LNY["location"]<-"Linyanti"
ME_LNY["location"]<-"Linyanti"

#add the supplementary data from non-primary surveys
#AFE <- rbind(AFE1,AFE2) 
#AHE <- rbind(AHE1,AHE2) 
#ME <- rbind(ME1,ME2) 
#TE <- rbind(TE1,TE2) 

plotAFE_LNY<-ggplot(data=DataAFEKHW3, aes(x=as.factor(Year), y=N, fill="AFE", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(2,30))+ 
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = NULL, y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .934),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=17),
        axis.text.y=element_text(size=15),
        axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=19,vjust= 1.8),
        axis.title.x = element_text(size=19,vjust= -0.5),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )

plotAHE_LNY<-ggplot(data=AHE_LNY3, aes(x=as.factor(Year), y=N, fill="AHE", linetype=stage)) + #library(ggplot2), linetype=stage
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0.19,3))+ 
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = NULL, y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .89),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=17),
        axis.text.y=element_text(size=15),
        axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=19,vjust= 1.8),
        axis.title.x = element_text(size=19,vjust= -0.5),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )

plotME_LNY<-ggplot(data=ME_LNY, aes(x=as.factor(Year), y=N, fill="ME", linetype=location)) + #library(ggplot2), linetype=stage
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0,2))+ 
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = NULL, y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.82, .89),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=17),
        axis.text.y=element_text(size=15),
        axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=19,vjust= 1.8),
        axis.title.x = element_text(size=19,vjust= -0.5),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )

plotSBS_LNY<-ggplot(data=SBS_LNY3, aes(x=as.factor(Year), y=N, fill="SBS", linetype=stage)) + #library(ggplot2), linetype=stage
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0.51,8))+ 
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .89),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=17),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15), #element_blank(),
     #   panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=19,vjust= 1.8),
        axis.title.x = element_text(size=19,vjust= -0.5),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )

plotTE_LNY<-ggplot(data=TE_LNY3, aes(x=as.factor(Year), y=N, fill="TE", linetype=stage)) + #library(ggplot2), linetype=stage
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0.51,10))+ 
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.82, .89),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=17),
        axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=19,vjust= 2.8),
        axis.title.x = element_text(size=19,vjust= -0.5),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )

plot_grid(plotAFE_LNY,plotAHE_LNY, plotSBS_LNY, plotTE_LNY, nrow=4, labels = "auto", label_size = 25, rel_widths = c(2,2))

########################################################################################################################################
####AFE plot for Khwai, based on midseason####
plotAFE_KHW<-ggplot(data=AFE_KHW2d, aes(x=as.factor(Year), y=N, fill="AFE", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("gray")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0.56,12))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number, AFE")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.82, .934),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=17),
        axis.text=element_text(size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=19,vjust= 1.8),
        axis.title.x = element_text(size=19,vjust= -0.5),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )

plotAHE_KHW<-ggplot(data=AHE_KHW3, aes(x=as.factor(Year), y=N, fill="AHE", linetype=stage)) + #library(ggplot2), linetype=stage
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0.19,3))+ 
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number, AHE")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .89),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=17),
        axis.text=element_text(size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=19,vjust= 1.8),
        axis.title.x = element_text(size=19,vjust= -0.5),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )

plotME_KHW<-ggplot(data=ME_KHW, aes(x=as.factor(Year), y=N, fill="ME", linetype=location)) + #library(ggplot2), linetype=stage
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0,2))+ 
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number, ME")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.82, .89),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=17),
        axis.text=element_text(size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=19,vjust= 1.8),
        axis.title.x = element_text(size=19,vjust= -0.5),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )

plotTE_KHW<-ggplot(data=TE_KHW3, aes(x=as.factor(Year), y=N, fill="TE", linetype=stage)) + #library(ggplot2), linetype=stage
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("gray")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0.51,4))+ 
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number, TE")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .89),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=17),
        axis.text=element_text(size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=19,vjust= 1.8),
        axis.title.x = element_text(size=19,vjust= -0.5),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )

plot_grid(plotAFE_KHW,plotTE_KHW, nrow=1, labels = "auto", label_size = 25, rel_widths = c(2,2))

########################################################################################################################################

AFE["species"]<-"AFE"
AHE["species"]<-"AHE"
ME["species"]<-"ME"
TE["species"]<-"TE"

allData <- rbind(AFE,AHE,ME,TE) 

plotAll<-ggplot(data=allData, aes(x=as.factor(Year), y=N,  linetype=species)) + #library(ggplot2)fill="TE",
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  # coord_cartesian(ylim=c(8,100))+ 
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .89),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=17),
        axis.text=element_text(size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=19,vjust= 1.8),
        axis.title.x = element_text(size=19,vjust= -0.5),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )
####then combine####
#DataLNY <- dplyr::rename(DataLNY, active = active_early)
DataKHW3 <- dplyr::rename(DataKHW3, active = active_mid)

DataBW <- rbind(DataLNY, DataKHW3) 
DataBW <- subset(DataBW, year!=2008 & stage!="ad")
windows()
p3<-  ggplot(data=DataBW, aes(x=as.factor(year), y=N, fill=colony)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white","gray","white","gray"),guide=FALSE) +
  scale_linetype_manual(values = c(1,2,1,2),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(8,100))+ 
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting numbers")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.75, .89),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=37),
        axis.text=element_text(size=30),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=30,vjust= 1.8),
        axis.title.x = element_text(size=30,vjust= 0.9),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )
p3
plot_grid(p1,p2,p3, nrow=1, labels = "auto", label_size = 25, rel_widths = c(2,2,2))

##########################################################################################
##########################################################################################

#### breeding success KHW #####
AFEbsLNYx<- subset(performance, location=="Linyanti"  & mid==1 & Year!=2008 & Year!=2013)
AFEbsLNYy<- subset(performance, location=="Linyanti"  & early==1 & Year!=2008 & Year!=2013)
AFEbsLNY <- subset(activeLNYmid, location=="Linyanti" & Year!=2008 & Year!=2013) #& early==1

AFEbs_LNY = summarySE(data=AFEbsLNYy,"verdict",na.rm = TRUE, groupvars="Year", conf.interval = 0.95)

#p5<-
  ggplot(AFEbs_LNY, aes(x = as.factor(Year), y = verdict ,ymax=1.0, ymin=0.0, fill="AFE LNY"))+ 
  geom_bar(stat="identity", colour = "black", width = 0.7)  + 
  scale_fill_manual(values=c("gray")) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0.05,1.0))+ 
  geom_errorbar(aes(ymax=verdict+se, ymin=verdict-se),width=0.1, size=0.5, color="black")  +
  #geom_text(aes(label=format(round(success, 2), nsmall = 2),hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=6) +             
  geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=6) +             
  labs(x = "year", y = "breeding success")  +
  ## ggtitle("Main title") +
  theme_classic()  +
  theme(legend.position=c(.85, .85),
        legend.text=element_text(size=17),
        legend.background = element_rect(fill = "transparent"),
        axis.text=element_text(size=17),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=19,vjust= 1.8),
        axis.title.x = element_text(size=19,vjust= -0.5)
  )

bsKHW <- subset(active, colony=="KHW" & year!=2007 & year!=2016 & active_mid==1 & !is.na(success))
bs_KHW = summarySE(data=bsKHW,"success", groupvars="year", conf.interval = 0.95)

p6<-ggplot(bs_KHW, aes(x = as.factor(year), y = success ,ymax=0.8, ymin=0.0, fill="KHW"))+ 
  geom_bar(stat="identity", colour = "black", width = 0.7)  + 
  scale_fill_manual(values=c("white")) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0.05,0.8))+ 
  geom_errorbar(aes(ymax=success+se, ymin=success-se),width=0.1, size=0.5, color="black")  +
  geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=6) +             
  #geom_text(aes(label=format(round(success, 2), nsmall = 2),hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=6) +             
  labs(x = "year", y = "breeding success")  +
  ## ggtitle("Main title") +
  theme_classic()  +
  theme(legend.position=c(.85, .85),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=17),
        axis.text=element_text(size=17),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=19,vjust= 1.8),
        axis.title.x = element_text(size=19,vjust= -0.5)
  )


plot_grid(p5,p6, nrow=1, labels = "auto",label_size = 24, rel_widths = c(2,2))

bsKHW["act"]<-1
bsLNY["act"]<-1
bsBW<-rbind(bsKHW, bsLNY)
bsBW<-subset(bsBW, !is.na(fledged)& act==1)
bs_BW = summarySE(data=bsBW,"success",na.rm = TRUE, groupvars="province", conf.interval = 0.95)

p7<-ggplot(bs_BW, aes(x = as.factor(province), y = success ,ymax=0.8, ymin=0.0, fill="overall"))+ 
  geom_bar(stat="identity", colour = "black", width = 0.7)  + 
  scale_fill_manual(values=c("gray50")) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0.05,0.8))+ 
  geom_errorbar(aes(ymax=success+se, ymin=success-se),width=0.1, size=0.5, color="black")  +
  #geom_text(aes(label=format(round(success, 2), nsmall = 2),hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=6) +             
  geom_text(aes(label=N,hjust=offset.h, vjust=-1),position=position_stack(vjust = 0),size=10) +             
  labs(x = "year", y = "breeding success")  +
  ## ggtitle("Main title") +
  theme_classic()  +
  theme(legend.position=c(.85, .85),
        legend.text=element_text(size=37),
        legend.background = element_rect(fill = "transparent"),
        axis.text=element_text(size=30),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=30,vjust= 1.8),
        axis.title.x = element_text(size=30,vjust= 0.9)
  )
p7
p3
plot_grid(p5,p6,p7, nrow=1, labels = "auto", rel_widths = c(2,2,2))
#write.csv(performance, "E:/bsBW300418.csv")






####glm BW####
bsKHW$yearfact<-as.factor(bsKHW$year)
fitKHW <- glm(success ~ year,weight=weeks, data=bsKHW, family="binomial"(link="logit"))
summary(fitKHW)
anova(fitKHW, test="Chisq")
lsmeans(fitKHW, pairwise ~ year, type = "response", na.rm=TRUE)

bsLNY$yearfact<-as.factor(bsLNY$year)
fitLNY <- glm(success ~ year,weight=weeks, data=bsLNY, family="binomial"(link="logit"))
summary(fitLNY)
anova(fitLNY, test="Chisq")
lsmeans(fitLNY, pairwise ~ year, type = "response", na.rm=TRUE)

fit <- glm(success ~ province+colony+province*colony,weight=weeks, data=bsBW, family="binomial"(link="logit"))

fit <- glm(success ~ province+colony+province*colony,weight=weeks, data=bsBW, family="binomial"(link="logit"))

summary(fit)
#confint(fit) # 95% CI for the coefficients

anova(fit, test="Chisq")
anova(fit) #this does not show the chi-square value

#anova(fit, type = 2)
#?anova()
lsmeans(fit, pairwise ~ province+colony, type = "response", na.rm=TRUE)
#lsmeans(fit, pairwise ~ colony, type = "response", na.rm=TRUE)


d <- summary(fit)$lsmeans[c("prob", "asymp.LCL", "asymp.UCL")]
ggplot(d, aes(time)) +
  geom_line(aes(y = lsmean)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2) +
  geom_point(aes(y = lsmean), size = 3, 
             shape = 21, fill = "white") +
  labs(x = "Time", y = "ls mean",
       title = "ls mean result over time") +
  theme_bw()

########0102017#######
write.csv(bsBW, "E:/190218.csv")
fit <- glm(success ~ province+colony,weight=weeks, data=bsBW, family="binomial"(link="logit"))#+observers
summary(fit)
#lsmeans(model, pairwise~time, adjust="tukey")
fitted<-summary(fit <-lsmeans(fit, pairwise ~ province+colony, type = "response", na.rm=TRUE))

fitted$lsmeans
tooth<-fitted$lsmeans
tooth$level<-paste(tooth$province,tooth$colony)

fit5 <- glm(success ~ colony+province,weight=weeks, data=bsBW, family="binomial"(link="logit"))#+observers
summary(fit5)
anova(fit5, test="Chisq")

#?cbind()
#library(gplots)

tooth <- dplyr::rename(tooth, period = province)

toothLNY<-subset(tooth,colony=="LNY")
toothKHW<-subset(tooth,colony=="KHW")

plot1<-ggplot(toothLNY, aes(x = period, y = prob ,ymax=0.8, ymin=0.19))+ 
  geom_bar(stat="identity", colour = "black", fill="gray", width = 0.7)+ 
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),  width = 0.19,  size  = 0.7)+
  theme(axis.title   = element_text(face  = "bold")) +
  coord_cartesian(ylim=c(0.04,0.8))+ 
  labs(x = "period", y = "mean breeding success")+
  theme(
    legend.position=c(.85, .85),
    legend.text=element_text(size=18),
    legend.background = element_rect(fill = "transparent"),
    axis.text=element_text(size=17)
  )

plot2<-ggplot(toothKHW, aes(x = period, y = prob ,ymax=0.8, ymin=0.19))+ 
    geom_bar(stat="identity", colour = "black", fill="white", width = 0.7)+ 
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),  width = 0.19,  size  = 0.7)+
    theme(axis.title   = element_text(face  = "bold")) +
    coord_cartesian(ylim=c(0.04,0.8))+ 
    labs(x = "period", y = "mean breeding success") +
  theme(
    legend.position=c(.85, .85),
    legend.text=element_text(size=18),
    legend.background = element_rect(fill = "transparent"),
    axis.text=element_text(size=17)
  )
    

plot_grid(plot1,plot2, nrow=1, labels = "auto",label_size = 24, rel_widths = c(2,2))
replace(tooth,"BW","")
######################


#### end glm####


####mzantsi we Afrika####

performanceRSA <- subset(performance, country=="RSA" & year>2012 & active_mid==1 & colonyCountPerYear.freq > 9 & !is.na(success)) 
RSA <-  which(performanceRSA$colony %in% c("HIP","MKH","PGR","PNR","THL"))
bsRSA  <- performanceRSA [-RSA,] 

bs_rsa <- read.csv("E:/2018/Feb/07/matched.csv")
bs_rsa <-ddply(bs_rsa,.(colony,year),transform,colonyCountPerYear = count(colony))
#bs_rsa$yearfact<- as.factor(bs_rsa$year)
#bs_rsa["act"]<-1

bsRSA <- rbind(bs_rsa, bsRSA) 

bsRSA$yearfact<- as.factor(bsRSA$year)
bsRSA["act"]<-1

bsBW$yearfact<- as.factor(bsBW$year)
str(bsBW)
str(bsRSA)

bsAll <- rbind(bsBW, bsRSA) 

##############
##############
      bsAll <-ddply(bsAll,.(colony,year),transform,colonyCountPerYear2 = count(colony))
      bsAll <- subset(bsAll, colonyCountPerYear2.freq > 9)

fitA <- glmer(success ~ province+(1|colony), data=bsAll, family="binomial"(link="logit"))
summary(fitA)
anova(fitA)
lsmeans(fitA, pairwise ~ province, type = "response", na.rm=TRUE)
mmg<-summary(lsmeans(fitA, pairwise ~ province, type = "response", na.rm=TRUE))
mmg
moodMusik<-mmg$lsmeans
moodMusik

summarisedRSA = summarySE(data=bsAll,"success",na.rm = TRUE, groupvars="province", conf.interval = 0.95)


#write.csv(bsAll, "E:/110218.csv")

fitA <- glmer(success ~ province+(1|colony), data=bsAll, family="binomial"(link="logit"))

offset.v = -1.2     # offsets for data labels
offset.h = 0.5
offset.v2 = -0.5 
offset.h2 = -1.99
ppp1<-ggplot(moodMusik, aes(x = province, y = prob ,ymax=1.0, ymin=0.19))+ 
  geom_bar(stat="identity", fill=c('white','white','gray','gray','gray'),color='#000000',width = 0.7)+ 
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),  width = 0.19,  size  = 0.7)+
  geom_text(aes(label=c('68','71','308','179','147'),hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=8) +             
  geom_text(aes(label=c('a','b','c','c','b'),hjust=offset.h2, vjust=offset.v2),size=8) +             
  theme(axis.title   = element_text(face  = "bold")) +
  coord_cartesian(ylim=c(0.05,1.0))+ 
  labs(x = "region", y = "mean breeding success",size=18)+
  theme(
    legend.position=c(.85, .85),
    legend.text=element_text(size=18),
    legend.background = element_rect(fill = "transparent"),
    #axis.title.y = element_text(size = rel(1.5)),
    axis.title.y = element_text(size=25,vjust= 4.99),
    axis.title.x = element_text(size=25,vjust= 0,hjust=0.45),
    #axis.title.x = element_text(size = rel(1.5)),
    axis.text=element_text(size=20)
  )

offset.v = -0.5     # offsets for mean letters
offset.h = 0.5
#par()
ppp2<-plot(lsmeans(fitA, pairwise ~ province, type = "response", na.rm=TRUE)) +
  labs(x = "province", y = "breeding success estimate")+ 
  #ggtitle("1")+
  #scale_fill_grey()+
  #scale_fill_manual(name="significance",values=c("gray","gray","gray","gray","gray"))+
  coord_cartesian(ylim=c(0, 1))+ 
  scale_y_continuous(expand = c(0,0))+ 
  #geom_text(aes(label=Estimate,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=4) + 
  #theme(plot.title = element_text(1)) +
  #scale_x_discrete(limit = c("BW 2006/07", "BW 2017", "KZN", "NC", "NW"))+ 
  theme_minimal()+
  # facet_wrap(~ Estimate) + 
  #theme(
  # strip.background = element_blank(),
  # strip.text.x = element_blank()
  #)
  # theme(plot.title = element_blank())+
  theme(plot.title = element_text(size = 16, color="#000000"),
        legend.position=c(.895, .89),
        axis.text.x = element_text(size = 15, color="#000000"),
        legend.text=element_text(size=15),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(color="#000000", face="bold", size=22),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid"))

fitA <- glmer(success ~ province+(1|colony), data=bsAll, family="binomial"(link="logit"))

mAll21 <- lmer(success ~ province+(1|colony),weight=weeks,data=bsAll)
lsmeansLT(mAll21, test.effs="active:province", na.rm=TRUE)  

lsmeansLT(mAll21)
header(lsmeansLT(mAll21))
head(lsmeansLT(mAll21))
str(lsmeansLT(mAll21))

#plot(lsmeans(fitA)) +
  
ppp3<- plot(lsmeansLT(mAll21)) +
  labs(x = "province", y = "breeding success estimate")+ 
  #ggtitle("1")+
  #scale_fill_grey()+
  #scale_fill_manual(name="significance",values=c("gray","gray","gray","gray","gray"))+
  coord_cartesian(ylim=c(0, 1))+ 
  scale_y_continuous(expand = c(0,0))+ 
  #geom_text(aes(label=Estimate,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=4) + 
  #theme(plot.title = element_text(1)) +
  scale_x_discrete(limit = c("BW 2006/07", "BW 2017", "KZN", "NC", "NW"))+ 
  theme_minimal()+
  # facet_wrap(~ Estimate) + 
  #theme(
  # strip.background = element_blank(),
  # strip.text.x = element_blank()
  #)
  # theme(plot.title = element_blank())+
  theme(plot.title = element_text(size = 16, color="#000000"),
        legend.position=c(.895, .89),
        axis.text.x = element_text(size = 15, color="#000000"),
        legend.text=element_text(size=15),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(color="#000000", face="bold", size=22),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
       )


#>>>>check#
fitA <- glmer(success ~ province+(1|colony), data=bsAll, family="binomial"(link="logit"))#,weight=weeks
mmg<-summary(lsmeans(fitA, pairwise ~ province, type = "response", na.rm=TRUE))
mmg$lsmeans #>>>this gives good color but no the resolution is poor

#?lme4::lmer()

mAll21 <-lmer(success ~ province+(1|colony), data=bsAll, weight=weeks)#, family="binomial"(link="logit")
lsmeansLT(mAll21) #>>>this gives good bars but no color
##############
##############

#isolate RSA to calculable records; then append BW data
fitAll <- glmer(success ~ province+(1|colony),weight=weeks, data=fitA, family="binomial"(link="logit"))
summary(fitAll)
anova(fitAll)
lsmeans(fitAll, pairwise ~ province, type = "response", na.rm=TRUE)
result<-summary(fitAll)

offset.v = -0.5     # offsets for mean letters
offset.h = 0.5
par()
plot(lsmeans(fitAll, pairwise ~ province, type = "response", na.rm=TRUE)) +
  labs(x = "province", y = "breeding success estimate")+ 
  #ggtitle("1")+
  #scale_fill_grey()+
  #scale_fill_manual(name="significance",values=c("gray","gray","gray","gray","gray"))+
  coord_cartesian(ylim=c(0, 1))+ 
  scale_y_continuous(expand = c(0,0))+ 
  #geom_text(aes(label=Estimate,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=4) + 
  #theme(plot.title = element_text(1)) +
 # scale_x_discrete(limit = c("BW 2006/07", "BW 2017", "KZN", "NC", "NW"))+ 
  theme_minimal()+
  # facet_wrap(~ Estimate) + 
  #theme(
  # strip.background = element_blank(),
  # strip.text.x = element_blank()
  #)
  # theme(plot.title = element_blank())+
  theme(plot.title = element_text(size = 16, color="#000000"),
        legend.position=c(.895, .89),
        axis.text.x = element_text(size = 15, color="#000000"),
        legend.text=element_text(size=15),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(color="#000000", face="bold", size=22),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid"))



####end of Mzantsi #####



fit <- glm(success ~ province+observers+observers*province, data=bsBW, family="binomial"(link="logit")) #, wt=weekdiff
summary(fit) # display results
#confint(fit) # 95% CI for the coefficients
anova(fit, test="Chisq")
#lsmeans(fit, pairwise ~ colony, type = "response", na.rm=TRUE)







fitBW <- glm(success ~ province, data=bsBW,weight=weeks, family="binomial"(link="logit"))
summary(fitBW) # display results
#confint(fit) # 95% CI for the coefficients
anova(fitBW, test="Chisq")
anova(fitBW, type='III')
#lsmeans(fit, pairwise ~ colony, type = "response", na.rm=TRUE)
?anova()

anova.glm(fitBW)
anova(fitBW)
sink()







