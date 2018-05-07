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

#I use early for Linyanti
activeLNY_AFE <- subset(performance, location=="Linyanti" & early==1 & Year!=2006 & species=="AFE")

#what to add to the `Early` survey visit
addLNY_AFE   <- subset(performance, location=="Linyanti"  & add2e==1 & Year!=2006 & species=="AFE")

DataLNY_AFE  = summarySE(data=activeLNY_AFE,"early", groupvars="Year", conf.interval = 0.95) 

DataLNY2_AFE = summarySE(data=addLNY_AFE,"add2e", groupvars="Year", conf.interval = 0.95) 

#rename "add2e" to "early" so I can collate the subsets
DataLNY2_AFE <- dplyr::rename(DataLNY2_AFE, early = add2e)  #  df <- rename(df, new_name = old_name)

DataLNY_AFE["stage"]<-"og"
DataLNY2_AFE["stage"]<-"ad"

#merging the 2 subsets
DataLNY3_AFE<-rbind(DataLNY_AFE,DataLNY2_AFE)

#for later, add a column called Linyanti
DataLNY3_AFE["location"]<-"Linyanti"

###///////from preliminary assessment, add seems more, so I might have to use "early"
####end Linyanti structuring####

offset.v = -1     # offsets for data labels
offset.h = 0.5

AFE_LNY_nn<-ggplot(data=DataLNY3_AFE, aes(x=as.factor(Year), y=N, fill="AFE", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("gray")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(2.1,30))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .91),
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

AFE_LNY_nn2<-ggplot(data=DataLNY3_AFE, aes(x=as.factor(Year), y=N, fill="LNY", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("gray")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(2.1,30))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .91),
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

#select for those with midseason nests
activeKHW_AFE <- subset(performance, location=="Khwai" & mid==1 & Year!=2006 & species=="AFE")
#select for nests that are extra to midseason
addKHW_AFE    <- subset(performance, location=="Khwai"  & add2m==1 & Year!=2006 & species=="AFE")

#summarise mid nests
DataKHW_AFE = summarySE(data=activeKHW_AFE,"mid", groupvars="Year", conf.interval = 0.95) 
#summarise additional nests
DataKHW2_AFE = summarySE(data=addKHW_AFE,"add2m", groupvars="Year", conf.interval = 0.95) 

DataKHW2_AFE <- dplyr::rename(DataKHW2_AFE, mid = add2m)  #  df <- rename(df, new_name = old_name)

DataKHW_AFE["stage"]<-"og"
DataKHW2_AFE["stage"]<-"ad"

#DataKHW <- dplyr::rename(DataKHW, early = add2e)

DataKHW3_AFE<-rbind(DataKHW_AFE,DataKHW2_AFE)
DataKHW3_AFE["location"]<-"Khwai"

####end of khwai restructuring####

AFE_KHW_nn<-ggplot(data=DataKHW3_AFE, aes(x=as.factor(Year), y=N, fill="AFE", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0.61,12))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.82, .89),
        plot.margin=unit(c(0,0,0.3,0.8),'cm'),
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

AFE_KHW_nn2<-ggplot(data=DataKHW3_AFE, aes(x=as.factor(Year), y=N, fill="KHW", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(1.4,30))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "AFE nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.82, .89),
        plot.margin=unit(c(0,0,0.3,0.8),'cm'),
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

dataOG_KHW_AFE <- DataKHW_AFE
dataOG_KHW_AFE["location"]<-"Khwai"
dataOG_LNY_AFE <- DataLNY_AFE
dataOG_LNY_AFE["location"]<-"Linyanti"
dataOG_KHW_AFE <- dplyr::rename(dataOG_KHW_AFE, count = mid)
dataOG_LNY_AFE <- dplyr::rename(dataOG_LNY_AFE, count = early)

Data_AFE <- rbind(dataOG_KHW_AFE,dataOG_LNY_AFE)
Data_AFE <- subset(Data_AFE, Year!=2008)

AFE_nn<-ggplot(data=Data_AFE, aes(x=as.factor(Year), y=N, fill=location)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white","gray")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(2.1,30))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.72, .91),legend.direction="horizontal",
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=13),
        axis.text=element_text(size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=13,vjust= 1.8,hjust= 0),
        axis.title.x = element_text(size=13,vjust= -0.5),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )

####end of AFE analysis#####

####start analysing AHE #####

#I use early for Linyanti
activeLNY_AHE <- subset(performance, location=="Linyanti" & early==1 & Year!=2006 & species=="AHE")

#what to add to the `Early` survey visit
addLNY_AHE   <- subset(performance, location=="Linyanti"  & add2e==1 & Year!=2006 & species=="AHE")

DataLNY_AHE  = summarySE(data=activeLNY_AHE,"early", groupvars="Year", conf.interval = 0.95) 

DataLNY2_AHE = summarySE(data=addLNY_AHE,"add2e", groupvars="Year", conf.interval = 0.95) 

#rename "add2e" to "early" so I can collate the subsets
DataLNY2_AHE <- dplyr::rename(DataLNY2_AHE, early = add2e)  #  df <- rename(df, new_name = old_name)

DataLNY_AHE["stage"]<-"og"
DataLNY2_AHE["stage"]<-"ad"

#merging the 2 subsets
DataLNY3_AHE<-rbind(DataLNY_AHE,DataLNY2_AHE)

#for later, add a column called Linyanti
DataLNY3_AHE["location"]<-"Linyanti"

###///////from preliminary assessment, add seems more, so I might have to use "early"
####end Linyanti AHE structuring####

offset.v = -1     # offsets for data labels
offset.h = 0.5

AHE_LNY_nn<-ggplot(data=DataLNY3_AHE, aes(x=as.factor(Year), y=N, fill="AHE", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("gray")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(.21,4))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .91),
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

####end of AHE analysis#####

####start analysing SBS #####

#I use early for Linyanti
activeLNY_SBS <- subset(performance, location=="Linyanti" & early==1 & Year!=2006 & species=="SBS")

#what to add to the `Early` survey visit
addLNY_SBS   <- subset(performance, location=="Linyanti"  & add2e==1 & Year!=2006 & species=="SBS")

DataLNY_SBS  = summarySE(data=activeLNY_SBS,"early", groupvars="Year", conf.interval = 0.95) 

DataLNY2_SBS = summarySE(data=addLNY_SBS,"add2e", groupvars="Year", conf.interval = 0.95) 

#rename "add2e" to "early" so I can collate the subsets
DataLNY2_SBS <- dplyr::rename(DataLNY2_SBS, early = add2e)  #  df <- rename(df, new_name = old_name)

DataLNY_SBS["stage"]<-"og"
DataLNY2_SBS["stage"]<-"ad"

#merging the 2 subsets
DataLNY3_SBS<-rbind(DataLNY_SBS,DataLNY2_SBS)

#for later, add a column called Linyanti
DataLNY3_SBS["location"]<-"Linyanti"

###///////from preliminary assessment, add seems more, so I might have to use "early"
####end Linyanti SBS structuring####

offset.v = -1     # offsets for data labels
offset.h = 0.5

SBS_LNY_nn<-ggplot(data=DataLNY3_SBS, aes(x=as.factor(Year), y=N, fill="SBS", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("gray")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(.521,8))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .91),
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

SBS_LNY_nn2<-ggplot(data=DataLNY3_SBS, aes(x=as.factor(Year), y=N, fill="LNY", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("gray")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(.57,12))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "SBS nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .91),
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

#select for those with midseason nests
activeKHW_SBS <- subset(performance, location=="Khwai" & mid==1 & Year!=2006 & species=="SBS")
#select for nests that are extra to midseason
addKHW_SBS    <- subset(performance, location=="Khwai"  & add2m==1 & Year!=2006 & species=="SBS")

#summarise mid nests
DataKHW_SBS = summarySE(data=activeKHW_SBS,"mid", groupvars="Year", conf.interval = 0.95) 
#summarise additional nests
DataKHW2_SBS = summarySE(data=addKHW_SBS,"add2m", groupvars="Year", conf.interval = 0.95) 

DataKHW2_SBS <- dplyr::rename(DataKHW2_SBS, mid = add2m)  #  df <- rename(df, new_name = old_name)

DataKHW_SBS["stage"]<-"og"
DataKHW2_SBS["stage"]<-"ad"

#DataKHW <- dplyr::rename(DataKHW, early = add2e)

DataKHW3_SBS<-rbind(DataKHW_SBS,DataKHW2_SBS)
DataKHW3_SBS["location"]<-"Khwai"

####end of khwai restructuring####

SBS_KHW_nn<-ggplot(data=DataKHW3_SBS, aes(x=as.factor(Year), y=N, fill="SBS", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0.61,12))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .89),
        plot.margin=unit(c(0,0,0.3,0.8),'cm'),
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

SBS_KHW_nn2<-ggplot(data=DataKHW3_SBS, aes(x=as.factor(Year), y=N, fill="KHW", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0.61,12))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "SBS nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .89),
        plot.margin=unit(c(0,0,0.3,0.8),'cm'),
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

dataOG_KHW_SBS <- DataKHW_SBS
dataOG_KHW_SBS["location"]<-"Khwai"
dataOG_LNY_SBS <- DataLNY_SBS
dataOG_LNY_SBS["location"]<-"Linyanti"
dataOG_KHW_SBS <- dplyr::rename(dataOG_KHW_SBS, count = mid)
dataOG_LNY_SBS <- dplyr::rename(dataOG_LNY_SBS, count = early)

Data_SBS <- rbind(dataOG_KHW_SBS,dataOG_LNY_SBS)
Data_SBS <- subset(Data_SBS, Year!=2008)

SBS_nn<-ggplot(data=Data_SBS, aes(x=as.factor(Year), y=N, fill=location)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white","gray")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(.56,12))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .91),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=13),
        axis.text=element_text(size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=13,vjust= 1.8,hjust= 0),
        axis.title.x = element_text(size=13,vjust= -0.5),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )
####end of SBS analysis#####

####start analysing TE #####

#I use early for Linyanti
activeLNY_TE <- subset(performance, location=="Linyanti" & early==1 & Year!=2006 & species=="TE")

#what to add to the `Early` survey visit
addLNY_TE   <- subset(performance, location=="Linyanti"  & add2e==1 & Year!=2006 & species=="TE")

DataLNY_TE  = summarySE(data=activeLNY_TE,"early", groupvars="Year", conf.interval = 0.95) 

DataLNY2_TE = summarySE(data=addLNY_TE,"add2e", groupvars="Year", conf.interval = 0.95) 

#rename "add2e" to "early" so I can collate the subsets
DataLNY2_TE <- dplyr::rename(DataLNY2_TE, early = add2e)  #  df <- rename(df, new_name = old_name)

DataLNY_TE["stage"]<-"og"
DataLNY2_TE["stage"]<-"ad"

#merging the 2 subsets
DataLNY3_TE<-rbind(DataLNY_TE,DataLNY2_TE)

#for later, add a column called Linyanti
DataLNY3_TE["location"]<-"Linyanti"

###///////from preliminary assessment, add seems more, so I might have to use "early"
####end Linyanti TE structuring####

offset.v = -1     # offsets for data labels
offset.h = 0.5

TE_LNY_nn<-ggplot(data=DataLNY3_TE, aes(x=as.factor(Year), y=N, fill="TE", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("gray")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(.521,10))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.82, .91),
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

TE_LNY_nn2<-ggplot(data=DataLNY3_TE, aes(x=as.factor(Year), y=N, fill="LNY", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("gray")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(.521,10))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "TE nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.82, .91),
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

#select for those with midseason nests
activeKHW_TE <- subset(performance, location=="Khwai" & mid==1 & Year!=2006 & species=="TE")
#select for nests that are extra to midseason
addKHW_TE    <- subset(performance, location=="Khwai"  & add2m==1 & Year!=2006 & species=="TE")

#summarise mid nests
DataKHW_TE = summarySE(data=activeKHW_TE,"mid", groupvars="Year", conf.interval = 0.95) 
#summarise additional nests
DataKHW2_TE = summarySE(data=addKHW_TE,"add2m", groupvars="Year", conf.interval = 0.95) 

DataKHW2_TE <- dplyr::rename(DataKHW2_TE, mid = add2m)  #  df <- rename(df, new_name = old_name)

DataKHW_TE["stage"]<-"og"
DataKHW2_TE["stage"]<-"ad"

#DataKHW <- dplyr::rename(DataKHW, early = add2e)

DataKHW3_TE<-rbind(DataKHW_TE,DataKHW2_TE)
DataKHW3_TE["location"]<-"Khwai"

####end of khwai restructuring####

TE_KHW_nn<-ggplot(data=DataKHW3_TE, aes(x=as.factor(Year), y=N, fill="TE", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0.23,4))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .89),
        plot.margin=unit(c(0,0,0.3,0.8),'cm'),
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

TE_KHW_nn2<-ggplot(data=DataKHW3_TE, aes(x=as.factor(Year), y=N, fill="KHW", linetype=stage)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(0.48,10))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "TE nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.2, .89),
        plot.margin=unit(c(0,0,0.3,0.8),'cm'),
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

dataOG_KHW_TE <- DataKHW_TE
dataOG_KHW_TE["location"]<-"Khwai"
dataOG_LNY_TE <- DataLNY_TE
dataOG_LNY_TE["location"]<-"Linyanti"
dataOG_KHW_TE <- dplyr::rename(dataOG_KHW_TE, count = mid)
dataOG_LNY_TE <- dplyr::rename(dataOG_LNY_TE, count = early)

Data_TE <- rbind(dataOG_KHW_TE,dataOG_LNY_TE)
Data_TE <- subset(Data_TE, Year!=2008)

TE_nn<-ggplot(data=Data_TE, aes(x=as.factor(Year), y=N, fill=location)) + #library(ggplot2)
  geom_bar(stat="identity", colour = "black",  width = 0.7) +  
  scale_fill_manual(values=c("white","gray")) +
  scale_linetype_manual(values = c(2,1,2,1),guide=FALSE) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(.39,8))+ 
  scale_y_continuous(breaks= pretty_breaks()) +
  #geom_errorbar(aes(ymax=N+se, ymin=N-se),width=0.0, size=0.5, color="black")  +
  #geom_text(aes(label=N,hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=5) +             
  labs(x = "year", y = "nesting number")  +
  ## ggtitle("Main title") +
  theme_bw()  +
  theme(legend.position=c(.82, .91),
        legend.background = element_rect(fill = "transparent"),
        legend.text=element_text(size=13),
        axis.text=element_text(size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey80"),
        plot.title = element_text(size = rel(1.5),face = "bold", vjust = 1.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(size=13,vjust= 1.8,hjust= 0),
        axis.title.x = element_text(size=13,vjust= -0.5),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid")
  )
TE_nn
####end of TE analysis#####

plot_grid(AFE_LNY_nn,AHE_LNY_nn, SBS_LNY_nn, TE_LNY_nn, nrow=2, labels = "auto", label_size = 25, rel_widths = c(2,2))
plot_grid(AFE_KHW_nn, SBS_KHW_nn, TE_KHW_nn, nrow=3, labels = "auto", label_size = 25)
plot_grid(AFE_nn, SBS_nn, TE_nn, nrow=3, labels = "auto", label_size = 25)

#AFE
plot_grid(AFE_LNY_nn2, AFE_KHW_nn2, nrow=1, labels = "auto", label_size = 25, rel_widths = c(2.6,2))
plot_grid(SBS_LNY_nn2,SBS_KHW_nn2, nrow=1, labels = "auto", label_size = 25, rel_widths = c(2.6,2))
plot_grid(TE_LNY_nn2,TE_KHW_nn2, nrow=1, labels = "auto", label_size = 25, rel_widths = c(2.6,2))

#SBS
#TE
