
library(multcompView)
library(lsmeans)
library(car) #why this was previously not working (instead of using teh default "anova")
library(ggplot2)
library(dplyr) ; library(tidyr)

performance <- read.csv("E:/2018/Apr/26/raptorsBW.csv", sep=";")

performance <-ddply(performance,.(species,Year),transform,speciesCountPerYear = count(species))

myfunction <- function(nonyane){
bs_LNY <- subset(performance, species==nonyane & Year!=2008 & early==1 & !is.na(verdict))
bs_LNY$yearfact<-as.factor(bs_LNY$Year)

Data = summarySE(data=bs_LNY,"early", groupvars="yearfact", conf.interval = 0.95)

M1c <- glm(verdict~ yearfact+location, family=binomial(link = "logit"), data=bs_LNY)
summary(M1c) # this summary gives you the estimates for the model
Anova(M1c, type="III") # this test then examines which variables are signficant in the model

lsM1c1<-lsmeans(M1c, pairwise ~ yearfact, type = "response")
lsM1c2<-lsmeans(M1c, pairwise ~ location, type = "response")
lsM1c3<-lsmeans(M1c, pairwise ~ yearfact*location, type = "response")
  
lsM1c1
lsM1c2
lsM1c3
cld(lsM1c1, alpha = .05)
diver<-cld(lsM1c1, alpha = .05)
diver
### Plot

offset.v = -0.5     # offsets for N labels
offset.h = 0.5

ggplot(diver, aes(x=yearfact,y=prob, fill=nonyane,label = .group)) +
  geom_bar(stat="identity", colour = "black",  width = 0.7) + 
  #geom_point(shape=15, size=4)+ #position = pd) +
  scale_fill_manual(values=c("gray")) +
  guides(fill=guide_legend(title=NULL))+ 
  coord_cartesian(ylim=c(.05,1))+ 
  geom_errorbar(aes(ymin=asymp.UCL,ymax=asymp.LCL),width=0.2,size=0.7)+ #position = pd
  geom_text(aes(label=c(17,5),hjust=offset.h, vjust=offset.v),position=position_stack(vjust = 0),size=6) + 
  theme_minimal() +
  theme(legend.position=c(.82, .89),
        legend.background = element_rect(fill = "transparent",color = "transparent"),
        legend.text=element_text(size=17),
        axis.text.x = element_text(size = 15, color="#000000"),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(color="#000000", face="bold", size=22),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "black", size = 0.51, linetype = "solid"),

        plot.caption = element_text(hjust = 0)) +
  ylab("breeding success")+
  xlab("year")+
  geom_text(nudge_x = c(0.1, 0.1),nudge_y = c(0.1, 0.1),color   = "black")
  
#ggtitle ("Midichlorian counts", subtitle = "In four U.S. cities") +
  #labs(caption  = paste0("\nMidichlorian counts for four locations. ", "Boxes indicate the LS mean. \n","Error bars indicate the 95% ",
  #                       "confidence interval of the LS mean. \n",
  #                       "Means sharing a letter are not ",
  #                       "significantly different (Tukey-adjusted \n",
  #                       "comparisons)."),
  #     hjust=0.5) +
  #
}

bsAnalysis("AFE")
bsAnalysis("AHE")
bsAnalysis("SBS")
bsAnalysis("TE")
