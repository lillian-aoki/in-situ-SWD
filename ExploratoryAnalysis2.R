library(tidyverse)
library(ggplot)
library(lubridate)

month_order <- c("June","July")

combo <- read.csv("CombinedGrowthData.csv")
combo$Prev <- as.factor(combo$Prev)
combo$Month <- ordered(combo$Month,levels=month_order)
cdplot(Prev~mm2.sh.d,data=combo,xlab = "Growth (mm per shoot per day)",ylab="Prevelance")
cdplot(Prev~g.sh.d,data=combo,xlab = "Growth (g per shoot per day)",ylab="Prevelance")

cdplot(Prev~sp.prod.mm2,data=combo, xlab="Specific productiviy (% leaf area per day)",ylab="Prevelance")
# combo2 <- combo[!is.na(combo$sp.prod.g), ]
# cdplot(Prev~sp.prod.g,data=combo2, xlab="Specific productiviy (% growth per day)",ylab="Prevelance")
# Not sure why, but the cdplot is a mess with the sp.prod.g, even leaving out the NAs

combo$Sheath <- as.numeric(combo$Sheath)
plot(Old.g~mm2.sh.d,data=combo,xlab="Growth (mm per shoot per day)",ylab="Starting biomass (g)")
plot(Old.g~g.sh.d,data=combo,xlab="Growth (g per shoot per day)",ylab="Starting biomass (g)")

ggplot(data=combo,aes(x=Month,y=sp.prod.mm2,fill=Prev))+geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=4, size=4, color="black",
               position=position_dodge(width = 0.75),show.legend = FALSE)+
  xlab("Month")+
  ylab("Specific productivity (% leaf area per shoot per day)")+
  scale_fill_brewer(type="qual",palette = "Set3")+
  theme_bw()

ggplot(data=combo,aes(x=Month,y=sp.prod.g,fill=Prev))+geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=4, size=4, color="black",
               position=position_dodge(width = 0.75),show.legend = FALSE)+
  xlab("Month")+
  ylab("Specific productivity (% biomass per shoot per day)")+
  scale_fill_brewer(type="qual",palette = "Set3")+
  theme_bw()

combo_summ <- combo %>%
  group_by(Month,Transect)%>%
  summarise(NumHealthy=length(which(Prev==0)),NumDisease=length(which(Prev==1)),
            PerDisease=NumDisease/(NumDisease+NumHealthy))
#Very few healthy plants! 
combo_summ %>% group_by(Month) %>% summarise(meanPerDisease=mean(PerDisease),sd=sd(PerDisease))
# Equivalent prevalence at the transect level, but more variability in June

ggplot(data=combo_summ,aes(x=Month,y=PerDisease))+
  stat_summary(fun.y=mean, geom="bar")+
  stat_summary(fun.ymax = sd,geom="point")
