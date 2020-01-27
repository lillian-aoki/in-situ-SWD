# Disease and growth

library(tidyverse)
library(ggplot)
library(lubridate)

month_order <- c("June","July")

combo <- read.csv("CombinedGrowthData.csv")
combo$Type <- "Final"

disease <- read.csv("In_Situ_Log_3.csv")
disease$Shoot <- as.numeric(str_extract(disease$file_name, "[0-9]{1,2}"))

combo <- full_join(combo,disease,by=c("Month"="month","Transect"="site","Type"="type","Shoot"))
combo$leaf_area <- combo$healthy_area+combo$lesion_area
combo$Prev <- as.factor(combo$Prev)
combo$Month <- ordered(combo$Month,levels=month_order)
combo$PrevMatch <- ifelse(combo$Prev==combo$prevelance,"TRUE","FALSE")
combo$severity <- combo$severity/100

ggplot(combo,aes(x=g.sh.d,y=severity/100,color=Month))+geom_point()+
  #stat_smooth(method="lm",show.legend = FALSE)+
  xlab("Growth (g biomass per shoot per day)")+
  ylab("WD severity")+
  scale_y_continuous(limits=c(0,0.5))+
  theme_bw()

ggplot(combo,aes(x=mm2.sh.d,y=severity/100,color=Month))+geom_point()+
  #stat_smooth(method="lm",show.legend = FALSE)+
  xlab("Growth (mm2 leaf area per shoot per day)")+
  ylab("WD severity")+
  scale_y_continuous(limits=c(0,0.5))+
  theme_bw()

ggplot(combo,aes(x=sp.prod.g,y=severity,color=Month))+geom_point()+
  #stat_smooth(method="lm",show.legend = FALSE)+
  xlab("Specific productivity (% biomass per day)")+
  ylab("WD severity")+
  scale_y_continuous(limits=c(0,0.5))+
  theme_bw()
ggplot(combo,aes(x=sp.prod.mm2,y=severity,color=Month))+geom_point()+
  #stat_smooth(method="lm",show.legend = FALSE)+
  xlab("Specific productivity (% leaf area per day)")+
  ylab("WD severity")+
  scale_y_continuous(limits=c(0,0.5))+
  theme_bw()

first_write <- 1
for(i in levels(combo$Month)){
  model <- lm(severity~sp.prod.mm2,data=combo[combo$Month==i,])
  sig_table_temp <- data.frame("Month"=i,
                               "Pvalue"=as.character(signif(summary(model)$coef[2,4], 2)),
                               "AdjR2"=as.character(signif(summary(model)$adj.r.squared, 2)),
                               "Slope"=as.character(signif(summary(model)$coef[2,1],2)))
  #sig_table_temp <- separate(sig_table_temp,col=Region.SiteCode,into=c("Region","SiteCode"),sep=" ")
  if(first_write==1){
    sig_table <- sig_table_temp
    first_write <- 0
  } else{
    sig_table <- rbind(sig_table,sig_table_temp)
  }
}

ggplot(combo,aes(y=mm2.sh.d,x=severity,color=Month))+geom_point()+
  stat_smooth(method="lm",show.legend = FALSE)+
  scale_x_continuous(limits=c(0,0.5))+
  theme_bw()

## So, at the shoot level, no relationship between growth and severity! Not what we expected...

ggplot(combo, aes(x=mm2.sh.d)) +
  geom_density(position = "fill")

combo_summ <- combo[combo$Subshoot=="Main",]%>% group_by(Month,Transect)%>%
  summarise(mm2.sh.d=mean(mm2.sh.d,na.omit=TRUE),g.sh.d=mean(g.sh.d,na.omit=TRUE),
            sp.prod.mm2=mean(sp.prod.mm2,na.omit=TRUE),sp.prod.g=mean(sp.prod.g,na.omit=TRUE),
            severity=mean(severity,na.omit=TRUE),prevalence=mean(prevelance,na.omit=TRUE))
