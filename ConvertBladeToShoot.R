#Convert blade area measurements to shoot specific productivity

blades <- read.csv("LeafGrowth.csv")
blades$Transect <- gsub(" ","",blades$Transect)
blades$Month <- gsub(" ","",blades$Month)

# Read in the dataset with the leaf-level measurements of old and new area
shoots <- blades %>%
  group_by(Month,Transect,Shoot,Subshoot,Days) %>%
  summarise(Old.mm=sum(OldArea.mm2),New.mm=sum(NewArea.mm2),
            NumLeavs=max(Total.leaves),Prev=mean(Prev))
shoots$Prev <- ifelse(shoots$Prev>0,"1","0")
shoots$Prev <- as.factor(shoots$Prev)
shoots$mm2.sh.d <- shoots$New.mm/shoots$Days
shoots$sp.prod.mm2=shoots$mm2.sh.d/shoots$Old.mm
shoots$sp.prod.mm2 <- replace(shoots$sp.prod.mm2,is.infinite(shoots$sp.prod.mm2),"NA")
shoots$Month <- gsub("July ","July",shoots$Month)
write.csv(shoots,"ShootGrowth.csv",row.names=FALSE)

## Separate data files for growth with babys included ("ShootGrowthWithBaby.csv") and without ("ShootGrowthMainOnly.csv")
shoots_baby <- blades %>%
  group_by(Month,Transect,Shoot,Days) %>%
  summarise(Old.mm=sum(OldArea.mm2),New.mm=sum(NewArea.mm2),
            NumLeavs=max(Total.leaves),Prev=mean(Prev))
shoots_baby$Prev <- ifelse(shoots_baby$Prev>0,"1","0")
shoots_baby$Prev <- as.factor(shoots_baby$Prev)
shoots_baby$mm2.sh.d <- shoots_baby$New.mm/shoots_baby$Days
shoots_baby$sp.prod.mm2=shoots_baby$mm2.sh.d/shoots_baby$Old.mm
shoots_baby$sp.prod.mm2 <- replace(shoots_baby$sp.prod.mm2,is.infinite(shoots_baby$sp.prod.mm2),"NA")
shoots_baby$Month <- gsub("July ","July",shoots_baby$Month)
write.csv(shoots_baby,"ShootGrowthWithBaby.csv",row.names=FALSE)

shoots_main <- subset(shoots,Subshoot=="Main")
write.csv(shoots_main,"ShootGrowthMainOnly.csv",row.names = FALSE)

## combine the leaf area and biomass measurements

growth <- read.csv("InSituGrowthData.csv")
growth$Prev.Field <- as.factor(growth$Prev.Field)
combo <- full_join(x=shoots_main,y=growth,by=c("Month","Transect","Shoot"="ShootNum"))
write.csv(combo,"CombinedGrowthData.csv",row.names = FALSE)
