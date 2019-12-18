library(ggplot2)
library(tidyverse)

growth <- read.csv("InSituGrowthData.csv")
growth <- na.omit(growth)
growth$Prev <- as.factor(growth$Prev)
cdplot(Prev~mm2.sh.d,data=growth,xlab = "Growth (mm per shoot per day)",ylab="Prevelance")
cdplot(Prev~Old.g,data=growth)
<<<<<<< HEAD
cdplot(Prev~g.sh.d,data=growth)
cdplot(Prev~sp.prod.mm2,data=growth, xlab="Specific productiviy (% growth per day)",ylab="Prevelance")
=======
>>>>>>> dec056c9c33e2755d67a89c851a2d8ed7e24726b
plot(Old.g~mm2.sh.d,data=growth)
plot(Old.g~sp.prod.mm2,data=growth)
growth$Sheath <- as.numeric(growth$Sheath)
plot(Sheath~mm2.sh.d,data=growth,xlab="Sheath length (mm)",ylab="Starting biomass (g)")

leaf <- read.csv("LeafGrowth.csv")
leaf$Prev <- as.factor(leaf$Prev)
cdplot(Prev~mm2.l.d,data=leaf)
<<<<<<< HEAD
leaf$sp.prod.mm2 <- leaf$NewArea.mm2/leaf$OldArea.mm2/6
cdplot(Prev~sp.prod.mm2,data=leaf)
=======
>>>>>>> dec056c9c33e2755d67a89c851a2d8ed7e24726b
