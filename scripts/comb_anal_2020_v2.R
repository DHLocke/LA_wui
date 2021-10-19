library(foreign)
#library(randomForest)
library(tree)
library(rpart)
library(sciplot)
library(party)
library(pROC) #for AUC calculation
library(pdp)
library(mlr)
library(glmulti)
#library(tidyverse)

##################################################
#Get the house data: basic BIA from bia_liveables and calculated summaries from bia2013_liveables_fire
#much massaging needed
setwd("c:/wollongong/projects/rfs2016/anal")
basicdat<-read.dbf("bia_liveables_flat.dbf")
neardat<-read.dbf("c:/wollongong/projects/rfs2016/bia/bia2013_liveables_clean.dbf")
vegdat<-read.dbf("house_veg_measures.dbf")
#Some of these variables need massaging
attach(basicdat)
#basicdat$DAM<-as.numeric(basicdat$DAMAGED)-1
basicdat$ROOF_PROFI[ROOF_PROFI=="Unknown"]<-NA
basicdat$CREWSUPP<-as.factor(ifelse(basicdat$CREW_PRE=="Yes","Pre",ifelse(basicdat$CREW_POST=="Yes","Post","None")))
basicdat$DOORMAT[DOORMAT=="Unknown"]<-NA
#NB as.numeric returns the levels so firs have to change using as.character
basicdat$GASBOT_FATE<-basicdat$GAS_BOTTLE
basicdat$GASBOTTLE<-as.factor(ifelse(GAS_BOTTLE=="NA","No","Yes"))
basicdat$STOREYS<-as.numeric(STOREYS)
basicdat$STOREYS[basicdat$STOREYS>3]<-NA
basicdat$SUB_HT<-as.numeric(as.character(SUB_HT))
basicdat$GAP_SIZE<-as.numeric(as.character(GAP_SIZE))
basicdat$BRANCH_HT<-as.numeric(as.character(BRANCH_HT))
basicdat$HOUSE_FRAM[HOUSE_FRAM=="Unknown"]<-NA
basicdat$DECK_ROOF<-as.factor(ifelse(basicdat$DECK_ROOF=="NA","No Deck",as.character(basicdat$DECK_ROOF)))

basicdat$DECK_MATER<-as.factor(ifelse(DECK_MATER=="Synthetic Wood","Other",as.character(DECK_MATER)))
#VEG_TYPE s with tiny samples 1, 2 and 3 respectively
basicdat$VEG_TYPE<-as.factor(ifelse(VEG_TYPE=="Herbaceous" | VEG_TYPE=="Vine / Climber" | VEG_TYPE=="Unknown (Unrecognisable)","NA",as.character(VEG_TYPE)))
basicdat$TANKCAPACI<-as.numeric(as.character(TANKCAPACI))
basicdat$FENCE_HT<-as.numeric(as.character(FENCE_HT))
basicdat$WINDOW_HT<-as.numeric(as.character(WINDOW_HT))
basicdat$FENCE_MTRL<-as.factor(ifelse(FENCE_MTRL=="Brushwood" | FENCE_MTRL=="Cement Sheet" ,"NA",as.character(FENCE_MTRL)))
basicdat$FENCE_OPEN<-as.numeric(as.character(FENCE_OPEN))
basicdat$PERSON_DAY[PERSON_DAY=="Unknown"]<-NA
basicdat$LEAVE<-as.factor(ifelse(LEAVE_POST=="Yes","Post",ifelse(LEAVE_DUR=="Yes","Dur",ifelse(LEAVE_PRIO=="Yes","Prior",as.character(LEAVE_PRIO)))))
basicdat$LEAVE<-as.factor(ifelse(basicdat$leave=="Unknown","NA",as.character(basicdat$leave)))
basicdat$GRASS_HT<-as.numeric(as.character(GRASS_HT))
basicdat$MULCH_DPTH<-as.numeric(as.character(MULCH_DPTH))
#There are some dodgy mulch depths
basicdat$TIMB_WIND[TIMB_WIND==-1]<-NA
basicdat$PLAIN_GLAS[PLAIN_GLAS==-1]<-NA
basicdat$WIND_USCRN[WIND_USCRN==-1]<-NA
basicdat$WIND_DMG[WIND_DMG==-1]<-NA
basicdat$DOOR_TIMB[DOOR_TIMB==-1]<-NA
basicdat$DOOR_BNT[DOOR_BNT==-1]<-NA
basicdat$DFRAM_TIM[DRFRAM_TIM==-1]<-NA
basicdat$CLADDING<-as.factor(substr(basicdat$CLADDING,1,9))
basicdat$BASE_TYPE<-as.factor(ifelse(basicdat$BASE_TYPE=="Above Ground","Elevated","On Ground"))
basicdat<-subset(basicdat,LIVE_ID!=212)
detach(basicdat)
attach(neardat)

#Preliminary anal suggests 10 m a good threshold for building dist
neardat$NRBLD<-ifelse(NRBLD_DIST>=0 & NRBLD_DIST<=10,1,0)
neardat$NRTRE_DIST<-ifelse(neardat$NRTRE_DIST==-1,60,neardat$NRTRE_DIST)
neardat$NRSHR_DIST<-ifelse(neardat$NRSHR_DIST==-1,60,neardat$NRSHR_DIST)
neardat$FENCE_DIST<-ifelse(BNFEN_DIST>=0 & BNFEN_DIST<UBFEN_DIST,BNFEN_DIST,UBFEN_DIST)
neardat$FENCE_DIST<-ifelse(neardat$FENCE_DIST==-1,60,neardat$FENCE_DIST)
neardat$GRASS_DIST<-ifelse(GRASS_DIST==-1,60,GRASS_DIST)
neardat$GRASS_AREA<-ifelse(GRASS_AREA==-1,0,GRASS_AREA)
neardat$MULCH_DIST<-ifelse(MULCH_DIST==-1,60,MULCH_DIST)
neardat$MULCH_AREA<-ifelse(MULCH_AREA==-1,0,MULCH_AREA)
neardat$HARD_DIST<-ifelse(HARD_DIST==-1,60,HARD_DIST)
neardat$HARD_AREA<-ifelse(HARD_AREA==-1,0,HARD_AREA)
neardat$NRBLD_BURN<-ifelse(NRBLD_BURN==-1,0,NRBLD_BURN)
neardat$NRGND_BURN<-ifelse(NRGND_BURN==-1,0,NRGND_BURN)
neardat$GRASS_BURN<-ifelse(GRASS_BURN==-1,0,GRASS_BURN)
neardat$MULCH_BURN<-ifelse(MULCH_BURN==-1,0,MULCH_BURN)
neardat$BNTRE_DIST<-10*round(ifelse(BNTRE_DIST==-1,60,BNTRE_DIST)/10)
neardat$BNSHR_DIST<-10*round(ifelse(BNSHR_DIST==-1,60,BNSHR_DIST)/10)
neardat$BNFEN_DIST<-10*round(ifelse(BNFEN_DIST==-1,60,BNFEN_DIST)/10)
neardat$WESTSW=ifelse(ASPECT>157 & ASPECT <337,1,0)
#disccovered some missing values in the near data
neardat$FFDI<-ifelse(neardat$IMP_TIME==15.3,44.8,neardat$FFDI)


basicdat[basicdat=="NA"] <- NA
basicdat[basicdat==-9999] <- NA

merge1dat<-merge(basicdat,neardat,by="ID")
merge2dat<-merge(merge1dat,vegdat,by="ID")

###########################################################################################
#get the rest of the data (Lidar)
lidardat<-read.dbf("house_lidar_anal.dbf")
combdat<-merge(merge2dat,lidardat,by="ID",all.x=TRUE)
combdat$DEFENCE<-combdat$DEF_COMB.x
#combdat$WESTSW <-combdat$WESTSW==1
combdat$WESTSW <-as.factor(ifelse(combdat$WESTSW==1,"Yes","No"))
#combdat<-subset(combdat,FIRE=="Linksview")


####Distance weighting (working)####
#distance weighted mean of damage
locdat<-combdat[,c(1,7,165,166)]
#locdat<-combdat[c(1:10),c(1,7,164,165)]
for (idno in 1 : nrow(locdat)){
   #make a copy of the data with the target id removed
   locdatcopy<-locdat[-idno,]
   testx<-locdat$X_COORD[idno]
   testy<-locdat$Y_COORD[idno]
   #this version is a linear distance weight
#   locdatcopy$distwt<-1/sqrt((locdatcopy$X_COORD-testx)^2+(locdatcopy$Y_COORD-testy)^2)
   #this version is a quadratic distance weight
   locdatcopy$distwt<-1/((locdatcopy$X_COORD-testx)^2+(locdatcopy$Y_COORD-testy)^2)
   distwtdam<-sum(locdatcopy$distwt*locdatcopy$DAMAGED.x)/ sum(locdatcopy$distwt)
   locdat$damwt[idno]<-distwtdam
}
locdat<-locdat[,c(1,5)]
combdatplus<-merge(combdat,locdat,by="ID")
#It appears this process has uncove
combdat<-subset(combdatplus,!is.nan(combdatplus$damwt))
combdat$anydam<-as.numeric(combdat$DAMAGE_LEV!="Untouched")
combdat$destroyed<-as.numeric(combdat$DAMAGE_LEV=="Destroyed")

#This is the final data so write it out
write.csv(combdat,"BIA_anal_2020.csv")
combdat<-read.csv("BIA_anal_2020.csv")

####cut down version for public use####
cutdat<-combdat[c(2:108,117:165,169:202,225:291,300)]
write.csv(cutdat,"FIRE_1322343_ms_data.csv")
testdat<-subset(combdat,RANDSEL.x>0.75)
moddat<-subset(combdat,RANDSEL.x<=0.75)

#Figure 2
windows(5,7)
par(mfrow=c(3,2))
par(mar=c(4,4,3,1))
hist(combdat$BURNHS_DIS,col=8,main="Burnt House Dist",xlab="Distance to Burnt House (m)")
hist(combdat$CONTVEG_W,col=8,main="Distance to Forest",xlab="Westerley Distance to Forest (m)")
hist(combdat$COV40M_W,col=8,main="%Vegcover40 W",xlab="% Veg Cover 40m W ")
hist(combdat$MIDCOV100,col=8,main="LiDAR Midcov 100", xlab="% LiDAR Mid Cover >100m")
hist(combdat$SLOPE,col=8,main="Slope",xlab="Slope (deg)")
hist(combdat$FFDI,col=8,main="FFDI",xlab="FFDI",xlim=c(7,80))


#Figure 4
windows(7,7)
par(mfrow=c(3,3))
par(mar=c(4,4,3,1))
bargraph.CI(50*round(combdat$BURNHS_DIS/50),combdat$DAMAGED.x,main="Burnt House Dist",xlab="Distance to Burnt House (m)",ylab="P(impact)",las=2,xlim=c(0,9.5))
bargraph.CI(combdat$DEFENCE,combdat$DAMAGED.x,main="Defence",ylab="P(impact)",xlab="Defence",las=2)
bargraph.CI(combdat$CLADDING,combdat$DAMAGED.x,main="Wall Cladding",xlab="",ylab="P(impact)",las=2)
par(mar=(c(6,4,4,2)))
bargraph.CI(combdat$LAWN_STATE,combdat$DAMAGED.x,main="Lawn State",ylab="P(impact)",las=2)
bargraph.CI(combdat$WESTSW,combdat$DAMAGED.x,main="West-south-west aspect",ylab="P(impact)",las=1)
par(mar=(c(5,4,4,2)))
bargraph.CI(20*round(combdat$MIDCOV100/20),combdat$DAMAGED.x,main="LiDAR Midcov100",ylab="P(impact)",xlab="LiDAR mid-cover >100m",las=1)
par(mar=(c(5,4,4,2)))
bargraph.CI(20*round(combdat$COV40M_W/20),combdat$DAMAGED.x,main="%Vegcover40 W",ylab="P(impact)",xlab="% Veg Cover 40 m W",las=2)
bargraph.CI(20*round(combdat$CONTVEG_W/20),combdat$DAMAGED.x,xlim=c(0,7),main="Cont For W",ylab="P(impact)",xlab="Westerley Distance to Forest (m)",las=2)
points(7,0.13,pch=19)
bargraph.CI(10*round(combdat$FFDI/10),combdat$DAMAGED.x,main="FFDI",xlab="FFDI (fire weather)",ylab="P(impact)",las=2,xlim=c(1.6,10))



par(mar=(c(8,4,2,2)))
bargraph.CI(basicdat$BASE_SUPP,basicdat$DAMAGED,main="Base Support",ylab="P(impact)",las=2)
bargraph.CI(combdat$MAINT_LEV,combdat$DAMAGED.x,main="Maintenance",xlab="Maintenance Level",ylab="P(impact)",las=2)


bargraph.CI(combdat$LAWN_STATE),fulldat$DAMAGED.x,main="Lawn State",ylab="P(damage)",xlab="Slope (Degrees)",las=1)
bargraph.CI(4*round(fulldat$SLOPE/4),fulldat$DAMAGED.x,main="Slope",ylab="P(damage)",xlab="Slope (Degrees)",las=1)

#New graphs for FIRE revision
bargraph.CI(10*round(combdat$FFDI/10),combdat$DAMAGED.x,main="FFDI",ylab="P(impact)",xlab="FFDI",las=1,xlim=c(3.8,8.5))



# Michaels code for testing accuracy
lidardat$pred <- predict(lidfor, OOB = TRUE)
lidardat$predtrue <- lidardat$pred>0.37
with(lidardat,table(dmg,predtrue))
varimp(lidfor)


#First examine the bargraphs and determnine whether they have non-sensical results, possibly as a result of damage bias
#  These variables have non-sensical results
#DRFRAM_TIM, DOORMAT, PLAIN_GLAS, DOOR_TIMB, DECK_PIERS
#MODELS WITH VARIABLES EXPLAINING >1% OF D
#this is the original version without burning houses or autocorrelation
final<-rpart(DAMAGED.x~LAWN_STATE+ BASE_SUPP+ WESTSW+ DEFENCE+COV40M_W+ CONTVEG_N+ CARPORT_CO+
               CLADDING+ COV40M_S+ DECK_TYPE+ COVER10M_W+ MAINT_LEV+ WATER_USED+ MIDCOV10+ MIDCOV100+ ELCOV100+
               LEAVE+  COVER10M_E+ WATER_TYPE+ SLOPE+ MIDCOV30+ COVER10M_N+ DECK_FLAMM+ COV40M_N+ ELCOV2+ 
               DECK_MATER+ CONTVEG_S+ CONTVEG_W+ NEARCOV10+ NEARVEG_E+ NEARVEG_S+ NEARVEG_W+ MIDCOV2+ ELCOV10+ 
               CREW_COMB+ UPCOV10+ MIDCOVHS+ GRASS_HT+ ELCOV30+ NRBLD_DIST + NEARVEG_N+ WINDSPEED+ SUB_HT+COVER10M_S+ 
               ROOF_MATER+ VEGOVERHAN+ FFDI, data=moddat, method='anova')


#This model includes spatial covariate (damwt), which is good for testing autocorelation but does not give sensible results
final<-rpart(DAMAGED.x~LAWN_STATE+ BASE_SUPP+ WESTSW+ DEFENCE+COV40M_W+ CONTVEG_N+ CARPORT_CO+
               CLADDING+ COV40M_S+ DECK_TYPE+ COVER10M_W+ MAINT_LEV+ WATER_USED+ MIDCOV10+ MIDCOV100+ ELCOV100+
               LEAVE+  COVER10M_E+ WATER_TYPE+ SLOPE+ MIDCOV30+ COVER10M_N+ DECK_FLAMM+ COV40M_N+ ELCOV2+ 
               DECK_MATER+ CONTVEG_S+ CONTVEG_W+ NEARCOV10+ NEARVEG_E+ NEARVEG_S+ NEARVEG_W+ MIDCOV2+ ELCOV10+ 
               CREW_COMB+ UPCOV10+ MIDCOVHS+ GRASS_HT+ ELCOV30+ NRBLD_DIST+ damwt + NEARVEG_N+ WINDSPEED+ SUB_HT+COVER10M_S+ 
               ROOF_MATER+ VEGOVERHAN+ FFDI, data=moddat, method='anova')

#this version included distance to nearest burnt house which accounts for house-to-house but not spatial autocorrelation
final<-rpart(DAMAGED.x~LAWN_STATE+ BASE_SUPP+ WESTSW+ DEFENCE+COV40M_W+ CONTVEG_N+ CARPORT_CO+
               CLADDING+ COV40M_S+ DECK_TYPE+ COVER10M_W+ MAINT_LEV+ WATER_USED+ MIDCOV10+ MIDCOV100+ ELCOV100+
               LEAVE+  COVER10M_E+ WATER_TYPE+ SLOPE+ MIDCOV30+ COVER10M_N+ DECK_FLAMM+ COV40M_N+ ELCOV2+ 
               DECK_MATER+ CONTVEG_S+ CONTVEG_W+ NEARCOV10+ NEARVEG_E+ NEARVEG_S+ NEARVEG_W+ MIDCOV2+ ELCOV10+ 
               CREW_COMB+ UPCOV10+ MIDCOVHS+ GRASS_HT+ ELCOV30+ NRBLD_DIST+ BURNHS_DIS + NEARVEG_N+ WINDSPEED+ SUB_HT+COVER10M_S+ 
               ROOF_MATER+ VEGOVERHAN+ FFDI, data=moddat, method='anova')



windows(4.5,4.5)
par(mar=c(1,1,2,1))
final3<-prune(final,cp=0.02)
plot(final3,branch=0.5);text(final3,cex=0.7)
printcp(final3)
print(final3)

combdat$predtree<-predict(final3, OOB = TRUE)
auc(roc(testdat$DAMAGED.x,predict(final2,testdat)))
auc(roc(moddat$DAMAGED.x,predict(final2,moddat)))
moddat$pred <- predict(final3, OOB = TRUE)
moddat$predtrue <- moddat$pred>0.37
with(moddat,table(DAMAGED.x,predtrue))

testdat$pred <- predict(final3,testdat, OOB = TRUE)
testdat$predtrue <- testdat$pred>0.15
with(testdat,table(DAMAGED.x,predtrue))


####Random Forest Model####
#this one plots all variables
cforestImpPlot <- function(x) {
  cforest_importance <<- v <- varimp(x)
  dotchart(v[order(v)],pch=19)
}

#This one only the best 20 variable
cforestImpPlot <- function(x) {
  cforest_importance <<- v <- varimp(x)
  v<-v[order(v)]
  end<-length(v)
  start<-end-20
  dotchart(v[start:end],pch=19)
}


forest<-cforest(DAMAGED.x~LAWN_STATE+ BASE_SUPP+ WESTSW+ DEFENCE+COV40M_W+ CONTVEG_N+ CARPORT_CO+
                  CLADDING+ COV40M_S+ DECK_TYPE+ COVER10M_W+ MAINT_LEV+ WATER_USED+ MIDCOV10+ MIDCOV100+ ELCOV100+
                  LEAVE+  COVER10M_E+ WATER_TYPE+ SLOPE+ MIDCOV30+ COVER10M_N+ DECK_FLAMM+ COV40M_N+ ELCOV2+ 
                  DECK_MATER+ CONTVEG_S+ CONTVEG_W+ NEARCOV10+ NEARVEG_E+ NEARVEG_S+ NEARVEG_W+ MIDCOV2+ ELCOV10+ 
                  CREW_COMB+ UPCOV10+ MIDCOVHS+ GRASS_HT+ ELCOV30+ NRBLD_DIST+ NEARVEG_N+ WINDSPEED+ SUB_HT+COVER10M_S+ 
                  ROOF_MATER+ VEGOVERHAN+ FFDI, data=combdat,controls=cforest_control(mtry=5, ntree=2000, mincriterion=0))

#this one includes distance to nearest burnt building
forest2<-cforest(DAMAGED.x~LAWN_STATE+ BASE_SUPP+ WESTSW+ DEFENCE+COV40M_W+ CONTVEG_N+ CARPORT_CO+
                  CLADDING+ COV40M_S+ DECK_TYPE+ COVER10M_W+ MAINT_LEV+ WATER_USED+ MIDCOV10+ MIDCOV100+ ELCOV100+
                  LEAVE+  COVER10M_E+ WATER_TYPE+ SLOPE+ MIDCOV30+ COVER10M_N+ DECK_FLAMM+ COV40M_N+ ELCOV2+ 
                  DECK_MATER+ CONTVEG_S+ CONTVEG_W+ NEARCOV10+ NEARVEG_E+ NEARVEG_S+ NEARVEG_W+ MIDCOV2+ ELCOV10+ 
                  CREW_COMB+ UPCOV10+ MIDCOVHS+ GRASS_HT+ ELCOV30+ NRBLD_DIST +BURNHS_DIS+ NEARVEG_N+ WINDSPEED+ SUB_HT+COVER10M_S+ 
                  ROOF_MATER+ VEGOVERHAN+ FFDI, data=combdat,controls=cforest_control(mtry=5, ntree=2000, mincriterion=0))

#this one includes distance weighted mean damage (spatial autocovariate)
forest3<-cforest(DAMAGED.x~LAWN_STATE+ BASE_SUPP+ WESTSW+ DEFENCE+COV40M_W+ CONTVEG_N+ CARPORT_CO+
                   CLADDING+ COV40M_S+ DECK_TYPE+ COVER10M_W+ MAINT_LEV+ WATER_USED+ MIDCOV10+ MIDCOV100+ ELCOV100+
                   LEAVE+  COVER10M_E+ WATER_TYPE+ SLOPE+ MIDCOV30+ COVER10M_N+ DECK_FLAMM+ COV40M_N+ ELCOV2+ 
                   DECK_MATER+ CONTVEG_S+ CONTVEG_W+ NEARCOV10+ NEARVEG_E+ NEARVEG_S+ NEARVEG_W+ MIDCOV2+ ELCOV10+ 
                   CREW_COMB+ UPCOV10+ MIDCOVHS+ GRASS_HT+ ELCOV30+ NRBLD_DIST +damwt+ NEARVEG_N+ WINDSPEED+ SUB_HT+COVER10M_S+ 
                   ROOF_MATER+ VEGOVERHAN+ FFDI, data=combdat,controls=cforest_control(mtry=5, ntree=2000, mincriterion=0))

forest<-forest2
combdat$predfor <- predict(forest, OOB = TRUE)
combdat$predtrue <- combdat$predfor>0.340
blob<-with(combdat,table(DAMAGED.x,predtrue))
blob
100*(blob[1]+blob[4])/sum(blob)
varimp(forest)
windows(3,6)
par(mar=c(4,1,2,1))
cforestImpPlot(forest)
windows(4,4)

forest %>% # the %>% operator is read as "and then"
  partial(pred.var = "CONTVEG_W") %>%
  plotPartial(smooth = FALSE, lwd = 2, ylab = "Prob. of Impact", xlim=c(0,150),ylim=c(0,0.5), xlab="Westerly Distance to Forest (m)")

forest %>% # the %>% operator is read as "and then"
  partial(pred.var = "COV40M_W") %>%
  plotPartial(smooth = FALSE, lwd = 2, ylab = "Prob. of Impact", xlim=c(0,100),ylim=c(0,0.5), xlab="Veg. Cover 40 m West (%)")

forest %>% # the %>% operator is read as "and then"
  partial(pred.var = "DEFENCE") %>%
  plotPartial(smooth = FALSE, lwd = 2, ylab = "Prob. of Impact",ylim=c(0,0.5), xlab="House Defended")

forest %>% # the %>% operator is read as "and then"
  partial(pred.var = "WESTSW") %>%
  plotPartial(smooth = FALSE, lwd = 2, ylab = "Prob. of Impact",ylim=c(0,0.5), xlab="Aspect is WSW")

forest %>% # the %>% operator is read as "and then"
  partial(pred.var = "BURNHS_DIS") %>%
  plotPartial(smooth = FALSE, lwd = 2, ylab = "Prob. of Impact",ylim=c(0,0.5),xlim=c(0,200), xlab="Distance to Burnt House")


forest %>% # the %>% operator is read as "and then"
  partial(pred.var = "ELCOV2") %>%
  plotPartial(smooth = FALSE, lwd = 2, ylab = "Prob. of Impact",ylim=c(0,0.5), xlab="Aspect is WSW")


############################################################
combdat$MAINTGREAT<-combdat$MAINT_LEV=="Great"
glmmod<-glm(DAMAGED.x~DEFENCE+CONTVEG_W+WESTSW+COV40M_S+MAINTGREAT+SLOPE,data=combdat,family=binomial)
summary(glmmod)


forest<-cforest(DAMAGED.x~ WESTSW+DEFENCE+CONTVEG_W+COV40M_W+COV40M_S+MAINT_LEV+SLOPE+CONTVEG_N+CLADDING+COVER10M_W, data=combdat,controls=cforest_control(mtry=3, ntree=500, mincriterion=0))
forest<-cforest(DAMAGED.x~ ELCOV2, data=combdat)
cforestImpPlot(forest)

aggregate(DAMAGED.x,by=list(WESTSW),FUN=mean)
liddat<-combdat[,178:193]
round(cor(liddat),2)
table(DEFENCE,BASE_SUPP)
sqrt(summary(lm(COV40M_W~LAWN_STATE))$r.squared)
sqrt(summary(lm(COV40M_W~BASE_SUPP))$r.squared)
sqrt(summary(lm(COV40M_W~CARPORT_CO))$r.squared)
sqrt(summary(lm(COV40M_W~CLADDING))$r.squared)
sqrt(summary(lm(COV40M_W~BASE_TYPE))$r.squared)
sqrt(summary(lm(COV40M_W~WATER_USED))$r.squared)
sqrt(summary(lm(COV40M_W~LEAVE))$r.squared)

#################################################################
#Basic plots

windows(4,4)
bargraph.CI(DEF_COMB,DAMAGED,main="Defence",ylab="P(impact)",las=2)
par(mar=(c(6,4,4,2)))
bargraph.CI(basicdat$LAWN_STATE,basicdat$DAMAGED,main="Lawn State",ylab="P(impact)",las=2)
par(mar=(c(8,4,2,2)))
bargraph.CI(basicdat$BASE_SUPP,basicdat$DAMAGED,main="Base Support",ylab="P(impact)",las=2)
bargraph.CI(WESTSW,DAMAGED.x,main="West-south-west aspect",ylab="P(impact)",las=1)
par(mar=(c(5,4,4,2)))
bargraph.CI(20*round(COV40M_W/20),DAMAGED.x,main="%Vegcover40 W",ylab="P(impact)",xlab="% Veg Cover 40 m W",las=2)
bargraph.CI(20*round(CONTVEG_W/20),DAMAGED.x,,xlim=c(0,7),main="Cont For W",ylab="P(impact)",xlab="Westerley Distance to Forest (m)",las=2)
points(7,0.13,pch=19)

bargraph.CI(10*round(FFDI/10),DAMAGED,main="FFDI",ylab="P(impact)",las=2)
summary(lm(DAMAGED~FFDI,family=binomial))



####Univariate models####
#setwd("c:/wollongong/projects/rfs2016/anal")
#combdat contains the data for analysis n=541, 300+ variables, DAMAGED.x is the dependent
combdat<-read.csv("bia_anal_2020.csv")
#table1 contains a list of predictor variables for analysis and vectors to store the results for each variable
table1 <- read.csv("table1_list.csv", stringsAsFactors = FALSE)

table1$dev2<-0
table1$n2<-0

dat.model <- data.frame(y = combdat$DAMAGED.x, x = 0)
i<-0
for (varname in table1$upvar) {
  i<-i+1
  if (varname %in% colnames(combdat)) {
    dat.model$x <- combdat[, varname]
    mod <- glm(y ~ x, data = dat.model, family = binomial())
  } else {
    warning(varname, " not found in combdat", immediate. = TRUE)
    mod <- NULL
  }
  
  if (!is.null(mod)) {
    table1$n2[i] <- nrow(combdat) - sum(is.na(dat.model$x))
    table1$dev2[i] <- 100 * (mod$null.deviance - mod$deviance) / mod$null.deviance
  }
}
table1<-table1[c(1,2,4,5,8,9)]
write.csv(table1,"table1c.csv")
