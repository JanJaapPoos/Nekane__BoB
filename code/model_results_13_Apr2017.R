###############################################################################
# Model results
# Fleet dynamics
# IJmuiden, IMARES
# 20th October 2016
###############################################################################

#==============================================================================
# libraries
#==============================================================================
library(nekaneR)
library(plyr)
library(reshape2)
library(FLCore)
library(ggplotFL)
library (FLFishery)
library(RColorBrewer)
library(RDynState5NAI)

setwd("~/Dropbox/BoB/DynState/2017/results")
#==============================================================================
# data
#==============================================================================
res <- list()

load("scenario1.RData")
res[[1]]<- z
rm(z)

load("scenario2.RData")
res[[2]]<- z
rm(z)

load("scenario3.RData")
res[[3]]<- z
rm(z)

load("scenario4.RData")
res[[4]]<- z
rm(z)

load("5spp_dynstate_Inew.RData")

load("~/Dropbox/BoB/DynState/Imares_july/data/model_conditioning.RData")

#==============================================================================
# Spatial and temporal patterns in cath rate
#==============================================================================

# map
# Dropbox/BoB/DynState/code/map.R

#==============================================================================
#  Figure 1: Spatial and temporal patterns in cath rates
#  Condition the model with the 2003-2012 average catch rates
#==============================================================================

catch_mean           <- lapply(catchmean, yearMeans)

# replace NA with 0, just for plotting
#-------------------------------------
catch_mean           <- lapply(catch_mean, function (x) replace (x,is.nan(x) | is.infinite(x),0))

catch_mean_df        <- as.data.frame(catch_mean)
catch_mean_df$q      <- with(catch_mean_df, paste(area, unit, sep=" "))
catch_mean_df        <- subset(catch_mean_df, select=c("len","season","data","qname","q"))
names(catch_mean_df) <- c("cat", "season","data","spp","area")

levels(catch_mean_df$spp) <- c(levels(catch_mean_df$spp),' horse\n mackerel')
catch_mean_df$spp[catch_mean_df$spp=="horse_mackerel"] <- c(' horse\n mackerel')

# remove areas VI PTB and VII PTB
#----------------------------------
catch_mean_df <-subset(catch_mean_df, !(area %in% c("VI PTB","VII PTB")))

# replace discards the others for just discards withpout MCRS
#-------------------------------------------------------------
a     <- subset(catch_mean_df, spp %in% "others" & cat %in% c('< MCRS'))
a$cat <- 'discards'

b     <- subset(catch_mean_df, spp %in% "others" & cat %in% c('> MCRS'))
b$cat <- 'landings'

catch_mean_df <-subset(catch_mean_df, !(spp %in% "others" & cat %in% c('< MCRS','> MCRS')))
catch_mean_df <- rbind(catch_mean_df, a, b)

rm(a, b, catch_mean)

# Order factor as I wish for the plotUnder a discard ban, different flexibilities could be applied to the system: international swaps, inter-species quota swap and de-minimis exceptions. 
#------------------------------------
catch_mean_df$cat  <- factor(catch_mean_df$cat, levels=c('> MCRS','< MCRS', 'landings','discards'))
catch_mean_df$area <- factor(catch_mean_df$area,levels=c("VI OTB","VII OTB","VIIIabd OTB","VIIIabd PTB"))
catch_mean_df$spp  <- factor(catch_mean_df$spp, levels=c("hake","megrim","mackerel",' horse\n mackerel',"others"))


#==============================================================================
#  Figure 1: Catch/vessel*days at sea
#==============================================================================
png(filename="~/Dropbox/BoB/DynState/2017/figures/cpue_mean_logscale.png",
    width=15, height=13, units="cm", res=500, pointsize=8)

options(scipen=8)

 ggplot() +  
          geom_line (data= catch_mean_df , aes(x =factor(season), y =pmax(data,0.01),group=cat, shape=cat, 
                   linetype=cat),size=0.25)+
          geom_point(data= catch_mean_df , aes(x =factor(season),y =pmax(data,0.01), group=cat, shape=cat, 
                   linetype=cat), size=1)+
          facet_grid(area~spp)+ 
          scale_shape_manual(name= "CPUE", labels = c("> MCRS","< MCRS","landings","discards"),
          values = c(19, 17, 1, 2), guide='legend')+
          scale_linetype_manual(name= "CPUE", labels = c("> MCRS","< MCRS","landings","discards"), 
          values=c('solid','dashed','solid','dashed'), guide='legend')+
          scale_y_log10(breaks=c(0.01,0.1,1,10,100))+
          theme_bw()+
          theme(panel.background = element_blank(),legend.title=element_blank(),
                   legend.position="bottom",axis.text=element_text(size=8),
                   strip.text = element_text(size = 8), text = element_text(size=7))+
          xlab("season") +
          ylab("CPUE (t/vessel)") 

dev.off()


#==============================================================================
# Location choice: Effort map along different scenarios
# Results compared to 2012 data ( we are using 2012 quota along different scenarios)
#==============================================================================

detach("package:FLFishery", unload=TRUE)
detach("package:nekaneR", unload=TRUE)
detach("package:ggplotFL", unload=TRUE)

# Effort (2012 fishing days) data from the study period
#------------------------------------
ref_effort           <- as.data.frame(effort_days[,as.character(2012)])
ref_effort           <- with(ref_effort,data.frame(unit,season,area,data))

ref_effort           <- subset(ref_effort, !(is.na(data)))
ref_effort$scenario  <- "2012" #"average"
ref_effort$choice    <- with(ref_effort, paste0(area,unit))
ref_effort$vessel    <- 1

names(ref_effort)    <- c("gear","season","area","effort","scenario", "choice", "vessel")
ref_effort           <- with(ref_effort,data.frame(choice,season,effort,scenario, area, gear, vessel)) 


# Effort average(average 2003- 2012 fishing days) data from the study period
#------------------------------------
# ref_effort           <- as.data.frame(yearMeans(effort_days))
# ref_effort           <- with(ref_effort,data.frame(unit,season,area,data))
# 
# ref_effort           <- subset(ref_effort, !(is.na(data)))
# ref_effort$scenario  <- "average"
# ref_effort$choice    <- with(ref_effort, paste0(area,unit))
# ref_effort$vessel    <- 1
# 
# names(ref_effort)    <- c("gear","season","area","effort","scenario", "choice", "vessel")
# ref_effort           <- with(ref_effort,data.frame(choice,season,effort,scenario, area, gear, vessel)) 

# List with all data that I need from the different scenarios
#------------------------------------
effort_res_allsimul  <- list()

scenario             <- c("Scenario I", "Scenario II", "Scenario III", "Scenario IV")


#==============================================================================
#  Figure 2: Effort, The simulations were made for 200 vessels 
#==============================================================================

detach("package:FLCore", unload=TRUE)

for(i in 1:4){ 
      
        effort                     <- as.matrix(effort(sim(res[[i]])))
        choice                     <- as.matrix(choice(sim(res[[i]])))
      
        effort_scenario            <- as.data.frame(cbind(effort, choice))
        effort_scenario$vessel     <- rep(1:200)
        effort_scenario$season     <- c(rep(1,200),rep(2,200),rep(3,200),rep(4,200)) 
        effort_scenario$scenario   <- scenario[i]
        effort_scenario$V2         <- as.character(effort_scenario$V2)
        effort_scenario$choice     <- as.character(effort_scenario$V2)
        effort_scenario$gear       <- substr(effort_scenario$V2, nchar(effort_scenario$V2)-3+1, nchar(effort_scenario$V2))
        effort_scenario$area       <- substr(effort_scenario$V2, nchar(effort_scenario$V2)-10+1, nchar(effort_scenario$V2)-3)
        effort_scenario            <- effort_scenario[,-2]

          
        names(effort_scenario)[[1]]<- "effort"

        effort_scenario            <- with(effort_scenario,data.frame(choice,season,effort,scenario, area, gear, vessel))

        effort_res_allsimul[[i]]   <- effort_scenario  

        rm (effort_scenario, effort, choice)
}


# EFFORT; all simulations + include the 2012 data (maybe delete it after)
#--------------------------------------------------------------------------
effort_res_allsimul_df          <- rbind (ref_effort, do.call(rbind, effort_res_allsimul))
                              
# Rename area where there is no effort to STAY IN PORT
#--------------------------------------------------------------------------                                    
levels(effort_res_allsimul_df$choice) <- c(levels(effort_res_allsimul_df$choice),"Stay in port")
levels(effort_res_allsimul_df$area)   <- c(levels(effort_res_allsimul_df$area),"Stay in port")
levels(effort_res_allsimul_df$gear)   <- c(levels(effort_res_allsimul_df$gear),"Stay in port")

effort_res_allsimul_df[is.na(effort_res_allsimul_df)] <- c("Stay in port")

effort_res_allsimul_df$choice         <- factor(effort_res_allsimul_df$choice,levels=c("Stay in port",
                                                      "VIOTB","VIIOTB","VIIIabdOTB","VIIIabdPTB"))
effort_res_allsimul_df$scenario       <- factor(effort_res_allsimul_df$scenario,levels=c("2012","Scenario I",
                                                      "Scenario II", "Scenario III", "Scenario IV"))


# Remove where is no effort from 15 simulations
#------------------------------------------------------------------------                                                          
random<- c(100,52, 98,24,101 ,129 ,197, 200,  58, 146, 174,  87, 190,  49,  64)

effort_res_15simul_df           <- subset(effort_res_allsimul_df,(vessel %in% random)) # only 15 vessels by season 
effort_res_15simul_df           <- subset(effort_res_15simul_df, !(effort %in% c("0")))
effort_res_15simul_df $effort   <- as.numeric(as.character(effort_res_15simul_df$effort))

# Aggregate effort by scenario, season and area
#--------------------------------------------------------------------------
effort_res_15simul_df           <- aggregate(effort ~ choice+season+scenario+area+gear, data=effort_res_15simul_df, FUN=sum)


# Here also with gear type
#--------------------------------------------------------------------------
data1 <- dcast (effort_res_15simul_df, choice+season ~scenario, value.var= "effort", fun.aggregate=sum)
data2 <- dcast (effort_res_15simul_df, choice ~scenario, value.var= "effort", fun.aggregate=sum)
data3 <- tapply(effort_res_15simul_df$effort, effort_res_15simul_df$scenario, FUN=sum)

# Create table with the total effort by gear, season, area and scenario
#--------------------------------------------------------------------------
write.table(data1, file="~/Dropbox/BoB/DynState/2017/data/effort_choice_season.csv", sep=";")
write.table(data2, file="~/Dropbox/BoB/DynState/2017/data/effort_choice.csv", sep=";")
write.table(data3, file="~/Dropbox/BoB/DynState/2017/data/effort_scenario.csv", sep=";")

rm(data1, data2, data3)


# Remove mean effort or 2012 effort
effort_res_15simul_df <- effort_res_15simul_df[!effort_res_15simul_df$scenario=="2012",]
#effort_res_15simul_df <- rbind(effort_res_15simul_df, ref_effort[,1:6])
#==============================================================================
png(filename="~/Dropbox/BoB/DynState/2017/figures/effort_area_gear_15simul.png",
    width=15, height=6, units="cm", res=500, pointsize=8)
   
        ggplot(effort_res_15simul_df, aes(season,effort, fill=choice)) + 
                geom_bar(stat="identity", colour= "black")+
                facet_grid(gear~scenario, scales = "free_y")+
                scale_fill_manual(values= c("gray80","gray60","gray40","gray20"))+  
                scale_y_continuous(breaks=seq(0,1800,300))+
                theme_bw()+
                theme(panel.background = element_blank(),legend.title=element_blank(), 
                        legend.position="right",axis.text=element_text(size=8),
                        strip.text = element_text(size = 8), text = element_text(size=7))+
                xlab("season") +
                ylab("Fishing effort (days/season)")

dev.off()
#==============================================================================

#==============================================================================
#  Figure 3: Area choice proportional stacked bar graph with 200 simulations
#==============================================================================
# When there is no effort, means the vessel STAYs IN PORT
# nedd to assign a value to effort to give a proportion when the choice is stay in port 
trip           <- count(effort_res_allsimul_df,c("scenario","choice", "season","area","gear"))
names(trip)[6] <- "trip"

all_effort     <-aggregate(as.numeric(effort) ~ scenario+choice+season+area+gear, data=effort_res_allsimul_df, FUN=sum)

names(all_effort)[6]<- "effort"
#all_effort$effort[all_effort$effort==0] <-1 # Being 0 does not appear in the graph

all_effort     <-merge(all_effort, trip, by=c("scenario","choice", "season","area","gear"),all.x=TRUE)
all_effort$trip[all_effort$scenario=="2012"]<- all_effort$effort[all_effort$scenario=="2012"]

all_effort <- all_effort[!all_effort$scenario=="2012",]
#==============================================================================

library(scales)

png(filename="~/Dropbox/BoB/DynState/2017/figures/choice_area_200simulations.png",
    width=15, height=5, units="cm", res=500, pointsize=8)
   
         ggplot(all_effort, aes(x=as.factor(season), y=trip, fill=choice)) + 
                  geom_bar(stat="identity", position = "fill", colour="black")+
                  facet_grid(.~scenario)+ 
                  scale_y_continuous( labels = percent)+
                  scale_fill_manual(values= c("gray100","gray80","gray60","gray40","gray20"))+
                  theme_bw()+
                  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                       panel.background = element_blank(),legend.title=element_blank(),
                       legend.position="right",axis.text=element_text(size=8),
                       strip.text = element_text(size = 8),
                       text = element_text(size=7))+
                  xlab("season") +
                  ylab("Fishing choice")

dev.off()
#==============================================================================

#==============================================================================
# Catch composition: Landings and discards along the different scenarios
#==============================================================================

#combine all
#-----------
catch_results <- vector(mode="list", length=5)

scenario<- c("2012", "Scenario I", "Scenario II", "Scenario III", "Scenario IV")
names(catch_results)<- scenario

# Species from each scenario varied in order
species       <- list()
species [[1]] <- c("mac", "hom", "hke", "lez", "others")
species [[2]] <- c("mac", "hke", "lez", "hom", "others")
species [[3]] <- c("mac", "hke", "lez", "hom", "others")
species [[4]] <- c("mac", "hom", "lez", "hke", "others")
species [[5]] <- c("mac", "hke", "hom", "lez", "others") 

library("FLCore")

# reorganizing 2012 FLQuants as I wish
# there are low values of <MCRS landings that I will add them to the discards
#----------- 

foo_na<- function(x){
      x[is.na(x)] <- 0
      return(x)
}

ref_landings <- lapply(lapply(landings, '[',j=10), foo_na)
ref_discards <- lapply(lapply(discards, '[',j=10), foo_na)

for (i in 1:5){  
         if (i<5){ 
                  ref_discards[[i]][1,] <- ref_discards[[i]][1,]+ref_landings[[i]][1,]
                  ref_landings[[i]][1,] <- 0
         }
         if (i==4){
                  ref_discards[[i]][2,] <- ref_landings[[i]][1,]
                  ref_discards[[i]][1,] <- ref_landings[[i]][1,] <- 0
         }
}

for(i in 1:5){ 

         if (i==1){

                  # 2012 data
                  #-----------
                  catch_res                   <- rbind(melt(ref_landings))
                  catch_res$ discards         <- c (melt(ref_discards)$value)
                  catch_res[is.na(catch_res)] <- 0
                  catch_res$ catch            <- catch_res$value + catch_res$discards
                  catch_res$ scenario         <- scenario[[i]]                               
                  catch_res$ effort           <- round(rep(rbind(melt(effort_days[,as.character(2012)][rep(seq_len(nrow(effort_days[,as.character(2012)])),each=2),]))$value,5)) 
                  #catch_res$ effort           <- round(rep(rbind(melt(yearMeans(effort_days)[rep(seq_len(nrow(yearMeans(effort_days))),each=2),]))$value,5)) 
                  catch_res$ effort[is.na(catch_res$ effort)] <- 0 
                  
                  names(catch_res) <- c("cat", "year","gear", "season", "area", "vessel", "landings", "spp", "discards", "catch", "scenario","effort")                    
                  catch_res <- catch_res[,c("landings","discards", "catch", "spp","scenario","cat", "season", "vessel","area","gear","effort")] 
                  catch_res <- melt(catch_res, id=c("scenario","season","spp","cat", "vessel","area",  "gear", "effort"))
                  catch_res$var <- apply(catch_res[,c("variable", "cat")],1,paste, collapse=" ")
         }

         
         else {
                  # In scenario I, discarding is allowed, therefore for the non conditioning species
                  # need to explicit allow to discard the undersized landings
                  if (i==2){
                           spp3Discards(sim(res[[i-1]]))[1,,] <-spp3Landings(sim(res[[i-1]]))[1,,]
                           spp3Landings(sim(res[[i-1]]))[1,,] <- 0
                           spp4Discards(sim(res[[i-1]]))[1,,] <-spp4Landings(sim(res[[i-1]]))[1,,]
                           spp4Landings(sim(res[[i-1]]))[1,,] <- 0
                  }

                  # scenarios results
                  #----------- 
                  spp5Discards(sim(res[[i-1]]))[2,,] <-spp5Landings(sim(res[[i-1]]))[1,,]
                  spp5Landings(sim(res[[i-1]]))[1,,] <- 0
           
                  catch_res             <- as.data.frame(rbind(as.matrix(spp1Landings(sim(res[[i-1]]))),
                                                             as.matrix(spp2Landings(sim(res[[i-1]]))),
                                                             as.matrix(spp3Landings(sim(res[[i-1]]))),
                                                             as.matrix(spp4Landings(sim(res[[i-1]]))),
                                                             as.matrix(spp5Landings(sim(res[[i-1]])))))      
                  names(catch_res)      <- "landings"
                  catch_res$discards    <- c(rbind(spp1dis=as.matrix(spp1Discards(sim(res[[i-1]]))), 
                                                   spp2dis=as.matrix(spp2Discards(sim(res[[i-1]]))), 
                                                   spp3dis=as.matrix(spp3Discards(sim(res[[i-1]]))), 
                                                   spp4dis=as.matrix(spp4Discards(sim(res[[i-1]]))), 
                                                   spp5dis=as.matrix(spp5Discards(sim(res[[i-1]])))))




                  catch_res $catch       <- catch_res$ landings + catch_res$discards
                  catch_res$spp          <- as.factor(c(rep(species[[i]], each=1600)))
                  levels(catch_res$spp)  <- c(levels(catch_res$spp),"hake","horse_mackerel","megrim","mackerel")
                  
                  catch_res$spp[catch_res$spp=="hke"] <- c("hake")
                  catch_res$spp[catch_res$spp=="hom"] <- c("horse_mackerel")
                  catch_res$spp[catch_res$spp=="lez"] <- c("megrim")
                  catch_res$spp[catch_res$spp=="mac"] <- c("mackerel")
                     
                  catch_res$scenario  <- scenario[[i]]
                  catch_res$cat       <- rep(c('< MCRS','> MCRS'))
                  catch_res$season    <- c(rep(1,400),rep(2,400),rep(3,400),rep(4,400))
                  catch_res$vessel    <- rep(1:200,each=2)   
                  catch_res$area      <- rep(rep(effort_res_allsimul[[i-1]]$area,each=2),5)
                  catch_res$gear      <- rep(rep(effort_res_allsimul[[i-1]]$gear,each=2),5)
                  catch_res$effort    <- rep(rep(effort_res_allsimul[[i-1]]$effort,each=2),5)
                  catch_res           <- melt(catch_res, id=c("scenario","season","spp","cat","vessel","area","gear","effort"))                     
                  catch_res$var       <- apply(catch_res[,c("variable", "cat")],1,paste, collapse=" ")
         }

         catch_results[[i]] <- catch_res
         rm(catch_res)
}

# CATCHES; all simulations + include the 2012 data (maybe delete it after)
#--------------------------------------------------------------------------
catch_res_allsimul_df <-  do.call(rbind, catch_results)

# Rename the species
#----------- 
catch_res_allsimul_df[c(8,10)]      <- lapply(catch_res_allsimul_df[c(8,10)], function(x) as.numeric(as.character(x)))
catch_res_allsimul_df[c(1:7,9,11)]  <- lapply(catch_res_allsimul_df[c(1:7,9,11)], function(x) as.factor(x))

levels(catch_res_allsimul_df$spp)   <- c(levels(catch_res_allsimul_df$spp)," horse\n mackerel")
catch_res_allsimul_df$spp[catch_res_allsimul_df$spp=="horse_mackerel"] <- c(' horse\n mackerel')

catch_res_allsimul_df$spp           <- factor(catch_res_allsimul_df$spp , 
                                        levels=c("hake","megrim","mackerel"," horse\n mackerel","others"))
catch_res_allsimul_df$scenario      <- factor(catch_res_allsimul_df$scenario,levels=c("2012","Scenario I",
                                                      "Scenario II", "Scenario III", "Scenario IV"))
catch_res_allsimul_df$var    <- factor(catch_res_allsimul_df$var , levels=c('discards < MCRS','discards > MCRS', 'landings < MCRS','landings > MCRS', "catch < MCRS", "catch > MCRS"))

write.table(catch_res_allsimul_df, file="~/Dropbox/BoB/DynState/2017/data/Results_catch_200simul.csv", sep=";")

# CPUE; mean catch by vessel with uncertainty (mean and quantiles)
#--------------------------------------------------------------------------
catch_res            <- catch_res_allsimul_df
catch_res$spp        <- factor(catch_res$spp , levels=c("hake","megrim","mackerel"," horse\n mackerel","others"))
catch_res$scenario   <- factor(catch_res$scenario,levels=c("2012","Scenario I", "Scenario II", "Scenario III", "Scenario IV"))
catch_res$var        <- factor(catch_res$var , levels=c('discards < MCRS','discards > MCRS', 'landings < MCRS','landings > MCRS', "catch < MCRS", "catch > MCRS"))

# Change betweemn "catch", "landings" and "discards"
#--------------------------------------------------------------------------
xx<- catch_res[catch_res$variable=="catch",]
xx <- xx[!xx$scenario=="2012",]

qqq<- aggregate(. ~ scenario+season+spp+var+gear+vessel, data=xx, FUN=sum)

df <- expand.grid( scenario=c("Scenario I", "Scenario II", "Scenario III", "Scenario IV"),season= c("1","2","3","4"), spp= c("hake","megrim","mackerel"," horse\n mackerel","others"), var=c("catch < MCRS", "catch > MCRS"), gear=c("OTB", "PTB"), vessel=c(1:200))

qq<- merge(df,qqq, all.x=T)
qq$value[is.na(qq$value)]<- 0

facetcond <- data.frame("scenario"=c("Scenario I","Scenario II", "Scenario III","Scenario IV"),"spp"=c("mackerel","mackerel","mackerel","mackerel","hake","hake"," horse\n mackerel","hake"))

#==============================================================================
#  Figure 4: CPUE composition along scenarios
#==============================================================================
png(filename="~/Dropbox/BoB/DynState/2017/figures/Res_catch_average_quantiles.png",
    width=15, height=13, units="cm", res=500, pointsize=8)

options(scipen=8)

ggplot()+ 
    facet_grid(spp~scenario)+ 
    stat_summary(data= qq, aes(factor(season), pmax(value,0.1), group=interaction(var, gear), fill= interaction(var, gear)), geom="ribbon", fun.ymin = function(x) quantile(x, 0.025), fun.ymax = function(x) quantile(x, 0.975), alpha=0.2) +
    stat_summary(data= qq, aes(factor(season), pmax(value,0.1), group=interaction(var, gear), colour=interaction(var, gear),linetype= interaction(var, gear)), geom="line", size= 0.25, fun.y=mean)+
    stat_summary(data= qq, aes(factor(season), pmax(value,0.1), group=interaction(var, gear), shape=interaction(var, gear), colour= interaction(var, gear)), geom="point", size= 1, , fun.y=mean)+
    scale_colour_manual(name= "", labels = c("OTB catch < MCRS","OTB catch > MCRS","PTB catch < MCRS","PTB catch > MCRS"),values = c("steelblue4", "navyblue", "gray8","black"), guide='legend')+
    scale_fill_manual(name= "", labels = c("OTB catch < MCRS","OTB catch > MCRS","PTB catch < MCRS","PTB catch > MCRS"),values = c("steelblue4", "navyblue", "gray8","black"), guide='legend')+
    scale_linetype_manual(name= "", labels = c("OTB catch < MCRS","OTB catch > MCRS","PTB catch < MCRS","PTB catch > MCRS"),values=c("dashed","solid","dashed","solid"), guide='legend')+
    scale_shape_manual(name= "", labels = c("OTB catch < MCRS","OTB catch > MCRS","PTB catch < MCRS","PTB catch > MCRS"),values = c(19, 17, 1, 2), guide='legend')+
    geom_rect(data = facetcond,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = 0.1) +
    theme_bw()+ scale_y_log10(breaks=c(0.01,0.1,1,10,100))+
    guides(fill=guide_legend(ncol=2)) +
    theme(panel.background = element_blank(),legend.title=element_blank(),
          legend.position="bottom",axis.text=element_text(size=8),
          #legend.position=c(0.9,0.14),axis.text=element_text(size=8),
          strip.text = element_text(size = 8), text = element_text(size=7))+
    xlab("season") +
    ylab("CPUE (t/season)") 

dev.off()

# Change betweemn "catch", "landings" and "discards"
#--------------------------------------------------------------------------
xx<- catch_res[catch_res$variable=="landings",]
xx <- xx[!xx$scenario=="2012",]

qqq<- aggregate(. ~ scenario+season+spp+var+gear+vessel, data=xx, FUN=sum)

df <- expand.grid( scenario=c("Scenario I", "Scenario II", "Scenario III", "Scenario IV"),season= c("1","2","3","4"), spp= c("hake","megrim","mackerel"," horse\n mackerel","others"), var=c("landings < MCRS", "landings > MCRS"), gear=c("OTB", "PTB"), vessel=c(1:200))

qq<- merge(df,qqq, all.x=T)
qq$value[is.na(qq$value)]<- 0

facetcond <- data.frame("scenario"=c("Scenario I","Scenario II", "Scenario III","Scenario IV"),"spp"=c("mackerel","mackerel","mackerel","mackerel","hake","hake"," horse\n mackerel","hake"))

#==============================================================================
#  Figure 5: LPUE composition along scenarios
#==============================================================================
png(filename="~/Dropbox/BoB/DynState/2017/figures/Res_landings_average_quantiles.png",
    width=15, height=13, units="cm", res=500, pointsize=8)

options(scipen=8)

ggplot()+ 
  facet_grid(spp~scenario)+ 
  stat_summary(data= qq, aes(factor(season), pmax(value,0.1), group=interaction(var, gear), fill= interaction(var, gear)), geom="ribbon", fun.ymin = function(x) quantile(x, 0.025), fun.ymax = function(x) quantile(x, 0.975), alpha=0.2) +
  stat_summary(data= qq, aes(factor(season), pmax(value,0.1), group=interaction(var, gear), colour=interaction(var, gear),linetype= interaction(var, gear)), geom="line", size= 0.25, fun.y=mean)+
  stat_summary(data= qq, aes(factor(season), pmax(value,0.1), group=interaction(var, gear), shape=interaction(var, gear), colour= interaction(var, gear)), geom="point", size= 1, , fun.y=mean)+
  scale_colour_manual(name= "", labels = c("OTB landings < MCRS","OTB landings > MCRS","PTB landings < MCRS","PTB landings > MCRS"),values = c("firebrick2", "red4", "gray8","black"), guide='legend')+
  scale_fill_manual(name= "", labels = c("OTB landings < MCRS","OTB landings > MCRS","PTB landings < MCRS","PTB landings > MCRS"),values = c("firebrick2", "red4", "gray8","black"), guide='legend')+
  scale_linetype_manual(name= "", labels = c("OTB landings < MCRS","OTB landings > MCRS","PTB landings < MCRS","PTB landings > MCRS"),values=c("dashed","solid","dashed","solid"), guide='legend')+
  scale_shape_manual(name= "", labels =c("OTB landings < MCRS","OTB landings > MCRS","PTB landings < MCRS","PTB landings > MCRS"),values = c(19, 17, 1, 2), guide='legend')+
  geom_rect(data = facetcond,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = 0.1) +
  theme_bw()+ scale_y_log10(breaks=c(0.01,0.1,1,10,100))+
  guides(fill=guide_legend(ncol=2)) +
  theme(panel.background = element_blank(),legend.title=element_blank(),
        legend.position="bottom",axis.text=element_text(size=8),
        #legend.position=c(0.9,0.14),axis.text=element_text(size=8),
        strip.text = element_text(size = 8), text = element_text(size=7))+
  xlab("season") +
  ylab("LPUE (t/season)") 

dev.off()

# Change betweemn "catch", "landings" and "discards"
#--------------------------------------------------------------------------
yy<- catch_res[catch_res$variable=="discards",]
yy <- yy[!yy$scenario=="2012",]

qqq<- aggregate(. ~ scenario+season+spp+var+gear+vessel, data=yy, FUN=sum)

df <- expand.grid( scenario=c("Scenario I", "Scenario II", "Scenario III", "Scenario IV"),season= c("1","2","3","4"), spp= c("hake","megrim","mackerel"," horse\n mackerel","others"), var=c("discards < MCRS", "discards > MCRS"), gear=c("OTB", "PTB"), vessel=c(1:200))

qq<- merge(df,qqq, all.x=T)
qq$value[is.na(qq$value)]<- 0

facetcond <- data.frame("scenario"=c("Scenario I","Scenario II", "Scenario III","Scenario IV"),"spp"=c("mackerel","mackerel","mackerel","mackerel","hake","hake"," horse\n mackerel","hake"))

#==============================================================================
#  Figure 6: DPUE composition along scenarios
#==============================================================================
png(filename="~/Dropbox/BoB/DynState/2017/figures/Res_discards_average_quantiles.png",
    width=15, height=13, units="cm", res=500, pointsize=8)

options(scipen=8)

ggplot()+ 
  facet_grid(spp~scenario)+ 
  stat_summary(data= qq, aes(factor(season), pmax(value,0.1), group=interaction(var, gear), fill= interaction(var, gear)), geom="ribbon", fun.ymin = function(x) quantile(x, 0.025), fun.ymax = function(x) quantile(x, 0.975), alpha=0.2) +
  stat_summary(data= qq, aes(factor(season), pmax(value,0.1), group=interaction(var, gear), colour=interaction(var, gear),linetype= interaction(var, gear)), geom="line", size= 0.25, fun.y=mean)+
  stat_summary(data= qq, aes(factor(season), pmax(value,0.1), group=interaction(var, gear), shape=interaction(var, gear), colour= interaction(var, gear)), geom="point", size= 1, , fun.y=mean)+
  scale_colour_manual(name= "", labels = c("OTB discards < MCRS","OTB discards > MCRS","PTB discards < MCRS","PTB discards > MCRS"),values = c("aquamarine4", "springgreen4", "gray8","black"), guide='legend')+
  scale_fill_manual(name= "", labels = c("OTB discards < MCRS","OTB discards > MCRS","PTB discards < MCRS","PTB discards > MCRS"),values = c("aquamarine4", "springgreen4", "gray8","black"), guide='legend')+
  scale_linetype_manual(name= "", labels =c("OTB discards < MCRS","OTB discards > MCRS","PTB discards < MCRS","PTB discards > MCRS"),values=c("dashed","solid","dashed","solid"), guide='legend')+
  scale_shape_manual(name= "", labels =c("OTB discards < MCRS","OTB discards > MCRS","PTB discards < MCRS","PTB discards > MCRS"),values = c(19, 17, 1, 2), guide='legend')+
  geom_rect(data = facetcond,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = 0.1) +
  theme_bw()+ scale_y_log10(breaks=c(0.01,0.1,1,10,100))+
  guides(fill=guide_legend(ncol=2)) +
  theme(panel.background = element_blank(),legend.title=element_blank(),
        legend.position="bottom",axis.text=element_text(size=8),
        #legend.position=c(0.9,0.14),axis.text=element_text(size=8),
        strip.text = element_text(size = 8), text = element_text(size=7))+
  xlab("season") +
  ylab("DPUE (t/season)") 

dev.off()

#==============================================================================
# ECONOMICS
#==============================================================================
#random<- sample(1:200,15) # 100  52  98  24 101 129 197 200  58 146 174  87 190  49  64

random<- c(100, 52, 98, 24, 101 ,129 ,197, 200,  58, 146, 174,  87, 190,  49,  64)
scenario<- c("Scenario I", "Scenario II", "Scenario III", "Scenario IV")
gross_annual<- list()
gross_gear  <- list()
net_gear    <- fuelcostss <- list()

#grossrev at season level by a unique category
grossrev <- function(dynstate){
  res <-   apply(sweep(spp1Landings(sim(dynstate)),c(1,3), dynstate@spp1Price, "*") +
                 sweep(spp2Landings(sim(dynstate)),c(1,3), dynstate@spp2Price, "*") + 
                 sweep(spp3Landings(sim(dynstate)),c(1,3), dynstate@spp3Price, "*") +
                 sweep(spp4Landings(sim(dynstate)),c(1,3), dynstate@spp4Price, "*") + 
                 sweep(spp5Landings(sim(dynstate)),c(1,3), dynstate@spp5Price, "*"), c(2,3), sum) 
  
  return(res)
}

fuelcost<- function (dynstate){
  res <- apply(dynstate@sim@effort* dynstate@control@fuelUse* dynstate@control@fuelPrice,c(2,3),sum)
  return(res)
}

gearcost<- function (dynstate){
  res <- apply(dynstate@sim@effort* dynstate@control@gearMaintenance,c(2,3),sum)
  return(res)
}

for(i in 1:4){
  #DynState function
  annual<- as.data.frame(as.matrix(grossRev(res[[i]])))
  names(annual)<-c(scenario[i])
  gross_annual[[i]] <- annual
  
  #The grossrevenue function above by season and an unique category
  gr_gear          <- data.frame(cbind(melt(round(grossrev(res[[i]])/1000), id=c("simultation")),as.matrix(res[[i]]@sim@choice)))
  names(gr_gear)   <- c("simulation", "season", "rev", "choice")
  gr_gear$rev      <- as.numeric(as.character(gr_gear$rev))
  gr_gear$choice   <- as.character(gr_gear$choice)
  gr_gear$gear     <- substr(gr_gear$choice, nchar(gr_gear$choice)-3+1, nchar(gr_gear$choice))
  gr_gear          <- gr_gear[,-4] #delete column choice, not needed
  gr_gear          <- dcast (gr_gear, simulation ~ gear, value.var= "rev", fun.aggregate=sum)
  gr_gear$scenario <- c(scenario[i])
  gr_gear          <- with(gr_gear,data.frame(simulation, OTB, PTB, scenario))
  gross_gear[[i]]  <- gr_gear
  # Costs
  #-----------------------------------------------------------------------------
  #crewshare is o.35 gross revenue
  crewshare <- gr_gear
  crewshare [,2:3] <- crewshare [,2:3] *0.35
  
  # Fuelcost
  fuelcosts<- data.frame(cbind(melt(round(fuelcost(res[[i]])/1000)),as.matrix(res[[i]]@sim@choice)))
  names(fuelcosts)   <- c("simulation", "season", "fuel", "choice")
  fuelcosts$fuel    <- as.numeric(as.character(fuelcosts$fuel))
  fuelcosts$choice   <- as.character(fuelcosts$choice)
  fuelcosts$gear     <- substr(fuelcosts$choice, nchar(fuelcosts$choice)-3+1, nchar(fuelcosts$choice))
  fuelcosts          <- fuelcosts[,-4] #delete column choice, not needed
  fuelcosts          <- dcast (fuelcosts, simulation ~ gear, value.var= "fuel", fun.aggregate=sum)
  fuelcosts$scenario <- c(scenario[i])
  fuelcosts          <- with(fuelcosts,data.frame(simulation, OTB, PTB, scenario))
  fuelcostss[[i]]    <- fuelcosts
  
  # Gearcost
  gearcosts<- data.frame(cbind(melt(round(gearcost(res[[i]])/1000)),as.matrix(res[[i]]@sim@choice)))
  names(gearcosts)   <- c("simulation", "season", "gearc", "choice")
  gearcosts$gearc    <- as.numeric(as.character(gearcosts$gearc))
  gearcosts$choice   <- as.character(gearcosts$choice)
  gearcosts$gear     <- substr(gearcosts$choice, nchar(gearcosts$choice)-3+1, nchar(gearcosts$choice))
  gearcosts          <- gearcosts[,-4] #delete column choice, not needed
  gearcosts          <- dcast (gearcosts, simulation ~ gear, value.var= "gearc", fun.aggregate=sum)
  gearcosts$scenario <- c(scenario[i])
  gearcosts          <- with(gearcosts,data.frame(simulation, OTB, PTB, scenario))

  #Net revenue
  netrev             <- gr_gear
  netrev$OTB         <- gr_gear$OTB - crewshare$OTB - fuelcosts$OTB - gearcosts$OTB
  netrev$PTB         <- gr_gear$PTB - crewshare$PTB - fuelcosts$PTB - gearcosts$PTB
  net_gear[[i]]      <- netrev
 
}

# 200 vessels
gross_revenue_allvessels  <- do.call(cbind, gross_annual)
gross_revenue_bygear      <- do.call(rbind, gross_gear)
net_revenue_bygear        <- do.call(rbind, net_gear)
fuel_costs_bygear         <- do.call(rbind, fuelcostss)

# Vessel mean gross revenue
gross_rev <- data.frame(total=round(colSums(gross_revenue_allvessels[random,]/1000)),
                       mean=round(colMeans(gross_revenue_allvessels/1000)), 
                       st= round(apply(gross_revenue_allvessels/1000, 2, sd)))

# Mean annual gross revenue by each gear
gross_rev_gear <- data.frame(mean=aggregate(. ~ scenario, gross_revenue_bygear[-1], mean), 
                         st= aggregate(. ~ scenario, gross_revenue_bygear[-1], sd))[,-4]
names(gross_rev_gear)[1]<- "scenario"
gross_rev_gear[,2:5] <- round(gross_rev_gear[,2:5])

# Mean annual net revenue by each gear
net_rev_gear <- data.frame(mean=aggregate(. ~ scenario, net_revenue_bygear[-1], mean), 
                             st= aggregate(. ~ scenario, net_revenue_bygear[-1], sd))[,-4]
names(net_rev_gear)[1]<- "scenario"
net_rev_gear[,2:5] <- round(net_rev_gear[,2:5])

# Fuel costs by each gear
fuel_costs_gear <- data.frame(mean=aggregate(. ~ scenario, fuel_costs_bygear[-1], mean), 
                           st= aggregate(. ~ scenario,fuel_costs_bygear[-1], sd))[,-4]
names(fuel_costs_gear)[1]<- "scenario"
fuel_costs_gear[,2:5] <- round(fuel_costs_gear[,2:5])
 
# List with all data that I need from the different scenarios
rev<- list()

for(i in 1:4){ 
  netrev<- as.matrix(netRev(res[[i]]))  
  netrev<-as.data.frame(netrev)
  names(netrev)<-c(scenario[[i]])
  rev[[i]]<- netrev
}
# 200 vessels
net_revenue<- cbind(rev[[1]],rev[[2]],rev[[3]],rev[[4]])
# Total from 15 random vessels
net_rev<- data.frame(total=colSums(net_revenue[random,]/1000),
                       mean=colMeans(net_revenue/1000), 
                       st=colwise(sd)(net_revenue/1000))

#-----------------------------------------------------------------------------
# Net revenue for the different gears
#-----------------------------------------------------------------------------

# Costs
#-----------------------------------------------------------------------------
#crewshare is o.35 gross revenue
fuelcost<- function (dynstate){
  res <- apply(dynstate@sim@effort* dynstate@control@fuelUse* dynstate@control@fuelPrice,2,sum)
  return(res)
}
lapply(lapply(lapply(res, fuelcost), `[`, random), function(x) sum(x)/1000)
gear<- function (dynstate){
  res <- apply(dynstate@sim@effort* dynstate@control@gearMaintenance,2,sum)
  return(res)
}
lapply(lapply(lapply(res, gear), `[`, random), function(x) sum(x)/1000)













# 
# 
# # CATCHES; mean from all simulations + include the 2012 data (maybe delete it after)
# #--------------------------------------------------------------------------
# cpue_results <- vector(mode="list", length=5)
# 
# scenario<- c("average", "Scenario I", "Scenario II", "Scenario III", "Scenario IV")
# names(cpue_results)<- scenario
# 
# for (i in 1:5){ 
# 
#          cpue_res         <- catch_results[[i]]
# 
#          if (i==1){
#                   #cpue_res$effort <- rep(melt(effort_days[,as.character(2012)]%/%number_vessels[,as.character(2012)])$value,each=2)
#                   cpue_res$effort <- rep(melt(yearMeans(effort_days)%/%yearMeans(number_vessels))$value,each=2)
#                   cpue_res$effort[is.na(cpue_res$effort)] <- 0
#          }
# 
#          cpue_res         <- dcast(cpue_res, scenario+season+spp+vessel+area+gear+variable+effort~ var)
#          cpue_res[c(8,14)]<- lapply(cpue_res[c(8,14)], function(x) as.numeric(as.character(x)))
#          cpue_res[c(9:14)]<- cpue_res[c(9:14)] / cpue_res[[8]]
#          cpue_res         <- melt(cpue_res, id= c("scenario","season","spp","vessel","area","gear","variable","effort"))
#          names(cpue_res)[9] <- "var"
#          cpue_res$value[is.na(cpue_res$value)] <- 0
# 
#          cpue_res_mean    <- aggregate(as.numeric(value) ~ scenario+season+spp+area+gear+variable+var, data=cpue_res, FUN=mean, na.rm=TRUE)
#          cpue_res_min    <- aggregate(as.numeric(value) ~ scenario+season+spp+area+gear+variable+var, data=cpue_res, FUN=min, na.rm=TRUE)
#          cpue_res_max    <- aggregate(as.numeric(value) ~ scenario+season+spp+area+gear+variable+var, data=cpue_res, FUN=max, na.rm=TRUE)
#          
#          cpue_res_mean[9]  <- cpue_res_min[8]      
#          cpue_res_mean[10]  <- cpue_res_max[8]  
# 
#          names(cpue_res_mean)[8:10]  <- c("means","mins","maxs")  
#          cpue_results[[i]] <- cpue_res_mean
#          rm(cpue_res_mean, cpue_res_min, cpue_res_max, cpue_res)
# }
# 
# cpue_res_allsimul_df <- rbind (cpue_results[[1]], cpue_results[[2]], cpue_results[[3]], cpue_results[[4]], cpue_results[[5]])
# 
# xx<- cpue_res_allsimul_df[cpue_res_allsimul_df$variable=="landings",]
# 
# levels(xx$spp)   <- c(levels(xx$spp)," horse\n mackerel")
# xx$spp[xx$spp=="horse_mackerel"] <- c(' horse\n mackerel')
# xx$spp        <- factor(xx$spp , levels=c("hake","megrim","mackerel"," horse\n mackerel","others"))
# xx$scenario   <- factor(xx$scenario,levels=c("2012","Scenario I", "Scenario II", "Scenario III", "Scenario IV"))
# xx$var        <- factor(xx$var , levels=c('discards < MCRS','discards > MCRS', 'landings < MCRS','landings > MCRS', "catch < MCRS", "catch > MCRS"))
# 
# xx<- subset(xx,(var %in% c("landings < MCRS","landings > MCRS")))
# 
# facetcond <- data.frame("scenario"=c("Scenario I","Scenario II", "Scenario III","Scenario IV"),"spp"=c("mackerel","mackerel","mackerel","mackerel","hake","hake"," horse\n mackerel","hake"))
# #==============================================================================
# #  Figure 5: CPUE along scenarios
# #==============================================================================
# png(filename="~/Dropbox/BoB/DynState/2017/figures/Res_LPUE.png",
#     width=15, height=13, units="cm", res=500, pointsize=8)
#          options(scipen=8)
#          ggplot() + 
#            geom_point(data= xx, aes(x =factor(season), y =pmax(means,0.01), group=interaction(var, gear), colour= interaction(var, gear), shape=interaction(var, gear)),size=1)+
#           geom_line (data= xx, aes(x =factor(season), y =pmax(means,0.01), group=interaction(var, gear), colour= interaction(var, gear), linetype=interaction(var, gear)),size=0.25)+
#           #geom_ribbon(data=xx, aes(y= means,ymin= mins, ymax=maxs), group=interaction(var, gear), fill=interaction(var, gear), alpha = .25)+ 
#           geom_rect(data = facetcond,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = 0.2) +
#           facet_grid(spp~scenario)+ 
#           theme_bw()+
#           scale_linetype_manual(values=c('dashed','solid','dashed','solid','dashed','solid'))+
#                scale_colour_manual(values=c( "black", "black",'red','red'))+#"#999999", "#999999",
#                scale_shape_manual(values = c(0,15 , 1,16 ,2,17))+
#                scale_y_log10(breaks=c(0.01,0.1,1,10,100,1000))+
#                theme(panel.background = element_blank(),legend.title=element_blank(),
#                        legend.position="bottom",axis.text=element_text(size=8),
#                        strip.text = element_text(size = 8), text = element_text(size=7))+
#                xlab("season") +
#                ylab("LPUE (t/vessel)") 
# 
# dev.off()
# #==============================================================================
# # Size composition of landings and discards
# #==============================================================================
# 
# lpue<- list()
# dpue<- list()
# 
# # Species fro each scenario varied in order
# species<- list()
# species [[1]]<- c("mac", "hke", "lez", "hom", "others")
# species [[2]]<- c("mac", "hke", "lez", "hom", "others")
# species [[3]]<- c("mac", "hom", "lez", "hke", "others")
# species [[4]]<- c("mac", "hke", "hom", "lez", "others")
# 
# scenario<- c("Scenario I", "Scenario II", "Scenario III", "Scenario IV")
# 
# for(i in 1:4){ 
#       spp1lnd<- as.matrix(spp1LndHold(sim(res[[i]])))
#       spp2lnd<- as.matrix(spp2LndHold(sim(res[[i]])))
#       spp3lnd<- as.matrix(spp3LndHold(sim(res[[i]])))
#       spp4lnd<- as.matrix(spp4LndHold(sim(res[[i]])))
#       spp5lnd<- as.matrix(spp5LndHold(sim(res[[i]]))) 
#       landings<- as.data.frame(cbind(spp1lnd, spp2lnd, spp3lnd, spp4lnd, spp5lnd))
#       names(landings)<- species[[i]]
#       landings$cat<- rep(c('< MCRS','> MCRS'))
#       landings$season<- c(rep(1,40),rep(2,40),rep(3,40),rep(4,40))
#       landings$vessel<- rep(1:20,each=2)
#       landings<-subset(landings,(vessel %in% 1:13))# only 11 vessels by season
#       landings$area<- rep(mapa[[i]]$area,each=2)
#       landings$gear<- rep(mapa[[i]]$gear,each=2)
#       landings$effort<- rep(mapa[[i]]$effort,each=2)
#       landings$scenario<- scenario[i]
#       landings<- melt(landings, id=c("cat", "season", "vessel", "area", "gear", "scenario", "effort"))
#       names(landings)<- c("cat", "season", "vessel", "area", "gear", 
#                           "scenario", "effort", "spp", "landings")
#       lpue[[i]]<- landings
#       rm(landings)
#       
#       spp1dis<- as.matrix(spp1DisHold(sim(res[[i]])))
#       spp2dis<- as.matrix(spp2DisHold(sim(res[[i]])))
#       spp3dis<- as.matrix(spp3DisHold(sim(res[[i]])))
#       spp4dis<- as.matrix(spp4DisHold(sim(res[[i]])))
#       spp5dis<- as.matrix(spp5DisHold(sim(res[[i]]))) 
#       discards<- as.data.frame(cbind(spp1dis, spp2dis, spp3dis, spp4dis, spp5dis))
#       names(discards)<- species[[i]]
#       discards$cat<- rep(c('< MCRS','> MCRS'))
#       discards$season<- c(rep(1,40),rep(2,40),rep(3,40),rep(4,40))
#       discards$vessel<- rep(1:20,each=2)
#       discards<-subset(discards,(vessel %in% 1:11))# only 11 vessels by season
#       discards$area<- rep(mapa[[i]]$area,each=2)
#       discards$gear<- rep(mapa[[i]]$gear,each=2)
#       discards$effort<- rep(mapa[[i]]$effort,each=2)
#       discards$scenario<- scenario[i]
#       discards<- melt(discards, id=c("cat", "season", "vessel", "area", "gear", "scenario", "effort"))
#       names(discards)<- c("cat", "season", "vessel", "area", "gear", 
#                           "scenario", "effort", "spp", "discards")
#       dpue[[i]]<- discards
#       rm(discards)
# }
# 
# # I also include the 2012 data here (maybe delete it after)
# land<- rbind(lpue[[1]], lpue[[2]], lpue[[3]], lpue[[4]])
# disc<- rbind(dpue[[1]], dpue[[2]], dpue[[3]], dpue[[4]])
# 
# # Remove where is no effort
# #land<- subset(land, !(effort %in% c("0")))
# #disc<- subset(disc, !(effort %in% c("0")))
# 
# land$landings<- as.numeric(as.character(land$landings))
# disc$discards<- as.numeric(as.character(disc$discards))
# 
# idx<- which(is.na(land$area))
# idx_season<-land$season[idx]-1
# idx_vessel<-land$vessel[idx]
# idx_scenario<-land$scenario[idx]
# idx_spp<-land$spp[idx]
# 
# 
# for (i in 1:length(idx)){
#       pos<- idx[i]
#       area<-land$area[land$scenario==idx_scenario[i] &
#                       land$season==idx_season[i] &
#                       land$vessel==idx_vessel[i] &
#                       land$spp==idx_spp[i]]
#       land$area[pos]<-as.character(area[1])
#       
#       gear<-land$gear[land$scenario==idx_scenario[i] &
#                            land$season==idx_season[i] &
#                            land$vessel==idx_vessel[i] &
#                            land$spp==idx_spp[i]]
#       land$gear[pos]<-as.character(gear[1])
# }
# 
# 
# land<- aggregate(landings ~ scenario+season+gear+cat+spp, data=land, FUN=sum)
# disc<- aggregate(discards ~ scenario+season+gear+cat+spp, data=disc, FUN=sum)
# 
# land$cat<- factor(land$cat, levels=c('> MCRS','< MCRS'))
# #land$area<- factor(land$area, levels=c("VI OTB","VII OTB","VIIIabd OTB","VIIIabd PTB"))
# land$spp<- factor(land$spp, levels=c("hke","lez","mac","hom","others"))
# land$season<- factor(land$season, levels=c("1","2","3","4"))
# disc$cat<- factor(disc$cat, levels=c('> MCRS','< MCRS'))
# #disc$area<- factor(disc$area, levels=c("VI OTB","VII OTB","VIIIabd OTB","VIIIabd PTB"))
# disc$spp<- factor(disc$spp, levels=c("hke","lez","mac","hom","others"))
# disc$season<- factor(disc$season, levels=c("1","2","3","4"))
# 
# catch<- merge(land, disc, by=c("cat", "season", "gear","scenario", "spp"))
# catch$catch<- catch$landings+catch$discards
# #-----------------------------------------------------------------------------
# png(filename="~/Documents/BoB/DynState/Paper/figures/land_disc.png",
#     width=14, height=17, units="cm", res=500, pointsize=8)
# ggplot() +  
#       geom_line(data= catch, aes(x =season, y =catch,group=cat, linetype=cat),size=0.75)+
#       geom_point(data=catch, aes(x =season, y =catch,group=cat, shape=cat),size=2)+
#       #geom_line(data= disc, aes(x =season, y =discards,group=cat, linetype=cat), colour="blue",size=0.75)+
#       #geom_point(data= disc, aes(x =season, y =discards,group=cat, shape=cat), colour="blue",size=2)+
#       facet_grid(scenario~area+gear+spp, scales = "free_y")+
#       theme_bw()+
#       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#             panel.background = element_blank(),legend.title=element_blank(),
#             legend.position=c(0.2,0.15),axis.text=element_text(size=8),
#             strip.text = element_text(size = 8),
#             text = element_text(size=7))+
#         xlab("season") +
#       ylab("CPUE (t/season)") 
# 
# dev.off()
# #-----------------------------------------------------------------------------
# 
# # Aggregate effort by scenario, season and area
# data<- aggregate(lpue ~ scenario+season+area+cat+spp+gear, data=dat, FUN=sum)
# 
# # Here also with gear type
# data2<- dcast(dat,scenario+season+area+cat+spp ~gear, value.var= "lpue", fun.aggregate=sum)
# 
# #-----------------------------------------------------------------------------
# ggplot(data = data,aes(season, lpue,group=spp)) +  geom_line(aes(colour = spp))+
#       facet_grid(area~scenario+gear)+ #geom_smooth(method = "loess", size = 0.5)+
#       labs(x = "season", y = "LPUE t season-1") +
#       scale_colour_discrete(name="Data",breaks=c("1", "2"),labels=c("Report", "Computed"))
# #-----------------------------------------------------------------------------
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #-----------------------------------------------------------------------------
# # Map; need to change the code if do not want to do it at gear level data$gear<- "OTB" 
# #-----------------------------------------------------------------------------------------------
# #data(bob_maps) #Just if want to use gg_map
# # otb <-my_ggmap(data=data, var="effort", gear_option="OTB", fgrid=(season~scenario),nbreaks=4)
# # ptb <-my_ggmap(data=data, var="effort", gear_option="PTB", fgrid=(season~scenario),nbreaks=4)
# # 
# # mylegend<-g_legend(otb)
# # grid.arrange(arrangeGrob(otb + theme(legend.position="none"),
# #                          ptb + theme(legend.position="none"),
# #                          ncol=1),mylegend, ncol=2,widths=c(10,2))
# 
# #my_ggmap(data=data, var="effort", gear_option="OTB", fgrid=(season~scenario),nbreaks=5)
# #----------------------------------------------------------------------------------------------
# #data$area<- factor(data$area, levels=c("VI OTB","VII OTB","VIIIabd OTB","VIIIabd PTB"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # # Catch data from 2012, in order to check the results from the different scenarios
# # #load("~/Documents/BoB/DynState/data/FLFish_FLBio.RData")
# # load("~/Documents/BoB/BoB_data/data/FINAL_out/others_finAL.RData")
# # load("~/Documents/BoB/BoB_data/data/FINAL_out/FLFishery_Basque_BoB.RData")
# # species<- names(flf)
# # 
# # # Ang[[5]] and lez[[4]] in area VI equal to zero
# # 
# # landings.wt(flf[[4]][,,,,"VI"])[] <- 0
# # discards.wt(flf[[4]][,,,,"VI"])[] <- 0
# # landings.wt(flf[[5]][,,,,"VI"])[] <- 0
# # discards.wt(flf[[5]][,,,,"VI"])[] <- 0
# # 
# # 
# # dimnames<-list(len=c('< MCRS','> MCRS'), year=c(2003:2012),unit=c("OTB","PTB"),
# #                season=as.character(1:4),area=c("VI","VII","VIIIabd"))
# # 
# # CPUE<-FLQuants(mac=FLQuant(, dimnames=dimnames), 
# #                hom=FLQuant(, dimnames=dimnames), 
# #                hke=FLQuant(, dimnames=dimnames),
# #                lez=FLQuant(, dimnames=dimnames), 
# #                anf=FLQuant(, dimnames=dimnames),
# #                others=FLQuant(, dimnames=dimnames)) 
# # 
# # for(i in 1:length(species)){
# #       CPUE1<- landings.wt(flf[[i]])
# #       catch.wt <- landings.wt(flf[[i]])+ discards.wt(flf[[i]])
# #       catch.wt[catch.wt==0]<- NA
# #       catch.wt<- catch.wt/1000
# #       units(catch.wt)<-"t"
# #       
# #       CPUE1 [,,"OTB"] <- catch.wt [,,"OTB"]   %/% effort_days[,as.character(2003:2012),"OTB"]      
# #       CPUE1 [,,"PTB"] <- catch.wt [,,"PTB"]   %/%(effort_days[,as.character(2003:2012),"PTB"]*2)
# #       
# #       CPUE1 [CPUE1 ==NaN]<- NA
# #       CPUE1 [CPUE1 ==Inf]<- NA
# #       CPUE1 [CPUE1 ==0]  <- NA
# #       units(CPUE1)<-"t/day"
# #       
# #       CPUE [[i]] <-CPUE1  
# #       #CPUE [[i]] <- catch.wt %/% (flf@effort[,as.character(y)] * number_vessels[,as.character(y)])
# #       
# # }
# # 
# # # CPUE <-lapply(CPUE,function (x) x/1000)
# # 
# # # Others is already in tonnes and in the proper format. < MCRS is the discard fraction,
# # # while > MCRS is the landing fraction for the group of species selected
# # 
# # CPUE [[6]] <- others %/% number_vessels[,as.character(y)]
# # CPUE[[6]] [,,"PTB"] <- CPUE[[6]][,,"PTB",] /2
# # 
# # CPUE <-   lapply(CPUE,function (x) {
# #       units(x) <- "t/vessel"
# #       return (x)
# # })
# 
# #combine all
# #-----------
# # catchmean_model<- as.data.frame(cbind(hake=as.matrix(catchMean(hke)),
# #                            megrim=as.matrix(catchMean(lez)),
# #                            mackerel=as.matrix(catchMean(mac)),
# #                            horse_mackerel=as.matrix(catchMean(hom)),
# #                            others=as.matrix(catchMean(others))))
# # names(catchmean_model)<- c("hake","megrim","mackerel"," horse\n mackerel","others")
# # catchmean_model$cat<- rep(c('< MCRS','> MCRS'))
# # catchmean_model$season<- c(rep(c(rep(1,2),rep(2,2),rep(3,2),rep(4,2)),4))
# # catchmean_model$area<- c(rep(c(rep("VI OTB",8),rep("VII OTB",8),rep("VIIIabd OTB",8),rep("VIIIabd PTB",8))))
# # catchmean_model<- melt(catchmean_model, id=c("cat", "season", "area"))
# # names(catchmean_model)<- c("cat", "season","area", "spp","catch_by_vessel")
# # catchmean_model$catch_by_vessel<- as.numeric(as.character(catchmean_model$catch_by_vessel))
# # 
# # catchmean_model$effort<- c(rep(c(rep(53.5,2),rep(62,2),rep(28,2),rep(39,2),
# #             rep(38,2),rep(47,2),rep(28,2),rep(28,2),
# #             rep(64,2),rep(54,2),rep(22,2),rep(58,2),
# #             rep(112,2),rep(114,2),rep(82,2),rep(120,2)),5))
# # 
# # catchmean_model$cpue   <- (catchmean_model$catch_by_vessel/catchmean_model$effort)*1000
# # catchmean_model$cat    <- factor(catchmean_model$cat, levels=c('> MCRS','< MCRS'))
# # catchmean_model$area   <- factor(catchmean_model$area, 
# #                                  levels=c("VI OTB","VII OTB","VIIIabd OTB","VIIIabd PTB"))
# # catchmean_model$spp    <- factor(catchmean_model$spp, 
# #                                  levels=c("hake","megrim","mackerel"," horse\n mackerel","others"))
