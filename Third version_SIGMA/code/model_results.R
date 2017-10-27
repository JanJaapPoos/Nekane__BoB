###############################################################################
# Model results
# Fleet dynamics
# IJmuiden, IMARES
# 20th October 2016
###############################################################################

#==============================================================================
# libraries
#==============================================================================
library(plyr)
library(FLCore)
library(ggplotFL)
library(reshape2)
library(RColorBrewer)
library(RDynState5NAsigmaseason6Size)

setwd("~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/results")
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

load("modelvalidation.RData")
res[[5]]<- z
rm(z)

numsimu <- 1500

load("~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/data/5spp_dynstate_Inew.RData")
load("~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/data/5spp_dynstate_modelvalidation.RData")
load("~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/data/model_conditioning.RData")

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

# Catch data from 2012, in order to check the results from the different scenarios
#----------------------------------------------------------------------------------
catch_2012           <- lapply(catchmean, '[',j=10)

# replace NA with 0, just for plotting
#-------------------------------------
catch_2012           <- lapply(catch_2012, function (x) replace (x,is.nan(x) | is.infinite(x),0))
catch_2012_df        <- as.data.frame(catch_2012)
catch_2012_df$q      <- with(catch_2012_df, paste(area, unit, sep=" "))
catch_2012_df        <- subset(catch_2012_df, select=c("len","season","data","qname","q"))
names(catch_2012_df) <- c("cat", "season","data","spp","area")

levels(catch_2012_df$spp) <- c(levels(catch_2012_df$spp),' horse\n mackerel')
catch_2012_df$spp[catch_2012_df$spp=="horse_mackerel"] <- c(' horse\n mackerel')

# remove areas VI PTB and VII PTB
#----------------------------------
catch_2012_df <-subset(catch_2012_df, !(area %in% c("VI PTB","VII PTB")))

# replace discards the others for just discards withpout MCRS
#-------------------------------------------------------------
a     <- subset(catch_2012_df, spp %in% "others" & cat %in% c('< MCRS'))
a$cat <- 'discards'
b     <- subset(catch_2012_df, spp %in% "others" & cat %in% c('> MCRS'))
b$cat <- 'landings'
catch_2012_df <-subset(catch_2012_df, !(spp %in% "others" & cat %in% c('< MCRS','> MCRS')))
catch_2012_df <- rbind(catch_2012_df, a, b)
rm(a, b, catch_2012)

# Order factor as I wish for the plotUnder a discard ban, different flexibilities could be applied to the system: international swaps, inter-species quota swap and de-minimis exceptions. 
#------------------------------------
catch_2012_df$cat  <- factor(catch_2012_df$cat, levels=c('> MCRS','< MCRS', 'landings','discards'))
catch_2012_df$area <- factor(catch_2012_df$area,levels=c("VI OTB","VII OTB","VIIIabd OTB","VIIIabd PTB"))
catch_2012_df$spp  <- factor(catch_2012_df$spp, levels=c("hake","megrim","mackerel",' horse\n mackerel',"others"))

#==============================================================================
#  Figure 1: Catch/vessel*days at sea
#==============================================================================
#-----------------------------------------------------------------------------
# Differences between the mean and the 2012 data (blue 2012, black: mean)
png(filename="~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/Figures/cpue_mean_2012_logscale.png",
    width=12, height=11, units="cm", res=500, pointsize=8)
options(scipen=8)
ggplot() +  
  geom_line (data= catch_2012_df, aes(x =factor(season), y =pmax(data,0.01), group=cat, shape=cat, linetype=cat ),size=0.25, colour="blue")+
  geom_point(data= catch_2012_df, aes(x =factor(season), y =pmax(data,0.01), group=cat, shape=cat, linetype=cat) ,size=1, colour="blue")+
  geom_line (data= catch_mean_df , aes(x =factor(season), y =pmax(data,0.01), group=cat, shape=cat, linetype=cat),size=0.25, colour="black")+
  geom_point(data= catch_mean_df , aes(x =factor(season),y =pmax(data,0.01), group=cat, shape=cat, linetype=cat), size=1, colour="black")+
  facet_grid(area~spp)+
  scale_shape_manual(name= "CPUE", labels = c("> MCRS","< MCRS","landings","discards"), values=c(19, 17, 1, 2), guide='legend')+
  scale_linetype_manual(name= "CPUE", labels = c("> MCRS","< MCRS","landings","discards"), values=c('solid','dashed','solid','dashed'), guide='legend')+
  scale_y_log10(breaks=c(0.01,0.1,1,10,100,1000))+
  theme_bw()+
  theme(panel.background = element_blank(), legend.title=element_blank(),
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
detach("package:ggplotFL", unload=TRUE)

# Effort (2012 fishing days) data from the study period
#------------------------------------
ref_effort           <- as.data.frame(effort_days[,as.character(2012)])
ref_effort           <- with(ref_effort,data.frame(unit,season,area,data))
ref_effort           <- subset(ref_effort, !(is.na(data)))
ref_effort$scenario  <- "2012" #"average"
ref_effort$choice    <- with(ref_effort, paste0(area,unit))
names(ref_effort)    <- c("gear","season","area","effort","scenario", "choice")
ref_effort           <- with(ref_effort,data.frame(choice,season,effort,scenario, area, gear)) 
ref_effort $randomgroup <- 1

# List with all data that I need from the different scenarios
#------------------------------------
effort_res_allsimul  <- list()
scenario             <- c("Scenario I", "Scenario II", "Scenario III", "Scenario IV", "Model validation")

#==============================================================================
#  Figure 2: Effort, The simulations were made for 200 vessels 
#==============================================================================
detach("package:FLCore", unload=TRUE)

for(i in 1:5){ #number of scenarios, 2012 is not included here, need to add it later
  
  effort                     <- as.matrix(effort(sim(res[[i]])))
  choice                     <- as.matrix(choice(sim(res[[i]])))
  
  effort_scenario            <- as.data.frame(cbind(effort, choice))
  effort_scenario$vessel     <- rep(1:numsimu)
  effort_scenario$season     <- c(rep(1,numsimu),rep(2,numsimu),rep(3,numsimu),rep(4,numsimu)) 
  effort_scenario$scenario   <- scenario[i]
  effort_scenario$V2         <- as.character(effort_scenario$V2)
  effort_scenario$choice     <- as.character(effort_scenario$V2)
  effort_scenario$gear       <- substr(effort_scenario$V2, nchar(effort_scenario$V2)-3+1, nchar(effort_scenario$V2))
  effort_scenario$area       <- substr(effort_scenario$V2, nchar(effort_scenario$V2)-10+1, nchar(effort_scenario$V2)-3)
  effort_scenario            <- effort_scenario[,-2]
  names(effort_scenario)[[1]]<- "effort"
  
  effort_scenario            <- with(effort_scenario,data.frame(choice,season,effort,scenario, area, gear))
  #Random effect already comes from the Dynstate, therefore I create a new column in which I give numbers from 1 to 100 (1500 simulations,
  # divided in 15 vessels (fleet dimension) give us a 100 samples. Then we estimate average of 100 samples)
  effort_scenario$randomgroup <- factor(rep(1:(numsimu/15)))
  
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
                                                                                         "Scenario II", "Scenario III", "Scenario IV", "Model validation"))
effort_res_allsimul_df$effort        <- as.numeric(as.character(effort_res_allsimul_df$effort))

# Sum along each sample and estimate the average from 100 samples
#--------------------------------------------------------------------------                                    
effort_group_sum_allsimul_df <- aggregate(effort ~ choice+season+scenario+area+gear+randomgroup, data=effort_res_allsimul_df, FUN=sum)
# Repetitions try to estimate the mean number of vessels that visit that choice in a given area, gear, season...
effort_group_sum_allsimul_df$repetitions <- aggregate(cbind(count = effort) ~ choice+season+scenario+area+gear+randomgroup, data=effort_res_allsimul_df, function(x){NROW(x)})$count

effort_group_mean_allsimul_df <- aggregate(effort ~ choice+season+scenario+area+gear, data=effort_group_sum_allsimul_df , FUN=mean)
# Repetitions try to estimate the mean number of vessels that visit that choice in a given area, gear, season...
effort_group_mean_allsimul_df$repetitions <- round(aggregate(repetitions ~ choice+season+scenario+area+gear, data=effort_group_sum_allsimul_df, FUN=mean)$repetitions)


# Remove where is no effort from 15 simulations
#------------------------------------------------------------------------                                            
effort_group          <- effort_group_mean_allsimul_df
effort_group          <- subset(effort_group, !(effort %in% c("0")))
effort_group$effort   <- as.numeric(as.character(effort_group$effort))

# Here also with gear type
#--------------------------------------------------------------------------
data1 <- dcast (effort_group, choice+season ~scenario, value.var= "effort", fun.aggregate=sum)
data2 <- dcast (effort_group, choice ~scenario, value.var= "effort", fun.aggregate=sum)
data3 <- as.matrix(tapply(effort_group$effort, effort_group$scenario, FUN=sum))

# Create table with the total effort by gear, season, area and scenario
#--------------------------------------------------------------------------
write.table(data1, file="~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/data/effort_choice_season.csv", sep=";")
write.table(data2, file="~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/data/effort_choice.csv", sep=";")
write.table(data3, file="~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/data/effort_scenario.csv", sep=";")

tot_effort<- as.matrix(tapply(effort_group$effort, effort_group$scenario, FUN=sum))
tot_effort[,1] <- round(tot_effort[,1])

#JUST FOR PLOTTING
#------------------------------------------
# Remove effort from scenarios
modeffort <- subset(effort_group_mean_allsimul_df, (scenario %in% c("2012","Model validation")))

modeffort$scenario       <- factor(modeffort$scenario,levels=c("2012","Model validation", 
                                                                            paste0("2012 \n (n=", tot_effort[1,1],")"),
                                                                            paste0("Model validation \n (n=", tot_effort[6,1],")")))
modeffort$scenario[modeffort$scenario=="2012"] <- paste0("2012 \n (n=", tot_effort[1,1],")")
modeffort$scenario[modeffort$scenario=="Model validation"] <- paste0("Model validation \n (n=", tot_effort[6,1],")")
modeffort$scenario       <- factor(modeffort$scenario,levels=c(paste0("2012 \n (n=", tot_effort[1,1],")"),
                                                               paste0("Model validation \n (n=", tot_effort[6,1],")")))
modeffort <- subset(modeffort,!(choice %in% c("Stay in port")))
# Calculate the percentages
modeffort <- ddply(modeffort, .(season, scenario), transform, percent = effort/sum(effort) * 100)
# Format the labels and calculate their positions
modeffort <- ddply(modeffort, .(season, scenario), transform, pos = (cumsum(effort) - 0.5 * effort))
modeffort$label <- paste0(sprintf("%.0f", modeffort$percent), "%")

# Remove mean effort or 2012 effort and model validation
effortscen<- subset(effort_group_mean_allsimul_df, !(scenario %in% c("2012","Model validation")))
effortscen$scenario       <- factor(effortscen$scenario,levels=c("Scenario I","Scenario II", "Scenario III", "Scenario IV",
                                                                 paste0("Scenario I \n (n=", tot_effort[2,1],")"),
                                                                 paste0("Scenario II \n (n=", tot_effort[3,1],")"),
                                                                 paste0("Scenario III \n (n=", tot_effort[4,1],")"),
                                                                 paste0("Scenario IV \n (n=", tot_effort[5,1],")")))
effortscen$scenario[effortscen$scenario=="Scenario I"]  <- paste0("Scenario I \n (n=", tot_effort[2,1],")")
effortscen$scenario[effortscen$scenario=="Scenario II"] <- paste0("Scenario II \n (n=", tot_effort[3,1],")")
effortscen$scenario[effortscen$scenario=="Scenario III"]<- paste0("Scenario III \n (n=", tot_effort[4,1],")")
effortscen$scenario[effortscen$scenario=="Scenario IV"] <- paste0("Scenario IV \n (n=", tot_effort[5,1],")")
effortscen$scenario       <- factor(effortscen$scenario,levels=c(paste0("Scenario I \n (n=", tot_effort[2,1],")"),
                                                                 paste0("Scenario II \n (n=", tot_effort[3,1],")"),
                                                                 paste0("Scenario III \n (n=", tot_effort[4,1],")"),
                                                                 paste0("Scenario IV \n (n=", tot_effort[5,1],")")))

effortscen<- subset(effortscen,!(choice %in% c("Stay in port")))
# Calculate the percentages
effortscen <- ddply(effortscen, .(season, scenario), transform, percent = effort/sum(effort) * 100)
# Format the labels and calculate their positions
effortscen <- ddply(effortscen, .(season, scenario), transform, pos = (cumsum(effort) - 0.5 * effort))
effortscen$label <- paste0(sprintf("%.0f", effortscen$percent), "%")

#==============================================================================
png(filename="~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/Figures/effort_area_gear_15_SIMUL.png",
    width=15, height=7, units="cm", res=500, pointsize=8)
ggplot(effortscen, aes(season,effort, fill=choice)) + #effort_res_15simul_df
  geom_bar(stat="identity", colour= "black")+
  facet_grid(~scenario)+
  geom_text(aes(y = pos, label = label), size = 2) +
  scale_fill_manual(values= c("gray80","gray60","gray40","gray20", "gray10"))+  
  scale_y_continuous(breaks=seq(0,1800,300))+
  theme_bw()+
  theme(panel.background = element_blank(),legend.title=element_blank(), 
        legend.position="right",axis.text=element_text(size=8),
        strip.text = element_text(size = 8), text = element_text(size=7))+
  xlab("season") +
  ylab("Fishing effort (days/season)")
dev.off()

png(filename="~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/Figures/effort_area_gear_modelvalidation.png",
    width=15, height=7, units="cm", res=500, pointsize=8)
ggplot(modeffort, aes(season,effort, fill=choice)) + 
  geom_bar(stat="identity", colour= "black")+
  facet_grid(~scenario)+
  geom_text(aes(y = pos, label = label), size = 2) +
  scale_fill_manual(values= c("gray80","gray60","gray40","gray20", "gray10"))+  
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
all_effort <- subset(all_effort, !(scenario %in% c("2012", "Model validation")))

# Calculate the percentages
all_effort <- ddply(all_effort, .(season, scenario), transform, percent = effort/sum(effort) * 100)
# Format the labels and calculate their positions
all_effort <- ddply(all_effort, .(season, scenario), transform, pos = (cumsum(effort) - 0.5 * effort))
all_effort$lab <- paste0(sprintf("%.0f", all_effort$percent), "%")
#==============================================================================

library(scales)

png(filename="~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/Figures/choice_area_200simulations.png",
    width=15, height=5, units="cm", res=500, pointsize=8)

ggplot(all_effort, aes(x=as.factor(season), y=trip, fill=choice)) + 
  geom_bar(stat="identity", position = "fill", colour="black")+
  facet_grid(.~scenario)+
  scale_y_continuous( labels = percent)+
  scale_fill_manual(values= c("gray100","gray80","gray60","gray40","gray20"))+
  #geom_text(aes(y = pos, label = lab), size = 2) +
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
catch_results <- vector(mode="list", length=6)

scenario<- c("2012", "Scenario I", "Scenario II", "Scenario III", "Scenario IV", "Model validation")
names(catch_results)<- scenario

# Species from each scenario varied in order
species       <- list()
species [[1]] <- c("mac", "hom", "hke", "lez", "others")
species [[2]] <- c("mac", "hke", "lez", "hom", "others")
species [[3]] <- c("mac", "hke", "lez", "hom", "others")
species [[4]] <- c("mac", "hom", "lez", "hke", "others")
species [[5]] <- c("mac", "hke", "hom", "lez", "others") 
species [[6]] <- c("mac", "hke", "lez", "hom", "others") 

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

for (spp in 1:5){  # allocate landings and discards for 2012 year
  if (spp<5){ 
    ref_discards[[spp]][1,] <- ref_discards[[spp]][1,]+ref_landings[[spp]][1,]
    ref_landings[[spp]][1,] <- 0
  } else{ #others, reconvert undersized landings to sized discards
    ref_discards[[spp]][2,] <- ref_landings[[spp]][1,]
    ref_discards[[spp]][1,] <- ref_landings[[spp]][1,] <- 0
  }
}

for(i in 1:6){ #scenarios
  
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
 
    }else {
    # In scenario I, discarding is allowed, therefore for the non conditioning species
    # need to explicit allow to discard the undersized landings
    if (i==2 | i==6){
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
                                                                                      "Scenario II", "Scenario III", "Scenario IV", "Model validation"))
catch_res_allsimul_df$var    <- factor(catch_res_allsimul_df$var , levels=c('discards < MCRS','discards > MCRS', 'landings < MCRS','landings > MCRS', "catch < MCRS", "catch > MCRS"))

write.table(catch_res_allsimul_df, file="~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/data/Results_catch_200simul.csv", sep=";")

# CPUE; mean catch by vessel with uncertainty (mean and quantiles)
#--------------------------------------------------------------------------
catch_res            <- catch_res_allsimul_df
catch_res$spp        <- factor(catch_res$spp , levels=c("hake","megrim","mackerel"," horse\n mackerel","others"))
catch_res$scenario   <- factor(catch_res$scenario,levels=c("2012","Scenario I", "Scenario II", "Scenario III", "Scenario IV", "Model validation"))
catch_res$var        <- factor(catch_res$var , levels=c('discards < MCRS','discards > MCRS', 'landings < MCRS','landings > MCRS', "catch < MCRS", "catch > MCRS"))

# Change betweemn "catch", "landings" and "discards"
#--------------------------------------------------------------------------
xx<- catch_res[catch_res$variable=="catch",]
xx <- subset(xx, !(scenario %in% c("2012", "Model validation")))

qqq<- aggregate(. ~ scenario+season+spp+var+gear+vessel, data=xx, FUN=sum)

df <- expand.grid( scenario=c("Scenario I", "Scenario II", "Scenario III", "Scenario IV"),season= c("1","2","3","4"), spp= c("hake","megrim","mackerel"," horse\n mackerel","others"), var=c("catch < MCRS", "catch > MCRS"), gear=c("OTB", "PTB"), vessel=c(1:200))

qq<- merge(df,qqq, all.x=T)
qq$value[is.na(qq$value)]<- 0

facetcond <- data.frame("scenario"=c("Scenario I","Scenario II", "Scenario III","Scenario IV"),"spp"=c("mackerel","mackerel","mackerel","mackerel","hake","hake"," horse\n mackerel","hake"))

yy<- catch_res[catch_res$variable=="catch",]
yy <- subset(yy, (scenario %in% c("2012", "Model validation")))

mmm<- aggregate(. ~ scenario+season+spp+var+gear+vessel, data=yy, FUN=sum)

dfy <- expand.grid( scenario=c("2012", "Model validation"),season= c("1","2","3","4"), spp= c("hake","megrim","mackerel"," horse\n mackerel","others"), var=c("catch < MCRS", "catch > MCRS"), gear=c("OTB", "PTB"), vessel=c(1:200))

qqq<- merge(dfy,mmm, all.x=T)
qqq<- subset(qqq, !(scenario %in% "2012" & !(vessel %in% "1")))
qqq$value[is.na(qqq$value)]<- 0

#==============================================================================
#  Figure 4: CPUE composition along scenarios
#==============================================================================
png(filename="~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/Figures/Res_catch_average_scenarios.png",
    width=15, height=13, units="cm", res=500, pointsize=8)

options(scipen=8)

ggplot()+ 
  facet_grid(spp~scenario)+ 
  stat_summary(data= qqq, aes(factor(season), pmax(value,0.1), group=interaction(var, gear), fill= interaction(var, gear)), geom="ribbon", fun.ymin = function(x) quantile(x, 0.025), fun.ymax = function(x) quantile(x, 0.975), alpha=0.2) +
  stat_summary(data= qqq, aes(factor(season), pmax(value,0.1), group=interaction(var, gear), colour=interaction(var, gear),linetype= interaction(var, gear)), geom="line", size= 0.25, fun.y=mean)+
  stat_summary(data= qqq, aes(factor(season), pmax(value,0.1), group=interaction(var, gear), shape=interaction(var, gear), colour= interaction(var, gear)), geom="point", size= 1, , fun.y=mean)+
  scale_colour_manual(name= "", labels = c("OTB catch < MCRS","OTB catch > MCRS","PTB catch < MCRS","PTB catch > MCRS"),values = c("steelblue4", "navyblue", "gray8","black"), guide='legend')+
  scale_fill_manual(name= "", labels = c("OTB catch < MCRS","OTB catch > MCRS","PTB catch < MCRS","PTB catch > MCRS"),values = c("steelblue4", "navyblue", "gray8","black"), guide='legend')+
  scale_linetype_manual(name= "", labels = c("OTB catch < MCRS","OTB catch > MCRS","PTB catch < MCRS","PTB catch > MCRS"),values=c("dashed","solid","dashed","solid"), guide='legend')+
  scale_shape_manual(name= "", labels = c("OTB catch < MCRS","OTB catch > MCRS","PTB catch < MCRS","PTB catch > MCRS"),values = c(19, 17, 1, 2), guide='legend')+
  #geom_rect(data = facetcond,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,alpha = 0.1) +
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

random<- c(100, 52, 98, 24, 101 ,129 ,197, 200,  58, 146, 174,  87, 190,  49,  64) #1 instead 58
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

#Relative to scenario I. It is by gear, then need to rerun to swicth between OTB and PTB
gross_rev_rel <- dcast(gross_revenue_bygear[,c(1,2,4)],  scenario~simulation, value.var="OTB")
gross_rev_rel[2:4,-1]<- gross_rev_rel[2:4,-1]*100/ as.list(gross_rev_rel[1,-1])
gross_rev_rel <- melt(gross_rev_rel)
gross_rev_rel <- data.frame(mean=aggregate(. ~ scenario, gross_rev_rel[-2], mean), 
                            st= aggregate(. ~ scenario, gross_rev_rel[-2], sd))

# Mean annual net revenue by each gear
net_rev_gear <- data.frame(mean=aggregate(. ~ scenario, net_revenue_bygear[-1], mean), 
                           st= aggregate(. ~ scenario, net_revenue_bygear[-1], sd))[,-4]
names(net_rev_gear)[1]<- "scenario"
net_rev_gear[,2:5] <- round(net_rev_gear[,2:5])

#Relative to scenario I. It is by gear, then need to rerun to swicth between OTB and PTB
net_rev_rel <- dcast(net_revenue_bygear[,c(1,2,4)],  scenario~simulation, value.var="OTB")
net_rev_rel[2:4,-1]<- net_rev_rel[2:4,-1]*100/ as.list(net_rev_rel[1,-1])
net_rev_rel <- melt(net_rev_rel)
net_rev_rel <- data.frame(mean=aggregate(. ~ scenario, net_rev_rel[-2], mean), 
                          st= aggregate(. ~ scenario, net_rev_rel[-2], sd))

# Fuel costs by each gear
fuel_costs_gear <- data.frame(mean=aggregate(. ~ scenario, fuel_costs_bygear[-1], mean), 
                              st= aggregate(. ~ scenario,fuel_costs_bygear[-1], sd))[,-4]
names(fuel_costs_gear)[1]<- "scenario"
fuel_costs_gear[,2:5] <- round(fuel_costs_gear[,2:5])

fuel_costs_rel <- dcast(fuel_costs_bygear[,c(1,2,4)],  scenario~simulation, value.var="OTB")
fuel_costs_rel[2:4,-1]<- fuel_costs_rel[2:4,-1]*100/ as.list(fuel_costs_rel[1,-1])
fuel_costs_rel <- melt(fuel_costs_rel)
fuel_costs_rel <- data.frame(mean=aggregate(. ~ scenario, fuel_costs_rel[-2], mean), 
                             st= aggregate(. ~ scenario, fuel_costs_rel[-2], sd))

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