#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# DYNAMIC MODEL
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
setwd("~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/results")
load("~/Dropbox/BoB/DynState/2017/data/5spp_dynstate_modelvalidation.RData")
load("~/Dropbox/BoB/DynState/Imares_july/data/model_conditioning.RData")
detach("package:FLFishery", unload=TRUE)
detach("package:FLCore", unload=TRUE)

library(RDynState5NAsigmaseason6Size)
numsimu <- 1500
#Remove area VII, but add it in the results to allow the comparison 

dim <- c(3,4); dimnames <- list(option=c("VIOTB","VIIIabdOTB", "VIIIabdPTB"),season=as.character(1:4))
#2012 effort
# effort  <- array(c(13,0,59,69,
#                    71,0,48,38,
#                    12,0,14,58,
#                    12,0,66,57), dim=dim, dimnames=dimnames)
effort  <- array(c(13,59,69,
                   71,48,38,
                   12,14,58,
                   12,66,57), dim=dim, dimnames=dimnames)

dim <- c(2,4); dimnames <- list(cat=1:2,season=as.character(1:4))
macPrice    <- array(c(0,  709, 0, 1898, 0,  700, 0,  669),dim=dim,dimnames=dimnames)
hkePrice    <- array(c(0, 2525, 0, 2847, 0, 1879, 0, 2347),dim=dim,dimnames=dimnames)
lezPrice    <- array(c(0, 5312, 0, 4564, 0, 4898, 0, 5055),dim=dim,dimnames=dimnames)
homPrice    <- array(c(0,  742, 0, 1014, 0, 1274, 0, 1669),dim=dim,dimnames=dimnames)
othersPrice <- array(c(0, 3001, 0, 2279, 0, 2807, 0, 3297),dim=dim,dimnames=dimnames)

# apply(mean_price_other, c(2), mean, na.rm = TRUE)
# 1        2        3        4 
# 3001.157 2278.688 2806.889 3297.455 
# hom<- mean_price_main_spp[mean_price_main_spp$spp=="HOM",]
# hom<- array(c(hom$value), dim=c(4,10), dimnames=list(season=as.character(1:4),year=as.character(2003:2012)))
# apply(hom, c(1), mean, na.rm = TRUE)

#-------------------------------------------------------------------------------
# subsection{Scenario 1: Hake and Mackarel will be the most probably choke species, they need to swap quota
# Although this scenario allows to discards, therefore there should not be any Unconstrained quotum for discarding
#-------------------------------------------------------------------------------
#LndQuotaMAC= 73 / 11 vessels = 7
#DisQuotaMAC= sum(catchMean(mac))- 7 = 289.6552
#LndQuotaHKE= 5919/ 11 = 538 
#DISQuotaHKE= sum(catchMean(hke))- 538 = 370.955 
#

#SIGMA <- c(90000,100000, 110000, 120000, 130000, 140000,150000, 160000)

#for (ii in SIGMA){
  
 ii<- 110000 #BEST OPTION, maybe is worth to estimate the correlation between both effort data
  control <- DynState.control(spp1LndQuota= 5,  spp2LndQuota= 395,#tn
                              spp1LndQuotaFine= 1E9, spp2LndQuotaFine=1E9, # tn
                              fuelUse =1 , 
                              fuelPrice = 1240, # euros/day
                              landingCosts= 121, # euros/tn
                              gearMaintenance= 271,
                              addNoFishing=TRUE, increments=148,
                              spp1DiscardSteps=1, spp2DiscardSteps=1,
                              sigma= ii, simNumber=numsimu, numThreads=16)
  
  z <- DynState(mac, hke, lez, hom, others, macPrice, hkePrice, lezPrice, homPrice, othersPrice, effort, control)

  save(z, file = "modelvalidation.RData")

  effort_scenario            <- as.data.frame(cbind(as.matrix(z@sim@effort), as.matrix(z@sim@choice)))
  effort_scenario$vessel     <- rep(1:numsimu)
  effort_scenario$season     <- c(rep(1,numsimu),rep(2,numsimu),rep(3,numsimu),rep(4,numsimu)) 
  effort_scenario$scenario   <- "Model validation"
  effort_scenario$V2         <- as.character(effort_scenario$V2)
  effort_scenario$choice     <- as.character(effort_scenario$V2)
  effort_scenario$gear       <- substr(effort_scenario$V2, nchar(effort_scenario$V2)-3+1, nchar(effort_scenario$V2))
  effort_scenario$area       <- substr(effort_scenario$V2, nchar(effort_scenario$V2)-10+1, nchar(effort_scenario$V2)-3)
  effort_scenario            <- effort_scenario[,-2]
  names(effort_scenario)[[1]]<- "effort"
  
  effort_scenario            <- with(effort_scenario,data.frame(choice,season,effort,scenario, area, gear, vessel))
  effort_res_allsimul       <- effort_scenario 
  
  #Random effect already comes from the Dynstate, therefore I create a new column in which I give numbers from 1 to 100 (1500 simulations,
  # divided in 15 vessels (fleet dimension) give us a 100 samples. Then we estimate average of 100 samples)
  effort_res_allsimul$randomgroup <- factor(rep(1:(numsimu/15)))
  rm (effort_scenario)
  
  # Rename area where there is no effort to STAY IN PORT
  #--------------------------------------------------------------------------                                    
  levels(effort_res_allsimul$choice) <- c(levels(effort_res_allsimul$choice),"Stay in port")
  levels(effort_res_allsimul$area)   <- c(levels(effort_res_allsimul$area),"Stay in port")
  levels(effort_res_allsimul$gear)   <- c(levels(effort_res_allsimul$gear),"Stay in port")
  effort_res_allsimul[is.na(effort_res_allsimul)] <- c("Stay in port")
  effort_res_allsimul$choice         <- factor(effort_res_allsimul$choice,levels=c("Stay in port", "VIOTB","VIIOTB","VIIIabdOTB","VIIIabdPTB"))
  effort_res_allsimul$effort        <- as.numeric(as.character(effort_res_allsimul$effort))
  
  # Sum along each sample and estimate the average from 100 samples
  #--------------------------------------------------------------------------                                    
  effort_group_allsimul <- aggregate(effort ~ choice+season+scenario+area+gear+randomgroup, data=effort_res_allsimul, FUN=sum)
  effort_group_allsimul <- aggregate(effort ~ choice+season+scenario+area+gear, data=effort_group_allsimul , FUN=mean)
  
  # Effort (2012 fishing days) data from the study period
  #------------------------------------
  library(FLCore)
  library(plyr)
  ref_effort           <- as.data.frame(effort_days[,as.character(2012)])
  ref_effort           <- with(ref_effort,data.frame(unit,season,area,data))
  
  ref_effort           <- subset(ref_effort, !(is.na(data)))
  ref_effort$scenario  <- "2012" #"average"
  ref_effort$choice    <- with(ref_effort, paste0(area,unit))
  #ref_effort$vessel    <- 1
  
  names(ref_effort)    <- c("gear","season","area","effort","scenario", "choice")#, "vessel")
  ref_effort           <- with(ref_effort,data.frame(choice,season, scenario, area, gear, effort)) 
  
  # EFFORT; all simulations + include the 2012 data 
  #--------------------------------------------------------------------------
  effort_res_allsimul_df          <- rbind (ref_effort, effort_group_allsimul)
  effort_res_allsimul_df$scenario <- factor(effort_res_allsimul_df$scenario,levels=c("2012", "Model validation"))

  # Aggregate effort by scenario, season and area
  #--------------------------------------------------------------------------
  tot_effort<- as.matrix(tapply(effort_res_allsimul_df$effort, effort_res_allsimul_df$scenario, FUN=sum))
  tot_effort[2,1] <- round(tot_effort[2,1])

  effort_res_allsimul_df$scenario <- factor(effort_res_allsimul_df$scenario,levels=c("2012","Model validation", 
                                                                 paste0("2012 \n (n=", tot_effort[1,1],")"),
                                                                 paste0("Model validation \n (n=", tot_effort[2,1],")")))
  effort_res_allsimul_df$scenario[effort_res_allsimul_df$scenario=="2012"]<- paste0("2012 \n (n=", tot_effort[1,1],")")
  effort_res_allsimul_df$scenario[effort_res_allsimul_df$scenario=="Model validation"] <- paste0("Model validation \n (n=", tot_effort[2,1],")")
  effort_res_allsimul_df$scenario  <- factor(effort_res_allsimul_df$scenario,levels=c(paste0("2012 \n (n=", tot_effort[1,1],")"),
                                                                 paste0("Model validation \n (n=", tot_effort[2,1],")")))
  effort_res_allsimul_df$choice    <- factor(effort_res_allsimul_df$choice,levels=c("Stay in port", "VIOTB","VIIOTB","VIIIabdOTB","VIIIabdPTB"))

  effort_res_allsimul_df<- subset(effort_res_allsimul_df,!(choice %in% c("Stay in port", "VIIOTB")))
  
  # Calculate the percentages
  effort_res_allsimul_df <- ddply(effort_res_allsimul_df, .(season, scenario), transform, percent = effort/sum(effort) * 100)
  
  # Format the labels and calculate their positions
  effort_res_allsimul_df <- ddply(effort_res_allsimul_df, .(season, scenario), transform, pos = (cumsum(effort) - 0.5 * effort))
  effort_res_allsimul_df$label <- paste0(sprintf("%.0f", effort_res_allsimul_df$percent), "%")
  
  setwd("~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/modelvalidation") 
  png(filename=paste("effort_area_gear_", paste0("SIGMA ", ii,".png"), sep=""),
      width=15, height=6, units="cm", res=500, pointsize=8)
  
  ggplot(effort_res_allsimul_df, aes(season,effort, fill=choice)) + 
    geom_bar(stat="identity", colour= "black")+
    facet_grid(~scenario)+
    geom_text(aes(y = pos, label = label), size = 2) +
    scale_fill_manual(values= c("gray80","gray60","gray40","gray20"))+  
    scale_y_continuous(breaks=seq(0,1800,300))+
    theme_bw()+
    theme(panel.background = element_blank(),legend.title=element_blank(), 
          legend.position="right",axis.text=element_text(size=8),
          strip.text = element_text(size = 8), text = element_text(size=7))+
    xlab("season") +
    ylab("Fishing effort (days/season)")
  dev.off()



