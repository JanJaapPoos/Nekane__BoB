#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# DYNAMIC MODEL
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
setwd("~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/results")
load("~/Dropbox/BoB/DynState/Git/Nekane__BoB/Third version_SIGMA/data/5spp_dynstate_Inew.RData")

library(RDynState5NAsigmaseason6Size)

#-------------------------------------------------------------------------------
## Effort needed for each season in areas VI(OTB), VII(OTB), VIIIabd (OTB) and VIIIabd (PTB)
#-------------------------------------------------------------------------------
#effort_days_needed<- effort_days[,as.character(2003:2012)]%/%number_vessels[,as.character(2003:2012)]
#effort_days_needed<- apply(effort_days_needed, c(1,3:6), mean, na.rm = TRUE)
#units(effort_days_needed)<- "days/ vessel"
#effort <- array(c(54,38,64,112,62,47,54,114,28,28,22,82,39,28,58,120), dim=dim, dimnames=dimnames)
dim <- c(4,4)
dimnames <- list(option=c("VIOTB","VIIOTB", "VIIIabdOTB", "VIIIabdPTB"),season=as.character(1:4))
effort  <- array(c(59,38,65,62,
                   65,47,54,61,
                   26,28,22,40,
                   39,28,58,62), dim=dim, dimnames=dimnames)

#-------------------------------------------------------------------------------
## Prices by category and season 
#-------------------------------------------------------------------------------
dim <- c(2,4)
dimnames <- list(cat=1:2,season=as.character(1:4))

# Prices need to be in euros/ tn !!!!
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
control <- DynState.control(spp1LndQuota= 45,  spp2LndQuota= 538, #tn
                            spp1LndQuotaFine= 1E9, spp2LndQuotaFine=1E9, # tn
                            fuelUse =1 , 
                            fuelPrice = 1240, # euros/day
                            landingCosts= 121, # euros/tn
                            gearMaintenance= 271,
                            addNoFishing=TRUE, increments=28,
                            spp1DiscardSteps=7, spp2DiscardSteps=7,
                            sigma= 110000, simNumber=1500, numThreads=16)

z <- DynState(mac, hke, lez, hom, others, macPrice, hkePrice, lezPrice, homPrice, othersPrice,effort, control)
save(z, file = "scenario1.RData")

#-------------------------------------------------------------------------------
# Scenario 2: Assume that the quota will be uplifted with the 75% of the estimated stock discards
# Hake and Mackarel will be the most probably choke species, they need to swap quota
# The international swaps are unknown under the L.O, therefore we do not consider any swap
# Under the L.O, only the 5% of the uplift quota could be discarded under de-minimis exception (constrained quotum for discarding)
# Discarding fine has to be higher than landing fine, then we ensure that discarding would not continue.
#-------------------------------------------------------------------------------
#units(effort_days_needed)<- "days/ vessel"

#-------------------------------------------------------------------------------
## Prices by category and season 
#-------------------------------------------------------------------------------
dim <- c(2,4)
dimnames <- list(cat=1:2,season=as.character(1:4))
# Prices need to be in euros/ tn !!!!
macPrice    <- array(c(200,  709, 200, 1898, 200,  700, 200,  669),dim=dim,dimnames=dimnames)
hkePrice    <- array(c(200, 2525, 200, 2847, 200, 1879, 200, 2347),dim=dim,dimnames=dimnames)
lezPrice    <- array(c(200, 5312, 200, 4564, 200, 4898, 200, 5055),dim=dim,dimnames=dimnames)
homPrice    <- array(c(200,  742, 200, 1014, 200, 1274, 200, 1669),dim=dim,dimnames=dimnames)
othersPrice <- array(c(  0, 3001,   0, 2279,   0, 2807,   0, 3297),dim=dim,dimnames=dimnames)

#LndQuotaMAC= 5 / 11 vessels = 0.45
#DisQuotaMAC= 0.45 * 0.05 = 0.0225
#LndQuotaHKE= 5583/ 11 = 508 
#DISQuotaHKE= 508* 0.05 = 25

control <- DynState.control(spp1LndQuota= 45,  spp2LndQuota= 508, #tn
                            spp1LndQuotaFine= 1E9, spp2LndQuotaFine=1E9, # tn
                            fuelUse =1 , 
                            fuelPrice = 1240, # euros/day
                            landingCosts= 121, # euros/tn
                            gearMaintenance= 271,
                            addNoFishing=TRUE, increments=28,
                            spp1DiscardSteps=0, spp2DiscardSteps=0,
                            sigma= 110000, simNumber=1500, numThreads=16)

z <- DynState(mac, hke, lez, hom, others, macPrice, hkePrice, lezPrice, homPrice, othersPrice, effort, control)
save(z, file = "scenario2.RData")

#-------------------------------------------------------------------------------
# Scenario 3: L.O. with international quota swaps 
#-------------------------------------------------------------------------------

#-- Trick for reducing increment size of mackerel
#- Catch > MCRS in seasons 1 and 4 in VIIIOTB are really high 
# >MCRS 144.873760 25.833943 7.3252156 51.5203687
mac@catchMean [2,c(1,4), "VIIIabdOTB"] <-25
mac@catchSigma [2,c(1,4), "VIIIabdOTB"] <-1

control <- DynState.control(spp1LndQuota= 6,  spp2LndQuota= 104, #tn
                            spp1LndQuotaFine= 1E9, spp2LndQuotaFine=1E9, # tn
                            fuelUse =1 , 
                            fuelPrice = 1240, # euros/day
                            landingCosts= 121, # euros/tn
                            gearMaintenance= 271,
                            addNoFishing=TRUE, increments=28,
                            spp1DiscardSteps=0, spp2DiscardSteps=0,
                            sigma= 110000, simNumber=1500, numThreads=16)

z <- DynState(mac, hom, lez, hke, others, macPrice, homPrice, lezPrice, hkePrice, othersPrice, effort, control)
save(z, file = "scenario3.RData")

#-------------------------------------------------------------------------------
# Scenario 4: use of inter-species quota flexibility 
#-------------------------------------------------------------------------------

# Another trick, because we have increased the mac quota due to inter-sp quota swaps
# we need to increase the catchmean of 25 to at least 38 (spp1LndQuota + spp1Disquota)
mac@catchMean [2,c(1,4), "VIIIabdOTB"] <-39

control <- DynState.control(spp1LndQuota= 32,  spp2LndQuota= 346, #tn
                            spp1LndQuotaFine= 1E9, spp2LndQuotaFine=1E9, # tn
                            fuelUse =1 , 
                            fuelPrice = 1240, # euros/day
                            landingCosts= 121, # euros/tn
                            gearMaintenance= 271,
                            addNoFishing=TRUE, increments=28,
                            spp1DiscardSteps=0, spp2DiscardSteps=0,
                            sigma= 110000, simNumber=1500, numThreads=16)

z <- DynState(mac, hke, hom, lez, others, macPrice, hkePrice, homPrice, lezPrice, othersPrice, effort, control)

save(z, file = "scenario4.RData")
