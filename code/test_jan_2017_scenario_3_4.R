library(RDynState5NAI)
load("5spp_dynstate_Inew.RData")

dim <- c(4,4); dimnames <- list(option=c("VIOTB","VIIOTB", "VIIIabdOTB", "VIIIabdPTB"),season=as.character(1:4))
effort  <- array(c(59,38,64,62,65,47,54,61,26,28,22,40,39,28,58,62), dim=dim, dimnames=dimnames)

#-------------------------------------------------------------------------------
# Scenario 3: L.O. with international quota swaps 
#-------------------------------------------------------------------------------
dim <- c(2,4); dimnames <- list(cat=1:2,season=as.character(1:4))

macPrice    <- array(c(200,  709, 200, 1898, 200,  700, 200,  669),dim=dim,dimnames=dimnames)
hkePrice    <- array(c(200, 2525, 200, 2847, 200, 1879, 200, 2347),dim=dim,dimnames=dimnames)
lezPrice    <- array(c(200, 5312, 200, 4564, 200, 4898, 200, 5055),dim=dim,dimnames=dimnames)
homPrice    <- array(c(200,  742, 200, 1014, 200, 1274, 200, 1669),dim=dim,dimnames=dimnames)
othersPrice <- array(c(  0, 3001,   0, 2279,   0, 2807,   0, 3297),dim=dim,dimnames=dimnames)

#-- Trick for reducing increment size of mackerel
#- Catch > MCRS in seasons 1 and 4 in VIIIOTB are really high 
# >MCRS 144.873760 25.833943 7.3252156 51.5203687
mac@catchMean [2,c(1,4), "VIIIabdOTB"] <-25
mac@catchSigma [2,c(1,4), "VIIIabdOTB"] <-1

control <- DynState.control(spp1LndQuota= 6, spp2LndQuota= 104, #tn
                            spp1DisQuota= 6, spp2DisQuota= 8, # tn
                            spp1LndQuotaFine= 1E9, spp2LndQuotaFine=1E9, # tn
                            spp1DisQuotaFine= 1E12, spp2DisQuotaFine=1E12, # tn
                            fuelUse =1, fuelPrice = 1240, landingCosts= 121,  gearMaintenance= 271,
                            addNoFishing=TRUE, increments=40, #be careful, with 38 increments is not working
                            spp1DiscardSteps=7, spp2DiscardSteps=7,
                            interpolationdistance=12, simNumber=200, numThreads=30)
                                                   
z <- DynState(mac, hom, lez, hke, others, macPrice, homPrice, lezPrice, hkePrice, othersPrice, effort, control)

save(z, file = "scenario3.RData")

#-------------------------------------------------------------------------------
# Scenario 4: use of inter-species quota flexibility 
#-------------------------------------------------------------------------------

# Another trick, because we have increased the mac quota due to inter-sp quota swaps
# we need to increase the catchmean of 25 to at least 38 (spp1LndQuota + spp1Disquota)
mac@catchMean [2,c(1,4), "VIIIabdOTB"] <-39

control <- DynState.control(spp1LndQuota= 32, spp2LndQuota= 346, #tn
                            spp1DisQuota= 6,  spp2DisQuota= 26, # tn
                            spp1LndQuotaFine= 1E9, spp2LndQuotaFine=1E9, # tn
                            spp1DisQuotaFine= 1E12, spp2DisQuotaFine=1E12, # tn
                            fuelUse = 1, fuelPrice = 1240, landingCosts= 121, gearMaintenance= 271,
                            addNoFishing=TRUE, increments=40,
                            spp1DiscardSteps=7, spp2DiscardSteps=7,
                            interpolationdistance=12, simNumber=200, numThreads=30)

z <- DynState(mac, hke, hom, lez, others, macPrice, hkePrice, homPrice, lezPrice, othersPrice, effort, control)

save(z, file = "scenario4.RData")
