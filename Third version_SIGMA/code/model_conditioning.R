###############################################################################
# Model conditioning
# Fleet dynamics
# Sukarrieta, Azti,
# 5th July 2016
###############################################################################

#==============================================================================
# libraries
#==============================================================================
library(nekaneR)
library(plyr)
library(reshape2)
library(devtools)
library(FLCore)
library(ggplotFL)
library (FLFishery)

#==============================================================================
# Read data
#==============================================================================
y<-2003:2012

# Catch at age for the Basque fleet (FLFishery=flf), Stock data for the selected stocks (FLBiol=stk_bio),
# years, order of the species (mac, hom, hke, lez, anf),  fishmort and 
# international catches for the last assessment (lez and mon in 2005, others in 2012) 

#load("~/Documents/BoB/DynState/data/FLFish_FLBio.RData")
load("~/Documents/BoB/BoB_data/data/FINAL_out/others_finAL.RData")
load("~/Documents/BoB/BoB_data/data/FINAL_out/FLFishery_Basque_BoB.RData")
species<- names(flf)

# Ang[[5]] and lez[[4]] in area VI equal to zero
#------------------------------------------------
landings.wt(flf[[4]][,,,,"VI"])[] <- 0
discards.wt(flf[[4]][,,,,"VI"])[] <- 0
landings.wt(flf[[5]][,,,,"VI"])[] <- 0
discards.wt(flf[[5]][,,,,"VI"])[] <- 0

number_vessels[,,"PTB"] <- number_vessels[,,"PTB"] *2
effort_days[,,"PTB"]    <- effort_days[,,"PTB"] *2
effort_trips[,,"PTB"]  <- effort_trips[,,"PTB"]  *2 
#==============================================================================
# Usefull functions
#==============================================================================

# assign units
#--------------
foo <- function (x, unit){
      units(x) <- unit
      return(x)
}

# reorganizing FLQuants as I wish
foo_catchrates<- function(x){
      catch_rates[[x]][1,] <- landings[[x]][2,]
      catch_rates[[x]][2,] <- discards[[x]][2,]
      catch_rates[[x]][3,] <- discards[[x]][1,]
      return(catch_rates[[x]])
}

# all species together, need to replace the NA with 0 to sum everything for the Reduce function
#Reduce('+', lapply(lapply(lapply(catch, apply, c(2:3,5:6),sum, na.rm=TRUE),yearMeans),foo_na))
# can not use apply with sum because it replaces NA with 0
#-----------------------------------------------------------------------------------------------

foo_na<- function(x){
      x[is.na(x)] <- 0
      return(x)
}

# Funtions for aggragating data in the same manner
#---------------------------------------------------

foo_area <- function(x){
      quantSums(seasonSums(x))
}

foo_tot <- function(x){
      quantSums(seasonSums(areaSums(unitSums(x))))
}

#lapply(lapply(model[[i]], '[',i=1),seasonSums)
foo_area_quant1 <- function(x){
      seasonSums(x[1])
}

foo_tot_quant1 <- function(x){
      seasonSums(areaSums(unitSums(x[1])))
}

#==============================================================================
# FLEET summary
#==============================================================================

#FISHING DAYS
#------------------------------------------------------------------------------
fish_days_area     <-yearMeans(foo_area(effort_days[,as.character(y)]))
fish_days_sig_area <-sqrt(yearVars(foo_area(effort_days[,as.character(y)]))%/%10)

fish_days_tot      <-yearMeans(foo_tot(effort_days[,as.character(y)]))
fish_days_sig_tot  <-sqrt(yearVars(foo_tot(effort_days[,as.character(y)]))%/%10)


#NUMBER VESSELS
#------------------------------------------------------------------------------
n_vessels_area      <-yearMeans(apply(number_vessels[,as.character(y)], 
                                      c(1:3, 5:6), max, na.rm = TRUE))
n_vessels_sig_area  <-sqrt(yearVars(apply(number_vessels[,as.character(y)], 
                                          c(1:3, 5:6), max, na.rm = TRUE)))

n_vessels_tot       <-yearMeans(apply(number_vessels[,as.character(y)], 
                                      c(1:2,6),max, na.rm = TRUE))
n_vessels_sig_tot   <-sqrt(yearVars(apply(number_vessels[,as.character(y)], 
                                          c(1:2,6), max, na.rm = TRUE)))

#FISHING TRIPS
#------------------------------------------------------------------------------
fish_trips_area     <-yearMeans(foo_area(effort_trips[,as.character(y)]))
fish_trips_sig_area <-sqrt(yearVars(foo_area(effort_trips[,as.character(y)]))%/%10)

fish_trips_tot      <-yearMeans(foo_tot(effort_trips[,as.character(y)]))
fish_trips_sig_tot  <-sqrt(yearVars(foo_tot(effort_trips[,as.character(y)]))%/%10)

#DURATION FISHING AREA (Prellezo et al., 2009) Fish Res. 97 (2009): 24-31
#------------------------------------------------------------------------------
dimnames                      <-list(quant="all", year=c(2003:2012),unit=c("OTB","PTB"),
                                season=as.character(1:4),area=c("VI","VII","VIIIabd"))
duration                      <- FLQuant(c(rep(c(rep(c(2.16,NA), rep(10,2))),4),
                                           rep(c(rep(c(2.72,NA), rep(10,2))),4),
                                           rep(c(rep(c(1.13), rep(10,1))),8)),
                                         dimnames=dimnames)
duration_fish__plus_ground     <-yearMeans(foo_area(effort_trips[,as.character(y)]*duration))
duration_sig                  <- c(1.79,NA,1.39, NA, 0.39, 0.39)
duration_fish_ground_sig_area <-sqrt(yearVars(foo_area(effort_trips[,as.character(y)]))%/%10)

duration_tot                  <- c(6.82)
duration_fish_ground_tot      <-yearMeans(foo_tot(effort_trips[,as.character(y)]*duration))
duration_sig_tot              <- c(2.40)
duration_fish_ground_sig_tot  <-sqrt(yearVars(foo_tot(effort_trips[,as.character(y)]))%/%10)

#FLEET SUMMARY
#------------------------------------------------------------------------------
summary_fleet    <-  cbind (rbind(melt(fish_days_area),melt(fish_days_tot)), 
                            c(melt(fish_days_sig_area)$value,melt(fish_days_sig_tot)$value),
                            c(melt(n_vessels_area)$value,melt(n_vessels_tot)$value),
                            c(melt(n_vessels_sig_area)$value,melt(n_vessels_sig_tot)$value),
                            c(melt(fish_trips_area)$value,melt(fish_trips_tot)$value),
                            c(melt(fish_trips_sig_area)$value,melt(fish_trips_sig_tot)$value)) 

names(summary_fleet)[7:12]<- c("average_days","stdev_days",
                               "average_vessels","stdev_vessels",
                               "average_trips","stdev_trips")

summary_fleet$ Area <-with(summary_fleet, paste(area, unit, sep=" "))
summary_fleet <- subset(summary_fleet, 
                            select=c("Area", "average_days", "stdev_days",
                                     "average_vessels", "stdev_vessels",
                                     "average_trips", "stdev_trips"))
names(summary_fleet)[1] <- "area"
summary_fleet<-subset(summary_fleet, !(area %in% c("VI PTB","VII PTB"))) 


rm(fish_days_area,fish_days_tot,fish_days_sig_area,fish_days_sig_tot,
         n_vessels_area,n_vessels_tot,n_vessels_sig_area,n_vessels_sig_tot,
         fish_trips_area,fish_trips_tot,fish_trips_sig_area,fish_trips_sig_tot) 

#==============================================================================
# Manipulating data for using it in the model
#==============================================================================

dimnames<-list(len=c('< MCRS','> MCRS'), year=c(2003:2012),unit=c("OTB","PTB"),
               season=as.character(1:4),area=c("VI","VII","VIIIabd"))

dimnames(others)$len <- c('< MCRS','> MCRS')

#==============================================================================
# Catch weight (landings+ discards) by gear, season, year, day, vessel
#==============================================================================

catch<-FLQuants(mackerel=FLQuant(, dimnames=dimnames), 
               horse_mackerel=FLQuant(, dimnames=dimnames), 
               hake=FLQuant(, dimnames=dimnames),
               megrim=FLQuant(, dimnames=dimnames), 
               anf=FLQuant(, dimnames=dimnames),
               others=FLQuant(, dimnames=dimnames))

landings<- discards<- price<- CPUE <-catchmean<- catch

# I use the total effort for the pair trawlers, which means the two vessels 
# needed for catching the total amount. However, I will consider that in number of vessels, 2 are just 1.
#---------------------------------------------------------------------------------------------------------
#effort_days[,,"PTB"]<- effort_days[,,"PTB"]*2

for (i in 1:(length(species)+1)){  
      
      if (i<6){
            catch.wt <- (landings.wt(flf[[i]])+ discards.wt(flf[[i]]))/1000
            landings.wt <- (landings.wt(flf[[i]]))/1000 
            discards.wt <- (discards.wt(flf[[i]]))/1000
      }
             
      if (i==6){
            # need to change the position of discards, because discards of others should not be < MCRS
            #----------------------------------------------------------------------------------------
            catch.wt <- landings.wt<- discards.wt<- others
            landings.wt [1] <- discards.wt [1] <-NA
            discards.wt [2] <- others[1]
            #catch.wt <- ifelse(is.na(landings.wt),0,landings.wt) + ifelse(is.na(discards.wt),0,discards.wt)
      }
      
      dimnames(catch.wt)$len <- dimnames(landings.wt)$len <- dimnames(discards.wt)$len <-c('< MCRS','> MCRS')
      units(catch.wt)<- units(landings.wt)<- units(discards.wt)<-"t"
      
      catch.wt[is.nan(catch.wt)| is.infinite(catch.wt)|catch.wt==0] <- NA
      landings.wt[is.nan(landings.wt)| is.infinite(landings.wt)|landings.wt==0] <- NA
      discards.wt[is.nan(discards.wt)| is.infinite(discards.wt)|discards.wt==0] <- NA
            
      catchmean_vessel <- catch.wt  %/% number_vessels[,as.character(y)]
      cpue  <- catch.wt  %/% effort_days[,as.character(y)] # * number_vessels[,as.character(y)])

      catchmean_vessel[is.nan(catchmean_vessel)| is.infinite(catchmean_vessel)|catchmean_vessel==0] <- NA
      cpue[is.nan(cpue)| is.infinite(cpue)|cpue==0] <- NA
      
      catch    [[i]] <-catch.wt
      landings [[i]] <-landings.wt
      discards [[i]] <-discards.wt
      catchmean[[i]] <-catchmean_vessel
      CPUE     [[i]] <-cpue
      
      rm(catch.wt, landings.wt, discards.wt, catchmean_vessel, cpue)
      
}

catch     <- lapply(catch, foo, "t")
landings  <- lapply(landings, foo, "t")
discards  <- lapply(discards, foo, "t")
catchmean <- lapply(catchmean, foo, "t/vessel")
CPUE      <- lapply(CPUE, foo, "t/day")

# remove the ang stock, which is already inside others (ANG is not going to be a choke sp)
#-------------------------------------------------------------------------------------------
catch    <- catch[-5]
landings <- landings[-5]
discards <- discards[-5]
CPUE     <- CPUE[-5]
catchmean<- catchmean[-5]

#==============================================================================
# Catch rates (catch/ effort(days)) by gear, season, year, day, vessel
# new structure with landings and discards diferenciated
#==============================================================================

dimnames<-list(len=c('landings > MCRS','discards > MCRS', 'discards < MCRS'), 
               year=c(2003:2012),unit=c("OTB","PTB"),
               season=as.character(1:4),area=c("VI","VII","VIIIabd"))

catch_rates<-FLQuants(mackerel=FLQuant(, dimnames=dimnames), 
                horse_mackerel=FLQuant(, dimnames=dimnames), 
                hake=FLQuant(, dimnames=dimnames),
                megrim=FLQuant(, dimnames=dimnames),
                others=FLQuant(, dimnames=dimnames))

catch_rates <- FLQuants(lapply(names(catch_rates),foo_catchrates))
names(catch_rates) <- names(landings)

#catch_rates <- lapply(catch_rates, function (x) x %/% effort_days[,as.character(y)])
catch_rates <- lapply(catch_rates, function (x) x %/% number_vessels[,as.character(y)])
catch_rates <- lapply(catch_rates, yearMeans)

#catch_rates <- lapply(catch_rates, function (x) x*1000)
catch_rates <- lapply(catch_rates, foo, "kg/day")

# replace NA with 0, just for plotting
#-------------------------------------
catch_rates           <- lapply(catch_rates, function (x) replace (x,is.nan(x) | is.infinite(x),0))

catch_rates_df        <- as.data.frame(catch_rates)
catch_rates_df$q      <- with(catch_rates_df, paste(area, unit, sep=" "))
catch_rates_df        <- subset(catch_rates_df, select=c("len","season","data","qname","q"))
names(catch_rates_df) <- c("cat", "season","data","species","area")

levels(catch_rates_df$species) <- c(levels(catch_rates_df$species),' horse \n mackerel')
catch_rates_df$species[catch_rates_df$species=="horse_mackerel"] <- c(' horse \n mackerel')

# remove areas VI PTB and VII PTB
#----------------------------------
catch_rates_df <-subset(catch_rates_df, !(area %in% c("VI PTB","VII PTB")))

# replace discards the others for just discards withpout MCRS
#-------------------------------------------------------------
a <- subset(catch_rates_df, species %in% "others" & cat %in% c('discards > MCRS'))
a$cat<- 'discards'

catch_rates_df <-subset(catch_rates_df, !(species %in% "others" & 
                              cat %in% c('discards > MCRS', 'discards < MCRS')))

catch_rates_df <- rbind(catch_rates_df, a)


# Order factor as I wish for the plot
#------------------------------------
catch_rates_df$cat <- factor(catch_rates_df$cat, 
                             levels=c('landings > MCRS','discards > MCRS', 'discards < MCRS', 'discards'))
catch_rates_df$area <- factor(catch_rates_df$area, 
                              levels=c("VI OTB","VII OTB","VIIIabd OTB","VIIIabd PTB"))
catch_rates_df$species <- factor(catch_rates_df$species, 
                                 levels=c("hake","megrim","mackerel",' horse \n mackerel',"others"))

#==============================================================================
#  Figure 1: Spatial and temporal patterns in cath rate
#==============================================================================

# png(filename="~/Documents/BoB/DynState/Paper/figures/cpue_jan.png",
#     width=12, height=11, units="cm", res=500, pointsize=8)
# 
#       ggplot() +  
#             geom_smooth(data= catch_rates_df, aes(x =season, y = data, group=cat, linetype=cat,colour=cat),
#                         method = "loess", size = 0.25)+
#             geom_point(data=  catch_rates_df, aes(x =season, y = data,group=cat, shape=cat, colour=cat),
#                         size=1)+
#             facet_grid(species~area, scales = "free")+
#             scale_color_manual (values=c("black", "#999999","red", 'blue'))+
#             theme_bw()+
#             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#                   panel.background = element_blank(),legend.title=element_blank(),
#                   legend.position="bottom",axis.text=element_text(size=8),
#                   #legend.position=c(0.88,0.165),axis.text=element_text(size=8),
#                   strip.text = element_text(size = 8),
#                   text = element_text(size=7))+
#             xlab("season") +
#             ylab("CPUE (t/vessel)")
# dev.off()

#==============================================================================       
# Prices; need to be in euros/ tn !!!!
#==============================================================================

price [[1]][] <- c(rep(c(0,  709),20), rep(c(0, 1898),20), rep(c(0,  700),20), rep(c(0,  669),20))
price [[2]][] <- c(rep(c(0,  742),20), rep(c(0, 1014),20), rep(c(0, 1274),20), rep(c(0, 1669),20))
price [[3]][] <- c(rep(c(0, 2525),20), rep(c(0, 2847),20), rep(c(0, 1879),20), rep(c(0, 2347),20))
price [[4]][] <- c(rep(c(0, 5312),20), rep(c(0, 4564),20), rep(c(0, 4898),20), rep(c(0, 5055),20))
price [[6]][] <- c(rep(c(0, 3001),20), rep(c(0, 2279),20), rep(c(0, 2807),20), rep(c(0, 3297),20))

price <-   lapply(price, foo, "euros")
# remove the ang stock
#---------------------
price <- price[-5]

#==============================================================================================         
# Table 1: summary fleet data: average values and standard deviation along the period 2003:2012
#==============================================================================================  

# Create a list to combine Catch(t), Landings(t), Discards (t) and discards < MCRS (%)
#-------------------------------------------------------------------------------------
model <- list(catch= catch, landings= landings, discards= discards, discards_MCRS= discards)
model_area <- model_tot <- vector(mode="list", length=4)
names(model_area) <- names(model_tot) <- names(model)

# AVERAGE
#--------
for (i in 1: length(model)){
      if (i<4){
            model_area[[i]] <- Reduce('+',lapply(lapply(lapply(model[[i]], foo_area),yearMeans),foo_na))
            model_tot [[i]] <- Reduce('+',lapply(lapply(lapply(model[[i]],foo_tot),yearMeans),foo_na))
      } 
      
      if (i==4){
            model_area[[i]] <- Reduce('+',lapply(lapply(lapply(model[[i]], foo_area_quant1),yearMeans),
                                                 foo_na))%/% 
                                    Reduce('+',lapply(lapply(lapply(model[[i]], foo_area),yearMeans),
                                                      foo_na))*100
            model_tot [[i]] <- Reduce('+',lapply(lapply(lapply(model[[i]], foo_tot_quant1),yearMeans),
                                                 foo_na))%/% 
                                    Reduce('+',lapply(lapply(lapply(model[[i]], foo_tot),yearMeans),
                                                      foo_na))*100
      } 
}

# melt behaves as do.call(rbind.data.frame,lapply(LIST NAME,as.data.frame))
#---------------------------------------------------------------------------
summary_conditioning <- rbind (melt(model_area), melt(model_tot))


# STANDARD DEVIATION
#-------------------
#variance divided by the number of years to estimate the variance of the mean
#------------------------------------------------------------------------------

for (i in 1: length(model)){
      if (i<4){
            model_area[[i]] <-sqrt(yearVars(Reduce('+',lapply(lapply(model[[i]], foo_area),foo_na)))%/%10)
            model_tot [[i]] <-sqrt(yearVars(Reduce('+',lapply(lapply(model[[i]],foo_tot),foo_na)))%/%10)
      } 
      
      if (i==4){
            model_area[[i]] <- sqrt(yearVars(Reduce('+',lapply(lapply(model[[i]], foo_area_quant1),foo_na))%/% 
                                             Reduce('+',lapply(lapply(model[[i]], foo_area),foo_na))*100)%/%10)
            model_tot [[i]] <- sqrt(yearVars(Reduce('+',lapply(lapply(model[[i]], foo_tot_quant1),foo_na))%/% 
                                                   Reduce('+',lapply(lapply(model[[i]], foo_tot),foo_na))*100)%/%10)
      } 
}   

summary_table2 <- rbind (melt(model_area), melt(model_tot)) 

summary_conditioning $ std_dev <- summary_table2$value  

summary_conditioning $ Area <-with(summary_conditioning, paste(area, unit, sep=" "))
summary_conditioning <- subset(summary_conditioning, select=c("len","Area","value","std_dev","L1"))
names(summary_conditioning) <- c("cat", "area","average","stdev", "variable")
summary_conditioning <-subset(summary_conditioning, !(area %in% c("VI PTB","VII PTB"))) 

rm(model_area, model_tot, summary_table2,i)


#FUEL COST
#------------------------------------------------------------------------------
# We are using 2235 euros by day; however Prellezo et al., 2016 considered 1240 euro/day (AER 2014)

fuel_cost       <- effort_days[,as.character(y)] * 1240
units(fuel_cost)<- "euros"

fuel_cost_out   <-   yearMeans(foo_area(fuel_cost))
fuel_cost_var   <-   sqrt(yearVars(foo_area(fuel_cost))%/%10)

fuel_cost_tout   <-   yearMeans(foo_tot(fuel_cost))
fuel_cost_tvar   <-   sqrt(yearVars(foo_tot(fuel_cost))%/%10)

summary_cost <- cbind (rbind(melt(fuel_cost_out),melt(fuel_cost_tout)), 
                         c(melt(fuel_cost_var)$value,melt(fuel_cost_tvar)$value)) 
names(summary_cost)[c(7,8)]<- c("average_fuel_cost","stdev_fuel_cost")

#GEAR MAINENANCE COST
#------------------------------------------------------------------------------
gear_cost       <- effort_days[,as.character(y)] * 287
units(gear_cost)<- "euros"

gear_cost_out   <-   yearMeans(foo_area(gear_cost))
gear_cost_var   <-   sqrt(yearVars(foo_area(gear_cost))%/%10)

gear_cost_tout   <-   yearMeans(foo_tot(gear_cost))
gear_cost_tvar   <-   sqrt(yearVars(foo_tot(gear_cost))%/%10)

summary_gear_cost <- cbind (rbind(melt(gear_cost_out),melt(gear_cost_tout)), 
                       c(melt(gear_cost_var)$value,melt(gear_cost_tvar)$value)) 
names(summary_gear_cost)[c(7,8)]<- c("average_gear_cost","stdev_gear_cost")


# The total variance for the turnover is the sum of the variances if the variables are independent
#-------------------------------------------------------------------------------------------------
turnover      <- catchmean
landing_cost  <- turnover

for(i in 1:length(species)){      
      
      turnover_sp                      <- catch[[i]] %*% price[[i]]
      turnover_sp[is.na(turnover_sp)]  <-0
      turnover [[i]]                   <-turnover_sp
      landing_cost [[i]]               <- catch[[i]] * 1213.377
}

# mean Turnover and expected from each stock and the combine one from all stocks together
#------------------------------------------------------------------------------
turnover_area <- yearMeans(Reduce("+",lapply(turnover,foo_area)))
turnover_tot  <- yearMeans(Reduce("+",lapply(turnover,foo_tot)))

turnover_ex_area <- yearMeans(Reduce("+",lapply(turnover,foo_area))%-% foo_area(fuel_cost))
turnover_ex_tot  <- yearMeans(Reduce("+",lapply(turnover,foo_tot ))%-% foo_tot (fuel_cost))

# mean std.dev. for the Turnover
#------------------------------------------------------------------------------
turnover_sig_area    <-  sqrt(yearVars(Reduce("+",lapply(turnover,foo_area))%/%10))
turnover_sig_tot     <-  sqrt(yearVars(Reduce("+",lapply(turnover,foo_tot))%/%10))

turnover_sig_ex_area <-  sqrt(yearVars(Reduce("+",lapply(turnover,foo_area))%-% foo_area(fuel_cost)%/%10))
turnover_sig_ex_tot  <-  sqrt(yearVars(Reduce("+",lapply(turnover,foo_tot ))%-% foo_tot (fuel_cost)%/%10))

summary_economics    <-  cbind (rbind(melt(turnover_area),melt(turnover_tot)), 
                                c(melt(turnover_sig_area)$value,melt(turnover_sig_tot)$value),
                                c(melt(turnover_ex_area)$value,melt(turnover_ex_tot)$value),
                                c(melt(turnover_sig_ex_area)$value,melt(turnover_sig_ex_tot)$value)) 
names(summary_economics)[c(7,8,9,10)]<- c("average_turn","stdev_turn","average_turn_exp","stdev_turn_exp")

summary_economics[c(11,12)] <- summary_cost[c(7,8)]

summary_economics $ Area <-with(summary_economics, paste(area, unit, sep=" "))
summary_economics <- subset(summary_economics, 
                            select=c("Area", "average_turn", "stdev_turn","average_turn_exp", 
                                     "stdev_turn_exp","average_fuel_cost", "stdev_fuel_cost"))
names(summary_economics)[1] <- "area"
summary_economics<-subset(summary_economics, !(area %in% c("VI PTB","VII PTB"))) 

summary_economics[2:7]<- round(summary_economics[2:7]/1000)
# thousand euros
      
rm(fuel_cost_out, fuel_cost_var,fuel_cost_tout,fuel_cost_tvar, summary_cost, fuel_cost,
   turnover_area, turnover_tot, turnover_ex_area,turnover_ex_tot,turnover_sig_area,turnover_sig_tot,
   turnover_sig_ex_area, turnover_sig_ex_tot)
# table cost, turnover, expected turnover

      
save(landings,discards, catch, catchmean, price, CPUE, catchmean, catch_rates_df,effort_days,
     number_vessels, summary_conditioning, summary_economics, summary_fleet, 
     file="~/Documents/BoB/DynState/data/model_conditioning.RData")
