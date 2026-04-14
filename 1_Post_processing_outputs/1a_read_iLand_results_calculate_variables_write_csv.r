
#install.packages("devtools")
#devtools::install_github("ricardo-bion/ggradar")
#
# Laura Dobor & Marco Baldo, CULS
# 2025 - 2026.04.14.
# Read iLand outputs and select variables, calculate a few things and make csv files from the desired results
# This script should be running for 2 cases, disaster and no-disaster




# Load required libraries
library(tidyr)
library(RSQLite)
library(readxl)
library(dplyr)
library(vegan)
library(ggplot2)
library(gridExtra)   
library(fields)
library(ggradar)
#_______________________________________________________________________________
# Path to search the data folder
# iLand outputs are here
dataroot <- "E:/iLandDist - new version 2.0 - 2025/output/"
outroot<- "D:/___PROJECTS/2025_iLand_management_study/04_work/3_analyses/Output_summary_tables/"


version<-""
version<-"DISASTER2"





fs <- list.files(dataroot, ".sqlite") 
file.info(paste0(dataroot,fs))$size


o<-grep("DISASTER", fs)
if (version=="") fs<-fs[-o]
if (version=="DISASTER2") fs<-fs[o]




# CREATE NEW EMPTY DATAFRAME
removals.all <- c()
lnd <- c()
damage.all<-c()
recovery.all<-c()
H_avg <- c()
annual.data.all <-c()
MF.all<-c()
options(warn = 1)

for (i in (1:length(fs)))  {


  f <- fs[i]
  
  mgm<-strsplit(f, "_")[[1]][1]
  windcase<-strsplit(f, "_")[[1]][2]
  part1<-  strsplit(f, "_")[[1]][3]
  part2<-  strsplit(f, "_")[[1]][4]
  model<-paste(part1,part2,sep='_')
  rcp<-strsplit(f, "_")[[1]][5]
  rcp<-strsplit(rcp,'.sqlite') [[1]][1]
  case<-  strsplit(f, ".sqlite")[[1]][1]
  

  if (is.na(rcp)==T) rcp<-"-"
  if (model=="refclim.sqlite_NA") model<-"refclim"

  print(f)
  #----------------------------------------------- read
  # connect to the database
  sqlite.driver <- dbDriver("SQLite")
  db1 <- dbConnect(sqlite.driver, dbname = paste0(dataroot,f))
  

 
  landscape <- dbReadTable(db1,"landscape")
  landscape.removed <- dbReadTable(db1,"landscape_removed")
  if (mgm!="UNMANAGED") abeUnit <- dbReadTable(db1, "abeUnit")

  barkbeetle <- dbReadTable(db1,"barkbeetle")
  wind <- dbReadTable(db1,"wind")
  carbon <- dbReadTable(db1,"carbon")
  carbonflow <- dbReadTable(db1,"carbonflow")

  dbDisconnect(db1)    
  
  #-----------------------------------------------------------------------------
  # CREATE THE CALCULATION FOR DAMAGES
  
  landscape.area<-landscape$area[1]                                             # CREATE THE VARIABLE FOR LANDSCAPE AREA          
  
  lnd_volume = landscape %>% group_by(year)  %>%                                # CREATE THE SUMMARIZATION OF THE SPECIES VOLUME PROPORTION TO CREATE A TOTAL LANDSCAPE VOLUME
                summarise(tot_vol = sum(volume_m3), .groups = 'drop')
  
                                                            
  
  # we need to shift the landscape volume by one year to calculate the relative impact of disturbances, because every output is the end-of-year value
  # and to see the killed volume relatively to the pre-damage level we need prev.year data
  
  lnd_volume.yearstart<-lnd_volume %>% mutate(year=year+1)

  # WIND AND BARKBEETLE MERGING

  damage <- data.frame(year=barkbeetle$year,                                    # CREATE THE DATA FRAME FOR FOR DAMAGE OF BARKBEETLE
                       barkbeetle=barkbeetle$killedVolume/landscape.area)
  
  # ADD WIND IMPACT IN THE DAMAGE DATA FRAME                                    # LEFT_JOIN IS A FUNCTION TO JOIN A VARIABLE IN THIS CASE COLUMN 1 AND 2 AND MANY ROWS BASE ON YEAR VARIABLE NUMBER OF ROWS
  damage<-left_join(damage,wind[,c(1,8)],by=("year"))                           # NB ...wind[,c(1,8)]... Means all the row, column 1 (year),8 (killedVolume). 
  damage<-left_join(damage,lnd_volume.yearstart,by=("year"))                              # ADD THE LANDSCAPE VOLUME IN THE DAMAGE DATA FRAME
  
  colnames(damage)<-c("year","barkbeetle","wind", "landscape_volume_yearstart")
  
  damage$wind[which(is.na(damage$wind)==TRUE)] <- 0            

  
  
  damage<-damage %>% mutate(wind=wind/landscape.area, impact=barkbeetle+wind) %>%
                     mutate(relimpact=100*(impact/landscape_volume_yearstart), 
                            model=model, mgm=mgm, rcp=rcp, windcase=windcase) 
  

  
  #-----------------------------------------------------------------------------
  # Make the 3 categories of removals:
  
  
  removals<-landscape.removed %>% group_by(year, reason) %>% summarize(removed.volume=sum(volume_m3)) %>% 
                                  mutate(model=model, mgm=mgm, rcp=rcp, windcase=windcase)
  


  #-----------------------------------------------------------------------------
  # RECOVERY ANALYSIS - CREATE THE DATASET FOR THE RECOVERY ANALYSIS
  #-----------------------------------------------------------------------------
  
  # Calculate total carbon for the runs
  
  annual.lnd <- landscape %>% filter(year>0) %>% group_by(year) %>% summarise(total_carbon_kg=sum(total_carbon_kg), volume_m3=sum(volume_m3))
 
  
  annual.data<-data.frame(year=annual.lnd$year) %>% mutate(total_carbon_kg=annual.lnd$total_carbon_kg, volume_m3=annual.lnd$volume_m3)
  

  year_49_value <- filter(annual.lnd, year == 49)
  years_49_to_100 <- filter(annual.lnd, year >= 49 & year <= 100)
  
  
  recovery_year_total_carbon <- min(annual.data$year[which(annual.data$year > 50 & annual.data$total_carbon_kg >= year_49_value$total_carbon_kg)])
  if (length(recovery_year_total_carbon)==0) recovery_year_total_carbon<-"NA"
  recovery_year_volume <- min(annual.data$year[which(annual.data$year > 50 & annual.data$volume_m3 >= year_49_value$volume_m3)])
  if (length(recovery_year_volume)==0) recovery_year_volume<-"NA"


  results <- years_49_to_100 %>% mutate(TC_49 = year_49_value$total_carbon_kg, 
                                        VOL_49=year_49_value$volume_m3,
                                        Perc_Diff_TC = (((TC_49 - total_carbon_kg) / TC_49) * 100) * -1,
                                        Perc_Diff_VOL = (((VOL_49 - volume_m3) / VOL_49) * 100) * -1,
                                        recovery_year_total_carbon=recovery_year_total_carbon,
                                        recovery_year_volume=recovery_year_volume,
                                        year_after_impact = year - 49) %>% 
                                  mutate(model=model, mgm=mgm, rcp=rcp, windcase=windcase)


  
  #-----------------------------------------------VARIABLES FOR MULTIFUCTIONALITY
  
  # create this MF variable, and append all the variables later which we need
  MF <- landscape %>%  group_by(year) %>% summarise(annual.increment=sum(gwl_m3),  standing.volume=sum(volume_m3),  LAI=sum(LAI))
 
  
  # ------------- HARVEST
  if (mgm!="UNMANAGED") ann.harv<-data.frame(year=abeUnit$year,annual.harvest=abeUnit$realizedHarvest)
  if (mgm=="UNMANAGED") ann.harv<-data.frame(year=MF$year,annual.harvest=rep(0,length(MF$year)))
  
  MF<-left_join(ann.harv,MF, by="year")  
  
  #-------------- LANDSCAPE SCALE SHANNON:
  sh<-landscape %>% group_by(year) %>%   summarize(shannon_BA_landscape = diversity(basal_area_m2, base = exp(1)), 
                                                   shannon_VOL_landscape = diversity(volume_m3, base = exp(1)))
  
  MF<- left_join(sh,MF, by="year")
  

  # ----------------DEADWOOD and carbonstock:
  
  carbon<-data.frame(year=carbon$year,deadwood.c.ag=carbon$snags_c+carbon$snagsOther_c_ag+carbon$downedWood_c_ag,
                          carbonstock=carbon$stem_c+carbon$branch_c+carbon$foliage_c+carbon$coarseRoot_c+carbon$fineRoot_c+ carbon$regeneration_c + carbon$snags_c +  carbon$snagsOther_c +  carbon$downedWood_c +  carbon$litter_c +  carbon$soil_c)
  MF<-left_join(carbon,MF, by="year")
  
  
  # ---------------DECIDIOUS TREE VOLUME and BA
  
  spec<-read.csv("species_codes.csv")
  
  decid<-spec$Sname[which(spec$Conifer.Broad=="B")]
  decid.volba<-landscape %>%  group_by(year) %>% filter(species %in% decid) %>% summarise(standing.volume.decidious=sum(volume_m3),BA.decidious=sum(basal_area_m2))
  
  MF<-left_join(decid.volba,MF, by="year")
 
  # -----------------NEP
  NEP<-data.frame(year=carbonflow$year,NEP=carbonflow$NEP)
 
  MF<-left_join(NEP,MF, by="year")
  

  # -----------------Disturbance impact
  
  dist.imp<-damage %>% select(year,impact)
  MF<-left_join(dist.imp,MF, by="year")
  
  # --------------------Natural mortality
  
  nat.mort<-removals %>% filter(reason=="N") %>% mutate(nat.mortality.volperha=removed.volume/landscape.area) %>% select(year, nat.mortality.volperha)
  MF<-left_join(nat.mort,MF, by="year")
  
  MF<-MF %>% mutate(model=model, mgm=mgm, rcp=rcp, windcase=windcase)
#  
  #-----------------------------------------------------------------------------

  removals.all<-rbind(removals.all,removals)
  damage.all<-rbind(damage.all, damage)

  landscape<- landscape %>%  mutate(model=model, mgm=mgm, rcp=rcp, windcase=windcase)
  lnd<-rbind(lnd, landscape)
  
  annual.data<-annual.data  %>%  mutate(model=model, mgm=mgm, rcp=rcp, windcase=windcase)
  annual.data.all<-rbind(annual.data.all, annual.data)

  recovery.all <- rbind(recovery.all,results)
  MF.all<-rbind(MF.all, MF)

  
}

#-------------------------------------------------------------------------------


write.csv(MF.all, paste0(outroot,Sys.Date(),"_multifunctionality",version,".csv"), row.names = FALSE)
write.csv(annual.data.all, paste0(outroot,Sys.Date(),"_annual_data",version,".csv"), row.names = FALSE)
write.csv(removals.all, paste0(outroot,Sys.Date(),"_removals",version,".csv"), row.names = FALSE)
write.csv(damage.all, paste0(outroot,Sys.Date(),"_damages",version,".csv"), row.names = FALSE)
write.csv(recovery.all, paste0(outroot,Sys.Date(),"_recovery",version,".csv"), row.names = FALSE)
write.csv(lnd, paste0(outroot,Sys.Date(),"_landscape",version,".csv"), row.names = FALSE)











