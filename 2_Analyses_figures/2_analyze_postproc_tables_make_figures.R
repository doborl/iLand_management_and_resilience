


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





version<-"NO_DISASTER"
version<-"DISASTER2"

version<-"NO_DISASTER"
#version<-"DISASTER2"
dataroot<-"D:/___PROJECTS/2025_iLand_management_study/04_work/analyses/Output_summary_tables/"





#--------------------------------------------- YOU CAN ALSO START FROM HERE:

landscape.area<-17749.26


if (version=="NO_DISASTER") {
  
  date<-"2025-04-19"
MF.all<-read.csv(paste0(dataroot,date,"_multifunctionality.csv"))
annual.data.all<-read.csv( paste0(dataroot,date,"_annual_data.csv"))
removals.all<-read.csv( paste0(dataroot,date,"_removals.csv"))
damage.all<-read.csv(paste0(dataroot,date,"_damages.csv"))
recovery.all<-read.csv( paste0(dataroot,date,"_recovery.csv"))
lnd<- read.csv(paste0(dataroot,date,"_landscape.csv"))


}




if (version=="DISASTER2") {
  
  date<-"2025-04-18"
  MF.all<-read.csv(paste0(dataroot,date,"_multifunctionality_DISASTER2.csv"))
  annual.data.all<-read.csv( paste0(dataroot,date,"_annual_data_DISASTER2.csv"))
  removals.all<-read.csv( paste0(dataroot,date,"_removals_DISASTER2.csv"))
  damage.all<-read.csv(paste0(dataroot,date,"_damages_DISASTER2.csv"))
  recovery.all<-read.csv( paste0(dataroot,date,"_recovery_DISASTER2.csv"))
  lnd<- read.csv(paste0(dataroot,date,"_landscape_DISASTER2.csv"))
  
  
}





text<-"last50y"
text<-"mid30y"


# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
pdf(paste0(dataroot, "plots/",date,"_",version,"_",text,"__LD.pdf"), height = 8, width = 12)

#_______________________________________________________________________________
# This tells the colors:

species.we.have<-unique(lnd$species)                                            # IT IS SAYING WHICH SPECIES WE HAVE IN THE DATABASE IN THIS CASE LANDSCAPE


# LIST OF ALL POSSIBLE SPECIES

cols.all=c( "rops"="#e0e0e0", "acpl"="#A9A9A9",   "alin"="#696969", "alvi"="#2e2e2e",
            "bepe"="#fadfad", 
            "casa"="#7eeadf", "coav"="#20c6b6",  
            "tipl"="#645394", "ulgl"="#311432" ,
            "saca"="#D8BFD8",  "soar"="#DDA0DD", "soau"="#BA55D3",
            "pice"="#D27D2D", "pini"="#a81c07",
            "algl"="#2ECBE9","tico"="#128FC8",  "potr"="#00468B","poni"="#5BAEB7",
            "frex"="#fe9cb5","cabe"="#fe6181","acps"="#fe223e",
            "lade"="#FFFE71","abal"="#FFD800", "pisy"="#A4DE02",
            "fasy"="#76BA1B", "piab"="#006600",
            "quro"="#FF7F00", "qupe"="#FF9900", "qupu"="#CC9900" 
)


# COLORATION ORDER FOR ALL THE POSSIBLE SPECIES

new_order_gg.all=c("alvi","alin", "acpl", "rops","bepe" ,"coav", "casa", "ulgl", "tipl",  "soau", "soar", "saca",  "pini", "pice",
                   "poni", "algl", "tico", "potr",  "frex","cabe", "acps",  "lade", "abal",  "qupu", "qupe","quro","pisy", "fasy", "piab")


# This will show at the end only the species we really have on the landscape. 

cols<-cols.all[names(cols.all) %in% species.we.have]
new_order_gg<- new_order_gg.all[new_order_gg.all %in% species.we.have]
#_______________________________________________________________________________




# STARTING PLOTS
for (jj in 5:9) {

w<-paste0("w",jj)
# column diagram
g1<-ggplot( (removals.all %>% filter(windcase==w)), aes(year, removed.volume/landscape.area, fill=factor(reason, levels=c("S","D","H", "N"))))+
  geom_bar(position="stack", stat="identity")+
  ggtitle("Removed volume m3/ha (from living trees)")+
  facet_grid(mgm+windcase~rcp+model)+
  labs(x = "Year",y="Removed volume m3/ha",fill = "Reason")+
  scale_fill_manual(values=rev(c("limegreen","#4897D8","#FFDB5C","#FA6E59")), labels=c("salvaged","disturbed","harvested","natural mort."))+               #"#B7B8B6","#34675C","#B3C100" grey and greens
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))

# Make a plot with ggplot, volume, colored by species for the transitional period for Clear cut management system
#-------------------------------------------------------------------------------

g2<-ggplot((lnd %>% filter(windcase==w)), aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("volume m3/ha")+
  facet_grid(mgm+windcase~rcp+model)+
  #  facet_wrap(mgm~model+windcase)+
  labs(x = "Year",y="Volume m3/ha",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  #ylim(0,400)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))

#-------------------------------------------------------------------------------

g3<-ggplot((lnd  %>% filter(windcase==w)), aes(year,total_carbon_kg/1000, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Total carbon t/ha")+
  facet_grid(mgm+windcase~rcp+model)+
  #facet_wrap(mgm~model+windcase)+
  labs(x = "Year",y="total carbon t/ha",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  #ylim(0,400)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))

#-------------------------------------------------------------------------------
# PLOT SECOND Y AXIS FOR KILLED VOLUME BY DISTURBANCES IN LANDSCAPE AVG LINE 75

area<-lnd$area[1]
ylim.bb <- c(0, 40)                                                                                          # in this example, precipitation look the link down
ylim.w <- c(0, 100) 

# This is needed to mantain the proportion in the two axis
b <- diff(ylim.w)/diff(ylim.bb)
a <- ylim.w[1] - b*ylim.bb[1] 


# TO MAKE 2 LINE AND 2 DIFFERENT SCALE PLOT "https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales"
g4<-ggplot((damage.all %>% filter(windcase==w)), aes(year,wind))+
  geom_col(fill="grey",col="black", size=0.5)+
  geom_line(aes(y = a+ barkbeetle*b), data = (damage.all %>% filter(windcase==w)), size=1, col="pink") +
  scale_y_continuous(name="Wind damage [m3/ha]", sec.axis = sec_axis(~ (. - a)/b,name = "Bark beetle damage [m3/ha]"))+
  xlab("Year")+
  ggtitle("Absolute damages m3/ha")+
  facet_grid(mgm+windcase~rcp+model)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))




#-------------------------------------------------------------------------------
# PLOT SECOND Y AXIS IN RELATIVE KILLED VOLUME BY DISTURBANCES IN LANDSCAPE AVG

# variant 1
ylim.bb <- c(0, 10)                                                                                          # in this example, precipitation look the link down
ylim.w <- c(0, 20)

b <- diff(ylim.w)/diff(ylim.bb)
a <- ylim.w[1] - b*ylim.bb[1] 

# TO MAKE 2 LINE AND 2 DIFFERENT SCALE PLOT "https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales"
g5<-ggplot((damage.all %>% filter(windcase==w)),aes(year,100*wind/landscape_volume_yearstart))+
  geom_col(fill="grey",col="black")+
  geom_line(aes(y = a+ 100*barkbeetle/landscape_volume_yearstart*b), data = (damage.all %>% filter(windcase==w)), size=0.9, col="pink") +
  scale_y_continuous(name="Wind relative damage %", sec.axis = sec_axis(~ (. - a)/b,name = "Bark beetle relative damage %"))+
  facet_grid(mgm+windcase~rcp+model)+
  ggtitle("Relative damages compared to pre-dist volume %")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))


print(g1)
print(g2)
print(g3)
print(g4)
print(g5)

}
# CUMULATIVE DAMAGES:

dam<-damage.all %>% group_by(rcp, model, mgm, windcase) %>% mutate(csimp=cumsum(impact), csrelimp=cumsum(relimpact))

g6<-ggplot(dam,aes(x=year,y=csimp, col=mgm))+
  geom_step(size=0.9)+ 
  facet_grid(windcase~rcp+model)+
  ggtitle("Cumulative damages m3/ha")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))


print(g6)
#summary table for disturbance impact:

# For each case I calculate the total bb, wind damage +total sum damage. the average rel impact 
dam.summary<-damage.all %>% group_by(rcp, model, mgm, windcase) %>% summarize(sum_bb=sum(barkbeetle),sum_wind=sum(wind), sum_total=sum(impact), 
                                                                              mean.lnd.vol=mean(landscape_volume_yearstart), 
                                                                              mean.rel.impact=mean(relimpact),
                                                                              mean_imp_rel_meanlndvolume=100*mean(impact)/mean(landscape_volume_yearstart)
                                                                              )
# the last 2 should be the same, but use the last one

# Here I calculate min max and average to be able to plot the bars on the barplot: min max of the models and windcases

dam.summary2<-dam.summary %>% group_by(rcp, mgm) %>% summarize(sum_total2=mean(sum_total), min=min(sum_total), max=max(sum_total),
                                                                        meanrel=mean(mean_imp_rel_meanlndvolume), minrel=min(mean_imp_rel_meanlndvolume), maxrel=max(mean_imp_rel_meanlndvolume))

g7<-ggplot(dam.summary2, aes( factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED")), sum_total2, fill=  factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED"))))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=min, ymax=max), width=.5,) +
  facet_grid(~rcp)+
  ggtitle("Whole period - absolute damage")+
  labs(x = "Management",y="Total damaged volume m3/ha",fill = "Management")+
  #scale_fill_manual(values=c("#4897D8","#FFDB5C","#FA6E59"), labels=c("HistClim","RCP4.5","RCP8.5"))+  
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

g8<-ggplot(dam.summary2, aes( factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED")), meanrel, fill=  factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED"))))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=minrel, ymax=maxrel), width=.5,) +
  facet_wrap(~rcp)+
  ggtitle("Whole period - rel. damage ")+
  labs(x = "Management",y="Mean dam.vol. rel. to mean landscape vol. %",fill = "Management")+
  #scale_fill_manual(values=c("#4897D8","#FFDB5C","#FA6E59"), labels=c("HistClim","RCP4.5","RCP8.5"))+  
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())





# ------------------------- do the same for the LAST 50years:


dam.summary<-damage.all %>% filter(year>49) %>% group_by(rcp, model, mgm,windcase) %>% summarize(sum_bb=sum(barkbeetle),sum_wind=sum(wind), sum_total=sum(impact), 
                                                                                                 mean.lnd.vol=mean(landscape_volume_yearstart), 
                                                                                                 mean.rel.impact=mean(relimpact),
                                                                                                 mean_imp_rel_meanlndvolume=100*mean(impact)/mean(landscape_volume_yearstart)
                                                                                                 )

dam.summary2<-dam.summary %>% group_by(rcp, mgm) %>% summarize(sum_total2=mean(sum_total), min=min(sum_total), max=max(sum_total),
                                                                         meanrel=mean(mean_imp_rel_meanlndvolume), minrel=min(mean_imp_rel_meanlndvolume), maxrel=max(mean_imp_rel_meanlndvolume))



g9<-ggplot(dam.summary2, aes( factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED")), sum_total2, fill=  factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED"))))+
  geom_bar(stat="identity")+
  ggtitle("Last 50 years -  absolute damage")+
  geom_errorbar(aes(ymin=min, ymax=max), width=.5,) +
  facet_grid(~rcp)+
  labs(x = "Management",y="Total damaged volume m3/ha",fill = "Management")+
  #scale_fill_manual(values=c("#4897D8","#FFDB5C","#FA6E59"), labels=c("HistClim","RCP4.5","RCP8.5"))+  
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

g10<-ggplot(dam.summary2, aes( factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED")), meanrel, fill=  factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED"))))+
  geom_bar(stat="identity")+
  ggtitle("Last 50 years - rel. damage")+
  geom_errorbar(aes(ymin=minrel, ymax=maxrel), width=.5,) +
  facet_grid(~rcp)+
  labs(x = "Management",y="Mean dam.vol. rel. to mean landscape vol. %",fill = "Management")+
  #scale_fill_manual(values=c("#4897D8","#FFDB5C","#FA6E59"), labels=c("HistClim","RCP4.5","RCP8.5"))+  
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



library(gridExtra)
grid.arrange(g7,g8, g9, g10)




# SHannon need to be handled
# some missing simulations?







# MULTI FUNC
#plot all annnual mf values.

colnames(MF.all)

variable_names <- c(
  `nat.mortality.volperha` = "MORT",
  `impact` = "WIND+BB",
  `NEP` = "NEP",
  `standing.volume.decidious` = "BroadlVOL",
  `BA.decidious` = "BroadlBA",
  `deadwood.c.ag` = "DEADWOOD",
  `carbonstock` = "TOTALC",
  `shannon_BA_landscape` = "ShannonBA",
  `shannon_VOL_landscape` = "ShannonVOL",
  `annual.harvest` = "HARVEST",
  `annual.increment` = "INCREMENT",
  `standing.volume` = "VOLUME",
  `LAI` = "LAI", 
  `rcp45` = "RCP4.5",
  `rcp85` = "RCP8.5",
  `EC-EARTH_RACMO22E-r1`="EC-EARTH_RACMO22E-r1",
  `HadGEM2_CCLM`="HadGEM2_CCLM",
  `refclim`="refclim",
  `MPI_CCLM`="MPI_CCLM",
  `NCC_HIRHAM5`='NCC_HIRHAM5',
  `-`="-",
  
  `w5` = "w5",
  `w6` = "w6", 
  `w7` = "w7", 
  `w8` = "w8",
  `w9` = "w9"

  
)



# STARTING PLOTS
for (jj in 5:9) {
  
  w<-paste0("w",jj)

MF.all1<-MF.all %>% filter(windcase==w)



MF_long1<-pivot_longer(MF.all1,cols=c(colnames(MF.all1)[2:8]))
MF_long2<-pivot_longer(MF.all1,cols=c(colnames(MF.all1)[9:14]))


g11<-ggplot(MF_long1,aes(year,value, color=mgm, ))+
  geom_line(lwd=0.8)+
  scale_color_manual(values=(c("#4897D8","black","#FA6E59","#FFDB5C","limegreen" )))+
  facet_grid(name~rcp+model+windcase, scales = "free",labeller = as_labeller(variable_names))+  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

g12<-ggplot(MF_long2,aes(year,value, color=mgm))+
  geom_line(lwd=0.8)+
  facet_grid(name~rcp+model+windcase, scales = "free",labeller = as_labeller(variable_names))+  theme_bw()+
  scale_color_manual(values=(c("#4897D8","black","#FA6E59","#FFDB5C","limegreen" )))+#, labels=c("salvaged","disturbed","harvested","natural mort."))+         
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#last50 years avg

print(g11)
print(g12)

}
library(scales)


# HERE TO SET WHAT TO USE!!!!!!!!!!!!!!!!!!!

#----------------------------A
# LAST 50y
if (text=="last50y") {
MF.last50yavg<-MF.all %>% group_by(rcp,model,mgm, windcase) %>% filter(year>49) %>% summarise_all(mean) %>% select(-year)
MF.last50yavg.long<-data.frame(pivot_longer(MF.last50yavg,cols=c(colnames(MF.last50yavg)[5:17])))
tab<-MF.last50yavg.long
rescale.alter<-MF.last50yavg.long %>% group_by(rcp,model,name, windcase) %>% reframe(value.normalized=rescale(value,to=c(0,1)),value.standardized=scale(value), mgm=mgm)
}

#----------------------------B
# 30y around the big disturbance
#y36-65
if (text=="mid30y") {
MF.mid30yavg<-MF.all %>% group_by(rcp,model,mgm, windcase) %>% filter(year>36&year<65) %>% summarise_all(mean) %>% select(-year)
MF.mid30yavg.long<-data.frame(pivot_longer(MF.mid30yavg,cols=c(colnames(MF.mid30yavg)[5:17])))
tab<-MF.mid30yavg.long
rescale.alter<-MF.mid30yavg.long %>% group_by(rcp,model,name, windcase) %>% reframe(value.abs=value, value.normalized=rescale(value,to=c(0,1)),value.standardized=scale(value), mgm=mgm)





              
globalNEPmin<-data.frame(MF.mid30yavg.long %>% filter(name=="NEP") %>% summarise(min=min(value)))$min
print(paste0("NEP minimum across all simulations: ", globalNEPmin))


MF.mid30yavg.long<-MF.mid30yavg.long %>% mutate(minfornorm= case_when(name=="NEP"   ~ globalNEPmin,
                                                name!="NEP" ~0))


rescale.alter<-MF.mid30yavg.long %>% group_by(rcp,model,name, windcase) %>% mutate(value.normalized1 = (value - minfornorm) / (max(value) - minfornorm), value.normalized2=rescale(value,to=c(0,1))) %>% ungroup()
head(rescale.alter)



vars<-c("NEP","carbonstock","LAI","standing.volume","annual.increment","annual.harvest","shannon_BA_landscape","deadwood.c.ag")


t1<-rescale.alter %>% filter(rcp=="rcp85"& model=="HadGEM2_CCLM" &windcase=="w5"& name %in% vars)


g1<-ggplot(t1, aes(mgm,value, fill=mgm)) +
  ggtitle(paste0(text," absolute"))+
  geom_bar(  stat="identity")+
  #geom_errorbar( aes(x=mgm, ymin=vmin, ymax=vmax), position = "dodge",width = 0.3)+
  facet_wrap(~name, scales="free_y",labeller = as_labeller(variable_names), nrow=4)+
  scale_fill_manual(values=(c("#4897D8","black","#FA6E59","#FFDB5C","limegreen" )))+         
  #scale_pattern_manual(values=c('stripe', 'none')) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom",legend.text=element_text(size=6),legend.key.size = unit(0.5, 'cm'),legend.title = element_text(size=6))


g2<-ggplot(t1, aes(mgm,value.normalized1, fill=mgm)) +
  ggtitle(paste0(text,"Normalized with the new method"))+
  geom_bar(  stat="identity")+
  #geom_errorbar( aes(x=mgm, ymin=vmin, ymax=vmax), position = "dodge",width = 0.3)+
  facet_wrap(~name, scales="free_y",labeller = as_labeller(variable_names), nrow=4)+
  scale_fill_manual(values=(c("#4897D8","black","#FA6E59","#FFDB5C","limegreen" )))+         
  #scale_pattern_manual(values=c('stripe', 'none')) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom",legend.text=element_text(size=6),legend.key.size = unit(0.5, 'cm'),legend.title = element_text(size=6))
g3<-ggplot(t1, aes(mgm,value.normalized2, fill=mgm)) +
  ggtitle(paste0(text,"Normalized to 0-1 method"))+
  geom_bar(  stat="identity")+
  #geom_errorbar( aes(x=mgm, ymin=vmin, ymax=vmax), position = "dodge",width = 0.3)+
  facet_wrap(~name, scales="free_y",labeller = as_labeller(variable_names), nrow=4)+
  scale_fill_manual(values=(c("#4897D8","black","#FA6E59","#FFDB5C","limegreen" )))+         
  #scale_pattern_manual(values=c('stripe', 'none')) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom",legend.text=element_text(size=6),legend.key.size = unit(0.5, 'cm'),legend.title = element_text(size=6))


grid.arrange(g1,g2,g3, ncol=3)





}


vars<-c("NEP","carbonstock","LAI","standing.volume","annual.increment","annual.harvest","shannon_BA_landscape","deadwood.c.ag")


tab<-rescale.alter %>% filter(name %in% vars) %>% group_by(name,rcp, mgm) %>% summarise(v=mean(value), vmin=min(value), vmax=max(value),
                                                    vn1=mean(value.normalized1),vn1min=min(value.normalized1), vn1max=max(value.normalized1),
                                                    vn2=mean(value.normalized2),vn2min=min(value.normalized2), vn2max=max(value.normalized2))



g13<-ggplot(tab, aes(mgm,v, fill=mgm)) +
  ggtitle(paste0(text," absolute"))+
  geom_bar(  stat="identity")+
  geom_errorbar( aes(x=mgm, ymin=vmin, ymax=vmax), position = "dodge",width = 0.3)+
  facet_grid(name~rcp, scales="free_y",labeller = as_labeller(variable_names))+
  scale_fill_manual(values=(c("#4897D8","black","#FA6E59","#FFDB5C","limegreen" )))+         
  #scale_pattern_manual(values=c('stripe', 'none')) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom",legend.text=element_text(size=6),legend.key.size = unit(0.5, 'cm'),legend.title = element_text(size=6))


g14<-ggplot(tab, aes(mgm,vn1, fill=mgm)) +
  ggtitle(paste0(text,"normalized (min: 0, for NEP: glob min)"))+
  geom_bar(  stat="identity")+
  geom_errorbar( aes(x=mgm, ymin=vn1min, ymax=vn1max), position = "dodge",width = 0.3)+
  facet_grid(name~rcp, scales="free_y",labeller = as_labeller(variable_names))+
  scale_fill_manual(values=(c("#4897D8","black","#FA6E59","#FFDB5C","limegreen" )))+         
  #scale_pattern_manual(values=c('stripe', 'none')) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom",legend.text=element_text(size=6),legend.key.size = unit(0.5, 'cm'),legend.title = element_text(size=6))



g15<-ggplot(tab, aes(mgm,vn2, fill=mgm)) +
  ggtitle(paste0(text," normalized"))+
  geom_bar(  stat="identity")+
  geom_errorbar( aes(x=mgm, ymin=vn2min, ymax=vn2max), position = "dodge",width = 0.3)+
  facet_grid(name~rcp, scales="free_y",labeller = as_labeller(variable_names))+
  scale_fill_manual(values=(c("#4897D8","black","#FA6E59","#FFDB5C","limegreen" )))+         
  #scale_pattern_manual(values=c('stripe', 'none')) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom",legend.text=element_text(size=6),legend.key.size = unit(0.5, 'cm'),legend.title = element_text(size=6))
grid.arrange(g13,g14, g15,ncol=3)







# Want to have the 4 indicators as the avg of different measueres



fintab.abs<-rescale.alter %>% select(rcp,model,mgm,windcase, name, value)
fintab.abs.wide<-fintab.abs %>% pivot_wider(names_from = name,values_from ="value")

#I Want the normalized values
fintab.norm<-rescale.alter %>% select(rcp,model,mgm,windcase, name, value.normalized1)
fintab.norm.wide<-fintab.norm %>% pivot_wider(names_from = name,values_from ="value.normalized1")




write.csv(fintab.abs.wide, paste0(dataroot,"generated_multi-functionality_tables/",date,"_",version,"_Multifuctionality_",text,"_absolute_values.csv"), row.names = FALSE)
write.csv(fintab.norm.wide, paste0(dataroot,"generated_multi-functionality_tables/",date,"_",version,"_Multifuctionality_",text,"_normalized_values_newmethod.csv"), row.names = FALSE)


library(ggiraphExtra)


# ABSOLUTE:
# makes no sense

# NORMALIZEd, new method (number 1 above):
 fintab.norm<-data.frame( fintab.norm.wide %>% group_by(rcp,model,mgm, windcase) %>% mutate(Climate=sum(NEP,carbonstock)/2.,
                        Water=sum(LAI,standing.volume)/2., 
                        Production=sum(annual.increment,annual.harvest)/2., 
                        Biodiversity=sum(shannon_BA_landscape,deadwood.c.ag)/2.) )
 
 fintab.norm<-fintab.norm %>% select("rcp","model","mgm","windcase","Climate","Production","Water","Biodiversity")
 pl<-fintab.norm %>% group_by(rcp, mgm) %>% summarise_all(mean) %>% select(-model, -windcase)
 
 g16<-ggRadar(data=pl,mapping = aes(colour = mgm, facet=rcp), 
         rescale = FALSE, interactive = FALSE, use.label = TRUE, size = 2,alpha=0.1,
         legend.position = "right", scales="free") +theme_bw()+ylim(0,1)+
   ggtitle("Values normalized with the new method")+
   scale_fill_manual(values=(c("#4897D8","black","#FA6E59","#FFDB5C","limegreen" )))+
   scale_color_manual(values=(c("#4897D8","black","#FA6E59","#FFDB5C","limegreen" )))
 


# 

score.norm<-fintab.norm %>% mutate(score=(Climate+Production+Water+Biodiversity))

long<-pivot_longer(score.norm,cols=c("Climate","Production","Water","Biodiversity"))
longstats<-long %>% group_by(rcp,mgm, name) %>% summarise(s=mean(value), smin=min(value), smax=max(value))

g17<-ggplot(longstats, aes(mgm,s, fill=mgm)) +
  ggtitle(paste0(text," MF score"))+
  scale_fill_manual(values=(c("#4897D8","black","#FA6E59","#FFDB5C","limegreen" )))+
  geom_bar(  stat="identity")+
  geom_errorbar( aes(x=mgm, ymin=smin, ymax=smax), position = "dodge",width = 0.3)+
  facet_grid(name~rcp)+  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))

# I Want a barplot with ranges
# and I want a barplot per management
MFs<-score.norm %>% select(rcp,model,mgm,windcase,score) %>% group_by(rcp,mgm) %>% summarise(s=mean(score), smin=min(score), smax=max(score))


  g18<-ggplot(MFs, aes(mgm,s, fill=mgm)) +
  ggtitle(paste0(text," MF score"))+
    scale_fill_manual(values=(c("#4897D8","black","#FA6E59","#FFDB5C","limegreen" )))+
  geom_bar(  stat="identity")+
  geom_errorbar( aes(x=mgm, ymin=smin, ymax=smax), position = "dodge",width = 0.3)+
  facet_wrap(~rcp)+  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))
  
  
  
grid.arrange(g16, g17,g18, ncol = 2, nrow = 2,layout_matrix= rbind(c(1,2), c(3,2)))





dev.off()

#