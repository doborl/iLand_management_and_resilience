


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





version<-"DISASTER2"
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





# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
pdf(paste0(dataroot, "plots/",date,"_",version,"_",text,"__LD_for_print.pdf"), height=8.3, width=11.7 )

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


w<-"w5"
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


# CUMULATIVE DAMAGES:

dam<-damage.all %>% group_by(rcp, model, mgm, windcase) %>% mutate(csimp=cumsum(impact), csrelimp=cumsum(relimpact))

g6<-ggplot(dam,aes(x=year,y=csimp, col=mgm))+
  geom_step(size=0.9)+ 
  facet_grid(windcase~rcp+model)+
  ggtitle("Cumulative damages m3/ha")+
  scale_color_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+  
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

g7<-ggplot(dam.summary2, aes( factor(mgm, levels=c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED")), sum_total2, fill=  factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED"))))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=min, ymax=max), width=.5,) +
  facet_grid(~rcp)+
  ggtitle("Whole period - absolute damage")+
  labs(x = "Management",y="Total damaged volume m3/ha",fill = "Management")+
  scale_fill_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+   
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

g8<-ggplot(dam.summary2, aes( factor(mgm, levels=c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED")), meanrel, fill=  factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED"))))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=minrel, ymax=maxrel), width=.5,) +
  facet_wrap(~rcp)+
  ggtitle("Whole period - rel. damage ")+
  labs(x = "Management",y="Mean dam.vol. rel. to mean landscape vol. %",fill = "Management")+
  scale_fill_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+ 
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



g9<-ggplot(dam.summary2, aes( factor(mgm, levels=c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED")), sum_total2, fill=  factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED"))))+
  geom_bar(stat="identity")+
  ggtitle("Last 50 years -  absolute damage")+
  geom_errorbar(aes(ymin=min, ymax=max), width=.5,) +
  facet_grid(~rcp)+
  labs(x = "Management",y="Total damaged volume m3/ha",fill = "Management")+
  scale_fill_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+   
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

g10<-ggplot(dam.summary2, aes( factor(mgm, levels=c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED")), meanrel, fill=  factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED"))))+
  geom_bar(stat="identity")+
  ggtitle("Last 50 years - rel. damage")+
  geom_errorbar(aes(ymin=minrel, ymax=maxrel), width=.5,) +
  facet_grid(~rcp)+
  labs(x = "Management",y="Mean dam.vol. rel. to mean landscape vol. %",fill = "Management")+
  scale_fill_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+ 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



library(gridExtra)
grid.arrange(g7,g8, g9, g10)







dev.off()

#