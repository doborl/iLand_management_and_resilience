


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

# Inputs I need:
#
#
#  damage values for the disaster2 case :

date<-"2025-04-18"
damage.all<-read.csv(paste0(dataroot,date,"_damages_DISASTER2.csv"))

d<-damage.all %>% filter(year>49& year<60)


outroot<-"D:/___PROJECTS/2025_iLand_management_study/04_work/analyses/Output_summary_tables/plots/"

pdf(paste0(outroot, date,"disaster2_analyses_of_the_disaster.pdf"), width=8, height=8)

area<-lnd$area[1]
ylim.bb <- c(0, 40)                                                                                          
ylim.w <- c(0, 100) 

# This is needed to mantain the proportion in the two axis
b <- diff(ylim.w)/diff(ylim.bb)
a <- ylim.w[1] - b*ylim.bb[1] 

# w<-"w5"
# # TO MAKE 2 LINE AND 2 DIFFERENT SCALE PLOT "https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales"
# ggplot((d %>% filter(windcase==w)),aes(year,100*wind/landscape_volume_yearstart))+
#   geom_col(fill="grey",col="black")+
#   geom_line(aes(y = a+ 100*barkbeetle/landscape_volume_yearstart*b), data = (d %>% filter(windcase==w)), size=0.9, col="pink") +
#   scale_y_continuous(name="Wind relative damage %", sec.axis = sec_axis(~ (. - a)/b,name = "Bark beetle relative damage %"))+
#   facet_grid(mgm+windcase~rcp+model)+
#   ggtitle("Relative damages compared to pre-dist volume %")+
#   theme_bw()+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.background =element_rect(fill="white"))





# I want to have a plot where models and winds are there as a range for each mgm

dsum<-d %>% group_by(mgm,year, rcp) %>% summarise(mean.wind=mean(wind), min.wind=min(wind), max.wind=max(wind),
                mean.bb=mean(barkbeetle), min.bb=min(barkbeetle), max.bb=max(barkbeetle))

ylim.bb <- c(0, 40)                                                                                          # in this example, precipitation look the link down
ylim.w <- c(0, 150) 

# This is needed to mantain the proportion in the two axis
b <- diff(ylim.w)/diff(ylim.bb)
a <- ylim.w[1] - b*ylim.bb[1] 

ggplot(dsum ,aes(year,mean.wind))+
  geom_col(fill="skyblue",col="black")+
  geom_errorbar(aes(ymin=min.wind, ymax=max.wind), width=.4,) +
  facet_grid(mgm~rcp)+
  geom_line(aes(y = a+ mean.bb*b), data = dsum, size=0.9, col="tomato")+ 
  geom_ribbon(data = dsum, alpha = 0.4, aes(ymin = a+min.bb*b, ymax = a+max.bb*b), fill="pink") +
  scale_y_continuous(name="Wind damage m3/ha", sec.axis = sec_axis(~ (. - a)/b,name = "Bark beetle damage m3/ha"))+
  ggtitle("Absolute damages")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))


# I want to have a plot with total damage  for each case
# First I summarize the damaged volume for each simulation run for the 10 year period which we filtered:

s<-d %>% group_by(mgm,model,windcase, rcp) %>% summarise(wind=sum(wind),bb=sum(barkbeetle)) %>% mutate(total=wind+bb)

# Then I calculate model and windcases minimums, maximums and averages:
smean<-s %>% group_by(mgm, rcp) %>% summarise(mean.wind=mean(wind), min.wind=min(wind), max.wind=max(wind),
                                                  mean.bb=mean(bb), min.bb=min(bb), max.bb=max(bb),
                                              mean.total=mean(total), min.total=min(total), max.total=max(total) )

# I want 3 plots, one for wind, one for bb and one for total, with the wind-model ranges

g1<-ggplot(smean ,aes( factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED")),
                   mean.wind, 
                   fill=  factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED"))))+
  geom_col()+
  labs(x = "Management",y="Sum of 10 years wind damage m3/ha",fill = "Management")+
  geom_errorbar(aes(ymin=min.wind, ymax=max.wind), width=.4,) +
   facet_grid(~rcp)+
  ggtitle("Wind damages")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        strip.background =element_rect(fill="white"))

g2<-ggplot(smean ,aes( factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED")),
                   mean.bb, 
                   fill=  factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED"))))+
  geom_col()+
  labs(x = "Management",y="Sum of 10 years bb damage m3/ha",fill = "Management")+
  geom_errorbar(aes(ymin=min.bb, ymax=max.bb), width=.4,) +
  facet_grid(~rcp)+
  ggtitle("BB damages")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        strip.background =element_rect(fill="white"))

g3<-ggplot(smean ,aes( factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED")),
                   mean.total, 
                   fill=  factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED"))))+
  geom_col()+
  labs(x = "Management",y="Sum of 10 years bb+wind damage m3/ha",fill = "Management")+
  geom_errorbar(aes(ymin=min.total, ymax=max.total), width=.4,) +
  facet_grid(~rcp)+
  ggtitle("Wind+bb damages")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        axis.text.x=element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))

grid.arrange(g1,g2,g3)

# how to translate these values to relative damage?
# compare it to pre.dist value??


lnd.vol<-d %>% filter(year==50) %>% group_by(mgm,rcp) %>% summarise(meanvol=mean(landscape_volume_yearstart), minvol=min(landscape_volume_yearstart),
                                                             maxvol=max(landscape_volume_yearstart))


lnd.vol.y50<-d %>% filter(year==50) %>% select(mgm, model, windcase, rcp, landscape_volume_yearstart)

g4<-ggplot(lnd.vol ,aes( factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED")),
                         meanvol, 
                       fill=  factor(mgm, levels=c("ADAPTATION","BIOECONOMY","BAU","CONSERVATION", "UNMANAGED"))))+
  geom_col()+
  labs(x = "Management",y="Landscape volume m3/ha",fill = "Management")+
  geom_errorbar(aes(ymin=minvol, ymax=maxvol), width=.4,) +
  facet_grid(~rcp)+
  ggtitle("Landscape volume in y50")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        axis.text.x=element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))



grid.arrange(g3,g4)




# Read in the multifunctionality based on the no-disaster casE:
MF.root<-"D:/___PROJECTS/2025_iLand_management_study/04_work/analyses/Output_summary_tables/generated_multi-functionality_tables/"
MF<-read.csv(paste0(MF.root,"2025-04-19_NO_DISASTER_Multifuctionality_mid30y_normalized_values_newmethod.csv"))

MF2<-data.frame( MF %>% group_by(rcp,model,mgm, windcase) %>% mutate(Climate=sum(NEP,carbonstock)/2.,
                                                                                           Water=sum(LAI,standing.volume)/2., 
                                                                                           Production=sum(annual.increment,annual.harvest)/2., 
                                                                                           Biodiversity=sum(shannon_BA_landscape,deadwood.c.ag)/2.) )

MF2<-MF2 %>% select("rcp","model","mgm","windcase","Climate","Production","Water","Biodiversity")


score<-MF2 %>% mutate(score=(Climate+Production+Water+Biodiversity)) %>% select(rcp,model,mgm,windcase,score)


# combine the score with the total damage:

to_plot<-left_join(s,score, by=c("mgm", "model", "windcase", "rcp"))
to_plot<-left_join(to_plot,lnd.vol.y50, by=c("mgm", "model", "windcase", "rcp"))

g4<-ggplot(to_plot ,aes( score,   total   ))+
  geom_point( aes(color=  factor(mgm, levels=c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED"))))+
  geom_smooth(method = "lm")+
  labs(x = "MF score",y="Total disturbed volume in 10 years m3/ha", col="mgm")+
    facet_grid(~rcp)+
  ggtitle("Absolute dist.volume")+
  scale_color_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))


g5<-ggplot(to_plot ,aes( score,   100*total/landscape_volume_yearstart ))+
  geom_point( aes(color=  factor(mgm, levels=c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED"))))+
  geom_smooth(method = "lm")+
  labs(x = "MF score",y="Relative impact %", col="mgm")+
  facet_grid(~rcp)+
  ggtitle("Relative impact. total dist vol/pre dist lnd vol")+
  scale_color_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))


grid.arrange(g4,g5)
dev.off()
