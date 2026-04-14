

# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)   
library(dplyr)

version<-"DISASTER2"
dataroot<-"D:/___PROJECTS/2025_iLand_management_study/04_work/3_analyses/Output_summary_tables/"



date<-"2025-04-18"
damage.all<-read.csv(paste0(dataroot,date,"_damages_DISASTER2.csv"))
recovery.all<-read.csv( paste0(dataroot,date,"_recovery_DISASTER2.csv"))
 
MF.root<-"D:/___PROJECTS/2025_iLand_management_study/04_work/3_analyses/Output_summary_tables/generated_multi-functionality_tables/"
MF<-read.csv(paste0(MF.root,"2025-04-19_NO_DISASTER_Multifuctionality_mid30y_normalized_values_newmethod.csv"))



# NEED TO OPEN A PDF WRITER AND GIVE IT THE ROOT, THE NAME, AND THE SIZE
pdf(paste0(dataroot, "plots/EGU_part3_",date,"_",version,"_",text,"__LD.pdf"), height = 10, width = 12)


# We look the 10 year period after the large disturbances:


d<-damage.all %>% filter(year>49& year<60)


# I want to have a plot where models and winds are there as a range for each mgm

dsum<-d %>% group_by(mgm,year, rcp) %>% summarise(mean.wind=mean(wind), min.wind=min(wind), max.wind=max(wind),
                                                  mean.bb=mean(barkbeetle), min.bb=min(barkbeetle), max.bb=max(barkbeetle))


# I want to have a plot with total damage  for each case
# First I summarize the damaged volume for each simulation run for the 10 year period which we filtered:

s<-d %>% group_by(mgm,model,windcase, rcp) %>% summarise(wind=sum(wind),bb=sum(barkbeetle)) %>% mutate(total=wind+bb)

# Then I calculate model and windcases minimums, maximums and averages:
smean<-s %>% group_by(mgm, rcp) %>% summarise(mean.wind=mean(wind), 
                                              mean.bb=mean(bb)  )

smean_long<-pivot_longer(smean, cols=c("mean.wind","mean.bb"))

# how to translate these values to relative damage?
# compare it to pre.dist value??

lnd.vol<-d %>% filter(year==50) %>% group_by(mgm,rcp) %>% summarise(meanvol=mean(landscape_volume_yearstart))
smean2<-data.frame(left_join(smean,lnd.vol, by=c("mgm","rcp") ) %>% mutate(perc.wind=100*mean.wind/meanvol, perc.bb=100*mean.bb/meanvol)) %>% dplyr::select(-mean.wind, -mean.bb, -meanvol)


smean2_long<-pivot_longer(smean2, cols=c("perc.wind","perc.bb"))


# Recovery times?

head(recovery.all)
recovery.all<-recovery.all %>% mutate(recovery.time=recovery_year_volume-year, case=paste0(windcase,model))
rec<-recovery.all %>% group_by(mgm,rcp,model,windcase) %>% summarize(recovery.time=max(recovery.time))

recovery.all1<-recovery.all %>% mutate(r=case_when(  year>recovery_year_volume ~ NA,
                                      year<=recovery_year_volume ~Perc_Diff_VOL ))

df_endpoints<-recovery.all1 %>% group_by(mgm,rcp,model,windcase) %>% summarise(rt=max(recovery.time)) %>% mutate(value=0)



# VALUES FOR EGU:
df_endpoints %>%  group_by(rcp,mgm) %>% summarise(n=n())
df_endpoints %>% filter(rt=="Inf") %>% group_by(rcp,mgm) %>% summarise(n=n())

u<-df_endpoints %>% filter(rcp=="-") 
u %>% group_by(mgm) %>% summarize(mean1=mean(rt), min1=min(rt), max1=max(rt))

u<-df_endpoints %>% filter(rcp=="rcp85") 
u %>% group_by(mgm) %>% summarize(mean1=mean(rt), min1=min(rt), max1=max(rt))



df_endpoints2<-df_endpoints %>% group_by(mgm,rcp) %>% summarize(mean1=mean(rt), min1=min(rt), max1=max(rt))


df_endpoints<-df_endpoints %>% filter(rt<50)


df_endpoints %>% group_by(mgm,rcp) %>% summarize(mean=mean(rt), max=max(rt))




g5<-ggplot(rec ,aes( mgm,recovery.time , 
                             fill=mgm  ))+
  geom_col()+
  labs(x = "Management",y="Recovery time (years)",fill = "mgm")+
 # scale_fill_manual(values=(c("chocolate","grey" )))+
  facet_grid(windcase~rcp+model)+
  scale_fill_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background =element_rect(fill="white"))
print(g5)


MF2<-data.frame( MF %>% group_by(rcp,model,mgm, windcase) %>% mutate(Climate=sum(NEP,carbonstock)/2.,
                                                                     Water=sum(LAI,standing.volume)/2., 
                                                                     Production=sum(annual.increment,annual.harvest)/2., 
                                                                     Biodiversity=sum(shannon_BA_landscape,deadwood.c.ag)/2.) )

MF2<-MF2 %>% select("rcp","model","mgm","windcase","Climate","Production","Water","Biodiversity")


score<-MF2 %>% mutate(score=(Climate+Production+Water+Biodiversity)) %>% dplyr::select(rcp,model,mgm,windcase,score)


# combine the score with the total damage:

lnd.vol.y50<-d %>% filter(year==50) %>% dplyr::select(mgm, model, windcase, rcp, landscape_volume_yearstart)
to_plot<-left_join(s,score, by=c("mgm", "model", "windcase", "rcp"))
to_plot<-left_join(to_plot,lnd.vol.y50, by=c("mgm", "model", "windcase", "rcp"))

to_plot<-left_join(to_plot,df_endpoints, by=c("mgm", "model", "windcase", "rcp"))



  
p_main<-ggplot(to_plot ,aes( score,   100*total/landscape_volume_yearstart ))+
  geom_point( aes(color=  factor(mgm, levels=c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED"))))+
  geom_smooth(method = "lm")+
  labs(x = "MF score",y="Relative impact %", col="mgm")+
  facet_grid(~rcp)+
  scale_color_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+
  theme_bw()+
  theme(legend.position = "bottom",panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))




x_ranges <- to_plot %>%
  group_by(mgm, rcp) %>%
  summarise(xmin = min(score), xmax = max(score), xmean=mean(score), xmedian=median(score))

p_top <- ggplot(x_ranges, aes(y = mgm, xmin = xmin, xmax = xmax, color = mgm)) +
  scale_color_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+
  geom_errorbarh(height = 0.1) +
  geom_point(aes(y=mgm, x=xmean))+
  geom_point(aes(y=mgm, x=xmedian), pch=2)+
  theme_minimal() +
  facet_wrap(~rcp)+
  theme(  legend.position = "none",  axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank())


y_ranges <- to_plot %>%
  group_by(mgm, rcp) %>%
  summarise(ymin = min(100*total/landscape_volume_yearstart), ymax = max(100*total/landscape_volume_yearstart), ymean=mean(100*total/landscape_volume_yearstart), ymedian=median(100*total/landscape_volume_yearstart))

p_right <- ggplot(y_ranges, aes(x = mgm, ymin = ymin, ymax = ymax, color = mgm)) +
  geom_errorbar(width = 0.1) +
  scale_color_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+
  geom_point(aes(x=mgm, y=ymean))+
  geom_point(aes(x=mgm, y=ymedian), pch=2)+
  theme_minimal() +
  facet_wrap(~rcp)+

  theme(legend.position = "none",    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank())
library(patchwork)

(p_top + plot_spacer()) /
  (p_main + p_right) +
  plot_layout(widths = c(4, 1), heights = c(1, 4))





dev.off()


pdf(paste0(dataroot, "plots/EGU_part3_",date,"_",version,"__LD_1.pdf"),height = 4, width = 12)
print((p_top + plot_spacer()) /
        (p_main + p_right) +
        plot_layout(widths = c(10, 0.5), heights = c(0.5, 6)))

  
dev.off()
#


to_plot$rt[which(is.na(to_plot$rt)==T)] <-50

p_main<-ggplot(to_plot ,aes( score,   rt ))+
  geom_point( aes(color=  factor(mgm, levels=c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED"))))+
  geom_smooth(method = "lm")+
  labs(x = "MF score",y="Recovery time [years]", col="mgm")+
  facet_grid(~rcp)+

  scale_color_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+
  theme_bw()+
  theme(legend.position = "bottom",panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))

p_main


x_ranges <- to_plot %>%
  group_by(mgm, rcp) %>%
  summarise(xmin = min(score), xmax = max(score), xmean=mean(score), xmedian=median(score))

p_top <- ggplot(x_ranges, aes(y = mgm, xmin = xmin, xmax = xmax, color = mgm)) +
  scale_color_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+
  geom_errorbarh(height = 0.1) +
  theme_minimal() +
  geom_point(aes(y=mgm, x=xmean))+
  geom_point(aes(y=mgm, x=xmedian), pch=2)+
  facet_wrap(~rcp)+
  theme(  legend.position = "none",  axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank())


y_ranges <- to_plot %>%
  group_by(mgm, rcp) %>%
  summarise(ymin = min(rt), ymax = max(rt),ymean=mean(rt), ymedian=median(rt))

p_right <- ggplot(y_ranges, aes(x = mgm, ymin = ymin, ymax = ymax, color = mgm)) +
  geom_errorbar(width = 0.1) +
  scale_color_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+
  geom_point(aes(x=mgm, y=ymean))+
  geom_point(aes(x=mgm, y=ymedian), pch=2)+
  theme_minimal() +
  facet_wrap(~rcp)+
  
  theme(legend.position = "none",    axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

library(patchwork)

(p_top + plot_spacer()) /
  (p_main + p_right) +
  plot_layout(widths = c(4, 1), heights = c(1, 4))





pdf(paste0(dataroot, "plots/EGU_part3_",date,"_",version,"__LD_2.pdf"), height = 4, width = 12)
print((p_top + plot_spacer()) /
  (p_main + p_right) +
  plot_layout(widths = c(10, 0.5), heights = c(0.5, 6)))

dev.off()


p_main<-ggplot(to_plot ,aes( rcp,   rt ))+
  geom_jitter( aes(color=  factor(mgm, levels=c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED"))))+
  geom_smooth(method = "lm")+
  labs(x = "MF score",y="Recovery time [years]", col="mgm")+
  
  
  scale_color_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+
  theme_bw()+
  theme(legend.position = "bottom",panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))

p_main<-ggplot(to_plot ,aes( rcp,   rt ))+
  geom_violin( aes(fill=  factor(mgm, levels=c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED"))))+
  geom_smooth(method = "lm")+
  labs(x = "MF score",y="Recovery time [years]", col="mgm")+
  
  
  scale_fill_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+
  theme_bw()+
  theme(legend.position = "bottom",panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))

p_main

