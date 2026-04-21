#----------------------------------------------------------------------------------2025/2026
# Laura Dobor, CZU, dobor@fld.czu.cz
# Study: iLand modeling management and resilience on Kostelec area
# 2026.04.20.
#
#     ANALYSES AND VISUALIZATION
#
#
#----------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)   





setwd("D:/___PROJECTS/2025_iLand_management_study/04_work/3_analyses/")

dataroot<-"Output_summary_tables/"
plotroot<-"Figures/"


version<-"DISASTER2"
date<-"2025-04-18"
damage.all<-read.csv(paste0(dataroot,date,"_damages_DISASTER2.csv"))
recovery.all<-read.csv( paste0(dataroot,date,"_recovery_DISASTER2.csv"))




# We look the 10 year period after the large disturbances:


d<-damage.all %>% filter(year>49& year<60)


# I want to have a plot where models and winds are there as a range for each mgm

#dsum<-d %>% group_by(mgm,year,model, wind,rcp) %>% summarise(mean.wind=mean(wind), min.wind=min(wind), max.wind=max(wind),
#                                                  mean.bb=mean(barkbeetle), min.bb=min(barkbeetle), max.bb=max(barkbeetle))



# I want to have a plot with total damage  for each case
# First I summarize the damaged volume for each simulation run for the 10 year period which we filtered:

s<-d %>% group_by(mgm,model,windcase, rcp) %>% summarise(wind=sum(wind),bb=sum(barkbeetle)) %>% mutate(total=wind+bb)


# I need to make difference tables.
 
ref.s<-s %>% filter(model=="refclim") %>% rename(windref=wind,bbref=bb,totalref=total) %>%  ungroup() %>% select(-rcp)

ref.s<-ref.s %>% select(-model)



diff<-left_join(s,ref.s,by=c("mgm","windcase")) %>% mutate(diff.wind=100*(wind-windref)/windref,
                                                           diff.bb=100*(bb-bbref)/bbref,
                                                           diff.total=100*(total-totalref)/totalref)


diff2<-diff %>% select(-wind,-bb,-total,-windref,-bbref,-totalref)

diff3<-diff2 %>% group_by(mgm,rcp) %>% summarise(mean.wind=mean(diff.wind), min.wind=min(diff.wind), max.wind=max(diff.wind),q1.wind=quantile(diff.wind,0.25),q2.wind=quantile(diff.wind,0.75),
                                               mean.bb=mean(diff.bb), min.bb=min(diff.bb), max.bb=max(diff.bb),q1.bb=quantile(diff.bb,0.25),q2.bb=quantile(diff.bb,0.75),
                                               mean.total=mean(diff.total), min.total=min(diff.total), max.total=max(diff.total),q1.total=quantile(diff.total,0.25),q2.total=quantile(diff.total,0.75))




diff3<-diff3 %>% filter(rcp!="-")

# QANTILES?????

diff_long <- diff3 %>%
  pivot_longer(
    cols = -c(mgm, rcp),
    names_to = c(".value", "variable"),
    names_sep = "\\."
  )

diff_long <- diff_long %>%
  mutate(variable = factor(variable,
                           levels = c("wind", "bb", "total")))

g1<-ggplot(diff_long, aes(mgm,mean, fill=rcp)) +
  ggtitle("Expected change in disturbed volume ( %, with interquartile range)")+
  scale_fill_manual(values=(c( "#5fad56", "#f2c14e" )))+
  geom_bar(  stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin = q1, ymax = q2),
                position = position_dodge(width = 0.9),
                width = 0.2,   # width of the whisker caps
                color = "black")+
  facet_wrap(~variable, ncol=3)+
  theme_bw()+ylab("% difference compared to reference climate case")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))

g2<-ggplot(diff_long, aes(mgm,mean, fill=variable)) +
  ggtitle("Expected change in disturbed volume ( %, with interquartile range)")+
  scale_fill_manual(values=(c( "#5fad56", "#f2c14e","tomato" )))+
  geom_bar(  stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin = q1, ymax = q2),
                position = position_dodge(width = 0.9),
                width = 0.2,   # width of the whisker caps
                color = "black")+
  facet_wrap(~rcp, ncol=3)+
  theme_bw()+ylab("% difference compared to reference climate case")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))



pdf(paste0(plotroot, "2e_Climate_change_effect_on_damaged_volume_per_agent.pdf"),height = 6, width = 10)
print(g1)

dev.off()



pdf(paste0(plotroot, "2e_Climate_change_effect_on_damaged_volume_per_rcp.pdf"),height = 6, width = 10)
print(g2)

dev.off()
