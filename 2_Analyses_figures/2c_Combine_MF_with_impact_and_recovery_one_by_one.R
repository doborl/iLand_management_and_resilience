#----------------------------------------------------------------------------------2025/2026
# Laura Dobor, CZU, dobor@fld.czu.cz
# Study: iLand modeling management and resilience on Kostelec area
# 2026.04.15.
#
#     ANALYSES AND VISUALIZATION
#
#
#----------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)   
library(dplyr)




setwd("D:/___PROJECTS/2025_iLand_management_study/04_work/3_analyses/")


dataroot<-"Output_summary_tables/"
plotroot<-"Figures/"



version<-"DISASTER2"
date<-"2025-04-18"


# READ DATA: --------------------------------
damage.all<-read.csv(paste0(dataroot,date,"_damages_DISASTER2.csv"))
recovery.all<-read.csv( paste0(dataroot,date,"_recovery_DISASTER2.csv"))
 

MF<-read.csv(paste0(dataroot,"generated_multi-functionality_tables/20260415_MF_ES_score.csv"))



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



score<-MF


# combine the score with the total damage:

lnd.vol.y50<-d %>% filter(year==50) %>% dplyr::select(mgm, model, windcase, rcp, landscape_volume_yearstart)
to_plot<-left_join(s,score, by=c("mgm", "model", "windcase", "rcp"))
to_plot<-left_join(to_plot,lnd.vol.y50, by=c("mgm", "model", "windcase", "rcp"))

to_plot<-left_join(to_plot,df_endpoints, by=c("mgm", "model", "windcase", "rcp"))




#----------chatgpt

# I need common scale and other order and remove the titles
rcp_levels <- unique(to_plot$rcp)[c(3,1,2)]


shared_xlim <- range(to_plot$score, na.rm = TRUE)

shared_ylim <- range(100 * to_plot$total / to_plot$landscape_volume_yearstart, na.rm = TRUE)

# List to store plots
combined_plots <- list()

for (rcp_val in rcp_levels) {
  df <- to_plot %>% filter(rcp == rcp_val)
  
  # Prepare main plot
  p_main_i <- ggplot(df ,aes(score, 100*total/landscape_volume_yearstart)) +
    geom_point(aes(color = factor(mgm, levels=c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED"))), size = 2) +
    geom_smooth(method = "lm") +
    labs(x = "MF score", y = "Relative impact %", col = "mgm") +
    scale_color_manual(values = c("#f2c14e", "chocolate", "black", "#62d75f", "#248721")) +
    theme_bw() +
    scale_x_continuous(limits = shared_xlim)+
    scale_y_continuous(limits = shared_ylim)+
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "white")) 
  
  # Top range plot
  x_ranges_i <- df %>%
    group_by(mgm) %>%
    summarise(xmin = min(score), xmax = max(score), xmean = mean(score), xmedian = median(score), .groups = "drop")
  
  p_top_i <- ggplot(x_ranges_i, aes(y = mgm, xmin = xmin, xmax = xmax, color = mgm)) +
    geom_errorbarh(height = 0.5, lwd=0.5) +
    geom_point(aes(x = xmean)) +
    geom_point(aes(x = xmedian), pch = 2) +
    scale_color_manual(values = c("#f2c14e", "chocolate", "black", "#62d75f", "#248721")) +
    theme_minimal() +
    scale_x_continuous(limits = shared_xlim)+
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),      # remove y labels
      axis.ticks.y = element_blank(),     # remove y ticks
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA)  # box around panel
    )
  
  # Right range plot
  y_ranges_i <- df %>%
    group_by(mgm) %>%
    summarise(ymin = min(100*total/landscape_volume_yearstart),
              ymax = max(100*total/landscape_volume_yearstart),
              ymean = mean(100*total/landscape_volume_yearstart),
              ymedian = median(100*total/landscape_volume_yearstart),
              .groups = "drop")
  
  p_right_i <- ggplot(y_ranges_i, aes(x = mgm, ymin = ymin, ymax = ymax, color = mgm)) +
    geom_errorbar(width = 0.5, lwd=0.5) +  # narrower
    geom_point(aes(y = ymean)) +
    geom_point(aes(y = ymedian), pch = 2) +
    scale_color_manual(values = c("#f2c14e", "chocolate", "black", "#62d75f", "#248721")) +
    scale_y_continuous(limits = shared_ylim)+
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),      # remove y labels
      axis.ticks.y = element_blank(),     # remove y ticks
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA)  # box around panel
    )
  
  if  (rcp_val!="rcp45") p_main_i <- p_main_i + theme(axis.title.x = element_blank(),legend.position = "none")
  if  (rcp_val!="-") p_main_i <- p_main_i + theme(axis.title.y = element_blank())
  
  
  p_top_i_clean <- p_top_i + 
    theme(plot.margin = margin(0, 0, 0, 0))
  
  p_main_i_clean <- p_main_i + 
    theme(plot.margin = margin(0, 0, 0, 0))
  
  p_right_i_clean <- p_right_i + 
    theme(plot.margin = margin(0, 0, 0, 0))
  
  # Row with no space between them
  main_row <- p_main_i_clean + p_right_i_clean + 
    plot_layout(widths = c(6, 1)) & 
    theme(plot.margin = margin(0, 3, 0, 0))  # just in case
  
  # Do the same for the top row if needed
  top_row <- p_top_i_clean + plot_spacer() + 
    plot_layout(widths = c(6, 1)) & 
    theme(plot.margin = margin(0, 0, 0, 0))
  
  # Combine vertically with no padding
  combined <- top_row / main_row + 
    plot_layout(heights = c(1, 8)) & 
    theme(plot.margin = margin(0, 3, 0, 0))

  combined_plots[[rcp_val]] <- combined
}



final_plot <- wrap_plots(combined_plots, ncol = 3) 



pdf(paste0(plotroot, "2c_Relative_impact_vs_MF.pdf"),height = 6, width = 10)
print(final_plot)
  
dev.off()



to_plot$rt[which(is.na(to_plot$rt)==T)] <-50

#-----------other graph:



shared_xlim <- range(to_plot$score, na.rm = TRUE)

shared_ylim <- range(to_plot$rt, na.rm = TRUE)

# List to store plots
combined_plots <- list()

for (rcp_val in rcp_levels) {
  df <- to_plot %>% filter(rcp == rcp_val)
  
  # Prepare main plot
  p_main_i <- ggplot(df ,aes(score, rt)) +
    geom_point(aes(color = factor(mgm, levels=c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED"))), size = 2) +
    geom_smooth(method = "lm") +
    labs(x = "MF score", y = "Recovery time [years]", col = "mgm") +
    scale_color_manual(values = c("#f2c14e", "chocolate", "black", "#62d75f", "#248721")) +
    theme_bw() +
    scale_x_continuous(limits = shared_xlim)+
    scale_y_continuous(limits = shared_ylim)+
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "white")) 
  
  # Top range plot
  x_ranges_i <- df %>%
    group_by(mgm) %>%
    summarise(xmin = min(score), xmax = max(score), xmean = mean(score), xmedian = median(score), .groups = "drop")
  
  p_top_i <- ggplot(x_ranges_i, aes(y = mgm, xmin = xmin, xmax = xmax, color = mgm)) +
    geom_errorbarh(height = 0.5, lwd=0.5) +
    geom_point(aes(x = xmean)) +
    geom_point(aes(x = xmedian), pch = 2) +
    scale_color_manual(values = c("#f2c14e", "chocolate", "black", "#62d75f", "#248721")) +
    theme_minimal() +
    scale_x_continuous(limits = shared_xlim)+
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),      # remove y labels
      axis.ticks.y = element_blank(),     # remove y ticks
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA)  # box around panel
    )
  
  # Right range plot
  y_ranges_i <- df %>%
    group_by(mgm) %>%
    summarise(ymin = min(rt),
              ymax = max(rt),
              ymean = mean(rt),
              ymedian = median(rt),
              .groups = "drop")
  
  p_right_i <- ggplot(y_ranges_i, aes(x = mgm, ymin = ymin, ymax = ymax, color = mgm)) +
    geom_errorbar(width = 0.5, lwd=0.5) +  # narrower
    geom_point(aes(y = ymean)) +
    geom_point(aes(y = ymedian), pch = 2) +
    scale_color_manual(values = c("#f2c14e", "chocolate", "black", "#62d75f", "#248721")) +
    scale_y_continuous(limits = shared_ylim)+
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),      # remove y labels
      axis.ticks.y = element_blank(),     # remove y ticks
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA)  # box around panel
    )
  
  if  (rcp_val!="rcp45") p_main_i <- p_main_i + theme(axis.title.x = element_blank(),legend.position = "none")
  if  (rcp_val!="-") p_main_i <- p_main_i + theme(axis.title.y = element_blank())
  
  
  p_top_i_clean <- p_top_i + 
    theme(plot.margin = margin(0, 0, 0, 0))
  
  p_main_i_clean <- p_main_i + 
    theme(plot.margin = margin(0, 0, 0, 0))
  
  p_right_i_clean <- p_right_i + 
    theme(plot.margin = margin(0, 0, 0, 0))
  
  # Row with no space between them
  main_row <- p_main_i_clean + p_right_i_clean + 
    plot_layout(widths = c(6, 1)) & 
    theme(plot.margin = margin(0, 3, 0, 0))  # just in case
  
  # Do the same for the top row if needed
  top_row <- p_top_i_clean + plot_spacer() + 
    plot_layout(widths = c(6, 1)) & 
    theme(plot.margin = margin(0, 0, 0, 0))
  
  # Combine vertically with no padding
  combined <- top_row / main_row + 
    plot_layout(heights = c(1, 8)) & 
    theme(plot.margin = margin(0, 3, 0, 0))
  
  combined_plots[[rcp_val]] <- combined
}



final_plot <- wrap_plots(combined_plots, ncol = 3) 


final_plot



pdf(paste0(plotroot, "2c_Recovery_time_vs_MF.pdf"),height = 6, width = 10)
print(final_plot)

dev.off()







