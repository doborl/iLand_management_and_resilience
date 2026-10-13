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
library(MESS)
library(patchwork)
library(ggpubr)
library(broom)
library(tidytext)

setwd("D:/___PROJECTS/2025_iLand_management_study/04_work/3_analyses/")


dataroot<-"Output_summary_tables/"
plotroot<-"Figures/"


version<-"DISASTER2"
date<-"2025-04-18"
damage.all<-read.csv(paste0(dataroot,date,"_damages_DISASTER2.csv"))
recovery.all<-read.csv( paste0(dataroot,date,"_recovery_DISASTER2.csv"))


MF<-read.csv(paste0(dataroot,"generated_multi-functionality_tables/20260630_MF_ES_score.csv"))


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
smean2<-left_join(smean,lnd.vol, by=c("mgm","rcp") ) %>% mutate(perc.wind=100*mean.wind/meanvol, perc.bb=100*mean.bb/meanvol) %>% dplyr::select(-mean.wind, -mean.bb, -meanvol)


smean2_long<-pivot_longer(smean2, cols=c("perc.wind","perc.bb"))




#-------------------------------------------------------------------------- Recovery times?

head(recovery.all)
recovery.all<-recovery.all %>% mutate(recovery.time=recovery_year_volume-year, case=paste0(windcase,model))
rec<-recovery.all %>% group_by(mgm,rcp,model,windcase) %>% summarize(recovery.time=max(recovery.time))

recovery.all1<-recovery.all %>% mutate(r=case_when(  year>recovery_year_volume ~ NA,
                                                     year<=recovery_year_volume ~Perc_Diff_VOL ))

df_endpoints<-recovery.all1 %>% group_by(mgm,rcp,model,windcase) %>% summarise(rt=max(recovery.time)) %>% mutate(value=0)

df_endpoints2<-df_endpoints %>% group_by(mgm,rcp) %>% summarize(mean1=mean(rt), min1=min(rt), max1=max(rt))



not.recovered<-df_endpoints %>% filter(rt==Inf) 
not.recoveredno<-df_endpoints %>% filter(rt==Inf) %>% summarize(n=n())
NCC_HIRHAM5<- df_endpoints %>% filter(model=="NCC_HIRHAM5"&rcp=="rcp45")



df_endpoints<-df_endpoints %>% filter(rt<50)




ii<-df_endpoints %>% group_by(rcp,mgm) %>% summarize(recovery_time=mean(rt))





less<-recovery.all1 %>% select(year_after_impact,model, mgm, rcp, windcase,volume_m3,r)

l1<-less %>% filter(is.na(r)==F) %>% group_by(model, rcp, mgm, windcase) %>%  
  arrange(year_after_impact) %>% 
  summarize(auc=auc(year_after_impact,r)) %>% 
  mutate(norm.auc=auc/-5000.)


# MAKE  EXAMPLE, WHERE PUT ALL THE RECOVERY TIME, AUC AND IMPACT VALUES ON THE PLOT


# look the adaptation and w8 case:

example<-l1 %>% filter((mgm=="ADAPTATION"&windcase=="w8"))

example<-left_join(example,rec,by=c("model","rcp","mgm","windcase"))

dsum2<-d %>% filter(year==50) %>% mutate(rel.wind.impact=100*wind/landscape_volume_yearstart) %>%
  select(-year,-barkbeetle,-wind,-landscape_volume_yearstart,-impact,-relimpact)

example<-left_join(example,dsum2,by=c("model","rcp","mgm","windcase"))




g1<-less %>%
  filter((mgm=="ADAPTATION"&windcase=="w8"),!is.na(r)) %>%
  group_by(model, rcp, mgm, windcase) %>%
  arrange(year_after_impact) %>%
  mutate(  auc = auc(year_after_impact, r)) %>%
  ggplot(aes(x = year_after_impact, y = r)) +
  geom_ribbon(aes(ymin = r, ymax = 0), fill = "skyblue", alpha = 0.4) +
  geom_line(color = "forestgreen") +
  facet_grid(rcp~ model + mgm + windcase) +
  labs(    x = "Years after impact",
           y = "% difference of volume compared to pre-dist level"
  ) +
  geom_text(    data = example,
                aes(x =40,              # position at right edge
                    y =-45,              # position at top
                    label =  paste("impact=",round(rel.wind.impact, 2))),
                hjust = 1., vjust = 1.,
                color = "blue", size = 3.5,
                inherit.aes = FALSE  )+
  geom_text(    data = example,
                aes(x =40,              # position at right edge
                    y =-55,              # position at top
                    label =  paste("RT=",round(recovery.time, 2))),
                hjust = 1., vjust = 1.,
                color = "blue", size = 3.5,
                inherit.aes = FALSE  )+
  geom_text(    data = example,
                aes(x =40,              # position at right edge
                    y =-65,              # position at top
                    label =  paste("AUC=",round(auc, 2))),
                hjust = 1., vjust = 1.,
                color = "blue", size = 3.5,
                inherit.aes = FALSE  )+
  geom_text(    data = example,
                aes(x =40,              # position at right edge
                    y =-75,              # position at top
                    label =  paste0("normAUC=",round(norm.auc, 2))),
                hjust = 1., vjust = 1.,
                color = "blue", size = 3.5,
                inherit.aes = FALSE  )+
  geom_text(    data = example,
                aes(x = 40,              # position at right edge
                    y = -85,              # position at top
                    label =  paste0("1-normAUC=",round(1-norm.auc, 2))),
                hjust = 1., vjust = 1.,
                color = "red", size = 3.5,
                inherit.aes = FALSE  )+
  xlim(0,50)+ylim(-100,0)+theme_bw()+
  theme(
   # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


pdf(paste0(plotroot, "2d_Resilience_calculation_example_AUC_time_series_example_with_values.pdf"), height = 10, width = 10)
print(g1)
dev.off()





#I want to put them all into one plot, coloring the resilience indicator


less<-left_join(less,l1,by=c("rcp", "model" , "mgm" , "windcase"))


g1<-ggplot(less, aes(x = year_after_impact, y = r, group=paste(rcp, model , mgm , windcase), colour = 1-norm.auc)) +
  geom_line() +
  labs(    x = "Years after impact",
           y = "% difference of volume compared to pre-dist level"
  ) +
  xlim(0,50)+ylim(-100,0)+theme_bw()+theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 50)+
  geom_hline(yintercept = -100)

pdf(paste0(plotroot, "2d_Resilience_spagetti_relative_volume_in_time.pdf"), height = 7, width = 8)
print(g1)
dev.off()



to_plot<-left_join(l1,MF,by=c("model","mgm","windcase","rcp"))





p_main<-ggplot(to_plot ,aes( score,   1-norm.auc ))+
  geom_point( aes(color=  factor(mgm, levels=c("ADAPTATION","BAU","BIOECONOMY","CONSERVATION", "UNMANAGED"))), size=2)+
  geom_smooth(method = "lm")+
  labs(x = "MF score",y="Resilience(1-norm.auc)", col="mgm")+
  facet_grid(~rcp)+
  scale_color_manual(values=(c( "#f2c14e","chocolate", "black", "#62d75f","#248721" )))+
  theme_bw()+
  theme(legend.position = "bottom",panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))



p_main





stats <- to_plot %>%
  group_by(rcp) %>%
  do(tidy(lm(I(1 - norm.auc) ~ score, data = .)))

stats


mods <- to_plot %>%
  group_by(rcp) %>%
  group_modify(~{
    m <- lm(I(1 - norm.auc) ~ score, data = .x)
    
    data.frame(      intercept = coef(m)[1],
      slope     = coef(m)[2],
      r2        = summary(m)$r.squared,
      p.value   = summary(m)$coefficients[2, 4]) })

mods




p.all<-p_main +
  stat_regline_equation(aes(label = ..eq.label..),
    label.x.npc = 0.9,
    label.y.npc = 0.95,
    hjust = 1,
    vjust = 1,
     size = 3  ) +
  stat_cor(    aes(label = ..rr.label.. ),
    label.x.npc = 0.9,
    label.y.npc = 0.9,
    hjust = 1,
    vjust = 1,
    size = 3  )+ 
  stat_cor(    aes(label =  ..p.label..),
                              label.x.npc = 0.9,
                              label.y.npc = 0.84,
                              hjust = 1,
                              vjust = 1,
                              size = 3  )




pdf(paste0(plotroot, "2d_Resilience_vs_MF_without_bars.pdf"), height = 6, width = 10)
print(p.all)
dev.off()



detailed<-to_plot %>%
  mutate(   resilience = 1 - norm.auc ) %>%
  select(model,rcp,mgm,windcase,score,resilience) 



detailed <- detailed %>%
  mutate( score_01 = score / 4,
    res_01   = pmin(pmax(resilience, 0), 1),
    dist = sqrt(  (1 - score_01)^2 +  (1 - res_01)^2) )



mm<-detailed %>% group_by(mgm, rcp) %>% summarise(score=mean(score_01),res=mean(res_01) ) %>% 
  mutate(dist = sqrt(  (1 - score)^2 +  (1 - res)^2) )

pp<-detailed %>% group_by(mgm, rcp) %>% summarise(d=mean(dist))


ideal <- data.frame(  mgm = unique(detailed$mgm),  x = 1,  y = 1)


 # replace "-" with refclim
detailed <- detailed %>%
  mutate(  rcp = recode(rcp, "-" = "refclim"),  rcp = factor(rcp, levels = c("refclim", "rcp45", "rcp85")) )

mm <- mm %>%
  mutate(  rcp = recode(rcp, "-" = "refclim"),  rcp = factor(rcp, levels = c("refclim", "rcp45", "rcp85")) )



mgm_cols <- c(  ADAPTATION   = "#f2c14e",
  BAU          = "chocolate",
  BIOECONOMY   = "black",
  CONSERVATION = "#62d75f",
  UNMANAGED    = "#248721")


p1<-ggplot() +
  # background: all simulations
  geom_segment(data=detailed,aes(x = score_01, y = res_01,xend = 1,yend = 1,colour = mgm),alpha = 0.1   )+
  geom_point(data=detailed,aes(x = score_01,y = res_01,colour = mgm,shape = rcp),alpha = 0.4,size = 1.5) +
  
  # foreground: means
  geom_segment(data= mm,aes(x = score,y = res,xend = 1,yend = 1,colour = mgm),linewidth = 0.5,    alpha = 0.8  ) +
  geom_point(data=mm,aes(x=score, y=res, colour=mgm, shape = rcp), size = 5) +
  geom_point(data=ideal,aes(x, y),inherit.aes = FALSE,shape = 8,size = 5,colour = "black")+
   scale_colour_manual(values = mgm_cols)+
  theme_bw() +
  xlim(0.6,1)+
  ylim(0.6,1)+
  theme(  panel.grid = element_blank())+
  labs(  x = "Multifunctionality (0–1 scaled)",   y = "Resilience (0–1 scaled)" )



# boxplot with distances, ordered




p2<-ggplot(  detailed,  aes(    x = reorder_within(mgm, dist, rcp),y = dist,fill = mgm , color=mgm), show.legend=F) +
  geom_boxplot() +
  facet_wrap(~rcp, scales = "free_x") +
  scale_x_reordered() +
  scale_fill_manual(values = mgm_cols)+
  scale_color_manual(values = mgm_cols)+
  theme_bw() +
  labs(  x = "Management",  y = "Distance")+
  theme(  panel.grid = element_blank(),
            legend.position = "none",
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
          strip.background = element_blank() )






p2


p3<-p1 +inset_element(p2,left= 0.05,bottom = 0.01,right  = 0.99,top    = 0.35  )

p3

pdf(paste0(plotroot, "2d_Distances_in_normalized_space.pdf"), height = 8, width = 10)
print(p3)
dev.off()


# write out distance values that are ordered:
mm2<-mm %>% arrange(rcp,dist)


# How the distances changed compared to refclim?
dist.ref<-detailed %>% filter(rcp=="refclim") %>% ungroup() %>% select( mgm, windcase, dist) %>% rename(refdif=dist)
dist.scen<-detailed %>% filter(rcp!="refclim") %>% select(model,rcp, mgm, windcase, dist) 

diff<-left_join(dist.scen, dist.ref, by=c("mgm", "windcase")) %>% mutate(diff=dist-refdif, percdiff=100*(dist-refdif)/refdif)

diff.aggreg<-diff %>% group_by(mgm, rcp) %>% summarize(mean.absdiff=mean(diff), min.absdiff=min(diff), max.absdiff=max(diff),
  mean.percdiff=mean(percdiff), min.percdiff=min(percdiff), max.percdiff=max(percdiff))


# write tables:
write.csv(mm2,paste0(dataroot,"2d_distances_in_the_normalized_space_20260630.csv"), row.names=F, quote=F)
write.csv(diff,paste0(dataroot,"2d_CC_effects_on_distance_detailed_20260630.csv"), row.names=F, quote=F)
write.csv(diff.aggreg,paste0(dataroot,"2d_CC_effects_on_distance_aggregated_20260630.csv"), row.names=F, quote=F)


#------------------------------------------------
#  Add pairwise comparison of distances in the space. Stay inside the same climate and wind group and visualize how much the managements differ.
#  (after review)
# -------------------------------------------------
# one row per block x management
base <- detailed %>%
  select(model, rcp, windcase, mgm, score_01, res_01)

# all pairs of managements within the same block
pairs <- base %>%
  inner_join(base,
             by = c("model", "rcp", "windcase"),
             suffix = c("_a", "_b"),
             relationship = "many-to-many") %>%
  filter(mgm_a < mgm_b) %>%                      # keep each pair once (alphabetical)
  mutate(
    pair   = paste(mgm_a, mgm_b, sep = " vs "),
    d_score = score_01_b - score_01_a,           # signed differences (useful for interpretation)
    d_res   = res_01_b   - res_01_a,
    dist    = sqrt(d_score^2 + d_res^2)          # Euclidean distance between managements
  )



# mean distance per pair and rcp
pair_summary <- pairs %>%
  group_by(rcp, pair) %>%
  summarise(mean_dist = mean(dist),
            sd_dist   = sd(dist),
            .groups = "drop")

# distance matrix per rcp (mean over models and wind cases)
dist_mat <- pairs %>%
  group_by(rcp, mgm_a, mgm_b) %>%
  summarise(dist = mean(dist), .groups = "drop")


pair_order <- pairs %>%
  group_by(pair) %>% summarise(m = median(dist)) %>% arrange(m) %>% pull(pair)

p_pairs <- ggplot(pairs,
                  aes(x = factor(pair, levels = pair_order), y = dist)) +
  geom_boxplot(fill = "grey85", colour = "grey30", outlier.size = 0.8) +
  facet_wrap(~rcp) +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = "Distance between managements\n(normalized score–resilience space)") +
  theme(panel.grid = element_blank(), strip.background = element_blank())
p_pairs

pdf(paste0(plotroot, "2d_Paired_distances.pdf"), height = 7, width = 12)
print(p_pairs)
dev.off()

p_heat <- ggplot(dist_mat, aes(mgm_a, mgm_b, fill = dist)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(dist, 2)), size = 3) +
  scale_fill_viridis_c(option = "D", direction = -1) +
  facet_wrap(~rcp) +
  theme_bw() +
  labs(x = NULL, y = NULL, fill = "Mean\ndistance") +
  theme(panel.grid = element_blank(), strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
p_heat
pdf(paste0(plotroot, "2d_Paired_distances_mean_heatmap.pdf"), height = 7, width = 12)
print(p_heat)
dev.off()

#-----------

base <- detailed %>%
  select(model, rcp, windcase, mgm, score_01, res_01, d_ideal = dist)

pairs <- base %>%
  inner_join(base, by = c("model", "rcp", "windcase"),
             suffix = c("_a", "_b"), relationship = "many-to-many") %>%
  filter(mgm_a < mgm_b) %>%
  mutate(
    pair    = paste(mgm_a, "vs", mgm_b),
    d_score = score_01_a - score_01_b,   # >0: A has higher multifunctionality
    d_res   = res_01_a   - res_01_b,     # >0: A has higher resilience
    d_comp  = d_ideal_b  - d_ideal_a,    # >0: A closer to ideal (better overall)
    dist    = sqrt(d_score^2 + d_res^2)  # unsigned magnitude, as before
  )

pairs_long <- pairs %>%
  select(rcp, pair, mgm_a, mgm_b, model, windcase, d_score, d_res, d_comp) %>%
  pivot_longer(c(d_score, d_res, d_comp), names_to = "metric", values_to = "diff") %>%
  mutate(metric = recode(metric,
                         d_score = "Multifunctionality",
                         d_res   = "Resilience",
                         d_comp  = "Overall (closeness to ideal)"))

consistency <- pairs_long %>%
  filter(diff != 0) %>%                                  # drop exact ties
  group_by(rcp, pair, mgm_a, mgm_b, metric) %>%
  summarise(n          = n(),
            mean_diff  = mean(diff),
            p_a_larger = mean(diff > 0),                 # share of cases where A > B
            .groups = "drop") %>%
  mutate(winner      = if_else(p_a_larger >= 0.5, mgm_a, mgm_b),
         consistency = pmax(p_a_larger, 1 - p_a_larger)) # 0.5 = coin flip, 1 = always same order


p_cons <- ggplot(consistency,
                 aes(x = metric, y = reorder(pair, mean_diff), fill = mean_diff)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = paste0(round(100 * p_a_larger), "%")), size = 3) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0,
                       name = "Mean diff\n(A − B)") +
  facet_wrap(~rcp) +
  theme_bw() +
  labs(x = NULL, y = NULL,
       caption = "Label: % of model × wind cases where the first management is larger") +
  theme(panel.grid = element_blank(), strip.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1))
p_cons

pdf(paste0(plotroot, "2d_Paired_distances_consistency_of_cases.pdf"), height = 8, width = 10)
print(p_cons)
dev.off()

