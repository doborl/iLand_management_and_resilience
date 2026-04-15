#
#
# Laura Dobor, CULS,
# Multifunctionality based on simulations without wind event!!!
# 20260414

# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)   
library(fields)
library(ggradar)
library(ggiraphExtra)

# read in csvs and make plots to the plots folder

version<-"NO_DISASTER"
dataroot<-"D:/___PROJECTS/2025_iLand_management_study/04_work/3_analyses/Output_summary_tables/"


  date<-"2025-04-19"
  MF.all<-read.csv(paste0(dataroot,date,"_multifunctionality.csv"))

  lnd<- read.csv(paste0(dataroot,date,"_landscape.csv"))
  



#_______________________________________________________________________________
# This tells the colors:

species.we.have<-unique(lnd$species)                                            # IT IS SAYING WHICH SPECIES WE HAVE IN THE DATABASE IN THIS CASE LANDSCAPE


# LIST OF ALL POSSIBLE SPECIES
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

#1) I want species composition barplots with volume


head(lnd)

avg.spec.vol<-lnd %>% filter(year>36&year<65) %>% group_by(mgm, rcp,species) %>% summarise(volume=mean(volume_m3))


g1<-ggplot(avg.spec.vol, aes(mgm,volume, fill=factor(species, levels=new_order_gg)))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Species comp. based on volume mid30y avg")+
  facet_grid(~rcp)+
  labs(x = "Management",y="Volume m3/ha",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))



# species composition, %

sum.vol<-lnd %>% filter(year>36&year<65) %>% group_by(mgm, rcp, model, windcase, year) %>% summarise(totalvolume=sum(volume_m3))
sum.vol2<-sum.vol %>% group_by(mgm,rcp) %>% summarise(totalvolume=mean(totalvolume))

avg.spec.vol2<-left_join(avg.spec.vol,sum.vol2, by=c("mgm", "rcp")) %>% mutate(vol.prop=volume/totalvolume)

g2<-ggplot(avg.spec.vol2, aes(mgm,vol.prop, fill=factor(species, levels=new_order_gg)))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("Species comp. based on volume  mid30y avg")+
  facet_grid(~rcp)+
  labs(x = "Management",y="Proportion",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))


# Go for the indicators --> I want spider diagrams with the mean 

vars<-c("NEP","carbonstock","LAI","standing.volume","annual.increment","annual.harvest","shannon_BA_landscape","deadwood.c.ag")


MF.all2<-MF.all %>% filter(year>36&year<65) %>% group_by(mgm, rcp) %>% summarise_all(mean) %>% select(-year) %>% mutate(deadwood.c.ag=deadwood.c.ag/1000,carbonstock=carbonstock/1000. )

MF.all2.long<-pivot_longer(MF.all2, cols=c("NEP","carbonstock","LAI","standing.volume","annual.increment","annual.harvest","shannon_BA_landscape","deadwood.c.ag")) %>% select(mgm,rcp,name, value)

MF.all3<-pivot_wider(MF.all2.long,names_from = mgm )



g3<-ggRadar(data=MF.all3,mapping = aes(colour = rcp, facet=name), 
             rescale = FALSE, interactive = FALSE, use.label = TRUE, size = 2,alpha=0.1,
             legend.position = "right", scales="free") +theme_bw()+
  ggtitle("Multifunctionality indicators mid30y avg")+
  scale_fill_manual(values=(c("#3a6ea5","#f2c14e","#f78154" )))+
  scale_color_manual(values=(c("#3a6ea5","#f2c14e","#f78154" )))+
  theme(strip.background =element_rect(fill="white"))

data.frame(MF.all3)


pdf(paste0(dataroot, "plots/Multifunctionality__",date,"_",version,"__LD.pdf"), height = 10, width = 12)

grid.arrange(g1,g2, ncol=2)
print(g3)

#grid.arrange(g1,g2,g3,ncol = 2, nrow = 2,layout_matrix= rbind(c(3,1), c(3,2)))




# Do the normalization of indicators:

# Make 30y average for all simulation runs:
  MF.mid30yavg<-MF.all %>% group_by(rcp,model,mgm, windcase) %>% filter(year>36&year<65) %>% summarise_all(mean) %>% select(-year)

  # Make a long version with the indicators in one column:
  MF.mid30yavg.long<-data.frame(pivot_longer(MF.mid30yavg,cols=c(colnames(MF.mid30yavg)[5:17])))

  # global maximums:
  
  
  
  globalmax<-data.frame(MF.mid30yavg.long %>% group_by(name) %>% summarise(max=max(value)))
  
  # We will use min-max normalization, using 0 as minimum in most of the cases, except NEP: NEP min will be the overall minimum:
  globalNEPmin<-data.frame(MF.mid30yavg.long %>% filter(name=="NEP") %>% summarise(min=min(value)))$min
  print(paste0("NEP minimum across all simulations: ", globalNEPmin))
  
  # Add a column on min for normalization:
  MF.mid30yavg.long<-MF.mid30yavg.long %>% mutate(minfornorm= case_when(name=="NEP"   ~ globalNEPmin,
                                                                        name!="NEP" ~0))
  
  
  
  MF.mid30yavg.long<-left_join(MF.mid30yavg.long,globalmax, by="name")
  # add the normlaized value
  rescale.alter<-MF.mid30yavg.long %>% mutate(value.normalized1 = (value - minfornorm) / (max - minfornorm)) 
  head(rescale.alter)
  
  
 pp<- rescale.alter %>% filter(name=="impact", windcase=="w5")
  
# select the indicators we want: calculate stats for each indicator, rcp and mgm:
#tab<-rescale.alter %>% filter(name %in% vars) %>% group_by(name,rcp, mgm) %>% summarise(v=mean(value), vmin=min(value), vmax=max(value),
  #                                                                                      vn1=mean(value.normalized1),vn1min=min(value.normalized1), vn1max=max(value.normalized1))



# Want to have the 4 indicators as the avg of different measuere

#I Want the normalized values
fintab.norm<-rescale.alter %>% select(rcp,model,mgm,windcase, name, value.normalized1)
fintab.norm.wide<-fintab.norm %>% pivot_wider(names_from = name,values_from ="value.normalized1")


fintab.norm<-data.frame(fintab.norm.wide %>% group_by(rcp,model,mgm, windcase) %>% mutate(Climate=sum(NEP,carbonstock)/2.,
                                                                                           Water=sum(LAI,standing.volume)/2., 
                                                                                           Production=sum(annual.increment,annual.harvest)/2., 
                                                                                           Biodiversity=sum(shannon_BA_landscape,deadwood.c.ag)/2.) )

fintab.norm<-fintab.norm %>% select("rcp","model","mgm","windcase","Climate","Production","Water","Biodiversity")
pl<-fintab.norm %>% group_by(rcp, mgm) %>% summarise_all(mean) %>% select(-model, -windcase)

g16<- ggRadar(data=pl,mapping = aes(colour = mgm, facet=rcp), 
             rescale = FALSE, interactive = FALSE, use.label = TRUE, size = 2,alpha=0.1,
             legend.position = "right", scales="free") +theme_bw()+ylim(0,1)+
  ggtitle("Indicator groups mid30y avg")+
  scale_fill_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))+
  scale_color_manual(values=(c("#4d9078", "#f2c14e", "#f78154",  "#b4436c", "#5fad56" )))






# 

score.norm<-fintab.norm %>% mutate(score=(Climate+Production+Water+Biodiversity))

long<-pivot_longer(score.norm,cols=c("Climate","Production","Water","Biodiversity"))
longstats<-long %>% group_by(rcp,mgm, name) %>% summarise(s=mean(value), smin=min(value), smax=max(value))
MFs<-score.norm %>% select(rcp,model,mgm,windcase,score) %>% group_by(rcp,mgm) %>% summarise(sall=mean(score), sallmin=min(score), sallmax=max(score))


longstats<-data.frame(left_join(longstats,MFs, by=c("rcp","mgm")))


g17<-ggplot(longstats, aes(mgm,s, fill=name)) +
  ggtitle("MF score mid30y avg")+
  scale_fill_manual(values=(c("#5fad56", "#f2c14e", "#f78154",  "#3a6ea5")))+
  geom_bar(  stat="identity")+
  geom_errorbar( aes(x=mgm, ymin=sallmin, ymax=sallmax), position = "identity",width = 0.3)+
  facet_grid(~rcp)+  theme_bw()+ylab("Multifunctionality score")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))

# I Want a barplot with ranges
# and I want a barplot per management

grid.arrange(g16, g17,  nrow = 2)



dev.off()





# We want to see which parts has climate change effect on?
#
#
# I will do the differences


todiff<-long %>% select(rcp,model, mgm, windcase, name, value)

todiffref<-todiff %>% filter(rcp=="-") %>% rename(rcpref=rcp,refval=value) %>% select(-model)
todifffut<-todiff %>% filter(rcp!="-")

diff<-left_join(todifffut,todiffref,by=c("mgm", "windcase","name")) %>% mutate(absdiff=value-refval, 
                                                                               percdiff=100*(value-refval)/refval)

meandiff<-data.frame(diff %>% group_by(rcp,mgm,name) %>% summarise(meanpercdiff=mean(percdiff)))



g29<-ggplot(meandiff, aes(mgm,meanpercdiff, fill=rcp)) +
  ggtitle("")+
  scale_fill_manual(values=(c("#5fad56", "#f2c14e", "#f78154",  "#3a6ea5")))+
  geom_bar(  stat="identity", position = "dodge")+
  facet_grid(~name)+  theme_bw()+ylab("% difference compared to reference climate case")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))



g30<-ggplot(meandiff, aes(name,meanpercdiff, fill=rcp)) +
  ggtitle("")+
  scale_fill_manual(values=(c("#5fad56", "#f2c14e", "#f78154",  "#3a6ea5")))+
  geom_bar(  stat="identity", position = "dodge")+
  facet_grid(~mgm)+  theme_bw()+ylab("% difference compared to reference climate case")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))



pdf(paste0(dataroot, "plots/Multifunctionality_2_",date,"_",version,"__LD.pdf"), height = 8, width = 10)
print(g16)
print(g29)
print(g30)
dev.off()
#
#
#
# Same differences for the overall multifunctionality


todiffref<- score.norm %>% filter(rcp=="-") %>% rename(rcpref=rcp,refscore=score) %>% select(-model, -Climate,-Production, -Water,-Biodiversity)
todifffut<- score.norm %>% filter(rcp!="-")

diff<-left_join(todifffut,todiffref,by=c("mgm", "windcase")) %>% mutate(absdiff=score-refscore, 
                                                                               percdiff=100*(score-refscore)/refscore)

meandiff2<-data.frame(diff %>% group_by(rcp,mgm) %>% summarise(meanpercdiff=mean(percdiff)))
#ADD ERRORBARA!!!!!!!!!!!!!!!!!!!! HHEREEE


g31<-ggplot(meandiff2, aes(mgm,meanpercdiff, fill=rcp)) +
  ggtitle("")+
  scale_fill_manual(values=(c("#5fad56", "#f2c14e", "#f78154",  "#3a6ea5")))+
  geom_bar(  stat="identity", position = "dodge")+
  theme_bw()+ylab("% difference compared to reference climate case")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank(),
        strip.background =element_rect(fill="white"))

#








# To make pdfs one by one


pdf(paste0(dataroot, "plots/Species_composition_based_on_vol_",date,"_",version,".pdf"), height = 4, width = 5)
print(g1)
dev.off()
pdf(paste0(dataroot, "plots/Species_composition_based_on_volprop_",date,"_",version,".pdf"), height = 4, width = 5)
print(g2)
dev.off()
#
pdf(paste0(dataroot, "plots/Spidergraph_indicators_",date,"_",version,".pdf"), height = 8, width = 10)
print(g3)
dev.off()

pdf(paste0(dataroot, "plots/MF_scolre_",date,"_",version,".pdf"), height = 4, width = 5)
print(g17)
dev.off()
