
library(vegan)

library(RSQLite)
library(dplyr)
library(ggplot2)

files<-c("DISASTER_BAU_w5_refclim_test1.sqlite",
         "DISASTER_BAU_w5_refclim_test2.sqlite",
         "DISASTER_BAU_w5_refclim_test3.sqlite",
         "BAU_w5_refclim.sqlite")

cases<-c("28ms_storm_60min",
         "35ms_storm_60min",
         "35ms_storm_120min",
         "no_storm")

n<-length(files)
dataroot<-"E:/iLandDist - new version 2.0 - 2025/output/"
figroot<-"D:/___PROJECTS/2025_iLand_management_study/04_work/analyses/Disaster_testing_20250403/"
lnd<-c()
wind<-c()
bb<-c()
damage<-c()
for (i in 1:n){

# Select the given file and case:  
  file1<-paste0(dataroot,files[i])
  case1<-cases[i]
  
# Read data:  
  sqlite.driver <- dbDriver("SQLite")
  db1 <- dbConnect(sqlite.driver, dbname = file1)
  landscape1 <- dbReadTable(db1,"landscape")
  landscape.removed1 <- dbReadTable(db1,"landscape_removed")
  wind1 <- dbReadTable(db1,"wind")
  bb1<-dbReadTable(db1,"barkbeetle")
  dbDisconnect(db1)    # close the file
  
# Add info on which is which:
  landscape1<-landscape1 %>% mutate(case=case1)
  wind1<-wind1 %>% mutate(case=case1)
  bb1<-bb1 %>% mutate(case=case1)
  
  
  lnd_volume = landscape1 %>% group_by(year)  %>%                                # CREATE THE SUMMARIZATION OF THE SPECIES VOLUME PROPORTION TO CREATE A TOTAL LANDSCAPE VOLUME
    summarise(tot_vol = sum(volume_m3),
              .groups = 'drop')
  lnd_volume.yearstart<-lnd_volume %>% mutate(year=year+1)
  landscape.area<-landscape1$area[1]
  # WIND AND BARKBEETLE MERGING
  
  damage1 <- data.frame(year=bb1$year,                                    # CREATE THE DATA FRAME FOR FOR DAMAGE OF BARKBEETLE
                       barkbeetle=bb1$killedVolume/landscape.area)
  
  # ADD WIND IMPACT IN THE DAMAGE DATA FRAME                                    # LEFT_JOIN IS A FUNCTION TO JOIN A VARIABLE IN THIS CASE COLUMN 1 AND 2 AND MANY ROWS BASE ON YEAR VARIABLE NUMBER OF ROWS
  damage1<-left_join(damage1,wind1[,c(1,8)],by=("year"))                           # NB ...wind[,c(1,8)]... Means all the row, column 1 (year),8 (killedVolume). 
  damage1<-left_join(damage1,lnd_volume.yearstart,by=("year"))                              # ADD THE LANDSCAPE VOLUME IN THE DAMAGE DATA FRAME
  
  colnames(damage1)<-c("year","barkbeetle","wind", "landscape_volume_yearstart")
  
  damage1$wind[which(is.na(damage1$wind)==TRUE)] <- 0            
  
  
  
  damage1<-damage1 %>% mutate(wind=wind/landscape.area, impact=barkbeetle+wind) %>%
    mutate(relimpact=100*(impact/landscape_volume_yearstart), case=case1) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
# Collect all
  lnd<-rbind(lnd,landscape1)
  wind<-rbind(wind,wind1)
  bb<-rbind(bb,bb1)
  damage<-rbind(damage,damage1)
  
  
}




#_______________________________________________________________________________
# This tells the colors:

species.we.have<-unique(lnd$species)                                            # IT IS SAYING WHICH SPECIES WE HAVE IN THE DATABASE IN THIS CASE LANDSCAPE

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


g1<-ggplot(lnd, aes(year,volume_m3, fill=factor(species, levels=new_order_gg)))+
  geom_area() +
  scale_fill_manual(values=cols[new_order_gg], guide=guide_legend(reverse=TRUE))+
  ggtitle("volume m3/ha")+
  facet_wrap(~factor(case,c("no_storm","28ms_storm_60min","35ms_storm_60min","35ms_storm_120min")), nrow=n)+
  
  labs(x = "Year",y="Volume m3/ha",fill = "Species")+
  theme(plot.title = element_text(hjust = 0.5))+
  #ylim(0,400)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))


ylim.bb <- c(0, 3)                                                                                          # in this example, precipitation look the link down
ylim.w <- c(0, 30)

b <- diff(ylim.w)/diff(ylim.bb)
a <- ylim.w[1] - b*ylim.bb[1] 
g4<-ggplot(damage,aes(year,100*wind/landscape_volume_yearstart))+
  geom_col(fill="grey",col="black")+
  geom_line(aes(y = a+ 100*barkbeetle/landscape_volume_yearstart*b), data = damage, size=0.9, col="pink") +
  scale_y_continuous(name="Wind relative damage %", sec.axis = sec_axis(~ (. - a)/b,name = "Bark beetle relative damage %"))+
  facet_wrap(~factor(case,c("no_storm","28ms_storm_60min","35ms_storm_60min","35ms_storm_120min")), nrow=n)+
  
  ggtitle("Relative damages compared to pre-dist volume %")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))




damage %>% filter(year==50)
damage %>% group_by(case) %>% summarise_all(mean)

library(gridExtra)

pdf(paste0(figroot,"test1_disaster.pdf"), width=10,height=10)
grid.arrange(g1,g4,ncol=2)
dev.off()


