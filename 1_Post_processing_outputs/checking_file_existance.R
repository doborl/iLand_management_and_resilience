


library(tidyr)
library(dplyr)
dataroot <- "E:/iLandDist - new version 2.0 - 2025/output/"


fs <- list.files(dataroot, ".sqlite") 
file.info(paste0(dataroot,fs))$size




# Take only those which are "not distaster-case"
o1<-grep("DISASTER", fs)
fs1<-fs[-o1]

o2<-grep("DISASTER2_", fs)
fs2<-fs[o2]

length(fs1)
length(fs2)


fs2<-data.frame(c=fs2, have="yes")

mgms<-c("ADAPTATION", "BAU", "CONSERVATION", "UNMANAGED", "BIOECONOMY")
models<-c("NCC_HIRHAM5", "MPI_CCLM", "EC-EARTH_RACMO22E-r1","HadGEM2_CCLM")
rcps<-c("rcp85.sqlite","rcp45.sqlite")
winds<-c("w5","w6","w7","w8","w9")



scenarios<-crossing(mgms, winds, models, rcps) %>% mutate(c=paste(mgms,winds,models,rcps, sep="_"))

ref<-crossing(mgms, winds, models="refclim.sqlite", rcps="-" ) %>% mutate(c=paste(mgms,winds,model, sep="_"))
f<-rbind(scenarios,ref)


scenarios<-crossing(mgms, winds, models, rcps) %>% mutate(c=paste("DISASTER2",mgms,winds,models,rcps, sep="_"))

ref<-crossing(mgms, winds, models="refclim.sqlite", rcps="-" ) %>% mutate(c=paste("DISASTER2",mgms,winds,models, sep="_"))
f2<-rbind(scenarios,ref)



fs2
check<-left_join(f2,fs2, by="c")

missing<-check %>% filter(is.na(have)==T)

