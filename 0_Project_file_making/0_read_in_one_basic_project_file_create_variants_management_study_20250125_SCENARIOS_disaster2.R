rm(list=ls()) 

# Load required libraries
library(tidyr)
library(RSQLite)
library(dplyr)
library(vegan)


#Laura Dobor, 2025.04.01



dataroot<-"E:/iLandDist - new version 2.0 - 2025/"
basefile<-paste0(dataroot,"BASIC_FOR_WINDTESTING_no_stand_outputs.xml")
mgms<-c("ADAPTATION", "BAU", "BIOECONOMY","CONSERVATION", "UNMANAGED")
browsing<-c(  0.5,    1,            1,         0.5,         0.5)
salvaging<-c(  0.8,   0.8,         0.8,         0.5,         0)
#https://czuvpraze.sharepoint.com/:x:/t/fld-t-gcrg/Ef0PeIYRazdPqBi99vCfCg8B6NBCc1YDjgCPXS7h7L4fSg?e=p89W0Z


models<-c("EC-EARTH_RACMO22E-r1","HadGEM2_CCLM","MPI_CCLM","NCC_HIRHAM5")
scens<-c("rcp45", "rcp85")


write( " ", (paste0(dataroot, "DISASTER2_RUN_models_20250414.bat")), append=FALSE)

for (ss in 1:2) {

  scen<-scens[ss]

  
  for (mm in 1:4) {

  
  
  
  
  
  model<-models[mm]
for (k in 1:length(mgms)) {

mgm<-mgms[k]
salv<-salvaging[k]
br<-browsing[k]



fs<-list.files(paste0(dataroot,"scripts/wind-disaster2/"),paste0(scen,"_LD_disaster.txt"))     #-----------


for (i in 1:length(fs)) {
  
  
  n<-read.table(basefile, sep="|", header=F, fill=T,blank.lines.skip=F,stringsAsFactors = FALSE)
 
  f<-fs[i]
  
  windcase<-strsplit(f,"_")[[1]][1]
  
  n[1,1]<-"<?xml version=\"1.0\" encoding=\"utf-8\"?>"   # to print "  need to write \"
  #output:
 
  n[16,1]<-paste0("<out>","DISASTER2_", mgm,"_",windcase,"_",model,"_",scen,".sqlite</out> ")    #*--------------------
  print( n[16,1])
  
  n[17,1]<-paste0(      "<climate>CZ_region_FORESEEv4_", model, "_",scen,".sqlite</climate>")
  #log:
 
  n[22,1]<-paste0("<logFile>log/DISASTR2_",mgm,"_",windcase,"_",model,"_",scen,".txt</logFile> ")
  print( n[22,1])

  
  #browsing:
  
  n[114,1]<-paste0("<browsingPressure>",br,"</browsingPressure> ")
  print( n[114,1])
  
  n[141,1]<-paste0("  <type id=\"1\"> <!-- deciduous broadleaved -->")
  n[149,1]<-paste0("  <type id=\"2\"> <!-- deciduous coniferous  -->")
  
  
  #wind events:
 
  n[175,1]<-paste0("<timeEventsFile>wind-disaster2/",  f, "</timeEventsFile>")   #-----------
  print( n[175,1])
  
  n[212,1]<- "<batchYears>80</batchYears> "
  n[216,1]<- "		<randomSamplingList>0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 77 73 64 73 71 69 67 71 59 67 65 71 62 76 68 59 67 74 55 60 74 62 57 60 55 68 59 74 75 68 62 60 56 62 79 75 79 70 76 77 71 79 69 58 77 56 64 58 76 77 68 57 70 74 63 62 65 56 79 63 57 62 68 71 63 62 61 77 55 57 55 61 78 60 71 56 63 64 63 69 74 65 55 74 59 76 78 58 78 64 63 66 70 65 72 63 59 79 68 69 63 76 62 60 71 67 76 79 74 61 60 79 77 70 74 59 57 61 78 67 74 70 56 60 66 67 59 61 65 68 69 58 72 58 69 76 71 67 71 75 65 79 74 75 71 72 59 69 73 62 69 77 76 77 76 55 79 70 61 79 66 60 78 72 68 65 75 66 57 78 77 64 63 69 67 70 72 57 71 70 72 58 60 63 79 66 78 66 58 65 73 71 73 55 62 61 57 64 76 75 62 55 79 68 71 69 57 76 71 61 65 74 72 76 58 59 58 77 59 62</randomSamplingList>"
 
  
  
  #management:
  
  n[249,1]<-paste0("<file>abe/test12/",mgm,".js</file>")
  print( n[249,1])
  
  if (mgm=="UNMANAGED") n[246,1]<-paste0(" <abeEnabled>false</abeEnabled>")
  
  
  
  
  # salvaging:
  
  n[448,1]<-paste0("      <remove>",salv,"</remove>")
  print( n[448,1])
  
  
  
  #write out:
outproj<-paste0("DISASTER2_",mgm,"_",windcase,"_",model,"_",scen,".xml")
out<-paste0(dataroot,outproj)
write.table(n,out, sep=" ", row.names = F,quote=F, col.names = F)


write( paste0("ilandc.exe ",outproj, " 100"), (paste0(dataroot, "DISASTER2_RUN_models_20250414.bat")),  append=TRUE)


  }


}
}
}
