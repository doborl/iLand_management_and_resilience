rm(list=ls()) 
#Laura Dobor, 2020.07.07


dataroot<-"E:/iLandDist - new version 2.0 - 2025/"
basefile<-paste0(dataroot,"BASIC_FOR_WINDTESTING_no_stand_outputs.xml")
mgms<-c("ADAPTATION", "BAU", "BIOECONOMY","CONSERVATION", "UNMANAGED")
browsing<-c(  0.5,    1,            1,         0.5,         0.5)
salvaging<-c(  0.8,   0.8,         0.8,         0.5,         0)
#https://czuvpraze.sharepoint.com/:x:/t/fld-t-gcrg/Ef0PeIYRazdPqBi99vCfCg8B6NBCc1YDjgCPXS7h7L4fSg?e=p89W0Z

write( " ", (paste0(dataroot, "RUN_DISASTER2_refclim_20250414.bat")), append=FALSE)

for (k in 1:length(mgms)) {

mgm<-mgms[k]

salv<-salvaging[k]
br<-browsing[k]


fs<-list.files(paste0(dataroot,"scripts/wind-disaster2/"),"refclim_LD_disaster.txt")


for (i in 1:length(fs)) {
  n<-read.table(basefile, sep="|", header=F, fill=T,blank.lines.skip=F,stringsAsFactors = FALSE)
 
  f<-fs[i]
  
  windcase<-strsplit(f,"_")[[1]][1]
  
  n[1,1]<-"<?xml version=\"1.0\" encoding=\"utf-8\"?>"   # to print "  need to write \"
  #output:
 
  n[16,1]<-paste0("<out>DISASTER2_",mgm,"_",windcase,"_refclim.sqlite</out> ")
  print( n[16,1])
  
  
  #log:
 
  n[22,1]<-paste0("<logFile>log/DISASTER2_",mgm,"_",windcase,"_refclim.txt</logFile> ")
  print( n[22,1])

  #browsing:
  
  n[114,1]<-paste0("<browsingPressure>",br,"</browsingPressure> ")
  print( n[114,1])
  
  n[141,1]<-paste0("  <type id=\"1\"> <!-- deciduous broadleaved -->")
  n[149,1]<-paste0("  <type id=\"2\"> <!-- deciduous coniferous  -->")
  
  
  #wind events:
 
  n[175,1]<-paste0("<timeEventsFile>wind-disaster2/",  f, "</timeEventsFile>")
  print( n[175,1])
  
  
  #management:
  
  n[249,1]<-paste0("<file>abe/test12/",mgm,".js</file>")
  print( n[249,1])
  
  if (mgm=="UNMANAGED") n[246,1]<-paste0(" <abeEnabled>false</abeEnabled>")
  
  # salvaging:
  
  n[448,1]<-paste0("      <remove>",salv,"</remove>")
  print( n[448,1])
  
  #write out:
outproj<-paste0("DISASTER2_",mgm,"_",windcase,"_refclim.xml")
out<-paste0(dataroot,outproj)
write.table(n,out, sep=" ", row.names = F,quote=F, col.names = F)


write( paste0("ilandc.exe ",outproj, " 100"), (paste0(dataroot, "RUN_DISASTER2_refclim_20250414.bat")),  append=TRUE)


  }


}

