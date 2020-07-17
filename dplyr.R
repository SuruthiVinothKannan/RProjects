# "dplyr" package: Select, Filter, Sample

library(dplyr)
samp<-select(data.combined,Pclass,Survived)

firstclass_sur<-filter(samp,Pclass=="1" & Survived=="1")
nrow(firstclass_sur)

firstclass_persh<-filter(samp,Pclass=="1" & Survived=="0")
nrow(firstclass_persh)
