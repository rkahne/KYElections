library(choroplethr)
library(ggplot2)

Election2015<-read.csv('./Election2015.csv')
Election2015$value<-Election2015$Bevin/Election2015$Total
Election2015$valueInverse<-1-Election2015$value
Medicaid<-read.csv('./MedicaidExpansion.csv')
colnames(Medicaid)<-c('County','value')
Medicaid<-subset(Medicaid,select=c('County','PercentExpansion'))
KYFIPS<-read.csv('./KYFIPS.csv',sep=';')

Election2015$region<-sapply(Election2015$County,function(i){
  return<-as.numeric(KYFIPS$FIP[which(KYFIPS$County==as.character(i))])
})

Medicaid$region<-sapply(Medicaid$County,function(i){
  return<-as.numeric(KYFIPS$FIP[which(KYFIPS$County==as.character(i))])
})

county_choropleth(Election2015, state_zoom = 'kentucky',title='Percentage of Vote for Matt Bevin & Jenean Hampton')
county_choropleth(Medicaid, state_zoom = 'kentucky',title='Per Capita Medicaid Expansion Population')

Model<-data.frame(Election2015$County,Election2015$value,Medicaid$value,Election2015$valueInverse)
colnames(Model)<-c('County','BevinPercent','MedicaidExpansion','NonBevinPercent')
lm(Model$BevinPercent~Model$MedicaidExpansion)
qplot(Model$BevinPercent,Model$MedicaidExpansion)
BevinString<-'['
for(i in 1:length(Model$BevinPercent)){
  if(i==length(Model$BevinPercent)){
    BevinString<-paste(BevinString,Model$BevinPercent[i],']',sep='')
  }else{
    BevinString<-paste(BevinString,Model$BevinPercent[i],',',sep='')
  }
}
MedicaidString<-'['
for(i in 1:length(Model$MedicaidExpansion)){
  if(i==length(Model$MedicaidExpansion)){
    MedicaidString<-paste(MedicaidString,Model$MedicaidExpansion[i],']',sep='')
  }else{
    MedicaidString<-paste(MedicaidString,Model$MedicaidExpansion[i],',',sep='')
  }
}
CountyString<-'['
for(i in 1:length(Model$County)){
  if(i==length(Model$County)){
    CountyString<-paste(CountyString,'\'',Model$County[i],'\']',sep='')
  }else{
    CountyString<-paste(CountyString,'\'',Model$County[i],'\',',sep='')
  }
}

Model$election2011<-sapply(kyelections$PercentDem2011, function(i){
  if(i>=0.5){
    return(1)
  }else{
    return(0)
  }
})
Model$election2007<-sapply(kyelections$PercentDem2007, function(i){
  if(i>=0.5){
    return(1)
  }else{
    return(0)
  }
})
Model$election2003<-sapply(kyelections$PercentDem2003, function(i){
  if(i>=0.5){
    return(1)
  }else{
    return(0)
  }
})
Model$election1999<-sapply(kyelections$PercentDem1999, function(i){
  if(i>=0.5){
    return(1)
  }else{
    return(0)
  }
})
Model$election1995<-sapply(kyelections$PercentDem1995, function(i){
  if(i>=0.5){
    return(1)
  }else{
    return(0)
  }
})
Model$election1991<-sapply(kyelections$PercentDem1991, function(i){
  if(i>=0.5){
    return(1)
  }else{
    return(0)
  }
})
Model$election1987<-sapply(kyelections$PercentDem1987, function(i){
  if(i>=0.5){
    return(1)
  }else{
    return(0)
  }
})
Model$election1983<-sapply(kyelections$PercentDem1983, function(i){
  if(i>=0.5){
    return(1)
  }else{
    return(0)
  }
})
Model$election1979<-sapply(kyelections$PercentDem1979, function(i){
  if(i>=0.5){
    return(1)
  }else{
    return(0)
  }
})
Model$election1975<-sapply(kyelections$PercentDem1975, function(i){
  if(i>=0.5){
    return(1)
  }else{
    return(0)
  }
})

registration<-read.csv('./MedicaidBevin/kyregistration.csv')
registration$County<-as.character(registration$County)
Model$PercentRepReg<-registration$Rep/registration$Registered

summary(lm(Model$BevinPercent~Model$MedicaidExpansion+Model$election2011+Model$election2007+Model$election2003+Model$election1999+
             +Model$election1995+Model$election1991+Model$election1987+Model$election1983+Model$election1979+Model$election1975+Model$PercentRepReg))

BevinSD<-sd(Model$BevinPercent)
MedicaidSD<-sd(Model$MedicaidExpansion)
NonBevinSD<-sd(Model$NonBevinPercent)
Model$BevinSDDiff<-(Model$BevinPercent-mean(Model$BevinPercent))/BevinSD
Model$MedicaidSDDiff<-(Model$MedicaidExpansion-mean(Model$MedicaidExpansion))/MedicaidSD
Model$NonBevinSDDiff<-(Model$NonBevinPercent-mean(Model$NonBevinPercent))/NonBevinSD
Model$HeatMap<-Model$MedicaidSDDiff-Model$NonBevinSDDiff
HeatMap<-data.frame(Model$County,Model$HeatMap)
colnames(HeatMap)<-c('County','value')
HeatMap$region<-sapply(HeatMap$County,function(i){
  return<-as.numeric(KYFIPS$FIP[which(KYFIPS$County==as.character(i))])
})
county_choropleth(HeatMap, state_zoom = 'kentucky',title='Medicaid Expansion and Non-Bevin/Hampton Vote\nBased on Z-Score', num_colors = 8)

scale(NonBevinSD)
