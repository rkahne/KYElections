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

summary(lm(Model$BevinPercent~Model$MedicaidExpansion))

BevinSD<-sd(Model$BevinPercent)
MedicaidSD<-sd(Model$MedicaidExpansion)
NonBevinSD<-sd(Model$NonBevinPercent)
Model$BevinSDDiff<-Model$BevinPercent/BevinSD
Model$MedicaidSDDiff<-Model$MedicaidExpansion/MedicaidSD
Model$NonBevinSDDiff<-Model$NonBevinPercent/NonBevinSD
Model$HeatMap<-Model$MedicaidSDDiff/Model$NonBevinSDDiff
HeatMap<-data.frame(Model$County,Model$HeatMap)
colnames(HeatMap)<-c('County','value')
HeatMap$region<-sapply(HeatMap$County,function(i){
  return<-as.numeric(KYFIPS$FIP[which(KYFIPS$County==as.character(i))])
})
county_choropleth(HeatMap, state_zoom = 'kentucky',title='Medicaid Expansion and Non-Bevin/Hampton Vote', num_colors = 8)
