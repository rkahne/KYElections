library(choroplethr)
library(ggplot2)

Election2015<-read.csv('./Election2015.csv')
Election2015$value<-Election2015$Bevin/Election2015$Total
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

Model<-data.frame(Election2015$County,Election2015$value,Medicaid$value)
colnames(Model)<-c('County','BevinPercent','MedicaidExpansion')
lm(Model$BevinPercent~Model$MedicaidExpansion)
qplot(Model$BevinPercent,Model$MedicaidExpansion)
