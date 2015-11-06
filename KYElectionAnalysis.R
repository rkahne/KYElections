# KY Election Result Analysis
# Robert Kahne

library(choroplethr)

#KYFIPS<-subset(read.csv('./FIPS.csv',header=FALSE),V1=='KY')
KYFIPS<-read.csv('./KYFIPS.csv',sep=';')
#KYFIPS$County<-sapply(KYFIPS$V4,function(i){
#  strsplit(as.character(i),split=' ')[[1]][1]
#})
#KYFIPS$FIP<-paste(KYFIPS$V2,KYFIPS$V3,sep='')
#rownames(KYFIPS)<-1:length(KYFIPS$County)

kyelections<-read.csv('./kyelections.csv',sep=';')
kytotalvotes<-subset(kyelections,select=c(County,TotalVotes2015,TotalVotes2011,TotalVotes2007,TotalVotes2003,TotalVotes1999,TotalVotes1995,TotalVotes1991,TotalVotes1987,
                                          TotalVotes1983,TotalVotes1979,TotalVotes1975))
kydempercent<-subset(kyelections,select=c(County,PercentDem2015,PercentDem2011,PercentDem2007,PercentDem2003,PercentDem1999,PercentDem1995,PercentDem1991,PercentDem1987,
                                          PercentDem1983,PercentDem1979,PercentDem1975))
Year<-2019-(4*1:length(colnames(kytotalvotes)))

#Jefferson County Data Frame
JeffCoPercent<-vector(length=length(colnames(kytotalvotes))-1)
for(i in 1:length(JeffCoPercent)){
  JeffCoPercent[i]<-as.numeric(kytotalvotes[56,][i+1]/sum(kytotalvotes[,i+1]))
}
JeffCoMargin<-as.numeric(kydempercent[56,][-1])
JeffCoVotes<-as.numeric(kytotalvotes[56,][-1])
JeffCoProportion<-JeffCoMargin*JeffCoPercent
JeffCoFrame<-data.frame(Year[1:length(Year)-1],JeffCoPercent,JeffCoMargin,JeffCoVotes,JeffCoProportion)

#Fayette County Data Frame
FayetteCoPercent<-vector(length=length(colnames(kytotalvotes))-1)
for(i in 1:length(FayetteCoPercent)){
  FayetteCoPercent[i]<-as.numeric(kytotalvotes[34,][i+1]/sum(kytotalvotes[,i+1]))
}
FayetteCoMargin<-as.numeric(kydempercent[34,][-1])
FayetteCoVotes<-as.numeric(kytotalvotes[34,][-1])
FayetteCoProportion<-FayetteCoMargin*FayetteCoPercent
FayetteCoFrame<-data.frame(Year[1:length(Year)-1],FayetteCoPercent,FayetteCoMargin,FayetteCoVotes,FayetteCoProportion)

#County Lists
Counties2015<-sapply(which(kydempercent$PercentDem2015>0.5),function(i){
  return<-as.character(kydempercent$County[i])
})
Counties2011<-sapply(which(kydempercent$PercentDem2011>0.5),function(i){
  return<-as.character(kydempercent$County[i])
})
Counties2007<-sapply(which(kydempercent$PercentDem2007>0.5),function(i){
  return<-as.character(kydempercent$County[i])
})
Counties2003<-sapply(which(kydempercent$PercentDem2003>0.5),function(i){
  return<-as.character(kydempercent$County[i])
})
Counties1999<-sapply(which(kydempercent$PercentDem1999>0.5),function(i){
  return<-as.character(kydempercent$County[i])
})
Counties1995<-sapply(which(kydempercent$PercentDem1995>0.5),function(i){
  return<-as.character(kydempercent$County[i])
})
Counties1991<-sapply(which(kydempercent$PercentDem1991>0.5),function(i){
  return<-as.character(kydempercent$County[i])
})
Counties1987<-sapply(which(kydempercent$PercentDem1987>0.5),function(i){
  return<-as.character(kydempercent$County[i])
})
Counties1983<-sapply(which(kydempercent$PercentDem1983>0.5),function(i){
  return<-as.character(kydempercent$County[i])
})
Counties1979<-sapply(which(kydempercent$PercentDem1979>0.5),function(i){
  return<-as.character(kydempercent$County[i])
})
Counties1975<-sapply(which(kydempercent$PercentDem1975>0.5),function(i){
  return<-as.character(kydempercent$County[i])
})
AllDemCounties<-data.frame(unique(c(Counties2011,Counties2007,Counties2003,Counties1999,Counties1995,Counties1991,Counties1987,Counties1983,Counties1979,Counties1975)))
colnames(AllDemCounties)<-'County'
AllDemCounties$ky2011<-sapply(AllDemCounties$County,function(i){
  return<-i %in% Counties2011
})
AllDemCounties$ky2007<-sapply(AllDemCounties$County,function(i){
  return<-i %in% Counties2007
})
AllDemCounties$ky2003<-sapply(AllDemCounties$County,function(i){
  return<-i %in% Counties2003
})
AllDemCounties$ky1999<-sapply(AllDemCounties$County,function(i){
  return<-i %in% Counties1999
})
AllDemCounties$ky2011<-sapply(AllDemCounties$County,function(i){
  return<-i %in% Counties1995
})
AllDemCounties$ky1991<-sapply(AllDemCounties$County,function(i){
  return<-i %in% Counties1991
})
AllDemCounties$ky1987<-sapply(AllDemCounties$County,function(i){
  return<-i %in% Counties1987
})
AllDemCounties$ky1983<-sapply(AllDemCounties$County,function(i){
  return<-i %in% Counties1983
})
AllDemCounties$ky1979<-sapply(AllDemCounties$County,function(i){
  return<-i %in% Counties1979
})
AllDemCounties$ky1975<-sapply(AllDemCounties$County,function(i){
  return<-i %in% Counties1975
})

AlwaysDemCounties<-vector(length=0)
for(i in 1:length(AllDemCounties$County)){
  if(!(FALSE %in% AllDemCounties[-1][i,])){
    AlwaysDemCounties<-c(AlwaysDemCounties,as.character(AllDemCounties$County[i]))
  }
}
Lost2015Counties<-data.frame(AlwaysDemCounties,AlwaysDemCounties %in% Counties2015)
Lost2015Counties$region<-sapply(Lost2015Counties$AlwaysDemCounties,function(i){
  return<-as.numeric(KYFIPS$FIP[which(KYFIPS$County==as.character(i))])
})
colnames(Lost2015Counties)<-c('region','value')

Lost2015Counties$value<-as.numeric(Lost2015Counties$value)
colnames(Lost2015Counties)<-c('County','value','region')

county_choropleth(Lost2015Counties,state_zoom='kentucky')
