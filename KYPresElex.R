# Let's now look at the Presidential Elections
PresElex<-read.csv('./PresElex.csv')
presdemvotes<-subset(PresElex,select=c(County,PercentDem2012,PercentDem2008,PercentDem2004,PercentDem2000))
Counties2012<-sapply(which(presdemvotes$PercentDem2012>0.5),function(i){
  return<-as.character(presdemvotes$County[i])
})
Counties2008<-sapply(which(presdemvotes$PercentDem2008>0.5),function(i){
  return<-as.character(presdemvotes$County[i])
})
Counties2004<-sapply(which(presdemvotes$PercentDem2004>0.5),function(i){
  return<-as.character(presdemvotes$County[i])
})
Counties2000<-sapply(which(presdemvotes$PercentDem2000>0.5),function(i){
  return<-as.character(presdemvotes$County[i])
})
AllDemPresCounties<-data.frame(unique(c(Counties2012,Counties2008,Counties2004,Counties2000)))
colnames(AllDemPresCounties)<-'County'
AllDemPresCounties$ky2012<-sapply(AllDemPresCounties$County,function(i){
  return<-i %in% Counties2012
})
AllDemPresCounties$ky2008<-sapply(AllDemPresCounties$County,function(i){
  return<-i %in% Counties2008
})
AllDemPresCounties$ky2004<-sapply(AllDemPresCounties$County,function(i){
  return<-i %in% Counties2004
})
AllDemPresCounties$ky2000<-sapply(AllDemPresCounties$County,function(i){
  return<-i %in% Counties2000
})
AlmostAlwaysDemPresCounties<-vector(length=0)
for(i in 1:length(AllDemPresCounties$County)){
  if(sum(AllDemPresCounties[-1][i,]==FALSE)<=2){
    AlmostAlwaysDemPresCounties<-c(AlmostAlwaysDemPresCounties,as.character(AllDemPresCounties$County[i]))
  }
}