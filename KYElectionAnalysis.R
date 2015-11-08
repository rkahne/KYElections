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

#Counties where Dems got more than 50% of the vote by year
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

#Which Counties voted Democrat in what years
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
 
# Which counties voted Democrat in every election going back to 1975
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

# Re-Do that experiment with Counties that Voted Dem every time, or only once for Republican.
AlmostAlwaysDemCounties<-vector(length=0)
for(i in 1:length(AllDemCounties$County)){
  if(sum(AllDemCounties[-1][i,]==FALSE)<=1){
    AlmostAlwaysDemCounties<-c(AlmostAlwaysDemCounties,as.character(AllDemCounties$County[i]))
  }
}
LostAlmost2015Counties<-data.frame(AlmostAlwaysDemCounties,AlmostAlwaysDemCounties %in% Counties2015)
LostAlmost2015Counties$region<-sapply(LostAlmost2015Counties$AlmostAlwaysDemCounties,function(i){
  return<-as.numeric(KYFIPS$FIP[which(KYFIPS$County==as.character(i))])
})
colnames(LostAlmost2015Counties)<-c('region','value')
LostAlmost2015Counties$value<-as.numeric(LostAlmost2015Counties$value)
colnames(LostAlmost2015Counties)<-c('County','value','region')

county_choropleth(LostAlmost2015Counties,state_zoom='kentucky')


# Now, lets redo the 2015 Election using different theories
Redo2015Election<-data.frame(kyelections$County,kyelections$TotalVotes2015,kyelections$PercentDem2015)
colnames(Redo2015Election)<-c('County','TotalVotes','ActualPercentDem')

#Strategy One: Consolidate the base of counties that have ALWAYS voted Democrat.
MedianMarginAlways<-data.frame(AlwaysDemCounties)
MedianMarginAlways$medianDemPercent<-sapply(MedianMarginAlways$AlwaysDemCounties,function(i){
  return<-median(as.numeric(kydempercent[which(as.character(i)==as.character(kydempercent$County)),][3:length(colnames(kydempercent))]))
})
Redo2015Election$ConsolidateBase<-sapply(Redo2015Election$County,function(i){
  if(as.character(i) %in% MedianMarginAlways$AlwaysDemCounties){
    return<-max(MedianMarginAlways$medianDemPercent[which(as.character(i) == MedianMarginAlways$AlwaysDemCounties)],Redo2015Election$ActualPercentDem[which(as.character(i) == Redo2015Election$County)])
  } else {
    return<-Redo2015Election$ActualPercentDem[which(as.character(i) == Redo2015Election$County)]
  }
})
ConsolidateBasePercent<-sum(Redo2015Election$ConsolidateBase*Redo2015Election$TotalVotes)/sum(Redo2015Election$TotalVotes) # Gets you to 46%

#Strategy One A: Consolidate the base of counties that have ALMOST ALWAYS voted Democrat
MedianMarginAlmostAlways<-data.frame(AlmostAlwaysDemCounties)
MedianMarginAlmostAlways$medianDemPercent<-sapply(MedianMarginAlmostAlways$AlmostAlwaysDemCounties,function(i){
  return<-median(as.numeric(kydempercent[which(as.character(i)==as.character(kydempercent$County)),][3:length(colnames(kydempercent))]))
})
Redo2015Election$ConsolidateExpandedBase<-sapply(Redo2015Election$County,function(i){
  if(as.character(i) %in% MedianMarginAlmostAlways$AlmostAlwaysDemCounties){
    return<-max(MedianMarginAlmostAlways$medianDemPercent[which(as.character(i) == MedianMarginAlmostAlways$AlmostAlwaysDemCounties)],Redo2015Election$ActualPercentDem[which(as.character(i) == Redo2015Election$County)])
  } else {
    return<-Redo2015Election$ActualPercentDem[which(as.character(i) == Redo2015Election$County)]
  }
})
ConsolidateExpandedBasePercent<-sum(Redo2015Election$ConsolidateExpandedBase*Redo2015Election$TotalVotes)/sum(Redo2015Election$TotalVotes) # Gets you to 50%

# Strategy Two: Exacerbate Urban Areas
Urbane<-data.frame(c('Jefferson', 'Fayette', 'Franklin', 'Daviess', 'McCracken', 'Henderson', 'Union','Boyd'))
colnames(Urbane)<-'County'
Urbane$Percent2015<-sapply(Urbane$County,function(i){
  y<-kyelections$TotalVotes2015
  return<-y[which(as.character(i)==as.character(kyelections$County))]/sum(y)
})
Urbane$Percent2011<-sapply(Urbane$County,function(i){
  y<-kyelections$TotalVotes2011
  return<-y[which(as.character(i)==kyelections$County)]/sum(y)
})
Urbane$Percent2007<-sapply(Urbane$County,function(i){
  y<-kyelections$TotalVotes2007
  return<-y[which(as.character(i)==kyelections$County)]/sum(y)
})
Urbane$Percent2003<-sapply(Urbane$County,function(i){
  y<-kyelections$TotalVotes2003
  return<-y[which(as.character(i)==kyelections$County)]/sum(y)
})
Urbane$Percent1999<-sapply(Urbane$County,function(i){
  y<-kyelections$TotalVotes1999
  return<-y[which(as.character(i)==kyelections$County)]/sum(y)
})
Urbane$Percent1995<-sapply(Urbane$County,function(i){
  y<-kyelections$TotalVotes1995
  return<-y[which(as.character(i)==kyelections$County)]/sum(y)
})
Urbane$Percent1991<-sapply(Urbane$County,function(i){
  y<-kyelections$TotalVotes1991
  return<-y[which(as.character(i)==kyelections$County)]/sum(y)
})
Urbane$Percent1987<-sapply(Urbane$County,function(i){
  y<-kyelections$TotalVotes1987
  return<-y[which(as.character(i)==kyelections$County)]/sum(y)
})
Urbane$Percent1983<-sapply(Urbane$County,function(i){
  y<-kyelections$TotalVotes1983
  return<-y[which(as.character(i)==kyelections$County)]/sum(y)
})
Urbane$Percent1979<-sapply(Urbane$County,function(i){
  y<-kyelections$TotalVotes1979
  return<-y[which(as.character(i)==kyelections$County)]/sum(y)
})
Urbane$Percent1975<-sapply(Urbane$County,function(i){
  y<-kyelections$TotalVotes1975
  return<-y[which(as.character(i)==kyelections$County)]/sum(y)
})
Urbane$MedianPercent<-sapply(Urbane$County,function(i){
  return<-median(as.numeric(kydempercent[which(as.character(i)==as.character(kydempercent$County)),][3:length(colnames(kydempercent))]))
})
Urbane$MaxPercent<-sapply(Urbane$County,function(i){
  return<-max(as.numeric(kydempercent[which(as.character(i)==as.character(kydempercent$County)),][3:length(colnames(kydempercent))]))
})

Redo2015Election$FocusUrban<-sapply(Redo2015Election$County,function(i){
  if(as.character(i) %in% as.character(Urbane$County)){
    return<-Urbane$MaxPercent[which(as.character(i)==as.character(Urbane$County))]
  } else{
    return<-Redo2015Election$ActualPercentDem[which(as.character(i)==as.character(Redo2015Election$County))]
  }
})
UrbanStrategyPercent<-sum(Redo2015Election$FocusUrban*Redo2015Election$TotalVotes)/sum(Redo2015Election$TotalVotes)

Redo2015Election$FocusUrbanWithAlways<-sapply(Redo2015Election$County,function(i){
  if(as.character(i) %in% as.character(Urbane$County)){
    return<-Urbane$MaxPercent[which(as.character(i)==as.character(Urbane$County))]
  } else{
    if(as.character(i) %in% as.character(MedianMarginAlways$AlwaysDemCounties)){
      return<-MedianMarginAlways$medianDemPercent[which(as.character(i)==as.character(MedianMarginAlways$AlwaysDemCounties))]
    }else{
      return<-Redo2015Election$ActualPercentDem[which(as.character(i)==as.character(Redo2015Election$County))]
    }
  }
})
UrbanStrategyWithAlwaysPercent<-sum(Redo2015Election$FocusUrbanWithAlways*Redo2015Election$TotalVotes)/sum(Redo2015Election$TotalVotes)

#Ambitious Progressive Strategy
preselex<-read.csv('./PresElex.csv')
Obama2012<-c('Elliott','Fayette','Franklin','Jefferson')
Obama2008<-data.frame(c('Elliott', 'Hancock', 'Henderson', 'Jefferson','Menifee','Rowan','Wolfe','Fayette','Franklin'))
colnames(Obama2008)<-'County'
Obama2008$County<-as.character(Obama2008$County)
Obama2008$MaxPercent<-sapply(Obama2008$County,function(i){
  return<-max(as.numeric(kydempercent[which(as.character(i)==as.character(kydempercent$County)),][3:length(colnames(kydempercent))]))
})

Redo2015Election$Obama2008Max<-sapply(Redo2015Election$County,function(i){
  if(as.character(i) %in% as.character(Obama2008$County)){
    return<-Obama2008$MaxPercent[which(as.character(i)==as.character(Obama2008$County))]
  } else{
    return<-Redo2015Election$ActualPercentDem[which(as.character(i)==as.character(Redo2015Election$County))]
  }
})
Obama2008MaxPercent<-sum(Redo2015Election$Obama2008Max*Redo2015Election$TotalVotes)/sum(Redo2015Election$TotalVotes)

Percentile75Obama2008<-sapply(which(preselex$PercentDem2008>sort(preselex$PercentDem2008)[90]),function(i){
  return<-as.character(preselex$County[i])
})
Percentile75Obama2008<-data.frame(Percentile75Obama2008)
colnames(Percentile75Obama2008)<-'County'
Percentile75Obama2008$MaxPercent<-sapply(Percentile75Obama2008$County,function(i){
  return<-max(as.numeric(kydempercent[which(as.character(i)==as.character(kydempercent$County)),][3:length(colnames(kydempercent))]))
})
Redo2015Election$Percentile75Obama2008Max<-sapply(Redo2015Election$County,function(i){
  if(as.character(i) %in% as.character(Percentile75Obama2008$County)){
    return<-Percentile75Obama2008$MaxPercent[which(as.character(i)==as.character(Percentile75Obama2008$County))]
  } else{
    return<-Redo2015Election$ActualPercentDem[which(as.character(i)==as.character(Redo2015Election$County))]
  }
})
Percentile75Obama2008MaxPercent<-sum(Redo2015Election$Percentile75Obama2008Max*Redo2015Election$TotalVotes)/sum(Redo2015Election$TotalVotes)
FocusCounties<-sapply(which(Percentile75Obama2008$County %in% Obama2008$County==FALSE),function(i){
  return<-as.character(Percentile75Obama2008$County[i])
})
FocusCounties<-data.frame(FocusCounties)
colnames(FocusCounties)<-'County'
FocusCounties$value<-1
FocusCounties$region<-sapply(FocusCounties$County,function(i){
  return<-as.numeric(KYFIPS$FIP[which(KYFIPS$County==as.character(i))])
})
county_choropleth(FocusCounties, state_zoom = 'kentucky')
LockupCounties<-sapply(which(Percentile75Obama2008$County %in% Obama2008$County==TRUE),function(i){
  return<-as.character(Percentile75Obama2008$County[i])
})
ProgressiveMap<-data.frame(c(as.character(FocusCounties$County),LockupCounties))
colnames(ProgressiveMap)<-'County'
ProgressiveMap$value<-sapply(ProgressiveMap$County,function(i){
  if(i %in% LockupCounties){
    return<-2
  }else if(i %in% as.character(FocusCounties$County)){
    return<-1
  } else{
    return<-0
  }
})
ProgressiveMap$region<-sapply(ProgressiveMap$County,function(i){
  return<-as.numeric(KYFIPS$FIP[which(KYFIPS$County==as.character(i))])
})
county_choropleth(ProgressiveMap,state_zoom = 'kentucky')
