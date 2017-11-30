#formatting

#read on raw data
setwd("H:/Bowler/Data ID 38 Hendrickx/data")
library(gdata)
datafile<-read.xls("Carabids Nieuwpoort Diana Bowler.xls",as.is=T,sheet=3)
names(datafile)<-c("Species","SiteID","Year","Count")

#sum across mutliple values
datafile<-ddply(datafile,.(Species,SiteID,Year),summarize,Count=sum(Count))

#add in 0s for when a species was not recorded but there was a census
makeComplete<-function(df){#to add zeros for absenses in a data frame
  df2<-expand.grid(SiteID=unique(df$SiteID),Year=unique(df$Year),Species=unique(df$Species))
  df3<-merge(df2,df,by=c("Species","SiteID","Year"),all.x=T)
  df3$Count[is.na(df3$Count)]<-0
  return(df3)
}
df<-makeComplete(datafile)

#just use data for sites A, C and E
df<-subset(df,SiteID=="BRA_A"|SiteID=="BRA_C"|SiteID=="BRA_E")
df$SiteID<-factor(df$SiteID)

#add NAs for when there was no survey
df$Count[df4$SiteID=="BRA_A"&df$Year==1993]<-NA
df$Count[df4$SiteID=="BRA_A"&df$Year==1994]<-NA
df$Count[df4$SiteID=="BRA_C"&df$Year==2005]<-NA
df$Count[df4$SiteID=="BRA_C"&df$Year==2006]<-NA

#convert names to full names
setwd("H:/Bowler/Data ID 38 Hendrickx")
speciesnames<-read.xls("specieslist_Hendrickx_namescheck.xls",as.is=T)
df$Species<-speciesnames$Turin_name[match(df$Species,speciesnames$Species)]

