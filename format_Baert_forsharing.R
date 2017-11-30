#each data sheet in the xls file was saved as a txt file
 
#read in data file
setwd("G:/work/2017 iDiv/Datasets/Baert spiders")
speciesfiles<-list.files(getwd(),pattern="\\.txt$")
library(gdata)
RawData<-lapply(1:3,function(ind) read.delim(speciesfiles[ind],as.is=T))
names(RawData)<-sapply(strsplit(speciesfiles,"_"),function(x)x[2])
names(RawData)<-sapply(strsplit(names(RawData),"\\."),function(x)x[1])

#reorganise
library(reshape2)
library(plyr)
Data<-ldply(RawData,function(sheet) {
out<-melt(sheet,id=names(sheet)[1])
names(out)<-c("Species","Year","Count")
return(out)
})
names(Data)[1]<-"SiteID" # lijkt me niet correct

#add zeros  
makeComplete<-function(df){#to add zeros for absenses in a data frame
df2<-expand.grid(SiteID=unique(df$SiteID),Year=unique(df$Year),Species=unique(df$Species))
df3<-merge(df2,df,by=c("Species","SiteID","Year"),all.x=T)
df3$Count[is.na(df3$Count)]<-0
return(df3)
} 
df<-makeComplete(Data)

#clean up Year
df<-subset(df,Year!="TOTAAL"&Year!="SUM")
df$Year<-gsub("\\.","",df$Year)
df$Year<-gsub("AB","A",df$Year)#check...
df$Year<-as.numeric(substr(df$Year,2,4))

#Ensure there are NAs in the right places  (i.e, in years and sites when there was no census)
df$Count[(df$SiteID=="SiteA"&df$Year=="3")    |(df$SiteID=="SiteA"&df$Year=="4")]<-NA
df$Count[(df$SiteID=="SiteC"&df$Year%in%15:18)|(df$SiteID=="SiteC"&df$Year%in%15:18)]<-NA
df$Count[(df$SiteID=="SiteC"&df$Year%in%22:23)|(df$SiteID=="SiteC"&df$Year%in%22:23)]<-NA
df$Count[(df$SiteID=="SiteA"&df$Year%in%22:23)|(df$SiteID=="SiteA"&df$Year%in%22:23)]<-NA

#fix year to real year
df$Year<-df$Year+1990

#get rid of empty space
df<-subset(df,Species!="")

#need to use Platnick species names
speciesnames<-read.delim("specieslist_Baert_checked.txt",as.is=T)
df$Species<-speciesnames$species.Platnick[match(df$Species,speciesnames$species.Diana)]

#then average over mistakes in species names
df<-ddply(df,.(Species,SiteID,Year),summarize,Count=ifelse(all(is.na(Count)),NA,sum(Count,na.rm=T)))
   