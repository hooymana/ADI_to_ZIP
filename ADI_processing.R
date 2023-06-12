rm(list=ls())

#library(ggplot2)

#Set working directory of ZIP files and load
setwd("C:/Users/Andrew Hooyman/Documents/ADIallzip")
myzip=read.csv("MyZip.csv")

#Neighborhood Atlas, All directories
adidir=list.dirs("C:/Users/Andrew Hooyman/Documents/ADIallzip")

#Read in individual 5 digit zip code data from ADI for national and statewide percentiles
#Preallocate variables
adifile=c()
#Final Parent data frame
usna=data.frame()
totalzip=0
for(i in 2:53){
  
  #Read in file names within each state ADI directory
  adifile=list.files(adidir[i])
  #Load in state ADI .csv with all columns treated as characters into a temporary data frame "temp"
  temp=read.csv(paste(adidir[i],"/",adifile[2],sep = ""),colClasses = c('character',rep(NULL,5)))
  #Add state abbreviation
  temp$state=substr(adifile[2],1,2)
  #Convert ADI for national and state percentile in numeric
  temp$ADI_NATRANK=as.numeric(temp$ADI_NATRANK)
  temp$ADI_STATERANK=as.numeric(temp$ADI_STATERANK)
  #Create variable that is 5 digit zip code only, "zip 5"
  temp$zip5=substr(temp$ZIP_4,1,5)
  #Generate average national and state percentile rank for each 5 digit zip code
  tempagg=aggregate(ADI_NATRANK~zip5+state,temp,mean)
  tempagg$ADI_STATERANK=unlist(aggregate(ADI_STATERANK~zip5+state,temp,mean)[3])
  #Put into parent data frame that will hold all state 5 digit zip code data
  usna=rbind(usna,tempagg)
  print(i)
  #To see total number of 9 digit zip codes across all states
  #totalzip=totalzip+dim(temp)[1]
  #print(totalzip)  
  
}

#Sanity Check
# zipall=merge(temp,tempagg,by="zip5")
# ggplot(zipall,aes(x=ADI_NATRANK.y,y=ADI_NATRANK.x))+
#   geom_point()

#Process my data
#Convert zip to character
myzip$zip5=as.character(myzip$zip5)

#add 0 to 4 digit zip code
for(i in 1:1397){
  if(nchar(myzip$zip5[i])<5){
    myzip$zip5[i]=paste("0",myzip$zip5[i],sep="")
  }
}



#ADIs=usna$ADI_STATERANK[usna$zip5 %in% myzip$zip5]

myzip=merge(usna,myzip,by="zip5",all.y=T)
#Surv=merge(Surv,myzip,by="id",all.x=T)
