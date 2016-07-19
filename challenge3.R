## load data for home price prediction: 
## need to install packages "httr", "jsonlite" and "xlm2", "readr", "data.table"

library(httr)
library(jsonlite)
library(xml2)
library(readr)
library(data.table)


## load the neighborhood data: 
URL <- "http://api.trulia.com/webservices.php"

Lib <- "library=LocationInfo"
Func <- "function=getNeighborhoodsInCity"
City <- "city=Chicago"
State <- "state=IL"
apiKey <- "apikey=ryr7ngde5p2tyzy95y87nfje"

API1 <- GET(paste(URL,"?",Lib,"&",Func,"&",City,"&",State,"&",apiKey,sep=""))

Text <- content(API1,as = "text")

idx1 = gregexpr("<neighborhood><id>",Text)
idx1 <- as.numeric(idx1[[1]])

idx2 <- gregexpr("</id><name>",Text)
idx2 <- as.numeric(idx2[[1]])

idx3 <- gregexpr("</name></neighborhood",Text)
idx3<-as.numeric(idx3[[1]])

numNeigh = length(idx1)
NeighborhoodID = rep(0,numNeigh)
NeighborhoodName = rep("",numNeigh)

for (n in 1:numNeigh)
{
  NeighborhoodID[n] <- as.numeric(substr(Text,start=idx1[n]+18,stop = idx2[n]-1))
  NeighborhoodName[n] <- substr(Text,start = idx2[n]+11, stop= idx3[n]-1) 
}

## combine the ID and name in a table: 
Neighborhood <- data.table(NeighborhoodID,NeighborhoodName)



## load the home price data: 
Lib <- "library=TruliaStats"
Func <- "function=getNeighborhoodStats"
NeighborhoodStat <- "neighborhoodId=2877"
startDate <- "startDate=2008-01-01"
endDate <- "endDate=2016-06-30"

API2 <- GET(paste(URL,"?",Lib,"&",Func,"&",NeighborhoodStat,"&",startDate,"&",endDate,"&","statType=listings","&",apiKey,sep=""))
Text <- content(API2,as="text")


idx1<- gregexpr("<weekEndingDate>",Text)
idx1 <- as.numeric(idx1[[1]])
idx1 <- c(idx1,nchar(Text))  ## add the end character for use in the loop

idx2 <- gregexpr("</weekEndingDate>",Text)
idx2 <- as.numeric(idx2[[1]])


numWeeks <- length(idx2)
WeekEndingDate <- 1:numWeeks
class(WeekEndingDate) <- "Date"

AvgPrice_All <- rep(NA,numWeeks)
AvgPrice_1BR <- rep(NA,numWeeks)
AvgPrice_2BR <- rep(NA,numWeeks)
AvgPrice_3BR <- rep(NA,numWeeks)
AvgPrice_4BR <- rep(NA,numWeeks)
AvgPrice_5BR <- rep(NA,numWeeks)
AvgPrice_6BR <- rep(NA,numWeeks)


for (n in 1:numWeeks)
{
  WeekEndingDate[n] <- as.Date(substr(Text,idx1[n]+ 16, idx2[n]-1),format="%Y-%m-%d")## date
  subText <- substr(Text,idx1[n],idx1[n+1])
  
  idx5 <- gregexpr("<averageListingPrice>",subText)
  idx5 <- as.numeric(idx5[[1]])
  idx6 <- gregexpr("</averageListingPrice>",subText)
  idx6 <- as.numeric(idx6[[1]])
  
  AvgPrice_All[n] <- as.numeric(substr(subText,start=idx5[1]+21,stop=idx6[1]-1))
  AvgPrice_1BR[n] <- as.numeric(substr(subText,start=idx5[2]+21,stop=idx6[2]-1))
  AvgPrice_2BR[n] <- as.numeric(substr(subText,start=idx5[3]+21,stop=idx6[3]-1))
  AvgPrice_3BR[n] <- as.numeric(substr(subText,start=idx5[4]+21,stop=idx6[4]-1))
  AvgPrice_4BR[n] <- as.numeric(substr(subText,start=idx5[5]+21,stop=idx6[5]-1))
  AvgPrice_5BR[n] <- as.numeric(substr(subText,start=idx5[6]+21,stop=idx6[6]-1))
  AvgPrice_6BR[n] <- as.numeric(substr(subText,start=idx5[7]+21,stop=idx6[7]-1))
  
}


AvgPrice <-data.table(WeekEndingDate,AvgPrice_1BR,AvgPrice_2BR,AvgPrice_3BR,AvgPrice_4BR,AvgPrice_5BR,AvgPrice_6BR,AvgPrice_All)

## remove repetitions:
AvgPrice <- AvgPrice[,.(AvgPrice_1BR=mean(`AvgPrice_1BR`),AvgPrice_2BR=mean(`AvgPrice_2BR`),AvgPrice_3BR=mean(`AvgPrice_3BR`),AvgPrice_4BR=mean(`AvgPrice_4BR`),AvgPrice_5BR=mean(`AvgPrice_5BR`),AvgPrice_6BR=mean(`AvgPrice_6BR`),AvgPrice_All=mean(`AvgPrice_All`)),by=`WeekEndingDate`]


API <- GET("https://www.quandl.com/api/v3/datasets/BLSI/CWSR0000SAF11211.csv")
inflationData <- content(API)
