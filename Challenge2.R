dataAnalysis_SF <- function()
{
## Data Incubator challenge QUestion 2: 
## use the R package "data.table" for handling large data sets
## install.packages("data.table")

library(data.table)

## read the data: 
data <- fread("Historic_Secured_Property_Tax_Rolls.csv")

## Remove rows with missing year number of block and lot number, these rows are senseless. 
data <- na.omit(data,cols= c("Closed Roll Fiscal Year", "Block and Lot Number"))

## sort data by block number and year & calculate total number assessments:
setkey(data,`Block and Lot Number`,`Closed Roll Fiscal Year`) 
numAssessments = dim(data)[1]



## Question 1: Find the fraction of assessments for properties of the most common class: 

## calculate the number of assessments for each property class: 
propertyClass <- data[,.N,by=`Property Class Code`] 

fraction <- propertyClass[,max(N)]/propertyClass[,sum(N)]
cat("Fraction of assessments for properties of most common class: ", format(fraction, digits=10),"\n") 


## Questions 2 & 3: 
## ===== Assessed improvement value data: use the latest assessments for each property.
dataIV <- data[,list(`Closed Roll Fiscal Year`, `Neighborhood Code`, `Block and Lot Number`, `Closed Roll Assessed Improvement Value`)]

## remove missing values in the improvement value column before analyzing it: 
dataIV <- na.omit(dataIV,"Closed Roll Assessed Improvement Value")
dataIV <- dataIV[`Neighborhood Code` !=""] ## remove empty neightborhood codes. 

## == Find the latest assessments for each property:
dataIV <- latestIndex(dataIV)


## the median of improvement value, consider only non-zero assessments,  
dataIV <- dataIV[`Closed Roll Assessed Improvement Value`!=0] ## remove zero improvement values. 
cat("Median assessed improvement value: ", format(median(dataIV[,`Closed Roll Assessed Improvement Value`]),digits=10),"\n")


## question 3: Average of the assessed improvement value in each neighborhood, then calculate max-min:
Average <- dataIV[,list(Avg = mean(`Closed Roll Assessed Improvement Value`)),by=`Neighborhood Code`]
cat("Difference between the greatest and least average improvement values: ", format(max(Average[,`Avg`]) - min(Average[,`Avg`]),digits=10),"\n")


## Question 4: Yearly growth rate of land value: 
dataLand <- data[,list(`Closed Roll Fiscal Year`, `Block and Lot Number`, `Closed Roll Assessed Land Value`)]
dataLand <- na.omit(dataLand,"Closed Roll Assessed Land Value")

dataLand <- dataLand[`Closed Roll Assessed Land Value` > 0]

t <- dataLand[,`Closed Roll Fiscal Year`]  ## time variable
t = as.numeric(t) ## convert to double. 

Y <- log(dataLand[,`Closed Roll Assessed Land Value`]) ## log of land value.


N <- length(t)
rate = (N*sum(Y*t) - (sum(Y)*sum(t)))/(N*sum(t^2) - (sum(t))^2)

cat("Yearly growth rate of land value: ", format(rate,digits=10),"\n")



## QUestion 5: Estimate the area of each neighborhood:
## ===== Assessed improvement value data: use the latest assessments for each property.
dataLocation <- data[,list(`Closed Roll Fiscal Year`, `Neighborhood Code`, `Block and Lot Number`, `Location`)]

## remove missing values in the improvement value column before analyzing it: 
dataLocation <- dataLocation[`Neighborhood Code` !="" & `Location` != ""]


## == Find the latest assessments for each property:
dataLocation <- latestIndex(dataLocation)

location <- dataLocation[,`Location`]; ## this is a character vector

N <- length(location)
latitude <- rep(0,N)
longitude <- rep(0,N)
a <- strsplit(location,",")

for (n in 1:N)
{
  b <- a[[n]]
  
  latitude[n] <- as.numeric(substr(b[1],2,nchar(b[1])))
  longitude[n] <- as.numeric(substr(b[2],1,nchar(b[2])-1))
}
a = sd(latitude)
b = sd(longitude)
area = pi*a*b; ## area of each lot. 

## area = pi*a*b*pi^2/180^2*6371^2; ## area of each lot. 

Neighborhood <- dataLocation[,`Neighborhood Code`]
Neighborhood <-table(Neighborhood)

Area = max(Neighborhood)*area
cat("Area of maximum neighborhood: ",format(Area,digits=10),"\n")




## Question 6: 
### ====== Building year data: use the earliest assessment for each property
dataBY <- data[,list(`Closed Roll Fiscal Year`, `Block and Lot Number`, `Year Property Built`, `Number of Units`)]

dataBY <- na.omit(dataBY,"Year Property Built")
dataBY <- na.omit(dataBY,"Number of Units") ## remove missing values

dataBY <- dataBY[`Number of Units` != 0]


## == Find the earliest assessments for each propertyL
dataBY <- earliestIndex(dataBY)


## the oldest building in SF was built in 1791. So remove years before 1700:
dataBYBefore1950 <- dataBY[`Year Property Built` < 1950 & `Year Property Built` > 1700]

dataBYAfter1950 <- dataBY[`Year Property Built` >= 1950 & `Year Property Built` <= 2015]

# difference between the average number of units built after and before 1950. 
cat("difference between the average number of units built after and before 1950: ",
    format(mean(dataBYAfter1950[,`Number of Units`]) - mean(dataBYBefore1950[,`Number of Units`]),digits=10),"\n")




### Question 7: 

dataBedRoom <- data[,list(`Closed Roll Fiscal Year`, `Block and Lot Number`, `Number of Bedrooms`,`Number of Units`,`Zipcode of Parcel`)]

## remove missing values in the improvement value column before analyzing it: 
dataBedRoom <- na.omit(dataBedRoom,"Number of Bedrooms")
dataBedRoom <- na.omit(dataBedRoom,"Number of Units")
dataBedRoom <- na.omit(dataBedRoom,"Zipcode of Parcel")

dataBedRoom <- dataBedRoom[`Number of Bedrooms` > 0 & `Number of Units` > 0]


## == Find the latest assessments for each property:
dataBedRoom <- latestIndex(dataBedRoom)

BedMean <- dataBedRoom[,.(Bed.mean=mean(`Number of Bedrooms`)), by=`Zipcode of Parcel`]
Unitmean <- dataBedRoom[,.(Unit.mean=mean(`Number of Units`)), by=`Zipcode of Parcel`]

cat("Maximum ratio of number of bedrooms and number of units: ", format(max(BedMean[,`Bed.mean`]/Unitmean[,`Unit.mean`]), digits=10),"\n")


## Question 8: 

dataArea <- data[,list(`Closed Roll Fiscal Year`, `Block and Lot Number`, `Property Area in Square Feet`,`Lot Area`,`Zipcode of Parcel`)]

## remove missing values in the improvement value column before analyzing it: 
dataArea <- na.omit(dataArea,"Property Area in Square Feet")
dataArea <- na.omit(dataArea,"Lot Area")
dataArea <- na.omit(dataArea,"Zipcode of Parcel")

## == Find the latest assessments for each property:
dataArea <- latestIndex(dataArea)


TotalPropertyArea <- dataArea[,.(prop.area = sum(`Property Area in Square Feet`)), by=`Zipcode of Parcel`]
TotalLandArea <- dataArea[,.(land.area = sum(`Lot Area`)), by=`Zipcode of Parcel`]

cat("Largest built-up ratio: ", format(max(TotalPropertyArea[,`prop.area`]/TotalLandArea[,`land.area`]),digits=10),"\n")

}


earliestIndex <- function(data)
{
  ## == Find the earliest and latest assessments for each property (for the next questions)
  blockCode <- data[,`Block and Lot Number`]
  blockCode <- factor(blockCode)
  blockCode <- table(blockCode) ## count the number of assessments for each block. 
  numCodes  <- length(blockCode) ## number of block and lot codes.
  
  earliestIdx <- rep(1,numCodes)  ## the earliest assessment of the first block is the first row
  latestIdx <- rep(blockCode[1],numCodes) ## the latest assessmnt of the first block is row number blockCode[1]
  
  for (n in 2:numCodes)
  {
    earliestIdx[n] <- earliestIdx[n-1] + blockCode[n-1] 
  }

  dataEL <- data[earliestIdx,] ## data with earliest assessment for each property
}

latestIndex <- function(data)
{
  ## == Find the earliest and latest assessments for each property (for the next questions)
  blockCode <- data[,`Block and Lot Number`]
  blockCode <- factor(blockCode)
  blockCode <- table(blockCode) ## count the number of assessments for each block. 
  numCodes  <- length(blockCode) ## number of block and lot codes.
  
  latestIdx <- rep(blockCode[1],numCodes) ## the latest assessmnt of the first block is row number blockCode[1]
  
  for (n in 2:numCodes)
  {
    latestIdx[n] <- latestIdx[n-1] + blockCode[n]
  }

  dataLT <- data[latestIdx,]   ## data with latest assessment of each property. 
}






