
## ---------------------------
##
## Script name: Velocity Calculator
##
## Author: Oliver Hugh
##
## Date Created: 2023-12-10
##
## Copyright (c) Perinatal Institute, 2023
## Email: data@perinatal.org.uk
##
## ---------------------------
##
## Notes: All methods are explained in https://doi.org/10.1002/uog.24860.
##   Please input your data in the Data Input section with identical column names
##
## ---------------------------

library(dplyr)

#=========================================================
# Limit Generation 
#=========================================================
tab <- matrix(c(13, -7.3, 10.8, 14, -7.0, 9.9, 21, -7.8, 8.6, 28, -8.2, 8.7,35,-8.8,9,42,-9.3,9.2,49,-9.1,9.8,56,-10.1,10), ncol=3, byrow=TRUE)

dataframe_data=as.data.frame(tab) 
colnames(dataframe_data) <- c('Gestation','Limit_S','Limit_A')
Limits <- matrix(c(1:140,ncol=1))
dataframe_Limits=distinct(as.data.frame(Limits))
colnames(dataframe_Limits) <- c('Gestation')
dataframe_Limits$ugest=floor(dataframe_Limits$Gestation/7)*7
dataframe_Limits$ogest=ceiling(dataframe_Limits$Gestation/7)*7
dataframe_Limits$ugest<- ifelse(dataframe_Limits$Gestation<14, 13, dataframe_Limits$ugest)
dataframe_Limits$ogest<- ifelse(dataframe_Limits$Gestation<14, 13, dataframe_Limits$ogest)
dataframe_Limits$ugest<- ifelse(dataframe_Limits$Gestation>56, 56, dataframe_Limits$ugest)
dataframe_Limits$ogest<- ifelse(dataframe_Limits$Gestation>56, 56, dataframe_Limits$ogest)
join <- merge(x=dataframe_Limits, y=dataframe_data, by.x="ogest", by.y="Gestation", all=TRUE)
join2 <- merge(x=join, y=dataframe_data, by.x="ugest", by.y="Gestation", all=TRUE)
join2$frac <- ifelse(join2$Gestation>=14 & join2$Gestation<56 , (join2$Gestation - join2$ugest)/7, 0)
join2$SLim = join2$Limit_S.y + join2$frac*(join2$Limit_S.x-join2$Limit_S.y)
join2$ALim = join2$Limit_A.y + join2$frac*(join2$Limit_A.x-join2$Limit_A.y)

limits = data.frame(subset(join2, select = c(Gestation, SLim, ALim)))

#=========================================================
# Data Input - Please clear example dataset
#=========================================================

test<-as.data.frame(matrix(c(235, 2300, 260, 2600, 224, 1500, 256, 3400, 226, 2500, 245, 3000), ncol=4, byrow=TRUE))
colnames(test) <- c('gest1', 'efw1', 'gest2','efw2')

#=========================================================
# Data Processing
#=========================================================

test$diff = test$gest2 - test$gest1
LimJoin <- merge(x=test, y=limits, by.x="diff", by.y="Gestation")

LimJoin$HADP50Scan1<-exp(0.578+0.332*(LimJoin$gest1/7)-0.00354*(LimJoin$gest1/7)^2)
LimJoin$HADP50Scan2<-exp(0.578+0.332*(LimJoin$gest2/7)-0.00354*(LimJoin$gest2/7)^2)

LimJoin$Expect<- LimJoin$efw1 * (LimJoin$HADP50Scan2/LimJoin$HADP50Scan1)
LimJoin$Lower <- floor(LimJoin$Expect*(100+LimJoin$SLim)/100)
LimJoin$Upper <- floor(LimJoin$Expect*(100+LimJoin$ALim)/100)
LimJoin$Expect<- floor(LimJoin$Expect)
LimJoin$Class <- ifelse(LimJoin$efw2<LimJoin$Lower, "Slow","Normal")
LimJoin$Class <- ifelse(LimJoin$efw2>LimJoin$Upper, "Accelerated",LimJoin$Class)

#=========================================================
# Data Output
#=========================================================

FinalTable = subset(LimJoin, select= -c(HADP50Scan1, HADP50Scan2, SLim, ALim, diff))
FinalTable


