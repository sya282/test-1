# Future Demand Nexus
# Renee Obringer
# 31 May 2019

# ***load midwestdata.Rdata

# libraries
library(mvtboost)
library(RColorBrewer)
library(ggplot2)
library(TSA)
library(gbm)
library(gridExtra)
library(gbm)
library(zoo)
library(measurements)
library(patternplot)
library(pracma)

# set working directory
setwd("/Users/robringe/OneDrive - purdue.edu/Research/data/futureClimate")

#################################   LOAD DATA - Future Climate ###################
indy85 <- list(my_data$IndyGFDL_rcp85, my_data$IndyHadGEM_rcp84, my_data$IndyIPSL_rcp85, my_data$IndyMIROC_rcp85, my_data$IndyNorESM_rcp85)
cbus85 <- list(my_data$ColumbusGFDL_rcp85, my_data$ColumbusHadGEM_rcp85, my_data$ColumbusIPSL_rcp85, my_data$ColumbusMIROC_rcp85, my_data$ColumbusNorESM_rcp85)
chi85 <- list(my_data$ChicagoGFDL_rcp85, my_data$ChicagoHadGEM_rcp85, my_data$ChicagoIPSL_rcp85, my_data$ChicagoMIROC_rcp85, my_data$ChicagoNorESM_rcp85)
clev85 <- list(my_data$ClevelandGFDL_rcp85, my_data$ClevelandHadGEM_rcp85, my_data$ClevelandIPSL_rcp85, my_data$ClevelandMIROC_rcp85, my_data$ClevelandNorESM_rcp85)
mad85 <- list(my_data$MadisonGFDL_rcp85, my_data$MadisonHadGEM_rcp85, my_data$MadisonIPSL_rcp85, my_data$MadisonMIROC_rcp85, my_data$MadisonNorESM_rcp85)
minn85 <- list(my_data$MinnGFDL_rcp85, my_data$MinnHadGEM_rcp85, my_data$MinnIPSL_rcp85, my_data$MinnMIROC_rcp85, my_data$MinnNorESM_rcp85)

indy60 <- list(my_data$IndyGFDL_rcp60, my_data$IndyHadGEM_rcp60, my_data$IndyIPSL_rcp60, my_data$IndyMIROC_rcp60, my_data$IndyNorESM_rcp60)
cbus60 <- list(my_data$ColumbusGFDL_rcp60, my_data$ColumbusHadGEM_rcp60, my_data$ColumbusIPSL_rcp60, my_data$ColumbusMIROC_rcp60, my_data$ColumbusNorESM_rcp60)
chi60 <- list(my_data$ChicagoGFDL_rcp60, my_data$ChicagoHadGEM_rcp60, my_data$ChicagoIPSL_rcp60, my_data$ChicagoMIROC_rcp60, my_data$ChicagoNorESM_rcp60)
clev60 <- list(my_data$ClevelandGFDL_rcp60, my_data$ClevelandHadGEM_rcp60, my_data$ClevelandIPSL_rcp60, my_data$ClevelandMIROC_rcp60, my_data$ClevelandNorESM_rcp60)
mad60 <- list(my_data$MadisonGFDL_rcp60, my_data$MadisonHadGEM_rcp60, my_data$MadisonIPSL_rcp60, my_data$MadisonMIROC_rcp60, my_data$MadisonNorESM_rcp60)
minn60 <- list(my_data$MinnGFDL_rcp60, my_data$MinnHadGEM_rcp60, my_data$MinnIPSL_rcp60, my_data$MinnMIROC_rcp60, my_data$MinnNorESM_rcp60)

indy45 <- list(my_data$IndyGFDL_rcp45, my_data$IndyHadGEM_rcp45, my_data$IndyIPSL_rcp45, my_data$IndyMIROC_rcp45, my_data$IndyNorESM_rcp45)
cbus45 <- list(my_data$ColumbusGFDL_rcp45, my_data$ColumbusHadGEM_rcp45, my_data$ColumbusIPSL_rcp45, my_data$ColumbusMIROC_rcp45, my_data$ColumbusNorESM_rcp45)
chi45 <- list(my_data$ChicagoGFDL_rcp45, my_data$ChicagoHadGEM_rcp45, my_data$ChicagoIPSL_rcp45, my_data$ChicagoMIROC_rcp45, my_data$ChicagoNorESM_rcp45)
clev45 <- list(my_data$ClevelandGFDL_rcp45, my_data$ClevelandHadGEM_rcp45, my_data$ClevelandIPSL_rcp45, my_data$ClevelandMIROC_rcp45, my_data$ClevelandNorESM_rcp45)
mad45 <- list(my_data$MadisonGFDL_rcp45, my_data$MadisonHadGEM_rcp45, my_data$MadisonIPSL_rcp45, my_data$MadisonMIROC_rcp45, my_data$MadisonNorESM_rcp45)
minn45 <- list(my_data$MinnGFDL_rcp45, my_data$MinnHadGEM_rcp45, my_data$MinnIPSL_rcp45, my_data$MinnMIROC_rcp45, my_data$MinnNorESM_rcp45)

indy26 <- list(my_data$IndyGFDL_rcp26, my_data$IndyHadGEM_rcp26, my_data$IndyIPSL_rcp26, my_data$IndyMIROC_rcp26, my_data$IndyNorESM_rcp26)
cbus26 <- list(my_data$ColumbusGFDL_rcp26, my_data$ColumbusHadGEM_rcp26, my_data$ColumbusIPSL_rcp26, my_data$ColumbusMIROC_rcp26, my_data$ColumbusNorESM_rcp26)
chi26 <- list(my_data$ChicagoGFDL_rcp26, my_data$ChicagoHadGEM_rcp26, my_data$ChicagoIPSL_rcp26, my_data$ChicagoMIROC_rcp26, my_data$ChicagoNorESM_rcp26)
clev26 <- list(my_data$ClevelandGFDL_rcp26, my_data$ClevelandHadGEM_rcp26, my_data$ClevelandIPSL_rcp26, my_data$ClevelandMIROC_rcp26, my_data$ClevelandNorESM_rcp26)
mad26 <- list(my_data$MadisonGFDL_rcp26, my_data$MadisonHadGEM_rcp26, my_data$MadisonIPSL_rcp26, my_data$MadisonMIROC_rcp26, my_data$MadisonNorESM_rcp26)
minn26 <- list(my_data$MinnGFDL_rcp26, my_data$MinnHadGEM_rcp826, my_data$MinnIPSL_rcp26, my_data$MinnMIROC_rcp26, my_data$MinnNorESM_rcp26)

#################################   FORMAT DATA  ###########################

colnames(chiC)[9] <- "MAXWindSpeed"
colnames(chiC)[10] <- "AVERAGEWindSpeed"
colnames(chiC)[11] <- "ACCPrecip"
colnames(indyC)[3] <- "WaterGalcap"

cities <- list(chiC, clevC, cbusC, indyC, madC, minnC)

# Convert to metric units 
for (i in 1:6) {
cities[[i]][,5] <- conv_unit(cities[[i]][,5],"F","C")             # deg F --> deg C
cities[[i]][,6] <- conv_unit(cities[[i]][,6],"F","C")             # deg F --> deg C
cities[[i]][,9] <- conv_unit(cities[[i]][,9],"mph","m_per_sec")   # mph --> m/s
cities[[i]][,10] <- conv_unit(cities[[i]][,10],"mph","m_per_sec") # mph --> m/s
cities[[i]][,11] <- conv_unit(cities[[i]][,11],"inch","cm")       # in --> cm
cities[[i]][,3] <- conv_unit(cities[[i]][,3],"us_gal","l")        # gal/cap --> L/cap
cities[[i]][,13] <- conv_unit(cities[[i]][,13],"F","C")             # deg F --> deg C

}

# detrend data (Sailor and Munoz 1997)
for (i in 1:6) {
  fullmean <- colMeans(cities[[i]][,3:4]) # get mean over entire period
  yearlymeans <- aggregate(x = cities[[i]][,3:4], by = list(cities[[i]][,1]), FUN = mean) # get yearly means
  yearlymeans <- yearlymeans[,2:3]
  
  # get adjustment term for each year and each variable
  fadj <- matrix(numeric(0), nrow = 10, ncol = 2)
  for (y in 1:10) {
    for (v in 1:2) {
      fadj[y,v] <- yearlymeans[y,v]/fullmean[v]
    }
  }
  
  # convert from yearly to monthly matrix
  monthlyfadj <- fadj[rep(1:nrow(fadj), c(12,12,12,12,12,12,12,12,12,12)), ]

  # remove trend
  detrendedwat <- cities[[i]][,3]/monthlyfadj[,1]
  detrendedele <- cities[[i]][,4]/monthlyfadj[,2]

  cities[[i]][,3] <- detrendedwat
  cities[[i]][,4] <- detrendedele
}

# future data
chidata <- list(chi85,chi60,chi45,chi26)
clevdata <- list(clev85,clev60,clev45,clev26)
cbusdata <- list(cbus85,cbus60,cbus45,cbus26)
indydata <- list(indy85,indy60,indy45,indy26)
maddata <- list(mad85,mad60,mad45,mad26)
minndata <- list(minn85,minn60,minn45,minn26)

futdata <- list(chidata,clevdata,cbusdata,indydata,maddata,minndata)
# futdata[[CITY]][[SCENARIO]][[MODEL]][row,col]

colnames(futdata[[1]][[4]][[3]])[colnames(futdata[[1]][[4]][[3]])=='DewPpint'] <- 'DewPoint'
colnames(futdata[[3]][[2]][[3]])[colnames(futdata[[3]][[2]][[3]])=='DrybBulbT'] <- 'DryBulbT'
colnames(futdata[[6]][[2]][[4]])[colnames(futdata[[6]][[2]][[4]])=='WindSpeed...6'] <- 'WindSpeed'
colnames(futdata[[6]][[2]][[4]])[colnames(futdata[[6]][[2]][[4]])=='WindSpeed...7'] <- 'Precip'



#################################   CROSS VALIDATION #############################

# Commented out because summer/winter analysis needs its own CV

# k <- 5
# n <- nrow(chiC)
# 
# for (c in 1:6) {
#   set.seed(12)
#   cities[[c]] <- cities[[c]][sample(n),] 
# }
# 
# folds <- cut(seq(1,n),breaks = k, labels = FALSE)

#################################   VARIABLE SELECTION  ###########################

preds <- list()
relinf <- list()

for (i in 1:6) {
  
  # run model
  Y <- cities[[i]][,3:4] # response
  X <- cities[[i]][,5:12] 
  #names(X) <- c('Dry Bulb Temp.','Dew Point Temp.','Max. Relative Humidity','Avg. Relative Humidity','Max. Wind Speed','Avg. Wind Speed','Precipitation','ENSO')
  #names(Y) <- c('Water Use','Electricity Use')
  
  Ys <- scale(Y)
  
  out <- mvtb(Y=Ys,X=X, n.trees=1000, shrinkage=.01, interaction.depth=3,cv.folds = 5)

  relinf[[i]] <- mvtb.ri(out,relative = "col")
  
  # numformat <- function(val){sub("^(-?)0.", "\\1.", sprintf("%5.2f", val))}
  # blues <- c('#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c','#08306b')
  # bgr <- colorRampPalette(blues,space = "Lab",bias = 7)
  # par(mar=c(10,10,1,1),mfrow=c(1,1),cex=0.9)
  mvtb.heat(t(relinf[[i]]),clust.method = NULL,cexRow=1,cexCol=1,numformat=numformat, col = bgr(500))

  limit <- 5 #quantile(relinf[[i]], 0.75) # percentile
  impvar <- (rowSums(relinf[[i]]>limit))>0
  names <- row.names(data.frame(impvar[which(impvar==T)]))
  
  print(names)
  
  preds[[i]] <- X[,names]
}

# regional relative influence
totrelinf <- relinf[[1]]+relinf[[2]]+relinf[[3]]+relinf[[4]]+relinf[[5]]+relinf[[6]]
mvtb.heat(t(totrelinf),clust.method = NULL,cexRow=1,cexCol=1,numformat=numformat, col = bgr(500))

limit <- quantile(totrelinf, 0.5) # percentile
impvar <- (rowSums(totrelinf>limit))>0
names <- row.names(data.frame(impvar[which(impvar==T)]))

print(names)

regpreds <- X[,names]

#################################   MODEL RUN w/ ORIGINAL SF  ###########################

# initialize
yhatall <- list()

for (i in 1:6) {
  
  # run model
  Y <- cities[[i]][,3:4] # response
  X <- cities[[i]][,c(12,6,8,5,10)] 
  names(X) <- c('ENSO','Dew Point','Relative Humidity','Dry Bulb Temp.','Wind Speed')
  names(Y) <- c('Water Use','Electricity Use')
  
  out <- mvtb(Y=Y,X=X, n.trees=1000, shrinkage=.01, interaction.depth=3)
  
  # variable influence
  # numformat <- function(val){sub("^(-?)0.", "\\1.", sprintf("%.1f", val))}
  # blues <- c('#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c','#08306b')
  # bgr <- colorRampPalette(blues,space = "Lab",bias = 7)
  # par(mar=c(15,10,1,25))
  # mvtb.heat(t(mvtb.ri(out)),clust.method = NULL,cexRow=1,cexCol=1,numformat=numformat,col = bgr(500))

  # get future projections for each scenario/model
  
  # initialize
  predscenarios <- list()
  
  for (s in 1:4) {
    # initialize
    predvalues <- list()
    for (m in 1:5) {
      # fit model
      yhat <- predict(out,newdata=futdata[[i]][[s]][[m]][,2:6])
      
      # store data
      predvalues[[m]] <- yhat
    }
    predscenarios[[s]] <- predvalues
  }
  
  yhatall[[i]] <- predscenarios
  
}


#################################   ANALYSIS #############################

# percent change in mean from 1971-2000 to mean in last 30 years when 1.5 deg threshold was reached (pos == % increase) [new-old/old]

# HadGEM RCP2.6 --> 2007-2036 ([433:792,])
pcW26HadGEM15 <- c()
pcE26HadGEM15 <- c()
for (i in 1:6) {
  pcW26HadGEM15[i] <- ((mean(yhatall[[i]][[4]][[2]][433:792,1])-mean(yhatall[[i]][[4]][[2]][1:360,1]))/mean(yhatall[[i]][[4]][[2]][1:360,1]))*100 
  pcE26HadGEM15[i] <- ((mean(yhatall[[i]][[4]][[2]][433:792,2])-mean(yhatall[[i]][[4]][[2]][1:360,2]))/mean(yhatall[[i]][[4]][[2]][1:360,2]))*100 
}

# IPSL RCP2.6 --> 2008-2037 ([445:804,])
pcW26IPSL15 <- c()
pcE26IPSL15 <- c()
for (i in 1:6) {
  pcW26IPSL15[i] <- ((mean(yhatall[[i]][[4]][[3]][445:804,1])-mean(yhatall[[i]][[4]][[3]][1:360,1]))/mean(yhatall[[i]][[4]][[3]][1:360,1]))*100 
  pcE26IPSL15[i] <- ((mean(yhatall[[i]][[4]][[3]][445:804,2])-mean(yhatall[[i]][[4]][[3]][1:360,2]))/mean(yhatall[[i]][[4]][[3]][1:360,2]))*100 
}

# MIROC RCP2.6 --> 2006-2035 ([421:780,])
pcW26MIROC15 <- c()
pcE26MIROC15 <- c()
for (i in 1:6) {
  pcW26MIROC15[i] <- ((mean(yhatall[[i]][[4]][[4]][421:780,1])-mean(yhatall[[i]][[4]][[4]][1:360,1]))/mean(yhatall[[i]][[4]][[4]][1:360,1]))*100 
  pcE26MIROC15[i] <- ((mean(yhatall[[i]][[4]][[4]][421:780,2])-mean(yhatall[[i]][[4]][[4]][1:360,2]))/mean(yhatall[[i]][[4]][[4]][1:360,2]))*100 
}

# NorESM RCP2.6 --> 2047-2076  ([913:1272,])
pcW26NorESM15 <- c()
pcE26NorESM15 <- c()
for (i in 1:6) {
  pcW26NorESM15[i] <- ((mean(yhatall[[i]][[4]][[5]][913:1272,1])-mean(yhatall[[i]][[4]][[5]][1:360,1]))/mean(yhatall[[i]][[4]][[5]][1:360,1]))*100 
  pcE26NorESM15[i] <- ((mean(yhatall[[i]][[4]][[5]][913:1272,2])-mean(yhatall[[i]][[4]][[5]][1:360,2]))/mean(yhatall[[i]][[4]][[5]][1:360,2]))*100 
}

# GFDL RCP8.5 --> 2021-2050 ([601:960,])
pcW85GFDL15 <- c()
pcE85GFDL15 <- c()
for (i in 1:6) {
  pcW85GFDL15[i] <- ((mean(yhatall[[i]][[1]][[1]][601:960,1])-mean(yhatall[[i]][[4]][[2]][1:360,1]))/mean(yhatall[[i]][[1]][[1]][1:360,1]))*100 
  pcE85GFDL15[i] <- ((mean(yhatall[[i]][[1]][[1]][601:960,2])-mean(yhatall[[i]][[4]][[2]][1:360,2]))/mean(yhatall[[i]][[1]][[1]][1:360,2]))*100 
}

# HadGEM RCP8.5 --> 2004-2033 ([397:756,])
pcW85HadGEM15 <- c()
pcE85HadGEM15 <- c()
for (i in 1:6) {
  pcW85HadGEM15[i] <- ((mean(yhatall[[i]][[1]][[2]][397:756,1])-mean(yhatall[[i]][[1]][[2]][1:360,1]))/mean(yhatall[[i]][[1]][[2]][1:360,1]))*100 
  pcE85HadGEM15[i] <- ((mean(yhatall[[i]][[1]][[2]][397:756,2])-mean(yhatall[[i]][[1]][[2]][1:360,2]))/mean(yhatall[[i]][[1]][[2]][1:360,2]))*100 
}

# IPSL RCP8.5 --> 2006-2035 ([421:780,])
pcW85IPSL15 <- c()
pcE85IPSL15 <- c()
for (i in 1:6) {
  pcW85IPSL15[i] <- ((mean(yhatall[[i]][[1]][[3]][421:780,1])-mean(yhatall[[i]][[1]][[3]][1:360,1]))/mean(yhatall[[i]][[1]][[3]][1:360,1]))*100 
  pcE85IPSL15[i] <- ((mean(yhatall[[i]][[1]][[3]][421:780,2])-mean(yhatall[[i]][[1]][[3]][1:360,2]))/mean(yhatall[[i]][[1]][[3]][1:360,2]))*100 
}

# MIROC RCP8.5 --> 2006-2035 ([421:780,])
pcW85MIROC15 <- c()
pcE85MIROC15 <- c()
for (i in 1:6) {
  pcW85MIROC15[i] <- ((mean(yhatall[[i]][[1]][[4]][421:780,1])-mean(yhatall[[i]][[1]][[4]][1:360,1]))/mean(yhatall[[i]][[1]][[4]][1:360,1]))*100 
  pcE85MIROC15[i] <- ((mean(yhatall[[i]][[1]][[4]][421:780,2])-mean(yhatall[[i]][[1]][[4]][1:360,2]))/mean(yhatall[[i]][[1]][[4]][1:360,2]))*100 
}

# NorESM RCP8.5 --> 2016-2045 ([541:900,])
pcW85NorESM15 <- c()
pcE85NorESM15 <- c()
for (i in 1:6) {
  pcW85NorESM15[i] <- ((mean(yhatall[[i]][[1]][[5]][541:900,1])-mean(yhatall[[i]][[1]][[5]][1:360,1]))/mean(yhatall[[i]][[1]][[5]][1:360,1]))*100 
  pcE85NorESM15[i] <- ((mean(yhatall[[i]][[1]][[5]][541:900,2])-mean(yhatall[[i]][[1]][[5]][1:360,2]))/mean(yhatall[[i]][[1]][[5]][1:360,2]))*100 
}

# percent change in mean from 1971-2000 to mean in last 30 years when 2.0 deg threshold was reached (pos == % increase) [new-old/old]

# HadGEM RCP2.6 --> 2029-2058 ([697:1056,])
pcW26HadGEM20 <- c()
pcE26HadGEM20 <- c()
for (i in 1:6) {
  pcW26HadGEM20[i] <- ((mean(yhatall[[i]][[4]][[2]][697:1056,1])-mean(yhatall[[i]][[4]][[2]][1:360,1]))/mean(yhatall[[i]][[4]][[2]][1:360,1]))*100 
  pcE26HadGEM20[i] <- ((mean(yhatall[[i]][[4]][[2]][697:1056,2])-mean(yhatall[[i]][[4]][[2]][1:360,2]))/mean(yhatall[[i]][[4]][[2]][1:360,2]))*100 
}

# IPSL RCP2.6 --> 2060-2089 ([1069:1428,])
pcW26IPSL20 <- c()
pcE26IPSL20 <- c()
for (i in 1:6) {
  pcW26IPSL20[i] <- ((mean(yhatall[[i]][[4]][[3]][1069:1428,1])-mean(yhatall[[i]][[4]][[3]][1:360,1]))/mean(yhatall[[i]][[4]][[3]][1:360,1]))*100 
  pcE26IPSL20[i] <- ((mean(yhatall[[i]][[4]][[3]][1069:1428,2])-mean(yhatall[[i]][[4]][[3]][1:360,2]))/mean(yhatall[[i]][[4]][[3]][1:360,2]))*100 
}

# MIROC RCP2.6 --> 2023-2052 ([625:984,])
pcW26MIROC20 <- c()
pcE26MIROC20 <- c()
for (i in 1:6) {
  pcW26MIROC20[i] <- ((mean(yhatall[[i]][[4]][[4]][625:984,1])-mean(yhatall[[i]][[4]][[4]][1:360,1]))/mean(yhatall[[i]][[4]][[4]][1:360,1]))*100 
  pcE26MIROC20[i] <- ((mean(yhatall[[i]][[4]][[4]][625:984,2])-mean(yhatall[[i]][[4]][[4]][1:360,2]))/mean(yhatall[[i]][[4]][[4]][1:360,2]))*100 
}

# GFDL RCP8.5 --> 2038-2067 ([805:1164,])
pcW85GFDL20 <- c()
pcE85GFDL20 <- c()
for (i in 1:6) {
  pcW85GFDL20[i] <- ((mean(yhatall[[i]][[1]][[1]][805:1164,1])-mean(yhatall[[i]][[4]][[2]][1:360,1]))/mean(yhatall[[i]][[1]][[1]][1:360,1]))*100 
  pcE85GFDL20[i] <- ((mean(yhatall[[i]][[1]][[1]][805:1164,2])-mean(yhatall[[i]][[4]][[2]][1:360,2]))/mean(yhatall[[i]][[1]][[1]][1:360,2]))*100 
}

# HadGEM RCP8.5 --> 2016-2045 ([541:900,])
pcW85HadGEM20 <- c()
pcE85HadGEM20 <- c()
for (i in 1:6) {
  pcW85HadGEM20[i] <- ((mean(yhatall[[i]][[1]][[2]][541:900,1])-mean(yhatall[[i]][[1]][[2]][1:360,1]))/mean(yhatall[[i]][[1]][[2]][1:360,1]))*100 
  pcE85HadGEM20[i] <- ((mean(yhatall[[i]][[1]][[2]][541:900,2])-mean(yhatall[[i]][[1]][[2]][1:360,2]))/mean(yhatall[[i]][[1]][[2]][1:360,2]))*100 
}

# IPSL RCP8.5 --> 2018-2047 ([565:924,])
pcW85IPSL20 <- c()
pcE85IPSL20 <- c()
for (i in 1:6) {
  pcW85IPSL20[i] <- ((mean(yhatall[[i]][[1]][[3]][565:924,1])-mean(yhatall[[i]][[1]][[3]][1:360,1]))/mean(yhatall[[i]][[1]][[3]][1:360,1]))*100 
  pcE85IPSL20[i] <- ((mean(yhatall[[i]][[1]][[3]][565:924,2])-mean(yhatall[[i]][[1]][[3]][1:360,2]))/mean(yhatall[[i]][[1]][[3]][1:360,2]))*100 
}

# MIROC RCP8.5 --> 2017-2046 ([553:912,])
pcW85MIROC20 <- c()
pcE85MIROC20 <- c()
for (i in 1:6) {
  pcW85MIROC20[i] <- ((mean(yhatall[[i]][[1]][[4]][553:912,1])-mean(yhatall[[i]][[1]][[4]][1:360,1]))/mean(yhatall[[i]][[1]][[4]][1:360,1]))*100 
  pcE85MIROC20[i] <- ((mean(yhatall[[i]][[1]][[4]][553:912,2])-mean(yhatall[[i]][[1]][[4]][1:360,2]))/mean(yhatall[[i]][[1]][[4]][1:360,2]))*100 
}

# NorESM RCP8.5 --> 2031-2060 ([721:1080,])
pcW85NorESM20 <- c()
pcE85NorESM20 <- c()
for (i in 1:6) {
  pcW85NorESM20[i] <- ((mean(yhatall[[i]][[1]][[5]][721:1080,1])-mean(yhatall[[i]][[1]][[5]][1:360,1]))/mean(yhatall[[i]][[1]][[5]][1:360,1]))*100 
  pcE85NorESM20[i] <- ((mean(yhatall[[i]][[1]][[5]][721:1080,2])-mean(yhatall[[i]][[1]][[5]][1:360,2]))/mean(yhatall[[i]][[1]][[5]][1:360,2]))*100 
}

# percent change in mean from 1971-2000 to mean in last 30 years when 3.0 deg threshold was reached (pos == % increase) [new-old/old]

# GFDLRCP8.5 --> 2067-2096 ([1153:1512,])
pcW85GFDL30 <- c()
pcE85GFDL30 <- c()
for (i in 1:6) {
  pcW85GFDL30[i] <- ((mean(yhatall[[i]][[1]][[1]][1153:1512,1])-mean(yhatall[[i]][[4]][[2]][1:360,1]))/mean(yhatall[[i]][[1]][[1]][1:360,1]))*100 
  pcE85GFDL30[i] <- ((mean(yhatall[[i]][[1]][[1]][1153:1512,2])-mean(yhatall[[i]][[4]][[2]][1:360,2]))/mean(yhatall[[i]][[1]][[1]][1:360,2]))*100 
}

# HadGEM RCP8.5 --> 2035-2064 ([769:1128,])
pcW85HadGEM30 <- c()
pcE85HadGEM30 <- c()
for (i in 1:6) {
  pcW85HadGEM30[i] <- ((mean(yhatall[[i]][[1]][[2]][769:1128,1])-mean(yhatall[[i]][[1]][[2]][1:360,1]))/mean(yhatall[[i]][[1]][[2]][1:360,1]))*100 
  pcE85HadGEM30[i] <- ((mean(yhatall[[i]][[1]][[2]][769:1128,2])-mean(yhatall[[i]][[1]][[2]][1:360,2]))/mean(yhatall[[i]][[1]][[2]][1:360,2]))*100 
}

# IPSL RCP8.5 --> 2038-2067 ([805:1164,])
pcW85IPSL30 <- c()
pcE85IPSL30 <- c()
for (i in 1:6) {
  pcW85IPSL30[i] <- ((mean(yhatall[[i]][[1]][[3]][805:1164,1])-mean(yhatall[[i]][[1]][[3]][1:360,1]))/mean(yhatall[[i]][[1]][[3]][1:360,1]))*100 
  pcE85IPSL30[i] <- ((mean(yhatall[[i]][[1]][[3]][805:1164,2])-mean(yhatall[[i]][[1]][[3]][1:360,2]))/mean(yhatall[[i]][[1]][[3]][1:360,2]))*100 
}

# MIROC RCP8.5 --> 2037-2066 ([793:1152,])
pcW85MIROC30 <- c()
pcE85MIROC30 <- c()
for (i in 1:6) {
  pcW85MIROC30[i] <- ((mean(yhatall[[i]][[1]][[4]][793:1152,1])-mean(yhatall[[i]][[1]][[4]][1:360,1]))/mean(yhatall[[i]][[1]][[4]][1:360,1]))*100 
  pcE85MIROC30[i] <- ((mean(yhatall[[i]][[1]][[4]][793:1152,2])-mean(yhatall[[i]][[1]][[4]][1:360,2]))/mean(yhatall[[i]][[1]][[4]][1:360,2]))*100 
}

# NorESM RCP8.5 --> 2057-2086 ([1033:1392,])
pcW85NorESM30 <- c()
pcE85NorESM30 <- c()
for (i in 1:6) {
  pcW85NorESM30[i] <- ((mean(yhatall[[i]][[1]][[5]][1033:1392,1])-mean(yhatall[[i]][[1]][[5]][1:360,1]))/mean(yhatall[[i]][[1]][[5]][1:360,1]))*100 
  pcE85NorESM30[i] <- ((mean(yhatall[[i]][[1]][[5]][1033:1392,2])-mean(yhatall[[i]][[1]][[5]][1:360,2]))/mean(yhatall[[i]][[1]][[5]][1:360,2]))*100 
}


# percent change between 1971-2000 and last 30 years when 1.5 deg threshold was reached (pos == % increase) [new-old/old]

# HadGEM RCP2.6 --> 2007-2036 ([433:792,])
pcW26HadGEM15all <- list()
pcE26HadGEM15all <- list()
for (i in 1:6) {
  pcW26HadGEM15all[[i]] <- ((yhatall[[i]][[4]][[2]][433:792,1]-yhatall[[i]][[4]][[2]][1:360,1])/yhatall[[i]][[4]][[2]][1:360,1])*100 
  pcE26HadGEM15all[[i]] <- ((yhatall[[i]][[4]][[2]][433:792,2]-yhatall[[i]][[4]][[2]][1:360,2])/yhatall[[i]][[4]][[2]][1:360,2])*100 
}

# IPSL RCP2.6 --> 2008-2037 ([445:804,])
pcW26IPSL15all <- list()
pcE26IPSL15all <- list()
for (i in 1:6) {
  pcW26IPSL15all[[i]] <- ((yhatall[[i]][[4]][[3]][445:804,1]-yhatall[[i]][[4]][[3]][1:360,1])/yhatall[[i]][[4]][[3]][1:360,1])*100 
  pcE26IPSL15all[[i]] <- ((yhatall[[i]][[4]][[3]][445:804,2]-yhatall[[i]][[4]][[3]][1:360,2])/yhatall[[i]][[4]][[3]][1:360,2])*100 
}

# MIROC RCP2.6 --> 2006-2035 ([421:780,])
pcW26MIROC15all <- list()
pcE26MIROC15all <- list()
for (i in 1:6) {
  pcW26MIROC15all[[i]] <- ((yhatall[[i]][[4]][[4]][421:780,1]-yhatall[[i]][[4]][[4]][1:360,1])/yhatall[[i]][[4]][[4]][1:360,1])*100 
  pcE26MIROC15all[[i]] <- ((yhatall[[i]][[4]][[4]][421:780,2]-yhatall[[i]][[4]][[4]][1:360,2])/yhatall[[i]][[4]][[4]][1:360,2])*100 
}

# NorESM RCP2.6 --> 2047-2076  ([913:1272,])
pcW26NorESM15all <- list()
pcE26NorESM15all <- list()
for (i in 1:6) {
  pcW26NorESM15all[[i]] <- ((yhatall[[i]][[4]][[5]][913:1272,1]-yhatall[[i]][[4]][[5]][1:360,1])/yhatall[[i]][[4]][[5]][1:360,1])*100 
  pcE26NorESM15all[[i]] <- ((yhatall[[i]][[4]][[5]][913:1272,2]-yhatall[[i]][[4]][[5]][1:360,2])/yhatall[[i]][[4]][[5]][1:360,2])*100 
}

# GFDL RCP8.5 --> 2021-2050 ([601:960,])
pcW85GFDL15all <- list()
pcE85GFDL15all <- list()
for (i in 1:6) {
  pcW85GFDL15all[[i]] <- ((yhatall[[i]][[1]][[1]][601:960,1]-yhatall[[i]][[4]][[2]][1:360,1])/yhatall[[i]][[1]][[1]][1:360,1])*100 
  pcE85GFDL15all[[i]] <- ((yhatall[[i]][[1]][[1]][601:960,2]-yhatall[[i]][[4]][[2]][1:360,2])/yhatall[[i]][[1]][[1]][1:360,2])*100 
}

# HadGEM RCP8.5 --> 2004-2033 ([397:756,])
pcW85HadGEM15all <- list()
pcE85HadGEM15all <- list()
for (i in 1:6) {
  pcW85HadGEM15all[[i]] <- ((yhatall[[i]][[1]][[2]][397:756,1]-yhatall[[i]][[1]][[2]][1:360,1])/yhatall[[i]][[1]][[2]][1:360,1])*100 
  pcE85HadGEM15all[[i]] <- ((yhatall[[i]][[1]][[2]][397:756,2]-yhatall[[i]][[1]][[2]][1:360,2])/yhatall[[i]][[1]][[2]][1:360,2])*100 
}

# IPSL RCP8.5 --> 2006-2035 ([421:780,])
pcW85IPSL15all <- list()
pcE85IPSL15all <- list()
for (i in 1:6) {
  pcW85IPSL15all[[i]] <- ((yhatall[[i]][[1]][[3]][421:780,1]-yhatall[[i]][[1]][[3]][1:360,1])/yhatall[[i]][[1]][[3]][1:360,1])*100 
  pcE85IPSL15all[[i]] <- ((yhatall[[i]][[1]][[3]][421:780,2]-yhatall[[i]][[1]][[3]][1:360,2])/yhatall[[i]][[1]][[3]][1:360,2])*100 
}

# MIROC RCP8.5 --> 2006-2035 ([421:780,])
pcW85MIROC15all <- list()
pcE85MIROC15all <- list()
for (i in 1:6) {
  pcW85MIROC15all[[i]] <- ((yhatall[[i]][[1]][[4]][421:780,1]-yhatall[[i]][[1]][[4]][1:360,1])/yhatall[[i]][[1]][[4]][1:360,1])*100 
  pcE85MIROC15all[[i]] <- ((yhatall[[i]][[1]][[4]][421:780,2]-yhatall[[i]][[1]][[4]][1:360,2])/yhatall[[i]][[1]][[4]][1:360,2])*100 
}

# NorESM RCP8.5 --> 2016-2045 ([541:900,])
pcW85NorESM15all <- list()
pcE85NorESM15all <- list()
for (i in 1:6) {
  pcW85NorESM15all[[i]] <- ((yhatall[[i]][[1]][[5]][541:900,1]-yhatall[[i]][[1]][[5]][1:360,1])/yhatall[[i]][[1]][[5]][1:360,1])*100 
  pcE85NorESM15all[[i]] <- ((yhatall[[i]][[1]][[5]][541:900,2]-yhatall[[i]][[1]][[5]][1:360,2])/yhatall[[i]][[1]][[5]][1:360,2])*100 
}

# percent change between 1971-2000 and last 30 years when 2.0 deg threshold was reached (pos == % increase) [new-old/old]

# HadGEM RCP2.6 --> 2029-2058 ([697:1056,])
pcW26HadGEM20all <- list()
pcE26HadGEM20all <- list()
for (i in 1:6) {
  pcW26HadGEM20all[[i]] <- ((yhatall[[i]][[4]][[2]][697:1056,1]-yhatall[[i]][[4]][[2]][1:360,1])/yhatall[[i]][[4]][[2]][1:360,1])*100 
  pcE26HadGEM20all[[i]] <- ((yhatall[[i]][[4]][[2]][697:1056,2]-yhatall[[i]][[4]][[2]][1:360,2])/yhatall[[i]][[4]][[2]][1:360,2])*100 
}

# IPSL RCP2.6 --> 2060-2089 ([1069:1428,])
pcW26IPSL20all <- list()
pcE26IPSL20all <- list()
for (i in 1:6) {
  pcW26IPSL20all[[i]] <- ((yhatall[[i]][[4]][[3]][1069:1428,1]-yhatall[[i]][[4]][[3]][1:360,1])/yhatall[[i]][[4]][[3]][1:360,1])*100 
  pcE26IPSL20all[[i]] <- ((yhatall[[i]][[4]][[3]][1069:1428,2]-yhatall[[i]][[4]][[3]][1:360,2])/yhatall[[i]][[4]][[3]][1:360,2])*100 
}

# MIROC RCP2.6 --> 2023-2052 ([625:984,])
pcW26MIROC20all <- list()
pcE26MIROC20all <- list()
for (i in 1:6) {
  pcW26MIROC20all[[i]] <- ((yhatall[[i]][[4]][[4]][625:984,1]-yhatall[[i]][[4]][[4]][1:360,1])/yhatall[[i]][[4]][[4]][1:360,1])*100 
  pcE26MIROC20all[[i]] <- ((yhatall[[i]][[4]][[4]][625:984,2]-yhatall[[i]][[4]][[4]][1:360,2])/yhatall[[i]][[4]][[4]][1:360,2])*100 
}

# GFDL RCP8.5 --> 2038-2067 ([805:1164,])
pcW85GFDL20all <- list()
pcE85GFDL20all <- list()
for (i in 1:6) {
  pcW85GFDL20all[[i]] <- ((yhatall[[i]][[1]][[1]][805:1164,1]-yhatall[[i]][[4]][[2]][1:360,1])/yhatall[[i]][[1]][[1]][1:360,1])*100 
  pcE85GFDL20all[[i]] <- ((yhatall[[i]][[1]][[1]][805:1164,2]-yhatall[[i]][[4]][[2]][1:360,2])/yhatall[[i]][[1]][[1]][1:360,2])*100 
}

# HadGEM RCP8.5 --> 2016-2045 ([541:900,])
pcW85HadGEM20all <- list()
pcE85HadGEM20all <- list()
for (i in 1:6) {
  pcW85HadGEM20all[[i]] <- ((yhatall[[i]][[1]][[2]][541:900,1]-yhatall[[i]][[1]][[2]][1:360,1])/yhatall[[i]][[1]][[2]][1:360,1])*100 
  pcE85HadGEM20all[[i]] <- ((yhatall[[i]][[1]][[2]][541:900,2]-yhatall[[i]][[1]][[2]][1:360,2])/yhatall[[i]][[1]][[2]][1:360,2])*100 
}

# IPSL RCP8.5 --> 2018-2047 ([565:924,])
pcW85IPSL20all <- list()
pcE85IPSL20all <- list()
for (i in 1:6) {
  pcW85IPSL20all[[i]] <- ((yhatall[[i]][[1]][[3]][565:924,1]-yhatall[[i]][[1]][[3]][1:360,1])/yhatall[[i]][[1]][[3]][1:360,1])*100 
  pcE85IPSL20all[[i]] <- ((yhatall[[i]][[1]][[3]][565:924,2]-yhatall[[i]][[1]][[3]][1:360,2])/yhatall[[i]][[1]][[3]][1:360,2])*100 
}

# MIROC RCP8.5 --> 2017-2046 ([553:912,])
pcW85MIROC20all <- list()
pcE85MIROC20all <- list()
for (i in 1:6) {
  pcW85MIROC20all[[i]] <- ((yhatall[[i]][[1]][[4]][553:912,1]-yhatall[[i]][[1]][[4]][1:360,1])/yhatall[[i]][[1]][[4]][1:360,1])*100 
  pcE85MIROC20all[[i]] <- ((yhatall[[i]][[1]][[4]][553:912,2]-yhatall[[i]][[1]][[4]][1:360,2])/yhatall[[i]][[1]][[4]][1:360,2])*100 
}

# NorESM RCP8.5 --> 2031-2060 ([721:1080,])
pcW85NorESM20all <- list()
pcE85NorESM20all <- list()
for (i in 1:6) {
  pcW85NorESM20all[[i]] <- ((yhatall[[i]][[1]][[5]][721:1080,1]-yhatall[[i]][[1]][[5]][1:360,1])/yhatall[[i]][[1]][[5]][1:360,1])*100 
  pcE85NorESM20all[[i]] <- ((yhatall[[i]][[1]][[5]][721:1080,2]-yhatall[[i]][[1]][[5]][1:360,2])/yhatall[[i]][[1]][[5]][1:360,2])*100 
}

# percent change between 1971-2000 and last 30 years when 3.0 deg threshold was reached (pos == % increase) [new-old/old]

# GFDLRCP8.5 --> 2067-2096 ([1153:1512,])
pcW85GFDL30all <- list()
pcE85GFDL30all <- list()
for (i in 1:6) {
  pcW85GFDL30all[[i]] <- ((yhatall[[i]][[1]][[1]][1153:1512,1]-yhatall[[i]][[4]][[2]][1:360,1])/yhatall[[i]][[1]][[1]][1:360,1])*100 
  pcE85GFDL30all[[i]] <- ((yhatall[[i]][[1]][[1]][1153:1512,2]-yhatall[[i]][[4]][[2]][1:360,2])/yhatall[[i]][[1]][[1]][1:360,2])*100 
}

# HadGEM RCP8.5 --> 2035-2064 ([769:1128,])
pcW85HadGEM30all <- list()
pcE85HadGEM30all <- list()
for (i in 1:6) {
  pcW85HadGEM30all[[i]] <- ((yhatall[[i]][[1]][[2]][769:1128,1]-yhatall[[i]][[1]][[2]][1:360,1])/yhatall[[i]][[1]][[2]][1:360,1])*100 
  pcE85HadGEM30all[[i]] <- ((yhatall[[i]][[1]][[2]][769:1128,2]-yhatall[[i]][[1]][[2]][1:360,2])/yhatall[[i]][[1]][[2]][1:360,2])*100 
}

# IPSL RCP8.5 --> 2038-2067 ([805:1164,])
pcW85IPSL30all <- list()
pcE85IPSL30all <- list()
for (i in 1:6) {
  pcW85IPSL30all[[i]] <- ((yhatall[[i]][[1]][[3]][805:1164,1]-yhatall[[i]][[1]][[3]][1:360,1])/yhatall[[i]][[1]][[3]][1:360,1])*100 
  pcE85IPSL30all[[i]] <- ((yhatall[[i]][[1]][[3]][805:1164,2]-yhatall[[i]][[1]][[3]][1:360,2])/yhatall[[i]][[1]][[3]][1:360,2])*100 
}

# MIROC RCP8.5 --> 2037-2066 ([793:1152,])
pcW85MIROC30all <- list()
pcE85MIROC30all <- list()
for (i in 1:6) {
  pcW85MIROC30all[[i]] <- ((yhatall[[i]][[1]][[4]][793:1152,1]-yhatall[[i]][[1]][[4]][1:360,1])/yhatall[[i]][[1]][[4]][1:360,1])*100 
  pcE85MIROC30all[[i]] <- ((yhatall[[i]][[1]][[4]][793:1152,2]-yhatall[[i]][[1]][[4]][1:360,2])/yhatall[[i]][[1]][[4]][1:360,2])*100 
}

# NorESM RCP8.5 --> 2057-2086 ([1033:1392,])
pcW85NorESM30all <- list()
pcE85NorESM30all <- list()
for (i in 1:6) {
  pcW85NorESM30all[[i]] <- ((yhatall[[i]][[1]][[5]][1033:1392,1]-yhatall[[i]][[1]][[5]][1:360,1])/yhatall[[i]][[1]][[5]][1:360,1])*100 
  pcE85NorESM30all[[i]] <- ((yhatall[[i]][[1]][[5]][1033:1392,2]-yhatall[[i]][[1]][[5]][1:360,2])/yhatall[[i]][[1]][[5]][1:360,2])*100 
}

# moving average

maperiod <- 3*12 # 3 year moving average

mawater <- list()
maelec <- list()

for (c in 1:6) {
  mascenw <- list()
  mascene <- list()
  for (s in 1:4) {
    mamodw <- list()
    mamode <- list()
    for (m in 1:5) {
      w <- rollapply(yhatall[[c]][[s]][[m]][,1], maperiod, mean, align = 'center',partial=T)
      e <- rollapply(yhatall[[c]][[s]][[m]][,2], maperiod, mean, align = 'center',partial=T)
      
      # store values
      mamodw[[m]] <- w
      mamode[[m]] <- e
    }
    mascenw[[s]] <- (mamodw[[1]]+mamodw[[2]]+mamodw[[3]]+mamodw[[4]]+mamodw[[5]])/5
    mascene[[s]] <- (mamode[[1]]+mamode[[2]]+mamode[[3]]+mamode[[4]]+mamode[[5]])/5
  }
  mawater[[c]] <- mascenw
  maelec[[c]] <- mascene
}

# moving average - actual data
mawaterACT <- list()
maelecACT <- list()

for (c in 1:6) {
  
  
  w <- rollapply(cities[[c]][,3], maperiod, mean, align = 'center',partial=T)
  e <- rollapply(cities[[c]][,4], maperiod, mean, align = 'center',partial=T)
  
  # store values
  mawaterACT[[c]] <- w
  maelecACT[[c]] <- e
}


#################################   FIGURES   ############################################

# means + min/max
# percent change per city (all scenarios/models)
for (i in 1:6) {
  
  avg15 <- c(mean(pcW26HadGEM15all[[i]]),mean(pcW26IPSL15all[[i]]),mean(pcW26MIROC15all[[i]]),mean(pcW26NorESM15all[[i]]),
             mean(pcW85GFDL15all[[i]]),mean(pcW85HadGEM15all[[i]]),mean(pcW85IPSL15all[[i]]),mean(pcW85MIROC15all[[i]]),mean(pcW85NorESM15all[[i]]))
  avg20 <- c(mean(pcW26HadGEM20all[[i]]),mean(pcW26IPSL20all[[i]]),mean(pcW26MIROC20all[[i]]),
             mean(pcW85GFDL20all[[i]]),mean(pcW85HadGEM20all[[i]]),mean(pcW85IPSL20all[[i]]),mean(pcW85MIROC20all[[i]]),mean(pcW85NorESM20all[[i]]))
  avg30 <- c(mean(pcW85GFDL30all[[i]]),mean(pcW85HadGEM30all[[i]]),mean(pcW85IPSL30all[[i]]),mean(pcW85MIROC30all[[i]]),mean(pcW85NorESM30all[[i]]))
  
  max15 <- c(max(pcW26HadGEM15all[[i]]),max(pcW26IPSL15all[[i]]),max(pcW26MIROC15all[[i]]),max(pcW26NorESM15all[[i]]),
             max(pcW85GFDL15all[[i]]),max(pcW85HadGEM15all[[i]]),max(pcW85IPSL15all[[i]]),max(pcW85MIROC15all[[i]]),max(pcW85NorESM15all[[i]]))
  max20 <- c(max(pcW26HadGEM20all[[i]]),max(pcW26IPSL20all[[i]]),max(pcW26MIROC20all[[i]]),
             max(pcW85GFDL20all[[i]]),max(pcW85HadGEM20all[[i]]),max(pcW85IPSL20all[[i]]),max(pcW85MIROC20all[[i]]),max(pcW85NorESM20all[[i]]))
  max30 <- c(max(pcW85GFDL30all[[i]]),max(pcW85HadGEM30all[[i]]),max(pcW85IPSL30all[[i]]),max(pcW85MIROC30all[[i]]),max(pcW85NorESM30all[[i]]))
  
  min15 <- c(min(pcW26HadGEM15all[[i]]),min(pcW26IPSL15all[[i]]),min(pcW26MIROC15all[[i]]),min(pcW26NorESM15all[[i]]),
             min(pcW85GFDL15all[[i]]),min(pcW85HadGEM15all[[i]]),min(pcW85IPSL15all[[i]]),min(pcW85MIROC15all[[i]]),min(pcW85NorESM15all[[i]]))
  min20 <- c(min(pcW26HadGEM20all[[i]]),min(pcW26IPSL20all[[i]]),min(pcW26MIROC20all[[i]]),
             min(pcW85GFDL20all[[i]]),min(pcW85HadGEM20all[[i]]),min(pcW85IPSL20all[[i]]),min(pcW85MIROC20all[[i]]),min(pcW85NorESM20all[[i]]))
  min30 <- c(min(pcW85GFDL30all[[i]]),min(pcW85HadGEM30all[[i]]),min(pcW85IPSL30all[[i]]),min(pcW85MIROC30all[[i]]),min(pcW85NorESM30all[[i]]))

  pchange <- c(mean(avg15),mean(avg20),mean(avg30))
  maxs <- c(max(max15),max(max20),max(max30)) 
  mins <- c(min(min15),min(min20),min(min30)) 
  
  Threshold <- c("1.5","2.0","3.0")
  
  pcdata <- data.frame(pchange,Threshold, mins,maxs)
  
  waterp <- ggplot(pcdata, aes(x=Threshold, y=pchange)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=mins, ymax=maxs), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Water Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) #+
    #scale_fill_manual(values=c('#fb9a99','#a6cee3','#1f78b4','#b2df8a','#33a02c'))+
    #facet_grid(Scenario ~ .) 
  
  
  avg15 <- c(mean(pcE26HadGEM15all[[i]]),mean(pcE26IPSL15all[[i]]),mean(pcE26MIROC15all[[i]]),mean(pcE26NorESM15all[[i]]),
             mean(pcE85GFDL15all[[i]]),mean(pcE85HadGEM15all[[i]]),mean(pcE85IPSL15all[[i]]),mean(pcE85MIROC15all[[i]]),mean(pcE85NorESM15all[[i]]))
  avg20 <- c(mean(pcE26HadGEM20all[[i]]),mean(pcE26IPSL20all[[i]]),mean(pcE26MIROC20all[[i]]),
             mean(pcE85GFDL20all[[i]]),mean(pcE85HadGEM20all[[i]]),mean(pcE85IPSL20all[[i]]),mean(pcE85MIROC20all[[i]]),mean(pcE85NorESM20all[[i]]))
  avg30 <- c(mean(pcE85GFDL30all[[i]]),mean(pcE85HadGEM30all[[i]]),mean(pcE85IPSL30all[[i]]),mean(pcE85MIROC30all[[i]]),mean(pcE85NorESM30all[[i]]))
  
  max15 <- c(max(pcE26HadGEM15all[[i]]),max(pcE26IPSL15all[[i]]),max(pcE26MIROC15all[[i]]),max(pcE26NorESM15all[[i]]),
             max(pcE85GFDL15all[[i]]),max(pcE85HadGEM15all[[i]]),max(pcE85IPSL15all[[i]]),max(pcE85MIROC15all[[i]]),max(pcE85NorESM15all[[i]]))
  max20 <- c(max(pcE26HadGEM20all[[i]]),max(pcE26IPSL20all[[i]]),max(pcE26MIROC20all[[i]]),
             max(pcE85GFDL20all[[i]]),max(pcE85HadGEM20all[[i]]),max(pcE85IPSL20all[[i]]),max(pcE85MIROC20all[[i]]),max(pcE85NorESM20all[[i]]))
  max30 <- c(max(pcE85GFDL30all[[i]]),max(pcE85HadGEM30all[[i]]),max(pcE85IPSL30all[[i]]),max(pcE85MIROC30all[[i]]),max(pcE85NorESM30all[[i]]))
  
  min15 <- c(min(pcE26HadGEM15all[[i]]),min(pcE26IPSL15all[[i]]),min(pcE26MIROC15all[[i]]),min(pcE26NorESM15all[[i]]),
             min(pcE85GFDL15all[[i]]),min(pcE85HadGEM15all[[i]]),min(pcE85IPSL15all[[i]]),min(pcE85MIROC15all[[i]]),min(pcE85NorESM15all[[i]]))
  min20 <- c(min(pcE26HadGEM20all[[i]]),min(pcE26IPSL20all[[i]]),min(pcE26MIROC20all[[i]]),
             min(pcE85GFDL20all[[i]]),min(pcE85HadGEM20all[[i]]),min(pcE85IPSL20all[[i]]),min(pcE85MIROC20all[[i]]),min(pcE85NorESM20all[[i]]))
  min30 <- c(min(pcE85GFDL30all[[i]]),min(pcE85HadGEM30all[[i]]),min(pcE85IPSL30all[[i]]),min(pcE85MIROC30all[[i]]),min(pcE85NorESM30all[[i]]))
  
  pchange <- c(mean(avg15),mean(avg20),mean(avg30))
  maxs <- c(max(max15),max(max20),max(max30)) 
  mins <- c(min(min15),min(min20),min(min30)) 
  
  pcdata <- data.frame(pchange,Threshold,mins,maxs)
  
  elecp <-ggplot(pcdata, aes(x=Threshold, y=pchange)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=mins, ymax=maxs), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Electricity Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) #+
    #scale_fill_manual(values=c('#fb9a99','#a6cee3','#1f78b4','#b2df8a','#33a02c')) +
    #facet_grid(Scenario ~ .)
  
  grid.arrange(waterp,elecp,ncol=2)
  
}
# percent change per city (all models)
for (i in 1:6) {
  
  avg15 <- c(mean(pcW26HadGEM15all[[i]]),mean(pcW26IPSL15all[[i]]),mean(pcW26MIROC15all[[i]]),mean(pcW26NorESM15all[[i]]),
             mean(pcW85GFDL15all[[i]]),mean(pcW85HadGEM15all[[i]]),mean(pcW85IPSL15all[[i]]),mean(pcW85MIROC15all[[i]]),mean(pcW85NorESM15all[[i]]))
  avg20 <- c(mean(pcW26HadGEM20all[[i]]),mean(pcW26IPSL20all[[i]]),mean(pcW26MIROC20all[[i]]),
             mean(pcW85GFDL20all[[i]]),mean(pcW85HadGEM20all[[i]]),mean(pcW85IPSL20all[[i]]),mean(pcW85MIROC20all[[i]]),mean(pcW85NorESM20all[[i]]))
  avg30 <- c(mean(pcW85GFDL30all[[i]]),mean(pcW85HadGEM30all[[i]]),mean(pcW85IPSL30all[[i]]),mean(pcW85MIROC30all[[i]]),mean(pcW85NorESM30all[[i]]))
  
  max15 <- c(max(pcW26HadGEM15all[[i]]),max(pcW26IPSL15all[[i]]),max(pcW26MIROC15all[[i]]),max(pcW26NorESM15all[[i]]),
             max(pcW85GFDL15all[[i]]),max(pcW85HadGEM15all[[i]]),max(pcW85IPSL15all[[i]]),max(pcW85MIROC15all[[i]]),max(pcW85NorESM15all[[i]]))
  max20 <- c(max(pcW26HadGEM20all[[i]]),max(pcW26IPSL20all[[i]]),max(pcW26MIROC20all[[i]]),
             max(pcW85GFDL20all[[i]]),max(pcW85HadGEM20all[[i]]),max(pcW85IPSL20all[[i]]),max(pcW85MIROC20all[[i]]),max(pcW85NorESM20all[[i]]))
  max30 <- c(max(pcW85GFDL30all[[i]]),max(pcW85HadGEM30all[[i]]),max(pcW85IPSL30all[[i]]),max(pcW85MIROC30all[[i]]),max(pcW85NorESM30all[[i]]))
  
  min15 <- c(min(pcW26HadGEM15all[[i]]),min(pcW26IPSL15all[[i]]),min(pcW26MIROC15all[[i]]),min(pcW26NorESM15all[[i]]),
             min(pcW85GFDL15all[[i]]),min(pcW85HadGEM15all[[i]]),min(pcW85IPSL15all[[i]]),min(pcW85MIROC15all[[i]]),min(pcW85NorESM15all[[i]]))
  min20 <- c(min(pcW26HadGEM20all[[i]]),min(pcW26IPSL20all[[i]]),min(pcW26MIROC20all[[i]]),
             min(pcW85GFDL20all[[i]]),min(pcW85HadGEM20all[[i]]),min(pcW85IPSL20all[[i]]),min(pcW85MIROC20all[[i]]),min(pcW85NorESM20all[[i]]))
  min30 <- c(min(pcW85GFDL30all[[i]]),min(pcW85HadGEM30all[[i]]),min(pcW85IPSL30all[[i]]),min(pcW85MIROC30all[[i]]),min(pcW85NorESM30all[[i]]))
  
  pchange <- c(mean(avg15[1:4]),mean(avg20[1:3]),0,mean(avg15[5:9]),mean(avg20[4:8]),mean(avg30))
  maxs <- c(max(max15[1:4]),max(max20[1:3]),NA,max(max15[5:9]),max(max20[4:8]),max(max30)) 
  mins <- c(min(min15[1:4]),min(min20[1:3]),NA,min(min15[5:9]),min(min20[4:8]),min(min30)) 
  
  Threshold <- c("1.5","2.0","3.0","1.5","2.0","3.0")
  
  Scenario <- c("RCP2.6","RCP2.6","RCP2.6","RCP8.5","RCP8.5","RCP8.5")
  
  pcdata <- data.frame(pchange,Threshold, mins,maxs, Scenario)
  
  waterp <- ggplot(pcdata, aes(x=Threshold, y=pchange,fill=Scenario)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=mins, ymax=maxs), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Water Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
    scale_fill_manual(values=c('#66c2a5','#fc8d62'))

  avg15 <- c(mean(pcE26HadGEM15all[[i]]),mean(pcE26IPSL15all[[i]]),mean(pcE26MIROC15all[[i]]),mean(pcE26NorESM15all[[i]]),
             mean(pcE85GFDL15all[[i]]),mean(pcE85HadGEM15all[[i]]),mean(pcE85IPSL15all[[i]]),mean(pcE85MIROC15all[[i]]),mean(pcE85NorESM15all[[i]]))
  avg20 <- c(mean(pcE26HadGEM20all[[i]]),mean(pcE26IPSL20all[[i]]),mean(pcE26MIROC20all[[i]]),
             mean(pcE85GFDL20all[[i]]),mean(pcE85HadGEM20all[[i]]),mean(pcE85IPSL20all[[i]]),mean(pcE85MIROC20all[[i]]),mean(pcE85NorESM20all[[i]]))
  avg30 <- c(mean(pcE85GFDL30all[[i]]),mean(pcE85HadGEM30all[[i]]),mean(pcE85IPSL30all[[i]]),mean(pcE85MIROC30all[[i]]),mean(pcE85NorESM30all[[i]]))
  
  max15 <- c(max(pcE26HadGEM15all[[i]]),max(pcE26IPSL15all[[i]]),max(pcE26MIROC15all[[i]]),max(pcE26NorESM15all[[i]]),
             max(pcE85GFDL15all[[i]]),max(pcE85HadGEM15all[[i]]),max(pcE85IPSL15all[[i]]),max(pcE85MIROC15all[[i]]),max(pcE85NorESM15all[[i]]))
  max20 <- c(max(pcE26HadGEM20all[[i]]),max(pcE26IPSL20all[[i]]),max(pcE26MIROC20all[[i]]),
             max(pcE85GFDL20all[[i]]),max(pcE85HadGEM20all[[i]]),max(pcE85IPSL20all[[i]]),max(pcE85MIROC20all[[i]]),max(pcE85NorESM20all[[i]]))
  max30 <- c(max(pcE85GFDL30all[[i]]),max(pcE85HadGEM30all[[i]]),max(pcE85IPSL30all[[i]]),max(pcE85MIROC30all[[i]]),max(pcE85NorESM30all[[i]]))
  
  min15 <- c(min(pcE26HadGEM15all[[i]]),min(pcE26IPSL15all[[i]]),min(pcE26MIROC15all[[i]]),min(pcE26NorESM15all[[i]]),
             min(pcE85GFDL15all[[i]]),min(pcE85HadGEM15all[[i]]),min(pcE85IPSL15all[[i]]),min(pcE85MIROC15all[[i]]),min(pcE85NorESM15all[[i]]))
  min20 <- c(min(pcE26HadGEM20all[[i]]),min(pcE26IPSL20all[[i]]),min(pcE26MIROC20all[[i]]),
             min(pcE85GFDL20all[[i]]),min(pcE85HadGEM20all[[i]]),min(pcE85IPSL20all[[i]]),min(pcE85MIROC20all[[i]]),min(pcE85NorESM20all[[i]]))
  min30 <- c(min(pcE85GFDL30all[[i]]),min(pcE85HadGEM30all[[i]]),min(pcE85IPSL30all[[i]]),min(pcE85MIROC30all[[i]]),min(pcE85NorESM30all[[i]]))
  
  pchange <- c(mean(avg15[1:4]),mean(avg20[1:3]),0,mean(avg15[5:9]),mean(avg20[4:8]),mean(avg30))
  maxs <- c(max(max15[1:4]),max(max20[1:3]),NA,max(max15[5:9]),max(max20[4:8]),max(max30)) 
  mins <- c(min(min15[1:4]),min(min20[1:3]),NA,min(min15[5:9]),min(min20[4:8]),min(min30)) 
  
  pcdata <- data.frame(pchange,Threshold,mins,maxs,Scenario)
  
  elecp <-ggplot(pcdata, aes(x=Threshold, y=pchange, fill=Scenario)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=mins, ymax=maxs), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Electricity Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
    scale_fill_manual(values=c('#66c2a5','#fc8d62'))

  grid.arrange(waterp,elecp,ncol=2)
  
}
# percent change per city
for (i in 1:6) {
  
  avg15 <- c(mean(pcW26HadGEM15all[[i]]),mean(pcW26IPSL15all[[i]]),mean(pcW26MIROC15all[[i]]),mean(pcW26NorESM15all[[i]]),
             mean(pcW85GFDL15all[[i]]),mean(pcW85HadGEM15all[[i]]),mean(pcW85IPSL15all[[i]]),mean(pcW85MIROC15all[[i]]),mean(pcW85NorESM15all[[i]]))
  avg20 <- c(mean(pcW26HadGEM20all[[i]]),mean(pcW26IPSL20all[[i]]),mean(pcW26MIROC20all[[i]]),
             mean(pcW85GFDL20all[[i]]),mean(pcW85HadGEM20all[[i]]),mean(pcW85IPSL20all[[i]]),mean(pcW85MIROC20all[[i]]),mean(pcW85NorESM20all[[i]]))
  avg30 <- c(mean(pcW85GFDL30all[[i]]),mean(pcW85HadGEM30all[[i]]),mean(pcW85IPSL30all[[i]]),mean(pcW85MIROC30all[[i]]),mean(pcW85NorESM30all[[i]]))
  
  max15 <- c(max(pcW26HadGEM15all[[i]]),max(pcW26IPSL15all[[i]]),max(pcW26MIROC15all[[i]]),max(pcW26NorESM15all[[i]]),
             max(pcW85GFDL15all[[i]]),max(pcW85HadGEM15all[[i]]),max(pcW85IPSL15all[[i]]),max(pcW85MIROC15all[[i]]),max(pcW85NorESM15all[[i]]))
  max20 <- c(max(pcW26HadGEM20all[[i]]),max(pcW26IPSL20all[[i]]),max(pcW26MIROC20all[[i]]),
             max(pcW85GFDL20all[[i]]),max(pcW85HadGEM20all[[i]]),max(pcW85IPSL20all[[i]]),max(pcW85MIROC20all[[i]]),max(pcW85NorESM20all[[i]]))
  max30 <- c(max(pcW85GFDL30all[[i]]),max(pcW85HadGEM30all[[i]]),max(pcW85IPSL30all[[i]]),max(pcW85MIROC30all[[i]]),max(pcW85NorESM30all[[i]]))
  
  min15 <- c(min(pcW26HadGEM15all[[i]]),min(pcW26IPSL15all[[i]]),min(pcW26MIROC15all[[i]]),min(pcW26NorESM15all[[i]]),
             min(pcW85GFDL15all[[i]]),min(pcW85HadGEM15all[[i]]),min(pcW85IPSL15all[[i]]),min(pcW85MIROC15all[[i]]),min(pcW85NorESM15all[[i]]))
  min20 <- c(min(pcW26HadGEM20all[[i]]),min(pcW26IPSL20all[[i]]),min(pcW26MIROC20all[[i]]),
             min(pcW85GFDL20all[[i]]),min(pcW85HadGEM20all[[i]]),min(pcW85IPSL20all[[i]]),min(pcW85MIROC20all[[i]]),min(pcW85NorESM20all[[i]]))
  min30 <- c(min(pcW85GFDL30all[[i]]),min(pcW85HadGEM30all[[i]]),min(pcW85IPSL30all[[i]]),min(pcW85MIROC30all[[i]]),min(pcW85NorESM30all[[i]]))
  
  pchange <- c(0,avg15[1:4],0,avg20[1:3],0,avg15[5:9],avg20[4:8],avg30)
  maxs <- c(NA,max15[1:4],NA,max20[1:3],NA,max15[5:9],max20[4:8],max30) 
  mins <- c(NA,min15[1:4],NA,min20[1:3],NA,min15[5:9],min20[4:8],min30) 
  
  Scenario <- c("RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP2.6",
                "RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP2.6",
                "RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5",
                "RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5",
                "RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5")
  
  Threshold <- c("1.5","1.5","1.5","1.5","1.5",
                 "2.0","2.0","2.0","2.0","2.0",
                 "1.5","1.5","1.5","1.5","1.5",
                 "2.0","2.0","2.0","2.0","2.0",
                 "3.0","3.0","3.0","3.0","3.0")
  
  Model <- c("GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M")
  
  pcdata <- data.frame(pchange,Threshold, mins,maxs, Scenario,Model)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange,fill=Model)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=mins, ymax=maxs), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Water Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
    scale_fill_manual(values=c('#fb9a99','#a6cee3','#1f78b4','#b2df8a','#33a02c'))+
    facet_grid(Scenario ~ .)) 
  
  
  avg15 <- c(mean(pcE26HadGEM15all[[i]]),mean(pcE26IPSL15all[[i]]),mean(pcE26MIROC15all[[i]]),mean(pcE26NorESM15all[[i]]),
             mean(pcE85GFDL15all[[i]]),mean(pcE85HadGEM15all[[i]]),mean(pcE85IPSL15all[[i]]),mean(pcE85MIROC15all[[i]]),mean(pcE85NorESM15all[[i]]))
  avg20 <- c(mean(pcE26HadGEM20all[[i]]),mean(pcE26IPSL20all[[i]]),mean(pcE26MIROC20all[[i]]),
             mean(pcE85GFDL20all[[i]]),mean(pcE85HadGEM20all[[i]]),mean(pcE85IPSL20all[[i]]),mean(pcE85MIROC20all[[i]]),mean(pcE85NorESM20all[[i]]))
  avg30 <- c(mean(pcE85GFDL30all[[i]]),mean(pcE85HadGEM30all[[i]]),mean(pcE85IPSL30all[[i]]),mean(pcE85MIROC30all[[i]]),mean(pcE85NorESM30all[[i]]))
  
  max15 <- c(max(pcE26HadGEM15all[[i]]),max(pcE26IPSL15all[[i]]),max(pcE26MIROC15all[[i]]),max(pcE26NorESM15all[[i]]),
             max(pcE85GFDL15all[[i]]),max(pcE85HadGEM15all[[i]]),max(pcE85IPSL15all[[i]]),max(pcE85MIROC15all[[i]]),max(pcE85NorESM15all[[i]]))
  max20 <- c(max(pcE26HadGEM20all[[i]]),max(pcE26IPSL20all[[i]]),max(pcE26MIROC20all[[i]]),
             max(pcE85GFDL20all[[i]]),max(pcE85HadGEM20all[[i]]),max(pcE85IPSL20all[[i]]),max(pcE85MIROC20all[[i]]),max(pcE85NorESM20all[[i]]))
  max30 <- c(max(pcE85GFDL30all[[i]]),max(pcE85HadGEM30all[[i]]),max(pcE85IPSL30all[[i]]),max(pcE85MIROC30all[[i]]),max(pcE85NorESM30all[[i]]))
  
  min15 <- c(min(pcE26HadGEM15all[[i]]),min(pcE26IPSL15all[[i]]),min(pcE26MIROC15all[[i]]),min(pcE26NorESM15all[[i]]),
             min(pcE85GFDL15all[[i]]),min(pcE85HadGEM15all[[i]]),min(pcE85IPSL15all[[i]]),min(pcE85MIROC15all[[i]]),min(pcE85NorESM15all[[i]]))
  min20 <- c(min(pcE26HadGEM20all[[i]]),min(pcE26IPSL20all[[i]]),min(pcE26MIROC20all[[i]]),
             min(pcE85GFDL20all[[i]]),min(pcE85HadGEM20all[[i]]),min(pcE85IPSL20all[[i]]),min(pcE85MIROC20all[[i]]),min(pcE85NorESM20all[[i]]))
  min30 <- c(min(pcE85GFDL30all[[i]]),min(pcE85HadGEM30all[[i]]),min(pcE85IPSL30all[[i]]),min(pcE85MIROC30all[[i]]),min(pcE85NorESM30all[[i]]))
  
  pchange <- c(0,avg15[1:4],0,avg20[1:3],0,avg15[5:9],avg20[4:8],avg30)
  maxs <- c(NA,max15[1:4],NA,max20[1:3],NA,max15[5:9],max20[4:8],max30) 
  mins <- c(NA,min15[1:4],NA,min20[1:3],NA,min15[5:9],min20[4:8],min30) 
  
  pcdata <- data.frame(pchange,Threshold,mins,maxs,Scenario)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange, fill=Model)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=mins, ymax=maxs), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Electricity Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
    scale_fill_manual(values=c('#fb9a99','#a6cee3','#1f78b4','#b2df8a','#33a02c')) +
    facet_grid(Scenario ~ .))
}

# means + sd
# percent change per city (all scenarios/models)
for (i in 1:6) {
  
  avg15 <- c(mean(pcW26HadGEM15all[[i]]),mean(pcW26IPSL15all[[i]]),mean(pcW26MIROC15all[[i]]),mean(pcW26NorESM15all[[i]]),
             mean(pcW85GFDL15all[[i]]),mean(pcW85HadGEM15all[[i]]),mean(pcW85IPSL15all[[i]]),mean(pcW85MIROC15all[[i]]),mean(pcW85NorESM15all[[i]]))
  avg20 <- c(mean(pcW26HadGEM20all[[i]]),mean(pcW26IPSL20all[[i]]),mean(pcW26MIROC20all[[i]]),
             mean(pcW85GFDL20all[[i]]),mean(pcW85HadGEM20all[[i]]),mean(pcW85IPSL20all[[i]]),mean(pcW85MIROC20all[[i]]),mean(pcW85NorESM20all[[i]]))
  avg30 <- c(mean(pcW85GFDL30all[[i]]),mean(pcW85HadGEM30all[[i]]),mean(pcW85IPSL30all[[i]]),mean(pcW85MIROC30all[[i]]),mean(pcW85NorESM30all[[i]]))
  
  sd15 <- c(sd(pcW26HadGEM15all[[i]]),sd(pcW26IPSL15all[[i]]),sd(pcW26MIROC15all[[i]]),sd(pcW26NorESM15all[[i]]),
             sd(pcW85GFDL15all[[i]]),sd(pcW85HadGEM15all[[i]]),sd(pcW85IPSL15all[[i]]),sd(pcW85MIROC15all[[i]]),sd(pcW85NorESM15all[[i]]))
  sd20 <- c(sd(pcW26HadGEM20all[[i]]),sd(pcW26IPSL20all[[i]]),sd(pcW26MIROC20all[[i]]),
             sd(pcW85GFDL20all[[i]]),sd(pcW85HadGEM20all[[i]]),sd(pcW85IPSL20all[[i]]),sd(pcW85MIROC20all[[i]]),sd(pcW85NorESM20all[[i]]))
  sd30 <- c(sd(pcW85GFDL30all[[i]]),sd(pcW85HadGEM30all[[i]]),sd(pcW85IPSL30all[[i]]),sd(pcW85MIROC30all[[i]]),sd(pcW85NorESM30all[[i]]))
  
  pchange <- c(mean(avg15),mean(avg20),mean(avg30))
  sds <- c(mean(sd15),mean(sd20),mean(sd30)) 
  
  Threshold <- c("1.5","2.0","3.0")
  
  pcdata <- data.frame(pchange,Threshold, sds)
  
  waterp <- ggplot(pcdata, aes(x=Threshold, y=pchange)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=pchange-sds, ymax=pchange+sds), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Water Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) #+

  avg15 <- c(mean(pcE26HadGEM15all[[i]]),mean(pcE26IPSL15all[[i]]),mean(pcE26MIROC15all[[i]]),mean(pcE26NorESM15all[[i]]),
             mean(pcE85GFDL15all[[i]]),mean(pcE85HadGEM15all[[i]]),mean(pcE85IPSL15all[[i]]),mean(pcE85MIROC15all[[i]]),mean(pcE85NorESM15all[[i]]))
  avg20 <- c(mean(pcE26HadGEM20all[[i]]),mean(pcE26IPSL20all[[i]]),mean(pcE26MIROC20all[[i]]),
             mean(pcE85GFDL20all[[i]]),mean(pcE85HadGEM20all[[i]]),mean(pcE85IPSL20all[[i]]),mean(pcE85MIROC20all[[i]]),mean(pcE85NorESM20all[[i]]))
  avg30 <- c(mean(pcE85GFDL30all[[i]]),mean(pcE85HadGEM30all[[i]]),mean(pcE85IPSL30all[[i]]),mean(pcE85MIROC30all[[i]]),mean(pcE85NorESM30all[[i]]))
  
  sd15 <- c(sd(pcE26HadGEM15all[[i]]),sd(pcE26IPSL15all[[i]]),sd(pcE26MIROC15all[[i]]),sd(pcE26NorESM15all[[i]]),
             sd(pcE85GFDL15all[[i]]),sd(pcE85HadGEM15all[[i]]),sd(pcE85IPSL15all[[i]]),sd(pcE85MIROC15all[[i]]),sd(pcE85NorESM15all[[i]]))
  sd20 <- c(sd(pcE26HadGEM20all[[i]]),sd(pcE26IPSL20all[[i]]),sd(pcE26MIROC20all[[i]]),
             sd(pcE85GFDL20all[[i]]),sd(pcE85HadGEM20all[[i]]),sd(pcE85IPSL20all[[i]]),sd(pcE85MIROC20all[[i]]),sd(pcE85NorESM20all[[i]]))
  sd30 <- c(sd(pcE85GFDL30all[[i]]),sd(pcE85HadGEM30all[[i]]),sd(pcE85IPSL30all[[i]]),sd(pcE85MIROC30all[[i]]),sd(pcE85NorESM30all[[i]]))
  
  pchange <- c(mean(avg15),mean(avg20),mean(avg30))
  sds <- c(mean(sd15),mean(sd20),mean(sd30))
  
  pcdata <- data.frame(pchange,Threshold,sds)
  
  elecp <-ggplot(pcdata, aes(x=Threshold, y=pchange)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=pchange-sds, ymax=pchange+sds), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Electricity Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) #+

  grid.arrange(waterp,elecp,ncol=2)
  
}
# percent change per city (all models)
for (i in 1:6) {
  
  avg15 <- c(mean(pcW26HadGEM15all[[i]]),mean(pcW26IPSL15all[[i]]),mean(pcW26MIROC15all[[i]]),mean(pcW26NorESM15all[[i]]),
             mean(pcW85GFDL15all[[i]]),mean(pcW85HadGEM15all[[i]]),mean(pcW85IPSL15all[[i]]),mean(pcW85MIROC15all[[i]]),mean(pcW85NorESM15all[[i]]))
  avg20 <- c(mean(pcW26HadGEM20all[[i]]),mean(pcW26IPSL20all[[i]]),mean(pcW26MIROC20all[[i]]),
             mean(pcW85GFDL20all[[i]]),mean(pcW85HadGEM20all[[i]]),mean(pcW85IPSL20all[[i]]),mean(pcW85MIROC20all[[i]]),mean(pcW85NorESM20all[[i]]))
  avg30 <- c(mean(pcW85GFDL30all[[i]]),mean(pcW85HadGEM30all[[i]]),mean(pcW85IPSL30all[[i]]),mean(pcW85MIROC30all[[i]]),mean(pcW85NorESM30all[[i]]))
  
  sd15 <- c(sd(pcW26HadGEM15all[[i]]),sd(pcW26IPSL15all[[i]]),sd(pcW26MIROC15all[[i]]),sd(pcW26NorESM15all[[i]]),
            sd(pcW85GFDL15all[[i]]),sd(pcW85HadGEM15all[[i]]),sd(pcW85IPSL15all[[i]]),sd(pcW85MIROC15all[[i]]),sd(pcW85NorESM15all[[i]]))
  sd20 <- c(sd(pcW26HadGEM20all[[i]]),sd(pcW26IPSL20all[[i]]),sd(pcW26MIROC20all[[i]]),
            sd(pcW85GFDL20all[[i]]),sd(pcW85HadGEM20all[[i]]),sd(pcW85IPSL20all[[i]]),sd(pcW85MIROC20all[[i]]),sd(pcW85NorESM20all[[i]]))
  sd30 <- c(sd(pcW85GFDL30all[[i]]),sd(pcW85HadGEM30all[[i]]),sd(pcW85IPSL30all[[i]]),sd(pcW85MIROC30all[[i]]),sd(pcW85NorESM30all[[i]]))
  
  pchange <- c(mean(avg15[1:4]),mean(avg20[1:3]),0,mean(avg15[5:9]),mean(avg20[4:8]),mean(avg30))
  sds <- c(mean(sd15[1:4]),mean(sd20[1:3]),0,mean(sd15[5:9]),mean(sd20[4:8]),mean(sd30))
  
  Threshold <- c("1.5","2.0","3.0","1.5","2.0","3.0")
  
  Scenario <- c("RCP2.6","RCP2.6","RCP2.6","RCP8.5","RCP8.5","RCP8.5")
  
  pcdata <- data.frame(pchange,Threshold,sds, Scenario)
  
  waterp <- ggplot(pcdata, aes(x=Threshold, y=pchange,fill=Scenario)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=pchange-sds, ymax=pchange+sds), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Water Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
    scale_fill_manual(values=c('#66c2a5','#fc8d62'))
  
  
  avg15 <- c(mean(pcE26HadGEM15all[[i]]),mean(pcE26IPSL15all[[i]]),mean(pcE26MIROC15all[[i]]),mean(pcE26NorESM15all[[i]]),
             mean(pcE85GFDL15all[[i]]),mean(pcE85HadGEM15all[[i]]),mean(pcE85IPSL15all[[i]]),mean(pcE85MIROC15all[[i]]),mean(pcE85NorESM15all[[i]]))
  avg20 <- c(mean(pcE26HadGEM20all[[i]]),mean(pcE26IPSL20all[[i]]),mean(pcE26MIROC20all[[i]]),
             mean(pcE85GFDL20all[[i]]),mean(pcE85HadGEM20all[[i]]),mean(pcE85IPSL20all[[i]]),mean(pcE85MIROC20all[[i]]),mean(pcE85NorESM20all[[i]]))
  avg30 <- c(mean(pcE85GFDL30all[[i]]),mean(pcE85HadGEM30all[[i]]),mean(pcE85IPSL30all[[i]]),mean(pcE85MIROC30all[[i]]),mean(pcE85NorESM30all[[i]]))
  
  sd15 <- c(sd(pcE26HadGEM15all[[i]]),sd(pcE26IPSL15all[[i]]),sd(pcE26MIROC15all[[i]]),sd(pcE26NorESM15all[[i]]),
            sd(pcE85GFDL15all[[i]]),sd(pcE85HadGEM15all[[i]]),sd(pcE85IPSL15all[[i]]),sd(pcE85MIROC15all[[i]]),sd(pcE85NorESM15all[[i]]))
  sd20 <- c(sd(pcE26HadGEM20all[[i]]),sd(pcE26IPSL20all[[i]]),sd(pcE26MIROC20all[[i]]),
            sd(pcE85GFDL20all[[i]]),sd(pcE85HadGEM20all[[i]]),sd(pcE85IPSL20all[[i]]),sd(pcE85MIROC20all[[i]]),sd(pcE85NorESM20all[[i]]))
  sd30 <- c(sd(pcE85GFDL30all[[i]]),sd(pcE85HadGEM30all[[i]]),sd(pcE85IPSL30all[[i]]),sd(pcE85MIROC30all[[i]]),sd(pcE85NorESM30all[[i]]))
  
  pchange <- c(mean(avg15[1:4]),mean(avg20[1:3]),0,mean(avg15[5:9]),mean(avg20[4:8]),mean(avg30))
  sds <- c(mean(sd15[1:4]),mean(sd20[1:3]),0,mean(sd15[5:9]),mean(sd20[4:8]),mean(sd30))
  
  pcdata <- data.frame(pchange,Threshold,sds,Scenario)
  
  elecp <-ggplot(pcdata, aes(x=Threshold, y=pchange, fill=Scenario)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=pchange-sds, ymax=pchange+sds), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Electricity Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
    scale_fill_manual(values=c('#66c2a5','#fc8d62'))

  grid.arrange(waterp,elecp,ncol=2)
  
}
# percent change per city 
for (i in 1:6) {
  
  avg15 <- c(mean(pcW26HadGEM15all[[i]]),mean(pcW26IPSL15all[[i]]),mean(pcW26MIROC15all[[i]]),mean(pcW26NorESM15all[[i]]),
             mean(pcW85GFDL15all[[i]]),mean(pcW85HadGEM15all[[i]]),mean(pcW85IPSL15all[[i]]),mean(pcW85MIROC15all[[i]]),mean(pcW85NorESM15all[[i]]))
  avg20 <- c(mean(pcW26HadGEM20all[[i]]),mean(pcW26IPSL20all[[i]]),mean(pcW26MIROC20all[[i]]),
             mean(pcW85GFDL20all[[i]]),mean(pcW85HadGEM20all[[i]]),mean(pcW85IPSL20all[[i]]),mean(pcW85MIROC20all[[i]]),mean(pcW85NorESM20all[[i]]))
  avg30 <- c(mean(pcW85GFDL30all[[i]]),mean(pcW85HadGEM30all[[i]]),mean(pcW85IPSL30all[[i]]),mean(pcW85MIROC30all[[i]]),mean(pcW85NorESM30all[[i]]))
  
  sd15 <- c(sd(pcW26HadGEM15all[[i]]),sd(pcW26IPSL15all[[i]]),sd(pcW26MIROC15all[[i]]),sd(pcW26NorESM15all[[i]]),
            sd(pcW85GFDL15all[[i]]),sd(pcW85HadGEM15all[[i]]),sd(pcW85IPSL15all[[i]]),sd(pcW85MIROC15all[[i]]),sd(pcW85NorESM15all[[i]]))
  sd20 <- c(sd(pcW26HadGEM20all[[i]]),sd(pcW26IPSL20all[[i]]),sd(pcW26MIROC20all[[i]]),
            sd(pcW85GFDL20all[[i]]),sd(pcW85HadGEM20all[[i]]),sd(pcW85IPSL20all[[i]]),sd(pcW85MIROC20all[[i]]),sd(pcW85NorESM20all[[i]]))
  sd30 <- c(sd(pcW85GFDL30all[[i]]),sd(pcW85HadGEM30all[[i]]),sd(pcW85IPSL30all[[i]]),sd(pcW85MIROC30all[[i]]),sd(pcW85NorESM30all[[i]]))
  
  pchange <- c(0,avg15[1:4],0,avg20[1:3],0,avg15[5:9],avg20[4:8],avg30)
  sds <- c(0,sd15[1:4],0,sd20[1:3],0,sd15[5:9],sd20[4:8],sd30)
  
  Scenario <- c("RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP2.6",
                "RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP2.6",
                "RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5",
                "RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5",
                "RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5")
  
  Threshold <- c("1.5","1.5","1.5","1.5","1.5",
                 "2.0","2.0","2.0","2.0","2.0",
                 "1.5","1.5","1.5","1.5","1.5",
                 "2.0","2.0","2.0","2.0","2.0",
                 "3.0","3.0","3.0","3.0","3.0")
  
  Model <- c("GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M")
  
  pcdata <- data.frame(pchange,Threshold,sds, Scenario,Model)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange,fill=Model)) + geom_bar(position="dodge", stat="identity") +
          geom_errorbar(aes(x=Threshold, ymin=pchange-sds, ymax=pchange+sds), width=.2, position=position_dodge(.9)) +
          xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Water Use')+
          theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
          scale_fill_manual(values=c('#fb9a99','#a6cee3','#1f78b4','#b2df8a','#33a02c'))+
          facet_grid(Scenario ~ .)) 
  
  
  avg15 <- c(mean(pcE26HadGEM15all[[i]]),mean(pcE26IPSL15all[[i]]),mean(pcE26MIROC15all[[i]]),mean(pcE26NorESM15all[[i]]),
             mean(pcE85GFDL15all[[i]]),mean(pcE85HadGEM15all[[i]]),mean(pcE85IPSL15all[[i]]),mean(pcE85MIROC15all[[i]]),mean(pcE85NorESM15all[[i]]))
  avg20 <- c(mean(pcE26HadGEM20all[[i]]),mean(pcE26IPSL20all[[i]]),mean(pcE26MIROC20all[[i]]),
             mean(pcE85GFDL20all[[i]]),mean(pcE85HadGEM20all[[i]]),mean(pcE85IPSL20all[[i]]),mean(pcE85MIROC20all[[i]]),mean(pcE85NorESM20all[[i]]))
  avg30 <- c(mean(pcE85GFDL30all[[i]]),mean(pcE85HadGEM30all[[i]]),mean(pcE85IPSL30all[[i]]),mean(pcE85MIROC30all[[i]]),mean(pcE85NorESM30all[[i]]))
  
  sd15 <- c(sd(pcE26HadGEM15all[[i]]),sd(pcE26IPSL15all[[i]]),sd(pcE26MIROC15all[[i]]),sd(pcE26NorESM15all[[i]]),
            sd(pcE85GFDL15all[[i]]),sd(pcE85HadGEM15all[[i]]),sd(pcE85IPSL15all[[i]]),sd(pcE85MIROC15all[[i]]),sd(pcE85NorESM15all[[i]]))
  sd20 <- c(sd(pcE26HadGEM20all[[i]]),sd(pcE26IPSL20all[[i]]),sd(pcE26MIROC20all[[i]]),
            sd(pcE85GFDL20all[[i]]),sd(pcE85HadGEM20all[[i]]),sd(pcE85IPSL20all[[i]]),sd(pcE85MIROC20all[[i]]),sd(pcE85NorESM20all[[i]]))
  sd30 <- c(sd(pcE85GFDL30all[[i]]),sd(pcE85HadGEM30all[[i]]),sd(pcE85IPSL30all[[i]]),sd(pcE85MIROC30all[[i]]),sd(pcE85NorESM30all[[i]]))
  
  pchange <- c(0,avg15[1:4],0,avg20[1:3],0,avg15[5:9],avg20[4:8],avg30)
  sds <- c(0,sd15[1:4],0,sd20[1:3],0,sd15[5:9],sd20[4:8],sd30)
  
  pcdata <- data.frame(pchange,Threshold,sds,Scenario)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange, fill=Model)) + geom_bar(position="dodge", stat="identity") +
          geom_errorbar(aes(x=Threshold, ymin=pchange-sds, ymax=pchange+sds), width=.2, position=position_dodge(.9)) +
          xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Electricity Use')+
          theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
          scale_fill_manual(values=c('#fb9a99','#a6cee3','#1f78b4','#b2df8a','#33a02c')) +
          facet_grid(Scenario ~ .))
}

# medians + 25/75 quantiles
# percent change per city (all scenarios/models)
for (i in 1:6) {
  
  med15 <- c(median(pcW26HadGEM15all[[i]]),median(pcW26IPSL15all[[i]]),median(pcW26MIROC15all[[i]]),median(pcW26NorESM15all[[i]]),
             median(pcW85GFDL15all[[i]]),median(pcW85HadGEM15all[[i]]),median(pcW85IPSL15all[[i]]),median(pcW85MIROC15all[[i]]),median(pcW85NorESM15all[[i]]))
  med20 <- c(median(pcW26HadGEM20all[[i]]),median(pcW26IPSL20all[[i]]),median(pcW26MIROC20all[[i]]),
             median(pcW85GFDL20all[[i]]),median(pcW85HadGEM20all[[i]]),median(pcW85IPSL20all[[i]]),median(pcW85MIROC20all[[i]]),median(pcW85NorESM20all[[i]]))
  med30 <- c(median(pcW85GFDL30all[[i]]),median(pcW85HadGEM30all[[i]]),median(pcW85IPSL30all[[i]]),median(pcW85MIROC30all[[i]]),median(pcW85NorESM30all[[i]]))
  
  q2515 <- c(quantile(pcW26HadGEM15all[[i]],0.25),quantile(pcW26IPSL15all[[i]],0.25),quantile(pcW26MIROC15all[[i]],0.25),quantile(pcW26NorESM15all[[i]],0.25),
             quantile(pcW85GFDL15all[[i]],0.25),quantile(pcW85HadGEM15all[[i]],0.25),quantile(pcW85IPSL15all[[i]],0.25),quantile(pcW85MIROC15all[[i]],0.25),quantile(pcW85NorESM15all[[i]],0.25))
  q2520 <- c(quantile(pcW26HadGEM20all[[i]],0.25),quantile(pcW26IPSL20all[[i]],0.25),quantile(pcW26MIROC20all[[i]],0.25),
             quantile(pcW85GFDL20all[[i]],0.25),quantile(pcW85HadGEM20all[[i]],0.25),quantile(pcW85IPSL20all[[i]],0.25),quantile(pcW85MIROC20all[[i]],0.25),quantile(pcW85NorESM20all[[i]],0.25))
  q2530 <- c(quantile(pcW85GFDL30all[[i]],0.25),quantile(pcW85HadGEM30all[[i]],0.25),quantile(pcW85IPSL30all[[i]],0.25),quantile(pcW85MIROC30all[[i]],0.25),quantile(pcW85NorESM30all[[i]],0.25))
  
  q7515 <- c(quantile(pcW26HadGEM15all[[i]],0.75),quantile(pcW26IPSL15all[[i]],0.75),quantile(pcW26MIROC15all[[i]],0.75),quantile(pcW26NorESM15all[[i]],0.75),
             quantile(pcW85GFDL15all[[i]],0.75),quantile(pcW85HadGEM15all[[i]],0.75),quantile(pcW85IPSL15all[[i]],0.75),quantile(pcW85MIROC15all[[i]],0.75),quantile(pcW85NorESM15all[[i]],0.75))
  q7520 <- c(quantile(pcW26HadGEM20all[[i]],0.75),quantile(pcW26IPSL20all[[i]],0.75),quantile(pcW26MIROC20all[[i]],0.75),
             quantile(pcW85GFDL20all[[i]],0.75),quantile(pcW85HadGEM20all[[i]],0.75),quantile(pcW85IPSL20all[[i]],0.75),quantile(pcW85MIROC20all[[i]],0.75),quantile(pcW85NorESM20all[[i]],0.75))
  q7530 <- c(quantile(pcW85GFDL30all[[i]],0.75),quantile(pcW85HadGEM30all[[i]],0.75),quantile(pcW85IPSL30all[[i]],0.75),quantile(pcW85MIROC30all[[i]],0.75),quantile(pcW85NorESM30all[[i]],0.75))
  
  pchange <- c(median(med15),median(med20),median(med30))
  q25 <- c(median(q2515),median(q2520),median(q2530)) 
  q75 <- c(median(q7515),median(q7520),median(q7530)) 
  
  Threshold <- c("1.5","2.0","3.0")
  
  pcdata <- data.frame(pchange,Threshold,q25,q75)
  
  waterp <- ggplot(pcdata, aes(x=Threshold, y=pchange)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=q25, ymax=q75), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Water Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) #+
 
  
  med15 <- c(median(pcE26HadGEM15all[[i]]),median(pcE26IPSL15all[[i]]),median(pcE26MIROC15all[[i]]),median(pcE26NorESM15all[[i]]),
             median(pcE85GFDL15all[[i]]),median(pcE85HadGEM15all[[i]]),median(pcE85IPSL15all[[i]]),median(pcE85MIROC15all[[i]]),median(pcE85NorESM15all[[i]]))
  med20 <- c(median(pcE26HadGEM20all[[i]]),median(pcE26IPSL20all[[i]]),median(pcE26MIROC20all[[i]]),
             median(pcE85GFDL20all[[i]]),median(pcE85HadGEM20all[[i]]),median(pcE85IPSL20all[[i]]),median(pcE85MIROC20all[[i]]),median(pcE85NorESM20all[[i]]))
  med30 <- c(median(pcE85GFDL30all[[i]]),median(pcE85HadGEM30all[[i]]),median(pcE85IPSL30all[[i]]),median(pcE85MIROC30all[[i]]),median(pcE85NorESM30all[[i]]))
  
  q2515 <- c(quantile(pcE26HadGEM15all[[i]],0.25),quantile(pcE26IPSL15all[[i]],0.25),quantile(pcE26MIROC15all[[i]],0.25),quantile(pcE26NorESM15all[[i]],0.25),
             quantile(pcE85GFDL15all[[i]],0.25),quantile(pcE85HadGEM15all[[i]],0.25),quantile(pcE85IPSL15all[[i]],0.25),quantile(pcE85MIROC15all[[i]],0.25),quantile(pcE85NorESM15all[[i]],0.25))
  q2520 <- c(quantile(pcE26HadGEM20all[[i]],0.25),quantile(pcE26IPSL20all[[i]],0.25),quantile(pcE26MIROC20all[[i]],0.25),
             quantile(pcE85GFDL20all[[i]],0.25),quantile(pcE85HadGEM20all[[i]],0.25),quantile(pcE85IPSL20all[[i]],0.25),quantile(pcE85MIROC20all[[i]],0.25),quantile(pcE85NorESM20all[[i]],0.25))
  q2530 <- c(quantile(pcE85GFDL30all[[i]],0.25),quantile(pcE85HadGEM30all[[i]],0.25),quantile(pcE85IPSL30all[[i]],0.25),quantile(pcE85MIROC30all[[i]],0.25),quantile(pcE85NorESM30all[[i]],0.25))
  
  q7515 <- c(quantile(pcE26HadGEM15all[[i]],0.75),quantile(pcE26IPSL15all[[i]],0.75),quantile(pcE26MIROC15all[[i]],0.75),quantile(pcE26NorESM15all[[i]],0.75),
             quantile(pcE85GFDL15all[[i]],0.75),quantile(pcE85HadGEM15all[[i]],0.75),quantile(pcE85IPSL15all[[i]],0.75),quantile(pcE85MIROC15all[[i]],0.75),quantile(pcE85NorESM15all[[i]],0.75))
  q7520 <- c(quantile(pcE26HadGEM20all[[i]],0.75),quantile(pcE26IPSL20all[[i]],0.75),quantile(pcE26MIROC20all[[i]],0.75),
             quantile(pcE85GFDL20all[[i]],0.75),quantile(pcE85HadGEM20all[[i]],0.75),quantile(pcE85IPSL20all[[i]],0.75),quantile(pcE85MIROC20all[[i]],0.75),quantile(pcE85NorESM20all[[i]],0.75))
  q7530 <- c(quantile(pcE85GFDL30all[[i]],0.75),quantile(pcE85HadGEM30all[[i]],0.75),quantile(pcE85IPSL30all[[i]],0.75),quantile(pcE85MIROC30all[[i]],0.75),quantile(pcE85NorESM30all[[i]],0.75))
  
  pchange <- c(median(med15),median(med20),median(med30))
  q25 <- c(median(q2515),median(q2520),median(q2530)) 
  q75 <- c(median(q7515),median(q7520),median(q7530)) 
  
  pcdata <- data.frame(pchange,Threshold,q25,q75)
  
  elecp <-ggplot(pcdata, aes(x=Threshold, y=pchange)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=q25, ymax=q75), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Electricity Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) 
  
  grid.arrange(waterp,elecp,ncol=2)
  
}
# percent change per city (all models)
for (i in 1:6) {
  
  med15 <- c(median(pcW26HadGEM15all[[i]]),median(pcW26IPSL15all[[i]]),median(pcW26MIROC15all[[i]]),median(pcW26NorESM15all[[i]]),
             median(pcW85GFDL15all[[i]]),median(pcW85HadGEM15all[[i]]),median(pcW85IPSL15all[[i]]),median(pcW85MIROC15all[[i]]),median(pcW85NorESM15all[[i]]))
  med20 <- c(median(pcW26HadGEM20all[[i]]),median(pcW26IPSL20all[[i]]),median(pcW26MIROC20all[[i]]),
             median(pcW85GFDL20all[[i]]),median(pcW85HadGEM20all[[i]]),median(pcW85IPSL20all[[i]]),median(pcW85MIROC20all[[i]]),median(pcW85NorESM20all[[i]]))
  med30 <- c(median(pcW85GFDL30all[[i]]),median(pcW85HadGEM30all[[i]]),median(pcW85IPSL30all[[i]]),median(pcW85MIROC30all[[i]]),median(pcW85NorESM30all[[i]]))
  
  q2515 <- c(quantile(pcW26HadGEM15all[[i]],0.25),quantile(pcW26IPSL15all[[i]],0.25),quantile(pcW26MIROC15all[[i]],0.25),quantile(pcW26NorESM15all[[i]],0.25),
             quantile(pcW85GFDL15all[[i]],0.25),quantile(pcW85HadGEM15all[[i]],0.25),quantile(pcW85IPSL15all[[i]],0.25),quantile(pcW85MIROC15all[[i]],0.25),quantile(pcW85NorESM15all[[i]],0.25))
  q2520 <- c(quantile(pcW26HadGEM20all[[i]],0.25),quantile(pcW26IPSL20all[[i]],0.25),quantile(pcW26MIROC20all[[i]],0.25),
             quantile(pcW85GFDL20all[[i]],0.25),quantile(pcW85HadGEM20all[[i]],0.25),quantile(pcW85IPSL20all[[i]],0.25),quantile(pcW85MIROC20all[[i]],0.25),quantile(pcW85NorESM20all[[i]],0.25))
  q2530 <- c(quantile(pcW85GFDL30all[[i]],0.25),quantile(pcW85HadGEM30all[[i]],0.25),quantile(pcW85IPSL30all[[i]],0.25),quantile(pcW85MIROC30all[[i]],0.25),quantile(pcW85NorESM30all[[i]],0.25))
  
  q7515 <- c(quantile(pcW26HadGEM15all[[i]],0.75),quantile(pcW26IPSL15all[[i]],0.75),quantile(pcW26MIROC15all[[i]],0.75),quantile(pcW26NorESM15all[[i]],0.75),
             quantile(pcW85GFDL15all[[i]],0.75),quantile(pcW85HadGEM15all[[i]],0.75),quantile(pcW85IPSL15all[[i]],0.75),quantile(pcW85MIROC15all[[i]],0.75),quantile(pcW85NorESM15all[[i]],0.75))
  q7520 <- c(quantile(pcW26HadGEM20all[[i]],0.75),quantile(pcW26IPSL20all[[i]],0.75),quantile(pcW26MIROC20all[[i]],0.75),
             quantile(pcW85GFDL20all[[i]],0.75),quantile(pcW85HadGEM20all[[i]],0.75),quantile(pcW85IPSL20all[[i]],0.75),quantile(pcW85MIROC20all[[i]],0.75),quantile(pcW85NorESM20all[[i]],0.75))
  q7530 <- c(quantile(pcW85GFDL30all[[i]],0.75),quantile(pcW85HadGEM30all[[i]],0.75),quantile(pcW85IPSL30all[[i]],0.75),quantile(pcW85MIROC30all[[i]],0.75),quantile(pcW85NorESM30all[[i]],0.75))

  pchange <- c(median(med15[1:4]),median(med20[1:3]),0,median(med15[5:9]),median(med20[4:8]),median(med30))
  q25 <- c(median(q2515[1:4]),median(q2520[1:3]),0,median(q2515[5:9]),median(q2520[4:8]),median(q2530))
  q75 <- c(median(q7515[1:4]),median(q7520[1:3]),0,median(q7515[5:9]),median(q7520[4:8]),median(q7530))
  
  Threshold <- c("1.5","2.0","3.0","1.5","2.0","3.0")
  
  Scenario <- c("RCP2.6","RCP2.6","RCP2.6","RCP8.5","RCP8.5","RCP8.5")
  
  pcdata <- data.frame(pchange,Threshold,q25,q75, Scenario)
  
  waterp <- ggplot(pcdata, aes(x=Threshold, y=pchange,fill=Scenario)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=q25, ymax=q75), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Water Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
    scale_fill_manual(values=c('#66c2a5','#fc8d62'))
  
  med15 <- c(median(pcE26HadGEM15all[[i]]),median(pcE26IPSL15all[[i]]),median(pcE26MIROC15all[[i]]),median(pcE26NorESM15all[[i]]),
             median(pcE85GFDL15all[[i]]),median(pcE85HadGEM15all[[i]]),median(pcE85IPSL15all[[i]]),median(pcE85MIROC15all[[i]]),median(pcE85NorESM15all[[i]]))
  med20 <- c(median(pcE26HadGEM20all[[i]]),median(pcE26IPSL20all[[i]]),median(pcE26MIROC20all[[i]]),
             median(pcE85GFDL20all[[i]]),median(pcE85HadGEM20all[[i]]),median(pcE85IPSL20all[[i]]),median(pcE85MIROC20all[[i]]),median(pcE85NorESM20all[[i]]))
  med30 <- c(median(pcE85GFDL30all[[i]]),median(pcE85HadGEM30all[[i]]),median(pcE85IPSL30all[[i]]),median(pcE85MIROC30all[[i]]),median(pcE85NorESM30all[[i]]))
  
  q2515 <- c(quantile(pcE26HadGEM15all[[i]],0.25),quantile(pcE26IPSL15all[[i]],0.25),quantile(pcE26MIROC15all[[i]],0.25),quantile(pcE26NorESM15all[[i]],0.25),
             quantile(pcE85GFDL15all[[i]],0.25),quantile(pcE85HadGEM15all[[i]],0.25),quantile(pcE85IPSL15all[[i]],0.25),quantile(pcE85MIROC15all[[i]],0.25),quantile(pcE85NorESM15all[[i]],0.25))
  q2520 <- c(quantile(pcE26HadGEM20all[[i]],0.25),quantile(pcE26IPSL20all[[i]],0.25),quantile(pcE26MIROC20all[[i]],0.25),
             quantile(pcE85GFDL20all[[i]],0.25),quantile(pcE85HadGEM20all[[i]],0.25),quantile(pcE85IPSL20all[[i]],0.25),quantile(pcE85MIROC20all[[i]],0.25),quantile(pcE85NorESM20all[[i]],0.25))
  q2530 <- c(quantile(pcE85GFDL30all[[i]],0.25),quantile(pcE85HadGEM30all[[i]],0.25),quantile(pcE85IPSL30all[[i]],0.25),quantile(pcE85MIROC30all[[i]],0.25),quantile(pcE85NorESM30all[[i]],0.25))
  
  q7515 <- c(quantile(pcE26HadGEM15all[[i]],0.75),quantile(pcE26IPSL15all[[i]],0.75),quantile(pcE26MIROC15all[[i]],0.75),quantile(pcE26NorESM15all[[i]],0.75),
             quantile(pcE85GFDL15all[[i]],0.75),quantile(pcE85HadGEM15all[[i]],0.75),quantile(pcE85IPSL15all[[i]],0.75),quantile(pcE85MIROC15all[[i]],0.75),quantile(pcE85NorESM15all[[i]],0.75))
  q7520 <- c(quantile(pcE26HadGEM20all[[i]],0.75),quantile(pcE26IPSL20all[[i]],0.75),quantile(pcE26MIROC20all[[i]],0.75),
             quantile(pcE85GFDL20all[[i]],0.75),quantile(pcE85HadGEM20all[[i]],0.75),quantile(pcE85IPSL20all[[i]],0.75),quantile(pcE85MIROC20all[[i]],0.75),quantile(pcE85NorESM20all[[i]],0.75))
  q7530 <- c(quantile(pcE85GFDL30all[[i]],0.75),quantile(pcE85HadGEM30all[[i]],0.75),quantile(pcE85IPSL30all[[i]],0.75),quantile(pcE85MIROC30all[[i]],0.75),quantile(pcE85NorESM30all[[i]],0.75))
  
  pchange <- c(median(med15[1:4]),median(med20[1:3]),0,median(med15[5:9]),median(med20[4:8]),median(med30))
  q25 <- c(median(q2515[1:4]),median(q2520[1:3]),0,median(q2515[5:9]),median(q2520[4:8]),median(q2530))
  q75 <- c(median(q7515[1:4]),median(q7520[1:3]),0,median(q7515[5:9]),median(q7520[4:8]),median(q7530))
  
  pcdata <- data.frame(pchange,Threshold,q25,q75,Scenario)
  
  elecp <-ggplot(pcdata, aes(x=Threshold, y=pchange, fill=Scenario)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=q25, ymax=q75), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Electricity Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
    scale_fill_manual(values=c('#66c2a5','#fc8d62'))
  
  grid.arrange(waterp,elecp,ncol=2)
  
}
# percent change per city 
for (i in 1:6) {
  
  med15 <- c(median(pcW26HadGEM15all[[i]]),median(pcW26IPSL15all[[i]]),median(pcW26MIROC15all[[i]]),median(pcW26NorESM15all[[i]]),
             median(pcW85GFDL15all[[i]]),median(pcW85HadGEM15all[[i]]),median(pcW85IPSL15all[[i]]),median(pcW85MIROC15all[[i]]),median(pcW85NorESM15all[[i]]))
  med20 <- c(median(pcW26HadGEM20all[[i]]),median(pcW26IPSL20all[[i]]),median(pcW26MIROC20all[[i]]),
             median(pcW85GFDL20all[[i]]),median(pcW85HadGEM20all[[i]]),median(pcW85IPSL20all[[i]]),median(pcW85MIROC20all[[i]]),median(pcW85NorESM20all[[i]]))
  med30 <- c(median(pcW85GFDL30all[[i]]),median(pcW85HadGEM30all[[i]]),median(pcW85IPSL30all[[i]]),median(pcW85MIROC30all[[i]]),median(pcW85NorESM30all[[i]]))
  
  q2515 <- c(quantile(pcW26HadGEM15all[[i]],0.25),quantile(pcW26IPSL15all[[i]],0.25),quantile(pcW26MIROC15all[[i]],0.25),quantile(pcW26NorESM15all[[i]],0.25),
             quantile(pcW85GFDL15all[[i]],0.25),quantile(pcW85HadGEM15all[[i]],0.25),quantile(pcW85IPSL15all[[i]],0.25),quantile(pcW85MIROC15all[[i]],0.25),quantile(pcW85NorESM15all[[i]],0.25))
  q2520 <- c(quantile(pcW26HadGEM20all[[i]],0.25),quantile(pcW26IPSL20all[[i]],0.25),quantile(pcW26MIROC20all[[i]],0.25),
             quantile(pcW85GFDL20all[[i]],0.25),quantile(pcW85HadGEM20all[[i]],0.25),quantile(pcW85IPSL20all[[i]],0.25),quantile(pcW85MIROC20all[[i]],0.25),quantile(pcW85NorESM20all[[i]],0.25))
  q2530 <- c(quantile(pcW85GFDL30all[[i]],0.25),quantile(pcW85HadGEM30all[[i]],0.25),quantile(pcW85IPSL30all[[i]],0.25),quantile(pcW85MIROC30all[[i]],0.25),quantile(pcW85NorESM30all[[i]],0.25))
  
  q7515 <- c(quantile(pcW26HadGEM15all[[i]],0.75),quantile(pcW26IPSL15all[[i]],0.75),quantile(pcW26MIROC15all[[i]],0.75),quantile(pcW26NorESM15all[[i]],0.75),
             quantile(pcW85GFDL15all[[i]],0.75),quantile(pcW85HadGEM15all[[i]],0.75),quantile(pcW85IPSL15all[[i]],0.75),quantile(pcW85MIROC15all[[i]],0.75),quantile(pcW85NorESM15all[[i]],0.75))
  q7520 <- c(quantile(pcW26HadGEM20all[[i]],0.75),quantile(pcW26IPSL20all[[i]],0.75),quantile(pcW26MIROC20all[[i]],0.75),
             quantile(pcW85GFDL20all[[i]],0.75),quantile(pcW85HadGEM20all[[i]],0.75),quantile(pcW85IPSL20all[[i]],0.75),quantile(pcW85MIROC20all[[i]],0.75),quantile(pcW85NorESM20all[[i]],0.75))
  q7530 <- c(quantile(pcW85GFDL30all[[i]],0.75),quantile(pcW85HadGEM30all[[i]],0.75),quantile(pcW85IPSL30all[[i]],0.75),quantile(pcW85MIROC30all[[i]],0.75),quantile(pcW85NorESM30all[[i]],0.75))
  
  pchange <- c(0,med15[1:4],0,med20[1:3],0,med15[5:9],med20[4:8],med30)
  q25 <- c(0,q2515[1:4],0,q2520[1:3],0,q2515[5:9],q2520[4:8],q2530)
  q75 <- c(0,q7515[1:4],0,q7520[1:3],0,q7515[5:9],q7520[4:8],q7530)
  
  Scenario <- c("RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP2.6",
                "RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP2.6",
                "RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5",
                "RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5",
                "RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5")
  
  Threshold <- c("1.5","1.5","1.5","1.5","1.5",
                 "2.0","2.0","2.0","2.0","2.0",
                 "1.5","1.5","1.5","1.5","1.5",
                 "2.0","2.0","2.0","2.0","2.0",
                 "3.0","3.0","3.0","3.0","3.0")
  
  Model <- c("GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M")
  
  pcdata <- data.frame(pchange,Threshold,q25,q75, Scenario,Model)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange,fill=Model)) + geom_bar(position="dodge", stat="identity") +
          geom_errorbar(aes(x=Threshold, ymin=q25, ymax=q75), width=.2, position=position_dodge(.9)) +
          xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Water Use')+
          theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
          scale_fill_manual(values=c('#fb9a99','#a6cee3','#1f78b4','#b2df8a','#33a02c'))+
          facet_grid(Scenario ~ .)) 
  
  
  med15 <- c(median(pcE26HadGEM15all[[i]]),median(pcE26IPSL15all[[i]]),median(pcE26MIROC15all[[i]]),median(pcE26NorESM15all[[i]]),
             median(pcE85GFDL15all[[i]]),median(pcE85HadGEM15all[[i]]),median(pcE85IPSL15all[[i]]),median(pcE85MIROC15all[[i]]),median(pcE85NorESM15all[[i]]))
  med20 <- c(median(pcE26HadGEM20all[[i]]),median(pcE26IPSL20all[[i]]),median(pcE26MIROC20all[[i]]),
             median(pcE85GFDL20all[[i]]),median(pcE85HadGEM20all[[i]]),median(pcE85IPSL20all[[i]]),median(pcE85MIROC20all[[i]]),median(pcE85NorESM20all[[i]]))
  med30 <- c(median(pcE85GFDL30all[[i]]),median(pcE85HadGEM30all[[i]]),median(pcE85IPSL30all[[i]]),median(pcE85MIROC30all[[i]]),median(pcE85NorESM30all[[i]]))
  
  q2515 <- c(quantile(pcE26HadGEM15all[[i]],0.25),quantile(pcE26IPSL15all[[i]],0.25),quantile(pcE26MIROC15all[[i]],0.25),quantile(pcE26NorESM15all[[i]],0.25),
             quantile(pcE85GFDL15all[[i]],0.25),quantile(pcE85HadGEM15all[[i]],0.25),quantile(pcE85IPSL15all[[i]],0.25),quantile(pcE85MIROC15all[[i]],0.25),quantile(pcE85NorESM15all[[i]],0.25))
  q2520 <- c(quantile(pcE26HadGEM20all[[i]],0.25),quantile(pcE26IPSL20all[[i]],0.25),quantile(pcE26MIROC20all[[i]],0.25),
             quantile(pcE85GFDL20all[[i]],0.25),quantile(pcE85HadGEM20all[[i]],0.25),quantile(pcE85IPSL20all[[i]],0.25),quantile(pcE85MIROC20all[[i]],0.25),quantile(pcE85NorESM20all[[i]],0.25))
  q2530 <- c(quantile(pcE85GFDL30all[[i]],0.25),quantile(pcE85HadGEM30all[[i]],0.25),quantile(pcE85IPSL30all[[i]],0.25),quantile(pcE85MIROC30all[[i]],0.25),quantile(pcE85NorESM30all[[i]],0.25))
  
  q7515 <- c(quantile(pcE26HadGEM15all[[i]],0.75),quantile(pcE26IPSL15all[[i]],0.75),quantile(pcE26MIROC15all[[i]],0.75),quantile(pcE26NorESM15all[[i]],0.75),
             quantile(pcE85GFDL15all[[i]],0.75),quantile(pcE85HadGEM15all[[i]],0.75),quantile(pcE85IPSL15all[[i]],0.75),quantile(pcE85MIROC15all[[i]],0.75),quantile(pcE85NorESM15all[[i]],0.75))
  q7520 <- c(quantile(pcE26HadGEM20all[[i]],0.75),quantile(pcE26IPSL20all[[i]],0.75),quantile(pcE26MIROC20all[[i]],0.75),
             quantile(pcE85GFDL20all[[i]],0.75),quantile(pcE85HadGEM20all[[i]],0.75),quantile(pcE85IPSL20all[[i]],0.75),quantile(pcE85MIROC20all[[i]],0.75),quantile(pcE85NorESM20all[[i]],0.75))
  q7530 <- c(quantile(pcE85GFDL30all[[i]],0.75),quantile(pcE85HadGEM30all[[i]],0.75),quantile(pcE85IPSL30all[[i]],0.75),quantile(pcE85MIROC30all[[i]],0.75),quantile(pcE85NorESM30all[[i]],0.75))
  
  pchange <- c(0,med15[1:4],0,med20[1:3],0,med15[5:9],med20[4:8],med30)
  q25 <- c(0,q2515[1:4],0,q2520[1:3],0,q2515[5:9],q2520[4:8],q2530)
  q75 <- c(0,q7515[1:4],0,q7520[1:3],0,q7515[5:9],q7520[4:8],q7530)
  
  pcdata <- data.frame(pchange,Threshold,q25,q75,Scenario)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange, fill=Model)) + geom_bar(position="dodge", stat="identity") +
          geom_errorbar(aes(x=Threshold, ymin=q25, ymax=q75), width=.2, position=position_dodge(.9)) +
          xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Change in Electricity Use')+
          theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
          scale_fill_manual(values=c('#fb9a99','#a6cee3','#1f78b4','#b2df8a','#33a02c')) +
          facet_grid(Scenario ~ .))
}



#################################   FIGURES - MOVING AVERAGE   ############################

date <- rollmean(seq(as.Date("2006-01-01"), as.Date("2099-12-31"), by="months"),maperiod)


# future water use

# RCP 8.5
ens <- scale(MAwaterpredENS85)

rcp85 <- scale(MAwaterpredMOD85)

means <- data.frame(rowMeans(rcp85[,1:5]),rowMeans(rcp85[,6:10]),rowMeans(rcp85[,11:15]),rowMeans(rcp85[,16:20]),rowMeans(rcp85[,21:25]),rowMeans(rcp85[,26:30]))

mins <- data.frame(apply(rcp85[,1:5],1,FUN = min),apply(rcp85[,6:10],1,FUN = min),apply(rcp85[,11:15],1,FUN = min),apply(rcp85[,16:20],1,FUN = min),apply(rcp85[,21:25],1,FUN = min),apply(rcp85[,26:30],1,FUN = min))
maxs <- data.frame(apply(rcp85[,1:5],1,FUN = max),apply(rcp85[,6:10],1,FUN = max),apply(rcp85[,11:15],1,FUN = max),apply(rcp85[,16:20],1,FUN = max),apply(rcp85[,21:25],1,FUN = max),apply(rcp85[,26:30],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','indyMean','cbusMean','chiMean','clevMean','madMean','minnMean',
                     'indyMin','cbusMin','chiMin','clevMin','madMin','minnMin',
                     'indyMax','cbusMax','chiMax','clevMax','madMax','minnMax')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.1) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.1) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.1) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.1) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.1) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.1) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Predicted Water Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 6.0
ens <- scale(MAwaterpredENS60)

rcp60 <- scale(MAwaterpredMOD60)

means <- data.frame(rowMeans(rcp60[,1:5]),rowMeans(rcp60[,6:10]),rowMeans(rcp60[,11:15]),rowMeans(rcp60[,16:20]),rowMeans(rcp60[,21:25]),rowMeans(rcp60[,26:30]))

mins <- data.frame(apply(rcp60[,1:5],1,FUN = min),apply(rcp60[,6:10],1,FUN = min),apply(rcp60[,11:15],1,FUN = min),apply(rcp60[,16:20],1,FUN = min),apply(rcp60[,21:25],1,FUN = min),apply(rcp60[,26:30],1,FUN = min))
maxs <- data.frame(apply(rcp60[,1:5],1,FUN = max),apply(rcp60[,6:10],1,FUN = max),apply(rcp60[,11:15],1,FUN = max),apply(rcp60[,16:20],1,FUN = max),apply(rcp60[,21:25],1,FUN = max),apply(rcp60[,26:30],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','indyMean','cbusMean','chiMean','clevMean','madMean','minnMean',
                     'indyMin','cbusMin','chiMin','clevMin','madMin','minnMin',
                     'indyMax','cbusMax','chiMax','clevMax','madMax','minnMax')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.1) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.1) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.1) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.1) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.1) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.1) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Predicted Water Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 4.5
ens <- scale(MAwaterpredENS45)

rcp45 <- scale(MAwaterpredMOD45)

means <- data.frame(rowMeans(rcp45[,1:5]),rowMeans(rcp45[,6:10]),rowMeans(rcp45[,11:15]),rowMeans(rcp45[,16:20]),rowMeans(rcp45[,21:25]),rowMeans(rcp45[,26:30]))

mins <- data.frame(apply(rcp45[,1:5],1,FUN = min),apply(rcp45[,6:10],1,FUN = min),apply(rcp45[,11:15],1,FUN = min),apply(rcp45[,16:20],1,FUN = min),apply(rcp45[,21:25],1,FUN = min),apply(rcp45[,26:30],1,FUN = min))
maxs <- data.frame(apply(rcp45[,1:5],1,FUN = max),apply(rcp45[,6:10],1,FUN = max),apply(rcp45[,11:15],1,FUN = max),apply(rcp45[,16:20],1,FUN = max),apply(rcp45[,21:25],1,FUN = max),apply(rcp45[,26:30],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','indyMean','cbusMean','chiMean','clevMean','madMean','minnMean',
                     'indyMin','cbusMin','chiMin','clevMin','madMin','minnMin',
                     'indyMax','cbusMax','chiMax','clevMax','madMax','minnMax')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.1) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.1) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.1) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.1) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.1) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.1) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Predicted Water Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 2.6
ens <- scale(MAwaterpredENS26)

rcp26 <- scale(MAwaterpredMOD26)

means <- data.frame(rowMeans(rcp26[,1:5]),rowMeans(rcp26[,6:10]),rowMeans(rcp26[,11:15]),rowMeans(rcp26[,16:20]),rowMeans(rcp26[,21:25]),rowMeans(rcp26[,26:30]))

mins <- data.frame(apply(rcp26[,1:5],1,FUN = min),apply(rcp26[,6:10],1,FUN = min),apply(rcp26[,11:15],1,FUN = min),apply(rcp26[,16:20],1,FUN = min),apply(rcp26[,21:25],1,FUN = min),apply(rcp26[,26:30],1,FUN = min))
maxs <- data.frame(apply(rcp26[,1:5],1,FUN = max),apply(rcp26[,6:10],1,FUN = max),apply(rcp26[,11:15],1,FUN = max),apply(rcp26[,16:20],1,FUN = max),apply(rcp26[,21:25],1,FUN = max),apply(rcp26[,26:30],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','indyMean','cbusMean','chiMean','clevMean','madMean','minnMean',
                     'indyMin','cbusMin','chiMin','clevMin','madMin','minnMin',
                     'indyMax','cbusMax','chiMax','clevMax','madMax','minnMax')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.1) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.1) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.1) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.1) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.1) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.1) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Predicted Water Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# future electricity use

# RCP 8.5
ens <- scale(MAelecpredENS85)

rcp85 <- scale(MAelecpredMOD85)

means <- data.frame(rowMeans(rcp85[,1:5]),rowMeans(rcp85[,6:10]),rowMeans(rcp85[,11:15]),rowMeans(rcp85[,16:20]),rowMeans(rcp85[,21:25]),rowMeans(rcp85[,26:30]))

mins <- data.frame(apply(rcp85[,1:5],1,FUN = min),apply(rcp85[,6:10],1,FUN = min),apply(rcp85[,11:15],1,FUN = min),apply(rcp85[,16:20],1,FUN = min),apply(rcp85[,21:25],1,FUN = min),apply(rcp85[,26:30],1,FUN = min))
maxs <- data.frame(apply(rcp85[,1:5],1,FUN = max),apply(rcp85[,6:10],1,FUN = max),apply(rcp85[,11:15],1,FUN = max),apply(rcp85[,16:20],1,FUN = max),apply(rcp85[,21:25],1,FUN = max),apply(rcp85[,26:30],1,FUN = max))

cities <- c("Indianapolis", "Columbus", "Chicago", "Cleveland", "Madison", "Minneapolis")

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','indyMean','cbusMean','chiMean','clevMean','madMean','minnMean',
                     'indyMin','cbusMin','chiMin','clevMin','madMin','minnMin',
                     'indyMax','cbusMax','chiMax','clevMax','madMax','minnMax')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.1) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.1) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.1) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.1) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.1) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.1) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Predicted Electricity Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 6.0
ens <- scale(MAelecpredENS60)

rcp60 <- scale(MAelecpredMOD60)

means <- data.frame(rowMeans(rcp60[,1:5]),rowMeans(rcp60[,6:10]),rowMeans(rcp60[,11:15]),rowMeans(rcp60[,16:20]),rowMeans(rcp60[,21:25]),rowMeans(rcp60[,26:30]))

mins <- data.frame(apply(rcp60[,1:5],1,FUN = min),apply(rcp60[,6:10],1,FUN = min),apply(rcp60[,11:15],1,FUN = min),apply(rcp60[,16:20],1,FUN = min),apply(rcp60[,21:25],1,FUN = min),apply(rcp60[,26:30],1,FUN = min))
maxs <- data.frame(apply(rcp60[,1:5],1,FUN = max),apply(rcp60[,6:10],1,FUN = max),apply(rcp60[,11:15],1,FUN = max),apply(rcp60[,16:20],1,FUN = max),apply(rcp60[,21:25],1,FUN = max),apply(rcp60[,26:30],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','indyMean','cbusMean','chiMean','clevMean','madMean','minnMean',
                     'indyMin','cbusMin','chiMin','clevMin','madMin','minnMin',
                     'indyMax','cbusMax','chiMax','clevMax','madMax','minnMax')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.1) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.1) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.1) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.1) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.1) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.1) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Predicted Electricity Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 4.5
ens <- scale(MAelecpredENS45)

rcp45 <- scale(MAelecpredMOD45)

means <- data.frame(rowMeans(rcp45[,1:5]),rowMeans(rcp45[,6:10]),rowMeans(rcp45[,11:15]),rowMeans(rcp45[,16:20]),rowMeans(rcp45[,21:25]),rowMeans(rcp45[,26:30]))

mins <- data.frame(apply(rcp45[,1:5],1,FUN = min),apply(rcp45[,6:10],1,FUN = min),apply(rcp45[,11:15],1,FUN = min),apply(rcp45[,16:20],1,FUN = min),apply(rcp45[,21:25],1,FUN = min),apply(rcp45[,26:30],1,FUN = min))
maxs <- data.frame(apply(rcp45[,1:5],1,FUN = max),apply(rcp45[,6:10],1,FUN = max),apply(rcp45[,11:15],1,FUN = max),apply(rcp45[,16:20],1,FUN = max),apply(rcp45[,21:25],1,FUN = max),apply(rcp45[,26:30],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','indyMean','cbusMean','chiMean','clevMean','madMean','minnMean',
                     'indyMin','cbusMin','chiMin','clevMin','madMin','minnMin',
                     'indyMax','cbusMax','chiMax','clevMax','madMax','minnMax')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.1) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.1) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.1) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.1) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.1) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.1) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Predicted Electricity Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 2.6
ens <- scale(MAelecpredENS26)

rcp26 <- scale(MAelecpredMOD26)

means <- data.frame(rowMeans(rcp26[,1:5]),rowMeans(rcp26[,6:10]),rowMeans(rcp26[,11:15]),rowMeans(rcp26[,16:20]),rowMeans(rcp26[,21:25]),rowMeans(rcp26[,26:30]))

mins <- data.frame(apply(rcp26[,1:5],1,FUN = min),apply(rcp26[,6:10],1,FUN = min),apply(rcp26[,11:15],1,FUN = min),apply(rcp26[,16:20],1,FUN = min),apply(rcp26[,21:25],1,FUN = min),apply(rcp26[,26:30],1,FUN = min))
maxs <- data.frame(apply(rcp26[,1:5],1,FUN = max),apply(rcp26[,6:10],1,FUN = max),apply(rcp26[,11:15],1,FUN = max),apply(rcp26[,16:20],1,FUN = max),apply(rcp26[,21:25],1,FUN = max),apply(rcp26[,26:30],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','indyMean','cbusMean','chiMean','clevMean','madMean','minnMean',
                     'indyMin','cbusMin','chiMin','clevMin','madMin','minnMin',
                     'indyMax','cbusMax','chiMax','clevMax','madMax','minnMax')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.1) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.1) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.1) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.1) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.1) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.1) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Predicted Electricity Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# by city

# Indianapolis water
ens <- scale(data.frame(MAwaterpredENS85[,1],MAwaterpredENS60[,1],MAwaterpredENS45[,1],MAwaterpredENS26[,1]))

water <- scale(data.frame(MAwaterpredMOD85[,1:5],MAwaterpredMOD60[,1:5],MAwaterpredMOD45[,1:5],MAwaterpredMOD26[,1:5]))

mins <- data.frame(apply(water[,1:5],1,FUN = min),apply(water[,6:10],1,FUN = min),apply(water[,11:15],1,FUN = min),apply(water[,16:20],1,FUN = min))
maxs <- data.frame(apply(water[,1:5],1,FUN = max),apply(water[,6:10],1,FUN = max),apply(water[,11:15],1,FUN = max),apply(water[,16:20],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.1) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.1) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.1) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.1) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Predicted Water Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Columbus water
ens <- scale(data.frame(MAwaterpredENS85[,2],MAwaterpredENS60[,2],MAwaterpredENS45[,2],MAwaterpredENS26[,2]))

water <- scale(data.frame(MAwaterpredMOD85[,6:10],MAwaterpredMOD60[,6:10],MAwaterpredMOD45[,6:10],MAwaterpredMOD26[,6:10]))

mins <- data.frame(apply(water[,1:5],1,FUN = min),apply(water[,6:10],1,FUN = min),apply(water[,11:15],1,FUN = min),apply(water[,16:20],1,FUN = min))
maxs <- data.frame(apply(water[,1:5],1,FUN = max),apply(water[,6:10],1,FUN = max),apply(water[,11:15],1,FUN = max),apply(water[,16:20],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.1) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.1) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.1) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.1) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Predicted Water Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Chicago water
ens <- scale(data.frame(MAwaterpredENS85[,3],MAwaterpredENS60[,3],MAwaterpredENS45[,3],MAwaterpredENS26[,3]))

water <- scale(data.frame(MAwaterpredMOD85[,11:15],MAwaterpredMOD60[,11:15],MAwaterpredMOD45[,11:15],MAwaterpredMOD26[,11:15]))

mins <- data.frame(apply(water[,1:5],1,FUN = min),apply(water[,6:10],1,FUN = min),apply(water[,11:15],1,FUN = min),apply(water[,16:20],1,FUN = min))
maxs <- data.frame(apply(water[,1:5],1,FUN = max),apply(water[,6:10],1,FUN = max),apply(water[,11:15],1,FUN = max),apply(water[,16:20],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.1) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.1) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.1) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.1) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Predicted Water Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Cleveland water
ens <- scale(data.frame(MAwaterpredENS85[,4],MAwaterpredENS60[,4],MAwaterpredENS45[,4],MAwaterpredENS26[,4]))

water <- scale(data.frame(MAwaterpredMOD85[,16:20],MAwaterpredMOD60[,16:20],MAwaterpredMOD45[,16:20],MAwaterpredMOD26[,16:20]))

mins <- data.frame(apply(water[,1:5],1,FUN = min),apply(water[,6:10],1,FUN = min),apply(water[,11:15],1,FUN = min),apply(water[,16:20],1,FUN = min))
maxs <- data.frame(apply(water[,1:5],1,FUN = max),apply(water[,6:10],1,FUN = max),apply(water[,11:15],1,FUN = max),apply(water[,16:20],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.1) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.1) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.1) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.1) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Predicted Water Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Madison water
ens <- scale(data.frame(MAwaterpredENS85[,5],MAwaterpredENS60[,5],MAwaterpredENS45[,5],MAwaterpredENS26[,5]))

water <- scale(data.frame(MAwaterpredMOD85[,21:25],MAwaterpredMOD60[,21:25],MAwaterpredMOD45[,21:25],MAwaterpredMOD26[,21:25]))

mins <- data.frame(apply(water[,1:5],1,FUN = min),apply(water[,6:10],1,FUN = min),apply(water[,11:15],1,FUN = min),apply(water[,16:20],1,FUN = min))
maxs <- data.frame(apply(water[,1:5],1,FUN = max),apply(water[,6:10],1,FUN = max),apply(water[,11:15],1,FUN = max),apply(water[,16:20],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.1) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.1) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.1) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.1) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Predicted Water Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Minneapolis water
ens <- scale(data.frame(MAwaterpredENS85[,6],MAwaterpredENS60[,6],MAwaterpredENS45[,6],MAwaterpredENS26[,6]))

water <- scale(data.frame(MAwaterpredMOD85[,25:30],MAwaterpredMOD60[,25:30],MAwaterpredMOD45[,25:30],MAwaterpredMOD26[,25:30]))

mins <- data.frame(apply(water[,1:5],1,FUN = min),apply(water[,6:10],1,FUN = min),apply(water[,11:15],1,FUN = min),apply(water[,16:20],1,FUN = min))
maxs <- data.frame(apply(water[,1:5],1,FUN = max),apply(water[,6:10],1,FUN = max),apply(water[,11:15],1,FUN = max),apply(water[,16:20],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.1) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.1) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.1) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.1) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Predicted Water Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Indianapolis electricity
ens <- scale(data.frame(MAelecpredENS85[,1],MAelecpredENS60[,1],MAelecpredENS45[,1],MAelecpredENS26[,1]))

elec <- scale(data.frame(MAelecpredMOD85[,1:5],MAelecpredMOD60[,1:5],MAelecpredMOD45[,1:5],MAelecpredMOD26[,1:5]))

mins <- data.frame(apply(elec[,1:5],1,FUN = min),apply(elec[,6:10],1,FUN = min),apply(elec[,11:15],1,FUN = min),apply(elec[,16:20],1,FUN = min))
maxs <- data.frame(apply(elec[,1:5],1,FUN = max),apply(elec[,6:10],1,FUN = max),apply(elec[,11:15],1,FUN = max),apply(elec[,16:20],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.1) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.1) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.1) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.1) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Predicted Electricity Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Columbus electricity
ens <- scale(data.frame(MAelecpredENS85[,2],MAelecpredENS60[,2],MAelecpredENS45[,2],MAelecpredENS26[,2]))

elec <- scale(data.frame(MAelecpredMOD85[,6:10],MAelecpredMOD60[,6:10],MAelecpredMOD45[,6:10],MAelecpredMOD26[,6:10]))

mins <- data.frame(apply(elec[,1:5],1,FUN = min),apply(elec[,6:10],1,FUN = min),apply(elec[,11:15],1,FUN = min),apply(elec[,16:20],1,FUN = min))
maxs <- data.frame(apply(elec[,1:5],1,FUN = max),apply(elec[,6:10],1,FUN = max),apply(elec[,11:15],1,FUN = max),apply(elec[,16:20],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.1) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.1) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.1) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.1) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Predicted Electricity Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Chicago electricity
ens <- scale(data.frame(MAelecpredENS85[,3],MAelecpredENS60[,3],MAelecpredENS45[,3],MAelecpredENS26[,3]))

elec <- scale(data.frame(MAelecpredMOD85[,11:15],MAelecpredMOD60[,11:15],MAelecpredMOD45[,11:15],MAelecpredMOD26[,11:15]))

mins <- data.frame(apply(elec[,1:5],1,FUN = min),apply(elec[,6:10],1,FUN = min),apply(elec[,11:15],1,FUN = min),apply(elec[,16:20],1,FUN = min))
maxs <- data.frame(apply(elec[,1:5],1,FUN = max),apply(elec[,6:10],1,FUN = max),apply(elec[,11:15],1,FUN = max),apply(elec[,16:20],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.1) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.1) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.1) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.1) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Predicted Electricity Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Cleveland electricity
ens <- scale(data.frame(MAelecpredENS85[,4],MAelecpredENS60[,4],MAelecpredENS45[,4],MAelecpredENS26[,4]))

elec <- scale(data.frame(MAelecpredMOD85[,16:20],MAelecpredMOD60[,16:20],MAelecpredMOD45[,16:20],MAelecpredMOD26[,16:20]))

mins <- data.frame(apply(elec[,1:5],1,FUN = min),apply(elec[,6:10],1,FUN = min),apply(elec[,11:15],1,FUN = min),apply(elec[,16:20],1,FUN = min))
maxs <- data.frame(apply(elec[,1:5],1,FUN = max),apply(elec[,6:10],1,FUN = max),apply(elec[,11:15],1,FUN = max),apply(elec[,16:20],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.1) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.1) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.1) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.1) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Predicted Electricity Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Madison electricity
ens <- scale(data.frame(MAelecpredENS85[,5],MAelecpredENS60[,5],MAelecpredENS45[,5],MAelecpredENS26[,5]))

elec <- scale(data.frame(MAelecpredMOD85[,21:25],MAelecpredMOD60[,21:25],MAelecpredMOD45[,21:25],MAelecpredMOD26[,21:25]))

mins <- data.frame(apply(elec[,1:5],1,FUN = min),apply(elec[,6:10],1,FUN = min),apply(elec[,11:15],1,FUN = min),apply(elec[,16:20],1,FUN = min))
maxs <- data.frame(apply(elec[,1:5],1,FUN = max),apply(elec[,6:10],1,FUN = max),apply(elec[,11:15],1,FUN = max),apply(elec[,16:20],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.1) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.1) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.1) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.1) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Predicted Electricity Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Minneapolis electricity
ens <- scale(data.frame(MAelecpredENS85[,6],MAelecpredENS60[,6],MAelecpredENS45[,6],MAelecpredENS26[,6]))

elec <- scale(data.frame(MAelecpredMOD85[,26:30],MAelecpredMOD60[,26:30],MAelecpredMOD45[,26:30],MAelecpredMOD26[,26:30]))

mins <- data.frame(apply(elec[,1:5],1,FUN = min),apply(elec[,6:10],1,FUN = min),apply(elec[,11:15],1,FUN = min),apply(elec[,16:20],1,FUN = min))
maxs <- data.frame(apply(elec[,1:5],1,FUN = max),apply(elec[,6:10],1,FUN = max),apply(elec[,11:15],1,FUN = max),apply(elec[,16:20],1,FUN = max))

preddata <- data.frame(date,ens,mins,maxs)
names(preddata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(preddata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.1) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.1) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.1) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.1) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Predicted Electricity Use (scaled)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 



# variable change in each scenario


# ENSO (Same for each city)

# RCP 8.5

enso85 <- data.frame(MAchi85[[1]][[1]],MAchi85[[2]][[1]],MAchi85[[3]][[1]],MAchi85[[4]][[1]],MAchi85[[5]][[1]])

means <- rowMeans(enso85)

mins <- apply(enso85,1,FUN = min)

maxs <- apply(enso85,1,FUN = max)

vardata <- data.frame(date,means,mins,maxs)

ggplot(vardata, aes(x=date, y=means)) + 
  geom_ribbon(aes(x=date, ymin=mins, ymax=maxs),alpha=0.3) + geom_line(color='red') +
  xlab('Date') + ylab(expression('ENSO Index')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm"),legend.position=c(0.9,0.9))

# RCP 6.0

enso60 <- data.frame(MAchi60[[1]][[1]],MAchi60[[2]][[1]],MAchi60[[3]][[1]],MAchi60[[4]][[1]],MAchi60[[5]][[1]])

means <- rowMeans(enso60)

mins <- apply(enso60,1,FUN = min)

maxs <- apply(enso60,1,FUN = max)

vardata <- data.frame(date,means,mins,maxs)

ggplot(vardata, aes(x=date, y=means)) + 
  geom_ribbon(aes(x=date, ymin=mins, ymax=maxs),alpha=0.3) + geom_line(color='red') +
  xlab('Date') + ylab(expression('ENSO Index')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm"),legend.position=c(0.9,0.9))

# RCP 4.5

enso45 <- data.frame(MAchi45[[1]][[1]],MAchi45[[2]][[1]],MAchi45[[3]][[1]],MAchi45[[4]][[1]],MAchi45[[5]][[1]])

means <- rowMeans(enso45)

mins <- apply(enso45,1,FUN = min)

maxs <- apply(enso45,1,FUN = max)

vardata <- data.frame(date,means,mins,maxs)

ggplot(vardata, aes(x=date, y=means)) + 
  geom_ribbon(aes(x=date, ymin=mins, ymax=maxs),alpha=0.3) + geom_line(color='red') +
  xlab('Date') + ylab(expression('ENSO Index')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm"),legend.position=c(0.9,0.9))

# RCP 2.6

enso26 <- data.frame(MAchi26[[1]][[1]],MAchi26[[2]][[1]],MAchi26[[3]][[1]],MAchi26[[4]][[1]],MAchi26[[5]][[1]])

means <- rowMeans(enso26)

mins <- apply(enso26,1,FUN = min)

maxs <- apply(enso26,1,FUN = max)

vardata <- data.frame(date,means,mins,maxs)

ggplot(vardata, aes(x=date, y=means)) + 
  geom_ribbon(aes(x=date, ymin=mins, ymax=maxs),alpha=0.3) + geom_line(color='red') +
  xlab('Date') + ylab(expression('ENSO Index')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm"),legend.position=c(0.9,0.9))


# DewPoint

# RCP 8.5

dp85 <- data.frame(MAchi85[[1]][[2]],MAchi85[[2]][[2]],MAchi85[[3]][[2]],MAchi85[[4]][[2]],MAchi85[[5]][[2]],
                   MAclev85[[1]][[2]],MAclev85[[2]][[2]],MAclev85[[3]][[2]],MAclev85[[4]][[2]],MAclev85[[5]][[2]],
                   MAcbus85[[1]][[2]],MAcbus85[[2]][[2]],MAcbus85[[3]][[2]],MAcbus85[[4]][[2]],MAcbus85[[5]][[2]],
                   MAindy85[[1]][[2]],MAindy85[[2]][[2]],MAindy85[[3]][[2]],MAindy85[[4]][[2]],MAindy85[[5]][[2]],
                   MAmad85[[1]][[2]],MAmad85[[2]][[2]],MAmad85[[3]][[2]],MAmad85[[4]][[2]],MAmad85[[5]][[2]],
                   MAminn85[[1]][[2]],MAminn85[[2]][[2]],MAminn85[[3]][[2]],MAminn85[[4]][[2]],MAminn85[[5]][[2]])
                   
means <- data.frame(rowMeans(dp85[,1:5]),rowMeans(dp85[,6:10]),rowMeans(dp85[,11:15]),rowMeans(dp85[,16:20]),rowMeans(dp85[,21:25]),rowMeans(dp85[,26:30]))

mins <- data.frame(apply(dp85[,1:5],1,FUN = min),apply(dp85[,6:10],1,FUN = min),apply(dp85[,11:15],1,FUN = min),apply(dp85[,16:20],1,FUN = min),apply(dp85[,21:25],1,FUN = min),apply(dp85[,26:30],1,FUN = min))

maxs <- data.frame(apply(dp85[,1:5],1,FUN = max),apply(dp85[,6:10],1,FUN = max),apply(dp85[,11:15],1,FUN = max),apply(dp85[,16:20],1,FUN = max),apply(dp85[,21:25],1,FUN = max),apply(dp85[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.3) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.3) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.3) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.3) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.3) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.3) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Dew Point Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 6.0

dp60 <- data.frame(MAchi60[[1]][[2]],MAchi60[[2]][[2]],MAchi60[[3]][[2]],MAchi60[[4]][[2]],MAchi60[[5]][[2]],
                   MAclev60[[1]][[2]],MAclev60[[2]][[2]],MAclev60[[3]][[2]],MAclev60[[4]][[2]],MAclev60[[5]][[2]],
                   MAcbus60[[1]][[2]],MAcbus60[[2]][[2]],MAcbus60[[3]][[2]],MAcbus60[[4]][[2]],MAcbus60[[5]][[2]],
                   MAindy60[[1]][[2]],MAindy60[[2]][[2]],MAindy60[[3]][[2]],MAindy60[[4]][[2]],MAindy60[[5]][[2]],
                   MAmad60[[1]][[2]],MAmad60[[2]][[2]],MAmad60[[3]][[2]],MAmad60[[4]][[2]],MAmad60[[5]][[2]],
                   MAminn60[[1]][[2]],MAminn60[[2]][[2]],MAminn60[[3]][[2]],MAminn60[[4]][[2]],MAminn60[[5]][[2]])

means <- data.frame(rowMeans(dp60[,1:5]),rowMeans(dp60[,6:10]),rowMeans(dp60[,11:15]),rowMeans(dp60[,16:20]),rowMeans(dp60[,21:25]),rowMeans(dp60[,26:30]))

mins <- data.frame(apply(dp60[,1:5],1,FUN = min),apply(dp60[,6:10],1,FUN = min),apply(dp60[,11:15],1,FUN = min),apply(dp60[,16:20],1,FUN = min),apply(dp60[,21:25],1,FUN = min),apply(dp60[,26:30],1,FUN = min))

maxs <- data.frame(apply(dp60[,1:5],1,FUN = max),apply(dp60[,6:10],1,FUN = max),apply(dp60[,11:15],1,FUN = max),apply(dp60[,16:20],1,FUN = max),apply(dp60[,21:25],1,FUN = max),apply(dp60[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.3) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.3) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.3) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.3) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.3) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.3) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Dew Point Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 4.5

dp45 <- data.frame(MAchi45[[1]][[2]],MAchi45[[2]][[2]],MAchi45[[3]][[2]],MAchi45[[4]][[2]],MAchi45[[5]][[2]],
                   MAclev45[[1]][[2]],MAclev45[[2]][[2]],MAclev45[[3]][[2]],MAclev45[[4]][[2]],MAclev45[[5]][[2]],
                   MAcbus45[[1]][[2]],MAcbus45[[2]][[2]],MAcbus45[[3]][[2]],MAcbus45[[4]][[2]],MAcbus45[[5]][[2]],
                   MAindy45[[1]][[2]],MAindy45[[2]][[2]],MAindy45[[3]][[2]],MAindy45[[4]][[2]],MAindy45[[5]][[2]],
                   MAmad45[[1]][[2]],MAmad45[[2]][[2]],MAmad45[[3]][[2]],MAmad45[[4]][[2]],MAmad45[[5]][[2]],
                   MAminn45[[1]][[2]],MAminn45[[2]][[2]],MAminn45[[3]][[2]],MAminn45[[4]][[2]],MAminn45[[5]][[2]])

means <- data.frame(rowMeans(dp45[,1:5]),rowMeans(dp45[,6:10]),rowMeans(dp45[,11:15]),rowMeans(dp45[,16:20]),rowMeans(dp45[,21:25]),rowMeans(dp45[,26:30]))

mins <- data.frame(apply(dp45[,1:5],1,FUN = min),apply(dp45[,6:10],1,FUN = min),apply(dp45[,11:15],1,FUN = min),apply(dp45[,16:20],1,FUN = min),apply(dp45[,21:25],1,FUN = min),apply(dp45[,26:30],1,FUN = min))
maxs <- data.frame(apply(dp45[,1:5],1,FUN = max),apply(dp45[,6:10],1,FUN = max),apply(dp45[,11:15],1,FUN = max),apply(dp45[,16:20],1,FUN = max),apply(dp45[,21:25],1,FUN = max),apply(dp45[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.3) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.3) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.3) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.3) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.3) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.3) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Dew Point Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 2.6

dp26 <- data.frame(MAchi26[[1]][[2]],MAchi26[[2]][[2]],MAchi26[[3]][[2]],MAchi26[[4]][[2]],MAchi26[[5]][[2]],
                   MAclev26[[1]][[2]],MAclev26[[2]][[2]],MAclev26[[3]][[2]],MAclev26[[4]][[2]],MAclev26[[5]][[2]],
                   MAcbus26[[1]][[2]],MAcbus26[[2]][[2]],MAcbus26[[3]][[2]],MAcbus26[[4]][[2]],MAcbus26[[5]][[2]],
                   MAindy26[[1]][[2]],MAindy26[[2]][[2]],MAindy26[[3]][[2]],MAindy26[[4]][[2]],MAindy26[[5]][[2]],
                   MAmad26[[1]][[2]],MAmad26[[2]][[2]],MAmad26[[3]][[2]],MAmad26[[4]][[2]],MAmad26[[5]][[2]],
                   MAminn26[[1]][[2]],MAminn26[[2]][[2]],MAminn26[[3]][[2]],MAminn26[[4]][[2]],MAminn26[[5]][[2]])

means <- data.frame(rowMeans(dp26[,1:5]),rowMeans(dp26[,6:10]),rowMeans(dp26[,11:15]),rowMeans(dp26[,16:20]),rowMeans(dp26[,21:25]),rowMeans(dp26[,26:30]))

mins <- data.frame(apply(dp26[,1:5],1,FUN = min),apply(dp26[,6:10],1,FUN = min),apply(dp26[,11:15],1,FUN = min),apply(dp26[,16:20],1,FUN = min),apply(dp26[,21:25],1,FUN = min),apply(dp26[,26:30],1,FUN = min))
maxs <- data.frame(apply(dp26[,1:5],1,FUN = max),apply(dp26[,6:10],1,FUN = max),apply(dp26[,11:15],1,FUN = max),apply(dp26[,16:20],1,FUN = max),apply(dp26[,21:25],1,FUN = max),apply(dp26[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.3) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.3) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.3) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.3) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.3) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.3) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Dew Point Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# Relative Humidity

# RCP 8.5

rh85 <- data.frame(MAchi85[[1]][[3]],MAchi85[[2]][[3]],MAchi85[[3]][[3]],MAchi85[[4]][[3]],MAchi85[[5]][[3]],
                  MAclev85[[1]][[3]],MAclev85[[2]][[3]],MAclev85[[3]][[3]],MAclev85[[4]][[3]],MAclev85[[5]][[3]],
                  MAcbus85[[1]][[3]],MAcbus85[[2]][[3]],MAcbus85[[3]][[3]],MAcbus85[[4]][[3]],MAcbus85[[5]][[3]],
                  MAindy85[[1]][[3]],MAindy85[[2]][[3]],MAindy85[[3]][[3]],MAindy85[[4]][[3]],MAindy85[[5]][[3]],
                  MAmad85[[1]][[3]],MAmad85[[2]][[3]],MAmad85[[3]][[3]],MAmad85[[4]][[3]],MAmad85[[5]][[3]],
                  MAminn85[[1]][[3]],MAminn85[[2]][[3]],MAminn85[[3]][[3]],MAminn85[[4]][[3]],MAminn85[[5]][[3]])

means <- data.frame(rowMeans(rh85[,1:5]),rowMeans(rh85[,6:10]),rowMeans(rh85[,11:15]),rowMeans(rh85[,16:20]),rowMeans(rh85[,21:25]),rowMeans(rh85[,26:30]))

mins <- data.frame(apply(rh85[,1:5],1,FUN = min),apply(rh85[,6:10],1,FUN = min),apply(rh85[,11:15],1,FUN = min),apply(rh85[,16:20],1,FUN = min),apply(rh85[,21:25],1,FUN = min),apply(rh85[,26:30],1,FUN = min))
maxs <- data.frame(apply(rh85[,1:5],1,FUN = max),apply(rh85[,6:10],1,FUN = max),apply(rh85[,11:15],1,FUN = max),apply(rh85[,16:20],1,FUN = max),apply(rh85[,21:25],1,FUN = max),apply(rh85[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.1) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.1) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.1) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.1) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.1) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.1) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Relative Humidity (%)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 6.0

rh60 <- data.frame(MAchi60[[1]][[3]],MAchi60[[2]][[3]],MAchi60[[3]][[3]],MAchi60[[4]][[3]],MAchi60[[5]][[3]],
                   MAclev60[[1]][[3]],MAclev60[[2]][[3]],MAclev60[[3]][[3]],MAclev60[[4]][[3]],MAclev60[[5]][[3]],
                   MAcbus60[[1]][[3]],MAcbus60[[2]][[3]],MAcbus60[[3]][[3]],MAcbus60[[4]][[3]],MAcbus60[[5]][[3]],
                   MAindy60[[1]][[3]],MAindy60[[2]][[3]],MAindy60[[3]][[3]],MAindy60[[4]][[3]],MAindy60[[5]][[3]],
                   MAmad60[[1]][[3]],MAmad60[[2]][[3]],MAmad60[[3]][[3]],MAmad60[[4]][[3]],MAmad60[[5]][[3]],
                   MAminn60[[1]][[3]],MAminn60[[2]][[3]],MAminn60[[3]][[3]],MAminn60[[4]][[3]],MAminn60[[5]][[3]])

means <- data.frame(rowMeans(rh60[,1:5]),rowMeans(rh60[,6:10]),rowMeans(rh60[,11:15]),rowMeans(rh60[,16:20]),rowMeans(rh60[,21:25]),rowMeans(rh60[,26:30]))

mins <- data.frame(apply(rh60[,1:5],1,FUN = min),apply(rh60[,6:10],1,FUN = min),apply(rh60[,11:15],1,FUN = min),apply(rh60[,16:20],1,FUN = min),apply(rh60[,21:25],1,FUN = min),apply(rh60[,26:30],1,FUN = min))
maxs <- data.frame(apply(rh60[,1:5],1,FUN = max),apply(rh60[,6:10],1,FUN = max),apply(rh60[,11:15],1,FUN = max),apply(rh60[,16:20],1,FUN = max),apply(rh60[,21:25],1,FUN = max),apply(rh60[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.1) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.1) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.1) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.1) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.1) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.1) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Relative Humidity (%)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 4.5

rh45 <- data.frame(MAchi45[[1]][[3]],MAchi45[[2]][[3]],MAchi45[[3]][[3]],MAchi45[[4]][[3]],MAchi45[[5]][[3]],
                   MAclev45[[1]][[3]],MAclev45[[2]][[3]],MAclev45[[3]][[3]],MAclev45[[4]][[3]],MAclev45[[5]][[3]],
                   MAcbus45[[1]][[3]],MAcbus45[[2]][[3]],MAcbus45[[3]][[3]],MAcbus45[[4]][[3]],MAcbus45[[5]][[3]],
                   MAindy45[[1]][[3]],MAindy45[[2]][[3]],MAindy45[[3]][[3]],MAindy45[[4]][[3]],MAindy45[[5]][[3]],
                   MAmad45[[1]][[3]],MAmad45[[2]][[3]],MAmad45[[3]][[3]],MAmad45[[4]][[3]],MAmad45[[5]][[3]],
                   MAminn45[[1]][[3]],MAminn45[[2]][[3]],MAminn45[[3]][[3]],MAminn45[[4]][[3]],MAminn45[[5]][[3]])

means <- data.frame(rowMeans(rh45[,1:5]),rowMeans(rh45[,6:10]),rowMeans(rh45[,11:15]),rowMeans(rh45[,16:20]),rowMeans(rh45[,21:25]),rowMeans(rh45[,26:30]))

mins <- data.frame(apply(rh45[,1:5],1,FUN = min),apply(rh45[,6:10],1,FUN = min),apply(rh45[,11:15],1,FUN = min),apply(rh45[,16:20],1,FUN = min),apply(rh45[,21:25],1,FUN = min),apply(rh45[,26:30],1,FUN = min))
maxs <- data.frame(apply(rh45[,1:5],1,FUN = max),apply(rh45[,6:10],1,FUN = max),apply(rh45[,11:15],1,FUN = max),apply(rh45[,16:20],1,FUN = max),apply(rh45[,21:25],1,FUN = max),apply(rh45[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.1) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.1) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.1) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.1) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.1) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.1) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Relative Humidity (%)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 2.6
rh26 <- data.frame(MAchi26[[1]][[3]],MAchi26[[2]][[3]],MAchi26[[3]][[3]],MAchi26[[4]][[3]],MAchi26[[5]][[3]],
                   MAclev26[[1]][[3]],MAclev26[[2]][[3]],MAclev26[[3]][[3]],MAclev26[[4]][[3]],MAclev26[[5]][[3]],
                   MAcbus26[[1]][[3]],MAcbus26[[2]][[3]],MAcbus26[[3]][[3]],MAcbus26[[4]][[3]],MAcbus26[[5]][[3]],
                   MAindy26[[1]][[3]],MAindy26[[2]][[3]],MAindy26[[3]][[3]],MAindy26[[4]][[3]],MAindy26[[5]][[3]],
                   MAmad26[[1]][[3]],MAmad26[[2]][[3]],MAmad26[[3]][[3]],MAmad26[[4]][[3]],MAmad26[[5]][[3]],
                   MAminn26[[1]][[3]],MAminn26[[2]][[3]],MAminn26[[3]][[3]],MAminn26[[4]][[3]],MAminn26[[5]][[3]])

means <- data.frame(rowMeans(rh26[,1:5]),rowMeans(rh26[,6:10]),rowMeans(rh26[,11:15]),rowMeans(rh26[,16:20]),rowMeans(rh26[,21:25]),rowMeans(rh26[,26:30]))

mins <- data.frame(apply(rh26[,1:5],1,FUN = min),apply(rh26[,6:10],1,FUN = min),apply(rh26[,11:15],1,FUN = min),apply(rh26[,16:20],1,FUN = min),apply(rh26[,21:25],1,FUN = min),apply(rh26[,26:30],1,FUN = min))
maxs <- data.frame(apply(rh26[,1:5],1,FUN = max),apply(rh26[,6:10],1,FUN = max),apply(rh26[,11:15],1,FUN = max),apply(rh26[,16:20],1,FUN = max),apply(rh26[,21:25],1,FUN = max),apply(rh26[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.1) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.1) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.1) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.1) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.1) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.1) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Relative Humidity (%)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 


# Dry Bulb Temperature

# RCP 8.5

dbt85 <- data.frame(MAchi85[[1]][[4]],MAchi85[[2]][[4]],MAchi85[[3]][[4]],MAchi85[[4]][[4]],MAchi85[[5]][[4]],
                    MAclev85[[1]][[4]],MAclev85[[2]][[4]],MAclev85[[3]][[4]],MAclev85[[4]][[4]],MAclev85[[5]][[4]],
                    MAcbus85[[1]][[4]],MAcbus85[[2]][[4]],MAcbus85[[3]][[4]],MAcbus85[[4]][[4]],MAcbus85[[5]][[4]],
                    MAindy85[[1]][[4]],MAindy85[[2]][[4]],MAindy85[[3]][[4]],MAindy85[[4]][[4]],MAindy85[[5]][[4]],
                    MAmad85[[1]][[4]],MAmad85[[2]][[4]],MAmad85[[3]][[4]],MAmad85[[4]][[4]],MAmad85[[5]][[4]],
                    MAminn85[[1]][[4]],MAminn85[[2]][[4]],MAminn85[[3]][[4]],MAminn85[[4]][[4]],MAminn85[[5]][[4]])

means <- data.frame(rowMeans(dbt85[,1:5]),rowMeans(dbt85[,6:10]),rowMeans(dbt85[,11:15]),rowMeans(dbt85[,16:20]),rowMeans(dbt85[,21:25]),rowMeans(dbt85[,26:30]))

mins <- data.frame(apply(dbt85[,1:5],1,FUN = min),apply(dbt85[,6:10],1,FUN = min),apply(dbt85[,11:15],1,FUN = min),apply(dbt85[,16:20],1,FUN = min),apply(dbt85[,21:25],1,FUN = min),apply(dbt85[,26:30],1,FUN = min))
maxs <- data.frame(apply(dbt85[,1:5],1,FUN = max),apply(dbt85[,6:10],1,FUN = max),apply(dbt85[,11:15],1,FUN = max),apply(dbt85[,16:20],1,FUN = max),apply(dbt85[,21:25],1,FUN = max),apply(dbt85[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.2) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.2) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.2) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.2) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.2) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.2) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Dry Bulb Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 6.0

dbt60 <- data.frame(MAchi60[[1]][[4]], MAchi60[[2]][[4]], MAchi60[[3]][[4]], MAchi60[[4]][[4]], MAchi60[[5]][[4]],
                    MAclev60[[1]][[4]],MAclev60[[2]][[4]],MAclev60[[3]][[4]],MAclev60[[4]][[4]],MAclev60[[5]][[4]],
                    MAcbus60[[1]][[4]],MAcbus60[[2]][[4]],MAcbus60[[3]][[4]],MAcbus60[[4]][[4]],MAcbus60[[5]][[4]],
                    MAindy60[[1]][[4]],MAindy60[[2]][[4]],MAindy60[[3]][[4]],MAindy60[[4]][[4]],MAindy60[[5]][[4]],
                    MAmad60[[1]][[4]], MAmad60[[2]][[4]], MAmad60[[3]][[4]], MAmad60[[4]][[4]], MAmad60[[5]][[4]],
                    MAminn60[[1]][[4]],MAminn60[[2]][[4]],MAminn60[[3]][[4]],MAminn60[[4]][[4]],MAminn60[[5]][[4]])

means <- data.frame(rowMeans(dbt60[,1:5]),rowMeans(dbt60[,6:10]),rowMeans(dbt60[,11:15]),rowMeans(dbt60[,16:20]),rowMeans(dbt60[,21:25]),rowMeans(dbt60[,26:30]))

mins <- data.frame(apply(dbt60[,1:5],1,FUN = min),apply(dbt60[,6:10],1,FUN = min),apply(dbt60[,11:15],1,FUN = min),apply(dbt60[,16:20],1,FUN = min),apply(dbt60[,21:25],1,FUN = min),apply(dbt60[,26:30],1,FUN = min))
maxs <- data.frame(apply(dbt60[,1:5],1,FUN = max),apply(dbt60[,6:10],1,FUN = max),apply(dbt60[,11:15],1,FUN = max),apply(dbt60[,16:20],1,FUN = max),apply(dbt60[,21:25],1,FUN = max),apply(dbt60[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.2) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.2) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.2) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.2) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.2) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.2) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Dry Bulb Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 4.5

dbt45 <- data.frame(MAchi45[[1]][[4]], MAchi45[[2]][[4]], MAchi45[[3]][[4]], MAchi45[[4]][[4]], MAchi45[[5]][[4]],
                    MAclev45[[1]][[4]],MAclev45[[2]][[4]],MAclev45[[3]][[4]],MAclev45[[4]][[4]],MAclev45[[5]][[4]],
                    MAcbus45[[1]][[4]],MAcbus45[[2]][[4]],MAcbus45[[3]][[4]],MAcbus45[[4]][[4]],MAcbus45[[5]][[4]],
                    MAindy45[[1]][[4]],MAindy45[[2]][[4]],MAindy45[[3]][[4]],MAindy45[[4]][[4]],MAindy45[[5]][[4]],
                    MAmad45[[1]][[4]], MAmad45[[2]][[4]], MAmad45[[3]][[4]], MAmad45[[4]][[4]], MAmad45[[5]][[4]],
                    MAminn45[[1]][[4]],MAminn45[[2]][[4]],MAminn45[[3]][[4]],MAminn45[[4]][[4]],MAminn45[[5]][[4]])

means <- data.frame(rowMeans(dbt45[,1:5]),rowMeans(dbt45[,6:10]),rowMeans(dbt45[,11:15]),rowMeans(dbt45[,16:20]),rowMeans(dbt45[,21:25]),rowMeans(dbt45[,26:30]))

mins <- data.frame(apply(dbt45[,1:5],1,FUN = min),apply(dbt45[,6:10],1,FUN = min),apply(dbt45[,11:15],1,FUN = min),apply(dbt45[,16:20],1,FUN = min),apply(dbt45[,21:25],1,FUN = min),apply(dbt45[,26:30],1,FUN = min))
maxs <- data.frame(apply(dbt45[,1:5],1,FUN = max),apply(dbt45[,6:10],1,FUN = max),apply(dbt45[,11:15],1,FUN = max),apply(dbt45[,16:20],1,FUN = max),apply(dbt45[,21:25],1,FUN = max),apply(dbt45[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.2) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.2) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.2) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.2) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.2) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.2) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Dry Bulb Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 2.6

dbt26 <- data.frame(MAchi26[[1]][[4]], MAchi26[[2]][[4]], MAchi26[[3]][[4]], MAchi26[[4]][[4]], MAchi26[[5]][[4]],
                    MAclev26[[1]][[4]],MAclev26[[2]][[4]],MAclev26[[3]][[4]],MAclev26[[4]][[4]],MAclev26[[5]][[4]],
                    MAcbus26[[1]][[4]],MAcbus26[[2]][[4]],MAcbus26[[3]][[4]],MAcbus26[[4]][[4]],MAcbus26[[5]][[4]],
                    MAindy26[[1]][[4]],MAindy26[[2]][[4]],MAindy26[[3]][[4]],MAindy26[[4]][[4]],MAindy26[[5]][[4]],
                    MAmad26[[1]][[4]], MAmad26[[2]][[4]], MAmad26[[3]][[4]], MAmad26[[4]][[4]], MAmad26[[5]][[4]],
                    MAminn26[[1]][[4]],MAminn26[[2]][[4]],MAminn26[[3]][[4]],MAminn26[[4]][[4]],MAminn26[[5]][[4]])

means <- data.frame(rowMeans(dbt26[,1:5]),rowMeans(dbt26[,6:10]),rowMeans(dbt26[,11:15]),rowMeans(dbt26[,16:20]),rowMeans(dbt26[,21:25]),rowMeans(dbt26[,26:30]))

mins <- data.frame(apply(dbt26[,1:5],1,FUN = min),apply(dbt26[,6:10],1,FUN = min),apply(dbt26[,11:15],1,FUN = min),apply(dbt26[,16:20],1,FUN = min),apply(dbt26[,21:25],1,FUN = min),apply(dbt26[,26:30],1,FUN = min))
maxs <- data.frame(apply(dbt26[,1:5],1,FUN = max),apply(dbt26[,6:10],1,FUN = max),apply(dbt26[,11:15],1,FUN = max),apply(dbt26[,16:20],1,FUN = max),apply(dbt26[,21:25],1,FUN = max),apply(dbt26[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.2) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.2) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.2) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.2) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.2) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.2) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Dry Bulb Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# Wind Speed

# RCP 8.5

ws85 <- data.frame(MAchi85[[1]][[5]], MAchi85[[2]][[5]], MAchi85[[3]][[5]], MAchi85[[4]][[5]], MAchi85[[5]][[5]],
                   MAclev85[[1]][[5]],MAclev85[[2]][[5]],MAclev85[[3]][[5]],MAclev85[[4]][[5]],MAclev85[[5]][[5]],
                   MAcbus85[[1]][[5]],MAcbus85[[2]][[5]],MAcbus85[[3]][[5]],MAcbus85[[4]][[5]],MAcbus85[[5]][[5]],
                   MAindy85[[1]][[5]],MAindy85[[2]][[5]],MAindy85[[3]][[5]],MAindy85[[4]][[5]],MAindy85[[5]][[5]],
                   MAmad85[[1]][[5]], MAmad85[[2]][[5]], MAmad85[[3]][[5]], MAmad85[[4]][[5]], MAmad85[[5]][[5]],
                   MAminn85[[1]][[5]],MAminn85[[2]][[5]],MAminn85[[3]][[5]],MAminn85[[4]][[5]],MAminn85[[5]][[5]])

means <- data.frame(rowMeans(ws85[,1:5]),rowMeans(ws85[,6:10]),rowMeans(ws85[,11:15]),rowMeans(ws85[,16:20]),rowMeans(ws85[,21:25]),rowMeans(ws85[,26:30]))

mins <- data.frame(apply(ws85[,1:5],1,FUN = min),apply(ws85[,6:10],1,FUN = min),apply(ws85[,11:15],1,FUN = min),apply(ws85[,16:20],1,FUN = min),apply(ws85[,21:25],1,FUN = min),apply(ws85[,26:30],1,FUN = min))
maxs <- data.frame(apply(ws85[,1:5],1,FUN = max),apply(ws85[,6:10],1,FUN = max),apply(ws85[,11:15],1,FUN = max),apply(ws85[,16:20],1,FUN = max),apply(ws85[,21:25],1,FUN = max),apply(ws85[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.2) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.2) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.2) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.2) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.2) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.2) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Wind Speed (m/s)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 6.0

ws60 <- data.frame(MAchi60[[1]][[5]], MAchi60[[2]][[5]], MAchi60[[3]][[5]], MAchi60[[4]][[5]], MAchi60[[5]][[5]],
                   MAclev60[[1]][[5]],MAclev60[[2]][[5]],MAclev60[[3]][[5]],MAclev60[[4]][[5]],MAclev60[[5]][[5]],
                   MAcbus60[[1]][[5]],MAcbus60[[2]][[5]],MAcbus60[[3]][[5]],MAcbus60[[4]][[5]],MAcbus60[[5]][[5]],
                   MAindy60[[1]][[5]],MAindy60[[2]][[5]],MAindy60[[3]][[5]],MAindy60[[4]][[5]],MAindy60[[5]][[5]],
                   MAmad60[[1]][[5]], MAmad60[[2]][[5]], MAmad60[[3]][[5]], MAmad60[[4]][[5]], MAmad60[[5]][[5]],
                   MAminn60[[1]][[5]],MAminn60[[2]][[5]],MAminn60[[3]][[5]],MAminn60[[4]][[5]],MAminn60[[5]][[5]])

means <- data.frame(rowMeans(ws60[,1:5]),rowMeans(ws60[,6:10]),rowMeans(ws60[,11:15]),rowMeans(ws60[,16:20]),rowMeans(ws60[,21:25]),rowMeans(ws60[,26:30]))

mins <- data.frame(apply(ws60[,1:5],1,FUN = min),apply(ws60[,6:10],1,FUN = min),apply(ws60[,11:15],1,FUN = min),apply(ws60[,16:20],1,FUN = min),apply(ws60[,21:25],1,FUN = min),apply(ws60[,26:30],1,FUN = min))
maxs <- data.frame(apply(ws60[,1:5],1,FUN = max),apply(ws60[,6:10],1,FUN = max),apply(ws60[,11:15],1,FUN = max),apply(ws60[,16:20],1,FUN = max),apply(ws60[,21:25],1,FUN = max),apply(ws60[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.2) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.2) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.2) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.2) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.2) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.2) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Wind Speed (m/s)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 4.5

ws45 <- data.frame(MAchi45[[1]][[5]], MAchi45[[2]][[5]], MAchi45[[3]][[5]], MAchi45[[4]][[5]], MAchi45[[5]][[5]],
                   MAclev45[[1]][[5]],MAclev45[[2]][[5]],MAclev45[[3]][[5]],MAclev45[[4]][[5]],MAclev45[[5]][[5]],
                   MAcbus45[[1]][[5]],MAcbus45[[2]][[5]],MAcbus45[[3]][[5]],MAcbus45[[4]][[5]],MAcbus45[[5]][[5]],
                   MAindy45[[1]][[5]],MAindy45[[2]][[5]],MAindy45[[3]][[5]],MAindy45[[4]][[5]],MAindy45[[5]][[5]],
                   MAmad45[[1]][[5]], MAmad45[[2]][[5]], MAmad45[[3]][[5]], MAmad45[[4]][[5]], MAmad45[[5]][[5]],
                   MAminn45[[1]][[5]],MAminn45[[2]][[5]],MAminn45[[3]][[5]],MAminn45[[4]][[5]],MAminn45[[5]][[5]])

means <- data.frame(rowMeans(ws45[,1:5]),rowMeans(ws45[,6:10]),rowMeans(ws45[,11:15]),rowMeans(ws45[,16:20]),rowMeans(ws45[,21:25]),rowMeans(ws45[,26:30]))

mins <- data.frame(apply(ws45[,1:5],1,FUN = min),apply(ws45[,6:10],1,FUN = min),apply(ws45[,11:15],1,FUN = min),apply(ws45[,16:20],1,FUN = min),apply(ws45[,21:25],1,FUN = min),apply(ws45[,26:30],1,FUN = min))
maxs <- data.frame(apply(ws45[,1:5],1,FUN = max),apply(ws45[,6:10],1,FUN = max),apply(ws45[,11:15],1,FUN = max),apply(w45[,16:20],1,FUN = max),apply(ws45[,21:25],1,FUN = max),apply(ws45[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.2) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.2) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.2) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.2) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.2) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.2) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Wind Speed (m/s)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 

# RCP 2.6

ws26 <- data.frame(MAchi26[[1]][[5]], MAchi26[[2]][[5]], MAchi26[[3]][[5]], MAchi26[[4]][[5]], MAchi26[[5]][[5]],
                   MAclev26[[1]][[5]],MAclev26[[2]][[5]],MAclev26[[3]][[5]],MAclev26[[4]][[5]],MAclev26[[5]][[5]],
                   MAcbus26[[1]][[5]],MAcbus26[[2]][[5]],MAcbus26[[3]][[5]],MAcbus26[[4]][[5]],MAcbus26[[5]][[5]],
                   MAindy26[[1]][[5]],MAindy26[[2]][[5]],MAindy26[[3]][[5]],MAindy26[[4]][[5]],MAindy26[[5]][[5]],
                   MAmad26[[1]][[5]], MAmad26[[2]][[5]], MAmad26[[3]][[5]], MAmad26[[4]][[5]], MAmad26[[5]][[5]],
                   MAminn26[[1]][[5]],MAminn26[[2]][[5]],MAminn26[[3]][[5]],MAminn26[[4]][[5]],MAminn26[[5]][[5]])

means <- data.frame(rowMeans(ws26[,1:5]),rowMeans(ws26[,6:10]),rowMeans(ws26[,11:15]),rowMeans(ws26[,16:20]),rowMeans(ws26[,21:25]),rowMeans(ws26[,26:30]))

mins <- data.frame(apply(ws26[,1:5],1,FUN = min),apply(ws26[,6:10],1,FUN = min),apply(ws26[,11:15],1,FUN = min),apply(ws26[,16:20],1,FUN = min),apply(ws26[,21:25],1,FUN = min),apply(ws26[,26:30],1,FUN = min))
maxs <- data.frame(apply(ws26[,1:5],1,FUN = max),apply(ws26[,6:10],1,FUN = max),apply(ws26[,11:15],1,FUN = max),apply(ws26[,16:20],1,FUN = max),apply(ws26[,21:25],1,FUN = max),apply(ws26[,26:30],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','chiMean','clevMean','cbusMean','indyMean','madMean','minnMean',
                    'chiMin','clevMin','cbusMin','indyMin','madMin','minnMin',
                    'chiMax','clevMax','cbusMax','indyMax','madMax','minnMax')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=chiMin, ymax=chiMax),alpha=0.2) + geom_line(aes(x=date,y=chiMean,color='Chicago')) +
  geom_ribbon(aes(x=date, ymin=clevMin, ymax=clevMax),alpha=0.2) + geom_line(aes(x=date,y=clevMean,color='Cleveland')) +
  geom_ribbon(aes(x=date, ymin=cbusMin, ymax=cbusMax),alpha=0.2) + geom_line(aes(x=date,y=cbusMean,color='Columbus')) +
  geom_ribbon(aes(x=date, ymin=indyMin, ymax=indyMax),alpha=0.2) + geom_line(aes(x=date,y=indyMean,color='Indianapolis')) +
  geom_ribbon(aes(x=date, ymin=madMin, ymax=madMax),alpha=0.2) + geom_line(aes(x=date,y=madMean,color='Madison')) +
  geom_ribbon(aes(x=date, ymin=minnMin, ymax=minnMax),alpha=0.2) + geom_line(aes(x=date,y=minnMean,color='Minneapolis')) +
  xlab('Date') + ylab(expression('Wind Speed (m/s)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="City",values=c(Chicago='red',Cleveland='blue',Columbus='green',Indianapolis='orange',Madison='purple',Minneapolis='pink')) 


# variable change in each city

# ENSO (Same for each city)

ensoALL <- data.frame(MAchi85[[1]][[1]],MAchi85[[2]][[1]],MAchi85[[3]][[1]],MAchi85[[4]][[1]],MAchi85[[5]][[1]],
                      MAchi60[[1]][[1]],MAchi60[[2]][[1]],MAchi60[[3]][[1]],MAchi60[[4]][[1]],MAchi60[[5]][[1]],
                      MAchi45[[1]][[1]],MAchi45[[2]][[1]],MAchi45[[3]][[1]],MAchi45[[4]][[1]],MAchi45[[5]][[1]],
                      MAchi26[[1]][[1]],MAchi26[[2]][[1]],MAchi26[[3]][[1]],MAchi26[[4]][[1]],MAchi26[[5]][[1]])

means <- data.frame(rowMeans(ensoALL[,1:5]),rowMeans(ensoALL[,6:10]),rowMeans(ensoALL[,11:15]),rowMeans(ensoALL[,16:20]))


mins <- data.frame(apply(ensoALL[,1:5],1,FUN = min),apply(ensoALL[,6:10],1,FUN = min),apply(ensoALL[,11:15],1,FUN = min),apply(ensoALL[,16:20],1,FUN = min))
maxs <- data.frame(apply(ensoALL[,1:5],1,FUN = max),apply(ensoALL[,6:10],1,FUN = max),apply(ensoALL[,11:15],1,FUN = max),apply(ensoALL[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('ENSO Index')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Dew Point

# Chicago

dpChi <- data.frame(MAchi85[[1]][[2]],MAchi85[[2]][[2]],MAchi85[[3]][[2]],MAchi85[[4]][[2]],MAchi85[[5]][[2]],
                    MAchi60[[1]][[2]],MAchi60[[2]][[2]],MAchi60[[3]][[2]],MAchi60[[4]][[2]],MAchi60[[5]][[2]],
                    MAchi45[[1]][[2]],MAchi45[[2]][[2]],MAchi45[[3]][[2]],MAchi45[[4]][[2]],MAchi45[[5]][[2]],
                    MAchi26[[1]][[2]],MAchi26[[2]][[2]],MAchi26[[3]][[2]],MAchi26[[4]][[2]],MAchi26[[5]][[2]])

means <- data.frame(rowMeans(dpChi[,1:5]),rowMeans(dpChi[,6:10]),rowMeans(dpChi[,11:15]),rowMeans(dpChi[,16:20]))

mins <- data.frame(apply(dpChi[,1:5],1,FUN = min),apply(dpChi[,6:10],1,FUN = min),apply(dpChi[,11:15],1,FUN = min),apply(dpChi[,16:20],1,FUN = min))
maxs <- data.frame(apply(dpChi[,1:5],1,FUN = max),apply(dpChi[,6:10],1,FUN = max),apply(dpChi[,11:15],1,FUN = max),apply(dpChi[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.2) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.2) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.2) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.2) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Dew Point Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Cleveland

dpClev <- data.frame(MAclev85[[1]][[2]],MAclev85[[2]][[2]],MAclev85[[3]][[2]],MAclev85[[4]][[2]],MAclev85[[5]][[2]],
                     MAclev60[[1]][[2]],MAclev60[[2]][[2]],MAclev60[[3]][[2]],MAclev60[[4]][[2]],MAclev60[[5]][[2]],
                     MAclev45[[1]][[2]],MAclev45[[2]][[2]],MAclev45[[3]][[2]],MAclev45[[4]][[2]],MAclev45[[5]][[2]],
                     MAclev26[[1]][[2]],MAclev26[[2]][[2]],MAclev26[[3]][[2]],MAclev26[[4]][[2]],MAclev26[[5]][[2]])

means <- data.frame(rowMeans(dpClev[,1:5]),rowMeans(dpClev[,6:10]),rowMeans(dpClev[,11:15]),rowMeans(dpClev[,16:20]))

mins <- data.frame(apply(dpClev[,1:5],1,FUN = min),apply(dpClev[,6:10],1,FUN = min),apply(dpClev[,11:15],1,FUN = min),apply(dpClev[,16:20],1,FUN = min))
maxs <- data.frame(apply(dpClev[,1:5],1,FUN = max),apply(dpClev[,6:10],1,FUN = max),apply(dpClev[,11:15],1,FUN = max),apply(dpClev[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Dew Point Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Columbus

dpCbus <- data.frame(MAcbus85[[1]][[2]],MAcbus85[[2]][[2]],MAcbus85[[3]][[2]],MAcbus85[[4]][[2]],MAcbus85[[5]][[2]],
                     MAcbus60[[1]][[2]],MAcbus60[[2]][[2]],MAcbus60[[3]][[2]],MAcbus60[[4]][[2]],MAcbus60[[5]][[2]],
                     MAcbus45[[1]][[2]],MAcbus45[[2]][[2]],MAcbus45[[3]][[2]],MAcbus45[[4]][[2]],MAcbus45[[5]][[2]],
                     MAcbus26[[1]][[2]],MAcbus26[[2]][[2]],MAcbus26[[3]][[2]],MAcbus26[[4]][[2]],MAcbus26[[5]][[2]])

means <- data.frame(rowMeans(dpCbus[,1:5]),rowMeans(dpCbus[,6:10]),rowMeans(dpCbus[,11:15]),rowMeans(dpCbus[,16:20]))

mins <- data.frame(apply(dpCbus[,1:5],1,FUN = min),apply(dpCbus[,6:10],1,FUN = min),apply(dpCbus[,11:15],1,FUN = min),apply(dpCbus[,16:20],1,FUN = min))
maxs <- data.frame(apply(dpCbus[,1:5],1,FUN = max),apply(dpCbus[,6:10],1,FUN = max),apply(dpCbus[,11:15],1,FUN = max),apply(dpCbus[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Dew Point Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Indianapolis

dpIndy <- data.frame(MAindy85[[1]][[2]],MAindy85[[2]][[2]],MAindy85[[3]][[2]],MAindy85[[4]][[2]],MAindy85[[5]][[2]],
                     MAindy60[[1]][[2]],MAindy60[[2]][[2]],MAindy60[[3]][[2]],MAindy60[[4]][[2]],MAindy60[[5]][[2]],
                     MAindy45[[1]][[2]],MAindy45[[2]][[2]],MAindy45[[3]][[2]],MAindy45[[4]][[2]],MAindy45[[5]][[2]],
                     MAindy26[[1]][[2]],MAindy26[[2]][[2]],MAindy26[[3]][[2]],MAindy26[[4]][[2]],MAindy26[[5]][[2]])

means <- data.frame(rowMeans(dpIndy[,1:5]),rowMeans(dpIndy[,6:10]),rowMeans(dpIndy[,11:15]),rowMeans(dpIndy[,16:20]))

mins <- data.frame(apply(dpIndy[,1:5],1,FUN = min),apply(dpIndy[,6:10],1,FUN = min),apply(dpIndy[,11:15],1,FUN = min),apply(dpIndy[,16:20],1,FUN = min))
maxs <- data.frame(apply(dpIndy[,1:5],1,FUN = max),apply(dpIndy[,6:10],1,FUN = max),apply(dpIndy[,11:15],1,FUN = max),apply(dpIndy[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Dew Point Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Madison

dpMad <- data.frame(MAmad85[[1]][[2]],MAmad85[[2]][[2]],MAmad85[[3]][[2]],MAmad85[[4]][[2]],MAmad85[[5]][[2]],
                    MAmad60[[1]][[2]],MAmad60[[2]][[2]],MAmad60[[3]][[2]],MAmad60[[4]][[2]],MAmad60[[5]][[2]],
                    MAmad45[[1]][[2]],MAmad45[[2]][[2]],MAmad45[[3]][[2]],MAmad45[[4]][[2]],MAmad45[[5]][[2]],
                    MAmad26[[1]][[2]],MAmad26[[2]][[2]],MAmad26[[3]][[2]],MAmad26[[4]][[2]],MAmad26[[5]][[2]])

means <- data.frame(rowMeans(dpMad[,1:5]),rowMeans(dpMad[,6:10]),rowMeans(dpMad[,11:15]),rowMeans(dpMad[,16:20]))

mins <- data.frame(apply(dpMad[,1:5],1,FUN = min),apply(dpMad[,6:10],1,FUN = min),apply(dpMad[,11:15],1,FUN = min),apply(dpMad[,16:20],1,FUN = min))
maxs <- data.frame(apply(dpMad[,1:5],1,FUN = max),apply(dpMad[,6:10],1,FUN = max),apply(dpMad[,11:15],1,FUN = max),apply(dpMad[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Dew Point Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Minneapolis

dpMinn <- data.frame(MAminn85[[1]][[2]],MAminn85[[2]][[2]],MAminn85[[3]][[2]],MAminn85[[4]][[2]],MAminn85[[5]][[2]],
                     MAminn60[[1]][[2]],MAminn60[[2]][[2]],MAminn60[[3]][[2]],MAminn60[[4]][[2]],MAminn60[[5]][[2]],
                     MAminn45[[1]][[2]],MAminn45[[2]][[2]],MAminn45[[3]][[2]],MAminn45[[4]][[2]],MAminn45[[5]][[2]],
                     MAminn26[[1]][[2]],MAminn26[[2]][[2]],MAminn26[[3]][[2]],MAminn26[[4]][[2]],MAminn26[[5]][[2]])

means <- data.frame(rowMeans(dpMinn[,1:5]),rowMeans(dpMinn[,6:10]),rowMeans(dpMinn[,11:15]),rowMeans(dpMinn[,16:20]))

mins <- data.frame(apply(dpMinn[,1:5],1,FUN = min),apply(dpMinn[,6:10],1,FUN = min),apply(dpMinn[,11:15],1,FUN = min),apply(dpMinn[,16:20],1,FUN = min))
maxs <- data.frame(apply(dpMinn[,1:5],1,FUN = max),apply(dpMinn[,6:10],1,FUN = max),apply(dpMinn[,11:15],1,FUN = max),apply(dpMinn[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Dew Point Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 


# Relative Humdidity

# Chicago

rhChi <- data.frame(MAchi85[[1]][[3]],MAchi85[[2]][[3]],MAchi85[[3]][[3]],MAchi85[[4]][[3]],MAchi85[[5]][[3]],
                    MAchi60[[1]][[3]],MAchi60[[2]][[3]],MAchi60[[3]][[3]],MAchi60[[4]][[3]],MAchi60[[5]][[3]],
                    MAchi45[[1]][[3]],MAchi45[[2]][[3]],MAchi45[[3]][[3]],MAchi45[[4]][[3]],MAchi45[[5]][[3]],
                    MAchi26[[1]][[3]],MAchi26[[2]][[3]],MAchi26[[3]][[3]],MAchi26[[4]][[3]],MAchi26[[5]][[3]])

means <- data.frame(rowMeans(rhChi[,1:5]),rowMeans(rhChi[,6:10]),rowMeans(rhChi[,11:15]),rowMeans(rhChi[,16:20]))

mins <- data.frame(apply(rhChi[,1:5],1,FUN = min),apply(rhChi[,6:10],1,FUN = min),apply(rhChi[,11:15],1,FUN = min),apply(rhChi[,16:20],1,FUN = min))
maxs <- data.frame(apply(rhChi[,1:5],1,FUN = max),apply(rhChi[,6:10],1,FUN = max),apply(rhChi[,11:15],1,FUN = max),apply(rhChi[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Relative Humidity (%)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Cleveland

rhClev <- data.frame(MAclev85[[1]][[3]],MAclev85[[2]][[3]],MAclev85[[3]][[3]],MAclev85[[4]][[3]],MAclev85[[5]][[3]],
                     MAclev60[[1]][[3]],MAclev60[[2]][[3]],MAclev60[[3]][[3]],MAclev60[[4]][[3]],MAclev60[[5]][[3]],
                     MAclev45[[1]][[3]],MAclev45[[2]][[3]],MAclev45[[3]][[3]],MAclev45[[4]][[3]],MAclev45[[5]][[3]],
                     MAclev26[[1]][[3]],MAclev26[[2]][[3]],MAclev26[[3]][[3]],MAclev26[[4]][[3]],MAclev26[[5]][[3]])

means <- data.frame(rowMeans(rhClev[,1:5]),rowMeans(rhClev[,6:10]),rowMeans(rhClev[,11:15]),rowMeans(rhClev[,16:20]))

mins <- data.frame(apply(rhClev[,1:5],1,FUN = min),apply(rhClev[,6:10],1,FUN = min),apply(rhClev[,11:15],1,FUN = min),apply(rhClev[,16:20],1,FUN = min))
maxs <- data.frame(apply(rhClev[,1:5],1,FUN = max),apply(rhClev[,6:10],1,FUN = max),apply(rhClev[,11:15],1,FUN = max),apply(rhClev[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Relative Humidity (%)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Columbus

rhCbus <- data.frame(MAcbus85[[1]][[3]],MAcbus85[[2]][[3]],MAcbus85[[3]][[3]],MAcbus85[[4]][[3]],MAcbus85[[5]][[3]],
                     MAcbus60[[1]][[3]],MAcbus60[[2]][[3]],MAcbus60[[3]][[3]],MAcbus60[[4]][[3]],MAcbus60[[5]][[3]],
                     MAcbus45[[1]][[3]],MAcbus45[[2]][[3]],MAcbus45[[3]][[3]],MAcbus45[[4]][[3]],MAcbus45[[5]][[3]],
                     MAcbus26[[1]][[3]],MAcbus26[[2]][[3]],MAcbus26[[3]][[3]],MAcbus26[[4]][[3]],MAcbus26[[5]][[3]])

means <- data.frame(rowMeans(rhCbus[,1:5]),rowMeans(rhCbus[,6:10]),rowMeans(rhCbus[,11:15]),rowMeans(rhCbus[,16:20]))

mins <- data.frame(apply(rhCbus[,1:5],1,FUN = min),apply(rhCbus[,6:10],1,FUN = min),apply(rhCbus[,11:15],1,FUN = min),apply(rhCbus[,16:20],1,FUN = min))
maxs <- data.frame(apply(rhCbus[,1:5],1,FUN = max),apply(rhCbus[,6:10],1,FUN = max),apply(rhCbus[,11:15],1,FUN = max),apply(rhCbus[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Relative Humidity (%)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Indianapolis

rhIndy <- data.frame(MAindy85[[1]][[3]],MAindy85[[2]][[3]],MAindy85[[3]][[3]],MAindy85[[4]][[3]],MAindy85[[5]][[3]],
                     MAindy60[[1]][[3]],MAindy60[[2]][[3]],MAindy60[[3]][[3]],MAindy60[[4]][[3]],MAindy60[[5]][[3]],
                     MAindy45[[1]][[3]],MAindy45[[2]][[3]],MAindy45[[3]][[3]],MAindy45[[4]][[3]],MAindy45[[5]][[3]],
                     MAindy26[[1]][[3]],MAindy26[[2]][[3]],MAindy26[[3]][[3]],MAindy26[[4]][[3]],MAindy26[[5]][[3]])

means <- data.frame(rowMeans(rhIndy[,1:5]),rowMeans(rhIndy[,6:10]),rowMeans(rhIndy[,11:15]),rowMeans(rhIndy[,16:20]))

mins <- data.frame(apply(rhIndy[,1:5],1,FUN = min),apply(rhIndy[,6:10],1,FUN = min),apply(rhIndy[,11:15],1,FUN = min),apply(rhIndy[,16:20],1,FUN = min))
maxs <- data.frame(apply(rhIndy[,1:5],1,FUN = max),apply(rhIndy[,6:10],1,FUN = max),apply(rhIndy[,11:15],1,FUN = max),apply(rhIndy[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Relative Humidity (%)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Madison

rhMad <- data.frame(MAmad85[[1]][[3]],MAmad85[[2]][[3]],MAmad85[[3]][[3]],MAmad85[[4]][[3]],MAmad85[[5]][[3]],
                    MAmad60[[1]][[3]],MAmad60[[2]][[3]],MAmad60[[3]][[3]],MAmad60[[4]][[3]],MAmad60[[5]][[3]],
                    MAmad45[[1]][[3]],MAmad45[[2]][[3]],MAmad45[[3]][[3]],MAmad45[[4]][[3]],MAmad45[[5]][[3]],
                    MAmad26[[1]][[3]],MAmad26[[2]][[3]],MAmad26[[3]][[3]],MAmad26[[4]][[3]],MAmad26[[5]][[3]])

means <- data.frame(rowMeans(rhMad[,1:5]),rowMeans(rhMad[,6:10]),rowMeans(rhMad[,11:15]),rowMeans(rhMad[,16:20]))

mins <- data.frame(apply(rhMad[,1:5],1,FUN = min),apply(rhMad[,6:10],1,FUN = min),apply(rhMad[,11:15],1,FUN = min),apply(rhMad[,16:20],1,FUN = min))
maxs <- data.frame(apply(rhMad[,1:5],1,FUN = max),apply(rhMad[,6:10],1,FUN = max),apply(rhMad[,11:15],1,FUN = max),apply(rhMad[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Relative Humidity (%)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Minneapolis

rhMinn <- data.frame(MAminn85[[1]][[3]],MAminn85[[2]][[3]],MAminn85[[3]][[3]],MAminn85[[4]][[3]],MAminn85[[5]][[3]],
                     MAminn60[[1]][[3]],MAminn60[[2]][[3]],MAminn60[[3]][[3]],MAminn60[[4]][[3]],MAminn60[[5]][[3]],
                     MAminn45[[1]][[3]],MAminn45[[2]][[3]],MAminn45[[3]][[3]],MAminn45[[4]][[3]],MAminn45[[5]][[3]],
                     MAminn26[[1]][[3]],MAminn26[[2]][[3]],MAminn26[[3]][[3]],MAminn26[[4]][[3]],MAminn26[[5]][[3]])

means <- data.frame(rowMeans(rhMinn[,1:5]),rowMeans(rhMinn[,6:10]),rowMeans(rhMinn[,11:15]),rowMeans(rhMinn[,16:20]))

mins <- data.frame(apply(rhMinn[,1:5],1,FUN = min),apply(rhMinn[,6:10],1,FUN = min),apply(rhMinn[,11:15],1,FUN = min),apply(rhMinn[,16:20],1,FUN = min))
maxs <- data.frame(apply(rhMinn[,1:5],1,FUN = max),apply(rhMinn[,6:10],1,FUN = max),apply(rhMinn[,11:15],1,FUN = max),apply(rhMinn[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Relative Humidity (%)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Dry Bulb Temperature

# Chicago

dbtChi <- data.frame(MAchi85[[1]][[4]],MAchi85[[2]][[4]],MAchi85[[3]][[4]],MAchi85[[4]][[4]],MAchi85[[5]][[4]],
                     MAchi60[[1]][[4]],MAchi60[[2]][[4]],MAchi60[[3]][[4]],MAchi60[[4]][[4]],MAchi60[[5]][[4]],
                     MAchi45[[1]][[4]],MAchi45[[2]][[4]],MAchi45[[3]][[4]],MAchi45[[4]][[4]],MAchi45[[5]][[4]],
                     MAchi26[[1]][[4]],MAchi26[[2]][[4]],MAchi26[[3]][[4]],MAchi26[[4]][[4]],MAchi26[[5]][[4]])

means <- data.frame(rowMeans(dbtChi[,1:5]),rowMeans(dbtChi[,6:10]),rowMeans(dbtChi[,11:15]),rowMeans(dbtChi[,16:20]))

mins <- data.frame(apply(dbtChi[,1:5],1,FUN = min),apply(dbtChi[,6:10],1,FUN = min),apply(dbtChi[,11:15],1,FUN = min),apply(dbtChi[,16:20],1,FUN = min))
maxs <- data.frame(apply(dbtChi[,1:5],1,FUN = max),apply(dbtChi[,6:10],1,FUN = max),apply(dbtChi[,11:15],1,FUN = max),apply(dbtChi[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Dry Bulb Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Cleveland

dbtClev <- data.frame(MAclev85[[1]][[4]],MAclev85[[2]][[4]],MAclev85[[3]][[4]],MAclev85[[4]][[4]],MAclev85[[5]][[4]],
                      MAclev60[[1]][[4]],MAclev60[[2]][[4]],MAclev60[[3]][[4]],MAclev60[[4]][[4]],MAclev60[[5]][[4]],
                      MAclev45[[1]][[4]],MAclev45[[2]][[4]],MAclev45[[3]][[4]],MAclev45[[4]][[4]],MAclev45[[5]][[4]],
                      MAclev26[[1]][[4]],MAclev26[[2]][[4]],MAclev26[[3]][[4]],MAclev26[[4]][[4]],MAclev26[[5]][[4]])

means <- data.frame(rowMeans(dbtClev[,1:5]),rowMeans(dbtClev[,6:10]),rowMeans(dbtClev[,11:15]),rowMeans(dbtClev[,16:20]))

mins <- data.frame(apply(dbtClev[,1:5],1,FUN = min),apply(dbtClev[,6:10],1,FUN = min),apply(dbtClev[,11:15],1,FUN = min),apply(dbtClev[,16:20],1,FUN = min))
maxs <- data.frame(apply(dbtClev[,1:5],1,FUN = max),apply(dbtClev[,6:10],1,FUN = max),apply(dbtClev[,11:15],1,FUN = max),apply(dbtClev[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Dry Bulb Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Columbus

dbtCbus <- data.frame(MAcbus85[[1]][[4]],MAcbus85[[2]][[4]],MAcbus85[[3]][[4]],MAcbus85[[4]][[4]],MAcbus85[[5]][[4]],
                      MAcbus60[[1]][[4]],MAcbus60[[2]][[4]],MAcbus60[[3]][[4]],MAcbus60[[4]][[4]],MAcbus60[[5]][[4]],
                      MAcbus45[[1]][[4]],MAcbus45[[2]][[4]],MAcbus45[[3]][[4]],MAcbus45[[4]][[4]],MAcbus45[[5]][[4]],
                      MAcbus26[[1]][[4]],MAcbus26[[2]][[4]],MAcbus26[[3]][[4]],MAcbus26[[4]][[4]],MAcbus26[[5]][[4]])

means <- data.frame(rowMeans(dbtCbus[,1:5]),rowMeans(dbtCbus[,6:10]),rowMeans(dbtCbus[,11:15]),rowMeans(dbtCbus[,16:20]))

mins <- data.frame(apply(dbtCbus[,1:5],1,FUN = min),apply(dbtCbus[,6:10],1,FUN = min),apply(dbtCbus[,11:15],1,FUN = min),apply(dbtCbus[,16:20],1,FUN = min))
maxs <- data.frame(apply(dbtCbus[,1:5],1,FUN = max),apply(dbtCbus[,6:10],1,FUN = max),apply(dbtCbus[,11:15],1,FUN = max),apply(dbtCbus[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Dry Bulb Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Indianapolis

dbtIndy <- data.frame(MAindy85[[1]][[4]],MAindy85[[2]][[4]],MAindy85[[3]][[4]],MAindy85[[4]][[4]],MAindy85[[5]][[4]],
                      MAindy60[[1]][[4]],MAindy60[[2]][[4]],MAindy60[[3]][[4]],MAindy60[[4]][[4]],MAindy60[[5]][[4]],
                      MAindy45[[1]][[4]],MAindy45[[2]][[4]],MAindy45[[3]][[4]],MAindy45[[4]][[4]],MAindy45[[5]][[4]],
                      MAindy26[[1]][[4]],MAindy26[[2]][[4]],MAindy26[[3]][[4]],MAindy26[[4]][[4]],MAindy26[[5]][[4]])

means <- data.frame(rowMeans(dbtIndy[,1:5]),rowMeans(dbtIndy[,6:10]),rowMeans(dbtIndy[,11:15]),rowMeans(dbtIndy[,16:20]))

mins <- data.frame(apply(dbtIndy[,1:5],1,FUN = min),apply(dbtIndy[,6:10],1,FUN = min),apply(dbtIndy[,11:15],1,FUN = min),apply(dbtIndy[,16:20],1,FUN = min))
maxs <- data.frame(apply(dbtIndy[,1:5],1,FUN = max),apply(dbtIndy[,6:10],1,FUN = max),apply(dbtIndy[,11:15],1,FUN = max),apply(dbtIndy[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Dry Bulb Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Madison

dbtMad <- data.frame(MAmad85[[1]][[4]],MAmad85[[2]][[4]],MAmad85[[3]][[4]],MAmad85[[4]][[4]],MAmad85[[5]][[4]],
                     MAmad60[[1]][[4]],MAmad60[[2]][[4]],MAmad60[[3]][[4]],MAmad60[[4]][[4]],MAmad60[[5]][[4]],
                     MAmad45[[1]][[4]],MAmad45[[2]][[4]],MAmad45[[3]][[4]],MAmad45[[4]][[4]],MAmad45[[5]][[4]],
                     MAmad26[[1]][[4]],MAmad26[[2]][[4]],MAmad26[[3]][[4]],MAmad26[[4]][[4]],MAmad26[[5]][[4]])

means <- data.frame(rowMeans(dbtMad[,1:5]),rowMeans(dbtMad[,6:10]),rowMeans(dbtMad[,11:15]),rowMeans(dbtMad[,16:20]))

mins <- data.frame(apply(dbtMad[,1:5],1,FUN = min),apply(dbtMad[,6:10],1,FUN = min),apply(dbtMad[,11:15],1,FUN = min),apply(dbtMad[,16:20],1,FUN = min))
maxs <- data.frame(apply(dbtMad[,1:5],1,FUN = max),apply(dbtMad[,6:10],1,FUN = max),apply(dbtMad[,11:15],1,FUN = max),apply(dbtMad[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Dry Bulb Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Minneapolis

dbtMinn <- data.frame(MAminn85[[1]][[4]],MAminn85[[2]][[4]],MAminn85[[3]][[4]],MAminn85[[4]][[4]],MAminn85[[5]][[4]],
                      MAminn60[[1]][[4]],MAminn60[[2]][[4]],MAminn60[[3]][[4]],MAminn60[[4]][[4]],MAminn60[[5]][[4]],
                      MAminn45[[1]][[4]],MAminn45[[2]][[4]],MAminn45[[3]][[4]],MAminn45[[4]][[4]],MAminn45[[5]][[4]],
                      MAminn26[[1]][[4]],MAminn26[[2]][[4]],MAminn26[[3]][[4]],MAminn26[[4]][[4]],MAminn26[[5]][[4]])

means <- data.frame(rowMeans(dbtMinn[,1:5]),rowMeans(dbtMinn[,6:10]),rowMeans(dbtMinn[,11:15]),rowMeans(dbtMinn[,16:20]))

mins <- data.frame(apply(dbtMinn[,1:5],1,FUN = min),apply(dbtMinn[,6:10],1,FUN = min),apply(dbtMinn[,11:15],1,FUN = min),apply(dbtMinn[,16:20],1,FUN = min))
maxs <- data.frame(apply(dbtMinn[,1:5],1,FUN = max),apply(dbtMinn[,6:10],1,FUN = max),apply(dbtMinn[,11:15],1,FUN = max),apply(dbtMinn[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Dry Bulb Temperature (degC)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Wind Speed

# Chicago

wsChi <- data.frame(MAchi85[[1]][[5]],MAchi85[[2]][[5]],MAchi85[[3]][[5]],MAchi85[[4]][[5]],MAchi85[[5]][[5]],
                    MAchi60[[1]][[5]],MAchi60[[2]][[5]],MAchi60[[3]][[5]],MAchi60[[4]][[5]],MAchi60[[5]][[5]],
                    MAchi45[[1]][[5]],MAchi45[[2]][[5]],MAchi45[[3]][[5]],MAchi45[[4]][[5]],MAchi45[[5]][[5]],
                    MAchi26[[1]][[5]],MAchi26[[2]][[5]],MAchi26[[3]][[5]],MAchi26[[4]][[5]],MAchi26[[5]][[5]])

means <- data.frame(rowMeans(wsChi[,1:5]),rowMeans(wsChi[,6:10]),rowMeans(wsChi[,11:15]),rowMeans(wsChi[,16:20]))

mins <- data.frame(apply(wsChi[,1:5],1,FUN = min),apply(wsChi[,6:10],1,FUN = min),apply(wsChi[,11:15],1,FUN = min),apply(wsChi[,16:20],1,FUN = min))
maxs <- data.frame(apply(wsChi[,1:5],1,FUN = max),apply(wsChi[,6:10],1,FUN = max),apply(wsChi[,11:15],1,FUN = max),apply(wsChi[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Wind Speed (m/s)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Cleveland

wsClev <- data.frame(MAclev85[[1]][[5]],MAclev85[[2]][[5]],MAclev85[[3]][[5]],MAclev85[[4]][[5]],MAclev85[[5]][[5]],
                     MAclev60[[1]][[5]],MAclev60[[2]][[5]],MAclev60[[3]][[5]],MAclev60[[4]][[5]],MAclev60[[5]][[5]],
                     MAclev45[[1]][[5]],MAclev45[[2]][[5]],MAclev45[[3]][[5]],MAclev45[[4]][[5]],MAclev45[[5]][[5]],
                     MAclev26[[1]][[5]],MAclev26[[2]][[5]],MAclev26[[3]][[5]],MAclev26[[4]][[5]],MAclev26[[5]][[5]])

means <- data.frame(rowMeans(wsClev[,1:5]),rowMeans(wsClev[,6:10]),rowMeans(wsClev[,11:15]),rowMeans(wsClev[,16:20]))

mins <- data.frame(apply(wsClev[,1:5],1,FUN = min),apply(wsClev[,6:10],1,FUN = min),apply(wsClev[,11:15],1,FUN = min),apply(wsClev[,16:20],1,FUN = min))
maxs <- data.frame(apply(wsClev[,1:5],1,FUN = max),apply(wsClev[,6:10],1,FUN = max),apply(wsClev[,11:15],1,FUN = max),apply(wsClev[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  #geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  #geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Wind Speed (m/s)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Columbus

wsCbus <- data.frame(MAcbus85[[1]][[5]],MAcbus85[[2]][[5]],MAcbus85[[3]][[5]],MAcbus85[[4]][[5]],MAcbus85[[5]][[5]],
                     MAcbus60[[1]][[5]],MAcbus60[[2]][[5]],MAcbus60[[3]][[5]],MAcbus60[[4]][[5]],MAcbus60[[5]][[5]],
                     MAcbus45[[1]][[5]],MAcbus45[[2]][[5]],MAcbus45[[3]][[5]],MAcbus45[[4]][[5]],MAcbus45[[5]][[5]],
                     MAcbus26[[1]][[5]],MAcbus26[[2]][[5]],MAcbus26[[3]][[5]],MAcbus26[[4]][[5]],MAcbus26[[5]][[5]])

means <- data.frame(rowMeans(wsCbus[,1:5]),rowMeans(wsCbus[,6:10]),rowMeans(wsCbus[,11:15]),rowMeans(wsCbus[,16:20]))

mins <- data.frame(apply(wsCbus[,1:5],1,FUN = min),apply(wsCbus[,6:10],1,FUN = min),apply(wsCbus[,11:15],1,FUN = min),apply(wsCbus[,16:20],1,FUN = min))
maxs <- data.frame(apply(wsCbus[,1:5],1,FUN = max),apply(wsCbus[,6:10],1,FUN = max),apply(wsCbus[,11:15],1,FUN = max),apply(wsCbus[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  #geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  #geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Wind Speed (m/s)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Indianapolis

wsIndy <- data.frame(MAindy85[[1]][[5]],MAindy85[[2]][[5]],MAindy85[[3]][[5]],MAindy85[[4]][[5]],MAindy85[[5]][[5]],
                     MAindy60[[1]][[5]],MAindy60[[2]][[5]],MAindy60[[3]][[5]],MAindy60[[4]][[5]],MAindy60[[5]][[5]],
                     MAindy45[[1]][[5]],MAindy45[[2]][[5]],MAindy45[[3]][[5]],MAindy45[[4]][[5]],MAindy45[[5]][[5]],
                     MAindy26[[1]][[5]],MAindy26[[2]][[5]],MAindy26[[3]][[5]],MAindy26[[4]][[5]],MAindy26[[5]][[5]])

means <- data.frame(rowMeans(wsIndy[,1:5]),rowMeans(wsIndy[,6:10]),rowMeans(wsIndy[,11:15]),rowMeans(wsIndy[,16:20]))

mins <- data.frame(apply(wsIndy[,1:5],1,FUN = min),apply(wsIndy[,6:10],1,FUN = min),apply(wsIndy[,11:15],1,FUN = min),apply(wsIndy[,16:20],1,FUN = min))
maxs <- data.frame(apply(wsIndy[,1:5],1,FUN = max),apply(wsIndy[,6:10],1,FUN = max),apply(wsIndy[,11:15],1,FUN = max),apply(wsIndy[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  #geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  #geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Wind Speed (m/s)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Madison

wsMad <- data.frame(MAmad85[[1]][[5]],MAmad85[[2]][[5]],MAmad85[[3]][[5]],MAmad85[[4]][[5]],MAmad85[[5]][[5]],
                    MAmad60[[1]][[5]],MAmad60[[2]][[5]],MAmad60[[3]][[5]],MAmad60[[4]][[5]],MAmad60[[5]][[5]],
                    MAmad45[[1]][[5]],MAmad45[[2]][[5]],MAmad45[[3]][[5]],MAmad45[[4]][[5]],MAmad45[[5]][[5]],
                    MAmad26[[1]][[5]],MAmad26[[2]][[5]],MAmad26[[3]][[5]],MAmad26[[4]][[5]],MAmad26[[5]][[5]])

means <- data.frame(rowMeans(wsMad[,1:5]),rowMeans(wsMad[,6:10]),rowMeans(wsMad[,11:15]),rowMeans(wsMad[,16:20]))

mins <- data.frame(apply(wsMad[,1:5],1,FUN = min),apply(wsMad[,6:10],1,FUN = min),apply(wsMad[,11:15],1,FUN = min),apply(wsMad[,16:20],1,FUN = min))
maxs <- data.frame(apply(wsMad[,1:5],1,FUN = max),apply(wsMad[,6:10],1,FUN = max),apply(wsMad[,11:15],1,FUN = max),apply(wsMad[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Wind Speed (m/s)')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 

# Minneapolis

wsMinn <- data.frame(MAminn85[[1]][[5]],MAminn85[[2]][[5]],MAminn85[[3]][[5]],MAminn85[[4]][[5]],MAminn85[[5]][[5]],
                     MAminn60[[1]][[5]],MAminn60[[2]][[5]],MAminn60[[3]][[5]],MAminn60[[4]][[5]],MAminn60[[5]][[5]],
                     MAminn45[[1]][[5]],MAminn45[[2]][[5]],MAminn45[[3]][[5]],MAminn45[[4]][[5]],MAminn45[[5]][[5]],
                     MAminn26[[1]][[5]],MAminn26[[2]][[5]],MAminn26[[3]][[5]],MAminn26[[4]][[5]],MAminn26[[5]][[5]])

means <- data.frame(rowMeans(wsMinn[,1:5]),rowMeans(wsMinn[,6:10]),rowMeans(wsMinn[,11:15]),rowMeans(wsMinn[,16:20]))

mins <- data.frame(apply(wsMinn[,1:5],1,FUN = min),apply(wsMinn[,6:10],1,FUN = min),apply(wsMinn[,11:15],1,FUN = min),apply(wsMinn[,16:20],1,FUN = min))
maxs <- data.frame(apply(wsMinn[,1:5],1,FUN = max),apply(wsMinn[,6:10],1,FUN = max),apply(wsMinn[,11:15],1,FUN = max),apply(wsMinn[,16:20],1,FUN = max))

vardata <- data.frame(date,means,mins,maxs)
names(vardata) <- c('date','mean85','mean60','mean45','mean26','min85','min60','min45','min26','max85','max60','max45','max26')

ggplot(vardata) + geom_ribbon(aes(x=date, ymin=min85, ymax=max85),alpha=0.15) + geom_line(aes(x=date,y=mean85,color='RCP8.5')) +
  geom_ribbon(aes(x=date, ymin=min60, ymax=max60),alpha=0.15) + geom_line(aes(x=date,y=mean60,color='RCP6.0')) +
  geom_ribbon(aes(x=date, ymin=min45, ymax=max45),alpha=0.15) + geom_line(aes(x=date,y=mean45,color='RCP4.5')) +
  geom_ribbon(aes(x=date, ymin=min26, ymax=max26),alpha=0.15) + geom_line(aes(x=date,y=mean26,color='RCP2.6')) +
  xlab('Date') + ylab(expression('Wind Speed')) +
  theme_light(base_size=17) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_colour_manual(name="Scenario",values=c(RCP8.5='red',RCP6.0='blue',RCP4.5='green',RCP2.6='orange')) 



#################################   MODEL RUN - PRECIP & TEMP ONLY ###########################

yhatallPT <- list()

for (i in 1:6) {
  
  # run model
  Y <- cities[[i]][,3:4] # response
  X <- cities[[i]][,c(5,11)] 
  names(X) <- c('Dry Bulb Temp.','Precipitation')
  names(Y) <- c('Water Use','Electricity Use')
  
  outPT <- mvtb(Y=Y,X=X, n.trees=1000, shrinkage=.01, interaction.depth=3)
  
  # variable influence
  # numformat <- function(val){sub("^(-?)0.", "\\1.", sprintf("%.1f", val))}
  # blues <- c('#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c','#08306b')
  # bgr <- colorRampPalette(blues,space = "Lab",bias = 7)
  # par(mar=c(15,10,1,25))
  # mvtb.heat(t(mvtb.ri(out)),clust.method = NULL,cexRow=1,cexCol=1,numformat=numformat,col = bgr(500))
  
  # get future projections for each scenario/model
  
  # initialize
  predscenariosPT <- list()
  
  for (s in 1:4) {
    # initialize
    predvaluesPT <- list()
    for (m in 1:5) {
      # fit model
      yhatPT <- predict(outPT,newdata=futdata[[i]][[s]][[m]][,c(5,7)])
      
      # store data
      predvaluesPT[[m]] <- yhatPT
    }
    predscenariosPT[[s]] <- predvaluesPT
  }
  
  yhatallPT[[i]] <- predscenariosPT
  
}

#################################   ANALYSIS - PRECIP & TEMP ONLY ###########################

# percent change between 1971-2000 and last 30 years when 1.5 deg threshold was reached (pos == % increase) [new-old/old]

# HadGEM RCP2.6 --> 2007-2036 ([433:792,])
pcW26HadGEM15allPT <- list()
pcE26HadGEM15allPT <- list()
for (i in 1:6) {
  pcW26HadGEM15allPT[[i]] <- ((yhatallPT[[i]][[4]][[2]][433:792,1]-yhatallPT[[i]][[4]][[2]][1:360,1])/yhatallPT[[i]][[4]][[2]][1:360,1])*100 
  pcE26HadGEM15allPT[[i]] <- ((yhatallPT[[i]][[4]][[2]][433:792,2]-yhatallPT[[i]][[4]][[2]][1:360,2])/yhatallPT[[i]][[4]][[2]][1:360,2])*100 
}

# IPSL RCP2.6 --> 2008-2037 ([445:804,])
pcW26IPSL15allPT <- list()
pcE26IPSL15allPT <- list()
for (i in 1:6) {
  pcW26IPSL15allPT[[i]] <- ((yhatallPT[[i]][[4]][[3]][445:804,1]-yhatallPT[[i]][[4]][[3]][1:360,1])/yhatallPT[[i]][[4]][[3]][1:360,1])*100 
  pcE26IPSL15allPT[[i]] <- ((yhatallPT[[i]][[4]][[3]][445:804,2]-yhatallPT[[i]][[4]][[3]][1:360,2])/yhatallPT[[i]][[4]][[3]][1:360,2])*100 
}

# MIROC RCP2.6 --> 2006-2035 ([421:780,])
pcW26MIROC15allPT <- list()
pcE26MIROC15allPT <- list()
for (i in 1:6) {
  pcW26MIROC15allPT[[i]] <- ((yhatallPT[[i]][[4]][[4]][421:780,1]-yhatallPT[[i]][[4]][[4]][1:360,1])/yhatallPT[[i]][[4]][[4]][1:360,1])*100 
  pcE26MIROC15allPT[[i]] <- ((yhatallPT[[i]][[4]][[4]][421:780,2]-yhatallPT[[i]][[4]][[4]][1:360,2])/yhatallPT[[i]][[4]][[4]][1:360,2])*100 
}

# NorESM RCP2.6 --> 2047-2076  ([913:1272,])
pcW26NorESM15allPT <- list()
pcE26NorESM15allPT <- list()
for (i in 1:6) {
  pcW26NorESM15allPT[[i]] <- ((yhatallPT[[i]][[4]][[5]][913:1272,1]-yhatallPT[[i]][[4]][[5]][1:360,1])/yhatallPT[[i]][[4]][[5]][1:360,1])*100 
  pcE26NorESM15allPT[[i]] <- ((yhatallPT[[i]][[4]][[5]][913:1272,2]-yhatallPT[[i]][[4]][[5]][1:360,2])/yhatallPT[[i]][[4]][[5]][1:360,2])*100 
}

# GFDL RCP8.5 --> 2021-2050 ([601:960,])
pcW85GFDL15allPT <- list()
pcE85GFDL15allPT <- list()
for (i in 1:6) {
  pcW85GFDL15allPT[[i]] <- ((yhatallPT[[i]][[1]][[1]][601:960,1]-yhatallPT[[i]][[4]][[2]][1:360,1])/yhatallPT[[i]][[1]][[1]][1:360,1])*100 
  pcE85GFDL15allPT[[i]] <- ((yhatallPT[[i]][[1]][[1]][601:960,2]-yhatallPT[[i]][[4]][[2]][1:360,2])/yhatallPT[[i]][[1]][[1]][1:360,2])*100 
}

# HadGEM RCP8.5 --> 2004-2033 ([397:756,])
pcW85HadGEM15allPT <- list()
pcE85HadGEM15allPT <- list()
for (i in 1:6) {
  pcW85HadGEM15allPT[[i]] <- ((yhatallPT[[i]][[1]][[2]][397:756,1]-yhatallPT[[i]][[1]][[2]][1:360,1])/yhatallPT[[i]][[1]][[2]][1:360,1])*100 
  pcE85HadGEM15allPT[[i]] <- ((yhatallPT[[i]][[1]][[2]][397:756,2]-yhatallPT[[i]][[1]][[2]][1:360,2])/yhatallPT[[i]][[1]][[2]][1:360,2])*100 
}

# IPSL RCP8.5 --> 2006-2035 ([421:780,])
pcW85IPSL15allPT <- list()
pcE85IPSL15allPT <- list()
for (i in 1:6) {
  pcW85IPSL15allPT[[i]] <- ((yhatallPT[[i]][[1]][[3]][421:780,1]-yhatallPT[[i]][[1]][[3]][1:360,1])/yhatallPT[[i]][[1]][[3]][1:360,1])*100 
  pcE85IPSL15allPT[[i]] <- ((yhatallPT[[i]][[1]][[3]][421:780,2]-yhatallPT[[i]][[1]][[3]][1:360,2])/yhatallPT[[i]][[1]][[3]][1:360,2])*100 
}

# MIROC RCP8.5 --> 2006-2035 ([421:780,])
pcW85MIROC15allPT <- list()
pcE85MIROC15allPT <- list()
for (i in 1:6) {
  pcW85MIROC15allPT[[i]] <- ((yhatallPT[[i]][[1]][[4]][421:780,1]-yhatallPT[[i]][[1]][[4]][1:360,1])/yhatallPT[[i]][[1]][[4]][1:360,1])*100 
  pcE85MIROC15allPT[[i]] <- ((yhatallPT[[i]][[1]][[4]][421:780,2]-yhatallPT[[i]][[1]][[4]][1:360,2])/yhatallPT[[i]][[1]][[4]][1:360,2])*100 
}

# NorESM RCP8.5 --> 2016-2045 ([541:900,])
pcW85NorESM15allPT <- list()
pcE85NorESM15allPT <- list()
for (i in 1:6) {
  pcW85NorESM15allPT[[i]] <- ((yhatallPT[[i]][[1]][[5]][541:900,1]-yhatallPT[[i]][[1]][[5]][1:360,1])/yhatallPT[[i]][[1]][[5]][1:360,1])*100 
  pcE85NorESM15allPT[[i]] <- ((yhatallPT[[i]][[1]][[5]][541:900,2]-yhatallPT[[i]][[1]][[5]][1:360,2])/yhatallPT[[i]][[1]][[5]][1:360,2])*100 
}

# percent change between 1971-2000 and last 30 years when 2.0 deg threshold was reached (pos == % increase) [new-old/old]

# HadGEM RCP2.6 --> 2029-2058 ([697:1056,])
pcW26HadGEM20allPT <- list()
pcE26HadGEM20allPT <- list()
for (i in 1:6) {
  pcW26HadGEM20allPT[[i]] <- ((yhatallPT[[i]][[4]][[2]][697:1056,1]-yhatallPT[[i]][[4]][[2]][1:360,1])/yhatallPT[[i]][[4]][[2]][1:360,1])*100 
  pcE26HadGEM20allPT[[i]] <- ((yhatallPT[[i]][[4]][[2]][697:1056,2]-yhatallPT[[i]][[4]][[2]][1:360,2])/yhatallPT[[i]][[4]][[2]][1:360,2])*100 
}

# IPSL RCP2.6 --> 2060-2089 ([1069:1428,])
pcW26IPSL20allPT <- list()
pcE26IPSL20allPT <- list()
for (i in 1:6) {
  pcW26IPSL20allPT[[i]] <- ((yhatallPT[[i]][[4]][[3]][1069:1428,1]-yhatallPT[[i]][[4]][[3]][1:360,1])/yhatallPT[[i]][[4]][[3]][1:360,1])*100 
  pcE26IPSL20allPT[[i]] <- ((yhatallPT[[i]][[4]][[3]][1069:1428,2]-yhatallPT[[i]][[4]][[3]][1:360,2])/yhatallPT[[i]][[4]][[3]][1:360,2])*100 
}

# MIROC RCP2.6 --> 2023-2052 ([625:984,])
pcW26MIROC20allPT <- list()
pcE26MIROC20allPT <- list()
for (i in 1:6) {
  pcW26MIROC20allPT[[i]] <- ((yhatallPT[[i]][[4]][[4]][625:984,1]-yhatallPT[[i]][[4]][[4]][1:360,1])/yhatallPT[[i]][[4]][[4]][1:360,1])*100 
  pcE26MIROC20allPT[[i]] <- ((yhatallPT[[i]][[4]][[4]][625:984,2]-yhatallPT[[i]][[4]][[4]][1:360,2])/yhatallPT[[i]][[4]][[4]][1:360,2])*100 
}

# GFDL RCP8.5 --> 2038-2067 ([805:1164,])
pcW85GFDL20allPT <- list()
pcE85GFDL20allPT <- list()
for (i in 1:6) {
  pcW85GFDL20allPT[[i]] <- ((yhatallPT[[i]][[1]][[1]][805:1164,1]-yhatallPT[[i]][[4]][[2]][1:360,1])/yhatallPT[[i]][[1]][[1]][1:360,1])*100 
  pcE85GFDL20allPT[[i]] <- ((yhatallPT[[i]][[1]][[1]][805:1164,2]-yhatallPT[[i]][[4]][[2]][1:360,2])/yhatallPT[[i]][[1]][[1]][1:360,2])*100 
}

# HadGEM RCP8.5 --> 2016-2045 ([541:900,])
pcW85HadGEM20allPT <- list()
pcE85HadGEM20allPT <- list()
for (i in 1:6) {
  pcW85HadGEM20allPT[[i]] <- ((yhatallPT[[i]][[1]][[2]][541:900,1]-yhatallPT[[i]][[1]][[2]][1:360,1])/yhatallPT[[i]][[1]][[2]][1:360,1])*100 
  pcE85HadGEM20allPT[[i]] <- ((yhatallPT[[i]][[1]][[2]][541:900,2]-yhatallPT[[i]][[1]][[2]][1:360,2])/yhatallPT[[i]][[1]][[2]][1:360,2])*100 
}

# IPSL RCP8.5 --> 2018-2047 ([565:924,])
pcW85IPSL20allPT <- list()
pcE85IPSL20allPT <- list()
for (i in 1:6) {
  pcW85IPSL20allPT[[i]] <- ((yhatallPT[[i]][[1]][[3]][565:924,1]-yhatallPT[[i]][[1]][[3]][1:360,1])/yhatallPT[[i]][[1]][[3]][1:360,1])*100 
  pcE85IPSL20allPT[[i]] <- ((yhatallPT[[i]][[1]][[3]][565:924,2]-yhatallPT[[i]][[1]][[3]][1:360,2])/yhatallPT[[i]][[1]][[3]][1:360,2])*100 
}

# MIROC RCP8.5 --> 2017-2046 ([553:912,])
pcW85MIROC20allPT <- list()
pcE85MIROC20allPT <- list()
for (i in 1:6) {
  pcW85MIROC20allPT[[i]] <- ((yhatallPT[[i]][[1]][[4]][553:912,1]-yhatallPT[[i]][[1]][[4]][1:360,1])/yhatallPT[[i]][[1]][[4]][1:360,1])*100 
  pcE85MIROC20allPT[[i]] <- ((yhatallPT[[i]][[1]][[4]][553:912,2]-yhatallPT[[i]][[1]][[4]][1:360,2])/yhatallPT[[i]][[1]][[4]][1:360,2])*100 
}

# NorESM RCP8.5 --> 2031-2060 ([721:1080,])
pcW85NorESM20allPT <- list()
pcE85NorESM20allPT <- list()
for (i in 1:6) {
  pcW85NorESM20allPT[[i]] <- ((yhatallPT[[i]][[1]][[5]][721:1080,1]-yhatallPT[[i]][[1]][[5]][1:360,1])/yhatallPT[[i]][[1]][[5]][1:360,1])*100 
  pcE85NorESM20allPT[[i]] <- ((yhatallPT[[i]][[1]][[5]][721:1080,2]-yhatallPT[[i]][[1]][[5]][1:360,2])/yhatallPT[[i]][[1]][[5]][1:360,2])*100 
}

# percent change between 1971-2000 and last 30 years when 3.0 deg threshold was reached (pos == % increase) [new-old/old]

# GFDLRCP8.5 --> 2067-2096 ([1153:1512,])
pcW85GFDL30allPT <- list()
pcE85GFDL30allPT <- list()
for (i in 1:6) {
  pcW85GFDL30allPT[[i]] <- ((yhatallPT[[i]][[1]][[1]][1153:1512,1]-yhatallPT[[i]][[4]][[2]][1:360,1])/yhatallPT[[i]][[1]][[1]][1:360,1])*100 
  pcE85GFDL30allPT[[i]] <- ((yhatallPT[[i]][[1]][[1]][1153:1512,2]-yhatallPT[[i]][[4]][[2]][1:360,2])/yhatallPT[[i]][[1]][[1]][1:360,2])*100 
}

# HadGEM RCP8.5 --> 2035-2064 ([769:1128,])
pcW85HadGEM30allPT <- list()
pcE85HadGEM30allPT <- list()
for (i in 1:6) {
  pcW85HadGEM30allPT[[i]] <- ((yhatallPT[[i]][[1]][[2]][769:1128,1]-yhatallPT[[i]][[1]][[2]][1:360,1])/yhatallPT[[i]][[1]][[2]][1:360,1])*100 
  pcE85HadGEM30allPT[[i]] <- ((yhatallPT[[i]][[1]][[2]][769:1128,2]-yhatallPT[[i]][[1]][[2]][1:360,2])/yhatallPT[[i]][[1]][[2]][1:360,2])*100 
}

# IPSL RCP8.5 --> 2038-2067 ([805:1164,])
pcW85IPSL30allPT <- list()
pcE85IPSL30allPT <- list()
for (i in 1:6) {
  pcW85IPSL30allPT[[i]] <- ((yhatallPT[[i]][[1]][[3]][805:1164,1]-yhatallPT[[i]][[1]][[3]][1:360,1])/yhatallPT[[i]][[1]][[3]][1:360,1])*100 
  pcE85IPSL30allPT[[i]] <- ((yhatallPT[[i]][[1]][[3]][805:1164,2]-yhatallPT[[i]][[1]][[3]][1:360,2])/yhatallPT[[i]][[1]][[3]][1:360,2])*100 
}

# MIROC RCP8.5 --> 2037-2066 ([793:1152,])
pcW85MIROC30allPT <- list()
pcE85MIROC30allPT <- list()
for (i in 1:6) {
  pcW85MIROC30allPT[[i]] <- ((yhatallPT[[i]][[1]][[4]][793:1152,1]-yhatallPT[[i]][[1]][[4]][1:360,1])/yhatallPT[[i]][[1]][[4]][1:360,1])*100 
  pcE85MIROC30allPT[[i]] <- ((yhatallPT[[i]][[1]][[4]][793:1152,2]-yhatallPT[[i]][[1]][[4]][1:360,2])/yhatallPT[[i]][[1]][[4]][1:360,2])*100 
}

# NorESM RCP8.5 --> 2057-2086 ([1033:1392,])
pcW85NorESM30allPT <- list()
pcE85NorESM30allPT <- list()
for (i in 1:6) {
  pcW85NorESM30allPT[[i]] <- ((yhatallPT[[i]][[1]][[5]][1033:1392,1]-yhatallPT[[i]][[1]][[5]][1:360,1])/yhatallPT[[i]][[1]][[5]][1:360,1])*100 
  pcE85NorESM30allPT[[i]] <- ((yhatallPT[[i]][[1]][[5]][1033:1392,2]-yhatallPT[[i]][[1]][[5]][1:360,2])/yhatallPT[[i]][[1]][[5]][1:360,2])*100 
}

# moving average
mawaterPT <- list()
maelecPT <- list()

for (c in 1:6) {
  mascenw <- list()
  mascene <- list()
  for (s in 1:4) {
    mamodw <- list()
    mamode <- list()
    for (m in 1:5) {
      w <- rollapply(yhatallPT[[c]][[s]][[m]][,1],maperiod,mean,align='center',partial=T)
      e <- rollapply(yhatallPT[[c]][[s]][[m]][,2],maperiod,mean,align='center',partial=T)
      
      # store values
      mamodw[[m]] <- w
      mamode[[m]] <- e
    }
    mascenw[[s]] <- (mamodw[[1]]+mamodw[[2]]+mamodw[[3]]+mamodw[[4]]+mamodw[[5]])/5
    mascene[[s]] <- (mamode[[1]]+mamode[[2]]+mamode[[3]]+mamode[[4]]+mamode[[5]])/5
  }
  mawaterPT[[c]] <- mascenw
  maelecPT[[c]] <- mascene
}

#################################   FIGURES - PRECIP & TEMP ONLY ###########################

# percent change per city (means/standard deviations)

for (i in 1:6) {
  
  pchange <- c(0,mean(pcW26HadGEM15allPT[[i]]),mean(pcW26IPSL15allPT[[i]]),mean(pcW26MIROC15allPT[[i]]),mean(pcW26NorESM15allPT[[i]]),
               0,mean(pcW26HadGEM20allPT[[i]]),mean(pcW26IPSL20allPT[[i]]),mean(pcW26MIROC20allPT[[i]]),0,
               mean(pcW85GFDL15allPT[[i]]),mean(pcW85HadGEM15allPT[[i]]),mean(pcW85IPSL15allPT[[i]]),mean(pcW85MIROC15allPT[[i]]),mean(pcW85NorESM15allPT[[i]]),
               mean(pcW85GFDL20allPT[[i]]),mean(pcW85HadGEM20allPT[[i]]),mean(pcW85IPSL20allPT[[i]]),mean(pcW85MIROC20allPT[[i]]),mean(pcW85NorESM20allPT[[i]]),
               mean(pcW85GFDL30allPT[[i]]),mean(pcW85HadGEM30allPT[[i]]),mean(pcW85IPSL30allPT[[i]]),mean(pcW85MIROC30allPT[[i]]),mean(pcW85NorESM30allPT[[i]]))
  
  sds <- c(NA,sd(pcW26HadGEM15allPT[[i]]),sd(pcW26IPSL15allPT[[i]]),sd(pcW26MIROC15allPT[[i]]),sd(pcW26NorESM15allPT[[i]]),
           NA,sd(pcW26HadGEM20allPT[[i]]),sd(pcW26IPSL20allPT[[i]]),sd(pcW26MIROC20allPT[[i]]),NA,
           sd(pcW85GFDL15allPT[[i]]),sd(pcW85HadGEM15allPT[[i]]),sd(pcW85IPSL15allPT[[i]]),sd(pcW85MIROC15allPT[[i]]),sd(pcW85NorESM15allPT[[i]]),
           sd(pcW85GFDL20allPT[[i]]),sd(pcW85HadGEM20allPT[[i]]),sd(pcW85IPSL20allPT[[i]]),sd(pcW85MIROC20allPT[[i]]),sd(pcW85NorESM20allPT[[i]]),
           sd(pcW85GFDL30allPT[[i]]),sd(pcW85HadGEM30allPT[[i]]),sd(pcW85IPSL30allPT[[i]]),sd(pcW85MIROC30allPT[[i]]),sd(pcW85NorESM30allPT[[i]]))
  
  
  Scenario <- c("RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP2.6",
                "RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP2.6",
                "RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5",
                "RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5",
                "RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5")
  
  Threshold <- c("1.5","1.5","1.5","1.5","1.5",
                 "2.0","2.0","2.0","2.0","2.0",
                 "1.5","1.5","1.5","1.5","1.5",
                 "2.0","2.0","2.0","2.0","2.0",
                 "3.0","3.0","3.0","3.0","3.0")
  
  Model <- c("GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M")
  
  pcdata <- data.frame(pchange,Scenario, Model,Threshold,sds)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange, fill=Model)) + geom_bar(position="dodge", stat="identity") +
          geom_errorbar(aes(x=Threshold, ymin=pchange-sds, ymax=pchange+sds), width=.2, position=position_dodge(.9)) +
          xlab('Temperature Threshold (degC)') + ylab(expression('Percent Change (%)')) + ggtitle('Change in Water Use')+
          theme_light(base_size=14) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
          scale_fill_manual(values=c('#fb9a99','#a6cee3','#1f78b4','#b2df8a','#33a02c')) +
          facet_grid(Scenario ~ .))
  
  
  pchange <- c(0,mean(pcE26HadGEM15allPT[[i]]),mean(pcE26IPSL15allPT[[i]]),mean(pcE26MIROC15allPT[[i]]),mean(pcE26NorESM15allPT[[i]]),
               0,mean(pcE26HadGEM20allPT[[i]]),mean(pcE26IPSL20allPT[[i]]),mean(pcE26MIROC20allPT[[i]]),0,
               mean(pcE85GFDL15allPT[[i]]),mean(pcE85HadGEM15allPT[[i]]),mean(pcE85IPSL15allPT[[i]]),mean(pcE85MIROC15allPT[[i]]),mean(pcE85NorESM15allPT[[i]]),
               mean(pcE85GFDL20allPT[[i]]),mean(pcE85HadGEM20allPT[[i]]),mean(pcE85IPSL20allPT[[i]]),mean(pcE85MIROC20allPT[[i]]),mean(pcE85NorESM20allPT[[i]]),
               mean(pcE85GFDL30allPT[[i]]),mean(pcE85HadGEM30allPT[[i]]),mean(pcE85IPSL30allPT[[i]]),mean(pcE85MIROC30allPT[[i]]),mean(pcE85NorESM30allPT[[i]]))
  
  sds <- c(NA,sd(pcE26HadGEM15allPT[[i]]),sd(pcE26IPSL15allPT[[i]]),sd(pcE26MIROC15allPT[[i]]),sd(pcE26NorESM15allPT[[i]]),
           NA,sd(pcE26HadGEM20allPT[[i]]),sd(pcE26IPSL20allPT[[i]]),sd(pcE26MIROC20allPT[[i]]),NA,
           sd(pcE85GFDL15allPT[[i]]),sd(pcE85HadGEM15allPT[[i]]),sd(pcE85IPSL15allPT[[i]]),sd(pcE85MIROC15allPT[[i]]),sd(pcE85NorESM15allPT[[i]]),
           sd(pcE85GFDL20allPT[[i]]),sd(pcE85HadGEM20allPT[[i]]),sd(pcE85IPSL20allPT[[i]]),sd(pcE85MIROC20allPT[[i]]),sd(pcE85NorESM20allPT[[i]]),
           sd(pcE85GFDL30allPT[[i]]),sd(pcE85HadGEM30allPT[[i]]),sd(pcE85IPSL30allPT[[i]]),sd(pcE85MIROC30allPT[[i]]),sd(pcE85NorESM30allPT[[i]]))
  
  
  pcdata <- data.frame(pchange,Scenario, Model,Threshold,sds)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange, fill=Model)) + geom_bar(position="dodge", stat="identity") +
          geom_errorbar(aes(x=Threshold, ymin=pchange-sds, ymax=pchange+sds), width=.2, position=position_dodge(.9)) +
          xlab('Temperature Threshold (degC)') + ylab(expression('Percent Change (%)')) + ggtitle('Change in Electricity Use')+
          theme_light(base_size=14) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
          scale_fill_manual(values=c('#fb9a99','#a6cee3','#1f78b4','#b2df8a','#33a02c')) +
          facet_grid(Scenario ~ .))
  
  
}

# percent change per city (medians/quantiles)

for (i in 1:6) {
  
  pchange <- c(0,median(pcW26HadGEM15allPT[[i]]),median(pcW26IPSL15allPT[[i]]),median(pcW26MIROC15allPT[[i]]),median(pcW26NorESM15allPT[[i]]),
               0,median(pcW26HadGEM20allPT[[i]]),median(pcW26IPSL20allPT[[i]]),median(pcW26MIROC20allPT[[i]]),0,
               median(pcW85GFDL15allPT[[i]]),median(pcW85HadGEM15allPT[[i]]),median(pcW85IPSL15allPT[[i]]),median(pcW85MIROC15allPT[[i]]),median(pcW85NorESM15allPT[[i]]),
               median(pcW85GFDL20allPT[[i]]),median(pcW85HadGEM20allPT[[i]]),median(pcW85IPSL20allPT[[i]]),median(pcW85MIROC20allPT[[i]]),median(pcW85NorESM20allPT[[i]]),
               median(pcW85GFDL30allPT[[i]]),median(pcW85HadGEM30allPT[[i]]),median(pcW85IPSL30allPT[[i]]),median(pcW85MIROC30allPT[[i]]),median(pcW85NorESM30allPT[[i]]))
  
  quant25 <- c(NA,quantile(pcW26HadGEM15allPT[[i]],0.25),quantile(pcW26IPSL15allPT[[i]],0.25),quantile(pcW26MIROC15allPT[[i]],0.25),quantile(pcW26NorESM15allPT[[i]],0.25),
               NA,quantile(pcW26HadGEM20allPT[[i]],0.25),quantile(pcW26IPSL20allPT[[i]],0.25),quantile(pcW26MIROC20allPT[[i]],0.25),NA,
               quantile(pcW85GFDL15allPT[[i]],0.25),quantile(pcW85HadGEM15allPT[[i]],0.25),quantile(pcW85IPSL15allPT[[i]],0.25),quantile(pcW85MIROC15allPT[[i]],0.25),quantile(pcW85NorESM15allPT[[i]],0.25),
               quantile(pcW85GFDL20allPT[[i]],0.25),quantile(pcW85HadGEM20allPT[[i]],0.25),quantile(pcW85IPSL20allPT[[i]],0.25),quantile(pcW85MIROC20allPT[[i]],0.25),quantile(pcW85NorESM20allPT[[i]],0.25),
               quantile(pcW85GFDL30allPT[[i]],0.25),quantile(pcW85HadGEM30allPT[[i]],0.25),quantile(pcW85IPSL30allPT[[i]],0.25),quantile(pcW85MIROC30allPT[[i]],0.25),quantile(pcW85NorESM30allPT[[i]],0.25))
  
  quant75 <- c(NA,quantile(pcW26HadGEM15allPT[[i]],0.75),quantile(pcW26IPSL15allPT[[i]],0.75),quantile(pcW26MIROC15allPT[[i]],0.75),quantile(pcW26NorESM15allPT[[i]],0.75),
               NA,quantile(pcW26HadGEM20allPT[[i]],0.75),quantile(pcW26IPSL20allPT[[i]],0.75),quantile(pcW26MIROC20allPT[[i]],0.75),NA,
               quantile(pcW85GFDL15allPT[[i]],0.75),quantile(pcW85HadGEM15allPT[[i]],0.75),quantile(pcW85IPSL15allPT[[i]],0.75),quantile(pcW85MIROC15allPT[[i]],0.75),quantile(pcW85NorESM15allPT[[i]],0.75),
               quantile(pcW85GFDL20allPT[[i]],0.75),quantile(pcW85HadGEM20allPT[[i]],0.75),quantile(pcW85IPSL20allPT[[i]],0.75),quantile(pcW85MIROC20allPT[[i]],0.75),quantile(pcW85NorESM20allPT[[i]],0.75),
               quantile(pcW85GFDL30allPT[[i]],0.75),quantile(pcW85HadGEM30allPT[[i]],0.75),quantile(pcW85IPSL30allPT[[i]],0.75),quantile(pcW85MIROC30allPT[[i]],0.75),quantile(pcW85NorESM30allPT[[i]],0.75))
  
  
  Scenario <- c("RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP2.6",
                "RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP2.6",
                "RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5",
                "RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5",
                "RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5")
  
  Threshold <- c("1.5","1.5","1.5","1.5","1.5",
                 "2.0","2.0","2.0","2.0","2.0",
                 "1.5","1.5","1.5","1.5","1.5",
                 "2.0","2.0","2.0","2.0","2.0",
                 "3.0","3.0","3.0","3.0","3.0")
  
  Model <- c("GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M")
  
  pcdata <- data.frame(pchange,Scenario, Model,Threshold,quant25,quant75)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange, fill=Model)) + geom_bar(position="dodge", stat="identity") +
          geom_errorbar(aes(x=Threshold, ymin=quant25, ymax=quant75), width=.2, position=position_dodge(.9)) +
          xlab('Temperature Threshold (degC)') + ylab(expression('Percent Change (%)')) + ggtitle('Change in Water Use')+
          theme_light(base_size=14) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
          scale_fill_manual(values=c('#fb9a99','#a6cee3','#1f78b4','#b2df8a','#33a02c')) +
          facet_grid(Scenario ~ .))
  
  
  pchange <- c(0,median(pcE26HadGEM15allPT[[i]]),median(pcE26IPSL15allPT[[i]]),median(pcE26MIROC15allPT[[i]]),median(pcE26NorESM15allPT[[i]]),
               0,median(pcE26HadGEM20allPT[[i]]),median(pcE26IPSL20allPT[[i]]),median(pcE26MIROC20allPT[[i]]),0,
               median(pcE85GFDL15allPT[[i]]),median(pcE85HadGEM15allPT[[i]]),median(pcE85IPSL15allPT[[i]]),median(pcE85MIROC15allPT[[i]]),median(pcE85NorESM15allPT[[i]]),
               median(pcE85GFDL20allPT[[i]]),median(pcE85HadGEM20allPT[[i]]),median(pcE85IPSL20allPT[[i]]),median(pcE85MIROC20allPT[[i]]),median(pcE85NorESM20allPT[[i]]),
               median(pcE85GFDL30allPT[[i]]),median(pcE85HadGEM30allPT[[i]]),median(pcE85IPSL30allPT[[i]]),median(pcE85MIROC30allPT[[i]]),median(pcE85NorESM30allPT[[i]]))
  
  quant25 <- c(NA,quantile(pcE26HadGEM15allPT[[i]],0.25),quantile(pcE26IPSL15allPT[[i]],0.25),quantile(pcE26MIROC15allPT[[i]],0.25),quantile(pcE26NorESM15allPT[[i]],0.25),
               NA,quantile(pcE26HadGEM20allPT[[i]],0.25),quantile(pcE26IPSL20allPT[[i]],0.25),quantile(pcE26MIROC20allPT[[i]],0.25),NA,
               quantile(pcE85GFDL15allPT[[i]],0.25),quantile(pcE85HadGEM15allPT[[i]],0.25),quantile(pcE85IPSL15allPT[[i]],0.25),quantile(pcE85MIROC15allPT[[i]],0.25),quantile(pcE85NorESM15allPT[[i]],0.25),
               quantile(pcE85GFDL20allPT[[i]],0.25),quantile(pcE85HadGEM20allPT[[i]],0.25),quantile(pcE85IPSL20allPT[[i]],0.25),quantile(pcE85MIROC20allPT[[i]],0.25),quantile(pcE85NorESM20allPT[[i]],0.25),
               quantile(pcE85GFDL30allPT[[i]],0.25),quantile(pcE85HadGEM30allPT[[i]],0.25),quantile(pcE85IPSL30allPT[[i]],0.25),quantile(pcE85MIROC30allPT[[i]],0.25),quantile(pcE85NorESM30allPT[[i]],0.25))
  
  quant75 <- c(NA,quantile(pcE26HadGEM15allPT[[i]],0.75),quantile(pcE26IPSL15allPT[[i]],0.75),quantile(pcE26MIROC15allPT[[i]],0.75),quantile(pcE26NorESM15allPT[[i]],0.75),
               NA,quantile(pcE26HadGEM20allPT[[i]],0.75),quantile(pcE26IPSL20allPT[[i]],0.75),quantile(pcE26MIROC20allPT[[i]],0.75),NA,
               quantile(pcE85GFDL15allPT[[i]],0.75),quantile(pcE85HadGEM15allPT[[i]],0.75),quantile(pcE85IPSL15allPT[[i]],0.75),quantile(pcE85MIROC15allPT[[i]],0.75),quantile(pcE85NorESM15allPT[[i]],0.75),
               quantile(pcE85GFDL20allPT[[i]],0.75),quantile(pcE85HadGEM20allPT[[i]],0.75),quantile(pcE85IPSL20allPT[[i]],0.75),quantile(pcE85MIROC20allPT[[i]],0.75),quantile(pcE85NorESM20allPT[[i]],0.75),
               quantile(pcE85GFDL30allPT[[i]],0.75),quantile(pcE85HadGEM30allPT[[i]],0.75),quantile(pcE85IPSL30allPT[[i]],0.75),quantile(pcE85MIROC30allPT[[i]],0.75),quantile(pcE85NorESM30allPT[[i]],0.75))
  
  
  pcdata <- data.frame(pchange,Scenario, Model,Threshold,quant25,quant75)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange, fill=Model)) + geom_bar(position="dodge", stat="identity") +
          geom_errorbar(aes(x=Threshold, ymin=quant25, ymax=quant75), width=.2, position=position_dodge(.9)) +
          xlab('Temperature Threshold (degC)') + ylab(expression('Percent Change (%)')) + ggtitle('Change in Electricity Use')+
          theme_light(base_size=14) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
          scale_fill_manual(values=c('#fb9a99','#a6cee3','#1f78b4','#b2df8a','#33a02c')) +
          facet_grid(Scenario ~ .))
  
  
} 


#################################   MODEL COMPARISON ###########################

dates <- seq(as.Date("2007-01-01"), as.Date("2016-12-31"), by="months")

# water
for (c in 1:6) {
  
  lim <- mawaterPT[[c]][[1]][433:552]
  ful <- mawater[[c]][[1]][433:552]
  act <- mawaterACT[[c]]
  
  comdata <- data.frame(lim, ful, act,dates)
  
  print(ggplot(comdata) + geom_line(aes(x=dates,y=lim,color='PrecipTemp'))+
          geom_line(aes(x=dates,y=ful,color='SelectedFeature')) +
          geom_line(aes(x=dates,y=act,color='Actual')) +
          scale_color_manual(name='Model',values = c(PrecipTemp = 'red',SelectedFeature = 'blue',Actual='black'))+
          xlab('Date') + ylab('Water Use (L/capita)') + theme_light(base_size=20))
  
}

# electricity
for (c in 1:6) {
  
  lim <- maelecPT[[c]][[1]][433:552]
  ful <- maelec[[c]][[1]][433:552]
  act <- maelecACT[[c]]
  
  comdata <- data.frame(lim, ful, act,dates)
  
  print(ggplot(comdata) + geom_line(aes(x=dates,y=lim,color='Limited'))+
          geom_line(aes(x=dates,y=ful,color='Full')) +
          geom_line(aes(x=dates,y=act,color='Actual')) +
          scale_color_manual(name='Model',values = c(Limited = 'red',Full = 'blue',Actual='black'))+
          xlab('Date') + ylab('Electricity Use (MWh/capita)') + theme_light(base_size=20))
  
}

# water
means <- c(mean(mawaterPT[[1]][[1]][433:552]),mean(mawater[[1]][[1]][433:552]),mean(mawaterACT[[1]]),
           mean(mawaterPT[[2]][[1]][433:552]),mean(mawater[[2]][[1]][433:552]),mean(mawaterACT[[2]]),
           mean(mawaterPT[[3]][[1]][433:552]),mean(mawater[[3]][[1]][433:552]),mean(mawaterACT[[3]]),
           mean(mawaterPT[[4]][[1]][433:552]),mean(mawater[[4]][[1]][433:552]),mean(mawaterACT[[4]]),
           mean(mawaterPT[[5]][[1]][433:552]),mean(mawater[[5]][[1]][433:552]),mean(mawaterACT[[5]]),
           mean(mawaterPT[[6]][[1]][433:552]),mean(mawater[[6]][[1]][433:552]),mean(mawaterACT[[6]]))

sds <- c(sd(mawaterPT[[1]][[1]][433:552]),sd(mawater[[1]][[1]][433:552]),sd(mawaterACT[[1]]),
         sd(mawaterPT[[2]][[1]][433:552]),sd(mawater[[2]][[1]][433:552]),sd(mawaterACT[[2]]),
         sd(mawaterPT[[3]][[1]][433:552]),sd(mawater[[3]][[1]][433:552]),sd(mawaterACT[[3]]),
         sd(mawaterPT[[4]][[1]][433:552]),sd(mawater[[4]][[1]][433:552]),sd(mawaterACT[[4]]),
         sd(mawaterPT[[5]][[1]][433:552]),sd(mawater[[5]][[1]][433:552]),sd(mawaterACT[[5]]),
         sd(mawaterPT[[6]][[1]][433:552]),sd(mawater[[6]][[1]][433:552]),sd(mawaterACT[[6]]))

Model <- c("Precip-Temp","Selected Feature","Actual Data",
           "Precip-Temp","Selected Feature","Actual Data",
           "Precip-Temp","Selected Feature","Actual Data",
           "Precip-Temp","Selected Feature","Actual Data",
           "Precip-Temp","Selected Feature","Actual Data",
           "Precip-Temp","Selected Feature","Actual Data")

city <- c("Chicago","Chicago","Chicago",
          "Cleveland","Cleveland","Cleveland",
          "Columbus","Columbus","Columbus",
          "Indianapolis","Indianapolis","Indianapolis",
          "Madison","Madison","Madison",
          "Minneapolis","Minneapolis","Minneapolis")

comdata <- data.frame(means,sds,Model, city)

ggplot(comdata, aes(x=city, y=means, fill=Model)) + geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(x=city, ymin=means-sds, ymax=means+sds), width=.2, position=position_dodge(.9)) +
  xlab('City') + ylab('Water Use (L/capita)') +
  theme_light(base_size=14) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_fill_manual(values=c('black','red','blue')) 

# electricity
means <- c(mean(maelecPT[[1]][[1]][433:552]),mean(maelec[[1]][[1]][433:552]),mean(maelecACT[[1]]),
           mean(maelecPT[[2]][[1]][433:552]),mean(maelec[[2]][[1]][433:552]),mean(maelecACT[[2]]),
           mean(maelecPT[[3]][[1]][433:552]),mean(maelec[[3]][[1]][433:552]),mean(maelecACT[[3]]),
           mean(maelecPT[[4]][[1]][433:552]),mean(maelec[[4]][[1]][433:552]),mean(maelecACT[[4]]),
           mean(maelecPT[[5]][[1]][433:552]),mean(maelec[[5]][[1]][433:552]),mean(maelecACT[[5]]),
           mean(maelecPT[[6]][[1]][433:552]),mean(maelec[[6]][[1]][433:552]),mean(maelecACT[[6]]))

sds <- c(sd(maelecPT[[1]][[1]][433:552]),sd(maelec[[1]][[1]][433:552]),sd(maelecACT[[1]]),
         sd(maelecPT[[2]][[1]][433:552]),sd(maelec[[2]][[1]][433:552]),sd(maelecACT[[2]]),
         sd(maelecPT[[3]][[1]][433:552]),sd(maelec[[3]][[1]][433:552]),sd(maelecACT[[3]]),
         sd(maelecPT[[4]][[1]][433:552]),sd(maelec[[4]][[1]][433:552]),sd(maelecACT[[4]]),
         sd(maelecPT[[5]][[1]][433:552]),sd(maelec[[5]][[1]][433:552]),sd(maelecACT[[5]]),
         sd(maelecPT[[6]][[1]][433:552]),sd(maelec[[6]][[1]][433:552]),sd(maelecACT[[6]]))

Model <- c("Precip-Temp","Selected Feature","Actual Data",
           "Precip-Temp","Selected Feature","Actual Data",
           "Precip-Temp","Selected Feature","Actual Data",
           "Precip-Temp","Selected Feature","Actual Data",
           "Precip-Temp","Selected Feature","Actual Data",
           "Precip-Temp","Selected Feature","Actual Data")

city <- c("Chicago","Chicago","Chicago",
          "Cleveland","Cleveland","Cleveland",
          "Columbus","Columbus","Columbus",
          "Indianapolis","Indianapolis","Indianapolis",
          "Madison","Madison","Madison",
          "Minneapolis","Minneapolis","Minneapolis")

comdata <- data.frame(means,sds,Model, city)

ggplot(comdata, aes(x=city, y=means, fill=Model)) + geom_bar(position="dodge", stat="identity") +
        geom_errorbar(aes(x=city, ymin=means-sds, ymax=means+sds), width=.2, position=position_dodge(.9)) +
        xlab('City') + ylab('Electricity Use (MWH/capita)') +
        theme_light(base_size=14) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
        scale_fill_manual(values=c('black','red','blue')) 





#################################   SUMMER/WINTER COMPARISON - INTITAL RUN ###########################

# create a new dataframe per season for all cities
summerdata <- rbind(cities[[1]][which(cities[[1]][,2] >= 6 & cities[[1]][,2] <= 9),],cities[[2]][which(cities[[2]][,2] >= 6 & cities[[2]][,2] <= 9),],
                    cities[[3]][which(cities[[3]][,2] >= 6 & cities[[3]][,2] <= 9),],cities[[4]][which(cities[[4]][,2] >= 6 & cities[[4]][,2] <= 9),],
                    cities[[5]][which(cities[[5]][,2] >= 6 & cities[[5]][,2] <= 9),],cities[[6]][which(cities[[6]][,2] >= 6 & cities[[6]][,2] <= 9),])

winterdata <- rbind(cities[[1]][which(cities[[1]][,2] >= 12 | cities[[1]][,2] <= 3),],cities[[2]][which(cities[[2]][,2] >= 12 | cities[[2]][,2] <= 3),],
                    cities[[3]][which(cities[[3]][,2] >= 12 | cities[[3]][,2] <= 3),],cities[[4]][which(cities[[4]][,2] >= 12 | cities[[4]][,2] <= 3),],
                    cities[[5]][which(cities[[5]][,2] >= 12 | cities[[5]][,2] <= 3),],cities[[6]][which(cities[[6]][,2] >= 12 | cities[[6]][,2] <= 3),])

intermdata <- rbind(cities[[1]][which(cities[[1]][,2] == 4 | cities[[1]][,2] == 5 | cities[[1]][,2] == 10 | cities[[1]][,2] == 11),],
                    cities[[2]][which(cities[[2]][,2] == 4 | cities[[1]][,2] == 5 | cities[[2]][,2] == 10 | cities[[2]][,2] == 11),],
                    cities[[3]][which(cities[[3]][,2] == 4 | cities[[3]][,2] == 5 | cities[[3]][,2] == 10 | cities[[3]][,2] == 11),],
                    cities[[4]][which(cities[[4]][,2] == 4 | cities[[4]][,2] == 5 | cities[[4]][,2] == 10 | cities[[4]][,2] == 11),],
                    cities[[5]][which(cities[[5]][,2] == 4 | cities[[5]][,2] == 5 | cities[[5]][,2] == 10 | cities[[5]][,2] == 11),],
                    cities[[6]][which(cities[[6]][,2] == 4 | cities[[6]][,2] == 5 | cities[[6]][,2] == 10 | cities[[6]][,2] == 11),])

City <- c(rep("Chicago",40),rep("Cleveland",40),rep("Columbus",40),rep("Indianapolis",40),rep("Madison",40),rep("Minneapolis",40))

summer <- cbind(City, summerdata)
winter <- cbind(City, winterdata)
interm <- cbind(City, intermdata)

seasons <- (list(summer,winter,interm))

# Cross validation

k <- 5
n <- nrow(summer)

for (s in 1:3) {
  set.seed(12)
  seasons[[s]] <- seasons[[s]][sample(n),] 
}

folds <- cut(seq(1,n),breaks = k, labels = FALSE)

# initialize
avgyhatS <- list()
avgrsqS <- list()
avgRMSE1S <- list()
avgRMSE2S <- list()
avgrelinfS <- list()
avgactYsS <- list()

# loop through each season
for (s in 1:3){
  
  # initialize variables
  yhatS <- list()
  rsqS <- list()
  RMSE1S <- list()
  RMSE2S <- list()
  relinfS <- list()
  actYsS <- list()
  
  for (j in 1:k) {
    testIndex <- which(folds == j, arr.ind = TRUE)
    testData <- seasons[[s]][testIndex,]
    trainData <- seasons[[s]][-testIndex,]
    
    scaledtest <- scale(testData[,4:5])
    
    # run model
    Y <- trainData[,4:5] # response
    X <- trainData[,c(6,7,9,11,12,13)] # exclude max RH, MAX WS, & MIN DBT
    Ys <- scale(Y)
    
    out <- mvtb(Y=Ys,X=X, n.trees=1000, shrinkage=.01, interaction.depth=3)
    
    # fit model
    yhat <- predict(out,newdata=testData[,c(6,7,9,11,12,13)])
    
    yhatS[[j]] <- t(apply(yhat, 1, function(r)r*attr(Ys,'scaled:scale') + attr(Ys, 'scaled:center')))
    
    # measures of error
    rsqS[[j]] <- var(yhat)/var(scaledtest)
    RMSE1S[[j]] <- sqrt(sum((yhat[,1]-scaledtest[,1])^2)/nrow(testData)) # calculate RMSE for water use
    RMSE2S[[j]] <- sqrt(sum((yhat[,2]-scaledtest[,2])^2)/nrow(testData)) # calculate RMSE for electricity use
    
    actYsS[[j]] <- testData[,4:5]
    
    relinfS[[j]] <- mvtb.ri(out, relative = 'col')

  }
  
  avgyhatS[[s]] <- (yhatS[[1]]+yhatS[[2]]+yhatS[[3]]+yhatS[[4]]+yhatS[[5]])/5
  avgrsqS[[s]] <- (rsqS[[1]]+rsqS[[2]]+rsqS[[3]]+rsqS[[4]]+rsqS[[5]])/5
  avgRMSE1S[[s]] <- (RMSE1S[[1]]+RMSE1S[[2]]+RMSE1S[[3]]+RMSE1S[[4]]+RMSE1S[[5]])/5
  avgRMSE2S[[s]] <- (RMSE2S[[1]]+RMSE2S[[2]]+RMSE2S[[3]]+RMSE2S[[4]]+RMSE2S[[5]])/5
  avgrelinfS[[s]] <- (relinfS[[1]]+relinfS[[2]]+relinfS[[3]]+relinfS[[4]]+relinfS[[5]])/5
  avgactYsS[[s]] <- (actYsS[[1]]+actYsS[[2]]+actYsS[[3]]+actYsS[[4]]+actYsS[[5]])/5
}


#################################   SUMMER/WINTER COMPARISON - VARIABLE SELECTION ##################### 

seldata <- list()

for (s in 1:3){
  
  numformat <- function(val){sub("^(-?)0.", "\\1.", sprintf("%5.2f", val))}
  blues <- c('#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c','#08306b')
  bgr <- colorRampPalette(blues,space = "Lab",bias = 7)
  par(mar=c(12,10,1,1),mfrow=c(1,1),cex=0.9)
  mvtb.heat(t(avgrelinfS[[s]]),clust.method = NULL,cexRow=1,cexCol=1,numformat=numformat, col = blues)
  
  # variable selction
  # criterion = any variable > 5% of relative influence on either response
  impvar <- (rowSums(avgrelinfS[[s]]>5))>0 
  names <- row.names(data.frame(impvar[which(impvar==T)]))
  
  print(names)
  
  preds <- seasons[[s]][,names]
  seldata[[s]] <- cbind(seasons[[s]][,c(1:5)],preds)
}


#################################   SUMMER/WINTER COMPARISON - SELECTED FEATURE ###########################


# initialize
avgyhatSfs <- list()
avgrsqSfs <- list()
avgRMSE1Sfs <- list()
avgRMSE2Sfs <- list()
avgrelinfSfs <- list()

# loop through each season
for (s in 1:3){
  
  # initialize variables
  yhatSfs <- list()
  rsqSfs <- list()
  RMSE1Sfs <- list()
  RMSE2Sfs <- list()
  relinfSfs <- list()
  actYsSfs <- list()
  
  for (j in 1:k) {
    testIndex <- which(folds == j, arr.ind = TRUE)
    testData <- seldata[[s]][testIndex,]
    trainData <- seldata[[s]][-testIndex,]
    
    scaledtest <- scale(testData[,4:5])
    
    # run model
    Y <- trainData[,4:5] # response
    X <- trainData[,c(6:ncol(seldata[[s]]))] 
    Ys <- scale(Y)

    out <- mvtb(Y=Ys,X=X, n.trees=1000, shrinkage=.01, interaction.depth=3)
    
    # fit model
    yhat <- predict(out,newdata=testData[,c(6:ncol(trainData))])
    
    yhatSfs[[j]] <- t(apply(yhat, 1, function(r)r*attr(Ys,'scaled:scale') + attr(Ys, 'scaled:center')))
    
    # measures of error
    rsqSfs[[j]] <- var(yhat)/var(scaledtest)
    RMSE1Sfs[[j]] <- sqrt(sum((yhat[,1]-scaledtest[,1])^2)/nrow(testData)) # calculate RMSE for water use
    RMSE2Sfs[[j]] <- sqrt(sum((yhat[,2]-scaledtest[,2])^2)/nrow(testData)) # calculate RMSE for electricity use
    
    actYsSfs[[j]] <- testData[,4:5]
    
    relinfSfs[[j]] <- mvtb.ri(out, relative = 'col')
    
  }
  
  # Partial Dependence
  par(mfrow=c(1,2),mar=c(5,5,4,1))
  plot(out,predictor.no=4,response.no=1,xlab = 'Mean Wind Speed (m/s)',ylab = 'Water Use (scaled)')
  plot(out,predictor.no=1,response.no=2,xlab = 'Max. Dry Bulb Temp. (degC)',ylab = 'Electricity Use (scaled)')
  
  avgyhatSfs[[s]] <- (yhatSfs[[1]]+yhatSfs[[2]]+yhatSfs[[3]]+yhatSfs[[4]]+yhatSfs[[5]])/5
  avgrsqSfs[[s]] <- (rsqSfs[[1]]+rsqSfs[[2]]+rsqSfs[[3]]+rsqSfs[[4]]+rsqSfs[[5]])/5
  avgRMSE1Sfs[[s]] <- (RMSE1Sfs[[1]]+RMSE1Sfs[[2]]+RMSE1Sfs[[3]]+RMSE1Sfs[[4]]+RMSE1Sfs[[5]])/5
  avgRMSE2Sfs[[s]] <- (RMSE2Sfs[[1]]+RMSE2Sfs[[2]]+RMSE2Sfs[[3]]+RMSE2Sfs[[4]]+RMSE2Sfs[[5]])/5
  avgrelinfSfs[[s]] <- (relinfSfs[[1]]+relinfSfs[[2]]+relinfSfs[[3]]+relinfSfs[[4]]+relinfSfs[[5]])/5
}

#################################   SUMMER/WINTER COMPARISON - PRECIP & TEMP ###########################

# precipitation and temperature only 

# initialize
avgyhatSpt <- list()
avgrsqSpt <- list()
avgRMSE1Spt <- list()
avgRMSE2Spt <- list()
avgrelinfSpt <- list()

# loop through each season
for (s in 1:3){
  
  # initialize variables
  yhatSpt <- list()
  rsqSpt <- list()
  RMSE1Spt <- list()
  RMSE2Spt <- list()
  relinfSpt <- list()
  actYsSpt <- list()
  
  for (j in 1:k) {
    testIndex <- which(folds == j, arr.ind = TRUE)
    testData <- seasons[[s]][testIndex,]
    trainData <- seasons[[s]][-testIndex,]
    
    scaledtest <- scale(testData[,4:5])
    
    Y <- trainData[,4:5] # response
    X <- trainData[,c(6,12)] 
    Ys <- scale(Y)
    
    # run model
    out <- mvtb(Y=Ys,X=X, n.trees=1000, shrinkage=.01, interaction.depth=3)
    
    # fit model
    yhat <- predict(out,newdata=testData[,c(6,12)])
    yhatSpt[[j]] <- t(apply(yhat, 1, function(r)r*attr(Ys,'scaled:scale') + attr(Ys, 'scaled:center')))
    
    # measures of error
    rsqSpt[[j]] <- var(yhat)/var(scaledtest)
    RMSE1Spt[[j]] <- sqrt(sum((yhat[,1]-scaledtest[,1])^2)/nrow(testData)) # calculate RMSE for water use
    RMSE2Spt[[j]] <- sqrt(sum((yhat[,2]-scaledtest[,2])^2)/nrow(testData)) # calculate RMSE for electricity use
    
    actYsSpt[[j]] <- testData[,4:5]
    
    relinfSpt[[j]] <- mvtb.ri(out, relative = 'col')
    
  }
  
  avgyhatSpt[[s]] <- (yhatSpt[[1]]+yhatSpt[[2]]+yhatSpt[[3]]+yhatSpt[[4]]+yhatSpt[[5]])/5
  avgrsqSpt[[s]] <- (rsqSpt[[1]]+rsqSpt[[2]]+rsqSpt[[3]]+rsqSpt[[4]]+rsqSpt[[5]])/5
  avgRMSE1Spt[[s]] <- (RMSE1Spt[[1]]+RMSE1Spt[[2]]+RMSE1Spt[[3]]+RMSE1Spt[[4]]+RMSE1Spt[[5]])/5
  avgRMSE2Spt[[s]] <- (RMSE2Spt[[1]]+RMSE2Spt[[2]]+RMSE2Spt[[3]]+RMSE2Spt[[4]]+RMSE2Spt[[5]])/5
  avgrelinfSpt[[s]] <- (relinfSpt[[1]]+relinfSpt[[2]]+relinfSpt[[3]]+relinfSpt[[4]]+relinfSpt[[5]])/5
}




#################################   SUMMER/WINTER COMPARISON - FIGURES ###########################

# water use "time series"

for (s in 1:3) {
  
  alldata <- c(avgyhatS[[s]][,1],avgyhatSpt[[s]][,1],avgyhatSfs[[s]][,1],avgactYsS[[s]][,1]) # ,avgyhatSfs2[[s]][,1])
  
  Model <- c(rep("All Variables",48),rep("Precip-Temp",48),rep("Selected Feature 1",48),rep("Actual Data",48)) # ,rep("Selected Feature 2",48))
  
  
  x <- rep(seq(1,48),4) # ,5)
  
  comdata <- data.frame(alldata,Model,x)
  
  print(ggplot() + geom_line(aes(x=x,y=alldata, color=Model))+
          ylab('Water Use (L/capita)') + theme_light(base_size=20) + 
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
          scale_color_manual(values = c('#fb9a99','#a6cee3','#1f78b4','#33a02c'))) # ,'#b2df8a')))
  
}

# electricity use "time series"

for (s in 1:3) {
  
  alldata <- c(avgyhatS[[s]][,2],avgyhatSpt[[s]][,2],avgyhatSfs[[s]][,2],avgactYsS[[s]][,2]) # , avgyhatSfs2[[s]][,2])
  
  Model <- c(rep("All Variables",48),rep("Precip-Temp",48),rep("Selected Feature 1",48),rep("Actual Data",48)) # ,rep("Selected Feature 2",48))
  
  
  x <- rep(seq(1,48),4) # ,5)
  
  comdata <- data.frame(alldata,Model,x)
  
  print(ggplot() + geom_line(aes(x=x,y=alldata, color=Model))+
          ylab('Electricity Use (MWh/capita)') + theme_light(base_size=20) + 
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
          scale_color_manual(values = c('#fb9a99','#a6cee3','#1f78b4','#33a02c'))) # ,'#b2df8a')))
  
}


# sorted water use "time series"

for (s in 1:3) {
  
  alldata <- c(sort(avgyhatSpt[[s]][,1]),sort(avgyhatSfs[[s]][,1]),sort(avgactYsS[[s]][,1])) 
  
  Model <- c(rep("Precip-Temp",48),rep("Selected Feature",48),rep("Actual Data",48)) 
  
  
  x <- rep(seq(1,48),3) 
  
  comdata <- data.frame(alldata,Model,x)
  
  print(ggplot() + geom_line(aes(x=x,y=alldata, color=Model),size=2)+
          ylab('Water Use (L/capita)') + theme_light(base_size=30) + 
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
          scale_color_manual(values = c('black','#6e99b4','#849e2a'))) 
  
}

# sorted electricity use "time series"

for (s in 1:3) {
  
  alldata <- c(sort(avgyhatSpt[[s]][,2]),sort(avgyhatSfs[[s]][,2]),sort(avgactYsS[[s]][,2])) 
  Model <- c(rep("Precip-Temp",48),rep("Selected Feature",48),rep("Actual Data",48)) 
  
  
  x <- rep(seq(1,48),3)
  
  comdata <- data.frame(alldata,Model,x)
  
  print(ggplot() + geom_line(aes(x=x,y=alldata, color=Model),size=2)+
          ylab('Electricity Use (MWh/capita)') + theme_light(base_size=30) + 
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
          scale_color_manual(values = c('black','#6e99b4','#849e2a'))) 
  
}


# mean +/- sd bar plots
means <- c(colMeans(avgyhatSpt[[1]]),colMeans(avgyhatSpt[[2]]),colMeans(avgyhatSpt[[3]]),
           colMeans(avgyhatSfs[[1]]),colMeans(avgyhatSfs[[2]]),colMeans(avgyhatSfs[[3]]),
           colMeans(avgactYsS[[1]]),colMeans(avgactYsS[[2]]),colMeans(avgactYsS[[3]]))

sds <- c(apply(avgyhatSpt[[1]],2,sd),apply(avgyhatSpt[[2]],2,sd),apply(avgyhatSpt[[3]],2,sd),
         apply(avgyhatSfs[[1]],2,sd),apply(avgyhatSfs[[2]],2,sd),apply(avgyhatSfs[[3]],2,sd),
         apply(avgactYsS[[1]],2,sd),apply(avgactYsS[[2]],2,sd),apply(avgactYsS[[3]],2,sd))

utilitylist <- c("Water Use (L/capita)","Electricity Use (MWh/capita)")
Utility <- c(rep(utilitylist,9))

Model <- c(rep("Precip-Temp",6),rep("Selected Feature",6),
           rep("Actual Data",6)) 

periodlist <- c(rep("Summer Months",2), rep("Winter Months",2),rep("Intermediate Months",2))
Period <- c(rep(periodlist,3))

plotdata <- data.frame(means,sds,Utility,Model,Period)

ggplot(plotdata, aes(x=Period, y=means, fill=Model)) + geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(x=Period, ymin=means-sds, ymax=means+sds), width=.2, position=position_dodge(.9)) +
  xlab('Time Period') + ylab('Mean Utility Consumption') +
  theme_light(base_size=18) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  facet_grid(Utility ~ ., scales='free') +
  scale_fill_manual(values = c('black','#fb9a99','#1f78b4'))

# RMSE bar plots
for (s in 1:3) {
  allrmse <- c(avgRMSE1Spt[[s]],avgRMSE2Spt[[s]],
               avgRMSE1Sfs[[s]],avgRMSE2Sfs[[s]])
  
  utilitylist <- c("Water","Electricity")
  Utility <- c(rep(utilitylist,2))
  
  Model <- c(rep("Precip-Temp",2),rep("Selected Feature",2))
  
  #periodlist <- c(rep("Summer Months",2), rep("Winter Months",2),rep("Intermediate Months",2))
  #Period <- c(rep(periodlist,2))
  
  plotdata <- data.frame(allrmse,Utility,Model) #Period
  
  print(ggplot(plotdata, aes(x=Utility, y=allrmse, fill=Model)) + geom_bar(position="dodge", stat="identity", color = 'black') +
          xlab('Utility') + ylab('RMSE') + coord_cartesian(ylim=c(.5,1.15)) +
          theme_light(base_size=30) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
          #facet_grid(Utility ~ ., scales='free') +
          scale_fill_manual(values = c('#6e99b4','#849e2a')))
}

# R^2 bar plots
for (s in 1:3) {
  allrsq <- c(avgrsqSpt[[s]][1,1],avgrsqSpt[[s]][2,2],
              avgrsqSfs[[s]][1,1],avgrsqSfs[[s]][2,2])
  
  utilitylist <- c("Water Use","Electricity Use")
  Utility <- c(rep(utilitylist,2))
  
  Model <- c(rep("Precip-Temp",2),rep("Selected Feature",2))
  
  #periodlist <- c(rep("Summer Months",2), rep("Winter Months",2),rep("Intermediate Months",2))
  #Period <- c(rep(periodlist,2))
  
  plotdata <- data.frame(allrsq,Utility,Model) #Period
  
  print(ggplot(plotdata, aes(x=Utility, y=allrsq, fill=Model)) + geom_bar(position="dodge", stat="identity") +
          xlab('Utility') + ylab('R-Squared') + coord_cartesian(ylim=c(.2,(max(allrsq)+.05))) +
          theme_light(base_size=18) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
          #facet_grid(Utility ~ ., scales='free') +
          scale_fill_manual(values = c('#fb9a99','#1f78b4')))
}



#################################   SUMMER/WINTER COMPARISON - ITERATIONS ######################

# store imporant variables from the different iterations

# ITERATION: ORIGINAL VARIABLES
withoutminFS1yhat <- avgyhatSfs
withoutminFS1rmse1 <- avgRMSE1Sfs
withoutminFS1rmse2 <- avgRMSE2Sfs
withoutminfS1rsq <- avgrsqSfs

# ITERATION: INCLUDE MIN DBT + ORIGINAL VARIABLES
withminFS1yhat <- avgyhatSfs
withminFS2yhat <- avgyhatSfs2
withminFS1rmse1 <- avgRMSE1Sfs
withminFS1rmse2 <- avgRMSE2Sfs
withminFS2rmse1 <- avgRMSE1Sfs2
withminFS2rmse2 <- avgRMSE2Sfs2
withminfS1rsq <- avgrsqSfs
withminfS2rsq <- avgrsqSfs2

# ITERATION: INCLUDE MIN DBT + ONLY AVG WS, RH
withoutmaxFS1yhat <- avgyhatSfs
withoutmaxFS1rmse1 <- avgRMSE1Sfs
withoutmaxFS1rmse2 <- avgRMSE2Sfs
withoutmaxfS1rsq <- avgrsqSfs

# ITERATION: INCLUDE ONLY AVG WS, RH & EXCLUDE MIN DBT
withoutmaxminFS1yhat <- avgyhatSfs
withoutmaxminFS1rmse1 <- avgRMSE1Sfs
withoutmaxminFS1rmse2 <- avgRMSE2Sfs
withoutmaxminfS1rsq <- avgrsqSfs

# manova tests

# summer
allyhats <- cbind(rbind(withoutminFS1yhat[[1]],withminFS1yhat[[1]],withoutmaxFS1yhat[[1]],withoutmaxminFS1yhat[[1]]),factor(rep(letters[1:4], each = 48)))
print(summary(manova(allyhats[,1:2] ~ allyhats[,3])))
summary.aov(manova(allyhats[,1:2] ~ allyhats[,3]))
# no significant difference

# winter
allyhats <- cbind(rbind(withoutminFS1yhat[[2]],withminFS1yhat[[2]],withoutmaxFS1yhat[[2]],withoutmaxminFS1yhat[[2]]),factor(rep(letters[1:4], each = 48)))
print(summary(manova(allyhats[,1:2] ~ allyhats[,3])))
summary.aov(manova(allyhats[,1:2] ~ allyhats[,3]))
# no significant difference

# intermediate
allyhats <- cbind(rbind(withoutminFS1yhat[[3]],withminFS1yhat[[3]],withoutmaxFS1yhat[[3]],withoutmaxminFS1yhat[[3]]),factor(rep(letters[1:4], each = 48)))
print(summary(manova(allyhats[,1:2] ~ allyhats[,3])))
summary.aov(manova(allyhats[,1:2] ~ allyhats[,3]))
# no significant difference

# k-s test
ks.test(withoutmaxminFS1yhat,withoutminFS1yhat)

# iteration comparison figures

# water use "time series"

for (s in 1:3) {
  
  alldata <- c(withoutminFS1yhat[[s]][,1],avgyhatSpt[[s]][,1],withminFS1yhat[[s]][,1],withoutmaxFS1yhat[[s]][,1],withoutmaxminFS1yhat[[s]][,1],avgactYsS[[s]][,1]) # ,avgyhatSfs2[[s]][,1])
  
  Model <- c(rep("Without Min Temp",48),rep("Precip-Temp",48),rep("With Min Temp",48),rep("Without Max WS & RH",48),rep("Without Min or Max",48),rep("Actual Data",48)) 
  
  
  x <- rep(seq(1,48),6)
  
  comdata <- data.frame(alldata,Model,x)
  
  print(ggplot() + geom_line(aes(x=x,y=alldata, color=Model))+
          ylab('Water Use (L/capita)') + theme_light(base_size=20) + 
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
          scale_color_manual(values = c('black','#fb9a99','#33a02c','#1f78b4','#a6cee3','#b2df8a')))
  
}

# electricity use "time series"

for (s in 1:3) {
  
  alldata <- c(withoutminFS1yhat[[s]][,2],avgyhatSpt[[s]][,2],withminFS1yhat[[s]][,2],withoutmaxFS1yhat[[s]][,2],withoutmaxminFS1yhat[[s]][,2],avgactYsS[[s]][,2]) 
  
  Model <- c(rep("Without Min Temp",48),rep("Precip-Temp",48),rep("With Min Temp",48),rep("Without Max WS & RH",48),rep("Without Min or Max",48),rep("Actual Data",48)) 
  
  
  x <- rep(seq(1,48),6)
  
  comdata <- data.frame(alldata,Model,x)
  
  print(ggplot() + geom_line(aes(x=x,y=alldata, color=Model))+
          ylab('Electricity Use (MWh/capita)') + theme_light(base_size=20) + 
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
          scale_color_manual(values = c('black','#fb9a99','#33a02c','#1f78b4','#a6cee3','#b2df8a')))
  
}


# sorted water use "time series"

for (s in 1:3) {
  
  alldata <- c(sort(withoutminFS1yhat[[s]][,1]),sort(avgyhatSpt[[s]][,1]),sort(withminFS1yhat[[s]][,1]),sort(withoutmaxFS1yhat[[s]][,1]),sort(withoutmaxminFS1yhat[[s]][,1]),sort(avgactYsS[[s]][,1])) 
  
  Model <- c(rep("Without Min Temp",48),rep("Precip-Temp",48),rep("With Min Temp",48),rep("Without Max WS & RH",48),rep("Without Min or Max",48),rep("Actual Data",48)) 
  
  
  x <- rep(seq(1,48),6)
  
  comdata <- data.frame(alldata,Model,x)
  
  print(ggplot() + geom_line(aes(x=x,y=alldata, color=Model))+
          ylab('Water Use (L/capita)') + theme_light(base_size=20) + 
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
          scale_color_manual(values = c('black','#fb9a99','#33a02c','#1f78b4','#a6cee3','#b2df8a')))
  
}

# sorted electricity use "time series"

for (s in 1:3) {
  
  alldata <- c(sort(withoutminFS1yhat[[s]][,2]),sort(avgyhatSpt[[s]][,2]),sort(withminFS1yhat[[s]][,2]),sort(withoutmaxFS1yhat[[s]][,2]),sort(withoutmaxminFS1yhat[[s]][,2]),sort(avgactYsS[[s]][,2])) 
  
  Model <- c(rep("Without Min Temp",48),rep("Precip-Temp",48),rep("With Min Temp",48),rep("Without Max WS & RH",48),rep("Without Min or Max",48),rep("Actual Data",48)) 
  
  
  x <- rep(seq(1,48),6)
  
  comdata <- data.frame(alldata,Model,x)
  
  print(ggplot() + geom_line(aes(x=x,y=alldata, color=Model))+
          ylab('Electricity Use (MWh/capita)') + theme_light(base_size=20) + 
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
          scale_color_manual(values = c('black','#fb9a99','#33a02c','#1f78b4','#a6cee3','#b2df8a')))
  
}


# mean +/- sd bar plots
means <- c(colMeans(withoutminFS1yhat[[1]]),colMeans(withoutminFS1yhat[[2]]),colMeans(withoutminFS1yhat[[3]]),
           colMeans(avgyhatSpt[[1]]),colMeans(avgyhatSpt[[2]]),colMeans(avgyhatSpt[[3]]),
           colMeans(withminFS1yhat[[1]]),colMeans(withminFS1yhat[[2]]),colMeans(withminFS1yhat[[3]]),
           colMeans(withoutmaxFS1yhat[[1]]),colMeans(withoutmaxFS1yhat[[2]]),colMeans(withoutmaxFS1yhat[[3]]),
           colMeans(withoutmaxminFS1yhat[[1]]),colMeans(withoutmaxminFS1yhat[[2]]),colMeans(withoutmaxminFS1yhat[[3]]),
           colMeans(avgactYsS[[1]]),colMeans(avgactYsS[[2]]),colMeans(avgactYsS[[3]]))

sds <- c(apply(withoutminFS1yhat[[1]],2,sd),apply(withoutminFS1yhat[[2]],2,sd),apply(withoutminFS1yhat[[3]],2,sd),
         apply(avgyhatSpt[[1]],2,sd),apply(avgyhatSpt[[2]],2,sd),apply(avgyhatSpt[[3]],2,sd),
         apply(withminFS1yhat[[1]],2,sd),apply(withminFS1yhat[[2]],2,sd),apply(withminFS1yhat[[3]],2,sd),
         apply(withoutmaxFS1yhat[[1]],2,sd),apply(withoutmaxFS1yhat[[2]],2,sd),apply(withoutmaxFS1yhat[[3]],2,sd),
         apply(withoutmaxminFS1yhat[[1]],2,sd),apply(withoutmaxminFS1yhat[[2]],2,sd),apply(withoutmaxminFS1yhat[[3]],2,sd),
         apply(avgactYsS[[1]],2,sd),apply(avgactYsS[[2]],2,sd),apply(avgactYsS[[3]],2,sd))

utilitylist <- c("Water Use (L/capita)","Electricity Use (MWh/capita)")
Utility <- c(rep(utilitylist,18))

Model <- c(rep("Without Min Temp",6),rep("Precip-Temp",6),rep("With Min Temp",6),
           rep("Without Max WS & RH",6),rep("Without Min or Max",6),rep("Actual Data",6)) 


periodlist <- c(rep("Summer Months",2), rep("Winter Months",2),rep("Intermediate Months",2))
Period <- c(rep(periodlist,6))

plotdata <- data.frame(means,sds,Utility,Model,Period)

ggplot(plotdata, aes(x=Period, y=means, fill=Model)) + geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(x=Period, ymin=means-sds, ymax=means+sds), width=.2, position=position_dodge(.9)) +
  xlab('Time Period') + ylab('Mean Utility Consumption') +
  theme_light(base_size=18) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  facet_grid(Utility ~ ., scales='free') +
  scale_fill_manual(values = c('black','#fb9a99','#33a02c','#1f78b4','#a6cee3','#b2df8a'))

# RMSE bar plots
allrmse <- c(withoutminFS1rmse1[[1]],withoutminFS1rmse2[[1]],withoutminFS1rmse1[[2]],withoutminFS1rmse2[[2]],withoutminFS1rmse1[[3]],withoutminFS1rmse2[[3]],
             avgRMSE1Spt[[1]],avgRMSE2Spt[[1]],avgRMSE1Spt[[2]],avgRMSE2Spt[[2]],avgRMSE1Spt[[3]],avgRMSE2Spt[[3]],
             withminFS1rmse1[[1]],withminFS1rmse2[[1]],withminFS1rmse1[[2]],withminFS1rmse2[[2]],withminFS1rmse1[[3]],withminFS1rmse2[[3]],
             withoutmaxFS1rmse1[[1]],withoutmaxFS1rmse2[[1]],withoutmaxFS1rmse1[[2]],withoutmaxFS1rmse2[[2]],withoutmaxFS1rmse1[[3]],withoutmaxFS1rmse2[[3]],
             withoutmaxminFS1rmse1[[1]],withoutmaxminFS1rmse2[[1]],withoutmaxminFS1rmse1[[2]],withoutmaxminFS1rmse2[[2]],withoutmaxminFS1rmse1[[3]],withoutmaxminFS1rmse2[[3]])

utilitylist <- c("Water Use (L/capita)","Electricity Use (MWh/capita)")
Utility <- c(rep(utilitylist,15))

Model <- c(rep("Without Min Temp",6),rep("Precip-Temp",6),rep("With Min Temp",6),
           rep("Without Max WS & RH",6),rep("Without Min or Max",6)) 

periodlist <- c(rep("Summer Months",2), rep("Winter Months",2),rep("Intermediate Months",2))
Period <- c(rep(periodlist,5))

plotdata <- data.frame(allrmse,Utility,Model,Period)

ggplot(plotdata, aes(x=Period, y=allrmse, fill=Model)) + geom_bar(position="dodge", stat="identity") +
  xlab('Time Period') + ylab('RMSE') +
  theme_light(base_size=18) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  facet_grid(Utility ~ ., scales='free') +
  scale_fill_manual(values = c('#fb9a99','#33a02c','#1f78b4','#a6cee3','#b2df8a'))

# R^2 bar plots
allrsq <- c(withoutminfS1rsq[[1]][1,1],withoutminfS1rsq[[1]][2,2],withoutminfS1rsq[[2]][1,1],withoutminfS1rsq[[2]][2,2],avgrsqS[[3]][1,1],avgrsqS[[3]][2,2],
            avgrsqSpt[[1]][1,1],avgrsqSpt[[1]][2,2],avgrsqSpt[[2]][1,1],avgrsqSpt[[2]][2,2],avgrsqSpt[[3]][1,1],avgrsqSpt[[3]][2,2],
            withminfS1rsq[[1]][1,1],withminfS1rsq[[1]][2,2],withminfS1rsq[[2]][1,1],withminfS1rsq[[2]][2,2],withminfS1rsq[[3]][1,1],withminfS1rsq[[3]][2,2],
            withoutmaxfS1rsq[[1]][1,1],withoutmaxfS1rsq[[1]][2,2],withoutmaxfS1rsq[[2]][1,1],withoutmaxfS1rsq[[2]][2,2],withoutmaxfS1rsq[[3]][1,1],withoutmaxfS1rsq[[3]][2,2],
            withoutmaxminfS1rsq[[1]][1,1],withoutmaxminfS1rsq[[1]][2,2],withoutmaxminfS1rsq[[2]][1,1],withoutmaxminfS1rsq[[2]][2,2],withoutmaxminfS1rsq[[3]][1,1],withoutmaxminfS1rsq[[3]][2,2])

utilitylist <- c("Water Use (L/capita)","Electricity Use (MWh/capita)")
Utility <- c(rep(utilitylist,15))

Model <- c(rep("Without Min Temp",6),rep("Precip-Temp",6),rep("With Min Temp",6),
           rep("Without Max WS & RH",6),rep("Without Min or Max",6)) 

periodlist <- c(rep("Summer Months",2), rep("Winter Months",2),rep("Intermediate Months",2))
Period <- c(rep(periodlist,5))

plotdata <- data.frame(allrsq,Utility,Model,Period)

ggplot(plotdata, aes(x=Period, y=allrsq, fill=Model)) + geom_bar(position="dodge", stat="identity") +
  xlab('Time Period') + ylab('R-Squared') +
  theme_light(base_size=18) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  facet_grid(Utility ~ ., scales='free') +
  scale_fill_manual(values = c('#fb9a99','#33a02c','#1f78b4','#a6cee3','#b2df8a'))

#################################   SUMMER/WINTER COMPARISON - FUTURE PROJECTIONS ###########################

# add months column to future data
months <- rep(seq(1,12),129)

for (c in 1:6) {
  for (s in 1:4) {
    for (m in 1:5) {
      futdata[[c]][[s]][[m]][,2] <- months
      colnames(futdata[[c]][[s]][[m]])[colnames(futdata[[c]][[s]][[m]])=='ENSO'] <- 'Month'
    }
  }
}

# create a new dataframe per season for all cities

# initialize city variables
sumdatacit <- list()
windatacit <- list()
intdatacit <- list()

# loop through cities
for (c in 1:6) {
  # initialize scenario variables
  sumdatascn <- list()
  windatascn <- list()
  intdatascn <- list()
  
  # loop through scenarios
  for (s in 1:4) {
    # initialize model variables
    sumdatamod <- list()
    windatamod <- list()
    intdatamod <- list()
    
    # loop through models
    for (m in 1:5) {
      sumdatamod[[m]] <- futdata[[c]][[s]][[m]][which(futdata[[c]][[s]][[m]][,2] >= 6 & futdata[[c]][[s]][[m]][,2] <= 9),]
      windatamod[[m]] <- futdata[[c]][[s]][[m]][which(futdata[[c]][[s]][[m]][,2] >= 12 | futdata[[c]][[s]][[m]][,2] <= 3),]
      intdatamod[[m]] <- futdata[[c]][[s]][[m]][which(futdata[[c]][[s]][[m]][,2] == 4 | futdata[[c]][[s]][[m]][,2] == 5 | 
                                                        futdata[[c]][[s]][[m]][,2] == 10 | futdata[[c]][[s]][[m]][,2] == 11),]
    }
    # store variables
    sumdatascn[[s]] <- sumdatamod
    windatascn[[s]] <- windatamod
    intdatascn[[s]] <- intdatamod
  }
  # store variables
  sumdatacit[[c]] <- sumdatascn
  windatacit[[c]] <- windatascn
  intdatacit[[c]] <- intdatascn
}

# combine season-separated data 
cityseasons <- list(sumdatacit,windatacit,intdatacit)

# aggreagate data so there are no city disinctions
futureseasons <- list()
City <- c(rep("Chicago",516),rep("Cleveland",516),rep("Columbus",516),rep("Indianapolis",516),rep("Madison",516),rep("Minneapolis",516))

for (p in 1:3) {
  scendata <- list()
  for (s in 1:4) {
    moddata <- list()
    for (m in 1:5) {
      modseasons <- rbind(cityseasons[[p]][[1]][[s]][[m]],cityseasons[[p]][[2]][[s]][[m]],cityseasons[[p]][[3]][[s]][[m]],
                          cityseasons[[p]][[4]][[s]][[m]],cityseasons[[p]][[5]][[s]][[m]],cityseasons[[p]][[6]][[s]][[m]])
      moddata[[m]] <- cbind(City,modseasons)
    }
    scendata[[s]] <- moddata
  }
  futureseasons[[p]] <- scendata
}

# run the model

# initialize
yhatall <- list()

for (i in 1:3) {
  
  # run model
  Y <- seldata[[i]][,4:5] # response
  X <- seldata[[i]][,c(7,8,6,9,10)] 
  names(X) <- c('Dew Point','Relative Humidity','Dry Bulb Temp.','Wind Speed','Precipitation')
  names(Y) <- c('Water Use','Electricity Use')
  
  out <- mvtb(Y=Y,X=X, n.trees=1000, shrinkage=.01, interaction.depth=3)
  
  # get future projections for each scenario/model
  
  # initialize
  predscenarios <- list()
  
  for (s in 1:4) {
    # initialize
    predvalues <- list()
    for (m in 1:5) {
      # fit model
      yhat <- predict(out,newdata=futureseasons[[i]][[s]][[m]][,4:8])
      
      # store data
      predvalues[[m]] <- yhat
    }
    predscenarios[[s]] <- predvalues
  }
  
  yhatall[[i]] <- predscenarios
  
}

#################################   SUMMER/WINTER COMPARISON - PT PROJECTIONS ###########################

yhatallPT <- list()

for (i in 1:3) {
  
  # run model
  Y <- seldata[[i]][,4:5] # response
  X <- seldata[[i]][,c(6,10)]
  names(X) <- c('Dry Bulb Temp.','Precipitation')
  names(Y) <- c('Water Use','Electricity Use')
  
  outPT <- mvtb(Y=Y,X=X, n.trees=1000, shrinkage=.01, interaction.depth=3)
  
  # initialize
  predscenariosPT <- list()
  
  for (s in 1:4) {
    # initialize
    predvaluesPT <- list()
    for (m in 1:5) {
      # fit model
      yhatPT <- predict(outPT,newdata=futureseasons[[i]][[s]][[m]][,c(6,8)])
      
      # store data
      predvaluesPT[[m]] <- yhatPT
    }
    predscenariosPT[[s]] <- predvaluesPT
  }
  
  yhatallPT[[i]] <- predscenariosPT
  
}


#################################   SUMMER/WINTER COMPARISON - ANALYSIS ########################

# add date column to the yhat dataframes
for (p in 1:3) {
  datesscen <- list()
  datesscenPT <- list()
  for (s in 1:4) {
    datesmod <- list()
    datesmodPT <- list()
    for (m in 1:5) {
      datesmod[[m]] <- data.frame(as.Date(futureseasons[[p]][[s]][[m]][,2]),yhatall[[p]][[s]][[m]])
      datesmodPT[[m]] <- data.frame(as.Date(futureseasons[[p]][[s]][[m]][,2]),yhatallPT[[p]][[s]][[m]])
    }
    datesscen[[s]] <- datesmod
    datesscenPT[[s]] <- datesmodPT
  }
  yhatall[[p]] <- datesscen
  yhatallPT[[p]] <- datesscenPT
}


# aggregate projections

aggdata <- list()
aggdataPT <- list()
for (p in 1:3) {
  scenaggdata <- list()
  scenaggdataPT <- list()
  for (s in 1:4) {
    modaggdata <- list()
    modaggdataPT <- list()
    for (m in 1:5) {
      modaggdata[[m]] <- aggregate(yhatall[[p]][[s]][[m]][,2:3], by = list(yhatall[[p]][[s]][[m]][,1]),FUN = mean)
      modaggdataPT[[m]] <- aggregate(yhatallPT[[p]][[s]][[m]][,2:3], by = list(yhatallPT[[p]][[s]][[m]][,1]),FUN = mean)
    }
    scenaggdata[[s]] <- modaggdata
    scenaggdataPT[[s]] <- modaggdataPT
  }
  aggdata[[p]] <- scenaggdata
  aggdataPT[[p]] <- scenaggdataPT
}


# percent change between 1971-2000 and last 30 years when 1.5 deg threshold was reached (pos == % increase) [new-old/old]

indices <- list(c(145,264),c(149,268),c(141,260),c(305,424),c(201,320),c(133,252),c(141,260),c(141,260),c(181,300))

seaspc15w <- list()
seaspc15e <- list()
for (s in 1:3) {
  pcwater <- list()
  pcelect <- list()
  mc26 <- 2
  mc85 <- 1
  for (i in 1:9) {
    if (i <= 4) { # RCP2.6
      pcwater[[i]] <- ((aggdata[[s]][[4]][[mc26]][indices[[i]][1]:indices[[i]][2],2]-aggdata[[s]][[4]][[mc26]][1:120,2])/aggdata[[s]][[4]][[mc26]][1:120,2])*100
      pcelect[[i]] <- ((aggdata[[s]][[4]][[mc26]][indices[[i]][1]:indices[[i]][2],3]-aggdata[[s]][[4]][[mc26]][1:120,3])/aggdata[[s]][[4]][[mc26]][1:120,3])*100
      mc26 <- mc26 + 1
    } else {      # RCP8.5
      pcwater[[i]] <- ((aggdata[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],2]-aggdata[[s]][[1]][[mc85]][1:120,2])/aggdata[[s]][[1]][[mc85]][1:120,2])*100
      pcelect[[i]] <- ((aggdata[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],3]-aggdata[[s]][[1]][[mc85]][1:120,3])/aggdata[[s]][[1]][[mc85]][1:120,3])*100
      mc85 <- mc85 + 1
    }
  }
  seaspc15w[[s]] <- pcwater
  seaspc15e[[s]] <- pcelect
}


# percent change between 1971-2000 and last 30 years when 2.0 deg threshold was reached (pos == % increase) [new-old/old]

indices <- list(c(233,352),c(357,476),c(209,258),c(269,388),c(181,300),c(189,308),c(185,304),c(241,360))

seaspc20w <- list()
seaspc20e <- list()
for (s in 1:3) {
  pcwater <- list()
  pcelect <- list()
  mc26 <- 2
  mc85 <- 1
  for (i in 1:8) {
    if (i <= 3) { # RCP2.6
      pcwater[[i]] <- ((aggdata[[s]][[4]][[mc26]][indices[[i]][1]:indices[[i]][2],2]-aggdata[[s]][[4]][[mc26]][1:120,2])/aggdata[[s]][[4]][[mc26]][1:120,2])*100
      pcelect[[i]] <- ((aggdata[[s]][[4]][[mc26]][indices[[i]][1]:indices[[i]][2],3]-aggdata[[s]][[4]][[mc26]][1:120,3])/aggdata[[s]][[4]][[mc26]][1:120,3])*100
      mc26 <- mc26 + 1
    } else {      # RCP8.5
      pcwater[[i]] <- ((aggdata[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],2]-aggdata[[s]][[1]][[mc85]][1:120,2])/aggdata[[s]][[1]][[mc85]][1:120,2])*100
      pcelect[[i]] <- ((aggdata[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],3]-aggdata[[s]][[1]][[mc85]][1:120,3])/aggdata[[s]][[1]][[mc85]][1:120,3])*100
      mc85 <- mc85 + 1
    }
  }
  seaspc20w[[s]] <- pcwater
  seaspc20e[[s]] <- pcelect
}

# percent change between 1971-2000 and last 30 years when 3.0 deg threshold was reached (pos == % increase) [new-old/old]

indices <- list(c(385,504),c(257,376),c(269,388),c(265,384),c(345,464))

seaspc30w <- list()
seaspc30e <- list()
for (s in 1:3) {
  pcwater <- list()
  pcelect <- list()
  mc85 <- 1
  for (i in 1:5) {
      pcwater[[i]] <- ((aggdata[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],2]-aggdata[[s]][[1]][[mc85]][1:120,2])/aggdata[[s]][[1]][[mc85]][1:120,2])*100
      pcelect[[i]] <- ((aggdata[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],3]-aggdata[[s]][[1]][[mc85]][1:120,3])/aggdata[[s]][[1]][[mc85]][1:120,3])*100
      mc85 <- mc85 + 1
  }
  seaspc30w[[s]] <- pcwater
  seaspc30e[[s]] <- pcelect
}

# preciptiation-temperature percent changes

# percent change between 1971-2000 and last 30 years when 1.5 deg threshold was reached (pos == % increase) [new-old/old]

indices <- list(c(145,264),c(149,268),c(141,260),c(305,424),c(201,320),c(133,252),c(141,260),c(141,260),c(181,300))

seaspc15wPT <- list()
seaspc15ePT <- list()
for (s in 1:3) {
  pcwater <- list()
  pcelect <- list()
  mc26 <- 2
  mc85 <- 1
  for (i in 1:9) {
    if (i <= 4) { # RCP2.6
      pcwater[[i]] <- ((aggdataPT[[s]][[4]][[mc26]][indices[[i]][1]:indices[[i]][2],2]-aggdataPT[[s]][[4]][[mc26]][1:120,2])/aggdataPT[[s]][[4]][[mc26]][1:120,2])*100
      pcelect[[i]] <- ((aggdataPT[[s]][[4]][[mc26]][indices[[i]][1]:indices[[i]][2],3]-aggdataPT[[s]][[4]][[mc26]][1:120,3])/aggdataPT[[s]][[4]][[mc26]][1:120,3])*100
      mc26 <- mc26 + 1
    } else {      # RCP8.5
      pcwater[[i]] <- ((aggdataPT[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],2]-aggdataPT[[s]][[1]][[mc85]][1:120,2])/aggdataPT[[s]][[1]][[mc85]][1:120,2])*100
      pcelect[[i]] <- ((aggdataPT[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],3]-aggdataPT[[s]][[1]][[mc85]][1:120,3])/aggdataPT[[s]][[1]][[mc85]][1:120,3])*100
      mc85 <- mc85 + 1
    }
  }
  seaspc15wPT[[s]] <- pcwater
  seaspc15ePT[[s]] <- pcelect
}


# percent change between 1971-2000 and last 30 years when 2.0 deg threshold was reached (pos == % increase) [new-old/old]

indices <- list(c(233,352),c(357,476),c(209,258),c(269,388),c(181,300),c(189,308),c(185,304),c(241,360))

seaspc20wPT <- list()
seaspc20ePT <- list()
for (s in 1:3) {
  pcwater <- list()
  pcelect <- list()
  mc26 <- 2
  mc85 <- 1
  for (i in 1:8) {
    if (i <= 3) { # RCP2.6
      pcwater[[i]] <- ((aggdataPT[[s]][[4]][[mc26]][indices[[i]][1]:indices[[i]][2],2]-aggdataPT[[s]][[4]][[mc26]][1:120,2])/aggdataPT[[s]][[4]][[mc26]][1:120,2])*100
      pcelect[[i]] <- ((aggdataPT[[s]][[4]][[mc26]][indices[[i]][1]:indices[[i]][2],3]-aggdataPT[[s]][[4]][[mc26]][1:120,3])/aggdataPT[[s]][[4]][[mc26]][1:120,3])*100
      mc26 <- mc26 + 1
    } else {      # RCP8.5
      pcwater[[i]] <- ((aggdataPT[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],2]-aggdataPT[[s]][[1]][[mc85]][1:120,2])/aggdataPT[[s]][[1]][[mc85]][1:120,2])*100
      pcelect[[i]] <- ((aggdataPT[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],3]-aggdataPT[[s]][[1]][[mc85]][1:120,3])/aggdataPT[[s]][[1]][[mc85]][1:120,3])*100
      mc85 <- mc85 + 1
    }
  }
  seaspc20wPT[[s]] <- pcwater
  seaspc20ePT[[s]] <- pcelect
}


# percent change between 1971-2000 and last 30 years when 3.0 deg threshold was reached (pos == % increase) [new-old/old]

indices <- list(c(385,504),c(257,376),c(269,388),c(265,384),c(345,464))

seaspc30wPT <- list()
seaspc30ePT <- list()
for (s in 1:3) {
  pcwater <- list()
  pcelect <- list()
  mc85 <- 1
  for (i in 1:5) {
    pcwater[[i]] <- ((aggdataPT[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],2]-aggdataPT[[s]][[1]][[mc85]][1:120,2])/aggdataPT[[s]][[1]][[mc85]][1:120,2])*100
    pcelect[[i]] <- ((aggdataPT[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],3]-aggdataPT[[s]][[1]][[mc85]][1:120,3])/aggdataPT[[s]][[1]][[mc85]][1:120,3])*100
    mc85 <- mc85 + 1
  }
  seaspc30wPT[[s]] <- pcwater
  seaspc30ePT[[s]] <- pcelect
}




# moving average

maperiod <- 3*12 # 3 year moving average

mawater <- list()
maelec <- list()

for (p in 1:3) {
  mascenw <- list()
  mascene <- list()
  for (s in 1:4) {
    mamodw <- list()
    mamode <- list()
    for (m in 1:5) {
      w <- rollapply(aggdata[[p]][[s]][[m]][1:120,2], maperiod, mean, align = 'center',partial=T)
      e <- rollapply(aggdata[[p]][[s]][[m]][1:120,3], maperiod, mean, align = 'center',partial=T)

      # store values
      mamodw[[m]] <- w
      mamode[[m]] <- e
    }
    mascenw[[s]] <- (mamodw[[1]]+mamodw[[2]]+mamodw[[3]]+mamodw[[4]]+mamodw[[5]])/5
    mascene[[s]] <- (mamode[[1]]+mamode[[2]]+mamode[[3]]+mamode[[4]]+mamode[[5]])/5
  }
  mawater[[p]] <- mascenw
  maelec[[p]] <- mascene
}

# # moving average - actual data
# mawaterACT <- list()
# maelecACT <- list()
# 
# for (c in 1:6) {
#   
#   
#   w <- rollapply(cities[[c]][,3], maperiod, mean, align = 'center',partial=T)
#   e <- rollapply(cities[[c]][,4], maperiod, mean, align = 'center',partial=T)
#   
#   # store values
#   mawaterACT[[c]] <- w
#   maelecACT[[c]] <- e
# }
#################################   SUMMER/WINTER COMPARISON - CITY ANALYSIS ########################


# percent change between 1971-2000 and last 30 years when 1.5 deg threshold was reached (pos == % increase) [new-old/old]
citiespc15w <- list()
citiespc15e <- list()

indices <- list(c(145,264),c(149,268),c(141,260),c(305,424),c(201,320),c(133,252),c(141,260),c(141,260),c(181,300),c(1,120))

for (c in 1:6) {
  
  seaspc15wC <- list()
  seaspc15eC <- list()
  for (s in 1:3) {
    pcwater <- list()
    pcelect <- list()
    mc26 <- 2
    mc85 <- 1
    for (i in 1:9) {
      if (i <= 4) { # RCP2.6
        pcwater[[i]] <- ((yhatall[[s]][[4]][[mc26]][indices[[i]][1]:indices[[i]][2],2]-yhatall[[s]][[4]][[mc26]][indices[[10]][1]:indices[[10]][2],2])/yhatall[[s]][[4]][[mc26]][indices[[10]][1]:indices[[10]][2],2])*100
        pcelect[[i]] <- ((yhatall[[s]][[4]][[mc26]][indices[[i]][1]:indices[[i]][2],3]-yhatall[[s]][[4]][[mc26]][indices[[10]][1]:indices[[10]][2],3])/yhatall[[s]][[4]][[mc26]][indices[[10]][1]:indices[[10]][2],3])*100
        mc26 <- mc26 + 1
      } else {      # RCP8.5
        pcwater[[i]] <- ((yhatall[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],2]-yhatall[[s]][[1]][[mc85]][indices[[10]][1]:indices[[10]][2],2])/yhatall[[s]][[1]][[mc85]][indices[[10]][1]:indices[[10]][2],2])*100
        pcelect[[i]] <- ((yhatall[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],3]-yhatall[[s]][[1]][[mc85]][indices[[10]][1]:indices[[10]][2],3])/yhatall[[s]][[1]][[mc85]][indices[[10]][1]:indices[[10]][2],3])*100
        mc85 <- mc85 + 1
      }
    }
    seaspc15wC[[s]] <- pcwater
    seaspc15eC[[s]] <- pcelect
  }
  citiespc15w[[c]] <- seaspc15wC
  citiespc15e[[c]] <- seaspc15eC
  
  for (j in 1:10) {
    indices[[j]] <- indices[[j]] + 516
  }
}

# percent change between 1971-2000 and last 30 years when 2.0 deg threshold was reached (pos == % increase) [new-old/old]
citiespc20w <- list()
citiespc20e <- list()

indices <- list(c(233,352),c(357,476),c(209,258),c(269,388),c(181,300),c(189,308),c(185,304),c(241,360),c(1,120))

for (c in 1:6) {
  

  seaspc20wC <- list()
  seaspc20eC <- list()
  for (s in 1:3) {
    pcwater <- list()
    pcelect <- list()
    mc26 <- 2
    mc85 <- 1
    for (i in 1:8) {
      if (i <= 3) { # RCP2.6
        pcwater[[i]] <- ((yhatall[[s]][[4]][[mc26]][indices[[i]][1]:indices[[i]][2],2]-yhatall[[s]][[4]][[mc26]][indices[[9]][1]:indices[[9]][2],2])/yhatall[[s]][[4]][[mc26]][indices[[9]][1]:indices[[9]][2],2])*100
        pcelect[[i]] <- ((yhatall[[s]][[4]][[mc26]][indices[[i]][1]:indices[[i]][2],3]-yhatall[[s]][[4]][[mc26]][indices[[9]][1]:indices[[9]][2],3])/yhatall[[s]][[4]][[mc26]][indices[[9]][1]:indices[[9]][2],3])*100
        mc26 <- mc26 + 1
      } else {      # RCP8.5
        pcwater[[i]] <- ((yhatall[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],2]-yhatall[[s]][[1]][[mc85]][indices[[9]][1]:indices[[9]][2],2])/yhatall[[s]][[1]][[mc85]][indices[[9]][1]:indices[[9]][2],2])*100
        pcelect[[i]] <- ((yhatall[[s]][[1]][[mc85]][indices[[i]][1]:indices[[i]][2],3]-yhatall[[s]][[1]][[mc85]][indices[[9]][1]:indices[[9]][2],3])/yhatall[[s]][[1]][[mc85]][indices[[9]][1]:indices[[9]][2],3])*100
        mc85 <- mc85 + 1
      }
    }
    seaspc20wC[[s]] <- pcwater
    seaspc20eC[[s]] <- pcelect
  }
  citiespc20w[[c]] <- seaspc20wC
  citiespc20e[[c]] <- seaspc20eC
  
  for (j in 1:9) {
    indices[[j]] <- indices[[j]] + 516
  }
}


#################################   SUMMER/WINTER COMPARISON - FIGURES ##############################

# percent change per seasons (all scenarios)
for (s in 1:3) {
  avg15w <- c()
  q2515w <- c()
  q7515w <- c()
  avg15e <- c()
  q2515e <- c()
  q7515e <- c()
  for (i in 1:9) {
    avg15w[i] <- median(seaspc15w[[s]][[i]])
    q2515w[i] <- quantile(seaspc15w[[s]][[i]])[2]
    q7515w[i] <- quantile(seaspc15w[[s]][[i]])[4]
    avg15e[i] <- median(seaspc15e[[s]][[i]])
    q2515e[i] <- quantile(seaspc15e[[s]][[i]])[2]
    q7515e[i] <- quantile(seaspc15e[[s]][[i]])[4]
  }
  
  avg20w <- c()
  q2520w <- c()
  q7520w <- c()
  avg20e <- c()
  q2520e <- c()
  q7520e <- c()
  for (i in 1:8) {
    avg20w[i] <- median(seaspc20w[[s]][[i]])
    q2520w[i] <- quantile(seaspc20w[[s]][[i]])[2]
    q7520w[i] <- quantile(seaspc20w[[s]][[i]])[4]
    avg20e[i] <- median(seaspc20e[[s]][[i]])
    q2520e[i] <- quantile(seaspc20e[[s]][[i]])[2]
    q7520e[i] <- quantile(seaspc20e[[s]][[i]])[4]
  }
  
  avg30w <- c()
  q2530w <- c()
  q7530w <- c()
  avg30e <- c()
  q2530e <- c()
  q7530e <- c()
  for (i in 1:5) {
    avg30w[i] <- median(seaspc30w[[s]][[i]])
    q2530w[i] <- quantile(seaspc30w[[s]][[i]])[2]
    q7530w[i] <- quantile(seaspc30w[[s]][[i]])[4]
    avg30e[i] <- median(seaspc30e[[s]][[i]])
    q2530e[i] <- quantile(seaspc30e[[s]][[i]])[2]
    q7530e[i] <- quantile(seaspc30e[[s]][[i]])[4]
  }
  
  pchange <- c(mean(avg15w),mean(avg20w),mean(avg30w))
  q25 <- c(mean(q2515w),mean(q2520w),mean(q2530w)) 
  q75 <- c(mean(q7515w),mean(q7520w),mean(q7530w)) 
  
  Threshold <- c("1.5","2.0","3.0")
  
  pcdata <- data.frame(pchange,Threshold,q25,q75)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange)) + geom_bar(position="dodge", stat="identity") +
          geom_errorbar(aes(x=Threshold, ymin=q25, ymax=q75), width=.2, position=position_dodge(.9)) +
          xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Water Use')+
          theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")))
  
  pchange <- c(mean(avg15e),mean(avg20e),mean(avg30e))
  q25 <- c(mean(q2515e),mean(q2520e),mean(q2530e)) 
  q75 <- c(mean(q7515e),mean(q7520e),mean(q7530e)) 
  
  pcdata <- data.frame(pchange,Threshold,q25,q75)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange)) + geom_bar(position="dodge", stat="identity") +
          geom_errorbar(aes(x=Threshold, ymin=q25, ymax=q75), width=.2, position=position_dodge(.9)) +
          xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Electricity Use')+
          theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm"))) 

  
}

# percent change per seasons (all models)
for (s in 1:3) {
  avg15w <- c()
  q2515w <- c()
  q7515w <- c()
  avg15e <- c()
  q2515e <- c()
  q7515e <- c()
  for (i in 1:9) {
    avg15w[i] <- median(seaspc15w[[s]][[i]])
    q2515w[i] <- quantile(seaspc15w[[s]][[i]])[2]
    q7515w[i] <- quantile(seaspc15w[[s]][[i]])[4]
    avg15e[i] <- median(seaspc15e[[s]][[i]])
    q2515e[i] <- quantile(seaspc15e[[s]][[i]])[2]
    q7515e[i] <- quantile(seaspc15e[[s]][[i]])[4]
  }
  
  avg20w <- c()
  q2520w <- c()
  q7520w <- c()
  avg20e <- c()
  q2520e <- c()
  q7520e <- c()
  for (i in 1:8) {
    avg20w[i] <- median(seaspc20w[[s]][[i]])
    q2520w[i] <- quantile(seaspc20w[[s]][[i]])[2]
    q7520w[i] <- quantile(seaspc20w[[s]][[i]])[4]
    avg20e[i] <- median(seaspc20e[[s]][[i]])
    q2520e[i] <- quantile(seaspc20e[[s]][[i]])[2]
    q7520e[i] <- quantile(seaspc20e[[s]][[i]])[4]
  }
  
  avg30w <- c()
  q2530w <- c()
  q7530w <- c()
  avg30e <- c()
  q2530e <- c()
  q7530e <- c()
  for (i in 1:5) {
    avg30w[i] <- median(seaspc30w[[s]][[i]])
    q2530w[i] <- quantile(seaspc30w[[s]][[i]])[2]
    q7530w[i] <- quantile(seaspc30w[[s]][[i]])[4]
    avg30e[i] <- median(seaspc30e[[s]][[i]])
    q2530e[i] <- quantile(seaspc30e[[s]][[i]])[2]
    q7530e[i] <- quantile(seaspc30e[[s]][[i]])[4]
  }
  
  pchange <- c(mean(avg15w[1:4]),mean(avg20w[1:3]),NA,mean(avg15w[5:9]),mean(avg20w[4:8]),mean(avg30w))
  q25 <- c(mean(q2515w[1:4]),mean(q2520w[1:3]),NA,mean(q2515w[5:9]),mean(q2520w[4:8]),mean(q2530w)) 
  q75 <- c(mean(q7515w[1:4]),mean(q7520w[1:3]),NA,mean(q7515w[5:9]),mean(q7520w[4:8]),mean(q7530w)) 
  
  Threshold <- c("1.5","2.0","3.0","1.5","2.0","3.0")
  Scenario <- c("aLow","aLow","aLow","High","High","High")
  
  pcdata <- data.frame(pchange,Threshold,q25,q75,Scenario)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange,fill=Scenario)) + geom_bar(position="dodge", stat="identity", color = 'black') +
          geom_errorbar(aes(x=Threshold, ymin=q25, ymax=q75), width=.2, position=position_dodge(.9)) +
          xlab(expression('Temperature Threshold ('*degree*C*')')) + ylab('Percent Change (%)') + ggtitle('Water Use')+
          theme_light(base_size=30) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
          scale_fill_manual(values = c('#085c11','#849e2a'), labels = c('Low Warming','High Warming')))
  
  pchange <- c(mean(avg15e[1:4]),mean(avg20e[1:3]),NA,mean(avg15e[5:9]),mean(avg20e[4:8]),mean(avg30e))
  q25 <- c(mean(q2515e[1:4]),mean(q2520e[1:3]),NA,mean(q2515e[5:9]),mean(q2520e[4:8]),mean(q2530e)) 
  q75 <- c(mean(q7515e[1:4]),mean(q7520e[1:3]),NA,mean(q7515e[5:9]),mean(q7520e[4:8]),mean(q7530e)) 
  
  pcdata <- data.frame(pchange,Threshold,q25,q75,Scenario)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange,fill=Scenario)) + geom_bar(position="dodge", stat="identity", color = 'black') +
          geom_errorbar(aes(x=Threshold, ymin=q25, ymax=q75), width=.2, position=position_dodge(.9)) +
          xlab(expression('Temperature Threshold ('*degree*C*')')) + ylab('Percent Change (%)') + ggtitle('Electricity Use')+
          theme_light(base_size=30) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
          scale_fill_manual(values = c('#085c11','#849e2a'), labels = c('Low Warming','High Warming')))
  
  
}

# percent change per seasons 
for (s in 1:3) {
  avg15w <- c()
  q2515w <- c()
  q7515w <- c()
  avg15e <- c()
  q2515e <- c()
  q7515e <- c()
  for (i in 1:9) {
    avg15w[i] <- median(seaspc15w[[s]][[i]])
    q2515w[i] <- quantile(seaspc15w[[s]][[i]])[2]
    q7515w[i] <- quantile(seaspc15w[[s]][[i]])[4]
    avg15e[i] <- median(seaspc15e[[s]][[i]])
    q2515e[i] <- quantile(seaspc15e[[s]][[i]])[2]
    q7515e[i] <- quantile(seaspc15e[[s]][[i]])[4]
  }
  
  avg20w <- c()
  q2520w <- c()
  q7520w <- c()
  avg20e <- c()
  q2520e <- c()
  q7520e <- c()
  for (i in 1:8) {
    avg20w[i] <- median(seaspc20w[[s]][[i]])
    q2520w[i] <- quantile(seaspc20w[[s]][[i]])[2]
    q7520w[i] <- quantile(seaspc20w[[s]][[i]])[4]
    avg20e[i] <- median(seaspc20e[[s]][[i]])
    q2520e[i] <- quantile(seaspc20e[[s]][[i]])[2]
    q7520e[i] <- quantile(seaspc20e[[s]][[i]])[4]
  }
  
  avg30w <- c()
  q2530w <- c()
  q7530w <- c()
  avg30e <- c()
  q2530e <- c()
  q7530e <- c()
  for (i in 1:5) {
    avg30w[i] <- median(seaspc30w[[s]][[i]])
    q2530w[i] <- quantile(seaspc30w[[s]][[i]])[2]
    q7530w[i] <- quantile(seaspc30w[[s]][[i]])[4]
    avg30e[i] <- median(seaspc30e[[s]][[i]])
    q2530e[i] <- quantile(seaspc30e[[s]][[i]])[2]
    q7530e[i] <- quantile(seaspc30e[[s]][[i]])[4]
  }
  
  pchange <- c(0,avg15w,0,avg20w[1:3],0,avg20w[4:8],0,0,0,0,0,avg30w)
  q25 <- c(NA,q2515w,NA,q2520w[1:3],NA,q2520w[4:8],NA,NA,NA,NA,NA,q2530w) 
  q75 <- c(NA,q7515w,NA,q7520w[1:3],NA,q7520w[4:8],NA,NA,NA,NA,NA,q7530w) 
  
  
  Threshold <- c("1.5","1.5","1.5","1.5","1.5","1.5","1.5","1.5","1.5","1.5",
                 "2.0","2.0","2.0","2.0","2.0","2.0","2.0","2.0","2.0","2.0",
                 "3.0","3.0","3.0","3.0","3.0","3.0","3.0","3.0","3.0","3.0")
  Scenario <- c("RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5",
                "RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5",
                "RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP2.6","RCP8.5","RCP8.5","RCP8.5","RCP8.5","RCP8.5")
  
  Model <- c("GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M",
             "GFDL-ESM2M","HadGEM2-ES","IPSL-CM5A-LR","MIROC-ESM-CHEM","NorESM1-M")
  
  pcdata <- data.frame(pchange,Threshold,q25,q75,Scenario,Model)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange,fill=Model)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=q25, ymax=q75), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Water Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
    facet_grid(Scenario~.) + scale_fill_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99')))
  
  pchange <- c(0,avg15e,0,avg20e[1:3],0,avg20e[4:8],0,0,0,0,0,avg30e)
  q25 <- c(NA,q2515e,NA,q2520e[1:3],NA,q2520e[4:8],NA,NA,NA,NA,NA,q2530e) 
  q75 <- c(NA,q7515e,NA,q7520e[1:3],NA,q7520e[4:8],NA,NA,NA,NA,NA,q7530e) 
  
  pcdata <- data.frame(pchange,Threshold,q25,q75,Scenario,Model)
  
  print(ggplot(pcdata, aes(x=Threshold, y=pchange,fill=Model)) + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(x=Threshold, ymin=q25, ymax=q75), width=.2, position=position_dodge(.9)) +
    xlab('Temperature Threshold (degC)') + ylab('Percent Change (%)') + ggtitle('Electricity Use')+
    theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
    facet_grid(Scenario ~.) + scale_fill_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99')))
  
  
}

allwat <- c(mawater[[1]][[1]],mawater[[2]][[1]],mawater[[3]][[1]])
dates <- c(aggdata[[1]][[1]][[1]][1:120,1],aggdata[[2]][[1]][[1]][1:120,1],aggdata[[3]][[1]][[1]][1:120,1])
Period <- c(rep("Summer",120),rep("Winter",120),rep("Intermediate",120))

plotdata <- data.frame(allwat,dates,Period)

ggplot(plotdata) + geom_line(aes(x=dates,y=allwat,color=Period)) +
  xlab('Year') + ylab('Water Use (L/capita)') +
  theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_color_manual(values = c('#33a02c','#fb9a99','#1f78b4'))

allele <- c(maelec[[1]][[1]],maelec[[2]][[1]],maelec[[3]][[1]])

plotdata <- data.frame(allele,dates,Period)

ggplot(plotdata) + geom_line(aes(x=dates,y=allele,color=Period)) +
  xlab('Year') + ylab('Electricity Use (MWh/capita)') +
  theme_light(base_size=20) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
  scale_color_manual(values = c('#33a02c','#fb9a99','#1f78b4'))


#################################   SUMMER/WINTER COMPARISON - CITY FIGURES ##############################

# percent change per seasons (all scenarios)
for (c in 1:6) {
  
  avg15w <- c()
  q2515w <- c()
  q7515w <- c()
  avg15e <- c()
  q2515e <- c()
  q7515e <- c()
  for (i in 1:9) {
    avg15w[i] <- median(citiespc15w[[c]][[1]][[i]])
    q2515w[i] <- quantile(citiespc15w[[c]][[1]][[i]])[2]
    q7515w[i] <- quantile(citiespc15w[[c]][[1]][[i]])[4]
    avg15e[i] <- median(citiespc15e[[c]][[1]][[i]])
    q2515e[i] <- quantile(citiespc15e[[c]][[1]][[i]])[2]
    q7515e[i] <- quantile(citiespc15e[[c]][[1]][[i]])[4]
  }
  
  avg20w <- c()
  q2520w <- c()
  q7520w <- c()
  avg20e <- c()
  q2520e <- c()
  q7520e <- c()
  for (i in 1:8) {
    avg20w[i] <- median(citiespc20w[[c]][[1]][[i]])
    q2520w[i] <- quantile(citiespc20w[[c]][[1]][[i]])[2]
    q7520w[i] <- quantile(citiespc20w[[c]][[1]][[i]])[4]
    avg20e[i] <- median(citiespc20e[[c]][[1]][[i]])
    q2520e[i] <- quantile(citiespc20e[[c]][[1]][[i]])[2]
    q7520e[i] <- quantile(citiespc20e[[c]][[1]][[i]])[4]
  }
  
  pchange <- c(mean(avg15w),mean(avg15e),mean(avg20w),mean(avg20e))
  q25 <- c(mean(q2515w),mean(q2515e),mean(q2520w),mean(q2520e)) 
  q75 <- c(mean(q7515w),mean(q7515e),mean(q7520w),mean(q7520e)) 
  
  Utility <- c("Water","Electricity","Water","Electricity")
  Threshold <- c("1.5","1.5","2.0","2.0")
  
  pcdata <- data.frame(pchange,Utility,q25,q75,Threshold)
  
  print(ggplot(pcdata, aes(x=Utility, y=pchange,fill=Threshold)) + geom_bar(position="dodge", stat="identity",color='black') +
          geom_errorbar(aes(x=Utility, ymin=q25, ymax=q75), width=.2, position=position_dodge(.9)) +
          xlab('Utility') + ylab('Percent Change (%)') + labs(fill = 'Temperature\nThreshold') +
          theme_light(base_size=30) + theme(plot.margin=unit(c(1,1,1,2),"cm")) +
          scale_fill_manual(values = c('#085c11','#849e2a'), labels = c(expression('1.5'*degree*'C'),expression('2.0'*degree*'C')))) 
  
  
}

