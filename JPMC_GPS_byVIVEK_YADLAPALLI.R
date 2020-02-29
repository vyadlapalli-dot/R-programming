
library(tidyverse)
library(httr)
library(jsonlite)
library ("graphics")
library("quantmod")

##Ticker List
Tickers<-list("DGS1","DGS2","DGS3","DGS5","DGS7","DGS10")

##Data frame which will hold the responses
US_Treasury_Data <-data.frame()

##Quering and retrieving data in a loop
for(n in 1:length(Tickers)){
  
  path <- paste0(
    "https://api.stlouisfed.org/fred/series/observations?series_id=",
                 as.character(Tickers[n]),
                 "&api_key=cf8d6b5242f8b3258ca0de375adc5d12&file_type=json")
  
  ##Request GET and Response content
  request <- GET(url = path)
  request$status_code
  response <- content(request, as = "text", encoding = "UTF-8")
  
  ##Converting to data frame from JSON
  df <- fromJSON(response, flatten = TRUE) %>% 
    data.frame()
  df$observations.date <- as.Date(df$observations.date)
  df$observations.value <- as.numeric(df$observations.value)
  df<- data.frame(df$observations.date,df$observations.value)
  
  ##Obating required variables Date and Value 
  names(df)<-c("Date",as.character(Tickers[n])) 
  
  ##Handling n==1
  if(n==1){
    US_Treasury_Data <- data.frame(df)
  }else{
    US_Treasury_Data<-merge(x=US_Treasury_Data,y=df,by="Date")
  }

}

##Omitting 1976, as data is not complete
US_Treasury_Data <- US_Treasury_Data[
  -which(US_Treasury_Data$Date<"1977-01-01"),]

rownames(US_Treasury_Data) <- US_Treasury_Data$Date
US_Treasury_Data <- US_Treasury_Data[,-1]
US_Treasury_Data <- na.omit(US_Treasury_Data)

##Creating a zoo series
US_Treasury_Data.diff = zoo(diff((coredata(as.xts(US_Treasury_Data)))),
                          order.by=time(as.xts(US_Treasury_Data))[-1])

par(mfcol=c(1,1))

##plot1 - Rainbow plot of Treasury maturity rates from 1977-2019
ts.plot(as.ts(as.xts(US_Treasury_Data)),
        col=rainbow(NCOL(as.xts(US_Treasury_Data))),
        bg="black",
        main="FRED Rates: 1977-2019\n[DGS1,DGS2,DGS3,DGS5,DGS7,DGS10]")

legend(x=30000000,y=15,
       legend=dimnames(as.xts(US_Treasury_Data))[[2]],
       lty=rep(1,times=NCOL(as.xts(US_Treasury_Data))),
       col=rainbow(NCOL(as.xts(US_Treasury_Data))),cex=0.70)

##Calculating yield changes from the data
yieldchanges <- US_Treasury_Data.diff

##Calculating Mean
yieldchanges.mean <- apply(yieldchanges,2,mean)

##Calculating Volatility
yieldchanges.vol <- sqrt(apply(yieldchanges,2,var))

print( round (data.frame(daily.mean=yieldchanges.mean,
                         daily.vol=yieldchanges.vol),digits=4))
##Calculating Correlation
yieldchanges.cor <- cor(US_Treasury_Data.diff)

print(round(yieldchanges.cor, digits=3))

##PART 2: Using Principal Component Analysis as a Dimension Reduction Method
pca<- princomp(coredata(US_Treasury_Data.diff))

print(summary(pca))

par(mfcol=c(1,2))

##Screeplot of explained(proportion) variances of principal components
screeplot(pca,
          main=paste("PCA US Treasury Yield Changes",sep="\n") )

##Calculating Volatility 
vol<-sqrt(apply(pca$scores,2,var))

#Comparing PCA volatility to volatitlity calculated earlier
ts.plot(cbind(as.matrix(yieldchanges.vol),
              as.matrix(vol)), type="b", col=c(3,4),
        xlab="Variables: US Treasury (Green) and PC Vars (Blue)",
        main="Volatility of Yield Changes")

##PCA Loadings
print(round(pca$loadings[,1:6],digits=6))

par(mfcol=c(2,1))

##PC1 Loadings
barplot(pca$loadings[,1],
        main="US Treasury Yield Changes: PCA\n Loadings: PC1")

##PC2 Loadings
barplot(pca$loadings[,2],
        main="Loadings: PC2")

##PC6 Loadings
barplot(pca$loadings[,6],
        main="Loadings: PC6")

##Finding PCA correlation of scores - Identity
print(round(cor(pca$scores)))

##comparing components 1,2,6
pairs(pca$scores[,c(1:2,6)])

par(mfcol=c(1,1))

##PC1 cummulative plot
ts.plot(cumsum(pca$scores[,1]),
        main="Cumulative PC\nPC1 (Level-Shifts)",
        ylab="Cumulative PC1",
        xlab="Day")

##PC2 cummulative plot
ts.plot(cumsum(pca$scores[,2]),
        main="Cumulative PC\nPC2 (Spread Between Long and Short Maturities)",
        ylab="Cumulative PC2",
        xlab="Day")

##PC6 cummulative plot
ts.plot(cumsum(pca$scores[,6]),
        main="Cumulative PC\nPC6 (5 and 10 Year vs 7 Year)",
        ylab="Cumulative PC6",
        xlab="Day")

##Analysing Stability through standard deviation
pov_pca_Stability <- data.frame("Comp.1"=NA,
                                "Comp.2"=NA,
                                "Comp.3"=NA,
                                "Comp.4"=NA,
                                "Comp.5"=NA,
                                "Comp.6"=NA)

sd_pca_Stability <- data.frame("Comp.1"=NA,
                               "Comp.2"=NA,
                               "Comp.3"=NA,
                               "Comp.4"=NA,
                               "Comp.5"=NA,
                               "Comp.6"=NA)

##loop to find pca's for consecutive time series 
for(s in 7:nrow(US_Treasury_Data.diff)){
  pca_Stability_temp<- princomp(coredata(US_Treasury_Data.diff[1:s,]))
  
  temp <- t(data.frame(pca_Stability_temp$sdev))
  
  row.names(temp) <- s
  
  sd_pca_Stability <- rbind(sd_pca_Stability,temp)
  
  temp <- t(data.frame(
    pca_Stability_temp$sdev^2/sum(pca_Stability_temp$sdev^2)))
  
  row.names(temp) <- s
  
  pov_pca_Stability <- rbind(pov_pca_Stability,temp)
  
  print(paste0(s,"/",nrow(US_Treasury_Data.diff)," Executed"))
}

sd_pca_Stability <- sd_pca_Stability[-1,]

pov_pca_Stability <- pov_pca_Stability[-1,]

row.names(sd_pca_Stability) <- index(
  US_Treasury_Data.diff[7:nrow(US_Treasury_Data.diff),])

row.names(pov_pca_Stability) <- index(
  US_Treasury_Data.diff[7:nrow(US_Treasury_Data.diff),])

##SD - Stabilty Analysis
ts.plot(as.ts(as.xts(sd_pca_Stability)),
        col=rainbow(NCOL(as.xts(sd_pca_Stability))),
        bg="black",
        main="Standard Deviation of Principal Components - Selective resampling")

legend(x=33000000,y=0.3,
       legend=dimnames(as.xts(sd_pca_Stability))[[2]],
       lty=rep(1,times=NCOL(as.xts(sd_pca_Stability))),
       col=rainbow(NCOL(as.xts(sd_pca_Stability))),
       cex=0.70)

ts.plot(as.ts(as.xts(pov_pca_Stability)),
        col=rainbow(NCOL(as.xts(pov_pca_Stability))),
        bg="black",
        main="Propotion of variation of Principal Components - Selective resampling")

legend(x=33000000,
       y=0.6,
       legend=dimnames(as.xts(sd_pca_Stability))[[2]],
       lty=rep(1,times=NCOL(as.xts(sd_pca_Stability))),
       col=rainbow(NCOL(as.xts(sd_pca_Stability))),
       cex=0.70)

##PART 3: Calculating pca's yearly including 2019
PCA_Year_Wise <- list()

temp <- US_Treasury_Data.diff

temp$Date <- row.names(US_Treasury_Data.diff)

count =1

for (n in 1978:2020){
  pca_temp<- princomp(coredata(
    US_Treasury_Data.diff[
      which(index(US_Treasury_Data.diff)<as.Date(paste0(n,"-01-01")) & 
              index(US_Treasury_Data.diff)>as.Date(paste0(n-2,"-12-31"))),]))
  
  PCA_Year_Wise[count] <- list(pca_temp)
  
  count = count + 1
  
  }

SIM_year <- data.frame("SIM_Index"=NA,"Year"=NA)

##Calculating SIM Index through loop
for(i in 1:42){
  sum=0
  for(j in 1:1){
    for(k in 1:1){
      sum = sum+(acos(cor(
        PCA_Year_Wise[[i]]$loadings[,paste0("Comp.",j)],
        PCA_Year_Wise[[43]]$loadings[,paste0("Comp.",k)]))/((2*pi)/360))^2
      
    }
  }
  
  SIM_year <- rbind(SIM_year,c(sum,1976+i))
  
}

w<-list()

EROS_year <- data.frame("EROS_Index"=NA,
                        "Year"=NA)

##Calculating EROS Index through loop
for(i in 1:42){
  sum=0
  w <- list((matrix(PCA_Year_Wise[[i]]$sdev)^2+
               matrix(PCA_Year_Wise[[43]]$sdev)^2)/2)
  w <- unlist(w)/sum(unlist(w))
  for(j in 1:6){
    sum = sum+w[j]*acos(cor(
      PCA_Year_Wise[[i]]$loadings[,paste0("Comp.",j)],
      PCA_Year_Wise[[43]]$loadings[,paste0("Comp.",j)]))/((2*pi)/360)
    }
  EROS_year <- rbind(EROS_year,c(sum,1976+i))
  
}

SIM_year <- SIM_year[order(-SIM_year$SIM_Index),]

EROS_year <- EROS_year[order(-EROS_year$EROS_Index),]

print(head(SIM_year))

print(head(EROS_year))

##END OF PROJECT