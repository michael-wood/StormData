#!/usr/bin/env Rscript

library(ggplot2)

# Set fileURL and download data if necessary                                    
fileURL <- ""
fileDL <- "./data/repdata_data_StormData.csv.bz2"                                                  
if(!file.exists(fileDL)){                                                       
  download.file(fileURL, destfile=fileDL, method="curl")                  
}                                                                               
csvData <- read.csv(fileDL)

# Summation of Fatalities by Event Type
fatalByEvt <- as.data.frame(
  tapply(csvData$FATALITIES, csvData$EVTYPE, FUN=sum))
colnames(fatalByEvt) <- c("Fatalities")
# Remove Events with ZERO occurances
FBE.NoZero <- as.data.frame(fatalByEvt[fatalByEvt$Fatalities!=0,])
colnames(FBE.NoZero) <- c("Fatalities")

# Summation of Injuries by Event Type
injuryByEvt <- as.data.frame(
  tapply(csvData$INJURIES, csvData$EVTYPE, FUN=sum))
colnames(injuryByEvt) <- c("Injuries")
# Remove Events with ZERO occurances
IBE.NoZero <- as.data.frame(injuryByEvt[injuryByEvt$Injuries!=0,])
colnames(IBE.NoZero) <- c("Injuries")

# Summation of Property Damage by Event Type
propDmgByEvt <- as.data.frame(
  tapply(csvData$PROPDMG, csvData$EVTYPE, FUN=sum))
colnames(propDmgByEvt) <- c("Count")
# Remove Events with ZERO occurances
PBE.NoZero <- as.data.frame(propDmgByEvt[propDmgByEvt$Count!=0,])
colnames(PBE.NoZero) <- c("Count")

# Summation of Crop Damage by Event Type
cropDmgByEvt <- as.data.frame(
  tapply(csvData$CROPDMG, csvData$EVTYPE, FUN=sum))
colnames(cropDmgByEvt) <- c("Count")
# Remove Events with ZERO occurances
CBE.NoZero <- as.data.frame(cropDmgByEvt[cropDmgByEvt$Count!=0,])
colnames(CBE.NoZero) <- c("Count")

## Fatalities
#topFatal <- as.data.frame(head(sort(fatalByEvt$Count, decreasing=TRUE), n=5))
qtilesFBE <- quantile(FBE.NoZero$Fatalities, probs = seq(0, 1, 0.1))
topFatal <- FBE.NoZero[FBE.NoZero$Fatalities>qtilesFBE[10],]
totalFatal <- sum(fatalByEvt$Fatalities)

## Injuries
#topInjury <- as.data.frame(head(sort(injuryByEvt$Count, decreasing=TRUE), n=5))
qtilesIBE <- quantile(IBE.NoZero$Injuries, probs = seq(0, 1, 0.1))
topInjury <- IBE.NoZero[IBE.NoZero$Injuries>qtilesIBE[10],]
totalInjury <- sum(injuryByEvt$Injuries)

## Property Damage
#topProperty <- as.data.frame(head(sort(propDmgByEvt$Count, decreasing=TRUE), n=5))
qtilesPBE <- quantile(PBE.NoZero$Count, probs = seq(0, 1, 0.1))
topPropDmg <- PBE.NoZero[PBE.NoZero$Count>qtilesPBE[10],]
totalPropDmg <- sum(propDmgByEvt$Count)

## Crop Damage
#topCrop <- as.data.frame(head(sort(cropDmgByEvt$Count, decreasing=TRUE), n=5))
qtilesCBE <- quantile(CBE.NoZero$Count, probs = seq(0, 1, 0.1))
topCropDmg <- CBE.NoZero[CBE.NoZero$Count>qtilesCBE[10],]
totalCropDmg <- sum(cropDmgByEvt$Count)

# Graphs
#qplot(row.names(topFatal), topFatal) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("Events") + ylab("Fatalities")
#qplot(row.names(topInjury), topInjury) + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ xlab("Events") + ylab("Injurities")
#plot(row.names(topPropDmg), topPropDmg) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlim(0, max(topPropDmg)) + xlab("Events") + ylab("Property Damager ($)")
