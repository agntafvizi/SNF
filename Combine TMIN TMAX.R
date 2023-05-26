library(ggplot2)
library(dplyr)
library(naniar)
library(extrafont)
#library(EcoHydrology)
library(imputeTS)
library(readr)
library(zoo)
library(hydroGOF)
library(operators)
library(topmodel)
library(DEoptim)
library(parallel)
library(baseflow)
library(XML)
library(EcoHydRology)
library("extrafont")


#########################NORTHBAY-RCP 4.5##############################
#Reading files
TMIN <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TmaxTminCombine/NorthBay-TMIN-BCCCSM8.5-2020.csv")
TMAX <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TmaxTminCombine/NorthBay-TMAX-BCCCSM8.5-2020.csv")


#Combine two excel files (Tmin and Tmax)
COMBINE = merge(TMIN,TMAX,by=c('NUMBER'),all.x=T, all.y=T)

#insert -9999 instead of NA to prepare for comparing in the next step
COMBINE$TMIN[is.na(COMBINE$TMIN)] <- -9999
COMBINE$TMAX[is.na(COMBINE$TMAX)] <- -9999

View(COMBINE)

#extract only tempmin and tempmax
COMBINE1= subset(COMBINE, select = c(TMIN, TMAX ))

#choose the maximum in each row to write
maximum <-apply(COMBINE1,1,max)

write.csv(maximum,"NORTHBAY-BCCCSM8.5-2020.csv")

#########################SUDBUR- RCP4.5##############################
#Reading files
TMIN <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TmaxTminCombine/SUDBURY-TMIN-BCCCSM8.5-2020.csv")
TMAX <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TmaxTminCombine/SUDBURY-TMAX-BCCCSM8.5-2020.csv")


#Combine two excel files (Tmin and Tmax)
COMBINE = merge(TMIN,TMAX,by=c('NUMBER'),all.x=T, all.y=T)

#insert -9999 instead of NA to prepare for comparing in the next step
COMBINE$TMIN[is.na(COMBINE$TMIN)] <- -9999
COMBINE$TMAX[is.na(COMBINE$TMAX)] <- -9999

View(COMBINE)

#extract only tempmin and tempmax
COMBINE1= subset(COMBINE, select = c(TMIN, TMAX ))

#choose the maximum in each row to write
maximum <-apply(COMBINE1,1,max)

write.csv(maximum,"SUDBURY-BCCCSM8.5-2020.csv")

#########################BARRAGE-RCP4.5##############################
#Reading files
TMIN <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TmaxTminCombine/BARRAGE-TMIN-BCCCSM8.5-2020.csv")
TMAX <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TmaxTminCombine/BARRAGE-TMAX-BCCCSM8.5-2020.csv")


#Combine two excel files (Tmin and Tmax)
COMBINE = merge(TMIN,TMAX,by=c('NUMBER'),all.x=T, all.y=T)

#insert -9999 instead of NA to prepare for comparing in the next step
COMBINE$TMIN[is.na(COMBINE$TMIN)] <- -9999
COMBINE$TMAX[is.na(COMBINE$TMAX)] <- -9999

View(COMBINE)

#extract only tempmin and tempmax
COMBINE1= subset(COMBINE, select = c(TMIN, TMAX ))

#choose the maximum in each row to write
maximum <-apply(COMBINE1,1,max)

write.csv(maximum,"BARRAGE-BCCCSM48.5-2020.csv") 

#########################NORTHBAY-RCP 8.5##############################
#Reading files
TMIN <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TmaxTminCombine/NorthBay-TMIN-CSIRO8.5-2020.csv")
TMAX <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TmaxTminCombine/NorthBay-TMAX-CSIRO8.5-2020.csv")


#Combine two excel files (Tmin and Tmax)
COMBINE = merge(TMIN,TMAX,by=c('NUMBER'),all.x=T, all.y=T)

#insert -9999 instead of NA to prepare for comparing in the next step
COMBINE$TMIN[is.na(COMBINE$TMIN)] <- -9999
COMBINE$TMAX[is.na(COMBINE$TMAX)] <- -9999

View(COMBINE)

#extract only tempmin and tempmax
COMBINE1= subset(COMBINE, select = c(TMIN, TMAX ))

#choose the maximum in each row to write
maximum <-apply(COMBINE1,1,max)

write.csv(maximum,"NORTHBAY-CSIRO8.5-2020.csv")

#########################SUDBURY-RCP8.5##############################
#Reading files
TMIN <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TmaxTminCombine/SUDBURY-TMIN-CSIRO8.5-2020.csv")
TMAX <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TmaxTminCombine/SUDBURY-TMAX-CSIRO8.5-2020.csv")


#Combine two excel files (Tmin and Tmax)
COMBINE = merge(TMIN,TMAX,by=c('NUMBER'),all.x=T, all.y=T)

#insert -9999 instead of NA to prepare for comparing in the next step
COMBINE$TMIN[is.na(COMBINE$TMIN)] <- -9999
COMBINE$TMAX[is.na(COMBINE$TMAX)] <- -9999

View(COMBINE)

#extract only tempmin and tempmax
COMBINE1= subset(COMBINE, select = c(TMIN, TMAX ))

#choose the maximum in each row to write
maximum <-apply(COMBINE1,1,max)

write.csv(maximum,"SUDBURY-CSIRO8.5-2020.csv")


#########################BARRAGE-RCP8.5##############################
#Reading files
TMIN <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TmaxTminCombine/BARRAGE-TMIN-CSIRO8.5-2020.csv")
TMAX <- read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/TmaxTminCombine/BARRAGE-TMAX-CSIRO8.5-2020.csv")


#Combine two excel files (Tmin and Tmax)
COMBINE = merge(TMIN,TMAX,by=c('NUMBER'),all.x=T, all.y=T)

#insert -9999 instead of NA to prepare for comparing in the next step
COMBINE$TMIN[is.na(COMBINE$TMIN)] <- -9999
COMBINE$TMAX[is.na(COMBINE$TMAX)] <- -9999

View(COMBINE)

#extract only tempmin and tempmax
COMBINE1= subset(COMBINE, select = c(TMIN, TMAX ))

#choose the maximum in each row to write
maximum <-apply(COMBINE1,1,max)

write.csv(maximum,"BARRAGE-CSIRO8.5-2020.csv") 

