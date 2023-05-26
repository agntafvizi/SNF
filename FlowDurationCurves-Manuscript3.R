install.packages("waterData")
install.packages("hydroTSM")
install.packages("tseries")
install.packages("units")

library(waterData)
library(hydroTSM)
library(dplyr)
library(ggplot2)
library(tseries)
library(units)
 
############**********************###########################
#######UPPERGOOSE#######
############**********************###########################

UG= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/UG-1990-2082.csv")
flow<-na.omit(UG)

flowbase <- subset(flow, YEAR >=1992 & YEAR < 2020)
write.csv(flowbase, "UGflowbase.csv")
flowbase= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/UGflowbase.csv")

#Sorting discharges in decreasing order
flowbase<-sort(flowbase$Flow,decreasing=T)

#Creating a data frame in which x column is hte percent of ie less than a specific time and
#y is the correspondent discharge.

flowbase<-data.frame(probbase=100/length(flowbase)*1:length(flowbase),flowbase=(flowbase/953.6))
write.csv(flowbase, "UG-FDC-BASEPERIOD.csv")

###########2020-2040#####################    
flowperiod1 <- subset(flow, YEAR >=2020 & YEAR < 2041)
write.csv(flowperiod1, "UGflowperiod1.csv")
flowperiod1= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/UGflowperiod1.csv")

flowperiod1<-sort(flowperiod1$Flow, decreasing=T)
flowperiod1<-data.frame(probperiod1=100/length(flowperiod1)*1:length(flowperiod1),flowperiod1=(flowperiod1/953.6))
write.csv(flowperiod1, "UG-FDC-2020-2040.csv")

################2041-2061#################################
flowperiod2 <- subset(flow, YEAR >=2041 & YEAR < 2062)
write.csv(flowperiod2, "UGflowperiod2.csv")
flowperiod2= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/UGflowperiod2.csv")

flowperiod2<-sort(flowperiod2$Flow, decreasing=T)
flowperiod2<-data.frame(probperiod2=100/length(flowperiod2)*1:length(flowperiod2),flowperiod2=(flowperiod2/953.6))
write.csv(flowperiod2, "UG-FDC-2041-2061.csv")

################2062-2082#################################

flowperiod3 <- subset(flow, YEAR >=2062 & YEAR <= 2082)
write.csv(flowperiod3, "UGflowperiod3.csv")
flowperiod3= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/UGflowperiod3.csv")

flowperiod3<-sort(flowperiod3$Flow, decreasing=T)
flowperiod3<-data.frame(probperiod3=100/length(flowperiod3)*1:length(flowperiod3),flowperiod3=(flowperiod3/953.6))
write.csv(flowperiod3, "UG-FDC-2062-2082.csv")

################Observation#################################
obs= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/Results/StreamAlPeriodAnalysis/ObservationFiles/UG_OBS.csv")
obs<-replace(obs,obs==-999,NA)
obs<-na.omit(obs)
obs<-sort(obs$FLOW, decreasing=T)
obs<-data.frame(probobs=100/length(obs)*1:length(obs),obs=(obs/953.6))
write.csv(obs, "UG-FDC-obs.csv")
##############################################################
#Plot
plot(x = df$x, y = df$y, type= "l", log = "y",ylab="'Daily Discharge( '*m^3/s/km^2*')'", ylim = c(0.001, 0.2), col= c("black"), lty=1,  lwd=3, 
     xlab="Exceedance Probability (%)",
     cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.2, ) 
#legend("top", legend=c("Upper Goose"),
#lty=2, lwd=3,col= c("white"),  bty = "n", cex = 1.5)    #lty=line type, lwd=thickness, bty= eliminate outer black box of legend
grid(nx=NULL, ny=NULL,col="lightgray", lty=21, lwd=0.5,equilogs=TRUE)

#####LONGTERM-SHORTTERM PLOTS#####


ggplot()+
    geom_line(data= flowbase, mapping = aes(x= probbase, y=flowbase, col="baseline"), size=1, linetype="longdash") +
    geom_line(data= flowperiod1, mapping = aes(x= probperiod1, y=flowperiod1, col="2020-2040"),size=1)+
    geom_line(data= flowperiod2, mapping = aes(x= probperiod2, y=flowperiod2, col="2041-2061"),size=1)+
    geom_line(data= flowperiod3, mapping = aes(x= probperiod3, y=flowperiod3, col="2062-2082"),size=1)+
    #geom_line(data= obs, mapping = aes(x= probobs, y=obs, col="Observation"),size=1)+
    scale_color_manual(values=c("green4", "deepskyblue4", "blueviolet", "black", "pink")) +
    xlab("Exceedance Probability (%)") +
    scale_y_log10()+
    ylab(bquote('Daily Discharge ( '*m^3/s/km^2*')')) +
    #scale_x_time(breaks=c("2","367", "733", "1098", "1463", "1828", "2194"), 
    #labeUG=c("2012", "2013", "2014", "2015", "2016", "2017", "2018")) + 
    #scale_y_time(breaks=c("-4","-8", "-12", "-16", "-18"), 
    #labeUG=c("-4", "-8", "-12", "-16", "-18")) +
    #theme_bw() +
    theme(text = element_text(size = 18)) +
    theme(legend.position = c(.95, -2),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6), legend.title = element_blank()
    )  
    
    
    

#######CHIPPEWA#######
    
CC= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurves.input/Chippewa_2012_2018.csv")
    flow<-na.omit(CC)
    
    #Sorting discharges in decreasing order
    flow<-sort(CC$Flow,decreasing=T)
    
    #Creating a data frame in which x column is hte percent of ie less than a specific time and
    #yis the correspondent discharge.
    df<-data.frame(x=100/length(flow)*1:length(flow),y=(flow/35.608))
    
    #Plot
    plot(x = df$x, y = df$y, log = "y", type="l",ylab="Daily Discharge(m3/s/km2)", col= c("black"), lty=1,  lwd=3, xlab="Exceedance Probability (%)",
         cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.2, ) 
    #legend("top", legend=c("Upper Goose"),
    #lty=2, lwd=3,col= c("white"),  bty = "n", cex = 1.5)    #lty=line type, lwd=thickness, bty= eliminate outer black box of legend
    grid(nx=NULL, ny=NULL,col="lightgray", lty=21, lwd=0.5,equilogs=TRUE)
    
    #####LONGTERM-SHORTTERM PLOTS#####
    ggplot(df)+
        geom_line(mapping = aes(x= x, y=y, col="long-term FDC"), size=0.75) +
        geom_point(mapping = aes(x= x, y=y, col="2012_2018 FDC"), shape=1, size=2, stroke = 1)+
        scale_color_manual(values=c("brown3", "black")) +
        xlab("Exceedance Probability (%)") +
        scale_y_log10()+
        ylab(bquote('Daily Discharge(m3/s/km2)')) +
        #scale_x_time(breaks=c("2","367", "733", "1098", "1463", "1828", "2194"), 
        #labels=c("2012", "2013", "2014", "2015", "2016", "2017", "2018")) + 
        #scale_y_time(breaks=c("-4","-8", "-12", "-16", "-18"), 
        #labels=c("-4", "-8", "-12", "-16", "-18")) +
        theme_bw() +
        theme(text = element_text(size = 18)) +
        theme(legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6), legend.title = element_blank()
        )
    
############**********************###########################
    #######WASI#######
############**********************###########################
    
    WASI= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/WASI-1990-2082.csv")
    flow<-na.omit(WASI)
    
    flowbase <- subset(flow, YEAR >=1992 & YEAR < 2020)
    write.csv(flowbase, "wasiflowbase.csv")
    flowbase= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/wasiflowbase.csv")
    
    #Sorting discharges in decreasing order
    flowbase<-sort(flowbase$Flow,decreasing=T)
    
    #Creating a data frame in which x column is hte percent of ie less than a specific time and
    #y is the correspondent discharge.
    
    flowbase<-data.frame(probbase=100/length(flowbase)*1:length(flowbase),flowbase=(flowbase/211.5))
    write.csv(flowbase, "WASI-FDC-BASEPERIOD.csv")
    
###########2020-2040#####################    
    flowperiod1 <- subset(flow, YEAR >=2020 & YEAR < 2041)
    write.csv(flowperiod1, "wasiflowperiod1.csv")
    flowperiod1= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/wasiflowperiod1.csv")
    
    flowperiod1<-sort(flowperiod1$Flow, decreasing=T)
    flowperiod1<-data.frame(probperiod1=100/length(flowperiod1)*1:length(flowperiod1),flowperiod1=(flowperiod1/211.5))
    write.csv(flowperiod1, "WASI-FDC-2020-2040.csv")
    
################2041-2061#################################
    flowperiod2 <- subset(flow, YEAR >=2041 & YEAR < 2062)
    write.csv(flowperiod2, "wasiflowperiod2.csv")
    flowperiod2= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/wasiflowperiod2.csv")
    
    flowperiod2<-sort(flowperiod2$Flow, decreasing=T)
    flowperiod2<-data.frame(probperiod2=100/length(flowperiod2)*1:length(flowperiod2),flowperiod2=(flowperiod2/211.5))
    write.csv(flowperiod2, "WASI-FDC-2041-2061.csv")
    
    ################2062-2082#################################
    
    flowperiod3 <- subset(flow, YEAR >=2062 & YEAR <= 2082)
    write.csv(flowperiod3, "wasiflowperiod3.csv")
    flowperiod3= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/wasiflowperiod3.csv")
    
    flowperiod3<-sort(flowperiod3$Flow, decreasing=T)
    flowperiod3<-data.frame(probperiod3=100/length(flowperiod3)*1:length(flowperiod3),flowperiod3=(flowperiod3/211.5))
    write.csv(flowperiod3, "WASI-FDC-2062-2082.csv")
    
    ################Observation#################################
    obs= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/Results/StreamAlPeriodAnalysis/ObservationFiles/WASI_OBS.csv")
    obs<-replace(obs,obs==-999,NA)
    obs<-na.omit(obs)
    obs<-sort(obs$FLOW, decreasing=T)
    obs<-data.frame(probobs=100/length(obs)*1:length(obs),obs=(obs/211.5))
    write.csv(obs, "WASI-FDC-obs.csv")
    ##############################################################
    #Plot
    plot(x = df$x, y = df$y, type= "l", log = "y",ylab="'Daily Discharge( '*m^3/s/km^2*')'", ylim = c(0.001, 0.2), col= c("black"), lty=1,  lwd=3, 
         xlab="Exceedance Probability (%)",
         cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.2, ) 
    #legend("top", legend=c("Upper Goose"),
    #lty=2, lwd=3,col= c("white"),  bty = "n", cex = 1.5)    #lty=line type, lwd=thickness, bty= eliminate outer black box of legend
    grid(nx=NULL, ny=NULL,col="lightgray", lty=21, lwd=0.5,equilogs=TRUE)
    
    #####LONGTERM-SHORTTERM PLOTS#####
    
    
    ggplot()+
        geom_line(data= flowbase, mapping = aes(x= probbase, y=flowbase, col="baseline"), size=1, linetype="longdash") +
        geom_line(data= flowperiod1, mapping = aes(x= probperiod1, y=flowperiod1, col="2020-2040"),size=1)+
        geom_line(data= flowperiod2, mapping = aes(x= probperiod2, y=flowperiod2, col="2041-2061"),size=1)+
        geom_line(data= flowperiod3, mapping = aes(x= probperiod3, y=flowperiod3, col="2062-2082"),size=1)+
        geom_line(data= obs, mapping = aes(x= probobs, y=obs, col="Observation"),size=1)+
        scale_color_manual(values=c("green4", "deepskyblue4", "blueviolet", "black", "pink")) +
        xlab("Exceedance Probability (%)") +
        scale_y_log10()+
        ylab(bquote('Daily Discharge ( '*m^3/s/km^2*')')) +
        #scale_x_time(breaks=c("2","367", "733", "1098", "1463", "1828", "2194"), 
        #labels=c("2012", "2013", "2014", "2015", "2016", "2017", "2018")) + 
        #scale_y_time(breaks=c("-4","-8", "-12", "-16", "-18"), 
        #labels=c("-4", "-8", "-12", "-16", "-18")) +
        #theme_bw() +
        theme(text = element_text(size = 18)) +
        theme(legend.position = c(.95, -2),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6), legend.title = element_blank()
        )  
    

#######VEUVE2#######
    VEUVE= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/VEUVE-1990-2082.csv")
    flow<-na.omit(VEUVE)
    
    flowbase <- subset(flow, YEAR >=1993 & YEAR < 2020)
    write.csv(flowbase, "VEUVEflowbase.csv")
    flowbase= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/VEUVEflowbase.csv")
    
    #Sorting discharges in decreasing order
    flowbase<-sort(flowbase$Flow,decreasing=T)
    
    #Creating a data frame in which x column is hte percent of ie less than a specific time and
    #y is the correspondent discharge.
    
    flowbase<-data.frame(probbase=100/length(flowbase)*1:length(flowbase),flowbase=(flowbase/924.319))
    write.csv(flowbase, "VEUVE-FDC-BASEPERIOD.csv")
    
    ###########2020-2040#####################    
    flowperiod1 <- subset(flow, YEAR >=2020 & YEAR < 2041)
    write.csv(flowperiod1, "VEUVEflowperiod1.csv")
    flowperiod1= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/VEUVEflowperiod1.csv")
    
    flowperiod1<-sort(flowperiod1$Flow, decreasing=T)
    flowperiod1<-data.frame(probperiod1=100/length(flowperiod1)*1:length(flowperiod1),flowperiod1=(flowperiod1/924.319))
    write.csv(flowperiod1, "VEUVE-FDC-2020-2040.csv")
    
    ################2041-2061#################################
    flowperiod2 <- subset(flow, YEAR >=2041 & YEAR < 2062)
    write.csv(flowperiod2, "VEUVEflowperiod2.csv")
    flowperiod2= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/VEUVEflowperiod2.csv")
    
    flowperiod2<-sort(flowperiod2$Flow, decreasing=T)
    flowperiod2<-data.frame(probperiod2=100/length(flowperiod2)*1:length(flowperiod2),flowperiod2=(flowperiod2/924.319))
    write.csv(flowperiod2, "VEUVE-FDC-2041-2061.csv")
    
    ################2062-2082#################################
    
    flowperiod3 <- subset(flow, YEAR >=2062 & YEAR <= 2082)
    write.csv(flowperiod3, "VEUVEflowperiod3.csv")
    flowperiod3= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/VEUVEflowperiod3.csv")
    
    flowperiod3<-sort(flowperiod3$Flow, decreasing=T)
    flowperiod3<-data.frame(probperiod3=100/length(flowperiod3)*1:length(flowperiod3),flowperiod3=(flowperiod3/924.319))
    write.csv(flowperiod3, "VEUVE-FDC-2062-2082.csv")
    
    ################Observation#################################
    obs= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/Results/StreamAlPeriodAnalysis/ObservationFiles/VEUVE_OBS.csv")
    obs<-replace(obs,obs==-999,NA)
    obs<-na.omit(obs)
    obs<-sort(obs$FLOW, decreasing=T)
    obs<-data.frame(probobs=100/length(obs)*1:length(obs),obs=(obs/924.319))
    write.csv(obs, "VEUVE-FDC-obs.csv")
    ##############################################################
    #Plot
    plot(x = df$x, y = df$y, type= "l", log = "y",ylab="'Daily Discharge( '*m^3/s/km^2*')'", ylim = c(0.001, 0.2), col= c("black"), lty=1,  lwd=3, 
         xlab="Exceedance Probability (%)",
         cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.2, ) 
    #legend("top", legend=c("Upper Goose"),
    #lty=2, lwd=3,col= c("white"),  bty = "n", cex = 1.5)    #lty=line type, lwd=thickness, bty= eliminate outer black box of legend
    grid(nx=NULL, ny=NULL,col="lightgray", lty=21, lwd=0.5,equilogs=TRUE)
    
    #####LONGTERM-SHORTTERM PLOTS#####
    
    
    ggplot()+
        geom_line(data= flowbase, mapping = aes(x= probbase, y=flowbase, col="baseline"), size=1, linetype="longdash") +
        geom_line(data= flowperiod1, mapping = aes(x= probperiod1, y=flowperiod1, col="2020-2040"),size=1)+
        geom_line(data= flowperiod2, mapping = aes(x= probperiod2, y=flowperiod2, col="2041-2061"),size=1)+
        geom_line(data= flowperiod3, mapping = aes(x= probperiod3, y=flowperiod3, col="2062-2082"),size=1)+
        geom_line(data= obs, mapping = aes(x= probobs, y=obs, col="Observation"),size=1)+
        scale_color_manual(values=c("green4", "deepskyblue4", "blueviolet", "black", "pink")) +
        xlab("Exceedance Probability (%)") +
        scale_y_log10()+
        ylab(bquote('Daily Discharge ( '*m^3/s/km^2*')')) +
        #scale_x_time(breaks=c("2","367", "733", "1098", "1463", "1828", "2194"), 
        #labels=c("2012", "2013", "2014", "2015", "2016", "2017", "2018")) + 
        #scale_y_time(breaks=c("-4","-8", "-12", "-16", "-18"), 
        #labels=c("-4", "-8", "-12", "-16", "-18")) +
        #theme_bw() +
        theme(text = element_text(size = 18)) +
        theme(legend.position = c(.95, -2),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6), legend.title = element_blank()
        )  

    ############**********************###########################
    #######GA#######
    ############**********************###########################
    
    GA= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/GA-1990-2082.csv")
    flow<-na.omit(GA)
    
    flowbase <- subset(flow, YEAR >=1993 & YEAR < 2020)
    write.csv(flowbase, "GAflowbase.csv")
    flowbase= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/GAflowbase.csv")
    
    #Sorting discharges in decreasing order
    flowbase<-sort(flowbase$Flow,decreasing=T)
    
    #Creating a data frame in which x column is hte percent of ie less than a specific time and
    #y is the correspondent discharge.
    
    flowbase<-data.frame(probbase=100/length(flowbase)*1:length(flowbase),flowbase=(flowbase/2999))
    write.csv(flowbase, "GA-FDC-BASEPERIOD.csv")
    
    ###########2020-2040#####################    
    flowperiod1 <- subset(flow, YEAR >=2020 & YEAR < 2041)
    write.csv(flowperiod1, "GAflowperiod1.csv")
    flowperiod1= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/GAflowperiod1.csv")
    
    flowperiod1<-sort(flowperiod1$Flow, decreasing=T)
    flowperiod1<-data.frame(probperiod1=100/length(flowperiod1)*1:length(flowperiod1),flowperiod1=(flowperiod1/2999))
    write.csv(flowperiod1, "GA-FDC-2020-2040.csv")
    
    ################2041-2061#################################
    flowperiod2 <- subset(flow, YEAR >=2041 & YEAR < 2062)
    write.csv(flowperiod2, "GAflowperiod2.csv")
    flowperiod2= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/GAflowperiod2.csv")
    
    flowperiod2<-sort(flowperiod2$Flow, decreasing=T)
    flowperiod2<-data.frame(probperiod2=100/length(flowperiod2)*1:length(flowperiod2),flowperiod2=(flowperiod2/2999))
    write.csv(flowperiod2, "GA-FDC-2041-2061.csv")
    
    ################2062-2082#################################
    
    flowperiod3 <- subset(flow, YEAR >=2062 & YEAR <= 2082)
    write.csv(flowperiod3, "GAflowperiod3.csv")
    flowperiod3= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/GAflowperiod3.csv")
    
    flowperiod3<-sort(flowperiod3$Flow, decreasing=T)
    flowperiod3<-data.frame(probperiod3=100/length(flowperiod3)*1:length(flowperiod3),flowperiod3=(flowperiod3/2999))
    write.csv(flowperiod3, "GA-FDC-2062-2082.csv")
    
    ################Observation#################################
    obs= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/Results/StreamAlPeriodAnalysis/ObservationFiles/GA_OBS.csv")
    obs<-replace(obs,obs==-999,NA)
    obs<-na.omit(obs)
    obs<-sort(obs$FLOW, decreasing=T)
    obs<-data.frame(probobs=100/length(obs)*1:length(obs),obs=(obs/2999))
    write.csv(obs, "GA-FDC-obs.csv")
    ##############################################################
    #Plot
    plot(x = df$x, y = df$y, type= "l", log = "y",ylab="'Daily Discharge( '*m^3/s/km^2*')'", ylim = c(0.001, 0.2), col= c("black"), lty=1,  lwd=3, 
         xlab="Exceedance Probability (%)",
         cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.2, ) 
    #legend("top", legend=c("Upper Goose"),
    #lty=2, lwd=3,col= c("white"),  bty = "n", cex = 1.5)    #lty=line type, lwd=thickness, bty= eliminate outer black box of legend
    grid(nx=NULL, ny=NULL,col="lightgray", lty=21, lwd=0.5,equilogs=TRUE)
    
    #####LONGTERM-SHORTTERM PLOTS#####
    
    
    ggplot()+
        geom_line(data= flowbase, mapping = aes(x= probbase, y=flowbase, col="baseline"), size=1, linetype="longdash") +
        geom_line(data= flowperiod1, mapping = aes(x= probperiod1, y=flowperiod1, col="2020-2040"),size=1)+
        geom_line(data= flowperiod2, mapping = aes(x= probperiod2, y=flowperiod2, col="2041-2061"),size=1)+
        geom_line(data= flowperiod3, mapping = aes(x= probperiod3, y=flowperiod3, col="2062-2082"),size=1)+
        geom_line(data= obs, mapping = aes(x= probobs, y=obs, col="Observation"),size=1)+
        scale_color_manual(values=c("green4", "deepskyblue4", "blueviolet", "black", "pink")) +
        xlab("Exceedance Probability (%)") +
        scale_y_log10()+
        ylab(bquote('Daily Discharge ( '*m^3/s/km^2*')')) +
        #scale_x_time(breaks=c("2","367", "733", "1098", "1463", "1828", "2194"), 
        #labels=c("2012", "2013", "2014", "2015", "2016", "2017", "2018")) + 
        #scale_y_time(breaks=c("-4","-8", "-12", "-16", "-18"), 
        #labels=c("-4", "-8", "-12", "-16", "-18")) +
        #theme_bw() +
        theme(text = element_text(size = 18)) +
        theme(legend.position = c(.95, -2),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6), legend.title = element_blank()
        ) 
    
#######LaVase#######
    LV= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurves.input/LaVase_2012_2018.csv")
    flow<-na.omit(LV)
    
    #Sorting discharges in decreasing order
    flow<-sort(LV$Flow,decreasing=T)
    
    #Creating a data frame in which x column is hte percent of ie less than a specific time and
    #y is the correspondent discharge.
    df<-data.frame(x=100/length(flow)*1:length(flow),y=(flow/84))
    
    #Plot
    plot(x = df$x, y = df$y, type= "l", log = "y",ylab="Daily Discharge(m3/s/km2)", col= c("black"), lty=1,  lwd=3, 
         xlab="Exceedance Probability (%)",
         cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.2, ) 
    #legend("top", legend=c("Upper Goose"),
    #lty=2, lwd=3,col= c("white"),  bty = "n", cex = 1.5)    #lty=line type, lwd=thickness, bty= eliminate outer black box of legend
    grid(nx=NULL, ny=NULL,col="lightgray", lty=21, lwd=0.5,equilogs=TRUE)
    
    ggplot(df)+
        geom_line(mapping = aes(x= x, y=y, col="long-term FDC"), size=0.75) +
        geom_point(mapping = aes(x= x, y=y, col="2012_2018 FDC"), shape=1, size=2, stroke = 1)+
        scale_color_manual(values=c("brown3", "black")) +
        xlab("Exceedance Probability (%)") +
        scale_y_log10()+
        ylab(bquote('Daily Discharge(m3/s/km2)')) +
        #scale_x_time(breaks=c("2","367", "733", "1098", "1463", "1828", "2194"), 
        #labels=c("2012", "2013", "2014", "2015", "2016", "2017", "2018")) + 
        #scale_y_time(breaks=c("-4","-8", "-12", "-16", "-18"), 
        #labels=c("-4", "-8", "-12", "-16", "-18")) +
        theme_bw() +
        theme(text = element_text(size = 18)) +
        theme(legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6), legend.title = element_blank()
        )
    
#######LittleSturgeon#######
    LS= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/LS-1990-2082.csv")
    flow<-na.omit(LS)
    
    flowbase <- subset(flow, YEAR >=1992 & YEAR < 2020)
    write.csv(flowbase, "LSflowbase.csv")
    flowbase= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/LSflowbase.csv")
    
    #Sorting discharges in decreasing order
    flowbase<-sort(flowbase$Flow,decreasing=T)
    
    #Creating a data frame in which x column is hte percent of ie less than a specific time and
    #y is the correspondent discharge.
    
    flowbase<-data.frame(probbase=100/length(flowbase)*1:length(flowbase),flowbase=(flowbase/197))
    write.csv(flowbase, "LS-FDC-BASEPERIOD.csv")
    
    ###########2020-2040#####################    
    flowperiod1 <- subset(flow, YEAR >=2020 & YEAR < 2041)
    write.csv(flowperiod1, "LSflowperiod1.csv")
    flowperiod1= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/LSflowperiod1.csv")
    
    flowperiod1<-sort(flowperiod1$Flow, decreasing=T)
    flowperiod1<-data.frame(probperiod1=100/length(flowperiod1)*1:length(flowperiod1),flowperiod1=(flowperiod1/197))
    write.csv(flowperiod1, "LS-FDC-2020-2040.csv")
    
    ################2041-2061#################################
    flowperiod2 <- subset(flow, YEAR >=2041 & YEAR < 2062)
    write.csv(flowperiod2, "LSflowperiod2.csv")
    flowperiod2= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/LSflowperiod2.csv")
    
    flowperiod2<-sort(flowperiod2$Flow, decreasing=T)
    flowperiod2<-data.frame(probperiod2=100/length(flowperiod2)*1:length(flowperiod2),flowperiod2=(flowperiod2/197))
    write.csv(flowperiod2, "LS-FDC-2041-2061.csv")
    
    ################2062-2082#################################
    
    flowperiod3 <- subset(flow, YEAR >=2062 & YEAR <= 2082)
    write.csv(flowperiod3, "LSflowperiod3.csv")
    flowperiod3= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurve/LSflowperiod3.csv")
    
    flowperiod3<-sort(flowperiod3$Flow, decreasing=T)
    flowperiod3<-data.frame(probperiod3=100/length(flowperiod3)*1:length(flowperiod3),flowperiod3=(flowperiod3/197))
    write.csv(flowperiod3, "LS-FDC-2062-2082.csv")
    
    ################Observation#################################
    obs= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/Results/StreamAlPeriodAnalysis/ObservationFiles/LS_OBS.csv")
    obs<-replace(obs,obs==-999,NA)
    obs<-na.omit(obs)
    obs<-sort(obs$FLOW, decreasing=T)
    obs<-data.frame(probobs=100/length(obs)*1:length(obs),obs=(obs/197))
    write.csv(obs, "LS-FDC-obs.csv")
    ##############################################################
    #Plot
    plot(x = df$x, y = df$y, type= "l", log = "y",ylab="'Daily Discharge( '*m^3/s/km^2*')'", ylim = c(0.001, 0.2), col= c("black"), lty=1,  lwd=3, 
         xlab="Exceedance Probability (%)",
         cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.2, ) 
    #legend("top", legend=c("Upper Goose"),
    #lty=2, lwd=3,col= c("white"),  bty = "n", cex = 1.5)    #lty=line type, lwd=thickness, bty= eliminate outer black box of legend
    grid(nx=NULL, ny=NULL,col="lightgray", lty=21, lwd=0.5,equilogs=TRUE)
    
    #####LONGTERM-SHORTTERM PLOTS#####
    
    
    ggplot()+
        geom_line(data= flowbase, mapping = aes(x= probbase, y=flowbase, col="baseline"), size=1, linetype="longdash") +
        geom_line(data= flowperiod1, mapping = aes(x= probperiod1, y=flowperiod1, col="2020-2040"),size=1)+
        geom_line(data= flowperiod2, mapping = aes(x= probperiod2, y=flowperiod2, col="2041-2061"),size=1)+
        geom_line(data= flowperiod3, mapping = aes(x= probperiod3, y=flowperiod3, col="2062-2082"),size=1)+
        #geom_line(data= obs, mapping = aes(x= probobs, y=obs, col="Observation"),size=1)+
        scale_color_manual(values=c("green4", "deepskyblue4", "blueviolet", "black", "pink")) +
        xlab("Exceedance Probability (%)") +
        scale_y_log10()+
        ylab(bquote('Daily Discharge ( '*m^3/s/km^2*')')) +
        #scale_x_time(breaks=c("2","367", "733", "1098", "1463", "1828", "2194"), 
        #labels=c("2012", "2013", "2014", "2015", "2016", "2017", "2018")) + 
        #scale_y_time(breaks=c("-4","-8", "-12", "-16", "-18"), 
        #labels=c("-4", "-8", "-12", "-16", "-18")) +
        #theme_bw() +
        theme(text = element_text(size = 18)) +
        theme(legend.position = c(.95, -2),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6), legend.title = element_blank()
        )  

#######SouthRiver#######
    SO= read.csv("C:/Ph.D/Thesis/SNF.Data/SNF Data Package/R codes/FlowDurationCurves.input/SouthRiver_2012_2018.csv")
    flow<-na.omit(SO)
    
    #Sorting discharges in decreasing order
    flow<-sort(SO$Flow,decreasing=T)
    
    #Creating a data frame in which x column is hte percent of ie less than a specific time and
    #y is the correspondent discharge.
    df<-data.frame(x=100/length(flow)*1:length(flow),y=(flow/706.3))
    
    #Plot
    plot(x = df$x, y = df$y, type= "l", log = "y",ylab="Daily Discharge(m3/s/km2)", col= c("black"), lty=1,  lwd=3, 
         xlab="Exceedance Probability (%)",
         cex.lab=1.2, cex.axis=1.2, cex.main=1.5, cex.sub=1.2, ) 
    #legend("top", legend=c("Upper Goose"),
    #lty=2, lwd=3,col= c("white"),  bty = "n", cex = 1.5)    #lty=line type, lwd=thickness, bty= eliminate outer black box of legend
    grid(nx=NULL, ny=NULL,col="lightgray", lty=21, lwd=0.5,equilogs=TRUE)
    
    ggplot(df)+
        geom_line(mapping = aes(x= x, y=y, col="long-term FDC"), size=0.75) +
        geom_point(mapping = aes(x= x, y=y, col="2012_2018 FDC"), shape=1, size=2, stroke = 1)+
        scale_color_manual(values=c("brown3", "black")) +
        xlab("Exceedance Probability (%)") +
        scale_y_log10()+
        ylab(bquote('Daily Discharge(m3/s/km2)')) +
        #scale_x_time(breaks=c("2","367", "733", "1098", "1463", "1828", "2194"), 
        #labels=c("2012", "2013", "2014", "2015", "2016", "2017", "2018")) + 
        #scale_y_time(breaks=c("-4","-8", "-12", "-16", "-18"), 
        #labels=c("-4", "-8", "-12", "-16", "-18")) +
        theme_bw() +
        theme(text = element_text(size = 18)) +
        theme(legend.position = c(.95, .95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6), legend.title = element_blank()
        )
    
    

