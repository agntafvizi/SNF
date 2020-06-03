##BASEFLOW SEPARATION##
#Install required packages#

install.packages("EcoHydRology")
install.packages("dplyr")

library(EcoHydRology)
library(dplyr)

#Reading the dataset#
BASEFLOW= read.csv("D:/Arghavan/Ph.D-20190508T193213Z-001/R/Muskoka/Baseflow/Threthewey.csv") 
View(BASEFLOW)

#Preparing the format of "Date"#
BASEFLOW <- BASEFLOW %>%
  transmute(DATE = as.Date(BASEFLOW$Date, format = "%m/%d/%y"), streamflow)

#baseflow separation#
bfs<-BaseflowSeparation(BASEFLOW$streamflow, filter_parameter = 0.925, passes=3)
bfs #The first column is baseflow while the second column is quick flow#

#Add the info of baseflow separation to the original dataset#
BASEFLOW<- BASEFLOW %>%
  mutate(DATE,streamflow, bfs$bt,bfs$qft)

#save the results to a text file#
m= write.table(BASEFLOW, file = "results.txt", sep = "\t",
               row.names = TRUE, col.names = NA)

#graph the hydrograph of streamflow and baseflow#
hydrograph(streamflow= BASEFLOW[,2], streamflow2=BASEFLOW[,3], 
           timeSeries = BASEFLOW[,1], 
           S.units = "m3s")
