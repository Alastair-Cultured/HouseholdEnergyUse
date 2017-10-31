#-----------------------Loading Data-----------------------#
setwd("/Users/alastairparker/Desktop/Masters/CITS4009/Project")

#importing the dataset
bah.orig=read.table(file='household_power_consumption.txt',header = TRUE,sep = ';')
bah.cleaned <- bah.orig
str(bah.cleaned)

#-----------------------Cleaning Data-----------------------#
#Ignoring observations with missing data
bah.cleaned <- na.omit(bah.cleaned)

#general scrub
bah.cleaned$Global_active_power <- as.numeric(as.character(bah.cleaned$Global_active_power))
bah.cleaned$Global_reactive_power <- as.numeric(as.character(bah.cleaned$Global_reactive_power))
bah.cleaned$Voltage <- as.numeric(as.character(bah.cleaned$Voltage))
bah.cleaned$Global_intensity <- as.numeric(as.character(bah.cleaned$Global_intensity))
bah.cleaned$Sub_metering_1 <- as.integer(as.character(bah.cleaned$Sub_metering_1))
bah.cleaned$Sub_metering_2 <- as.integer(as.character(bah.cleaned$Sub_metering_2))
bah.cleaned$Sub_metering_3 <- as.integer(as.character(bah.cleaned$Sub_metering_3))
bah.cleaned$datetime <- as.POSIXct(paste(bah.cleaned$Date, bah.cleaned$Time), format="%d/%m/%Y %H:%M:%S")
bah.cleaned$Date <- as.Date(bah.cleaned$Date, format = "%d/%m/%Y")
str(bah.cleaned)

#Logarithm for GAP + GRP
bah.cleaned$log.gap <- log(bah.cleaned$Global_active_power)
bah.cleaned$log.grp <- log(bah.cleaned$Global_reactive_power)

#Grouping dataset by time interval
bah.hour <- subset(bah.cleaned, bah.cleaned$Date >= "2007-02-01" & bah.cleaned$Date <= "2007-03-01")
bah.day <- subset(bah.cleaned, bah.cleaned$Date >= "2007-01-01" & bah.cleaned$Date <= "2008-01-01")
bah.month <- bah.cleaned
bah.month2 <- subset(bah.cleaned, bah.cleaned$Date >= "2007-01-01")

#Putting together the summary table -> gives cleaner visualisation
bah.hour.summary <- as.data.frame(aggregate(list(GAP = bah.hour$Global_active_power,GRP = bah.hour$Global_reactive_power,VOL = bah.hour$Voltage, GI=bah.hour$Global_intensity, SM1=bah.hour$Sub_metering_1,SM2=bah.hour$Sub_metering_2,SM3=bah.hour$Sub_metering_3),list(hourofday = cut(bah.hour$datetime, "1 hour")), mean))
str(bah.hour.summary)
bah.day.summary <- as.data.frame(aggregate(list(GAP = bah.day$Global_active_power,GRP = bah.day$Global_reactive_power,VOL = bah.day$Voltage, GI=bah.day$Global_intensity, SM1=bah.day$Sub_metering_1,SM2=bah.day$Sub_metering_2,SM3=bah.day$Sub_metering_3),list(hourofday = cut(bah.day$datetime, "1 day")), mean))
str(bah.day.summary)
bah.month.summary <- as.data.frame(aggregate(list(GAP = bah.month$Global_active_power,GRP = bah.month$Global_reactive_power,VOL = bah.month$Voltage, GI=bah.month$Global_intensity, SM1=bah.month$Sub_metering_1,SM2=bah.month$Sub_metering_2,SM3=bah.month$Sub_metering_3),list(hourofday = cut(bah.month$datetime, "1 day")), mean))
str(bah.month.summary)
bah.month2.summary <- as.data.frame(aggregate(list(GAP = bah.month2$Global_active_power,GRP = bah.month2$Global_reactive_power,VOL = bah.month2$Voltage, GI=bah.month2$Global_intensity, SM1=bah.month2$Sub_metering_1,SM2=bah.month2$Sub_metering_2,SM3=bah.month2$Sub_metering_3),list(hourofday = cut(bah.month2$datetime, "1 month")), mean))
str(bah.month2.summary)

#scaling the data for models needing it
bah.hour.summary.sc <- scale(bah.hour.summary[c(2,3,4,5,6,7,8)])
bah.hour.summary.sc <- as.matrix(bah.hour.summary.sc)
bah.day.summary.sc <- as.data.frame(scale(bah.day.summary[c(2,3,4,5,6,7,8)]))
bah.day.summary.sc <- as.matrix(bah.day.summary.sc)
bah.month.summary.sc <- as.data.frame(scale(bah.month.summary[c(2,3,4,5,6,7,8)]))
bah.month.summary.sc <- as.matrix(bah.month.summary.sc)

#-----------------------Simple Data Exploration -----------------------#
#-----------------------Univariate------------------------------#
#----------'Day' perspective----------
par(mfrow=c(2,4))
for (i in c(2,3,4,5,6,7,8)){
  mylab <- colnames(bah.hour.summary)[i]
  hist(bah.hour.summary[,i], xlab = mylab, main = '')
}

#Boxplots
par(mfrow=c(2,4))
for (i in c(2,3,4,5,6,7,8)){
  mylab <- colnames(bah.hour.summary)[i]
  boxplot(bah.hour.summary[,i], xlab = mylab, main = '')
}

#----------'Week' perspective----------
#Histograms
par(mfrow=c(2,4))
for (i in c(2,3,4,5,6,7,8)){
  mylab <- colnames(bah.day.summary)[i]
  hist(bah.day.summary[,i], xlab = mylab, main = '')
}

#Boxplots
par(mfrow=c(2,4))
for (i in c(2,3,4,5,6,7,8)){
  mylab <- colnames(bah.day.summary)[i]
  boxplot(bah.day.summary[,i], xlab = mylab, main = '')
}

#----------'Month' perspective----------
#Histograms
par(mfrow=c(2,4))
for (i in c(2,3,4,5,6,7,8)){
  mylab <- colnames(bah.month.summary)[i]
  hist(bah.month.summary[,i], xlab = mylab, main = '')
}

#Boxplots
par(mfrow=c(2,4))
for (i in c(2,3,4,5,6,7,8)){
  mylab <- colnames(bah.month.summary)[i]
  boxplot(bah.month.summary[,i], xlab = mylab, main = '')
}
#--------Month2--------
#Histograms
par(mfrow=c(2,4))
for (i in c(2,3,4,5,6,7,8)){
  mylab <- colnames(bah.month2.summary)[i]
  hist(bah.month2.summary[,i], xlab = mylab, main = '')
}

#Boxplots
par(mfrow=c(2,4))
for (i in c(2,3,4,5,6,7,8)){
  mylab <- colnames(bah.month.summary)[i]
  boxplot(bah.month.summary[,i], xlab = mylab, main = '')
}

#-----------------------Bivariate------------------------------#
library("PerformanceAnalytics")
library("corrr")
library("corrplot")
library(ggplot2)

#-----------------Corrplot-----------------#
#----------'Day' Perspective---------
par(mfrow=c(1,1))
corrplot.mixed(cor(bah.hour.summary[c(2,3,4,5,6,7,8)]), order="hclust", tl.col="black")
#----------'Week' Perspective---------
par(mfrow=c(1,1))
#Interesting why Voltage is no longer a massive neg association
corrplot.mixed(cor(bah.day.summary[c(2,3,4,5,6,7,8)]), order="hclust", tl.col="black")
#----------'Month' Perspective---------
par(mfrow=c(1,1))
#Why is voltage positively associated with GAP outside of those days. 
corrplot.mixed(cor(bah.month.summary[c(2,3,4,5,6,7,8)]), order="hclust", tl.col="black")


#------------------------Time Series Visualisation--------------------------#
#------------------------Variable Visualisation Time Series
library(ggplot2)
#Clean the Date VAR
bah.hour.summary$hourofday <- as.POSIXct(bah.hour.summary$hourofday, format="%Y-%m-%d %H:%M:%S")
bah.day.summary$hourofday <- as.POSIXct(bah.day.summary$hourofday, format="%Y-%m-%d %H:%M:%S")
bah.month.summary$hourofday <- as.POSIXct(bah.month.summary$hourofday, format="%Y-%m-%d %H:%M:%S")

#-------'Day overview'
#Global Active Power
ggplot(data=bah.hour.summary, aes(x=hourofday, y=GAP, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = 'Feb 2007 Monthly Overview (Hourly unit measurement)', date_breaks = '1 day', date_labels = '%d') +
  scale_y_continuous(name="Global Active Power (Mean Kilowatts/min)", labels = scales::comma)
#Global Reactive Power
ggplot(data=bah.hour.summary, aes(x=hourofday, y=GRP, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = 'Feb 2007 Monthly Overview (Hourly unit measurement)', date_breaks = '1 day', date_labels = '%d') +
  scale_y_continuous(name="Global Reactive Power (Mean Kilowatts/min)", labels = scales::comma)
#Voltage
ggplot(data=bah.hour.summary, aes(x=hourofday, y=VOL, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = 'Feb 2007 Monthly Overview (Hourly unit measurement)', date_breaks = '1 day', date_labels = '%d') +
  scale_y_continuous(name="Voltage (Mean Volts/min)", labels = scales::comma)
#Global Intensity
ggplot(data=bah.hour.summary, aes(x=hourofday, y=GI, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = 'Feb 2007 Monthly Overview (Hourly unit measurement)', date_breaks = '1 day', date_labels = '%d') +
  scale_y_continuous(name="Global Intensity (Mean Ampere/min)", labels = scales::comma)
#Sub Meter 1 - Kitchen
ggplot(data=bah.hour.summary, aes(x=hourofday, y=SM1, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = 'Feb 2007 Monthly Overview (Hourly unit measurement)', date_breaks = '1 day', date_labels = '%d') +
  scale_y_continuous(name="Sub Meter 1 (Mean Kilowatts/min)", labels = scales::comma)
#Sub Meter 2 - Laundry
ggplot(data=bah.hour.summary, aes(x=hourofday, y=SM2, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = 'Feb 2007 Monthly Overview (Hourly unit measurement)', date_breaks = '1 day', date_labels = '%d') +
  scale_y_continuous(name="Sub Meter 2 (Mean Kilowatts/min)", labels = scales::comma)
#Sub Meter 3 - water heater & Air Con
ggplot(data=bah.hour.summary, aes(x=hourofday, y=SM3, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = 'Feb 2007 Monthly Overview (Hourly unit measurement)', date_breaks = '1 day', date_labels = '%d') +
  scale_y_continuous(name="Sub Meter 3 (Mean Kilowatts/min)", labels = scales::comma)

#-------'Week overview'
#Global Active Power
ggplot(data=bah.day.summary, aes(x=hourofday, y=GAP, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = '2007 Overview (Daily unit of measurement)', date_breaks = '1 month', date_labels = '%m') +
  scale_y_continuous(name="Global Active Power (Mean Kilowatts/min)", labels = scales::comma)
#Global Reactive Power
ggplot(data=bah.day.summary, aes(x=hourofday, y=GRP, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = '2007 Overview (Daily unit of measurement)', date_breaks = '1 month', date_labels = '%m') +
  scale_y_continuous(name="Global Reactive Power (Mean Kilowatts/min)", labels = scales::comma)
#This explains why voltage no longer has a neg affect, for some reason is dropped for a few months in winter-ish
#Voltage
ggplot(data=bah.day.summary, aes(x=hourofday, y=VOL, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = '2007 Overview (Daily unit of measurement)', date_breaks = '1 month', date_labels = '%m') +
  scale_y_continuous(name="Voltage (Mean Volts/min)", labels = scales::comma)
#Global Intensity
ggplot(data=bah.day.summary, aes(x=hourofday, y=GI, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = '2007 Overview (Daily unit of measurement)', date_breaks = '1 month', date_labels = '%m') +
  scale_y_continuous(name="Global Intensity (Mean Ampere/min)", labels = scales::comma)
#Sub Meter 1 - Kitchen
ggplot(data=bah.day.summary, aes(x=hourofday, y=SM1, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = '2007 Overview (Daily unit of measurement)', date_breaks = '1 month', date_labels = '%m') +
  scale_y_continuous(name="Sub Meter 1 (Mean Kilowatts/min)", labels = scales::comma)
#Sub Meter 2 - Laundry
ggplot(data=bah.day.summary, aes(x=hourofday, y=SM2, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = '2007 Overview (Daily unit of measurement)', date_breaks = '1 month', date_labels = '%m') +
  scale_y_continuous(name="Sub Meter 2 (Mean Kilowatts/min)", labels = scales::comma)
#Sub Meter 3 - water heater & Air Con
ggplot(data=bah.day.summary, aes(x=hourofday, y=SM3, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = '2007 Overview (Daily unit of measurement)', date_breaks = '1 month', date_labels = '%m') +
  scale_y_continuous(name="Sub Meter 3 (Mean Kilowatts/min)", labels = scales::comma)

#-------'Month overview'
#Global Active Power
ggplot(data=bah.month.summary, aes(x=hourofday, y=GAP, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = 'Dataset Overview (Daily unit of measurement)', date_breaks = '3 months', date_labels = '%m') +
  scale_y_continuous(name="Global Active Power (Mean Kilowatts/min)", labels = scales::comma)
#Global Reactive Power
ggplot(data=bah.month.summary, aes(x=hourofday, y=GRP, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = 'Dataset Overview (Daily unit of measurement)', date_breaks = '3 month', date_labels = '%m') +
  scale_y_continuous(name="Global Reactive Power (Mean Kilowatts/min)", labels = scales::comma)
#Even though voltage seasonally looks to be inverse to GAP... for some reason
#Sometimes it just dips down. 
#Voltage
ggplot(data=bah.month.summary, aes(x=hourofday, y=VOL, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = 'Dataset Overview (Daily unit of measurement)', date_breaks = '3 month', date_labels = '%m') +
  scale_y_continuous(name="Voltage (Mean Volts/min)", labels = scales::comma)
#Global Intensity
ggplot(data=bah.month.summary, aes(x=hourofday, y=GI, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = 'Dataset Overview (Daily unit of measurement)', date_breaks = '3 month', date_labels = '%m') +
  scale_y_continuous(name="Global Intensity (Mean Ampere/min)", labels = scales::comma)
#Sub Meter 1 - Kitchen
ggplot(data=bah.month.summary, aes(x=hourofday, y=SM1, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = 'Dataset Overview (Daily unit of measurement)', date_breaks = '3 month', date_labels = '%m') +
  scale_y_continuous(name="Sub Meter 1 (Mean Kilowatts/min)", labels = scales::comma)
#Sub Meter 2 - Laundry
ggplot(data=bah.month.summary, aes(x=hourofday, y=SM2, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = 'Dataset Overview (Daily unit of measurement)', date_breaks = '3 month', date_labels = '%m') +
  scale_y_continuous(name="Sub Meter 2 (Mean Kilowatts/min)", labels = scales::comma)
#Sub Meter 3 - water heater & Air Con
ggplot(data=bah.month.summary, aes(x=hourofday, y=SM3, group=1)) +
  geom_line() + theme_minimal() + scale_x_datetime(name = 'Dataset Overview (Daily unit of measurement)', date_breaks = '3 month', date_labels = '%m') +
  scale_y_continuous(name="Sub Meter 3 (Mean Kilowatts/min)", labels = scales::comma)

#------------------------Extended Data Exploration--------------------------#
library(tidyverse)
library(cluster)
library(factoextra)
library(ggplot2)
library(ggpubr)

#-------------------------------------Kmeans---------------------------------#
#--------------Hour unit of measurement perspective
set.seed(1)
#Get a first initial feel - Summary Only, dataset too big otherwise
distance.sum <- get_dist(bah.hour.summary)
#Get a feel for patterns
fviz_dist(distance.sum, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#for first run just trying with k=2
k2.sum <- kmeans(bah.hour.summary.sc, centers = 2, nstart = 25)
str(k2.sum)
#initial look -> it's actually pretty simple to just explain it as 'high vs low' energy
k2.sum

#Visualization of the cluster
fviz_cluster(k2.sum, data = bah.hour.summary.sc)

#Summary
k3.sum <- kmeans(bah.hour.summary.sc, centers = 3, nstart = 25)
k4.sum <- kmeans(bah.hour.summary.sc, centers = 4, nstart = 25)
k5.sum <- kmeans(bah.hour.summary.sc, centers = 5, nstart = 25)
k6.sum <- kmeans(bah.hour.summary.sc, centers = 6, nstart = 25)
k7.sum <- kmeans(bah.hour.summary.sc, centers = 7, nstart = 25)
k8.sum <- kmeans(bah.hour.summary.sc, centers = 8, nstart = 25)
k9.sum <- kmeans(bah.hour.summary.sc, centers = 9, nstart = 25)
k10.sum <- kmeans(bah.hour.summary.sc, centers = 10, nstart = 25)

#Summary plots for comparison
p2.sum <- fviz_cluster(k2.sum, geom = "point", data = bah.hour.summary.sc) + ggtitle("k = 2")
p3.sum <- fviz_cluster(k3.sum, geom = "point",  data = bah.hour.summary.sc) + ggtitle("k = 3")
p4.sum <- fviz_cluster(k4.sum, geom = "point",  data = bah.hour.summary.sc) + ggtitle("k = 4")
p5.sum <- fviz_cluster(k5.sum, geom = "point",  data = bah.hour.summary.sc) + ggtitle("k = 5")
p6.sum <- fviz_cluster(k6.sum, geom = "point",  data = bah.hour.summary.sc) + ggtitle("k = 6")
p7.sum <- fviz_cluster(k7.sum, geom = "point",  data = bah.hour.summary.sc) + ggtitle("k = 7")
p8.sum <- fviz_cluster(k8.sum, geom = "point",  data = bah.hour.summary.sc) + ggtitle("k = 8")
p9.sum <- fviz_cluster(k9.sum, geom = "point",  data = bah.hour.summary.sc) + ggtitle("k = 9")
p10.sum <- fviz_cluster(k10.sum, geom = "point",  data = bah.hour.summary.sc) + ggtitle("k = 10")

library(gridExtra)
#looks like K3 would be the best as far as interpretation
grid.arrange(p2.sum,p3.sum,p4.sum,p5.sum,p6.sum,p7.sum,p8.sum,p9.sum,p10.sum, nrow = 3)

#elbow method => 3 looks good
hr.elbow <- fviz_nbclust(bah.hour.summary.sc, kmeans, method = "wss")

#AVG silhoette method => 4 (but 3 also looks good)
hr.silo <- fviz_nbclust(bah.hour.summary.sc, kmeans, method = "silhouette")

#Gap statistic method => 5
gap_stat <- clusGap(bah.hour.summary.sc, FUN = kmeans, nstart = 25, K.max = 20, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
hr.gappo <- fviz_gap_stat(gap_stat)

grid.arrange(hr.elbow,hr.silo,hr.gappo, nrow = 2)

#This tells a nice story: 1=>No major power usage, 2=> aircon/water heater on
#3=> when using the laundry/kitchen
k3.sum
x.sum <- data.frame("cluster" = c("Cluster 1","Cluster 2", "Cluster 3"), "GAP" = c(0.570,-0.790,1.698), 
                         "GRP" = c(-0.107, -0.293,1.655), "VOL" = c(-0.387,0.528,-1.114), 
                         "GI" =c(0.556,-0.784,1.716), "SM1" = c(-0.070,-0.285,1.503), 
                         "SM2" =c(-0.199,-0.257,1.792), "SM3" =c(0.771,-0.743,0.839))
str(x.sum)

VOL.sum <- ggplot(x.sum, aes(cluster, VOL, fill=cluster)) + geom_bar(stat="identity", position="identity") + ylab("VOL") + xlab("Cluster") + scale_fill_discrete(name="Cluster", labels=c("1", "2","3")) + ylim(-1.5,2)
GI.sum <- ggplot(x.sum, aes(cluster, GI, fill=cluster)) + geom_bar(stat="identity", position="identity") + ylab("GI") + xlab("Cluster") + scale_fill_discrete(name="Cluster", labels=c("1", "2","3")) + ylim(-1.5,2)
SM1.sum <- ggplot(x.sum, aes(cluster, SM1, fill=cluster)) + geom_bar(stat="identity", position="identity") + ylab("SM1") + xlab("Cluster") + scale_fill_discrete(name="Cluster", labels=c("1", "2","3")) + ylim(-1.5,2)
SM2.sum <- ggplot(x.sum, aes(cluster, SM2, fill=cluster)) + geom_bar(stat="identity", position="identity") + ylab("SM2") + xlab("Cluster") + scale_fill_discrete(name="Cluster", labels=c("1", "2","3")) + ylim(-1.5,2)
SM3.sum <- ggplot(x.sum, aes(cluster, SM3, fill=cluster)) + geom_bar(stat="identity", position="identity") + ylab("SM3") + xlab("Cluster") + scale_fill_discrete(name="Cluster", labels=c("1", "2","3")) + ylim(-1.5,2)
GAP.sum <- ggplot(x.sum, aes(cluster, GAP, fill=cluster)) + geom_bar(stat="identity", position="identity") + ylab("GAP") + xlab("Cluster") + scale_fill_discrete(name="Cluster", labels=c("1", "2","3")) + ylim(-1.5,2)
GRP.sum <- ggplot(x.sum, aes(cluster, GRP, fill=cluster)) + geom_bar(stat="identity", position="identity") + ylab("GRP") + xlab("Cluster") + scale_fill_discrete(name="Cluster", labels=c("1", "2","3")) + ylim(-1.5,2)

#visualise the three clusters
ggarrange(GAP.sum, GRP.sum, VOL.sum, GI.sum, SM1.sum, SM2.sum, SM3.sum, ncol = 3, nrow = 3)

#------'Day' unit of measurement Perspective ---NOT USED IN REPORT
set.seed(10)
#Get a first initial feel 
distance.month <- get_dist(bah.month.summary.sc)
#----- THIS TAKES A LITTLE WHILE---------Get a feel for patterns 
fviz_dist(distance.month, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#for first run just trying with k=2
k2.month <- kmeans(bah.month.summary.sc, centers = 2, nstart = 25)
str(k2.month)
#initial look -> it's actually pretty simple to just explain it as 'high vs low' energy
k2.month

#Visualization of the cluster
fviz_cluster(k2.month, data = bah.month.summary.sc)

#Plots
k3.month <- kmeans(bah.month.summary.sc, centers = 3, nstart = 25)
k4.month <- kmeans(bah.month.summary.sc, centers = 4, nstart = 25)
k5.month <- kmeans(bah.month.summary.sc, centers = 5, nstart = 25)
k6.month <- kmeans(bah.month.summary.sc, centers = 6, nstart = 25)
k7.month <- kmeans(bah.month.summary.sc, centers = 7, nstart = 25)
k8.month <- kmeans(bah.month.summary.sc, centers = 8, nstart = 25)
k9.month <- kmeans(bah.month.summary.sc, centers = 9, nstart = 25)
k10.month <- kmeans(bah.month.summary.sc, centers = 10, nstart = 25)

#Weeksum plots for comparison
p2.month <- fviz_cluster(k2.month, geom = "point", data = bah.month.summary.sc) + ggtitle("k = 2")
p3.month <- fviz_cluster(k3.month, geom = "point",  data = bah.month.summary.sc) + ggtitle("k = 3")
p4.month <- fviz_cluster(k4.month, geom = "point",  data = bah.month.summary.sc) + ggtitle("k = 4")
p5.month <- fviz_cluster(k5.month, geom = "point",  data = bah.month.summary.sc) + ggtitle("k = 5")
p6.month <- fviz_cluster(k6.month, geom = "point",  data = bah.month.summary.sc) + ggtitle("k = 6")
p7.month <- fviz_cluster(k7.month, geom = "point",  data = bah.month.summary.sc) + ggtitle("k = 7")
p8.month <- fviz_cluster(k8.month, geom = "point",  data = bah.month.summary.sc) + ggtitle("k = 8")
p9.month <- fviz_cluster(k9.month, geom = "point",  data = bah.month.summary.sc) + ggtitle("k = 9")
p10.month <- fviz_cluster(k10.month, geom = "point",  data = bah.month.summary.sc) + ggtitle("k = 10")

library(gridExtra)
#Looks like only 2/3 would be easy to explain
grid.arrange(p2.month,p3.month,p4.month,p5.month,p6.month,p7.month,p8.month,p9.month,p10.month, nrow = 3)

#elbow method => 2 or 3 works
fviz_nbclust(bah.month.summary.sc, kmeans, method = "wss")

#AVG silhoette method => 2
fviz_nbclust(bah.month.summary.sc, kmeans, method = "silhouette")

#-----TAKES A LITTLE WHILE-------Gap statistic method => 11?
gap_stat <- clusGap(bah.month.summary.sc, FUN = kmeans, nstart = 25, K.max = 20, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

#---------- 3 Clusters Chosen
k3.month
x.month <- data.frame("cluster" = c("Cluster 1","Cluster 2","Cluster 3"), "GAP" = c(1.138,0.189,-0.933), 
                      "GRP" = c(0.546,-0.488,0.191), "VOL" = c(-0.120,0.393,-0.357), 
                      "GI" =c(1.156,0.166,-0.920), "SM1" = c(1.039,-0.174,-0.470), 
                      "SM2" =c(1.158,-0.336,-0.367), "SM3" =c(0.616,0.465,-0.905))
str(x.month)

VOL.month <- ggplot(x.month, aes(cluster, VOL, fill=cluster)) + geom_bar(stat="identity", position="identity") + ylab("VOL") + xlab("Cluster") + scale_fill_discrete(name="Cluster", labels=c("1", "2", "3")) + ylim(-1,1.5)
GI.month <- ggplot(x.month, aes(cluster, GI, fill=cluster)) + geom_bar(stat="identity", position="identity") + ylab("GI") + xlab("Cluster") + scale_fill_discrete(name="Cluster", labels=c("1", "2", "3")) + ylim(-1,1.5)
SM1.month <- ggplot(x.month, aes(cluster, SM1, fill=cluster)) + geom_bar(stat="identity", position="identity") + ylab("SM1") + xlab("Cluster") + scale_fill_discrete(name="Cluster", labels=c("1", "2", "3")) + ylim(-1,1.5)
SM2.month <- ggplot(x.month, aes(cluster, SM2, fill=cluster)) + geom_bar(stat="identity", position="identity") + ylab("SM2") + xlab("Cluster") + scale_fill_discrete(name="Cluster", labels=c("1", "2", "3")) + ylim(-1,1.5)
SM3.month <- ggplot(x.month, aes(cluster, SM3, fill=cluster)) + geom_bar(stat="identity", position="identity") + ylab("SM3") + xlab("Cluster") + scale_fill_discrete(name="Cluster", labels=c("1", "2", "3")) + ylim(-1,1.5)
GAP.month <- ggplot(x.month, aes(cluster, GAP, fill=cluster)) + geom_bar(stat="identity", position="identity") + ylab("GAP") + xlab("Cluster") + scale_fill_discrete(name="Cluster", labels=c("1", "2", "3")) + ylim(-1,1.5)
GRP.month <- ggplot(x.month, aes(cluster, GRP, fill=cluster)) + geom_bar(stat="identity", position="identity") + ylab("GRP") + xlab("Cluster") + scale_fill_discrete(name="Cluster", labels=c("1", "2", "3")) + ylim(-1,1.5)

#visualise the three clusters
ggarrange(GAP.month, GRP.month, VOL.month, GI.month, SM1.month, SM2.month, SM3.month, ncol = 3, nrow = 3)

#-----Principle Component Analysis - NOT USED IN DOC ----#
#--------------Hour Perspective---------------#
library(FactoMineR)
library(factoextra)
library(corrplot)
library(cluster)
set.seed(2)

bah.hour.summary.sc.out <- prcomp(bah.hour.summary.sc, scale=TRUE)
names(bah.hour.summary.sc.out)
#Numerical Description
bah.hour.summary.sc.out$rotation

bah.hour.summary.sc.var=bah.hour.summary.sc.out$sdev^2
pve=bah.hour.summary.sc.var/sum(bah.hour.summary.sc.var)
pve
#probably about 3 (54%, 14%, 12%, 8%)

plot(cumsum(pve), 
     xlab="Principal Component ", 
     ylab=" Cumulative Proportion of Variance Explained ", 
     ylim=c(0,1), 
     type='b')
abline(h = 0.7, col = 'red') #2 PCs
abline(h = 0.8, col = 'red') #2 PCs
abline(h = 0.9, col = 'red') #4 PCs

#2 PCs
screeplot(bah.hour.summary.sc.out, type = 'lines')

#2 PCs
eig.val <- get_eigenvalue(bah.hour.summary.sc.out)
eig.val

#All look quite Sig.
fviz_pca_var(bah.hour.summary.sc.out, col.var = "black")
fviz_pca_var(bah.hour.summary.sc.out, col.var = "black", axes = c(3, 4))
fviz_pca_var(bah.hour.summary.sc.out, col.var = "black", axes = c(2, 3))

#4 here looks pretty alright 
var <- get_pca_var(bah.hour.summary.sc.out)
corrplot(var$contrib, is.corr=FALSE) 

#-------------'Day' Perspective---------
set.seed(11)

bah.month.summary.sc.out <- prcomp(bah.month.summary.sc, scale=TRUE)
names(bah.month.summary.sc.out)
#Numerical Description
bah.month.summary.sc.out$rotation

bah.month.summary.sc.var=bah.month.summary.sc.out$sdev^2
pve=bah.month.summary.sc.var/sum(bah.month.summary.sc.var)
pve
# Possibly about 4 PCs (46%, 18%, 13%, 11%)

plot(cumsum(pve), 
     xlab="Principal Component ", 
     ylab=" Cumulative Proportion of Variance Explained ", 
     ylim=c(0,1), 
     type='b')
abline(h = 0.7, col = 'red') #2 PCs
abline(h = 0.8, col = 'red') #3 PCs
abline(h = 0.9, col = 'red') #4 PCs

#2 PCs
screeplot(bah.month.summary.sc.out, type = 'lines')

#2 PCs
eig.val <- get_eigenvalue(bah.month.summary.sc.out)
eig.val

#they all seem to be going in the same direction
fviz_pca_var(bah.month.summary.sc.out, col.var = "black")
#Hmmm begin to lose significance a little, I'd probably say 2 is best, maybe 3
fviz_pca_var(bah.month.summary.sc.out, col.var = "black", axes = c(3, 4))
fviz_pca_var(bah.month.summary.sc.out, col.var = "black", axes = c(2, 3))

#Suggests 3/4 
var <- get_pca_var(bah.month.summary.sc.out)
corrplot(var$contrib, is.corr=FALSE) 

#Overall I prefer K-means 

#--------------SOM Neural Network-------------#
library(kohonen)
library(ggcorrplot)
library("PerformanceAnalytics")
library("corrr")
library(corrplot)

set.seed(22)
bah.month.summary.us <- as.matrix(bah.month.summary[,-9])
bah.month.summary.us <- as.matrix(bah.month.summary.us[,-1])

#12 chosen due to node size aim of 10
bah.month.summary.sc.som <- som(bah.month.summary.sc, grid = somgrid(12, 12, "hexagonal"), rlen=1000)
#doesn't look like it's convered
par(mfrow = c(2,2))
plot(bah.month.summary.sc.som, type = "changes", main = "Household Energy data: SOM")

colors <- function(n, alpha = 1) {
  rev(heat.colors(n, alpha))
}

#What's the frequencies like?
#Actually quite a nice spread, with no empty nodes
p2 <- plot(bah.month.summary.sc.som, type = "counts", palette.name = colors, heatkey = TRUE)

#-------How close are the nodes?-------
#Got some nodes quite far away from each other in the top right.
#combined with frequency there's just a few. 
plot(bah.month.summary.sc.som, type="dist.neighbours", main = "SOM neighbour distances", 
     palette.name = colors)

#Bait map  - Not using
#plot(bah.month.summary.sc.som, main = "Household Energy data")

#heatmaps for each VAR
var <- 1
var_unscaled <- aggregate(as.numeric(bah.month.summary.us[,var]), by=list(bah.month.summary.sc.som$unit.classif), FUN=mean, simplify=TRUE)[,2]
plot(bah.month.summary.sc.som, type = "property", property=var_unscaled, main=colnames(getCodes(bah.month.summary.sc.som))[var], palette.name=colors)

par(mfrow = c(2,4))
for (var in 1:dim(bah.month.summary.sc)[2]){
  var_unscaled <- aggregate(as.numeric(bah.month.summary.us[,var]), 
                            by=list(bah.month.summary.sc.som$unit.classif), FUN=mean, simplify=TRUE)[,2]
  plot(bah.month.summary.sc.som, type = "property", property=var_unscaled, 
       main=colnames(getCodes(bah.month.summary.sc.som))[var], palette.name=colors)
} 
#Heat maps look sick -> def talk about this in the big picture

#-------------------Time Series----------------------#
library(ggplot2)
library(fpp2)
library(zoo)
library(gridExtra)

# spilt into date and returns
dates <- bah.hour.summary$hourofday
values3 <- bah.hour.summary[,-1]
values4 <- bah.day.summary[,-1]
values5 <- bah.month2.summary[,-1]

# generate an ts object
?ts
bah.hour.summary.ts <- ts(values3, start = c(1, 1), frequency=24)
bah.hour.summary.ts
bah.day.summary.ts <- ts(values4, start = c(1, 1), frequency=7)
bah.day.summary.ts
bah.month.summary.ts <- ts(values5, start = 2007, frequency=12)
bah.month.summary.ts
#------------------------Day Perspective---------------------------------#
#-------------------Seasonal Plots---------------------------------#
#--------------GAP
hr.GAP <- ggseasonplot(bah.hour.summary.ts[,"GAP"], year.labels = TRUE,  year.labels.left = TRUE) + theme_minimal() +
  scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Global Active Power") + theme(plot.title = element_text(hjust = 0.5))
#--------------GRP
hr.GRP <- ggseasonplot(bah.hour.summary.ts[,"GRP"],year.labels = TRUE,  year.labels.left = TRUE) + theme_minimal() +
  scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Global Reactive Power") + theme(plot.title = element_text(hjust = 0.5))
#--------------VOL
hr.VOL <- ggseasonplot(bah.hour.summary.ts[,"VOL"],year.labels = TRUE,  year.labels.left = TRUE) + theme_minimal() +
  scale_y_continuous(name="Mean Volts/min", labels = scales::comma) +
  ggtitle("Voltage") + theme(plot.title = element_text(hjust = 0.5))
#--------------GI
hr.GI <- ggseasonplot(bah.hour.summary.ts[,"GI"],year.labels = TRUE,  year.labels.left = TRUE) + theme_minimal() +
  scale_y_continuous(name="Mean Ampere/min", labels = scales::comma) +
  ggtitle("Global Intensity") + theme(plot.title = element_text(hjust = 0.5))
#--------------SM1
hr.SM1 <- ggseasonplot(bah.hour.summary.ts[,"SM1"],year.labels = TRUE,  year.labels.left = TRUE) + theme_minimal() +
  scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Sub Meter 1") + theme(plot.title = element_text(hjust = 0.5))
#--------------SM2
hr.SM2 <- ggseasonplot(bah.hour.summary.ts[,"SM2"],year.labels = TRUE,  year.labels.left = TRUE) + theme_minimal() +
  scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Sub Meter 2") + theme(plot.title = element_text(hjust = 0.5))
#--------------SM3
hr.SM3 <- ggseasonplot(bah.hour.summary.ts[,"SM3"],year.labels = TRUE,  year.labels.left = TRUE) + theme_minimal() +
  scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Sub Meter 3") + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(hr.GAP,hr.GRP,hr.VOL,hr.GI,hr.SM1,hr.SM2,hr.SM3, nrow = 3)
#------------------------Week Perspective---------------------------------#
#-------------------Seasonal Plots---------------------------------#
#COLOUR SCHEME NICELY reflects the 'monthly' difference
# Generally power usage seems to go up on weekends

#--------------GAP
pl.1 <- ggseasonplot(bah.day.summary.ts[,"GAP"],year.labels = TRUE) + theme_minimal() +
  scale_y_continuous(name="Global Active Power (Mean Kilowatts/min)", labels = scales::comma) +
  ggtitle("Global Active Power") + theme(plot.title = element_text(hjust = 0.5))
#--------------GRP
ggseasonplot(bah.day.summary.ts[,"GRP"],year.labels = TRUE) + theme_minimal() +
  scale_y_continuous(name="Global Reactive Power (Mean Kilowatts/min)", labels = scales::comma) +
  ggtitle("Global Reactive Power") + theme(plot.title = element_text(hjust = 0.5))
#--------------VOL
#THIS IS A GOOD GRAPH TO EXPLAIN VOLTAGE DISCREPANCY
pl.2 <- ggseasonplot(bah.day.summary.ts[,"VOL"],year.labels = TRUE) + theme_minimal() +
  scale_y_continuous(name="Voltage (Mean Volts/min)", labels = scales::comma) +
  ggtitle("Voltage") + theme(plot.title = element_text(hjust = 0.5))
#--------------GI
ggseasonplot(bah.day.summary.ts[,"GI"],year.labels = TRUE) + theme_minimal() +
  scale_y_continuous(name="Global Intensity (Mean Ampere/min)", labels = scales::comma) +
  ggtitle("Global Intensity") + theme(plot.title = element_text(hjust = 0.5))
#--------------SM1
ggseasonplot(bah.day.summary.ts[,"SM1"],year.labels = TRUE) + theme_minimal() +
  scale_y_continuous(name="Sub Meter 1 (Mean Kilowatts/min)", labels = scales::comma) +
  ggtitle("Sub Meter 1") + theme(plot.title = element_text(hjust = 0.5))
#--------------SM2
ggseasonplot(bah.day.summary.ts[,"SM2"],year.labels = TRUE) + theme_minimal() +
  scale_y_continuous(name="Sub Meter 2 (Mean Kilowatts/min)", labels = scales::comma) +
  ggtitle("Sub Meter 2") + theme(plot.title = element_text(hjust = 0.5))
#--------------SM3
ggseasonplot(bah.day.summary.ts[,"SM3"],year.labels = TRUE) + theme_minimal() +
  scale_y_continuous(name="Sub Meter 3 (Mean Kilowatts/min)", labels = scales::comma) +
  ggtitle("Sub Meter 3") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(pl.1,pl.2, nrow = 1)
#------------------------Month Perspective---------------------------------#
#-------------------Seasonal Plots---------------------------------#
#--------------GAP
p1<-ggseasonplot(bah.month.summary.ts[,"GAP"]) + theme_minimal() +
  scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Global Active Power") + theme(plot.title = element_text(hjust = 0.5))
#--------------GRP
p2<-ggseasonplot(bah.month.summary.ts[,"GRP"]) + theme_minimal() +
  scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Global Reactive Power") + theme(plot.title = element_text(hjust = 0.5))
#--------------VOL
p3<-ggseasonplot(bah.month.summary.ts[,"VOL"]) + theme_minimal() +
  scale_y_continuous(name="Mean Volts/min", labels = scales::comma) +
  ggtitle("Voltage") + theme(plot.title = element_text(hjust = 0.5))
#--------------GI
p4<-ggseasonplot(bah.month.summary.ts[,"GI"]) + theme_minimal() +
  scale_y_continuous(name="Mean Ampere/min", labels = scales::comma) +
  ggtitle("Global Intensity") + theme(plot.title = element_text(hjust = 0.5))
#--------------SM1
p5<-ggseasonplot(bah.month.summary.ts[,"SM1"]) + theme_minimal() +
  scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Sub Meter 1") + theme(plot.title = element_text(hjust = 0.5))
#--------------SM2
p6<-ggseasonplot(bah.month.summary.ts[,"SM2"]) + theme_minimal() +
  scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Sub Meter 2") + theme(plot.title = element_text(hjust = 0.5))
#--------------SM3
p7<-ggseasonplot(bah.month.summary.ts[,"SM3"]) + theme_minimal() +
  scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Sub Meter 3") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(p1,p2,p3,p4,p5,p6,p7, nrow = 3)
#------------------------Day Perspective---------------------------------#
#------------------------Sub-Seasonal Plots---------------------------------#
hr.GAP <- ggsubseriesplot(bah.hour.summary.ts[,"GAP"]) + scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Global Active Power") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Hour of the day (24hr time)")
hr.GAP
hr.GRP <- ggsubseriesplot(bah.hour.summary.ts[,"GRP"]) + scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Global Reactive Power") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Hour of the day (24hr time)")
hr.VOL <- ggsubseriesplot(bah.hour.summary.ts[,"VOL"]) + scale_y_continuous(name="Mean Volts/min", labels = scales::comma) +
  ggtitle("Voltage") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Hour of the day (24hr time)")
hr.GI <- ggsubseriesplot(bah.hour.summary.ts[,"GI"]) + scale_y_continuous(name="Mean Ampere/min", labels = scales::comma) +
  ggtitle("Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Hour of the day (24hr time)")
hr.SM1 <- ggsubseriesplot(bah.hour.summary.ts[,"SM1"]) + scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Sub Meter 1") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Hour of the day (24hr time)")
hr.SM2 <- ggsubseriesplot(bah.hour.summary.ts[,"SM2"]) + scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Sub Meter 2") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Hour of the day (24hr time)")
hr.SM3 <- ggsubseriesplot(bah.hour.summary.ts[,"SM3"]) + scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Sub Meter 3") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Hour of the day (24hr time)")
grid.arrange(hr.GAP,hr.GRP,hr.VOL,hr.GI,hr.SM1,hr.SM2,hr.SM3, nrow = 3)
#------------------------Week Perspective---------------------------------#
#------------------------Sub-Seasonal Plots---------------------------------#
#not as bigger weekly variation as I thought there would be
#Numerically the weekends are often higher... but doesn't look massive. 

p1<-ggsubseriesplot(bah.day.summary.ts[,"GAP"]) + scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Global Active Power") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Day of the Week (2007 Overview)")
p2<-ggsubseriesplot(bah.day.summary.ts[,"GRP"]) + scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Global Reactive Power") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Day of the Week (2007 Overview)")
p3<-ggsubseriesplot(bah.day.summary.ts[,"VOL"]) + scale_y_continuous(name="Mean Volts/min", labels = scales::comma) +
  ggtitle("Voltage") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Day of the Week (2007 Overview)")
p4<-ggsubseriesplot(bah.day.summary.ts[,"GI"]) + scale_y_continuous(name="Mean Ampere/min", labels = scales::comma) +
  ggtitle("Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Day of the Week (2007 Overview)")
p5<-ggsubseriesplot(bah.day.summary.ts[,"SM1"]) + scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Sub Meter 1") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Day of the Week (2007 Overview)")
p6<-ggsubseriesplot(bah.day.summary.ts[,"SM2"]) + scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Sub Meter 2") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Day of the Week (2007 Overview)")
p7<-ggsubseriesplot(bah.day.summary.ts[,"SM3"]) + scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Sub Meter 3") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Day of the Week (2007 Overview)")
grid.arrange(p1,p2,p3,p4,p5,p6,p7, nrow = 3)
#------------------------Month Perspective---------------------------------#
#------------------------Sub-Seasonal Plots---------------------------------#
p1<-ggsubseriesplot(bah.month.summary.ts[,"GAP"]) + scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Global Active Power") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Month of the year")
p2<-ggsubseriesplot(bah.month.summary.ts[,"GRP"]) + scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Global Reactive Power") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Month of the year")
p3<-ggsubseriesplot(bah.month.summary.ts[,"VOL"]) + scale_y_continuous(name="Mean Volts/min", labels = scales::comma) +
  ggtitle("Voltage") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Month of the year")
p4<-ggsubseriesplot(bah.month.summary.ts[,"GI"]) + scale_y_continuous(name="Mean Ampere/min", labels = scales::comma) +
  ggtitle("Global Intensity") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Month of the year")
p5<-ggsubseriesplot(bah.month.summary.ts[,"SM1"]) + scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Sub Meter 1") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Month of the year")
p6<-ggsubseriesplot(bah.month.summary.ts[,"SM2"]) + scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Sub Meter 2") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Month of the year")
p7<-ggsubseriesplot(bah.month.summary.ts[,"SM3"]) + scale_y_continuous(name="Mean Kilowatts/min", labels = scales::comma) +
  ggtitle("Sub Meter 3") + theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Month of the year")
grid.arrange(p1,p2,p3,p4,p5,p6,p7, nrow = 3)
#------------------------Autocorrelations
#------------------------Day Perspective---------------------------------#
#This essentially shows how 'correlated' the values are the the lag'th variable
#behind the current. 
hr.GAP <- ggAcf(bah.hour.summary.ts[,"GAP"], lag=96) + ggtitle("GAP") + theme(plot.title = element_text(hjust = 0.5))
hr.GRP <- ggAcf(bah.hour.summary.ts[,"GRP"], lag=96) + ggtitle("GRP") + theme(plot.title = element_text(hjust = 0.5))
hr.VOL <- ggAcf(bah.hour.summary.ts[,"VOL"], lag=96) + ggtitle("VOL") + theme(plot.title = element_text(hjust = 0.5))
hr.GI <- ggAcf(bah.hour.summary.ts[,"GI"], lag=96) + ggtitle("GI") + theme(plot.title = element_text(hjust = 0.5))
hr.SM1 <- ggAcf(bah.hour.summary.ts[,"SM1"], lag=96) + ggtitle("SM1") + theme(plot.title = element_text(hjust = 0.5))
hr.SM2 <- ggAcf(bah.hour.summary.ts[,"SM2"], lag=96) + ggtitle("SM2") + theme(plot.title = element_text(hjust = 0.5))
hr.SM3 <- ggAcf(bah.hour.summary.ts[,"SM3"], lag=96) + ggtitle("SM3") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(hr.GAP,hr.GRP,hr.VOL,hr.GI,hr.SM1,hr.SM2,hr.SM3, nrow = 3)

#------------------------Week Perspective---------------------------------#
#Seems to be some seasonality to the week in GAP, GRP, GI, SM's
#Not crazy big though
p1<-ggAcf(bah.day.summary.ts[,"GAP"], lag=70) + ggtitle("GAP") + theme(plot.title = element_text(hjust = 0.5))
p2<-ggAcf(bah.day.summary.ts[,"GRP"], lag=70) + ggtitle("GRP") + theme(plot.title = element_text(hjust = 0.5))
p3<-ggAcf(bah.day.summary.ts[,"VOL"], lag=70) + ggtitle("VOL") + theme(plot.title = element_text(hjust = 0.5))
p4<-ggAcf(bah.day.summary.ts[,"GI"], lag=70) + ggtitle("GI") + theme(plot.title = element_text(hjust = 0.5))
p5<-ggAcf(bah.day.summary.ts[,"SM1"], lag=70) + ggtitle("SM1") + theme(plot.title = element_text(hjust = 0.5))
p6<-ggAcf(bah.day.summary.ts[,"SM2"], lag=70) + ggtitle("SM2") + theme(plot.title = element_text(hjust = 0.5))
p7<-ggAcf(bah.day.summary.ts[,"SM3"], lag=70) + ggtitle("SM3") + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(p1,p2,p3,p4,p5,p6,p7, nrow = 3)
#------------------------Month Perspective---------------------------------#
p1<-ggAcf(bah.month.summary.ts[,"GAP"], lag=48)
p2<-ggAcf(bah.month.summary.ts[,"GRP"], lag=48)
p3<-ggAcf(bah.month.summary.ts[,"VOL"], lag=48)
p4<-ggAcf(bah.month.summary.ts[,"GI"], lag=48)
p5<-ggAcf(bah.month.summary.ts[,"SM1"], lag=48)
p6<-ggAcf(bah.month.summary.ts[,"SM2"], lag=48)
p7<-ggAcf(bah.month.summary.ts[,"SM3"], lag=48)
grid.arrange(p1,p2,p3,p4,p5,p6,p7, nrow = 3)
#------------------------Simple Forecasting - GAP
#-----------Day Perspective-------------#
summary <- window(bah.hour.summary.ts[,"GAP"],start=1)
summaryfit1 <- meanf(summary,h=24)
summaryfit2 <- rwf(summary,h=24)
summaryfit3 <- snaive(summary,h=24)
summaryfit4 <- rwf(summary, drift=TRUE, h=24)
#Forecasts
pred.gap <- autoplot(summary) +
  forecast::autolayer(summaryfit1, series="Mean", PI=FALSE) +
  forecast::autolayer(summaryfit2, series="Naïve", PI=FALSE) +
  forecast::autolayer(summaryfit3, series="Seasonal naïve", PI=FALSE) +
  forecast::autolayer(summaryfit4, PI=FALSE, series="Drift") +
  xlab("Days (Feb 2007 Overview)") + ylab("GAP") +
  ggtitle("Simple Forecasts") +
  guides(colour=guide_legend(title="Forecast"))

summary <- window(bah.hour.summary.ts[,"GRP"],start=1)
summaryfit1 <- meanf(summary,h=24)
summaryfit2 <- rwf(summary,h=24)
summaryfit3 <- snaive(summary,h=24)
summaryfit4 <- rwf(summary, drift=TRUE, h=24)
#Forecasts
pred.grp <- autoplot(summary) +
  forecast::autolayer(summaryfit1, series="Mean", PI=FALSE) +
  forecast::autolayer(summaryfit2, series="Naïve", PI=FALSE) +
  forecast::autolayer(summaryfit3, series="Seasonal naïve", PI=FALSE) +
  forecast::autolayer(summaryfit4, PI=FALSE, series="Drift") +
  xlab("Days (Feb 2007 Overview)") + ylab("GRP") +
  ggtitle("Simple Forecasts") +
  guides(colour=guide_legend(title="Forecast"))

summary <- window(bah.hour.summary.ts[,"VOL"],start=1)
summaryfit1 <- meanf(summary,h=24)
summaryfit2 <- rwf(summary,h=24)
summaryfit3 <- snaive(summary,h=24)
summaryfit4 <- rwf(summary, drift=TRUE, h=24)
#Forecasts
pred.vol <- autoplot(summary) +
  forecast::autolayer(summaryfit1, series="Mean", PI=FALSE) +
  forecast::autolayer(summaryfit2, series="Naïve", PI=FALSE) +
  forecast::autolayer(summaryfit3, series="Seasonal naïve", PI=FALSE) +
  forecast::autolayer(summaryfit4, PI=FALSE, series="Drift") +
  xlab("Days (Feb 2007 Overview)") + ylab("VOL") +
  ggtitle("Simple Forecasts") +
  guides(colour=guide_legend(title="Forecast"))

summary <- window(bah.hour.summary.ts[,"GI"],start=1)
summaryfit1 <- meanf(summary,h=24)
summaryfit2 <- rwf(summary,h=24)
summaryfit3 <- snaive(summary,h=24)
summaryfit4 <- rwf(summary, drift=TRUE, h=24)
#Forecasts
pred.gi <- autoplot(summary) +
  forecast::autolayer(summaryfit1, series="Mean", PI=FALSE) +
  forecast::autolayer(summaryfit2, series="Naïve", PI=FALSE) +
  forecast::autolayer(summaryfit3, series="Seasonal naïve", PI=FALSE) +
  forecast::autolayer(summaryfit4, PI=FALSE, series="Drift") +
  xlab("Days (Feb 2007 Overview)") + ylab("GI") +
  ggtitle("Simple Forecasts") +
  guides(colour=guide_legend(title="Forecast"))

grid.arrange(pred.gap, pred.grp, pred.vol, pred.gi, nrow = 3)





#-----------Week Perspective-------------#
summary.2 <- window(bah.day.summary.ts[,"GAP"],start=1)
summary.2fit1 <- meanf(summary.2,h=7)
summary.2fit2 <- rwf(summary.2,h=7)
summary.2fit3 <- snaive(summary.2,h=7)
summary.2fit4 <- rwf(summary.2, drift=TRUE, h=7)
#Forecasts
autoplot(summary.2) +
  forecast::autolayer(summary.2fit1, series="Mean", PI=FALSE) +
  forecast::autolayer(summary.2fit2, series="Naïve", PI=FALSE) +
  forecast::autolayer(summary.2fit3, series="Seasonal naïve", PI=FALSE) +
  forecast::autolayer(summary.2fit4, PI=FALSE, series="Drift") +
  xlab("Weeks (2007 Overview)") + ylab("GAP") +
  ggtitle("Simple Forecasts for GAP") +
  guides(colour=guide_legend(title="Forecast"))

#Come back to
summary2 <- window(bah.hour.summary.ts[,"GAP"],start=)
accuracy(summaryfit1, summary2)
accuracy(summaryfit2, summary2)
accuracy(summaryfit3, summary2)
accuracy(summaryfit4, summary2)

checkresiduals(summary.2fit1)
checkresiduals(summary.2fit2)
checkresiduals(summary.2fit3)
checkresiduals(summary.2fit4)

#-----------Month Perspective-------------#
summary.3 <- window(bah.month.summary.ts[,"GAP"])
summary.3fit1 <- meanf(summary.3,h=12)
summary.3fit2 <- rwf(summary.3,h=12)
summary.3fit3 <- snaive(summary.3,h=12)
summary.3fit4 <- rwf(summary.3, drift=TRUE, h=12)
#Forecasts
p1<-autoplot(summary.3) +
  forecast::autolayer(summary.3fit1, series="Mean", PI=FALSE) +
  forecast::autolayer(summary.3fit2, series="Naïve", PI=FALSE) +
  forecast::autolayer(summary.3fit3, series="Seasonal naïve", PI=FALSE) +
  forecast::autolayer(summary.3fit4, PI=FALSE, series="Drift") +
  xlab("Monthly Overview") + ylab("GAP") +
  ggtitle("Simple Forecasts for GAP") +
  guides(colour=guide_legend(title="Forecast"))

summary.3 <- window(bah.month.summary.ts[,"GRP"])
summary.3fit1 <- meanf(summary.3,h=12)
summary.3fit2 <- rwf(summary.3,h=12)
summary.3fit3 <- snaive(summary.3,h=12)
summary.3fit4 <- rwf(summary.3, drift=TRUE, h=12)
#Forecasts
p2<-autoplot(summary.3) +
  forecast::autolayer(summary.3fit1, series="Mean", PI=FALSE) +
  forecast::autolayer(summary.3fit2, series="Naïve", PI=FALSE) +
  forecast::autolayer(summary.3fit3, series="Seasonal naïve", PI=FALSE) +
  forecast::autolayer(summary.3fit4, PI=FALSE, series="Drift") +
  xlab("Monthly Overview") + ylab("GRP") +
  ggtitle("Simple Forecasts for GRP") +
  guides(colour=guide_legend(title="Forecast"))

summary.3 <- window(bah.month.summary.ts[,"VOL"])
summary.3fit1 <- meanf(summary.3,h=12)
summary.3fit2 <- rwf(summary.3,h=12)
summary.3fit3 <- snaive(summary.3,h=12)
summary.3fit4 <- rwf(summary.3, drift=TRUE, h=12)
#Forecasts
p3<-autoplot(summary.3) +
  forecast::autolayer(summary.3fit1, series="Mean", PI=FALSE) +
  forecast::autolayer(summary.3fit2, series="Naïve", PI=FALSE) +
  forecast::autolayer(summary.3fit3, series="Seasonal naïve", PI=FALSE) +
  forecast::autolayer(summary.3fit4, PI=FALSE, series="Drift") +
  xlab("Monthly Overview") + ylab("VOL") +
  ggtitle("Simple Forecasts for VOL") +
  guides(colour=guide_legend(title="Forecast"))

summary.3 <- window(bah.month.summary.ts[,"GI"])
summary.3fit1 <- meanf(summary.3,h=12)
summary.3fit2 <- rwf(summary.3,h=12)
summary.3fit3 <- snaive(summary.3,h=12)
summary.3fit4 <- rwf(summary.3, drift=TRUE, h=12)
#Forecasts
p4<-autoplot(summary.3) +
  forecast::autolayer(summary.3fit1, series="Mean", PI=FALSE) +
  forecast::autolayer(summary.3fit2, series="Naïve", PI=FALSE) +
  forecast::autolayer(summary.3fit3, series="Seasonal naïve", PI=FALSE) +
  forecast::autolayer(summary.3fit4, PI=FALSE, series="Drift") +
  xlab("Monthly Overview") + ylab("GI") +
  ggtitle("Simple Forecasts for GI") +
  guides(colour=guide_legend(title="Forecast"))

grid.arrange(p1, p2, p3, p4, nrow = 2)

#Come back to
summary3 <- window(bah.month2.summary.ts[,"GAP"],start=)
accuracy(summaryfit1, summary2)
accuracy(summaryfit2, summary2)
accuracy(summaryfit3, summary2)
accuracy(summaryfit4, summary2)

checkresiduals(summary.3fit1)
checkresiduals(summary.3fit2)
checkresiduals(summary.3fit3)
checkresiduals(summary.3fit4)
