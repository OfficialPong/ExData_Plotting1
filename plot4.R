# Loads data from UC Irvine Machine Learning Repository, 
# "Individual household electric power consumption Data Set"
# into Data. Only the dates 2007-02-01 to 2007-02-02 are returned
loadData <- function()
{
  data<-read.table("data.txt",sep=";",header=TRUE,stringsAsFactors=FALSE)
  
  # convert Date strings to Date objects
  data$Date<-as.Date(data$Date,format="%d/%m/%Y")
  
  # subset only dates from 2007-02-01 to 2007-02-02
  data<-data[data$Date>="2007-02-01" & data$Date<="2007-02-02",]
  
  # create dateAndTime column
  data$dateAndTime <- paste(data$Date,data$Time)
  data$datetime<-strptime(data$dateAndTime,format="%Y-%m-%d %H:%M:%S", tz="")
  
  # convert sub_metering_1 to numeric
  data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
  
  # convert sub_metering_2 to numeric
  data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)
  
  # convert Global active power to numeric to allow plotting
  data$Global_active_power <- as.numeric(data$Global_active_power)
  
  # convert Global reactive power to numeric to allow plotting
  data$Global_reactive_power <- as.numeric(data$Global_reactive_power)
  
  # convert voltage to numeric to allow plotting
  data$Voltage <- as.numeric(data$Voltage)
  
  data
}


# Plots 4 plots on one plot.
# 1) datetime vs. Global Active Power
# 2) datetime vs. Voltage
# 3) datetime vs. Energy sub metering
# 4) datetime vs Global reactive power
plot4 <- function()
{
  # load data from disk
  data<-loadData()
  
  # set up graphic device
  png(filename="plot4.png",width=480,height=480,units="px")
  
  # set matrix of 2 x 2 plots
  par(mfrow=c(2,2))
  
  # plot top left
  with(data,plot(datetime,Global_active_power,type="l",ylab="Global Active Power", xlab=""))
  
  # plot top right
  with(data,plot(datetime,Voltage,type="l"))
  
  # plot bot left
  
  # plot sub metering 1
  with(data,plot(datetime,Sub_metering_1,type="l",xlab="",ylab="Energy sub metering"))
  
  # add sub metering 2 to the plot
  with(data,lines(datetime,Sub_metering_2,type="l",col="red"))
  
  # add sub metering 3 to the plot
  with(data,lines(datetime,Sub_metering_3,type="l",col="blue"))
  
  # add legend
  with(data,legend("topright",bty="n",lty=c(1,1),col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3")))
  
  # plot bot right
  with(data,plot(datetime,Global_reactive_power,type="l"))
  
  # close graphic device
  dev.off()
}