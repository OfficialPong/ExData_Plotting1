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


# Plots 3 data sets, Sub_metering_[123] onto the same plot.
# Black = sub_metering_1
# Red = sub_metering_2
# Blue = sub_metering_3
# Outputs to a file named plot3.png
plot3 <- function(data)
{
  # set up graphic device
  png(filename="plot3.png",width=480,height=480,units="px")
  
  # plot sub metering 1
  with(data,plot(datetime,Sub_metering_1,type="l",xlab="",ylab="Energy sub metering"))
  
  # add sub metering 2 to the plot
  with(data,lines(datetime,Sub_metering_2,type="l",col="red"))
  
  # add sub metering 3 to the plot
  with(data,lines(datetime,Sub_metering_3,type="l",col="blue"))
  
  # add legend
  with(data,legend("topright",lty=c(1,1),col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3")))
  
  lty=c(1,1)
  
  # close graphic device
  dev.off()
}
