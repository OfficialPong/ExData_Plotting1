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


# Plots a line plot of datetime against Global Active Power for a dataframe
# passed in. Outputs to a file named plot2.png.
plot2 <- function(data)
{
  # set up graphic device
  png(filename="plot2.png",width=480,height=480,units="px")
  
  with(data,plot(datetime,Global_active_power,type="l",ylab="Global Active Power (kilowatts)", xlab=""))
  
  # close graphic device
  dev.off()
}