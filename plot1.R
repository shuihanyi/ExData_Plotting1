# The following code is based on Version 3.1.1 of R and  Windows platform

# If the file does not exist, this function will download it from the Internet 
# The file name and complete URL should be specific in this function
dl_file <- function(fileName,fileUrl){
    if(!file.exists(fileName)){
        download.file(fileUrl, destfile=fileName) 
        # If you work on a Mac, please specify the method to be used for downloading files 
        # download.file(fileUrl, destfile=fileName, method = "curl")
        } 
    return(fileName)
}


# Importing data and perparing it for use
# If the data has already been prepared, then this function gets the data from the cache
rd_file <- function(){
    cacheFile <- "data_plotting.csv" 
    if(file.exists(cacheFile)){
        Data <- read.csv(cacheFile)
        Date_Time<- paste(Data$Date, Data$Time, sep = " ")
        Data$Time <- strptime(Date_Time, format = "%d/%m/%Y %H:%M:%S")      
    }
    else{
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        conData <- dl_file("household_power_consumption.zip", fileUrl)
        rawData <- unz(conData, "household_power_consumption.txt") 
        allData <- read.table(rawData, header=T, sep=';', na.strings="?", colClasses=c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
        Data <- subset(allData, Date == "1/2/2007" | Date == "2/2/2007")
        write.csv(Data, cacheFile)
        Date_Time<- paste(Data$Date, Data$Time, sep = " ")
        Data$Time <- strptime(Date_Time, format = "%d/%m/%Y %H:%M:%S")        
    }
    Data
}


# This function generates a histogram of the household global active power in kilowatts
plot_1 <- function(){
    Data <- rd_file()
    png(filename = "plot1.png", width = 480, height = 480, units = "px", type = "cairo-png")
    attach(Data) # adds the data frame to the R search path
    opar <- par(no.readonly=TRUE) # produces a list of current graphical settings that can be modified
    par(bg = "transparent", family = "serif")
    title <- "Global Active Power"
    xlab = "Global Active Power (kilowatts)"
    hist(Global_active_power, main = title, xlab = xlab, col="red", axes = FALSE)
    axis(side=1, at=seq(0, 6, by=2))
    axis(side=2, at=seq(0, 1200, by=200)) 
    par(opar)
    detach(Data) # removes the data frame from the search path
    dev.off()
}

plot_1()