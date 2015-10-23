# Downloading and loading data into R
remoteFile <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
localZipFile <- "/data.zip"
download.file(remoteFile, destfile = localZipFile, mode = "wb",method="curl")
unzip (localZipFile)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# loading packages
library(dplyr)  
library(ggplot2)

# Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? Using the base plotting system, make a plot 
# showing the total PM2.5 emission from all sources for each of 
# the years 1999, 2002, 2005, and 2008.
# Group of emissions per year and then sum them

totalPM25 <- 
  NEI %>% group_by(year) %>% 
  summarise(total_PM25 = sum(Emissions, na.rm = TRUE))
  
# Plot of year vs Emission 

png("plot1.png", width=480, height=480)
plot(totalPM25$year, totalPM25$total_PM25, 
     main = "Total Emissions in USA"
     ,xlab = "Year", ylab = "Emissions", col ="red", type= "l")
points(totalPM25$year, totalPM25$total_PM25,
       col ="red", pch = 16)  
dev.off()

# Have total emissions from PM2.5 decreased in the Baltimore City,
# Maryland (fips == "24510") from 1999 to 2008? Use the base 
# plotting system to make a plot answering this question.

# Group of emissions of Baltimore City (fips == "24510") per year and then sum them

totalBaltimore <- 
  filter(NEI, fips == "24510") %>% 
  group_by(year) %>% 
  summarise(total_Baltimore = sum(Emissions, na.rm = TRUE))
  
# Plot of year vs Emission

png("plot2.png", width=480, height=480)
plot(totalBaltimore$year, totalBaltimore$total_Baltimore, 
     main = "Total Emissions in Baltimore City"
     ,xlab = "Year", ylab = "Emissions",col = "blue", type = "l")
points(totalBaltimore$year, totalBaltimore$total_Baltimore,  
       col = "blue", pch = 16)
dev.off()

# Of the four types of sources indicated by the type (point, 
# nonpoint, onroad, nonroad) variable, which of these four 
# sources have seen decreases in emissions from 1999–2008 for 
# Baltimore City? Which have seen increases in emissions from 
# 1999–2008? Use the ggplot2 plotting system to make a plot 
# answer this question.
# group the sum of emissions per type and year

byType <- 
  filter(NEI, fips == "24510") %>% 
  group_by(year, type) %>% 
  summarise(total_bytype = sum(Emissions, na.rm = TRUE))
byType

# chart of emissions per type vs year
png("plot3.png", width=480, height=480)
g <- qplot(year, total_bytype, data = byType, 
           color = type, geom = c("line", "point"))
g + ylab("Emissions")
dev.off()

# Across the United States, how have emissions from coal 
# combustion-related sources changed from 1999–2008?

# Mergin tables NEI and SCC by the "SCC" column
merged <- merge(NEI,SCC, by="SCC")

# Emission from Coal sources

CoalTotal <- merged[grep("Coal",merged$Short.Name),] %>% 
  group_by(year) %>% 
  summarise(Emissions = sum(Emissions, na.rm = TRUE))
  
# Plot of year vs Emission for Coal related sources

png("plot4.png", width=480, height=480)
plot(CoalTotal$year, CoalTotal$Emissions, 
     main = "Total Emissions from Coal sources"
     ,xlab = "Year", ylab = "Emissions",
     col = "green", type = "l")
points(CoalTotal$year, CoalTotal$Emissions,  
       col = "green", pch = 16)
dev.off()

#How have emissions from motor vehicle sources changed from 
# 1999–2008 in Baltimore City?

MotorTotal <- merged[grep("Vehicle", merged$Short.Name), ] %>% 
  filter(fips == "24510") %>%
  group_by(year) %>% 
  summarise(Emissions = sum(Emissions, na.rm = TRUE))
  
# Plot of year vs Emission for Motor vehicles in Baltimore City

png("plot5.png", width=480, height=480)
plot(MotorTotal$year, MotorTotal$Emissions, 
     main = "Total Emissions from Vehicles in Baltimore"
     ,xlab = "Year", ylab = "Emissions",
     col = "blue", type = "l")
points(MotorTotal$year, MotorTotal$Emissions,  
       col = "blue", pch = 16)
dev.off()

#Compare emissions from motor vehicle sources in Baltimore City
# with emissions from motor vehicle sources in Los Angeles 
# County, California (fips == "06037"). Which city has seen 
# greater changes over time in motor vehicle emissions?

BalCal <- merged[grep("Vehicle", merged$Short.Name), ] %>% 
  filter(fips == "24510" | fips == "06037") %>%
  group_by(year, fips) %>% 
  summarise(Emissions = sum(Emissions, na.rm = TRUE))
  
BalCal$fips <- gsub("06037", "Los Angeles, Ca", BalCal$fips)
BalCal$fips <- gsub("24510", "Baltimore City, Md", BalCal$fips)

# Plot of year vs Emission for Motor vehicles in Baltimore City 
# and Los Angeles

png("plot6.png", width=480, height=480)
q <- qplot(year, Emissions, data = BalCal, 
           color = fips, geom = c("line", "point"))
q + ylab("Emissions")
dev.off()
