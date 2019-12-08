#import and interrogate air quality data
#jedenfalls
#08-12-2019

####install libraries####
#you only have to do this once
install.packages("stringr",dependencies=TRUE)

####load libraries####
library(readxl)
require(stringr)

####identify working directory####
getwd()

####starting to code####
datawd<-"/cloud/project/"

# lists files in working directory
listOfFiles<-list.files(datawd)

# find out which files have extensions csv
listOfFiles<-listOfFiles[grep(".xls",listOfFiles)]

# creating the full path
myPaths<-str_c(datawd,listOfFiles)

#warnings()
#rm(combinedData)

#create empty list
nameOfFile <- list()

#loop through read_excel function for all files listed.
#(note the double square brackets)

for (k in 1:length(myPaths)){
  #read the excel file, skip the first 4 rows, and then rename the column headers
  nameOfFile[[k]] = read_excel(myPaths[k], skip = 4, col_names = c("Time ","MeasPt","Wind Spd. m/s","Wind Dir. degree","Global Rad w/m2","RH %","Temp °C","NO ppb","NO2 ppb","NOx ppb","O3 ug/m3"))
}

#merge the list to create one (1) data frame
combinedData <- do.call(rbind.data.frame, nameOfFile)

####explore the data####
summary(cocombinedData)


####visualising the data####
#plot(bwPopulation, type="l", col = "green", sub="work in progress", main="Maun Air Monitoring")
