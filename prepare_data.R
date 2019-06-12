# Script to combine and arrange the data
# Author: Zeynep Kurkcuoglu
# Date: 2019-06-11
library(data.table)

## Read raw data
# Get the list of files
myfilelist <- list.files("data/")
myfilelist <- myfilelist[grep("modified",myfilelist)]

mydf <- data.frame(PROVINCIE = character(), Age = character(), Male = numeric(), Female = numeric(), Year = numeric())

# Read .csv files and combine them
for (i in myfilelist){
  myfile <- read.csv2(paste("data/",i,sep = ""), stringsAsFactors = FALSE, na.strings = c(""," ","NA","NULL"))
  myfile$Year <- substr(i, 1, 4)
  mydf <- rbind(mydf, myfile)
}

colnames(mydf)[grep("PROVINCIE",colnames(mydf))] <- "PROVINCIE"

# Clean provincie names
mydf$PROVINCIE <- gsub(" \\(PV\\)","",mydf$PROVINCIE)

# Remove 16, 17, 18, 19 jaar values
age_categ <- names(table(mydf$Age))
mydf <- mydf[mydf$Age %in% setdiff(age_categ, c("16 jaar", "17 jaar", "18 jaar", "19 jaar")),]
# Long data table format
final_data <- melt(mydf, id.vars = c("PROVINCIE","Age","Year"))

# Rename variables
colnames(final_data) <- c("PROVINCIE","Age","Year","Gender","Freq")

## Write the data file
write.csv(final_data, "clean_data.csv", row.names = FALSE)