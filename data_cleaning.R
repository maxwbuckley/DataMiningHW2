# Dependancies.
library("plyr")

# Clear R environment.
rm(list=ls())
# Set working directory .
setwd("~/UCD/DataMining/HW2/")

output_dir <- "CleanedData"
input_dir <- "InputData"

input_csv_files <- dir(input_dir)

# List to store all the data frames.
df_list <- list()
for(i in 1:length(input_csv_files)){
  file <- input_csv_files[i]
  df <- read.csv(paste0(input_dir, "/", file),
                 stringsAsFactors=FALSE)
  # Store a season indicator.
  df$Season <- gsub(".csv", "", file)
  df_list[[i]] <- df
}

# Combine all the data frames into one.
cleaned_data <- rbind.fill(df_list[1:i])
# Format date as proper R date object.
cleaned_data$Date <- as.Date(cleaned_data$Date, "%d/%m/%y")
# Factor desired character columns.
cleaned_data$FTR <- as.factor(cleaned_data$FTR)
cleaned_data$HTR <- as.factor(cleaned_data$HTR)
cleaned_data$HomeTeam <- as.factor(cleaned_data$HomeTeam)
cleaned_data$AwayTeam <- as.factor(cleaned_data$AwayTeam)
cleaned_data$Referee <- as.factor(cleaned_data$Referee)
cleaned_data$Season <- as.factor(cleaned_data$Season)

# Remove Division as it is a constant. It adds no information.
cleaned_data$Div <- NULL

#Calculate points
HomePoints<-rep(1, nrow(cleaned_data))
HomePoints[cleaned_data$FTR=="H"] <- 3
HomePoints[cleaned_data$FTR=="A"] <- 0
cleaned_data$HomePoints <- HomePoints
AwayPoints<-rep(1,nrow(cleaned_data))
AwayPoints[cleaned_data$FTR=="H"] <- 0
AwayPoints[cleaned_data$FTR=="A"] <- 3
cleaned_data$AwayPoints <-AwayPoints
cleaned_data$TotalGoals <-cleaned_data$FTHG + cleaned_data$FTAG


# Save as output to use later. Use .Rda format to preserve R
# type information in addition to data values.
save(cleaned_data, file = paste0(output_dir, "/",
                                 "cleaned_data.Rda"))