# This R script is written in the context of the Digital Methods Summer School 2015
# The goal of this script is to translate timecodes in the right format
# [[PROJECT AND FILE SPECIFIC SCRIPT - USE THIS AS AN INSPIRATION]]

data <- read.csv("scotusmarriage_images.csv")

data$created_time <- strptime(data$created_time, "%m/%e/%Y %H:%M:%S")
data$created_time <- as.numeric(as.POSIXlt(data$created_time))

write.csv(data, "scotusmarriage_images_correctedtimestamps.csv")
