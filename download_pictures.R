data <- read.csv("DMI facebook img - lovewins.csv")

for (i in 1:nrow(data)){
        download.file(as.character(data[i,1]), paste("FB_lovewins", i, ".jpg", sep=""), method="auto", mode = 'wb')
}


names <- data.frame()

for (i in 1:nrow(data)){
        rbind(names,(paste("FB_lovewins", i, ".jpg", sep="")
}

names <- paste("FB_lovewins", 1:nrow(data), ".jpg", sep="")
 write.csv(names, "names.csv")


