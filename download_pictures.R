data <- read.csv("../R/links.csv")

for (i in 1:nrow(data)){
        download.file(as.character(data[i,1]), paste("celebratepride", i, ".jpg", sep=""), method="auto", mode = 'wb')
}


