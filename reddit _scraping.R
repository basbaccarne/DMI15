
# This R script is written in the context of the Digital Methods Summer School 2015
# The goal of this script is to query the reddit database and collect the responses in a clean dataset
# A second goal is to present the associated thumbnails in a HTML page for easier review
# [[WORK IN PROGRESS - PLEASE FEEL FREE TO CONTRIBUTE]]

##################################
##################################
######## ISSUES AND IDEAS ########
##################################
##################################

# IDEAS

##### all reddit and imgur pages age accessable as .json by adding .json to the url

##### also possible to search through google [insite:reddit.com "keyword"]

##### only little variables are selected in this version, and 'digital probing' is possible


# ISSUES

##### make error proof for keywords that return no results

##### only returns the first 100 results [iterations goofy]


##################################
##################################
### INSTRUCTIONS AND VARIABLES ###
##################################
##################################

# 1) Choose your keywords below, between " "
# 2) Choose the amount of iterations (each iteration has 100 items) [malfuntioning]
# 3) source this script, results are exported as a csv and a html file in the project folder


keywords<-c("lovewins")
iterations<-NULL


##################################
##################################
############ THE CODE ############ 
##################################
##################################


# Loading libraries (and installing them if needed) #
#####################################################

if("jsonlite" %in% rownames(installed.packages()) == FALSE) {install.packages("jsonlite")}
if("xtable" %in% rownames(installed.packages()) == FALSE) {install.packages("xtable")}
library(jsonlite)
library(xtable)


# generation of query links #
#############################

generate_queries <- function() {
        
        items <- factor()
        for (item in keywords) {
                new.item <- paste("http://www.reddit.com/search.json?q=", item, "&limit=100", sep="")
                items <- c(items, new.item)
        }
        queries <<- items
}

generate_queries()


# run queries and store results in a data.matrix #
##################################################

get_queries <- function(){
        
        output <- data.frame()
        
        for (i in length(queries)) {
                json <- fromJSON (queries[i])
                parent_data <- json$data$children$data
                clean <- parent_data[ ,c(45, 51, 26)]
                output <- rbind(output, clean)
        }
        
        my.df <<- output
        write.csv(my.df, "output.csv")        
}

get_queries()

# generate a HTML table #
#########################

generate_html <- function(){
        
        my.df$thumbnail <- paste('<img src="', my.df$thumbnail, '">', sep='')
        html_content <- xtable(my.df)
        print(html_content, type='html', file="./output.html")
        html <- readLines("./output.html")
        html <- gsub("&lt;", "<", html)
        html <- gsub("&gt;", ">", html)
        write(html, file = "./output.html")       
}

generate_html()
