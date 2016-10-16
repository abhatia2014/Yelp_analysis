library(stringr)
library(jsonlite)
library(rjson)
list.files("./yelp_dataset_challenge_academic_dataset")
list.files()
untar("yelp_dataset_challenge_academic_dataset.tar",list=TRUE)

install.packages("RJSONIO")
library(RJSONIO)

list.files()
lines=readLines("./yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json")
business=as.data.frame(t(sapply(lines,fromJSON)),row.names=FALSE)
rm(lines)
head(business)
#now read the review file

lines= readLines("./yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json",n = 200000)
review=as.data.frame(t(sapply(lines, fromJSON)),row.names=FALSE)
