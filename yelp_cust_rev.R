library(stringr)
library(jsonlite)

library(readr)
library(dplyr)
list.files("./yelp_dataset_challenge_academic_dataset")
list.files()



lines=readLines("./yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json")
business=as.data.frame(t(sapply(lines,fromJSON)),row.names=FALSE)
rm(lines)


#now read the review file
#
lines= read_lines("./yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json",n_max =  200000,progress=FALSE)
reviews_combined <- str_c("[", str_c(lines, collapse = ", "), "]")


reviews=fromJSON(reviews_combined)%>%
  flatten()%>%
  tbl_df()



rm(lines)
library(dplyr)
library(tidytext)
?str_c
review$votes=NULL
review=review%>%
  flatten()%>%
  tbl_df()
review_words=review%>%
  unnest_tokens(word,text)

rm(review_words)
head(review_words,6)
  # filter(!word %in% stop_words$word,
  #        str_detect(word,"^[a-z']+$"))