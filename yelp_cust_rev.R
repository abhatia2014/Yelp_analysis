library(stringr)
library(jsonlite)
library(rjson)
list.files("./yelp_dataset_challenge_academic_dataset")
#each line is a json object - the fastest way is to combine into a single json
jsonfile="./yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json"
winner1=fromJSON(file=jsonfile,unexpected.escape = "skip")
reviews_combined <- str_c("[", str_c(jsonfile, collapse = ", "), "]")
reviews=fromJSON(reviews_combined)%>%
  flatten()%>%
  tbl_df()
