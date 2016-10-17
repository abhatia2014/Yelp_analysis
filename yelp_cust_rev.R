library(stringr)
library(jsonlite)

library(readr)
library(dplyr)
list.files("./yelp_dataset_challenge_academic_dataset")
list.files()



#lines=readLines("./yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json")
#business=as.data.frame(t(sapply(lines,fromJSON)),row.names=FALSE)
#rm(lines)


#now read the review file
#
lines= read_lines("./yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_review.json",n_max =  200000,progress=FALSE)
reviews_combined <- str_c("[", str_c(lines, collapse = ", "), "]")


reviews=fromJSON(reviews_combined)%>%
  flatten()%>%
  tbl_df()



rm(lines)

library(tidytext)


review_words=reviews%>%
  unnest_tokens(word,text)

#remove stop words
data("stop_words")
head(stop_words)

review_words=review_words%>%
  anti_join(stop_words)
  
#reduced words from 22 M to 7.7 M

#remove words that are not alphabetic
review_words=review_words%>%
  filter(!word %in% stop_words$word,
         str_detect(word,"^[a-z']+$"))

rm(review_words_mod,reviews)

review_words
#we'll now use sentiment analysis using the lexicon AFINN that provides a positive and negative score

data("sentiments")
head(sentiments)
AFINN=sentiments%>%
  filter(lexicon=="AFINN")%>%
  select(word,afinn_score=score)

#now we perform an inner join of AFINN with review_words
reviews_sentiment=review_words%>%
  inner_join(AFINN,by="word")%>%
  group_by(review_id,stars)%>%
  summarize(sentiment=mean(afinn_score))

reviews_sentiment

#lets see if the average sentiment correlates with the star rating

library(ggplot2)
ggplot(reviews_sentiment,aes(stars,sentiment,group=stars))+
  geom_boxplot()+ylab("Average Sentiment Score")
