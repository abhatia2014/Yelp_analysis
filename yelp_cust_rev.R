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

cor(reviews_sentiment$stars,reviews_sentiment$sentiment)
#57.3% positive correlation

#though it is well correlated, there are some five star ratings with negative sentiment score

#which words are suggestive of positive reviews and which words are suggestive of negative reviews

?count
review_words_counted=review_words%>%
  count(review_id,business_id,stars,word)%>%
  ungroup()

word_summaries=review_words_counted%>%
  group_by(word)%>%
  summarize(businesses=n_distinct(business_id),
            reviews=n(),
            uses=sum(n),
            average_stars=mean(stars))%>%
  ungroup()

word_summaries
tail(word_summaries,10)

#let's only look at words that appear in atleast 200 times out of 2M reviews
#also only filter out the words that appear in atleast 10 businesses


word_summaries_filtered=word_summaries%>%
  filter(reviews>=200,businesses>=10)
word_summaries_filtered

#what are the most positive or negative words based on star ratings

word_summaries_filtered%>%
  arrange(desc(average_stars))

#and the most negative

word_summaries_filtered%>%
  arrange(average_stars)

#lets now plot positivity by frequency

ggplot(word_summaries_filtered,aes(reviews,average_stars))+
  geom_point()+geom_text(aes(label=word),check_overlap = TRUE,vjust=1,hjust=1,size=2)+
  scale_x_log10()+
  geom_hline(yintercept = mean(review_words$stars),color="red",lty=2)+
  xlab("#of Reviews")+ylab("Average Stars")

#compare with the scores from AFINN lexicon using innerjoin

words_afinn=word_summaries_filtered%>%
  inner_join(AFINN)
words_afinn

#plot it using ggplot

ggplot(words_afinn,aes(afinn_score,average_stars,group=afinn_score))+
  geom_boxplot()+
  xlab("AFINN Score of Word")+
  ylab("Average Star reviews")

#so here there is a clear correlation between the affin_score and average_Stars

cor(words_afinn$average_stars,words_afinn$afinn_score)
#+6646 correlation which is very high

#to check which positive and negative words were more successful in predicting

ggplot(words_afinn,aes(afinn_score,average_stars,group=afinn_score))+
  geom_point()+
  geom_text(aes(label=word),check_overlap = TRUE,vjust=1,hjust=1,size=2)
