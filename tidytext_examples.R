# Jane Austin Novels using janeaustenr package

library(janeaustenr)
library(dplyr)
library(stringr)

original_books=austen_books() %>%
  group_by(book)%>%
  mutate(linenumber=row_number(),
         chapter=cumsum(str_detect(text,regex("^chapter[\\divxlc]",ignore.case=TRUE))))%>%
  ungroup()

original_books

library(tidytext)

#convert to one token per row format using the unnest_tokens function

?unnest_tokens
tidy_books=original_books%>%
  unnest_tokens(word,text)
#725054 rows
tidy_books
#now the data is in one word per row format

#We can now remove the stop words from the dataset using anti_join function of dplyr

?anti_join
#anti join returns all rows from x where there are not matching values in y keeping just the columns from x

data("stop_words")
#stop_words is a dataset in tidy_Text

tidy_books=tidy_books%>%
  anti_join(stop_words)
#217609 words, 
725054-217609
# remove 507445 stop words

?count
# we can use count to find the most common words in the book as a whole

tidy_books%>%
  count(word,sort=TRUE)
#interestingly the word 'miss' has been used 1855 times


# Sentiment Analysis ------------------------------------------------------

#using an inner join
#inner join- returns all rows from x where there are matching values in y, and all 
#columns from x and y

#sentiments dataset
data("sentiments")
nrow(sentiments)
str(sentiments)
table(sentiments$lexicon)
# 3 lexicons AFINN, bing, nrc

table(sentiments$sentiment)
#sentiments - anger, anticipation, disgust, fear, joy, negative, positive, sadness, surprise,trust

#positive and negative part of the bing lexicon 

#let's look at the words with the joy score in the nrc lexicon,
#most common joy words in Emma novel

nrcjoy=sentiments%>%
  filter(lexicon=="nrc",sentiment=="joy")
#total 689 words in the lexicon, let's see how many words in Emma novel

tidy_books

#using semi_join function of dplyr
#semi_join- returns all rows of x where there are matching values in y,keeping
#only columns from x

tidy_books%>%
  filter(book=="Emma")%>%
  semi_join(nrcjoy)%>%
  count(word,sort=TRUE)

#total 298 joy words in the Emma book with 'friend' used most 166 times, and 
#'hope' used 143 times

#do this analysis for all novels in tidy_books

tidy_books%>%
  semi_join(nrcjoy)%>%
  count(word, sort=TRUE)
#for all novels, hope is used 601 times


