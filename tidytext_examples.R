# Jane Austin Novels using janeaustenr package

library(janeaustenr)
library(dplyr)
library(stringr)

original_books=austen_books() %>%
  group_by(book)%>%
  mutate(linenumber=row_number(),
         chapter=cumsum(str_detect(text,regex("^(Chapter|CHAPTER [\\dIVXLC])",ignore.case=TRUE))))%>%
  ungroup()

original_books
table(original_books$chapter)
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

#lets now use bing lexicon to count the number of positive and negative words

library(tidyr)

bing=sentiments%>%
  filter(lexicon=="bing")%>%
  select(-score)
#6788 words expressing positive and negative sentiments from the bing lexicon

#sentiment analysis
#using inner_join with the bing dataset, getting  matching rows in bing and all
#columns from both datasets
#then counting the number of positive and negative words in defined sections of each novel

?count

janeautensentiment=tidy_books%>%
  #inner joins with bing, retaining all columns and matching rows
  inner_join(bing)%>%
  # counts the number of instances of positive and negative words 
  #and by index 
  count(book,index=linenumber %/% 80,sentiment)%>%
  #the spread function spreads the sentiment separately as positive and negative
  spread(sentiment,n, fill=0)%>%
  # find the diffference of the positive and negative sentiment
  mutate(sentiment=positive-negative)
  

#now we can plot these sentiments across the plot trajectory of each novel

library(ggplot2)
  
ggplot(janeautensentiment,aes(index,sentiment,fill= book))+
  geom_bar(stat="identity",show.legend = FALSE)+
  facet_wrap(~book, ncol=2,scales = "free_x")
  
#lets find the most common positive and negative words in the tidy_books

bingwordcounts=tidy_books%>%
  inner_join(bing)%>%
  count(word,sentiment,sort=TRUE)%>%
  ungroup()

#lets pipe it straight into ggplot2
#also let's remove miss word from the dataset

bingwordcounts%>%
  filter(!word=="miss")%>%
  filter(n>100)%>%
  mutate(n=ifelse(sentiment=="negative",-n,n))%>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n,fill=sentiment))+
  geom_bar(stat="identity",show.legend = FALSE)+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ylab("contribution to sentiment")


# Wordclouds --------------------------------------------------------------

#load the wordcloud packages

library(wordcloud)

tidy_books%>%
  count(word)%>%
  with(wordcloud(word,n,max.words=100,random.color = TRUE))
  
  
#let's use the other funtion, comparison cloud

library(reshape2)
?acast
tidy_books%>%
  inner_join(bing)%>%
  count(word, sentiment, sort=TRUE)%>%
  acast(word~sentiment,value.var="n",fill=0)%>%
  comparison.cloud(colors=c("red","blue"),max.words=100)


# Tokenize into sentences -------------------------------------------------

PandP_Sentences=data_frame(text=prideprejudice)%>%
  unnest_tokens(sentence,text,token='sentences')

#let's look at one sentence

PandP_Sentences$sentence[2]

#let's split the Jane Austen novels using a regex pattern
#we split into a dataframe by chapters

austen_chapter=austen_books()%>%
  group_by(book)%>%
  unnest_tokens(chapter,text,token="regex",pattern="Chapter|CHAPTER [\\dIVXLC]")%>%
  ungroup()
#this gives the number of chapters by books
austen_chapter%>%
  group_by(book)%>%
  summarise(chapters=n())

#to find what are the most negative chapters in all of Jane Austin's books

#first let's get the list of all negative words from the bing lexicon

bingnegative=sentiments%>%
  filter(lexicon=="bing",sentiment=="negative")
#4782 words

#lets make a dataframe of number of words in each chapter

wordcounts=tidy_books%>%
  group_by(book, chapter)%>%
  summarize(words=n())


#next let's find the number of negative words in each chapter and divide that by the total # of words

#using left join- return all rows from x and all columns from x, y- NA for rows in x 
#with no match in y, all values are returned for multiple matches

tidy_books%>%
  semi_join(bingnegative)%>%
  group_by(book,chapter)%>%
  summarise(negativewords=n())%>%
  left_join(wordcounts,by=c("book","chapter"))%>%
  mutate(ratio=negativewords/words)%>%
  top_n(1)
  