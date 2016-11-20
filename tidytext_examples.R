getwd()

# Evaluating Jane Austin Novels Using Janeautenr package ------------------


library(janeaustenr)
library(dplyr)
library(stringr)
?regex
original_books=austen_books() %>%
  group_by(book)%>%
  mutate(linenumber=row_number(),
         chapter=cumsum(str_detect(text,regex("^(Chapter|CHAPTER [\\dIVXLC])",ignore.case=TRUE))))%>%
  ungroup()

original_books
table(original_books$chapter,by=original_books$book)
library(tidytext)
library(mlr)
#convert to one token per row format using the unnest_tokens function

?unnest_tokens
tidy_books=original_books%>%
  unnest_tokens(word,text)
#725054 rows
tidy_books
#now the data is in one word per row format
names(tidy_books)
#We can now remove the stop words from the dataset using anti_join function of dplyr

?anti_join
#anti join returns all rows from x where there are not matching values in y keeping just the columns from x

data("stop_words")
head(stop_words,30)
table(stop_words$lexicon)
#stop_words is a dataset in tidy_Text

tidy_books=tidy_books%>%
  anti_join(stop_words)%>%
  arrange(linenumber)
#217609 words, 
725054-217609
# remove 507445 stop words

?count
# we can use count to find the most common words in the book as a whole

tidy_books%>%
  count(word,sort=TRUE)
#interestingly the word 'miss' has been used 1855 times


# Sentiment Analysis ------------------------------------------------------

#using an inner join (intersection of x,y with columns from both)
#inner join- returns all rows from x where there are matching values in y, and all 
#columns from x and y

#sentiments dataset
data("sentiments")
sentiments
nrow(sentiments)
str(sentiments)
table(sentiments$lexicon)
# 3 lexicons AFINN, bing, nrc

table(sentiments$sentiment,by=sentiments$lexicon)
#sentiments - anger, anticipation, disgust, fear, joy, negative, positive, sadness, surprise,trust

#positive and negative part of the bing lexicon 

#let's look at the words with the joy score in the nrc lexicon, (689 words)
#most common joy words in Emma novel

nrcjoy=sentiments%>%
  filter(lexicon=="nrc",sentiment=="joy")
#total 689 words in the lexicon, let's see how many words in Emma novel

tidy_books
?semi_join
#using semi_join function of dplyr (intersection, keeping just values of x)
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
#for all novels, hope is used 601 times and fried 593 times

#separate out the nrc lexicon

emotion_words=sentiments%>%
  filter(lexicon=="nrc")

#find the number of emotion words by book


book_emotions=tidy_books%>%
  inner_join(emotion_words)%>%
  group_by(book)%>%
  count(sentiment,sort=TRUE)

#count the total number of words by book
word_book=tidy_books%>%
  group_by(book)%>%
  summarize(total_words=n())

#join the dataframes book_emotions and word_book



book_emotions_join=book_emotions%>%
  inner_join(word_book)%>%
  mutate(emotion_percent=round(n/total_words*100,1))

#plot it using ggplot
#theme(axis.text.x=element_text(angle=90,hjust=1))+
library(ggplot2)

ggplot(book_emotions_join,aes(sentiment,emotion_percent,fill=sentiment))+geom_bar(stat='identity',show.legend = FALSE)+
  facet_wrap(~book,scales = "free_y")+theme(axis.text.x=element_text(angle = 90,hjust=1))+
  geom_text(aes(sentiment,emotion_percent,label=emotion_percent),size=3)+labs(x="Emotions",y="Emotion Percentage",title="Emotion analysis of Jane Austen Novels")

ggplot(book_emotions,aes(book,n,fill=book))+geom_bar(stat='identity',show.legend = FALSE)+
  facet_wrap(~sentiment,scales = "free_y")+theme(axis.text.x=element_text(angle = 90,hjust=1))+
  geom_text(aes(book,n+5,label=n),size=3)+labs("Books",y="# of Instances",title="Tone Analysis of Jane Austin Novels")

#we will find the flow of emotions through the chapters of a single book
#let's look at the Book EMMA

tidy_book_emma=tidy_books%>%
  filter(book=="Emma")
#Let's look at the NRC lexicon only and remove Positive and Negative words
#we'll use the emotion_words dataframe created previously

emotion_words_trim=emotion_words%>%
  filter(!(sentiment %in% c("positive","negative")))

str(tidy_book_emma)

# we want to join the tidy_book_emma dataframe with emotion words trim using inner join

#and count the sentiments grouped by chapter

tidy_book_emma_emotion=tidy_book_emma%>%
  inner_join(emotion_words_trim)%>%
  group_by(chapter)%>%
  count(sentiment)

#first let's plot the emotions as a chapter facet

#we'll see how the emotion varies by chapter

tail(tidy_book_emma_emotion)
ggplot(tidy_book_emma_emotion,aes(chapter,n,color=sentiment))+geom_bar(stat="identity",show.legend = FALSE)+
  facet_wrap(~sentiment,ncol = 4)+xlim(0,60)+labs(x="Chapters",y="Count of words expressing emotions",title="Emotion Analysis of Jane Austen's Novel - Emma ")

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
  
#calculate the total sentiment

janeautensentiment%>%
  group_by(book)%>%
  summarise(sum_sentiment=sum(sentiment))

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

?wordcloud
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
  

# Network of Words --------------------------------------------------------

pride_prejudice_words=tidy_books%>%
  filter(book=="Pride & Prejudice")

#pair_counts counts the pair of items that occur together within a group
#here we count the words that occur together in pride and prejudice

word_occurances=pride_prejudice_words%>%
  pair_count(linenumber,word,sort=TRUE)
  
word_occurances

#now we'll plot this as a network of co-occuring words with the igraph and ggraph packages
install.packages("devtools")

devtools::install_github('hadley/ggplot2')
devtools::install_github('thomasp85/ggforce')
devtools::install_github('thomasp85/ggraph')
library(igraph)

set.seed(1813)

#predictive analytics (machine learning)
#Now we'll try and predict the book, given the words, 
#let's consider the tidy_books dataset

tidy_books
  
#remove linenumber and chapter

tidy_books_try=tidy_books[,c(1,4)]

str(tidy_books_try)

#take a sample out as training set and testset

trainID=sample(nrow(tidy_books_try),0.65*nrow(tidy_books_try),replace = FALSE)

head(trainID)
testID=setdiff(1:nrow(tidy_books_try),trainID)
length(trainID)
length(testID)
#change the word to feature factor

tidy_books_try$word=factor(tidy_books_try$word)
tidy.train=tidy_books_try[trainID,]
tidy.test=tidy_books_try[testID,]

#create a train and test task

tidy.train=as.data.frame(tidy.train)
tidy.test=as.data.frame(tidy.test)

tidy.train.task=makeClassifTask(data = tidy.train,target = "book")

#similarly create a task for tidy.test.task

tidy.test.task=makeClassifTask(data=tidy.test,target = "book")

tidy.train.task

#find models for multiclassification

mylearners=listLearners(tidy.train.task)


# Benchmarking to select the right model ----------------------------------

learners=list(makeLearner('classif.C50'),makeLearner('classif.boosting'),makeLearner('classif.glmnet'))


resamp.bench=makeResampleDesc("Holdout")

tidy.benchmark=benchmark(learners = learners,tasks = tidy.train.task,resamplings = resamp.bench,measures = list(acc,mmce))



#we'll use ksvm to do the predictions

getParamSet("classif.ksvm")

#1. make learner

ksvmlearner=makeLearner("classif.ksvm",predict.type = "response")

#2. create the search space

searchksvm=makeParamSet(
  makeDiscreteParam("C",values=2^c(-8,-4,-2,0,2)),
  makeDiscreteParam("sigma",values=2^c(-8,-4,0,4))
  
)

#3. specify search algorithm

search.algo.ksvm=makeTuneControlGrid()

#4. specify resampling strategy

sample.ksvm=makeResampleDesc("CV",iters=3)

#5. set performance measures

measures.ksvm=acc

#tuning the hyperparameters to select the best tuning parameters

ksvm.tune=tuneParams(learner = ksvmlearner,task = tidy.train.task,resampling = sample.ksvm,
                    par.set = searchksvm,control = search.algo.ksvm,measures = measures.ksvm)
