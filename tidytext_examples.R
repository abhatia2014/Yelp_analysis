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

#convert to one token per row

?unnest_tokens
tidy_books=original_books%>%
  unnest_tokens(word,text)

?anti_join
data("sentiments")
tail(sentiments,50)
?sentiments
nrow(sentiments)

