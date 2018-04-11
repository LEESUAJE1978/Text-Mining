library(tidytext)
sentiments

#the three general-purpose lexicons are
#AFINN from Finn Arup Nielsen(http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010), score between -5 and 5
#Bing from Bing Liu and collaborators(https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html). negative, positive
#NRC from Saif Mohammad and Peter Turney(http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm), positive, negative, anger, anticipation, 
#disgust, fear, joy, sadness, surprise and trust. 


###3 type of sentiment lexicon 감성 사전 3종, 'afinn', 'bing', 'nrc'
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books<- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber =row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter[\\divxlc]",
                                                 ignore_case = TRUE)))) %>% 
  ungroup() %>% 
  unnest_tokens(word, text)
tidy_books

nrc_joy<- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

nrc_joy

tidy_books %>% 
filter(book == "Emma") %>% 
  inner_join(nrc_joy) %>% 
  count(word, sort = TRUE)

library(tidyr)

janeaustensentiment<- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(book, index =linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% #tidyr package
  mutate(sentiment = positive - negative)

library(ggplot2)
ggplot(janeaustensentiment, aes(index, sentiment, fill =book))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~book, ncol = 2, scales = "free_x")


pride_prejudice<- tidy_books %>% 
  filter(book=="Pride & Prejudice")

###.p.20
afinn<- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber%/% 80) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")


bing_and_nrc<-bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>% 
    mutate(method="Bing et al."),
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc")) %>% 
    filter(sentiment %in% c("positive","negative"))) %>% 
  mutate(method = "NRC") %>% 
  count(method, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill =0) %>% 
  mutate(sentiment = positive-negative)
  

bind_rows(afinn, bing_and_nrc) %>% 
  ggplot(aes(index, sentiment, fill=method))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~method, ncol = 1, scales = "free_y")

bing_word_counts<- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = T) %>% 
  ungroup()

bing_word_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill= sentiment))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y ="Contribution to sentiment", x=NULL)+
  coord_flip()

### Stop_words 추가하기, 불용어 처리
custom_stop_words<-bind_rows(data_frame(word =c("miss"), lexicon=c("custom")), stop_words)
custom_stop_words


###Wordclouds
library(wordcloud)
tidy_books %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100))
