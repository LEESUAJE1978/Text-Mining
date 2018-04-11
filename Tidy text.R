#The Unnest_tokens function
library(dplyr)
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
#list와 character의 차이

#transform character into data frame
text_df<- data_frame(line=1:4, text=text)
text_df

#tokenization 
library(tidytext)
text_df %>% 
  unnest_tokens(word, text, to_lower = TRUE) #to_lower는 TRUE 가 기본값

#janeaustenr 패키지 활용하여 Jane Austen의 6개의 소설 분석

library(janeaustenr)
library(stringr)

original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>% 
  ungroup() #groupdf vs tbl_df 차이

original_books

#tokenization
tidy_books <- original_books %>% 
  unnest_tokens(word, text)
tidy_books

#stop word remove with anti_join()
data("stop_words")
tidy_books<-tidy_books %>% 
  anti_join(stop_words)
tidy_books

#Word count
tidy_books %>% 
  count(word, sort = TRUE)

#data visualization
library(ggplot2)
tidy_books %>% 
  count(word, sort = TRUE) %>% 
  filter(n >600) %>% 
  mutate(word =reorder(word, n)) %>% 
  ggplot(aes(word,n ))+
  geom_col()+
  xlab(NULL)+
  coord_flip()


#gutenbergr package(Robinson 2016)
#reference : http://www.gutenberg.org/ebooks/
#"The Time Machine"(35), "The War of the Worlds"(36), "The Invisible Man"(5230), "The Island of Doctor Moreau"(159)

library(gutenbergr)
hgwells <- gutenberg_download(c(35,36,5239,159))
tidy_hgwells <- hgwells %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

#frequency of words
tidy_hgwells %>% 
  count(word, sort = T)

#2. "Jane Eyre"(1260), "Wuthering Heights"(768), "The Tenant of Wildfell Hall"(969), "Villette"(9182), "Agenes Grey"(767)
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

#frequency of words
tidy_bronte %>% 
  count(word, sort = T)


#reshape three sets of novels 
library(tidyr)
frequency <- bind_rows(mutate(tidy_hgwells, author = "H.G. Wells"),
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word =str_extract(word, "[a-z']+")) %>% 
  count(author,word) %>% 
  group_by(author) %>% 
  mutate(proportion = n /sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, "H.G. Wells":"Jane Austen")

#plot
library(scales)
ggplot(frequency, aes(x = proportion, y = 'H.G. Wells',
                      color = abs('H.G. Wells'- proportion)))+
  geom_abline(color = "gray40", lty =2)+
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3)+
  geom_text(aes(label = word), check_overlap = TRUE, vjust =1.5)+
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels = percent_format())+
  scale_color_gradient(limits = c(0,0.001),
                       low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y = "H.G. Wells", x = NULL)
