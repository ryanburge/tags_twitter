library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(httr)
library(dplyr)
library(wordcloud2)
library(extrafont)
library(scales)

tweets_dec <- read.csv("tweets_dec.csv")

tweets_dec <- tweets_dec %>%
  mutate(date = dmy_hms(time)) %>% mutate(day = as.Date(date))

count_tweets_dec <- tweets_dec %>% group_by(day) %>% count()

tweets <- read.csv("tweets.csv")


tweets <- tweets %>%
  mutate(date = dmy_hms(time)) %>% mutate(day = as.Date(date))

count_tweets <- tweets %>% group_by(day) %>% count()

t_count <- bind_rows(count_tweets, count_tweets_dec) %>% mutate(date = factor(day))


bar_rb <- function(base_size = 25, base_family = "IBM Plex Serif") 
{theme(legend.position = "bottom", 
       legend.title = element_blank(), 
       legend.spacing.x = unit(1, "cm"),
       legend.spacing.y = unit(1, "cm"),
       panel.background = element_rect(fill = "white"), 
       panel.grid.major.y =  element_line(colour = "gray48", size = .25), 
       panel.grid.minor.y =  element_line(colour = "gray48", size = .25, linetype = "dashed"),
       text = element_text(base_family, size = 28),
       plot.title = element_text(family = "IBM Plex Serif", size = 40, vjust =2, face = "bold"),
       plot.subtitle = element_text(family = "IBM Plex Serif", size = 20, vjust =-1),
       plot.caption = element_text(family = "IBM Plex Serif", size =20),
       axis.title.x =  element_text(family = "IBM Plex Serif", size =32),
       axis.title.y =  element_text(family = "IBM Plex Serif", size =32), 
       axis.text.x = element_text(family = "IBM Plex Serif", size =24, angle = 45, hjust = 1)
)
  
}

t_count %>% 
  na.omit() %>% 
  ggplot(., aes(x=as.factor(day), y=n)) + 
  geom_col(fill = "cornflowerblue", color = "black") +
  bar_rb() + 
  # scale_x_date(breaks = date_breaks("weeks"), labels = date_format("%b. %d")) +
  labs(x= "Date", y = "Number of Tweets", title = "Explosion in the Use of 'Evangelical' on Twitter") +
  geom_vline(xintercept = 38.45, linetype = "dashed", color = "red", size = 2) +
  geom_rect(data=NULL,aes(xmin=38.45,xmax=44.5,ymin=0,ymax=Inf), fill="gray74", alpha = 0.015)

ggsave(file="D://tags_twitter/count_day_compare_small.png", type = "cairo-png", width = 18, height = 15)



reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets_dec <- tweets_dec %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


tidy_tweets <- tweets %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))



a1 <- tidy_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(day, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) 


a2 <- tidy_tweets_dec %>%
  inner_join(get_sentiments("bing")) %>%
  count(day, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) 


sent <- bind_rows(a1, a2)

sent <- sent  %>% 
  mutate(group = c("Feb. - Mar.",  "Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.","Feb. - Mar.", "December", "December", "December","December","December","December" ))

sent$group_f = factor(sent$group, levels=c('Feb. - Mar.','December'))



ggplot(sent, aes(x=day, y=sentiment)) + 
  geom_col(fill = "red4", color = "black") +
  bar_rb() + 
  # scale_x_date(breaks = date_breaks("weeks"), labels = date_format("%b. %d")) +
  labs(x= "Date", y = "Total Daily Sentiment", title = "The Sentiment of Tweets Containing 'Evangelical'", subtitle = "Using the 'Bing' Lexicon") +
  facet_grid(~group_f, scale = "free_x")



ggsave(file="D://tags_twitter/sentiment_compare_facet_small.png", type = "cairo-png", width = 18, height = 15)


positive <- tidy_tweets_dec %>% 
  inner_join(get_sentiments("bing")) %>% 
  filter(sentiment == "positive") %>% count(word) %>% arrange(-n)

negative <- tidy_tweets_dec %>% 
  inner_join(get_sentiments("bing")) %>% 
  filter(sentiment == "negative") %>% count(word) %>% arrange(-n)


pos <- positive %>% 
  filter(n >150) %>% 
  ggplot(., aes(x=reorder(word,n), y=n)) +
  geom_col(color = "black", fill = "darkorchid") +
  coord_flip()+
  flip_bar_rb() +
  labs(x="", y="", title = "Most Common Positive Words") 

neg <- negative %>% 
  filter(n >150) %>% 
  ggplot(., aes(x=reorder(word,n), y=n)) +
  geom_col(color = "black", fill = "black") +
  coord_flip()+
  flip_bar_rb() +
  labs(x="", y="", title = "Most Common Negative Words") 

pos + neg

ggsave(file="D://tags_twitter/sentiment_compare_words.png", type = "cairo-png", width = 18, height = 15)

