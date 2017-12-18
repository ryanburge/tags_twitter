library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(httr)
library(dplyr)
library(wordcloud2)
library(extrafont)
library(scales)
library(patchwork)

tweets_dec <- read.csv("tweets_dec.csv")

tweets_dec <- tweets_dec %>%
  mutate(date = dmy_hms(time)) %>% mutate(day = as.Date(date))

count_tweets_dec <- tweets_dec %>% group_by(day) %>% count()

tweets <- read.csv("tweets.csv")


tweets <- tweets %>%
  mutate(date = dmy_hms(time)) %>% mutate(day = as.Date(date))

count_tweets <- tweets_dec %>% group_by(day) %>% count()


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

ggplot(count_tweets_dec, aes(x=day, y=n)) + 
  geom_col(fill = "cornflowerblue", color = "black") +
  bar_rb() + 
  scale_x_date(breaks = date_breaks("days"), labels = date_format("%b. %d")) +
  labs(x= "Date", y = "Number of Tweets", title = "Explosion in the Use of 'Evangelical' on Twitter")

ggsave(file="D://tags_twitter/count_day_dec.png", type = "cairo-png", width = 20, height = 15)



reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets_dec %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

count <- tidy_tweets %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) != '#', # omit hashtags
         substr(word, 1, 1) != '@') %>% # omit Twitter handles
  mutate(word = reorder(word, n))

count <- count[-1,]
count <- count[-7,]


wordcloud2(count)

a2 <- tidy_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(day, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(a1, aes(x=day, y=sentiment)) + 
  geom_col(fill = "firebrick3", color = "black") +
  bar_rb() +
  scale_x_date(breaks = date_breaks("days"), labels = date_format("%b. %d")) +
  labs(x= "Date", y = "Number of Tweets", title = "Explosion in the Use of 'Evangelical' on Twitter")

ggsave(file="D://tags_twitter/count_day_dec.png", type = "cairo-png", width = 20, height = 15)


tidy_bigrams_dec <- tweets_dec %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  mutate(next_word = lead(word)) %>%
  filter(!word %in% stop_words$word, # remove stop words
         !next_word %in% stop_words$word, # remove stop words
         substr(word, 1, 1) != '@', # remove user handles to protect privacy
         substr(next_word, 1, 1) != '@', # remove user handles to protect privacy
         substr(word, 1, 1) != '#', # remove hashtags
         substr(next_word, 1, 1) != '#',
         str_detect(word, "[a-z]"), # remove words containing ony numbers or symbols
         str_detect(next_word, "[a-z]")) %>% # remove words containing ony numbers or symbols
  filter(id_str == lead(id_str)) %>% # needed to ensure bigrams to cross from one tweet into the next
  unite(bigram, word, next_word, sep = ' ') %>%
  select(bigram, from_user, date, id_str, user_followers_count, user_friends_count, user_location)


bigrams_dec <- tidy_bigrams_dec %>%
  count(bigram, sort=TRUE) %>%
  filter(n >= 30) %>%
  mutate(bigram = reorder(bigram, n))


bi_dec <- tidy_bigrams_dec %>%
  count(bigram, sort=TRUE) %>%
  filter(n >= 150) %>%
  mutate(bigram = reorder(bigram, n))


bi2 <- bi_dec %>% 
  filter(n >150) %>% 
  ggplot(., aes(x=reorder(bigram,n), y=n)) +
  geom_col(color = "black", fill = "deepskyblue") +
  coord_flip()+
  flip_bar_rb() +
  labs(x="", y="", title = "Most Common Bigrams in Dec.") 



tidy_bigrams <- tweets %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  mutate(next_word = lead(word)) %>%
  filter(!word %in% stop_words$word, # remove stop words
         !next_word %in% stop_words$word, # remove stop words
         substr(word, 1, 1) != '@', # remove user handles to protect privacy
         substr(next_word, 1, 1) != '@', # remove user handles to protect privacy
         substr(word, 1, 1) != '#', # remove hashtags
         substr(next_word, 1, 1) != '#',
         str_detect(word, "[a-z]"), # remove words containing ony numbers or symbols
         str_detect(next_word, "[a-z]")) %>% # remove words containing ony numbers or symbols
  filter(id_str == lead(id_str)) %>% # needed to ensure bigrams to cross from one tweet into the next
  unite(bigram, word, next_word, sep = ' ') %>%
  select(bigram, from_user, date, id_str, user_followers_count, user_friends_count, user_location)


bigrams<- tidy_bigrams %>%
  count(bigram, sort=TRUE) %>%
  filter(n >= 30) %>%
  mutate(bigram = reorder(bigram, n))


bi <- tidy_bigrams %>%
  count(bigram, sort=TRUE) %>%
  filter(n >= 150) %>%
  mutate(bigram = reorder(bigram, n))


bi1 <- bi %>% 
  filter(n >150) %>% 
  ggplot(., aes(x=reorder(bigram,n), y=n)) +
  geom_col(color = "black", fill = "deepskyblue") +
  coord_flip()+
  flip_bar_rb() +
  labs(x="", y="", title = "Most Common Bigrams in Feb.-Mar.") 

bi1 + bi2


ggsave(file="D://tags_twitter/bigram_compare.png", type = "cairo-png", width = 22, height = 15)




reg <- "([^A-Za-z_\\d#@:/']|'(?![A-Za-z_\\d#@:/]))"
urls_temp <- tweets_dec %>%
  unnest_tokens(word, text, token = "regex", pattern = reg, to_lower = FALSE) %>%
  mutate(word = str_replace_all(word, "https|//t|http|&amp;|&lt;|&gt;", ""),
         word = str_replace_all(word, "co/", "https://t.co/")) %>%
  select(word) %>%
  filter(grepl('https://t.co/', word, fixed = TRUE)) %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word, n))

urls_common <- urls_temp %>%
  filter(n >= 25) %>%
  mutate(source_url = as.character(word)) %>%
  select(source_url, count = n)


url <- t(sapply(urls_common$source_url, GET)) %>%
  as_tibble() %>%
  select(url, status_code)

url_list <- cbind(urls_common, unnest(url)) %>%
  as_tibble() %>%
  select(url, count, status_code) %>%
  filter(status_code != 404,
         url != 'https://t.co/',
         !grepl('https://twitter.com/', url))

extract_domain <- function(url) {
  return(gsub('www.', '', unlist(strsplit(unlist(strsplit(as.character(url), '//'))[2], '/'))[1]))
}

# count the frequency of a domain's occurrence in the most frequent URL list
domain_list_dec <- url_list %>%
  mutate(domain = mapply(extract_domain, url)) %>%
  group_by(domain) %>%
  summarize(domain_count = sum(count)) %>%
  arrange(desc(domain_count))




domain1 <- domain_list %>% 
  filter(domain_count >50) %>% 
  ggplot(., aes(x=reorder(domain,domain_count), y=domain_count)) +
  geom_col(color = "black", fill = "darkslateblue") +
  coord_flip()+
  flip_bar_rb() +
  labs(x="", y="", title = "Domains in Feb.-Mar.") 


domain2 <- domain_list_dec %>% 
  filter(domain_count >50) %>% 
  ggplot(., aes(x=reorder(domain,domain_count), y=domain_count)) +
  geom_col(color = "black", fill = "darkslateblue") +
  coord_flip()+
  flip_bar_rb() +
  labs(x="", y="", title = "Domains in Dec.") 

domain1 + domain2

ggsave(file="D://tags_twitter/domain_compare.png", type = "cairo-png", width = 18, height = 15)


domain_list %>%
  mutate(domain = reorder(domain, domain_count)) %>%
  filter(domain_count > 75) %>% 
  ggplot(aes(domain, domain_count)) +
  geom_bar(color = "black", fill = "deepskyblue", stat = 'identity') +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle(paste('Domains linked in tweets containing evangelical')) +
  theme(legend.position="none") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans"))
  
