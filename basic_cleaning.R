library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(httr)
library(dplyr)
library(wordcloud2)

tweets <- read.csv("tweets.csv")

tweets <- tweets %>%
  mutate(date = mdy(paste(substring(created_at, 5, 10), substring(created_at, 27 ,30))))

reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>%
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

wordcloud2(count)


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


bigrams <- tidy_bigrams %>%
  count(bigram, sort=TRUE) %>%
  filter(n >= 30) %>%
  mutate(bigram = reorder(bigram, n))

tidy_bigrams %>%
  count(bigram, sort=TRUE) %>%
  filter(n >= 150) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_bar(stat = 'identity', color = "black", fill = "deepskyblue") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle(paste('Most common bigrams containing evangelical')) +
  theme(legend.position="none") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=18, family="KerkisSans"))

reg <- "([^A-Za-z_\\d#@:/']|'(?![A-Za-z_\\d#@:/]))"
urls_temp <- tweets %>%
  unnest_tokens(word, text, token = "regex", pattern = reg, to_lower = FALSE) %>%
  mutate(word = str_replace_all(word, "https|//t|http|&amp;|&lt;|&gt;", ""),
         word = str_replace_all(word, "co/", "https://t.co/")) %>%
  select(word) %>%
  filter(grepl('https://t.co/', word, fixed = TRUE)) %>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word, n))

urls_common <- urls_temp %>%
  filter(n >= 5) %>%
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
domain_list <- url_list %>%
  mutate(domain = mapply(extract_domain, url)) %>%
  group_by(domain) %>%
  summarize(domain_count = sum(count)) %>%
  arrange(desc(domain_count))

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
  
