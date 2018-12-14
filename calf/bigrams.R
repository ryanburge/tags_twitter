## I need to divide these up to give me words by each block of time. 
## This is February and March of 2017
first <- tweets %>% 
  filter(month == 2 | month == 3 | month == 4) %>% 
  filter(year == 2017) 



tidy_bigrams <- first %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  mutate(next_word = lead(word)) %>%
  filter(!word %in% stop_words$word, # remove stop words
         !next_word %in% stop_words$word, # remove stop words
         substr(word, 1, 1) != '@', # remove user handles to protect privacy
         substr(next_word, 1, 1) != '@', # remove user handles to protect privacy
         substr(word, 1, 1) != '#', # remove hashtags
         substr(next_word, 1, 1) != '#',
         str_detect(word, "[a-z]"), # remove words containing ony numbers or symbols
         str_detect(next_word, "[a-z]")) %>% # remove words containing ony numbers or symbols
  filter(status_id == lead(status_id)) %>% # needed to ensure bigrams to cross from one tweet into the next
  unite(bigram, word, next_word, sep = ' ')


bigrams <- tidy_bigrams %>%
  count(bigram, sort=TRUE) %>%
  filter(n >= 30) %>%
  mutate(bigram = reorder(bigram, n))



bigrams <- bigrams %>% 
  head(25)

bigrams$bigram <- iconv(bigrams$bigram, "latin1", "ASCII", sub="")

bigrams$pol <- c(0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0)


bigrams %>% 
  mutate(pol = as.factor(pol))  %>% 
  ggplot(., aes(x=reorder(bigram, n), y = n, fill = pol)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("azure3", "firebrick3")) +
  coord_flip() +
  geom_text(aes(y = n - 85, label = n), position = position_dodge(width = .9), size = 10, family = "font") +
  theme_gg("Roboto Slab") +
  labs(x = "", y = "Count", title = "Most Frequent Bigrams in Feb. + Mar. 2017", caption = "Data: Twitter REST API") + 
  ggsave("D://tags_twitter/calf/first_bigram.png", width = 7)


## I need to divide these up to give me words by each block of time. 
## This is February and March of 2017
first <- tweets %>% 
  filter(month == 12) %>% 
  filter(year == 2017) 



tidy_bigrams <- first %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  mutate(next_word = lead(word)) %>%
  filter(!word %in% stop_words$word, # remove stop words
         !next_word %in% stop_words$word, # remove stop words
         substr(word, 1, 1) != '@', # remove user handles to protect privacy
         substr(next_word, 1, 1) != '@', # remove user handles to protect privacy
         substr(word, 1, 1) != '#', # remove hashtags
         substr(next_word, 1, 1) != '#',
         str_detect(word, "[a-z]"), # remove words containing ony numbers or symbols
         str_detect(next_word, "[a-z]")) %>% # remove words containing ony numbers or symbols
  filter(status_id == lead(status_id)) %>% # needed to ensure bigrams to cross from one tweet into the next
  unite(bigram, word, next_word, sep = ' ')


bigrams <- tidy_bigrams %>%
  count(bigram, sort=TRUE) %>%
  filter(n >= 30) %>%
  mutate(bigram = reorder(bigram, n))


bigrams <- bigrams %>% 
  head(25)

bigrams$bigram <- iconv(bigrams$bigram, "latin1", "ASCII", sub="")

bigrams$pol <- c(0,0,0,0,1,0,1,0,1,1,0,0,0,1,0,1,0,0,1,1,1,0,1,0,1)


bigrams %>% 
  mutate(pol = as.factor(pol))  %>% 
  ggplot(., aes(x=reorder(bigram, n), y = n, fill = pol)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("azure3", "firebrick3")) +
  coord_flip() +
  geom_text(aes(y = n - 85, label = n), position = position_dodge(width = .9), size = 10, family = "font") +
  theme_gg("Roboto Slab") +
  labs(x = "", y = "Count", title = "Most Frequent Bigrams in Dec. 2017", caption = "Data: Twitter REST API") + 
  ggsave("D://tags_twitter/calf/second_bigram.png", width = 7)
  


## I need to divide these up to give me words by each block of time. 
## This is April of 2018
first <- tweets %>% 
  filter(month == 4) %>% 
  filter(year == 2018) 



tidy_bigrams <- first %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  mutate(next_word = lead(word)) %>%
  filter(!word %in% stop_words$word, # remove stop words
         !next_word %in% stop_words$word, # remove stop words
         substr(word, 1, 1) != '@', # remove user handles to protect privacy
         substr(next_word, 1, 1) != '@', # remove user handles to protect privacy
         substr(word, 1, 1) != '#', # remove hashtags
         substr(next_word, 1, 1) != '#',
         str_detect(word, "[a-z]"), # remove words containing ony numbers or symbols
         str_detect(next_word, "[a-z]")) %>% # remove words containing ony numbers or symbols
  filter(status_id == lead(status_id)) %>% # needed to ensure bigrams to cross from one tweet into the next
  unite(bigram, word, next_word, sep = ' ')


bigrams <- tidy_bigrams %>%
  count(bigram, sort=TRUE) %>%
  filter(n >= 30) %>%
  mutate(bigram = reorder(bigram, n))


bigrams <- bigrams %>% 
  head(25)

bigrams$bigram <- iconv(bigrams$bigram, "latin1", "ASCII", sub="")

bigrams$pol <- c(1,0,1,0,0,1,1,1,1,1,0,1,0,1,1,1,0,0,1,0,1,1,1,1,0)


bigrams %>% 
  mutate(pol = as.factor(pol))  %>% 
  ggplot(., aes(x=reorder(bigram, n), y = n, fill = pol)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("azure3", "firebrick3")) +
  coord_flip() +
  geom_text(aes(y = n - 65, label = n), position = position_dodge(width = .9), size = 10, family = "font") +
  theme_gg("Roboto Slab") +
  labs(x = "", y = "Count", title = "Most Frequent Bigrams in Apr. 2018", caption = "Data: Twitter REST API") + 
  ggsave("D://tags_twitter/calf/third_bigram.png", width = 7)



## I need to divide these up to give me words by each block of time. 
## This is April of 2018
first <- tweets %>% 
  filter(month == 8) %>% 
  filter(year == 2018) 



tidy_bigrams <- first %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  mutate(next_word = lead(word)) %>%
  filter(!word %in% stop_words$word, # remove stop words
         !next_word %in% stop_words$word, # remove stop words
         substr(word, 1, 1) != '@', # remove user handles to protect privacy
         substr(next_word, 1, 1) != '@', # remove user handles to protect privacy
         substr(word, 1, 1) != '#', # remove hashtags
         substr(next_word, 1, 1) != '#',
         str_detect(word, "[a-z]"), # remove words containing ony numbers or symbols
         str_detect(next_word, "[a-z]")) %>% # remove words containing ony numbers or symbols
  filter(status_id == lead(status_id)) %>% # needed to ensure bigrams to cross from one tweet into the next
  unite(bigram, word, next_word, sep = ' ')


bigrams <- tidy_bigrams %>%
  count(bigram, sort=TRUE) %>%
  filter(n >= 30) %>%
  mutate(bigram = reorder(bigram, n))


bigrams <- bigrams %>% 
  head(25)

bigrams$bigram <- iconv(bigrams$bigram, "latin1", "ASCII", sub="")

bigrams$pol <- c(0,1,1,1,1,1,1,0,1,1,0,1,1,1,0,0,1,1,1,1,1,0,1,1,1)


bigrams %>% 
  mutate(pol = as.factor(pol))  %>% 
  ggplot(., aes(x=reorder(bigram, n), y = n, fill = pol)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("azure3", "firebrick3")) +
  coord_flip() +
  geom_text(aes(y = n - 95, label = n), position = position_dodge(width = .9), size = 10, family = "font") +
  theme_gg("Roboto Slab") +
  labs(x = "", y = "Count", title = "Most Frequent Bigrams in Aug. 2018", caption = "Data: Twitter REST API") + 
  ggsave("D://tags_twitter/calf/fourth_bigram.png", width = 7)



## I need to divide these up to give me words by each block of time. 
## This is April of 2018
first <- tweets %>% 
  filter(month == 12) %>% 
  filter(year == 2018) 



tidy_bigrams <- first %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  mutate(next_word = lead(word)) %>%
  filter(!word %in% stop_words$word, # remove stop words
         !next_word %in% stop_words$word, # remove stop words
         substr(word, 1, 1) != '@', # remove user handles to protect privacy
         substr(next_word, 1, 1) != '@', # remove user handles to protect privacy
         substr(word, 1, 1) != '#', # remove hashtags
         substr(next_word, 1, 1) != '#',
         str_detect(word, "[a-z]"), # remove words containing ony numbers or symbols
         str_detect(next_word, "[a-z]")) %>% # remove words containing ony numbers or symbols
  filter(status_id == lead(status_id)) %>% # needed to ensure bigrams to cross from one tweet into the next
  unite(bigram, word, next_word, sep = ' ')


bigrams <- tidy_bigrams %>%
  count(bigram, sort=TRUE) %>%
  filter(n >= 30) %>%
  mutate(bigram = reorder(bigram, n))


bigrams <- bigrams %>% 
  head(25)

bigrams$bigram <- iconv(bigrams$bigram, "latin1", "ASCII", sub="")

bigrams$pol <- c(1,0,0,0,0,1,1,0,1,1,0,0,0,0,0,0,1,1,1,1,0,0,1,1,0)


bigrams %>% 
  mutate(pol = as.factor(pol))  %>% 
  ggplot(., aes(x=reorder(bigram, n), y = n, fill = pol)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("azure3", "firebrick3")) +
  coord_flip() +
  geom_text(aes(y = n - 65, label = n), position = position_dodge(width = .9), size = 10, family = "font") +
  theme_gg("Roboto Slab") +
  labs(x = "", y = "Count", title = "Most Frequent Bigrams in Dec. 2018", caption = "Data: Twitter REST API") + 
  ggsave("D://tags_twitter/calf/fifth_bigram.png", width = 7)
