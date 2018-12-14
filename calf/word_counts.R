## I need to divide these up to give me words by each block of time. 
## This is February and March of 2017
first <- tweets %>% 
  filter(month == 2 | month == 3 | month == 4) %>% 
  filter(year == 2017) 


tidy_tweets <- first %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

count <- tidy_tweets %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) != '#', # omit hashtags
         substr(word, 1, 1) != '@') %>% # omit Twitter handles
  mutate(word = reorder(word, n))

count <- count[-1,]

count <- count %>% 
  head(25) 

count$pol <- c(0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0)

count %>% 
  mutate(pol = as.factor(pol)) %>% 
  ggplot(., aes(x= word, y = n, fill = pol)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("azure3", "firebrick3")) +
  coord_flip() +
  geom_text(aes(y = n - 85, label = n), position = position_dodge(width = .9), size = 10, family = "font") +
  theme_gg("Roboto Slab") +
  labs(x = "", y = "Count", title = "Most Frequent Words in Feb. + Mar. 2017", caption = "Data: Twitter REST API") + 
  ggsave("D://tags_twitter/calf/first_count.png")

## I need to divide these up to give me words by each block of time. 
## This is December of 2017
first <- tweets %>% 
  filter(month == 12) %>% 
  filter(year == 2017) 


tidy_tweets <- first %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

count <- tidy_tweets %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) != '#', # omit hashtags
         substr(word, 1, 1) != '@') %>% # omit Twitter handles
  mutate(word = reorder(word, n))

count <- count[-1,]

count <- count %>% 
  head(25) 

count$pol <- c(1,0,0,1,0,0,1,0,0,1,1,0,0,0,0,0,0,0,1,0,0,0,1,0,1)

count %>% 
  mutate(pol = as.factor(pol)) %>% 
  ggplot(., aes(x= word, y = n, fill = pol)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("azure3", "firebrick3")) +
  coord_flip() +
  geom_text(aes(y = n - 125, label = n), position = position_dodge(width = .9), size = 10, family = "font") +
  theme_gg("Roboto Slab") +
  labs(x = "", y = "Count", title = "Most Frequent Words in Dec. 2017", caption = "Data: Twitter REST API") + 
  ggsave("D://tags_twitter/calf/second_count.png")



## I need to divide these up to give me words by each block of time. 
## This is April of 2018
first <- tweets %>% 
  filter(month == 4) %>% 
  filter(year == 2018) 


tidy_tweets <- first %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

count <- tidy_tweets %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) != '#', # omit hashtags
         substr(word, 1, 1) != '@') %>% # omit Twitter handles
  mutate(word = reorder(word, n))

count <- count[-1,]

count <- count %>% 
  head(25) 

count$pol <- c(1,1,0,0,0,0,0,0,0,1,0,0,0,1,1,0,1,0,0,0,0,0,0,0,0)

count %>% 
  mutate(pol = as.factor(pol)) %>% 
  ggplot(., aes(x= word, y = n, fill = pol)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("azure3", "firebrick3")) +
  coord_flip() +
  geom_text(aes(y = n - 125, label = n), position = position_dodge(width = .9), size = 10, family = "font") +
  theme_gg("Roboto Slab") +
  labs(x = "", y = "Count", title = "Most Frequent Words in Apr. 2018", caption = "Data: Twitter REST API") + 
  ggsave("D://tags_twitter/calf/third_count.png")





## I need to divide these up to give me words by each block of time. 
## This is April of 2018
first <- tweets %>% 
  filter(month == 8) %>% 
  filter(year == 2018) 


tidy_tweets <- first %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

count <- tidy_tweets %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) != '#', # omit hashtags
         substr(word, 1, 1) != '@') %>% # omit Twitter handles
  mutate(word = reorder(word, n))

count <- count[-1,]

count <- count %>% 
  head(25) 

count$pol <- c(1,0,1,1,1,1,1,1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,0,0)

count %>% 
  mutate(pol = as.factor(pol)) %>% 
  ggplot(., aes(x= word, y = n, fill = pol)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("azure3", "firebrick3")) +
  coord_flip() +
  geom_text(aes(y = n - 155, label = n), position = position_dodge(width = .9), size = 10, family = "font") +
  theme_gg("Roboto Slab") +
  labs(x = "", y = "Count", title = "Most Frequent Words in Aug. 2018", caption = "Data: Twitter REST API") + 
  ggsave("D://tags_twitter/calf/fourth_count.png")




## I need to divide these up to give me words by each block of time. 
## This is April of 2018
first <- tweets %>% 
  filter(month == 12) %>% 
  filter(year == 2018) 


tidy_tweets <- first %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

count <- tidy_tweets %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) != '#', # omit hashtags
         substr(word, 1, 1) != '@') %>% # omit Twitter handles
  mutate(word = reorder(word, n))

count <- count[-1,]

count <- count %>% 
  head(25) 

count$pol <- c(1,0,0,1,0,1,1,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0)

count %>% 
  mutate(pol = as.factor(pol)) %>% 
  ggplot(., aes(x= word, y = n, fill = pol)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("azure3", "firebrick3")) +
  coord_flip() +
  geom_text(aes(y = n - 155, label = n), position = position_dodge(width = .9), size = 10, family = "font") +
  theme_gg("Roboto Slab") +
  labs(x = "", y = "Count", title = "Most Frequent Words in Dec. 2018", caption = "Data: Twitter REST API") + 
  ggsave("D://tags_twitter/calf/fifth_count.png")


