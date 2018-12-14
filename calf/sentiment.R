tidy <- tweets %>% 
  unnest_tokens(word, text)

sent <- tidy %>% 
  anti_join(get_stopwords())


bing <- get_sentiments("bing")

sent <- sent %>% 
  inner_join(bing) %>% 
  group_by(date2) %>% 
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

sent %>% 
  ggplot(., aes(x=date2, y = sentiment)) +
  geom_point() +
  geom_smooth(method = lm, color = "firebrick3") +
  scale_fill_gradient(low = "black", high = "firebrick3") +
  theme_gg("Roboto Slab") +
  labs(x = "Date", y = "Overall Sentiment", title = "Sentiment Scores of Evangelical Tweets", subtitle = "Using the Bing Lexicon", caption = "Data: Twitter REST API") +
  # scale_x_date(labels = date_format("%b-%Y")) +
  ggsave("D://tags_twitter/calf/bing_long.png", width = 10, height = 6)



afinn <- get_sentiments("afinn")

sent <- tidy %>% 
  inner_join(afinn) %>% 
  group_by(date2) %>% 
  count(score) %>% 
  mutate(score = score * n) %>% 
  summarise(sum = sum(score))
  

sent %>% 
  ggplot(., aes(x=date2, y = sum)) +
  geom_point() +
  geom_smooth(method = lm, color = "firebrick3") +
  theme_gg("Roboto Slab") +
  labs(x = "Date", y = "Overall Sentiment", title = "Sentiment Scores of Evangelical Tweets", subtitle = "Using the Afinn Lexicon", caption = "Data: Twitter REST API") +
  ggsave("D://tags_twitter/calf/afinn_long.png", width = 10, height = 6)



