

word <- 'trump|Trump'
trump <- subset(tweets, grepl(word, text))


total <- tweets %>% 
  group_by(date2) %>% 
  count()

trump2 <- trump %>% 
  group_by(date2) %>% 
  count() %>% 
  rename(trump_n = n)

merged <- left_join(total, trump2) %>% 
  mutate(pct = trump_n/n)

merged %>% 
  na.omit() %>% 
  ggplot(., aes(x=date2, y = pct)) +
  geom_point() +
  geom_smooth(method = lm, color = "firebrick3") +
  theme_gg("Roboto Slab") +
  scale_y_continuous(labels = percent) +
  labs(x = "Date", y = "Percent of Tweets Containing Trump", title = "Trump is Becoming More Linked with Evangelicals on Twitter", caption = "Data: Twitter REST API") +
  # scale_x_date(labels = date_format("%b-%Y")) +
  ggsave("D://tags_twitter/calf/trump_long.png", width = 10, height = 6)