
# ----figure-1-box-and-whisker----


Figure_1_box_and_whisker <-
  data %>% rename(`CIE status` = registered)  %>%  
  filter(gender == "male" | gender == "female", age > 17) %>%
  ggplot(aes(
    `CIE status`,  age,
    group = `CIE status`,
    fill = `CIE status`
  )) +
  #  geom_histogram(aes(y = ..density..), breaks = seq(10, 100, by = 2), col = "grey", fill = "green",  alpha = 0
  geom_boxplot(alpha = 0.2) +
  labs(title = "Box and whisker CIE use and gender") +
  labs(subtitle = "CIE registered compared to not registered out of total Cerner population") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "", y = "Age") +
  theme_tufte() + theme(text = element_text(family = "serif")) +
  theme(legend.title = element_blank()) +
  facet_wrap(~ gender) 

allreg_summary_table <- data %>% 
  filter(gender == "male" | gender == "female", age > 17 ) %>%
  group_by(gender, registered) %>% 
  summarize(mean = mean(age), sd = sd(age)) %>% 
  ungroup()



