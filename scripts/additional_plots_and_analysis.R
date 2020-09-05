library(conflicted)
library(coin)
library(ggpubr)
library(rstatix)

conflict_prefer("filter", "dplyr")
conflict_prefer("wilcox_test", "rstatix")

# ----additional-testing-is-data-normal----

qqplot_of_age <- ggqqplot(data, x = "age")

sq.ggsave("write_up/qqplot.png") # clearly not a normal distribution

allreg_summary_table_not_normal <- data %>% 
  filter(gender == "male" | gender == "female") %>% 
  group_by(registered, gender) %>% 
  get_summary_stats(age, type = "median_iqr")



subdata <- data %>% filter(gender =="male")

bxp <- subdata %>%  
  ggboxplot(
  x = "registered", y = "age", 
  ylab = "age", xlab ="Registered", add = "Jitter"
)

stat.test <- subdata %>% 
  wilcox_test(age ~ registered) %>% 
  add_significance()

stat.test <- stat.test %>% add_xy_position(x = "registered")
bxp +
 stat_pvalue_manual(stat.test, tip.length = 0) +
    labs(title = "Males", subtitle = get_test_label(stat.test, detailed = TRUE))

male_reg_wilcoxon <- stat.test
male_reg_wilcoxon_bxp <- bxp
saveRDS(male_reg_wilcoxon, "data/male_reg_wilcoxon.RDS")

ggsave("write_up/male_reg_wilcoxon_bxp.png")

subdata <- data %>% filter(gender =="female")

bxp <- subdata %>%  
  ggboxplot(
    x = "registered", y = "age", 
    ylab = "age", xlab ="Registered", add = "Jitter"
  )


stat.test <- subdata %>% 
  wilcox_test(age ~ registered) %>% 
  add_significance()

stat.test <- stat.test %>% add_xy_position(x = "registered")
bxp +
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(title = "Females", subtitle = get_test_label(stat.test, detailed = TRUE))

female_reg_wilcoxon <- stat.test
female_reg_wilcoxon_bxp <- bxp
saveRDS(female_reg_wilcoxon, "data/female_reg_wilcoxon.RDS")
ggsave("write_up/female_reg_wilcoxon_bxp.png")


data.wilcox.effectsize.male <- data %>% 
  filter(gender == "male") %>% 
  wilcox_effsize(age ~ registered)

data.wilcox.effectsize.female <- data %>% 
  filter(gender == "female") %>% 
  wilcox_effsize(age ~ registered)

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

ggsave("write_up/Figure_1_box_and_whisker.png")

allreg_summary_table <- data %>% 
  filter(gender == "male" | gender == "female", age > 17 ) %>%
  group_by(gender, registered) %>% 
  summarize(mean = mean(age), sd = sd(age)) %>% 
  ungroup()

saveRDS(allreg_summary_table, "data/allreg_summary_table.RDS")

