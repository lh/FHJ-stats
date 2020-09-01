#  Preparation for publication
#
#  This file removes all the tables constructed in analysis for plots.R
#  So - either run plots first, or not at all. plots.R is
#  designed for exploration of the dataset.
#  To run plots.R after this either reload all the data via scripts, or replace
#  workspace saved after analysis.R was run.
#

##---- libraries
##

list.of.packages <-
  c(
    "gt",
    "glue",
    "webshot",
    "tidyverse",
    "ggthemes",
    "readxl",
    "rmarkdown",
    "skimr",
    "devtools",
    "hexbin",
    "scales"
  )
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)


if (!require(visdat)) {
  devtools::install_github("ropensci/visdat")
}
library(webshot)
library(gt)
library(glue)
library(tidyverse)
library(ggthemes)
library(readxl)
library(skimr)
library(rmarkdown)
library(devtools)
library(visdat)
library(hexbin)
library(scales)

##---- cleanup

# rm(list = ls(pattern = "^t.*_", all.names = TRUE))   # clean the workspace of temporary files

##---- printing-functions
sq.ggsave <-
  function(filename = default_name(plot),
           height = 4,
           width = 4,
           ...) {
    ggsave(filename = filename,
           height = height,
           width = width,
           ...)
  }

tall.ggsave <-
  function(filename = default_name(plot),
           height = 6,
           width = 4,
           ...) {
    ggsave(filename = filename,
           height = height,
           width = width,
           ...)
  }

fat.ggsave <-
  function(filename = default_name(plot),
           height = 4,
           width = 6,
           ...) {
    ggsave(filename = filename,
           height = height,
           width = width,
           ...)
  }

thin.ggsave <-
  function(filename = default_name(plot),
           height = 4,
           width = 2,
           ...) {
    ggsave(filename = filename,
           height = height,
           width = width,
           ...)
  }
##---- stats-functions
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

kurtosis <- function(x) {
  m4 <- mean((x - mean(x)) ^ 4)
  kurtosis <- m4 / (sd(x) ^ 4) - 3
  kurtosis
}

skewness <-  function(x) {
  m3 <- mean((x - mean(x)) ^ 3)
  skewness <- m3 / (sd(x) ^ 3)
  skewness
}

##---- Figure-1

Figure_1 <-
  data %>% rename(`CIE status` = registered)  %>%  
  filter(gender == "male" | gender == "female", age > 17) %>%
  ggplot(aes(
    age,
    group = `CIE status`,
    fill = `CIE status`
  )) +
  #  geom_histogram(aes(y = ..density..), breaks = seq(10, 100, by = 2), col = "grey", fill = "green",  alpha = 0
  geom_density(alpha = 0.2) +
  labs(title = "Figure 1: Age distribution by CIE use and gender") +
  labs(subtitle = "CIE registered compared to not registered out of total Cerner population") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Age", y = "Density") +
    theme_tufte() + theme(text = element_text(family = "serif")) +
  theme(legend.title = element_blank()) +
  facet_wrap(~ gender) 

##---- Figure-1-data

Figure_1_data <-
  data %>% rename(`CIE status` = registered)  %>%  filter(gender == "male" |
                                                            gender == "female", age > 17) %>%
  group_by(`CIE status`, gender)    %>%
  summarise(
    mean = mean(age),
    median = median(age),
    mode = mode(age),
    IQR = IQR(age),
    skewness = skewness(age),
    kurtosis = kurtosis(age)
  )


##---- Figure-1-boxplot

Figure_1_boxplot <-
  #  data %>% rename(`CIE status` = registered)  %>%  filter(gender == "male" | gender == "female", age > 17)
  data %>% rename(`CIE status` = registered)  %>%  filter(gender == "male" |
                                                            gender == "female", age > 17) %>%
  ggplot(aes(
    age,
    group = `CIE status`,
    fill = `CIE status`,
    linetype = `CIE status`
  )) +
  #  geom_histogram(aes(y = ..density..), breaks = seq(10, 100, by = 2), col = "grey", fill = "green",  alpha = 0
  geom_boxplot(alpha = 0.2) +
  labs(title = "Age distribution by CIE use and gender") +
  labs(subtitle = "CIE registered compared to not registered out of total Cerner population") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Age", y = "") +
    theme_tufte() + theme(text = element_text(family = "serif")) +
  facet_wrap(~ gender) # +  fat.ggsave("figures/Figure_1_data.png")

# Figure_1_boxplot

Figure_1_hist <-
  data %>% rename(`CIE status` = registered)  %>%  filter(gender == "male" |
                                                            gender == "female", age > 17) %>%
  ggplot(aes(
    age,
    group = `CIE status`,
    fill = `CIE status`,
    linetype = `CIE status`
  )) +
  #  geom_histogram(aes(y = ..density..), breaks = seq(10, 100, by = 2), col = "grey", fill = "green",  alpha = 0
  geom_histogram(alpha = 0.3, binwidth = 10) +
  labs(title = "Age distribution by CIE use and gender") +
  labs(subtitle = "CIE registered compared to not registered out of total Cerner population") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Age", y = "Number") +
    theme_tufte() + theme(text = element_text(family = "serif")) +
  
  facet_wrap(~ gender)


# Figure_1_hist

binwidth <-  10


Figure_1_data_hist_labeled_female <-
  data %>%
  rename(`CIE status` = registered)  %>%
  filter(gender == "female", age > 17) %>%
  
  ggplot(aes(
    x = age,
    group = `CIE status`,
    fill = `CIE status`,
    linetype = `CIE status`
  )) +
  geom_histogram(alpha = 0.3, aes(y = (..count..) / sum(..count..)), binwidth = binwidth) +
  
  stat_bin(
    aes(
      y = (..count..) / sum(..count..),
      label = paste0(round((..count..) / sum(..count..) * 100, 1), "%")
    ),
    geom = "text",
    size = 4,
    binwidth = 10,
    vjust = -1.5
  )  # + facet_wrap( ~ gender)

# Figure_1_data_hist_labelled_female

Figure_1_data_hist_labelled_male <-
  data %>%
  rename(`CIE status` = registered)  %>%
  filter(gender == "male" , age > 17) %>%
  
  
  ggplot(aes(
    x = age,
    group = `CIE status`,
    fill = `CIE status`,
    linetype = `CIE status`
  )) +
  geom_histogram(alpha = 0.3, aes(y = (..count..) / sum(..count..)), binwidth = binwidth) +
  
  stat_bin(
    aes(
      y = (..count..) / sum(..count..),
      label = paste0(round((..count..) / sum(..count..) * 100, 1), "%")
    ),
    geom = "text",
    size = 4,
    binwidth = 10,
    vjust = -1.5
  )  # + facet_wrap( ~ gender)

# Figure_1_data_hist_labelled_male

################################################
#                                              #
#           Figure 2                           #
#                                              #
################################################





Figure_2 <-
  data %>%
  count(`EthnicCat_r`, registered) %>%
  group_by(registered, `EthnicCat_r`)  %>%
  summarize(Sum = sum(n)) %>%
  spread(key = registered, value = Sum) %>%
  mutate(Registered_Portion = Registered / (Registered + Unregistered) * 100) %>%
  
  ggplot(aes(
    x = reorder(`EthnicCat_r`,-Registered_Portion),
    y = Registered_Portion,
    fill = EthnicCat_r
  ), ) +
  
  geom_bar(stat = "identity", color = "grey21") +
  scale_fill_brewer(palette = "RdBu") +
  coord_flip() +
  labs(title = "Figure 2: CIE registration liklihood by ethnicity",
       subtitle = "Ethnicity recorded by Cerner") +
  ylab("%") +
  xlab("") +
    theme_tufte() + theme(text = element_text(family = "serif")) +
  theme(legend.position = "none") #+ fat.ggsave("figures/Figure_2a.png")

# Figure_2a


temp.1 <- data %>%
  count(`EthnicCat_r`, registered) %>%
  group_by(registered, `EthnicCat_r`)  %>%
  summarize(Sum = sum(n)) %>%
  spread(key = registered, value = Sum) %>%
  mutate(Registered_Portion = Registered / (Registered + Unregistered) * 100)


temp.2  <- data %>%
  count(`EthnicCat_r`, engaged) %>%
  group_by(engaged, `EthnicCat_r`)  %>%
  summarize(Sum = sum(n)) %>%
  spread(key = engaged, value = Sum) %>%
  mutate(Engaged_Portion = Engaged_with_CIE / (Engaged_with_CIE + Not_engaged_with_CIE) * 100)

ethnicity_reg_and_engaged <-
  left_join(temp.1, temp.2) %>%  mutate(portion_of_CIE_engaged = Engaged_with_CIE /
                                          Registered * 100)

rm(temp.1, temp.2)



Figure_2b <-
  ethnicity_reg_and_engaged  %>%
  ggplot(aes(
    x = reorder(`EthnicCat_r`, -portion_of_CIE_engaged),
    y = portion_of_CIE_engaged,
    fill = EthnicCat_r
  ),) +
  geom_bar(stat = "identity", color = "grey21") +
  scale_fill_brewer(palette = "RdBu") +
  coord_flip() +
  labs(title = "Proportion of CIE who engaged with CIE, by Ethnicity",
       subtitle = "Ethnicity as recorded on Cerner") +
  ylab("%") +
  xlab("") +
    theme_tufte() + theme(text = element_text(family = "serif")) +
  theme(legend.position = "none") #+ fat.ggsave("figures/Figure_2b.png")

# Figure_2b

#+ fat.ggsave("figures/Figure_2c.png")

# Figure_2c


##------------Fig 2 data---------



engaged_percentages_by_race <- data %>%
  count(`EthnicCat_r`, engaged) %>%
  group_by(engaged, `EthnicCat_r`) %>%
  summarize(Sum = sum(n)) %>%
  spread(key = engaged, value = Sum) %>%
  select(EthnicCat_r, Engaged_with_CIE) %>%
  pivot_wider(names_from = c(EthnicCat_r),
              values_from = Engaged_with_CIE) %>%
  mutate(total = `Asian or asian british` + `Black or black british` + Mixed + `Other ethnic groups` + Unspecified + White + `NA`) %>%
  mutate(
    `Asian or asian british` = `Asian or asian british` * 100 / total,
    `Black or black british` = `Black or black british` *
      100 / total,
    `Mixed` = Mixed * 100 / total,
    `Other ethnic groups` = `Other ethnic groups` * 100 / total,
    `Unspecified` = Unspecified * 100 / total,
    `White` = White * 100 / total,
    `NA` = `NA` * 100 / total,
  ) %>%
  pivot_longer(
    cols = c(
      `Asian or asian british` ,
      `Black or black british`,
      `Mixed`,
      `Other ethnic groups` ,
      `Unspecified`,
      `White`,
      `NA`
    ),
    names_to = "Race",
    values_to = "percentage"
  ) %>%
  select(Race, percentage)
# engaged_percentages_by_race



registered_percentages_by_race <- data %>%
  count(`EthnicCat_r`, registered) %>%
  group_by(registered, `EthnicCat_r`)  %>%
  summarize(Sum = sum(n)) %>%
  spread(key = registered, value = Sum) %>%
  select(EthnicCat_r, Registered) %>%
  pivot_wider(names_from = c(EthnicCat_r),
              values_from = Registered)  %>%
  mutate(total = `Asian or asian british` + `Black or black british` + Mixed + `Other ethnic groups` + Unspecified + White + `NA`) %>%
  mutate(
    `Asian or asian british` = `Asian or asian british` * 100 / total,
    `Black or black british` = `Black or black british` *
      100 / total,
    `Mixed` = Mixed * 100 / total,
    `Other ethnic groups` = `Other ethnic groups` * 100 / total,
    `Unspecified` = Unspecified * 100 / total,
    `White` = White * 100 / total,
    `NA` = `NA` * 100 / total,
  ) %>%
  pivot_longer(
    cols = c(
      `Asian or asian british` ,
      `Black or black british`,
      `Mixed`,
      `Other ethnic groups` ,
      `Unspecified`,
      `White`,
      `NA`
    ),
    names_to = "Race",
    values_to = "percentage"
  ) %>%
  select(Race, percentage)

# registered_percentages_by_race

registration_by_ethnicity_scaled_to_white_100 <-  data %>%
  count(`EthnicCat_r`, registered) %>%
  group_by(registered, `EthnicCat_r`)  %>%
  summarize(Sum = sum(n)) %>%
  spread(key = registered, value = Sum) %>%
  mutate(Registered_Portion = Registered / (Registered + Unregistered) * 100) %>% select(EthnicCat_r, Registered_Portion) %>%
  pivot_wider(names_from = EthnicCat_r,
              values_from = Registered_Portion) %>%
  mutate(across(everything()), . / White * 100) %>%
  pivot_longer(-White, names_to = "Ethnicity", values_to = "Percentage registered compared to white = 100") %>%
  select(-White)




################################################
#                                              #
#           Figure 3a, 3b, 3c                  #
#                                              #
################################################


##---- Figure-3-a
Figure_3a_registered_and_engaged <-
  left_join(
    data %>%
      count(`Index of Multiple Deprivation Decile`, registered) %>%
      group_by(registered, `Index of Multiple Deprivation Decile`) %>%
      summarize(Sum = sum(n)) %>%
      spread(key = registered, value = Sum) %>%
      mutate(Registered_Portion = Registered / (Registered + Unregistered) * 100),
    
    data %>% count(`Index of Multiple Deprivation Decile`, engaged) %>%
      group_by(engaged, `Index of Multiple Deprivation Decile`) %>%
      summarize(Sum = sum(n)) %>%
      spread(key = engaged, value = Sum) %>%
      mutate(
        Engaged_Portion = Engaged_with_CIE / (Engaged_with_CIE + Not_engaged_with_CIE) * 100
      )
  ) %>%
  select(IMD_Decile = `Index of Multiple Deprivation Decile`,
         Registered = Registered_Portion,
         Engaged = Engaged_Portion) %>%
  pivot_longer(-IMD_Decile, names_to = "status", values_to = "Percent")  %>%
  
  ggplot(aes(fill = status, y = Percent, x = IMD_Decile)) +
  geom_bar(stat = "identity",
           position = "identity",
           alpha = 1) +
  scale_x_discrete(limits = factor(1:10)) +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.title = element_blank()) +
  labs(
    title = 
"Figure 3a: Percentage of Cerner patients registered with CIE, 
showing those also engaged with CIE, by Index of Multiple Deprivation (IMD)
estimated by postcode sector",
    subtitle = "Lower is more deprived"
  ) +
  ylab("%") +
  xlab("IMD Decile") +
    theme_tufte() + theme(text = element_text(family = "serif")) +
  theme(legend.title = element_blank()) +
  theme(axis.title.y = element_text(angle = 0))




##---- Figure-3-b
Figure_3b_registered_and_engaged <-
  
  left_join(
    data %>% count(`Health and Disability Decile`, registered) %>%
      group_by(registered, `Health and Disability Decile`) %>%
      summarize(Sum = sum(n)) %>%
      spread(key = registered, value = Sum) %>%
      mutate(Registered_Portion = Registered / (Registered + Unregistered) * 100),
    
    data %>% count(`Health and Disability Decile`, engaged) %>%
      group_by(engaged, `Health and Disability Decile`) %>%
      summarize(Sum = sum(n)) %>%
      spread(key = engaged, value = Sum) %>%
      mutate(
        Engaged_Portion = Engaged_with_CIE / (Engaged_with_CIE + Not_engaged_with_CIE) * 100
      )
  ) %>%
  
  select(IMD_Decile = `Health and Disability Decile`,
         Registered = Registered_Portion,
         Engaged = Engaged_Portion) %>%
  pivot_longer(-IMD_Decile, names_to = "status", values_to = "Percent")  %>%
  
  ggplot(aes(fill = status, y = Percent, x = IMD_Decile)) +
  geom_bar(stat = "identity",
           position = "identity",
           alpha = 1) +
  scale_x_discrete(limits = factor(1:10)) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title =
"Figure 3b: Percentage of Cerner patients registered with CIE, 
showing those also engaged with CIE, by Health and Disability Decile 
estimated by postcode sector",
    subtitle = "Lower is less healthy"
  ) +
  ylab("%") +
  xlab("Health and Disability Decile") +
    theme_tufte() + theme(text = element_text(family = "serif")) +
  theme(legend.title = element_blank()) +
  theme(axis.title.y = element_text(angle = 0))



##---- Figure-3-c
Figure_3c_registered_and_engaged <-
  
  left_join(
    data %>% count(`Income Decile`, registered) %>%
      group_by(registered, `Income Decile`) %>%
      summarize(Sum = sum(n)) %>%
      spread(key = registered, value = Sum) %>%
      mutate(Registered_Portion =
               Registered /
               (Registered + Unregistered) * 100),
    
    data %>% count(`Income Decile`, engaged) %>%
      group_by(engaged, `Income Decile`) %>%
      summarize(Sum = sum(n)) %>%
      spread(key = engaged, value = Sum) %>%
      mutate(
        Engaged_Portion =
          Engaged_with_CIE /
          (Engaged_with_CIE + Not_engaged_with_CIE) * 100
      )
  ) %>%
  
  select(`Income Decile`, Registered = Registered_Portion, Engaged = Engaged_Portion) %>%
  pivot_longer(-`Income Decile`, names_to = "status", values_to = "Percent")  %>%
  
  ggplot(aes(fill = status, y = Percent, x = `Income Decile`)) +
  geom_bar(stat = "identity",
           position = "identity",
           alpha = 1) +
  scale_x_discrete(limits = factor(1:10)) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title =
"Figure 3c: Percentage of Cerner patients registered with CIE, 
showing those also engaged with CIE, by Income Decile estimated by 
postcode sector",
    subtitle = "Lower is less wealthy"
  ) +
  ylab("%") +
  xlab("Income Decile") +
    theme_tufte() + theme(text = element_text(family = "serif")) +
  theme(legend.title = element_blank()) +
  theme(axis.title.y = element_text(angle = 0))

##---- Table-3-a
`Index of Multiple Deprivation Decile Differences` <-
  left_join(
    data %>%
      count(`Index of Multiple Deprivation Decile`, registered) %>%
      group_by(registered, `Index of Multiple Deprivation Decile`) %>%
      summarize(Sum = sum(n)) %>%
      spread(key = registered, value = Sum) %>%
      mutate(Registered_Portion = Registered / (Registered + Unregistered) * 100),
    
    data %>% count(`Index of Multiple Deprivation Decile`, engaged) %>%
      group_by(engaged, `Index of Multiple Deprivation Decile`) %>%
      summarize(Sum = sum(n)) %>%
      spread(key = engaged, value = Sum) %>%
      mutate(
        Engaged_Portion = Engaged_with_CIE / (Engaged_with_CIE + Not_engaged_with_CIE) * 100
      )
  ) %>%
  select(IMD_Decile = `Index of Multiple Deprivation Decile`,
         Registered = Registered_Portion,
         Engaged = Engaged_Portion) %>%
  pivot_longer(-IMD_Decile, names_to = "status", values_to = "Percent") %>%
  filter(IMD_Decile == 1 | IMD_Decile == 10) %>%
  mutate(IMD_Decile = as.character(IMD_Decile)) %>%
  pivot_wider(names_from = "IMD_Decile", values_from = "Percent") %>%
  rename(`Lowest Decile` = `1`, `Highest Decile` = `10`) %>%
  mutate("Liklihood" = `Highest Decile` / `Lowest Decile`)

##---- Table-3-b
`Health and Disability Decile Differences` <-
  left_join(
    data %>% count(`Health and Disability Decile`, registered) %>%
      group_by(registered, `Health and Disability Decile`) %>%
      summarize(Sum = sum(n)) %>%
      spread(key = registered, value = Sum) %>%
      mutate(Registered_Portion = Registered / (Registered + Unregistered) * 100),
    
    data %>% count(`Health and Disability Decile`, engaged) %>%
      group_by(engaged, `Health and Disability Decile`) %>%
      summarize(Sum = sum(n)) %>%
      spread(key = engaged, value = Sum) %>%
      mutate(
        Engaged_Portion = Engaged_with_CIE / (Engaged_with_CIE + Not_engaged_with_CIE) * 100
      )
  ) %>%
  
  select(
    `Health and Disability Decile Differences` = `Health and Disability Decile`,
    Registered = Registered_Portion,
    Engaged = Engaged_Portion
  ) %>%
  pivot_longer(
    -`Health and Disability Decile Differences`,
    names_to = "status",
    values_to = "Percent"
  )  %>%
  filter(
    `Health and Disability Decile Differences` == 1 |
      `Health and Disability Decile Differences` == 10
  ) %>%
  mutate(
    `Health and Disability Decile Differences` = as.character(`Health and Disability Decile Differences`)
  ) %>%
  pivot_wider(names_from = `Health and Disability Decile Differences`, values_from = "Percent") %>%
  rename(`Lowest Decile` = `1`, `Highest Decile` = `10`) %>%
  mutate("Liklihood" = `Highest Decile` / `Lowest Decile`)

##---- Table-3-c
`Income Decile Differences` <-
  left_join(
    data %>% count(`Income Decile`, registered) %>%
      group_by(registered, `Income Decile`) %>%
      summarize(Sum = sum(n)) %>%
      spread(key = registered, value = Sum) %>%
      mutate(Registered_Portion =
               Registered /
               (Registered + Unregistered) * 100),
    
    data %>% count(`Income Decile`, engaged) %>%
      group_by(engaged, `Income Decile`) %>%
      summarize(Sum = sum(n)) %>%
      spread(key = engaged, value = Sum) %>%
      mutate(
        Engaged_Portion =
          Engaged_with_CIE /
          (Engaged_with_CIE + Not_engaged_with_CIE) * 100
      )
  ) %>%
  
  select(`Income Decile`, Registered = Registered_Portion, Engaged = Engaged_Portion) %>%
  pivot_longer(-`Income Decile`, names_to = "status", values_to = "Percent")  %>%
  filter(`Income Decile` == 1 | `Income Decile` == 10) %>%
  mutate(`Income Decile` = as.character(`Income Decile`)) %>%
  pivot_wider(names_from = `Income Decile`, values_from = "Percent") %>%
  rename(`Lowest Decile` = `1`, `Highest Decile` = `10`) %>%
  mutate("Liklihood" = `Highest Decile` / `Lowest Decile`)

################################################
#                                              #
#           Languages                          #
#                                              #
################################################


################################################
#                                              #
#           Figure 4a                          #
#                                              #
################################################

# Top 30 languages, sorted in order, with languages on y axis including english
All_lang_graph_data <- data %>%
  count(language) %>%
  filter(!is.na(language)) %>% 
  arrange(desc(n)) 

CIE_lang_graph_data <- data %>%
  filter(registered == "Registered") %>%
  count(language) %>%
  filter(!is.na(language)) %>% 
  arrange(desc(n)) 


CIE_lang_graph_data <- left_join(CIE_lang_graph_data, manual_language_groups)  
All_lang_graph_data <- left_join(All_lang_graph_data, manual_language_groups)  









Figure_4a <-  All_lang_graph_data %>%  
  slice_head(n = 30) %>% 
  ggplot(aes(x = n, y = reorder(language, -n), fill = Language_Group )) + 
  geom_col(alpha = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(
    trans = 'log10',
    breaks = c(10, 100, 1000, 10000, 100000),
    labels = comma
  ) +
  theme(legend.title = element_blank()) +
  theme_tufte() + theme(text = element_text(family = "serif")) +
  labs(
    title =
      "Figure 4a: Languages recorded for all patients",
    subtitle = "Note logarithmic scale",
    y = NULL,
    x = expression("The number of people recording each language")
  )  

temp.Figure_4a_data <- data %>%
  count(language) %>%
  filter(!is.na(language)) %>%
  mutate("language_count" = n) %>%
  select(-n)

Figure_4a_count_of_languages_defined  <-
  temp.Figure_4a_data %>%
  summarize(Sum = sum(language_count))


Figure_4a_data  <-
  temp.Figure_4a_data %>%
  mutate(
    "language_percent" = (
      language_count / Figure_4a_count_of_languages_defined$Sum * 100
    )
  ) %>%
  arrange(desc(language_count)) %>%
  slice_head(n = 5)

rm(temp.Figure_4a_data)




################################################
#                                              #
#           Figure 4b                          #
#                                              #
################################################
Figure_4b <-CIE_lang_graph_data %>%  
  slice_head(n = 30) %>% 
  ggplot(aes(x = n, y = reorder(language, -n), fill = Language_Group )) + 
  geom_col(alpha = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(
    trans = 'log10',
    breaks = c(10, 100, 1000, 10000, 100000),
    labels = comma
  ) +
  theme_tufte() + theme(text = element_text(family = "serif")) +
  theme(legend.title = element_blank()) +
  labs(
    title =
      "Figure 4b: Languages recorded for patients registered
with CIE",
    subtitle = "Note logarithmic scale",
    y = NULL,
    x = expression("The number of people recording each language")
  )

temp.Figure_4b_data <- data %>%
  filter(registered == "Registered") %>%
  count(language) %>%
  filter(!is.na(language)) %>%
  mutate("language_count" = n) %>%
  select(-n)

Figure_4b_count_of_languages_defined  <-
  temp.Figure_4b_data %>%
  summarize(Sum = sum(language_count))


Figure_4b_data  <-
  temp.Figure_4b_data %>%
  mutate(
    "language_percent" = (
      language_count / Figure_4b_count_of_languages_defined$Sum * 100
    )
  ) %>%
  arrange(desc(language_count)) %>%
  slice_head(n = 5)

CIE_lang_graph_data <- 
  CIE_lang_graph_data %>% 
  rename(CIE = n)

All_lang_graph_data <-  
  All_lang_graph_data %>% 
  rename(All = n)

Combined_lang_graph_data <- left_join(All_lang_graph_data, CIE_lang_graph_data)

Combined_lang_graph_data <-     Combined_lang_graph_data %>% mutate(recruited = CIE/All * 966341/55452)

Figure_4c <- 
  Combined_lang_graph_data %>%  
  arrange((recruited)) %>% 
  filter(All > 25) %>% 
  #     slice_head(n = 30) %>% 
  ggplot(aes(x = recruited, y = reorder(language, -recruited), fill = Language_Group )) + 
  geom_col(alpha = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(
    trans = 'log10',
    breaks = c( .5, 1, 2, 5, 10),
    labels = c( 0.5, 1, 2, 5, 10)
  ) +
  geom_vline(
    aes(xintercept = 1.0),
    size = .2,
    show.legend = FALSE
  ) +
  theme_tufte() + theme(text = element_text(family = "serif")) +
  labs(
    title = "Figure 4c: The odds of being registered with CIE,\nby recorded language",
    subtitle = "Log scale: languages to the left are less often recruited than English",
    y = NULL,
    x = " "
  )  +
  theme(legend.title = element_blank())




################################################
#                                              #
#           Figure 5                           #
#                                              #
################################################

Figure_5 <-   data %>%
  group_by(registered) %>%
  summarise_at(
    vars(
      English_Languages,
      European_Languages,
      Arabic_Languages,
      `West/Central_Asian_Languages`,
      South_Asian_Languages,
      East_Asian_Languages,
      African_Languages,
      Other_Languages
    ),
    list(~ sum(., na.rm = TRUE))
  )  %>%
  rename(
    English = English_Languages,
    European = European_Languages,
    Arabic = Arabic_Languages,
    `West/Central Asian` =  `West/Central_Asian_Languages`,
    `South Asian` = South_Asian_Languages,
    `East Asian` =  East_Asian_Languages,
    `African` = African_Languages,
    Other  = Other_Languages
  ) %>%
  pivot_longer(-registered, names_to = "Language_Group", values_to = "Sum") %>%
  spread(key = registered, value = Sum)  %>%  
  mutate(Language_Group = as.factor(Language_Group))

levels(Figure_5$Language_Group) <- levels(CIE_lang_graph_data$Language_Group)

Figure_5 <- Figure_5 %>%
  mutate(Portion_Registering = Registered / (Registered + Unregistered) * 100) %>%
  ggplot(aes(
    x = reorder(`Language_Group`, -Portion_Registering),
    y = Portion_Registering, 
    fill = Language_Group
  )) +
  theme_tufte() + theme(text = element_text(family = "serif")) +
  theme(legend.title = element_blank()) +
  geom_bar(stat = "identity", alpha = 0.7 ) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(name = "%", limits = c(0, 8)) +
  
  labs(
    title =
      "Figure 5: Estimate of the percentage of Cerner patients
registered with the CIE by language group",
    subtitle =
      "Language preference estimate from
2011 census household language by postcode sector",
    x = NULL
  ) +
  coord_flip()  # + fat.ggsave("figures/Figure_5_data.png")




################################################
#                                              #
#           Figure 6a, 6b, 6c                  #
#                                              #
################################################

##----Figure-6-a

Figure_6a <-  data %>%
  filter(registered == "Registered")  %>%
  count(`EthnicCat_r`, engaged) %>%
  group_by(engaged, `EthnicCat_r`)  %>%
  summarize(Sum = sum(n)) %>%
  spread(key = engaged, value = Sum) %>%
  mutate(Engaged_Portion = Engaged_with_CIE / (Engaged_with_CIE + Not_engaged_with_CIE) * 100) %>%
  ggplot(aes(
    x = reorder(`EthnicCat_r`, -Engaged_Portion),
    y = Engaged_Portion,
    fill = EthnicCat_r
  ),) +
  
  geom_bar(stat = "identity", color = "grey21") +
  scale_fill_brewer(palette = "RdBu") +
  labs(title = "Figure 6a: Portion of CIE engaged, by ethnicity",
       subtitle = "Ethnicity as recorded on Cerner") +
  labs(x = NULL, y = "%") +
  
  coord_flip() +
    theme_tufte() + theme(text = element_text(family = "serif")) +
  theme(legend.position = "none") 

## ----Figure-6-b----

Figure_6b <-
  
  data %>%
  count(`EthnicCat_r`, engaged) %>%
  group_by(engaged, `EthnicCat_r`)  %>%
  summarize(Sum = sum(n)) %>%
  spread(key = engaged, value = Sum) %>%
  mutate(Engaged_Portion = Engaged_with_CIE / (Engaged_with_CIE + Not_engaged_with_CIE) * 100)      %>%
  
  ggplot(aes(
    x = reorder(`EthnicCat_r`, -Engaged_Portion),
    y = Engaged_Portion,
    fill = EthnicCat_r
  ),) +
  
  geom_bar(stat = "identity", color = "grey21") +
  scale_fill_brewer(palette = "RdBu") +
  coord_flip() +
  labs(title = 
"Figure 6b: Proportion of Cerner who engaged with CIE,
by Ethnicity",
       subtitle = "Ethnicity as recorded on Cerner") +
  ylab("%") +
  xlab("") +
    theme_tufte() + theme(text = element_text(family = "serif")) +
  theme(legend.position = "none") 
##---- Figure-6-c

Figure_6c <-
  data %>% filter(gender == "male" | gender == "female") %>%
  filter(age > 17, registered == "Registered") %>%
  ggplot(aes(age, group = engaged, fill = engaged)) +
  geom_density(alpha = 0.2) +
  labs(title = "Figure 6c: Age distribution of CIE engagement") +
  labs(subtitle = "CIE engaged compared to CIE registered but not engaged") +
  labs(x = "Age", y = "Density") +
    theme_tufte() + theme(text = element_text(family = "serif")) +
  guides(fill = guide_legend(title = NULL)) +
  scale_fill_discrete(
    name = "",
    breaks = c("Engaged_with_CIE", "Not_engaged_with_CIE"),
    labels = c("Engaged with CIE", "Not engaged")
  ) +
  facet_wrap(~ gender) # + fat.ggsave("figures/Figure_6b_data.png")


# Portion of registered answering the questionnaire, by EthnicCat_r

##----Figure-6-c

Figure_6d <-  data %>%
  filter(registered == "Registered")  %>%
  count(EthnicCat_r, did_answer_questionnaire) %>%
  group_by(did_answer_questionnaire, `EthnicCat_r`)  %>%
  summarize(Sum = sum(n)) %>%
  spread(key = did_answer_questionnaire, value = Sum) %>%
  mutate(
    Answering_Questionnaire = `Answered questionnaire` / (`Answered questionnaire` + `Didn't answer questionnaire`) * 100
  ) %>%
  ggplot(aes(
    x = reorder(`EthnicCat_r`, -Answering_Questionnaire),
    y = Answering_Questionnaire,
    fill = EthnicCat_r
  ),) +
  
  geom_bar(stat = "identity", color = "grey21") +
  scale_fill_brewer(palette = "RdBu") +
  labs(title = 
"Figure 6d: Portion of CIE answering questionnaires, by ethnicity",
       subtitle = "Ethnicity as recorded on Cerner") +
  labs(x = NULL, y = "%") +
  
  coord_flip() +
    theme_tufte() + theme(text = element_text(family = "serif")) +
  theme(legend.position = "none") # + fat.ggsave("figures/Figure_6c_data.png")

##----Figure-6-a-data

Figure_6a_data <- data %>%
  filter(registered == "Registered")  %>%
  count(`EthnicCat_r`, engaged) %>%
  group_by(engaged, `EthnicCat_r`)  %>%
  summarize(Sum = sum(n)) %>%
  spread(key = engaged, value = Sum) %>%
  mutate(Engaged_Portion =
           Engaged_with_CIE /
           (Engaged_with_CIE + Not_engaged_with_CIE) * 100) %>%
  select(EthnicCat_r, Engaged_Portion) %>%
  pivot_wider(names_from = EthnicCat_r, values_from = Engaged_Portion) %>%
  mutate(across(everything()), . / White * 100) %>%
  pivot_longer(-White, names_to = "Ethnicity", values_to = "Percentage engaged compared to white = 100") %>%
  select(-White)

## ----Figure-6-b-data----

Figure_6b_data  <-
  data   %>%
  filter(gender == "male" |
           gender == "female",
         age > 17,
         registered == "Registered") %>%
  group_by(engaged, gender)    %>%
  summarise(
    mean = mean(age),
    median = median(age),
    mode = mode(age),
    IQR = IQR(age),
    skewness = skewness(age),
    kurtosis = kurtosis(age)
  )

##----Figure-6-c-data

portion_of_CIE_answering_q_by_ethnicity_scaled_to_white_100 <-
  data %>%
  filter(registered == "Registered")  %>%
  count(`EthnicCat_r`, did_answer_questionnaire) %>%
  group_by(did_answer_questionnaire, `EthnicCat_r`)  %>%
  summarize(Sum = sum(n)) %>%
  spread(key = did_answer_questionnaire, value = Sum) %>%
  mutate(
    Answering_Questionnaire = `Answered questionnaire` / (`Answered questionnaire` + `Didn't answer questionnaire`) * 100
  ) %>%
  select(EthnicCat_r, Answering_Questionnaire) %>%
  pivot_wider(names_from = EthnicCat_r,
              values_from = Answering_Questionnaire) %>%
  mutate(across(everything()), . / White * 100) %>%
  pivot_longer(-White, names_to = "Ethnicity", values_to = "Percentage did questionnaire if white = 100") %>%
  select(-White)


################################################
#                                              #
#           Figure 7                           #
#                                              #
################################################


mu <- data %>%
  filter(gender == "male" | gender == "female") %>%
  filter(registered == "Registered") %>%
  filter(age > 17) %>%
  group_by(`Is ILD patient`) %>%
  summarize(median = median(age))

Figure_7 <-
  data %>% 
  filter(registered == "Registered") %>%
  filter(age > 17) %>%
  ggplot(aes(age, group = `Is ILD patient`, fill = `Is ILD patient`)) +
  geom_density(alpha = 0.2) +
  labs(title = "Figure 7a: Age distribution of ILD patients") +
  labs(subtitle = "CIE registered with ILD compared to non ILD patients") +
  labs(x = "Age", y = "Density") + # commented out the mean line as not normal dist.
# geom_vline(
#   data = mu,
#   aes(xintercept = median,
#       color = `Is ILD patient`),
#   size = 1,
#   show.legend = FALSE
# ) +
# geom_text(
#   data = mu,
#   aes(
#     x = median,
#     label = str_c(round(median, digits = 1), "    ", sep = " ", collapse = NULL)
#   ),
#   y = 0,
#   angle = 0,
#   vjust  = 1.8,
#   hjust = 0.7,
#   size = 08 * 0.352777778,
#   family = "serif"
# ) +
  theme_tufte() + theme(text = element_text(family = "serif")) +
  #   guides(fill=guide_legend(title=NULL)) +
  scale_fill_discrete(
    name = "",
    breaks = c("Known_ILD_Patient", "Not_a_known_ILD_Patient"),
    labels = c("Known ILD", "Not known ILD")
  ) 


Figure_7_by_gender <-
  
  data %>% filter(gender == "male" | gender == "female") %>%
  filter(registered == "Registered") %>%
  filter(age > 17) %>%
  ggplot(aes(
    age,
    group = `Is ILD patient`,
    fill = `Is ILD patient`,
  )) +
  
  geom_density(alpha = 0.2) +
  labs(title = "Figure 7a: Age distribution of ILD patients by gender") +
  labs(subtitle = "CIE registered with ILD compared to non ILD patients") +
  labs(x = "Age", y = "Density") + 
    theme_tufte() + theme(text = element_text(family = "serif")) +
  scale_fill_discrete(
    name = "",
    breaks = c("Known_ILD_Patient", "Not_a_known_ILD_Patient"),
    labels = c("Known ILD", "Not known ILD")
  ) +
  facet_wrap(~ gender)





##----Figure-7-b-data

Figure_7b_data <-
  data %>%   filter(gender == "male" | gender == "female",
                    age > 17,
                    registered == "Registered") %>%
  group_by(`Is ILD patient`, gender)    %>%
  summarise(
    mean = mean(age),
    median = median(age),
    mode = mode(age),
    IQR = IQR(age),
    skewness = skewness(age),
    kurtosis = kurtosis(age)
  )
##----Figure-7-b-data-no-gender

Figure_7b_data_no_gender <-
  data %>%   filter(
                    age > 17,
                    registered == "Registered") %>%
  group_by(`Is ILD patient`)    %>%
  summarise(
    mean = mean(age),
    median = median(age),
    mode = mode(age),
    IQR = IQR(age),
    skewness = skewness(age),
    kurtosis = kurtosis(age)
  )

##----Figure-8


Figure_8 <-
  
  data %>%
  filter(age > 17, registered == "Registered") %>%
  
  count(`EthnicCat_r`, `Is ILD patient`) %>%
  group_by(`Is ILD patient`, `EthnicCat_r`)  %>%
  summarize(Sum = sum(n)) %>%
  spread(key = `Is ILD patient`, value = Sum) %>%
  mutate(ILD_Portion =
           Known_ILD_Patient /
           (Known_ILD_Patient +
              Not_a_known_ILD_Patient) * 100) %>%
  filter(!is.na(ILD_Portion)) %>%
  ggplot(aes(
    x = reorder(`EthnicCat_r`,-ILD_Portion),
    y = ILD_Portion,
    fill = EthnicCat_r
  ), ) +
  
  geom_bar(stat = "identity", color = "grey21") +
  scale_fill_brewer(palette = "RdBu") +
  coord_flip() +
  labs(title = "Figure 7b: Identification as ILD case by ethnicity",
       subtitle = "Ethnicity recorded by Cerner") +
  ylab("%") +
  xlab("") +
    theme_tufte() + theme(text = element_text(family = "serif")) +
  theme(legend.position = "none")

##----Figure-8-data

Figure_8_data <-
  data %>%
  filter(age > 17, registered == "Registered") %>%
  
  count(`EthnicCat_r`, `Is ILD patient`) %>%
  group_by(`Is ILD patient`, `EthnicCat_r`)  %>%
  summarize(Sum = sum(n)) %>%
  spread(key = `Is ILD patient`, value = Sum) %>%
  mutate(ILD_Portion =
           Known_ILD_Patient /
           (Known_ILD_Patient +
              Not_a_known_ILD_Patient) * 100) %>%
  filter(!is.na(ILD_Portion))

######################################
#                                    #
#    Not plots, just numbers         #
#                                    #
#                                    #
######################################
##--------Figure 1 data-----------

CIE_percentage_table_age_and_gender_1 <- data %>%
  rename(`CIE status` = registered)  %>%
  filter(gender == "male" | gender == "female", age > 17) %>%
  select(age, `CIE status`, gender) %>%
  mutate(aged = if_else(age > 74, "age_75_and_over", "age_under_75")) %>%
  group_by(gender, aged, `CIE status`) %>%
  summarise(count = n())

CIE_percentage_table_age_and_gender_1_summary <-
  CIE_percentage_table_age_and_gender_1 %>%
  pivot_wider(
    names_from = c(`CIE status`, gender),
    values_from = count,
    names_sep = "."
  )

CIE_percentage_table_age_and_gender_2_summary <-
  CIE_percentage_table_age_and_gender_1 %>%
  pivot_wider(names_from = c(`CIE status`),
              values_from = count) %>%
  mutate(Registered_percentage = Registered / (Unregistered + Registered) * 100)

CIE_percentage_table_age_and_gender_3_summary <-
  CIE_percentage_table_age_and_gender_1 %>%
  pivot_wider(
    names_from = c(aged),
    values_from = count,
    names_sep = "."
  ) %>%
  mutate(aged_75_and_over_percent = age_75_and_over / (age_75_and_over +
                                                         age_under_75) * 100)


##---- Table-1-a

CIE_percentage_table_age_and_gender <-
  data %>%
  rename(`CIE status` = registered)  %>%
  filter(gender == "male" | gender == "female", age > 17) %>%
  select(age, `CIE status`, gender) %>%
  mutate(aged = case_when(
    age < 30   ~ "18 - 29",
    (age < 55 & age > 29) ~ "30 - 54",
    (age < 75 & age > 54)  ~ "55 - 74",
    age > 74   ~ "75+"
  )) %>%
  group_by(gender, aged, `CIE status`) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = c(`CIE status`),
              values_from = count) %>%
  mutate(Registered_percentage = Registered / (Unregistered + Registered) * 100)






##--------Figure 7 data-----------

## ----ILD-engagement----

ILD_stats_engagement <- data %>%
  filter(registered == "Registered") %>%
  count(`Is ILD patient`, engaged) %>%
  group_by(engaged, `Is ILD patient`) %>%
  summarize(Sum = sum(n)) %>%
  spread(key = engaged, value = Sum) %>%
  mutate(Engaged_Portion = Engaged_with_CIE / (Engaged_with_CIE + Not_engaged_with_CIE) * 100)
## ----ILD-questionnaire----

ILD_stats_questionnaire <- data %>%
  filter(registered == "Registered") %>%
  count(`Is ILD patient`, did_answer_questionnaire) %>%
  group_by(did_answer_questionnaire, `Is ILD patient`) %>%
  summarize(Sum = sum(n)) %>%
  spread(key = did_answer_questionnaire, value = Sum) %>%
  mutate(Answering_Portion = `Answered questionnaire` / (`Answered questionnaire` +
                                                           `Didn't answer questionnaire`) * 100)

ILD_COVID_questionnaire <-
  data %>%
  filter(registered == "Registered") %>%
  count(`Is ILD patient`, icht_covidsurvey_member) %>%
  group_by(icht_covidsurvey_member, `Is ILD patient`) %>%
  summarize(Sum = sum(n)) %>%
  spread(key = icht_covidsurvey_member, value = Sum) %>%
  mutate("Answered (%)" =
           round(
             Answered_COVID_Questionnaire /
               (
                 Answered_COVID_Questionnaire +
                   `Didn't_answer_COVID_Questionnaire`
               ) * 100
           ) ,
         1) %>%
  select(`Is ILD patient`, `Answered (%)`)
# save.image("~/Dropbox/R/FHJ/write_up/HSJ.RData")
# 
xRegistered_with_CIE <- data  %>% filter(registered == "Registered")  %>% tally()

xTotal <- data %>% tally()

xILD<- data  %>% filter(`Is ILD patient` == "Known_ILD_Patient")  %>% tally()

xRegILD <- data  %>% filter(`Is ILD patient` == "Known_ILD_Patient" , registered == "Registered")  %>% tally()

#########
######### 
Ethnicity_ILD_Reg <- 
  left_join(
    data %>%
      filter(age > 17) %>%
      filter(age > 17, registered == "Registered") %>%
      count(`EthnicCat_r`, `Is ILD patient`) %>%
      group_by(`Is ILD patient`, `EthnicCat_r`)  %>%
      summarize(Sum = sum(n)) %>%
      spread(key = `Is ILD patient`, value = Sum) %>%
      mutate(Registered_ILD_Portion =
               Known_ILD_Patient /
               (Known_ILD_Patient +
                  Not_a_known_ILD_Patient * 100), 
             Registered_ILD = Known_ILD_Patient, 
             Registered_Not_ILD = Not_a_known_ILD_Patient
      ) %>% select(EthnicCat_r, Registered_ILD_Portion, Registered_ILD,Registered_Not_ILD ),
    
    data %>%
      filter(age > 17) %>%
      filter(age > 17, registered == "Unregistered") %>%
      count(`EthnicCat_r`, `Is ILD patient`) %>%
      group_by(`Is ILD patient`, `EthnicCat_r`)  %>%
      summarize(Sum = sum(n)) %>%
      spread(key = `Is ILD patient`, value = Sum) %>%
      mutate(Unregistered_ILD_Portion =
               Known_ILD_Patient /
               (Known_ILD_Patient +
                  Not_a_known_ILD_Patient * 100), 
             Unregistered_ILD = Known_ILD_Patient, 
             Unregistered_Not_ILD = Not_a_known_ILD_Patient
      ) %>% select(EthnicCat_r, Unregistered_ILD_Portion, Unregistered_ILD, Unregistered_Not_ILD)
    
  )  %>%  
  mutate("ILD" = 100 *  Registered_ILD/(Registered_ILD + Unregistered_ILD) ,
         "No_ILD" = 100 * Registered_Not_ILD/(Registered_Not_ILD + Unregistered_Not_ILD)) %>% 
  select(EthnicCat_r, ILD , No_ILD) %>% 
  pivot_longer(-EthnicCat_r, names_to = "status", values_to = "percent")   %>% 
  
  ggplot(aes( x = reorder(`EthnicCat_r`, -percent), y = percent, fill = status)) +
  geom_bar(stat = "identity",
           position = "identity",
           alpha = 1) +
  scale_x_discrete() +
  labs(
    title = 
      "Figure 7c: ILD and non-ILD registration rates with CIE by ethnicity") +
  ylab("% of Cerner registering with CIE") +
  xlab("") +
  theme_tufte() + theme(text = element_text(family = "serif")) +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(
    palette = "Paired", 
    name = "",
    breaks = c("ILD", "No_ILD"),
    labels = c("Known ILD", "Not known ILD")
  ) +
  coord_flip()
###########
############ add

Ethnicity_ILD_CIE_ratio <-
  left_join(
    data %>%
      filter(age > 17) %>%
      filter(age > 17, registered == "Registered") %>%
      count(`EthnicCat_r`, `Is ILD patient`) %>%
      group_by(`Is ILD patient`, `EthnicCat_r`)  %>%
      summarize(Sum = sum(n)) %>%
      spread(key = `Is ILD patient`, value = Sum) %>%
      mutate(
        Registered_ILD_Portion =
          Known_ILD_Patient /
          (Known_ILD_Patient +
             Not_a_known_ILD_Patient * 100),
        Registered_ILD = Known_ILD_Patient,
        Registered_Not_ILD = Not_a_known_ILD_Patient
      ) %>% select(
        EthnicCat_r,
        Registered_ILD_Portion,
        Registered_ILD,
        Registered_Not_ILD
      ),
    
    data %>%
      filter(age > 17) %>%
      filter(age > 17, registered == "Unregistered") %>%
      count(`EthnicCat_r`, `Is ILD patient`) %>%
      group_by(`Is ILD patient`, `EthnicCat_r`)  %>%
      summarize(Sum = sum(n)) %>%
      spread(key = `Is ILD patient`, value = Sum) %>%
      mutate(
        Unregistered_ILD_Portion =
          Known_ILD_Patient /
          (Known_ILD_Patient +
             Not_a_known_ILD_Patient * 100),
        Unregistered_ILD = Known_ILD_Patient,
        Unregistered_Not_ILD = Not_a_known_ILD_Patient
      ) %>% select(
        EthnicCat_r,
        Unregistered_ILD_Portion,
        Unregistered_ILD,
        Unregistered_Not_ILD
      )
    
    
  )  %>%
  mutate(
    "ILD" = 100 *  Registered_ILD / (Registered_ILD + Unregistered_ILD) ,
    "No_ILD" = 100 * Registered_Not_ILD / (Registered_Not_ILD + Unregistered_Not_ILD)
  ) %>%
  mutate("ILD_reg_premium" = 100 * ILD / No_ILD) %>% 
  select(EthnicCat_r, ILD_reg_premium) %>%
  
  ggplot(aes(
    x = reorder(`EthnicCat_r`,-ILD_reg_premium),
    y = ILD_reg_premium,
    fill = EthnicCat_r
  )) +
  geom_bar(stat = "identity", color = "grey21") +
  scale_fill_brewer(palette = "RdBu") +
  scale_x_discrete() +
  labs(title =
         "Increase in registration for ILD patients above Cerner population") +
  ylab("%") +
  xlab("") +
  theme_tufte() + theme(text = element_text(family = "serif")) +
  coord_flip()















