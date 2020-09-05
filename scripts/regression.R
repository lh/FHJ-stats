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
library(conflicted)

conflict_prefer("filter", "dplyr")

library(epitools)
library(pscl)
library(ROCR)

## ----figure-4-c-repeated-from-data----
## 
data <- readRDS("data/data.RDS")
manual_language_groups <- readRDS("data/mlg.RDS")



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

Figure_4c

## take the top languages, 100 or more speakers in cohort
## 

## ----logit-analysis-identify-unique-languages----

languages_for_regression <- 
  Combined_lang_graph_data %>% 
  filter(All >= 100) %>% 
  select(language, all = All, cie = CIE ) %>% 
  mutate_if( is.character, as.factor) %>% 
  select(language)

## ----logit-analysis-by-language-versus-registration-and-engagement----

ttdata <- 
  data %>% 
  select(language, registered, engaged) %>% 
  mutate(language = ifelse(is.na(language), "unrecorded", language ))%>% 
  mutate(reg = ifelse(registered == "Registered", 1, 0), 
         eng = ifelse(engaged == "Engaged_with_CIE", 1, 0)) %>% 
  select(language, reg, eng) %>% 
  mutate_if( is.character, as.factor)

ttdata <- left_join(languages_for_regression, ttdata)
seed = 456
dt = sort(sample(nrow(ttdata), nrow(ttdata)*.7))



train<-ttdata[dt,]
test<-ttdata[-dt,]



## ----registration-language-logit----


train_reg <- 
  train %>% 
  select(-eng)
test_reg <- 
  test %>% 
  select(-eng)

model <- glm(reg ~.,family=binomial(link='logit'),data=train_reg)
summary(model)

anova(model, test="Chisq")
pR2(model)


fitted.results <- predict(model, newdata = test_reg,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_reg$reg)
print(paste('Accuracy',1-misClasificError))

p <- predict(model, test_reg, type="response")
pr <- prediction(p, test_reg$reg)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

## ----engagement-language-logit----
## 

train_eng <- 
  train %>% 
  select(-reg)
test_eng <- 
  test %>% 
  select(-reg)

model <- glm(eng ~.,family=binomial(link='logit'),data=train_eng)
summary(model)

anova(model, test="Chisq")
pR2(model)


fitted.results <- predict(model, newdata = test_eng,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_eng$eng)
print(paste('Accuracy',1-misClasificError))

p <- predict(model, test_eng, type="response")
pr <- prediction(p, test_eng$eng)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


## ----engagement-having-registered-language-logit----
## 
## 


ttdata <- 
  data %>% 
  filter(registered == "Registered") %>% 
  select(language, engaged) %>% 
  mutate(language = ifelse(is.na(language), "unrecorded", language ))%>% 
  mutate(eng = ifelse(engaged == "Engaged_with_CIE", 1, 0)) %>% 
  select(language, eng) %>% 
  mutate_if( is.character, as.factor)

ttdata <- left_join(languages_for_regression, ttdata)
seed = 456
dt = sort(sample(nrow(ttdata), nrow(ttdata)*.7))



train<-ttdata[dt,]
test<-ttdata[-dt,]


train_eng <- 
  train 
test_eng <- 
  test 

model <- glm(eng ~.,family=binomial(link='logit'),data=train_eng)
summary(model)

anova(model, test="Chisq")
pR2(model)


fitted.results <- predict(model, newdata = test_eng,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_eng$eng)
print(paste('Accuracy',1-misClasificError))

p <- predict(model, test_eng, type="response")
pr <- prediction(p, test_eng$eng)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

