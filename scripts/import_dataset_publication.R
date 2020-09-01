# Importation of data file from CIE

## ----package-housekeeping----
list.of.packages <-
  c("tidyverse",
    "ggthemes",
    "readxl",
    "rmarkdown",
    "skimr",
    "devtools",
    "hexbin"
  )
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)


if (!require(visdat)) {
  devtools::install_github("ropensci/visdat")
}



## load required libraries
library(tidyverse)
library(ggthemes)
library(readxl)
library(skimr)
library(rmarkdown)
library(devtools)
library(visdat)
library(hexbin)


## ----read-in-the-main-file----
data <-
  read_csv("data/ILD_PS_For_Luke.csv")

## drop any blank lines
data <- data %>% filter(!(is.na(`Sector Post code`) & is.na(gender) & is.na(age)))

# explicitly set factors
genders <- c("male", "female", "indeterminate", "unknown")



## parse into factors where useful
data$gender <- parse_factor(data$gender, levels = genders)
data$did_answer_questionnaire <- factor(data$did_answer_questionnaire)
data$registered <- factor(data$registered)
data$icht_covidsurvey_member <- factor(data$icht_covidsurvey_member)
data$`Is ILD patient` <-  factor(data$`Is ILD patient`)




## Make the factors more useful for labelling graphs
data <- data %>% mutate(
  did_answer_questionnaire = recode(did_answer_questionnaire,
                                    `TRUE` = "Answered questionnaire",
                                    `FALSE` = "Didn't answer questionnaire")
)
data <- data %>% mutate(registered = recode(registered,
                                        `TRUE` = "Registered",
                                        `FALSE` = "Unregistered"))

data <- data %>% mutate(icht_covidsurvey_member = recode(icht_covidsurvey_member,
                                        `TRUE` = "Answered_COVID_Questionnaire",
                                        `FALSE` = "Didn't_answer_COVID_Questionnaire"))

data <- data %>% mutate(`Is ILD patient` = recode(`Is ILD patient`,
                                                     `TRUE` = "Known_ILD_Patient",
                                                     `FALSE` = "Not_a_known_ILD_Patient"))

## Make the names more useful for labelling graphs
data <- data %>% rename(postcode_sector = `Sector Post code`)

## ----create-derived-column----

## engaged are those who are registered and have used the system before the questionnaire.
data <-
  data %>% mutate(
    engaged = case_when(
      registered == "Registered" &
        (
          did_enter_diagnosis |
            did_enter_symptom |
            did_send_message 
          ) ~ "Engaged_with_CIE",
      TRUE ~ "Not_engaged_with_CIE"
    )
  )



# ILD_set <- data %>% filter(`Is ILD patient` == "Known_ILD_Patient" )

# write_csv(ILD_set, "2020_06_29_BAME_with_deprivation_only_ILD.csv")

