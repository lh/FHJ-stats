library(tidyverse)



# if "import_tidy_nomix_language.R" has already been run and there is a .csv file this will add the data.
# This is the longest operation so do not repeat unless needed. It is independant
# of any CIE or Cerner data and can be run at any time. 

## ----import-the-prerun-data-from-pc-lang-csv-if-it-is-available----
if (file.exists("data/pc_lang.csv")) {
  pc_lang <-  read_csv("data/pc_lang.csv")
  
} else {
## ----generate-languages-lokkup-from-NOMIX-data
##  this is slow!  
##  The logic of the assigning of languages 
##  to different language groups is contained in 
##  here. Check ++
  bulk <- read_csv("data/bulk.csv") # %>%
  bulk <- bulk %>%  pivot_longer(
    cols = 4:348,
    names_to = "key",
    values_to = "cases",
    values_drop_na = TRUE
  ) # %>%
  
  bulk <-  bulk %>%   mutate(
    postcode_sector = stringr::str_squish(geography),
    postcode_sector = stringr::str_replace(postcode_sector, " ", ""),
    key = stringr::str_replace(key, "Age 3 and over;", "all_ages;"),
    key = stringr::str_replace(key, "; measures: Value", ""),
    key = stringr::str_replace(key, "Sex: ", ""),
    key = stringr::str_replace(key, "Females", "female"),
    key = stringr::str_replace(key, "Males", "male"),
    key = stringr::str_replace(key, "All persons", "all_people"),
    key = stringr::str_replace(key, "Main language: ", ""),
    key = stringr::str_replace(key, "All categories: Main language", "all_cat: all_lang"),
    key = stringr::str_replace(
      key,
      "Other European language (non EU)",
      "Other European language (non EU): "
    )
    
  )  # %>%
  
  bulk <-  bulk %>%
    separate(key, c("gender", "age_range", "main"), sep = ";")  %>%
    mutate(
      age_range = stringr::str_replace(age_range, "Age", ""),
      gender = stringr::str_trim(gender),
      age_range = stringr::str_replace(age_range, "Age", ""),
      age_range = stringr::str_replace(age_range, "All categories", ""),
      age_range = stringr::str_replace(age_range, ":", ""),
      age_range = stringr::str_replace(age_range, ":", ""),
      age_range = stringr::str_trim(age_range)
    ) %>%
    mutate(main = case_when(
      stringr::str_detect(main, ".+:.+") ~ main,!stringr::str_detect(main, ".+:.+") ~ stringr::str_replace(main, "(.+)", ":\\1")
    )) %>%
    separate(main, c("pc_language_group", "pc_language"), sep = ": ")
  
  bulk <- bulk %>% select(-date,-geography,-`geography code`)
  
  pc_lang <-  bulk %>%
    select(-pc_language_group) %>%
    filter(pc_language != "Total") %>%
    pivot_wider(names_from = pc_language, values_from = cases)  %>%
    mutate_at(vars(-all_lang,-gender,-age_range,-postcode_sector),
              funs(. / all_lang)) %>%
    select(-all_lang)  %>%
    filter(gender != "all_people") %>%
    filter(age_range != "all_ages")
  
  # add some columns summarising by language groups.
  
  pc_lang <- rowwise(pc_lang) %>%
    mutate(English_Languages = sum(c_across(`English (English or Welsh if in Wales)`))) %>%
    mutate(European_Languages = sum(c_across(
      French:`Other European language (non EU)`
    ))) %>%
    mutate(Arabic_Languages = sum(c_across(Arabic))) %>%
    mutate(`West/Central_Asian_Languages` = sum(`West/Central Asian language`)) %>%
    mutate(South_Asian_Languages = sum(c_across(Panjabi:`Any other South Asian language`))) %>%
    mutate(East_Asian_Languages = sum(c_across(Chinese:`Any other East Asian language`))) %>%
    mutate(African_Languages = sum(`African language`)) %>%
    mutate(Other_Languages = sum(`Other language`)) %>%
    as_tibble()
  
  #  "English", "European", "Arabic", "West/Central Asian", "South Asian", "East Asian", "African",  "Other"
  
  # approach to assign probabilities to individual ages but we don't have the data so better to put the main data in bins of age range.
  # do not use this. Instead change the big dataset to match these bins. Left in case of usefulness later.
  # bulk1 <- bulk %>%
  #   mutate(age_range = case_when(stringr::str_detect(age_range, "3 to 15") ~ toString(c(3:15)),
  #                             stringr::str_detect(age_range, "16 to 49") ~ toString(c(16:49)),
  #                                stringr::str_detect(age_range, "50 to 64") ~ toString(c(50:64)),
  #                                stringr::str_detect(age_range, "65 to 100") ~ toString(c(65:100))
  #   ))
  
  # manipulate data to put the ages in the same bins
  # the number of years between the postcode data and the CIE/PAS data
  
  
  write_csv(pc_lang, "data/pc_lang.csv")
  
  rm(bulk)
}
