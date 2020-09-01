library(readr)
# uses data from the 2011 census, only has English as main or any household language. 
# normalises to numbers of households, i.e. all households = 1. This imports from the file 
# supplied by the census. 

post_codes_language <- read_csv("data/post_codes_language.csv")

post_codes_language <-
  post_codes_language %>% mutate(postcode_sector = gsub(' ', '', postcode_sector))
post_codes_language <-  post_codes_language %>% 
  mutate(Total_households = All_english + Some_adult_english + Only_child_english + No_english)

post_codes_language <-  post_codes_language %>% 
  mutate(All_english = All_english/Total_households, 
  Some_adult_english = Some_adult_english/Total_households, 
  Only_child_english = Only_child_english/Total_households, 
  No_english = No_english/Total_households)
 



