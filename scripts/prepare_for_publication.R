########################################
########################################
## Prepare for publication.           ## 
## this is equivalent to analysis.R   ##  
## but does not leave any temporary   ## 
## objects; just prepares for plots   ## 
## All the analysis remains in        ## 
## analysis.R but do                  ## 
## not run analysis unless            ## 
## needed for exploration             ## 
########################################
######################################## 
# This is the second of 2 files importing and analysing PKB data. This is run after the initial importation with "import_sort.R"
# 



data <- data %>% mutate(EthnicCat_r = 
                          ifelse((EthnicCat == "UNKNOWN"|
                                    EthnicCat == "NULL" |
                                    EthnicCat == "Not Known" |
                                    EthnicCat == "DEFAULTNULL"|
                                    EthnicCat == "Not stated" 
                          ), "Unspecified", EthnicCat ))

data <- data %>% mutate(postcode_sector = gsub(' ', '', postcode_sector)) 


data <- left_join(data, post_codes_language, by =  "postcode_sector")

data <- left_join(data, pc_lang)

data <- data %>% filter(age < 115)  # removes about a dozen apparently very old subjects with no other data present - possible test cases in the data

rm(pc_lang) # not needed again, and large









