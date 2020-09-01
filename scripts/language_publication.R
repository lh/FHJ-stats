# this runs after Import_sort.R: generates unique language codes to create lookup table. 
# After the first run of "Import_sort.R" use "prep.R" to import subset for analysis.
# this is the code to import the ISO codes: used to prepare the lookup table. Probably only run once then save and comment out. 

age_offset <- 9 # difference between the time of the census (2011) and data dump (2020)

ISO_lang <- read_excel("data/Cerner_ISO_lang.xlsx", 
                       sheet = "Sheet3", col_types = c("skip", 
                                                       "text", "text", "skip", "skip"))

Cerner_lang <- read_excel("data/Cerner_ISO_lang.xlsx")




# regularise the names
ISO_lang <- rename(ISO_lang, Alias = "ISO 639-1 Code", language = "English name of Language")
Cerner_lang <- rename(Cerner_lang, language = "Code Value Display")

# get a list of all the language codes used in PKB data
lang <- data %>% distinct(languagecode, .keep_all = FALSE)

# regularise the names
lang <- rename(lang, Alias = languagecode)

# this should produce the lookup tibble "lang" that contains a list of Aliases and Languages, 
# at first with missing vallues
lang <- lang  %>% left_join(Cerner_lang, by = "Alias")
lang <- lang  %>% left_join(ISO_lang, by = "Alias")

# sort though and choose the appropriate value that is not missing. 
lang <- lang %>% mutate(
  language = case_when(
    !is.na(language.x) ~ language.x,
    !is.na(language.y) ~ language.y,
    !is.na(Alias) ~ Alias
  )
)  %>% select(Alias, language)

# make sure Alias is unique and not NA
lang <- lang %>%  distinct(Alias, .keep_all = TRUE) %>% drop_na()

# Resultant lookup tibble "lang" contains a list of Aliases and Languages 
##########################################################################
 write_csv(lang, "data/language_lookup.csv")

# add a copy of languagecode in Alias
data <- data %>% mutate(Alias = languagecode)
data <- left_join(data, lang, by = "Alias")


# there are several different "NULL" or "UNKNOWN" values - all wrangled into one.
# the new column is called EthnicityDescription_r (for rationalised)
data <- data %>% mutate(EthnicityDescription_r = 
                      ifelse((EthnicityDescription == "UNKNOWN"|
                                EthnicityDescription == "NULL" |
                                EthnicityDescription == "Not Known" |
                                EthnicityDescription == "DEFAULTNULL"|
                                EthnicityDescription == "Not stated" 
                      ), "Unspecified", EthnicityDescription ))

# Ethnicity Description_r is a compound description for some, eg "African, first language French
# this code splits EthnicityDesctiption_r into two columns, eg "African" and "First language French". Called "Ethnicity_r" and "Language_r"
#

##----choose-ethnicity-descriptor----
##
## This choice is based on the detailed ethnicity descriptor in the compound variable 
## in "Ethnicity Description_r" as this is probably the best descriptor in the 
## Cerner data. However it is a  compound of ethnicity group, detail, and sometimes
## language. Hence the wrangling.  
data <- data %>% 
  mutate(EthnicDes = EthnicityDescription_r) %>% 
  separate(col = EthnicDes, into = c("Ethnicity_r","Language_r"), sep = ", ")

data <-  data %>% 
  mutate(Ethnicity_r = ifelse(is.na(Ethnicity_r), EthnicityDescription_r, Ethnicity_r))


## ----creating-language-from-compound-language-and-ethnicity
## there are many definitions of language; it is also in some of the ethnicity data, 
## this makes a field "language_eth" to show that is is derived from the ethnicity data. 
## the data is incomplete but interesting. 
data <- data %>% 
  mutate(language_eth = str_replace(Language_r, "Main spoken language", "")) %>% 
  mutate(language_eth = str_replace(Language_r, "Language", "")) 


## ----grouping-language-by-ons-descriptors----
# Language families: ONS data defines languages in broad families: 
#   European
#   Arabic
#   West/Central Asian
#   South Asian
#   East Asian
#   African
#   Other
#
#
# These have been defined in file "manual_language_groups.csv" following the principles of the ONS
# from the ONS grouping. 

manual_language_groups <- read_csv("data/manual_language_groups.csv")

lang_gp <- c("English", "European", "Arabic", "West/Central Asian", "South Asian", "East Asian", "African",  "Other")
manual_language_groups$Language_Group <- parse_factor(manual_language_groups$Language_Group, levels = lang_gp)
# make sure joining parameter is unique and not NA
manual_language_groups <- manual_language_groups %>%  distinct(language, .keep_all = TRUE) %>% drop_na()

data <- left_join(data, manual_language_groups)

## ----grouping-by-ages-for-matching-----
# preparation for adding the NOMIX dataset (in age groups)
data <- data %>% mutate(age_range = 
                      case_when(age < 15 + age_offset ~ "3 to 15", 
                            age > 15 + age_offset  & age <= 49 + age_offset ~"16 to 49", 
                            age > 49 + age_offset & age <= 64 + age_offset ~"50 to 64", 
                            age > 64 + age_offset ~"65 and over"))

