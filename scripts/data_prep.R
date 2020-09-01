
## ----runs-all-the-scripts-in-order-without-making-the-analysis-document--for-development----
# saves the data for later analysis in RDS format. 


source("scripts/import_dataset_publication.R") 

source("scripts/import_postcode_data.R")  

source("scripts/language_publication.R") 

source("scripts/import_tidy_nomix_language.R")

source("scripts/prepare_for_publication.R") 

saveRDS(data, file = "data/data.RDS")               # these saves and read are for dev use. 
saveRDS(manual_language_groups, "data/mlg.RDS")
