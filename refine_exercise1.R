#load packages
library(dplyr)
library(tidyr)
library(readr)

#Import dataset
read.csv("~/R/Datasets/refine_original.csv")

#Clean up brand names
refine_original$company <- gsub("^[p|f].*", "philips", refine_original$company, ignore.case = TRUE)
refine_original$company <- gsub("^a.*", "akzo", refine_original$company, ignore.case = TRUE)
refine_original$company <- gsub("^v.*", "van houten", refine_original$company, ignore.case = TRUE)
refine_original$company <- gsub("^u.*", "unilever", refine_original$company, ignore.case = TRUE)

#Separate product code and product number
refine_original <- refine_original %>% separate("Product code / number", c("product_code", "product_number"), sep = "-")

#Add product categories
prodcat <- function(x) {
  if (x == "p"){
    return("Smartphone")
  } else if (x == "x") {
    return("Laptop")
  } else if (x == "v") {
    return("TV")
  } else if (x == "q") {
    return("Tablet")
  }
  }

refine_original <- refine_original %>% mutate(category = sapply(product_code, prodcat))

#Concatenate addresses
refine_original <- refine_original %>% unite(full_address, address, city, country, sep = ",")

#Create dummy binary variables for company and product category
refine_original <- refine_original %>% mutate(company_philips = (company == "philips")) %>% 
  mutate(company_akzo = (company == "akzo")) %>% 
  mutate(company_van_houten = (company == "van houten")) %>% 
  mutate(company_unilever = (company == "unilever"))

refine_original <- refine_original %>% mutate(product_smartphone = (category == "Smartphone")) %>% 
  mutate(product_tv = (category == "TV")) %>% 
  mutate(product_laptop = (category == "Laptop")) %>% 
  mutate(product_tablet = (category == "Tablet"))

#Save as refine_clean
write.csv(refine_original, file = "refine_clean.csv")
