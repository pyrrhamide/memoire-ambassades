####### Liste de tous les pays du jdd

library(tidyverse)
library(magrittr)
library(spData)
library(psData)

data("countrycode_data")
data("world")
f <- readRDS("~/memoire-ambassades/data/diplomatic_relations_all.rds")

pays <- f %>% select(destination,recepteur,expediteur)

pays_clean <- pays %>% 
  pivot_longer(c(destination,recepteur,expediteur),values_to = "pays") %>% 
  select(pays) %>% 
  distinct() %>% 
  na.omit() %>% 
  arrange(pays)

pays_clean %<>% 
  mutate(pays_new = case_when(str_detect(pays,"Korea, Democratic")==TRUE ~ "Korea, Democratic People's Republic of",
                              str_detect(pays,"Lao")==TRUE ~ "Lao People's Democratic Republic",
                              str_detect(pays,"Congo-Kinshasa")==TRUE ~ "Congo-Kinshasa",
                              str_detect(pays,"Grenadines")==TRUE ~ "Saint Vincent and the Grenadines",
                              pays=="Viet Namublic of" ~ "Viet Nam",
                              TRUE ~ pays))

# test countrycode
library(countrycode)

pays_clean %<>% mutate(iso_a2 = countrycode(pays_new, origin = "country.name", destination = "iso2c"),
                       ecb = countrycode(pays_new, origin = "country.name", destination = "ecb"),
                       eurostat = countrycode(pays_new, origin = "country.name", destination = "eurostat"),
                       genc2c = countrycode(pays_new, origin = "country.name", destination = "genc2c"))
  # Czechoslovakia, German Democratic Republic, Kosovo, Tanganyika, Yugoslavia, Zanzibar - FAILURE

# liste tous les pays du dataset
pays <- pays_clean %>% select(pays_new) %>% distinct
pays <- pays %>% mutate(continent = countrycode(pays_new,origin = "country.name",destination = "continent"),
                        region = countrycode(pays_new,origin = "country.name",destination = "region"))

pays %>% mutate(continent2 = case_when(pays=="Czechoslovakia" ~ "Europe",
                                              pays=="German Democratic Republic" ~ "Europe",
                                              pays=="Kosovo" ~ "Europe",
                                              pays=="Yugoslavia" ~ "Europe",
                                              pays=="Tanganyika" ~ "Africa",pays=="Zanzibar" ~ "Africa",TRUE ~ continent))

write.csv(pays,"data/diplomatic_exchange_pays.csv",row.names = FALSE,fileEncoding = "UTF-8")

### Appliquer changements directement sur main jdd ###
f %>% map_chr(class)
f %>% map_dbl(n_distinct)

f <- f %>% pays_new(expediteur)
f <- f %>% pays_new(recepteur)
f <- f %>% pays_new(destination)

f %<>% select(-c(expediteur,recepteur,destination)) 
f %<>% select(expediteur_new,recepteur_new,destination_new,everything())

write_rds(f, "data/diplomatic_relations_all.rds")
k <- readRDS("~/memoire-ambassades/data/diplomatic_relations_all.rds")
