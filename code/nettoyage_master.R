####### HOUSE CLEANING - MASTER FILE #######

library(tidyverse)
library(magrittr)
library(questionr)

source("code/fonctions.R")
pays <- read.csv("data/pays_tous.csv",encoding = "UTF-8")

#### Setup ####
# Importation base - déjà en tibble
d <- readxl::read_xlsx("data/Diplomatic_Exchange_V3.16.16.xlsx") %>% janitor::clean_names()

# Observation rapide
class(d)
names(d)
str(d)
d %>% map_chr(class)
d %>% map_dbl(n_distinct)


#### Recodage ####
# On enlève les variables OLD et on change Year en num
k <- d %>% 
  # select(-`Embassy Old`, -`Focus Old`) %>% 
  mutate(year = as.numeric(year))

# On renomme les variables
k %<>%
  rename(expediteur = country,
         # type_rep = `Embassy New`,
         # degre_rep = `Focus New`,
         niv_rep = lor,
         recepteur = location)

# On attribue des labels à type_rep et degre_rep

table(k$embassy_new, useNA = "ifany")
#ou
k %>% count(embassy_new)

k %<>%
  mutate(
    embassy_lab = case_when(
      embassy_new == 1 ~ "Inconnu",
      embassy_new == 2 ~ "Tierce partie",
      embassy_new == 3 ~ "Interest desk",
      embassy_new == 4 ~ "Chargé d'affaires",
      embassy_new == 5 ~ "Ministre, consul, émissaire",
      embassy_new == 6 ~ "Ambassadeur",
      niv_rep == 0.1 & is.na(embassy_new) ~ "Tierce partie",
      niv_rep == 0.5 & is.na(embassy_new) ~ "Ambassadeur",
      niv_rep == 1 & is.na(embassy_new) ~ "Ambassadeur",
      is.na(embassy_new) ~ "Inconnu",
      TRUE ~ as.character(embassy_new)
    ) %>% factor %>%
      fct_relevel(
        c(
          "Inconnu",
          "Tierce partie",
          "Interest desk",
          "Chargé d'affaires",
          "Ministre, consul, émissaire",
          "Ambassadeur"
        )
      )
  )

k %>% count(embassy_new, embassy_lab)

table(k$focus_new, useNA = "ifany")
k %>% count(focus_old, focus_new)

k %<>%
  mutate(
    focus_lab = case_when(
      focus_new %in% c(0, 3) ~ "Relation rompue ou inexistante",
      focus_new == 1 ~ "Mission unique",
      focus_new == 2 ~ "Mission multiple",
      TRUE ~ as.character(focus_new)
    ) %>% factor %>%
      fct_relevel(
        c(
          "Relation rompue ou inexistante",
          "Mission multiple",
          "Mission unique"
        )
      )
  )

k %>% count(focus_new,focus_lab)

#### Certains types de représentants sont en NA, on se base sur le score de rep pour assigner les labels manquants ####
k %>% count(niv_rep,embassy_lab) %>% print(n=Inf)

k %>% mutate(embassy_lab = case_when(niv_rep==0.1 & is.na(embassy_lab) ~ "Tierce partie",
                                     niv_rep==0.5 & is.na(embassy_lab) ~ "Ambassadeur",
                                     niv_rep==1 & is.na(embassy_lab) ~ "Ambassadeur",
                                     is.na(embassy_lab) ~ "Inconnu",
                                     TRUE ~ as.character(embassy_lab))) %>% count(niv_rep,embassy_lab)


#### Enlever relations non-existantes ####
# k %<>%
#   filter(degre_rep != 0,
#          niv_rep != 0)

#### Décennies ####
k %<>%
  mutate(decade = case_when(year %in% 1960:1969 ~ "Années 60",
                            year %in% 1970:1979 ~ "Années 70",
                            year %in% 1980:1989 ~ "Années 80",
                            year %in% 1990:1999 ~ "Années 90",
                            year %in% 2000:2009 ~ "Années 2000",
                            TRUE ~ "2010-2013") %>% factor %>% 
           fct_relevel(c("Années 60","Années 70","Années 80","Années 90","Années 2000","2010-2013")))

#### Noms des pays ####
k <- k %>% pays_new(expediteur)
k <- k %>% pays_new(recepteur)
k <- k %>% pays_new(destination)

k %<>% select(-c(expediteur,recepteur,destination)) 
k %<>% select(expediteur = expediteur_new,recepteur = recepteur_new,destination = destination_new,everything()) 


## noms des pays en français - DONE

## regions et continents
library(countrycode)

pays %<>% mutate(iso_a2 = countrycode(pays_new, origin = "country.name", destination = "iso2c"),
                 ecb = countrycode(pays_new, origin = "country.name", destination = "ecb"),
                 eurostat = countrycode(pays_new, origin = "country.name", destination = "eurostat"),
                 genc2c = countrycode(pays_new, origin = "country.name", destination = "genc2c"),
                 continent = countrycode(pays_new,origin = "country.name",destination = "continent"),
                 region = countrycode(pays_new,origin = "country.name",destination = "region"))

# regions manquantes
  # which(is.na(pays$region))
  # pays[181,] # Tanganyika
pays$region[181] <- "Sub-Saharan Africa"

# continents manquants
  # unique(pays$continent)
  # which(is.na(pays$continent))  
  # pays[c(which(is.na(pays$continent))),]  
pays$continent[c(48,66,96,203)] <- "Europe" 
pays$continent[c(181,205)] <- "Africa"

## ajouter données géolocalisation
library(tidygeocoder)

pays_loc <- geocode(pays, country = "pays_francais", method = "osm")

# geom manquants
  which(is.na(pays_loc$lat))
  pays_loc[which(is.na(pays_loc$lat)),]

pays_loc$lat[25] <- pays_loc$lat[92]
pays_loc$long[25] <- pays_loc$long[92]
  
pays_loc$lat[48] <- pays_loc$lat[47]
pays_loc$long[48] <- pays_loc$long[47]

pays_loc$lat[66] <- 52.434
pays_loc$long[66] <- 12.5145

pays_loc$lat[77] <- 41.9038795
pays_loc$long[77] <- 12.4520834

pays_loc$lat[136] <- 35.248036
pays_loc$long[136] <- 33.657724

pays_loc$lat[151] <- pays_loc$lat[150]
pays_loc$long[151] <- pays_loc$long[150]

pays_loc$lat[170] <- 6.876991
pays_loc$long[170] <- 31.306978

pays_loc$lat[181] <- pays_loc$lat[182]
pays_loc$long[181] <- pays_loc$long[182]

pays_loc$lat[203] <- 44.81899672
pays_loc$long[203] <- 20.457331504

pays_loc$lat[205] <- -6.1333328
pays_loc$long[205] <- 39.3166654

pays_sf <- st_as_sf(x = pays_loc,
                      coords = c("long", "lat"),
                      crs = 4326)

write.csv(pays_loc,"data/pays_tous_geom.csv",row.names = F,fileEncoding = "UTF-8")

# j'enlève les codes iso2c et tout ça, ne me sont pas utiles
pays %<>% select(-c(iso_a2:genc2c))
write.csv(pays,"data/pays_clean.csv",row.names = F,fileEncoding = "UTF-8")

#### Coller noms des pays français à main base ####
pays_tous <- read.csv("data/pays_tous.csv",encoding = "UTF-8")

k <- k %>% left_join(pays_tous,by=c("expediteur"="pays_new")) %>% rename(expediteur_fr = pays_francais)
k <- k %>% left_join(pays_tous,by=c("recepteur"="pays_new")) %>% rename(recepteur_fr = pays_francais)
k <- k %>% left_join(pays_tous,by=c("destination"="pays_new")) %>% rename(destination_fr = pays_francais)

#### Coller noms des continents à main base ####
pays <- read.csv("data/pays_clean.csv",encoding = "UTF-8")

pays %<>% mutate(continent_fr = case_when(continent == "Africa" ~ "Afrique",
                                          continent == "Americas" ~ "Amériques",
                                          continent == "Asia" ~ "Asie",
                                          continent == "Oceania" ~ "Océanie",
                                          TRUE ~ continent))

k <- k %>% left_join(pays %>% select(id,continent),by=c("expediteur"="id")) %>% rename(cont_exp = continent)
k <- k %>% left_join(pays %>% select(id,continent),by=c("recepteur"="id")) %>% rename(cont_rec = continent)
k <- k %>% left_join(pays %>% select(id,continent),by=c("destination"="id")) %>% rename(cont_des = continent)

#### Coller noms des régions à main base ####

k <- k %>% left_join(pays %>% select(id,region),by=c("expediteur"="id")) %>% rename(reg_exp = region)
k <- k %>% left_join(pays %>% select(id,region),by=c("recepteur"="id")) %>% rename(reg_rec = region)
k <- k %>% left_join(pays %>% select(id,region),by=c("destination"="id")) %>% rename(reg_des = region)

#### bon port ####
k %<>% mutate(
  bon_port = case_when(
    destination == recepteur ~ "Hôte et cible correspondent",
    TRUE ~ "Hôte et cible ne correspondent pas"
  ),
  match_rd = case_when(destination == recepteur ~ TRUE, TRUE ~ FALSE)
)

write_rds(k, "data/diplomatic_relations_all.rds")
k <- readRDS("data/diplomatic_relations_all.rds")
