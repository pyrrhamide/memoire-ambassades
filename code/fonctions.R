### Packages ####
library(tidyverse)
library(janitor)
library(magrittr)
library(glue)

#### Liste des liens ####
score_lien <- function(data,x,y,z) {
  data <- data %>% mutate(match = case_when({{y}} == {{z}} ~ TRUE, TRUE ~ FALSE))
  
  direct <- data %>% 
    filter(match == TRUE) %>% 
    mutate(score = 4) %>% 
    select({{x}},{{y}},score) %>% 
    rename(source := {{x}}, target := {{y}})
  indirect <- data %>% 
    filter(match == FALSE) %>% 
    mutate(score = 1) %>% 
    select({{x}},{{z}},score) %>% 
    rename(source := {{x}}, target := {{z}})
  int_out <- data %>% 
    filter(match == FALSE) %>% 
    mutate(score = 3) %>% 
    select({{y}},{{z}},score) %>% 
    rename(source := {{y}}, target := {{z}})
  int_in  <- data %>% 
    filter(match == FALSE) %>% 
    mutate(score = 2) %>% 
    select({{x}},{{y}},score) %>% 
    rename(source := {{x}}, target := {{y}})
  
  bind_rows(direct, indirect, int_out, int_in) %>%
    group_by(source, target) %>%
    summarise(wgt_u = sum(score)) %>%
    ungroup() %>%
    mutate(weight = wgt_u / max(wgt_u) * 100)
}

#### Liste des noeuds ####
score_pays <- function(data,x,y,z) {
  pays <- read.csv("data/pays_clean.csv",encoding = "UTF-8")
  
  all <- score_lien(data,{{x}},{{y}},{{z}})
  
  all_out <- all %>% 
    group_by(source) %>% 
    summarise(wgt_out=sum(wgt_u)) %>% 
    rename(id := source)
  all_in <- all %>% 
    group_by(target) %>% 
    summarise(wgt_in=sum(wgt_u)) %>% 
    rename(id := target)
  
  score_tab <- full_join(all_out,all_in) %>% 
    mutate(across(wgt_out:wgt_in, ~ case_when(is.na(.x)~0,TRUE~.x)))
  
  left_join(score_tab,pays) %>% select(id,label,everything())
}

score_pays_simpl <- function(edge_list) {
  pays <- read.csv("data/pays_clean.csv",encoding = "UTF-8")
  
  all_out <- edge_list %>% 
    group_by(source) %>% 
    summarise(wgt_out=sum(wgt_u)) %>% 
    rename(id = source)
  all_in <- edge_list %>% 
    group_by(target) %>% 
    summarise(wgt_in=sum(wgt_u)) %>% 
    rename(id = target)
  
  score_tab <- full_join(all_out,all_in) %>% 
    mutate(across(wgt_out:wgt_in, ~replace(.x, is.na(.x), 0)),
           wgt_sum = wgt_in + wgt_out) 
  
  left_join(score_tab,pays) %>% select(id,label,everything())
}

score_pays_dis <- function(edge_list) {
  pays <- read.csv("../data/pays_clean.csv",encoding = "UTF-8")

  all_out <- as_tibble(tapply(edge_list$wgt_u,edge_list$source,sum),
                       rownames = "id")
  all_in  <- as_tibble(tapply(edge_list$wgt_u,edge_list$target,sum),
                       rownames = "id")
  
  score_tab <- full_join(all_out,all_in,by="id") %>% 
    rename(wgt_out = value.x,
           wgt_in  = value.y) %>% 
    mutate(across(wgt_out:wgt_in, ~replace(.x, is.na(.x), 0)),
           wgt_sum = wgt_in + wgt_out)
  
  left_join(score_tab,pays) %>% select(id,label,everything())
}

#### Réseau simple - liens ####
res_simple_e <- function(data,x,y) {
  data %>% 
    select(source := {{x}}, target := {{y}}) %>% 
    count(source,target)
}

#### Réseau simple - noeuds ####
res_simple_n <- function(data,x,y) {
  edges <- res_simple_e(data,{{x}},{{y}})
  pays <- read.csv("data/pays_clean.csv",encoding = "UTF-8")
  
  edges %>% 
    pivot_longer(source:target,values_to = "id") %>% 
    select(id) %>% 
    distinct() %>% 
    left_join(pays)
}

res_simple_n_dis <- function(edge_list) {
  pays <- read.csv("data/pays_clean.csv",encoding = "UTF-8")
  
  edge_list %>% 
    pivot_longer(source:target,values_to = "id") %>% 
    select(id) %>% 
    distinct() %>% 
    left_join(pays)
}

#### Renommer les pays ####
pays_new <- function(data,x) {
  data %>%
    mutate("{{x}}_new" := case_when(str_detect({{x}},"Korea, Democratic")==TRUE ~ "Korea, Democratic People's Republic of",
                            str_detect({{x}},"Lao")==TRUE ~ "Lao People's Democratic Republic",
                            str_detect({{x}},"Congo-Kinshasa")==TRUE ~ "Congo-Kinshasa",
                            str_detect({{x}},"Grenadines")==TRUE ~ "Saint Vincent and the Grenadines",
                            {{x}}=="Viet Namublic of" ~ "Viet Nam",
                            TRUE ~ {{x}}))
}

#### Tri à plat "personnalisé" (il y a plus simple) ####
tri_plat <- function(data,x) {
  data %>% 
    count({{x}}) %>% 
    #mutate(pourcent = prop.table(n)) %>%
    mutate(pourcent = round(n/sum(n)*100,1)) %>% 
    adorn_totals("row")
}

# alternative avec le package janitor. /!\ pas aligné sur la décimale quand on passe par kbl
# tri_plat2 <- function(data,x) {
#   data %>% 
#     tabyl({{x}}) %>% 
#     adorn_totals("row") %>% 
#     adorn_pct_formatting(digits = 1, affix_sign = F)
# }

# plus simple (urgh)
# questionr::freq(d$x,valid=FALSE,total=TRUE)


#### Tableau avec toutes les mesures pour les réseaux ####

tab_summary_reseau <- function(...) {
  list_rez <- enquos(...)
  noms <- as.character(list_rez) %>% map_chr(~str_sub(.x, start = 2))
  list_rez <- map(list_rez, rlang::eval_tidy)
  
  return(map2_dfr(list_rez, noms, ~ tibble(
    # "Graphe" = .y,
    "Type graphe" = case_when(grepl("_m",.y) ~ "Missions multiples",
                              grepl("_u",.y) ~ "Missions uniques",
                              grepl("_c",.y) ~ "Reseau complet",
                              TRUE ~ .y),
    "Noeuds" = vcount(.x), 
    "Liens" = ecount(.x), 
    "Densité" = graph.density(.x), 
    "Transitivité" = transitivity(.x), 
    "Diamètre" = diameter(.x), 
    "Distance moyenne" = average.path.length(.x)
  )))
}

tab_summary_reseau_uw <- function(...) {
  list_rez <- enquos(...)
  noms <- as.character(list_rez) %>% map_chr(~str_sub(.x, start = 2))
  list_rez <- map(list_rez, rlang::eval_tidy)
  
  return(map2_dfr(list_rez, noms, ~ tibble(
    # "Graphe" = .y,
    "Type graphe" = case_when(grepl("_m",.y) ~ "Missions multiples",
                              grepl("_u",.y) ~ "Missions uniques",
                              grepl("_c",.y) ~ "Reseau complet",
                              TRUE ~ .y),
    "Noeuds" = gorder(.x), 
    "Liens" = gsize(.x), 
    "Densité" = edge_density(.x), 
    "Transitivité" = transitivity(.x,weights=NA), 
    "Diamètre" = diameter(.x,weights=NA), 
    "Distance moyenne" = mean_distance(.x)
  )))
}


# tab_summary_reseau_old <- function(graph) {
#   
#   nom_graphe <- deparse(substitute(graph))
#   
#   tibble(
#     "Graphe" = nom_graphe,
#     "Noeuds" = vcount(graph), 
#     "Liens" = ecount(graph), 
#     "Densité" = graph.density(graph), 
#     "Transitivité" = transitivity(graph), 
#     "Diamètre" = diameter(graph), 
#     "Distance moyenne" = average.path.length(graph)
#   )
#   
# }

#### Tableau corrélations ----
tab_corr_reseau <- function(...) {
  list_rez <- enquos(...)
  noms <- as.character(list_rez) %>% map_chr(~str_sub(.x, start = 2))
  list_rez <- map(list_rez, rlang::eval_tidy)
  
  return(map2_dfr(list_rez, noms, ~ tibble(
    graphe = .y,
    
    btw.in = cor(V(.x)$wgt_in,centr_betw(.x)$res),
    btw.out = cor(V(.x)$wgt_out,centr_betw(.x)$res),
    btw.sum = cor(V(.x)$wgt_sum,centr_betw(.x)$res),
    
    deg.in = cor(V(.x)$wgt_in,degree(.x,mode = "in")),
    deg.out = cor(V(.x)$wgt_out,degree(.x,mode="out")),
    deg.sum = cor(V(.x)$wgt_sum,degree(.x))
  )))
}

#### Noeuds max ----
max_intermed <- function(graph) {
  max_node <- which.max(centr_betw(graph)$res)
  
  V(graph)$name[max_node]
}

tab_max_stuff <- function(graph) {
  node_deg <- which.max(degree(graph))
  node_deg_in <- which.max(degree(graph,mode = "in"))
  node_deg_out <- which.max(degree(graph,mode = "out"))
  node_btw <- which.max(centr_betw(graph)$res)
  
  data.frame(
    max.deg = names(node_deg),
    max.deg.in = names(node_deg_in),
    max.deg.out = names(node_deg_out),
    max.btw = V(graph)$name[node_btw]
  )
}

tab_max_nodes <- function(...) {
  List <- function(...) {
    names <- as.list(substitute(list(...)))[-1L]
    setNames(list(...), names)
  }
  list_rez <- List(...)
  list_tab <- lapply(list_rez,tab_max_stuff)
  do.call(rbind,list_tab)
}

#### Sauvegarde réseau csv ----

# save_res_csv <- function(edge,node) {
#   edge_name <- substitute(edge)
#   node_name <- substitute(node)
#   
#   write.csv(edge,
#             paste(edge_name,".csv",sep = ""),
#             fileEncoding = "UTF-8",row.names = F)
#   write.csv(node,
#             paste(node_name,".csv",sep = ""),
#             fileEncoding = "UTF-8",row.names = F)
# }

save.res.csv <- function (...) {
  objects <- list(...)
  objects_names <- as.list(substitute(list(...)))[-1L]
  nobjects <- length(objects)
  
  for (i in 1:nobjects) {
    write.csv(objects[[i]],
              glue("{objects_names[[i]]}.csv"),
              fileEncoding = "UTF-8",row.names = F)
  }
}
