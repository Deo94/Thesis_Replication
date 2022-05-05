## Required Packages

install.packages("httr")
install.packages("jsonlite")
install.packages("tidywikidatar")
install.packages("WikidataR")
install.packages("plyr")
install.packages("lubridate")
install.packages("wikipediatrend")

library(httr)
library(jsonlite)
library(tidyverse)
library(tidywikidatar)
library(WikidataR)
library(plyr)
library(lubridate)
library(wikipediatrend)

## Extract 17th Lok Sabha QIDs

mp <- tibble::tribble(
  ~p, ~q, 
  "P39", c("Q56051771")
) 

mp

mp_df_17 <- tw_query(query = mp) %>% as.data.frame() 

head(mp_df_17)

## Extract 16th Lok Sabha QIDs

mp <- tibble::tribble(
  ~p, ~q, 
  "P39", c("Q42509248")
) 

mp

mp_df_16 <- tw_query(query = mp) %>% as.data.frame() 

head(mp_df_16)

## Extract 15th Lok Sabha QIDs

mp <- tibble::tribble(
  ~p, ~q, 
  "P39", c("Q15686919")
) 

mp

mp_df_15 <- tw_query(query = mp) %>% as.data.frame() 

head(mp_df_15)

## Row bind MP ID's and remove duplicates

mp_df_all <- rbind(mp_df_17, mp_df_16) %>% distinct()

mp_df_all <- rbind(mp_df_all, mp_df_15) %>% distinct()

## Extract correct labels from Wikidata 

getPageTitle <- function(id) {
  
  tempsl <- GET( url = "https://wikidata.org/w/api.php", query = list(action="wbgetentities", ids= id, format="json", props= "sitelinks", languages= "en"))
  
  tempsl <- fromJSON(content(tempsl, "text"))
  
  PageTitle <- tempsl[[1]][[1]][[3]]$enwiki$title
  
  PageID <- tempsl[[1]][[1]][[2]]
  
  bound <- as.data.frame(cbind(PageID, PageTitle))
  
  return(bound)
}

# run for loop over all MP labels

names_all <- NULL

for (i in mp_df_all$id) {
  
  names_all[[i]] <- getPageTitle(i) 
  
}

names_all_df <- as.data.frame(do.call(rbind.fill, names_all))

colnames(names_all_df) <- c("id", "label")

# removing 6 N/A's [Total N = 1201]

which(is.na(names_all_df))

names_all_df <- na.omit(names_all_df)

## write to csv

getwd()

write.csv(names_all_df,"/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//names_all_df.csv", row.names = FALSE)

## Get Page Assessments ## 

# Write a function to extract information

getPageAssessment <- function(title) {
  
  url <- paste0("https://en.wikipedia.org/w/api.php?action=query&prop=pageassessments&format=json&titles=",title)
  
  encoded_url <- URLencode(url)
  
  assessment <- GET(url = encoded_url)
  
  parsed_assessment <- fromJSON(content(assessment, "text"))
  
  pageid <- parsed_assessment[[2]][[1]][[1]]['pageid']
  title <- parsed_assessment[[2]][[1]][[1]]['title']
  class <- parsed_assessment[[2]][[1]][[1]][[4]][[1]]['class']
  
  assessment_final <- cbind(pageid,title,class)
  
  return(assessment_final)
  
}

# Run a for loop over all MP pages

Page_assessment_all <- NULL

for (i in names_all_df$label) {
  
  try(Page_assessment_all[[i]] <- getPageAssessment(i), silent = T)
}

Page_assessment_all_df <- as.data.frame(do.call(rbind, Page_assessment_all))

Page_assessment_all_df$pageid <- as.character(Page_assessment_all_df$pageid)

Page_assessment_all_df$title <- as.character(Page_assessment_all_df$title)

Page_assessment_all_df$class <- as.character(Page_assessment_all_df$class)

Page_assessment_all_df <- Page_assessment_all_df %>%  na_if(., "")

# Write to csv

write.csv(Page_assessment_all_df,"/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//pageassessment_all_df.csv", row.names = F)

## Extract Core Data ## 

# Write a function to extract values and cbind them

getCoreClaims <- function(id) {
  
  all_claims <- get_item(id)
  
  party_affiliation <- (all_claims[[1]][['claims']][['P102']][['mainsnak']][['datavalue']][['value']][['id']])
  religion <- (all_claims[[1]][['claims']][['P140']][['mainsnak']][['datavalue']][['value']][['id']])
  gender <- (all_claims[[1]][['claims']][['P21']][['mainsnak']][['datavalue']][['value']][['id']])
  DoB <- (all_claims[[1]][['claims']][['P569']][['mainsnak']][['datavalue']][['value']][['time']])
  DoD <- (all_claims[[1]][['claims']][['P570']][['mainsnak']][['datavalue']][['value']][['time']])
  
  CoreClaims <- cbind(party_affiliation, religion, gender, DoB, DoD)
  
  return(CoreClaims)
}

# Run a for loop over all MP pages

core_claims <- NULL

for (i in names_all_df$id) {
  
  core_claims[[i]] <- getCoreClaims(i)
}

#Rowbind the core claims of all MP pages

core_claims_df <- plyr::ldply(core_claims, rbind)

core_claims_df <- core_claims_df %>%  distinct(.id, .keep_all = TRUE)

# Extracting labels from Wikipedia IDs for core claims w/ a function

return_claim_value <- function(id) {
  
  if (any(is.na(id))) {
    return(NA)
  }
  
  else{
    store <- get_item(id)
    
    return(store[[1]]$labels$en$value)
    
  }
}

# Run a for loop over core claims

for (j in 1:ncol(core_claims_df[-c(5,6)])){
  for (i in 1:nrow(core_claims_df)){
    
    core_claims_df[i, j] <- return_claim_value(core_claims_df[i,j])
    
  }}

# Cbind core claims and MP QID and labels and write to csv 

core_claims_unique_df <- cbind(names_all_df, core_claims_df[,c(-1)])

write.csv(core_claims_unique_df,"/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//coreclaims_all_df.csv", row.names = FALSE)

## Extract Political Claims ##

# Write a function to extract values and cbind them

getPoliticalClaims <- function(id) {
  
  all_claims <- get_item(id)
  
  political_office <- (all_claims[[1]][['claims']][['P39']][['mainsnak']][['datavalue']][['value']][['id']])
  
  return(political_office)
}

# Run a for loop over MP pages

political_claims <- NULL

for (i in names_all_df$id) {
  
  political_claims[[i]] <- getPoliticalClaims(i)
}

political_claims_df <- plyr::ldply(political_claims, rbind)

#Run a for loop over political claims to extract values from QIDs

for (j in 1:ncol(political_claims_df)){
  for (i in 1:nrow(political_claims_df)){
    
    political_claims_df[i, j] <- return_claim_value(political_claims_df[i,j])
    
  }}

# Cbind political claims and MP QID and labels and write to csv   

political_claims_df <- cbind(names_all_df, political_claims_df[,c(-1)])

write.csv(political_claims_df,"/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//politicalclaims_all_df.csv", row.names = FALSE)

## Extract Occupation Claims ##

getOccupationClaims <- function(id) {
  
  all_claims <- get_item(id)
  
  occupation <- (all_claims[[1]][['claims']][['P106']][['mainsnak']][['datavalue']][['value']][['id']])
  
  return(occupation)
}

occupation_claims <- NULL

for (i in names_all_df$id) {
  
  occupation_claims[[i]] <- getOccupationClaims(i)
}

occupation_claims_df <- plyr::ldply(occupation_claims, rbind)

# Run a for loop over occupation claims to extract claim values from claim QIDs

for (j in 1:ncol(occupation_claims_df)){
  for (i in 1:nrow(occupation_claims_df)){
    
    occupation_claims_df[i, j] <- return_claim_value(occupation_claims_df[i,j])
  }}

# Cbind political claims and MP QID and labels and write to csv   

occupation_claims_df <- cbind(names_all_df, occupation_claims_df[,c(-1)])

write.csv(occupation_claims_df,"/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//occupationclaims_all_df.csv", row.names = FALSE)




