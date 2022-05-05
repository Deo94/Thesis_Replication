## Required Packages

install.packages("wikipediatrend")
install.packages("tidyverse")

library(wikipediatrend)
library(tidyverse)

## Formatting names for the wikipediatrend packages

names_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//names_all_df.csv", header = T)

pv_names_df <- cbind(as.data.frame(str_replace_all(names_all_df$label, " ", "_" )), names_all_df$id)

colnames(pv_names_df) <- c('label', 'id')

pageviews_all <- list()

for (i in pv_names_df$label) {
  
  try(pageviews_all[[i]] <- wp_trend (
    page = i,
    from = "2008-01-01",
    to = "2021-12-31",
    lang = "en",
    warn = TRUE
  ))
}

pv_all_df <- as.data.frame(do.call(rbind, pageviews_all))

## Formatting results to bind with QID and other information

colnames(pv_all_df)[colnames(pv_all_df) == 'article'] <- 'label'

pv_names_temp <- pv_names_df

pv_names_temp$label <- tolower(pv_names_temp$label)

pv_all_df <- left_join(pv_all_df, pv_names_temp, by = "label")

pv_all_df <- right_join(pv_all_df[,-c(1,2)], names_all_df, by = "id")

pv_all_df <- pv_all_df[,c(4,3,2,1)]

write.csv(pv_all_df,"/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//pv_all_df.csv", row.names = FALSE)

