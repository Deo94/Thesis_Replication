install.packages("tidyverse")
library(tidyverse)

names_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//names_all_df.csv", header = T)

## Write function to return revision history

getRevHist <- function(titles) {
  
  base_url= "http://en.wikipedia.org/w/api.php" 
  
  query_param =  list (  action  = "query",
                         titles = titles, 
                         format = "json",
                         prop = "revisions",
                         rvprop = "timestamp|ids|user|userid",
                         rvlimit = "max",
                         rvstart = "2008-01-01T12:00:00Z",
                         rvend = "2021-12-31T23:59:00Z",
                         rvdir = "newer")
  
  x <- GET(url = base_url, query = query_param)
  
  y <- fromJSON(content(x, "text"))
  
  revision_df <- y[[2]][[1]][[1]][[4]][c("revid", "parentid", "user", "userid", "timestamp")]
  
  page_title <- y[[2]][[1]][[1]][[3]]
  
  revision_df_title <- cbind(page_title, revision_df)
  
  while ("||" %in% y[[1]]) {
    
    continue_param <- y[[1]][[1]]
    
    continue_query_param <- list ( action  = "query",
                                   titles = titles, 
                                   format = "json",
                                   prop = "revisions",
                                   rvprop = "timestamp|ids|user|userid",
                                   rvlimit = "max",
                                   rvstart = "2008-01-01T12:00:00Z",
                                   rvend = "2021-12-31T23:59:00Z",
                                   rvdir = "newer",
                                   rvcontinue = continue_param)
    
    x <- GET(url = base_url, query = continue_query_param)
    
    y <- fromJSON(content(x, "text"))
    
    continue_revision_df <- y[[2]][[1]][[1]][[4]][c("revid", "parentid", "user", "userid", "timestamp")]
    
    revision_df <- rbind(revision_df, continue_revision_df)
    
    revision_df_title <- cbind(page_title, revision_df)
  }
  
  return(revision_df_title)
  
}

all_rev_hist <- NULL

for (i in names_all_df$label) {
  
  all_rev_hist[[i]] <- getRevHist(i)
} 

rev_hist_df <- as.data.frame(do.call(rbind, all_rev_hist))

colnames(rev_hist_df)[colnames(rev_hist_df) == 'page_title'] <- 'label'

rev_hist_df <- right_join(rev_hist_df, names_all_df, by = "label")

rev_hist_df <- rev_hist_df[,c(7,1,2,3,4,5,6)]

rev_hist_df$timestamp <- as.Date(rev_hist_df$timestamp)

## Filter all anonymous edits 

anon_rev_hist_df <- rev_hist_df %>% filter(userid == "0")

## Write to CSV

write.csv(anon_rev_hist_df,"/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//anon_revhist_all_df.csv", row.names = FALSE)

write.csv(rev_hist_df,"/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//revhist_all_df.csv", row.names = FALSE)

## Extract IP related information 

getIP <- function(ip) {
  
  temp <-GET(paste0("http://ipwhois.pro/json/",ip,"?key=PwyJvjhnRaN0qQ1R&objects=country,region,city,org,isp"))
  ip <- fromJSON(content(temp, "text"))
  
  return(ip)
}

all_ip_info <- NULL

for (i in anon_rev_hist_df$user){
  
  all_ip_info[[i]] <- getIP(i)
  
}

ip_df <-  as.data.frame(do.call(rbind, all_ip_info))

ip_df <- rownames_to_column(ip_df, "user")

ip_df <-apply(ip_df,2,as.character)

ip_df <- as.data.frame(ip_df)

write.csv(ip_df,"/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//ip_df.csv", row.names = FALSE)



