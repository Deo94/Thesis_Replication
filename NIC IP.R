## Required packages 

install.packages("tidyverse")
install.packages("lubridate")
install.packages("sclaes")
install.packages("showtext")

library(tidyverse)
library(lubridate)
library(scales)
library(showtext)

## Font for visualization 

font_add_google("Quattrocento Sans")
showtext_auto()

anon_revhist_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//anon_revhist_all_df.csv")
coreclaims_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//coreclaims_all_df.csv")
ip_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//ip_df.csv")

## Filtering anonymous edits by those originating from the National Informatics Centre 

ip_df_1 <- left_join(anon_revhist_all_df, coreclaims_all_df, by = "id")

ip_df_1 <- left_join(ip_df_1, ip_df, by = "user")

ip_df_1 <- ip_df_1[,-c(3,4,6,8,10,11,12,13)]

ip_df_2 <- ip_df_1 %>% 
  filter_all(any_vars(str_detect(., pattern = "National Informatics Centre")))

ip_df_2 %>% 
  group_by(party_affiliation) %>% 
  count(party_affiliation) %>%
  ungroup() %>% 
  slice_max(order_by = n, n =5)

ip_df_2_plot <- ip_df_2 %>% 
  group_by(timestamp) %>% 
  count(timestamp)

ip_df_2_plot$date <- ymd(ip_df_2_plot$timestamp)
ip_df_2_plot[, "year"] <- format(ip_df_2_plot$date, "%Y")


ggplot(ip_df_2_plot, aes(x = date, y = n)) + 
  geom_line() + 
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("1 year")) +
  geom_vline(xintercept = as.numeric(as.Date("2009-05-29")), linetype="dotted", color = "red", size=1.5) +
  geom_vline(xintercept = as.numeric(as.Date("2014-05-17")), linetype="dotted", color = "red", size=1.5) +
  geom_vline(xintercept = as.numeric(as.Date("2019-05-16")), linetype="dotted", color = "red", size=1.5) +
  labs(
    title = "Edits Originating from the National Informatics Council IP Address Range",
    y = "Number of Edits",
    x = "Year",
    caption = "Red line represents Indian general elections"
  ) +
  theme(text = element_text(family = "Quattrocento Sans")) +
  theme(text = element_text(size = 19)) 