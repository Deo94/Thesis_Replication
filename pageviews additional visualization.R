## Required Packages

install.packages("tidyverse")
install.packages("showtext")
library(tidyverse)
library(showtext)

## Adding fonts for visualization

font_add_google("Quattrocento Sans")
showtext_auto()

## Loading required data

coreclaims_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//coreclaims_all_df.csv")
pv_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//pv_all_df.csv")

## Formatting dates and joining dataframes

pv_plot <- left_join(pv_all_df, coreclaims_all_df, by = "label")

pv_plot$year <- format(as.Date(pv_plot$date, format="%Y-%m-%d"),"%Y")

## Plotting pageviews per political party

pv_plot_1 <- pv_plot %>% 
  select(views, party_affiliation) %>% 
  group_by(party_affiliation) %>% 
  summarise(sum = sum(views, na.rm = T)) %>% 
  mutate(percent = prop.table(sum)) %>% 
  slice_max(order_by = sum, n = 5)

p1<- ggplot(pv_plot_1, aes(x=reorder(party_affiliation,+sum), y=sum, fill = party_affiliation)) + geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(title = "Top 5 Political Parties by Pageviews (2008-2021)",
       x = "Political Parties",
       y = "Total Pageviews") +
  theme(text = element_text(family = "Quattrocento Sans")) +
  theme(text = element_text(size = 19)) 

## Plotting pageviews per political party by year 2008-2021

pv_plot_2 <- pv_plot %>% 
  select(views, party_affiliation, year) %>% 
  group_by(party_affiliation, year) %>% 
  summarise(sum = sum(views, na.rm = T)) %>% 
  mutate(percent = prop.table(sum)) %>% 
  group_by(year) %>% 
  slice_max(order_by = sum, n = 5) 

p2<- ggplot(na.omit(pv_plot_2), aes(fill=party_affiliation, y=sum, x=year)) +
  scale_y_continuous(labels = scales::comma) +
  geom_bar(position="stack", stat="identity") +
  guides(fill = guide_legend(title = "Political Party")) +
  labs(title = "Top 5 Political Parties by Pageviews Per Year" ,
       x = "Year",
       y = "Total Pageviews") +
  theme(text = element_text(family = "Quattrocento Sans")) +
  theme(legend.text = element_text(size = 15)) +
  theme(text = element_text(size = 19)) 

## Unused in Text (top and bottom 10 politicians)

pv_plot_3 <- pv_plot %>% 
  select(views, date, label, party_affiliation) %>% 
  group_by(label) %>% 
  summarise(sum = sum(views, na.rm = T),
            mean = mean(views,na.rm = T)) %>% 
  mutate(percent = prop.table(sum)) %>% 
  ungroup() %>% 
  slice_max(order_by = sum, n = 10) 

pv_plot_3 <- pv_plot_3 %>% right_join(select(pv_plot, label, party_affiliation), by = "label") %>% distinct() %>% drop_na()

pv_plot_4 <- pv_plot %>% 
  select(views, date, label, party_affiliation) %>% 
  group_by(label) %>% 
  summarise(sum = sum(views, na.rm = T),
            mean = mean(views,na.rm = T)) %>% 
  mutate(percent = prop.table(sum)) %>% 
  ungroup() %>% 
  slice_min(order_by = sum, n = 10) %>% 
  arrange(desc(sum))














