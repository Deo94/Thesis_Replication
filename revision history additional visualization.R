## Required packages

install.packages("tidyverse")
install.packages("showtext")

library(tidyverse)
library(showtext)

## Fonts for visualization

font_add_google("Quattrocento Sans")
showtext_auto()

coreclaims_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//coreclaims_all_df.csv")
revhist_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//revhist_all_df.csv")

rv_plot <- left_join(revhist_all_df, coreclaims_all_df, by = "label")

rv_plot$year <- format(as.Date(rv_plot$timestamp, format="%Y-%m-%d"),"%Y")

## Edits per Political Party (Top 5)

rv_plot1 <- rv_plot %>% 
  select(label, party_affiliation, timestamp, year) %>% 
  group_by(party_affiliation) %>% 
  tally() %>% 
  mutate(percentage = prop.table(n)) %>% 
  slice_max(order_by = n, n = 5) 


r1 <- ggplot(rv_plot1, aes(x=reorder(party_affiliation,+n), y=n, fill = party_affiliation)) + geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(title = "Top 5 Political Parties by Edits (2008-2021)",
       x = "Political Parties",
       y = "Total Edits") +
  theme(text = element_text(family = "Quattrocento Sans"))+
  theme(text = element_text(size = 19))

## Edits per political party per year (Top 5 from 2008-2021)

rv_plot2 <-  rv_plot %>% 
  select(label, party_affiliation, timestamp, year) %>% 
  group_by(party_affiliation, year) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(year) %>% 
  slice_max(order_by = n, n = 5)

r2<- ggplot(rv_plot2, aes(fill=party_affiliation, y=n, x=year)) +
  scale_y_continuous(labels = scales::comma) +
  geom_bar(position="stack", stat="identity") +
  guides(fill = guide_legend(title = "Party Affiliation")) +
  labs(title = "Top 5 Political Parties by Edits Per Year" ,
       x = "Year",
       y = "Total Edits") +
  theme(text = element_text(family = "Quattrocento Sans"))+
  theme(text = element_text(size = 19))

## Unused plots 

rv_plot3 <- rv_plot %>% 
  select(label, party_affiliation, timestamp, year) %>% 
  group_by(label) %>% 
  tally() %>% 
  slice_max(order_by = n, n = 10)



