## Required Packages

install.packages("lubdridate")
install.packages("stringr")
install.packages("tidyverse")
install.packages("gppmisc")
install.packages("showtext")

library(lubridate)
library(stringr)
library(ggpmisc)
library(scales)
library(tidyverse)
library(showtext)

## Fonts for visualization

font_add_google("Quattrocento Sans")
showtext_auto()

## Load required data 

politicalclaims_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//politicalclaims_all_df.csv")
revhist_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//revhist_all_df.csv")

##15th Lok Sabha Revision Histories 

# Filter politicians who were members of the 15th Lok Sabha and bind their revision histories 

LS15_rev_df <- politicalclaims_all_df %>%  filter_all(any_vars(str_detect(., pattern = "15th")))

LS15_rev_df <- right_join(revhist_all_df, LS15_rev_df, by="label")

length(unique(LS15_rev_df$label))

# remove extra variables

LS15_rev_df <- LS15_rev_df[,-c(8:21)]

# Create new data frame with average edits per day 

LS15_rev_viz <- LS15_rev_df %>% 
  group_by(timestamp) %>% 
  summarise(mean(n()))

# formatting the dataframe for visualisation 

names(LS15_rev_viz)[2] <- 'events'
names(LS15_rev_viz)[1] <- 'date'

LS15_rev_viz$CommonDate <- ymd(paste0("2000-",str_sub(as.character(LS15_rev_viz$date),-5)))
LS15_rev_viz$date <-as.Date(LS15_rev_viz$date)
LS15_rev_viz[, "year"] <- format(LS15_rev_viz$date, "%Y")

# Ensure that only edits for the time period of the 15th Lok Sabha are being considered

LS15_rev_viz <- LS15_rev_viz[LS15_rev_viz$date >="2009-04-01" & LS15_rev_viz$date <= "2014-03-31", ]

# Visualize with faceting for years 

ggplot(na.omit(LS15_rev_viz), aes(CommonDate, events)) + 
  geom_line() +
  facet_grid(year~., scales="free") +
  stat_peaks(geom="label", ignore_threshold = 0.3, span = 23, x.label.fmt = "%d-%b", color="red", angle=0, hjust=-0.1) +
  scale_x_date(labels = date_format("%d-%b"), breaks = "1 months") +
  labs(
    title = "Peaks in Editing Activity for the 15th Lok Sabha Session",
    x = "Date",
    y = "Average Daily Edits for all Members of Parliament"
  ) +
  theme(text = element_text(family = "Quattrocento Sans"))+
  theme(text = element_text(size = 19))


## 16th LS 

LS16_rev_df <- politicalclaims_all_df %>%  filter_all(any_vars(str_detect(., pattern = "16th")))

LS16_rev_df <- right_join(revhist_all_df, LS16_rev_df, by="label")

LS16_rev_df <- LS16_rev_df[,-c(8:21)]

LS16_rev_viz <- LS16_rev_df %>% 
  group_by(timestamp) %>% 
  summarise(mean(n()))

names(LS16_rev_viz)[2] <- 'events'
names(LS16_rev_viz)[1] <- 'date'

LS16_rev_viz$CommonDate <- ymd(paste0("2000-",str_sub(as.character(LS16_rev_viz$date),-5)))
LS16_rev_viz$date <-as.Date(LS16_rev_viz$date)
LS16_rev_viz[, "year"] <- format(LS16_rev_viz$date, "%Y")

LS16_rev_viz <- LS16_rev_viz[LS16_rev_viz$date >="2014-04-01" & LS16_rev_viz$date <= "2019-03-31", ]

ggplot(na.omit(LS16_rev_viz), aes(CommonDate, events)) + 
  geom_line() +
  facet_grid(year~., scales="free") +
  stat_peaks(geom="label", ignore_threshold = 0.3, span = 19, x.label.fmt = "%d-%b", color="red", angle=0, hjust=-0.1) +
  scale_x_date(labels = date_format("%d-%b"), breaks = "1 months") +
  labs(
    title = "Peaks in Editing Activity for the 16th Lok Sabha Session",
    x = "Date",
    y = "Average Daily Edits for all Members of Parliament"
  ) +
  theme(text = element_text(family = "Quattrocento Sans"))+
  theme(text = element_text(size = 19))

## 17th LS

LS17_rev_df <- politicalclaims_all_df %>%  filter_all(any_vars(str_detect(., pattern = "17th")))

LS17_rev_df <- right_join(revhist_all_df, LS17_rev_df, by="label")

LS17_rev_df <- LS17_rev_df[,-c(8:21)]

LS17_rev_viz <- LS17_rev_df %>% 
  group_by(timestamp) %>% 
  summarise(mean(n()))

names(LS17_rev_viz)[2] <- 'events'
names(LS17_rev_viz)[1] <- 'date'

LS17_rev_viz$CommonDate <- ymd(paste0("2000-",str_sub(as.character(LS17_rev_viz$date),-5)))
LS17_rev_viz$date <-as.Date(LS17_rev_viz$date)
LS17_rev_viz[, "year"] <- format(LS17_rev_viz$date, "%Y")

LS17_rev_viz <- LS17_rev_viz[LS17_rev_viz$date >="2019-04-01" & LS17_rev_viz$date <= "2021-12-31", ]

ggplot(na.omit(LS17_rev_viz), aes(CommonDate, events)) + 
  geom_line() +
  facet_grid(year~., scales="free") +
  stat_peaks(geom="label", ignore_threshold = 0.3, span = 19, x.label.fmt = "%d-%b", color="red", angle=0, hjust=-0.1) +
  scale_x_date(labels = date_format("%d-%b"), breaks = "1 months")+
  labs(
    title = "Peaks in Editing Activity for the 17th Lok Sabha Session",
    x = "Date",
    y = "Average Daily Edits for all Members of Parliament"
  ) +
  theme(text = element_text(family = "Quattrocento Sans"))+
  theme(text = element_text(size = 19))
