#Required packages 

install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggpmisc")
install.packages("ggrepel")
install.packages("scales")
install.packages("showtext")
library(tidyverse)
library(lubridate)
library(ggpmisc)
library(ggrepel)
library(scales)
library(showtext)

## Adding fonts for visualization

font_add_google("Quattrocento Sans")
showtext_auto()

## Loading required data

politicalclaims_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//politicalclaims_all_df.csv")
pv_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//pv_all_df.csv")

### 15th LS 

## Filtering politicians from 15th Lok Sabha, binding pageviews info, and selecting appropriate time period

LS15_pv_df <- politicalclaims_all_df %>%  filter_all(any_vars(str_detect(., pattern = "15th")))

LS15_pv_df <- right_join(pv_all_df, LS15_pv_df, by = "label")

LS15_pv_df <- LS15_pv_df[,-c(5:18)]

LS15_pv_df <- LS15_pv_df[LS15_pv_df$date >="2009-04-01" & LS15_pv_df$date <= "2014-03-31", ] 

LS15_pv_df$date <- ymd(LS15_pv_df$date)

## Formatting data for visualization 

LS15_viz <- LS15_pv_df %>%group_by(date)

LS15_viz<-summarize(LS15_viz, mean = mean(views, na.rm = TRUE))

LS15_viz$CommonDate <- ymd(paste0("2000-",str_sub(as.character(LS15_viz$date),-5)))
LS15_viz[, "year"] <- format(LS15_viz$date, "%Y")

ggplot(na.omit(LS15_viz), (aes(CommonDate, mean))) + 
  geom_line() +
  stat_peaks(geom="label", span = 31, ignore_threshold = 0.4, x.label.fmt = "%d-%b" , color="red", angle=0, hjust=-0.1, strict = T) +
  facet_grid(year~., scales="free")+
  scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("1 month")) +
  labs(
    title = "Peaks in Pageviews for the 15th Lok Sabha Session",
    x = "Date",
    y = "Average Daily Pageviews for all Members of Parliament"
  ) +
  theme(text = element_text(family = "Quattrocento Sans"))+
  theme(text = element_text(size = 20))

### 16th LS

LS16_pv_df <- politicalclaims_all_df %>%  filter_all(any_vars(str_detect(., pattern = "16th")))

LS16_pv_df <- right_join(pv_all_df, LS16_pv_df, by = "label")

LS16_pv_df <- LS16_pv_df[,-c(5:18)]

LS16_pv_df <- LS16_pv_df[LS16_pv_df$date >="2014-04-01" & LS16_pv_df$date <= "2019-03-31", ] 

LS16_pv_df$date <- ymd(LS16_pv_df$date)

LS16_viz <- LS16_pv_df %>%group_by(date)

LS16_viz<-
  summarize(LS16_viz, mean = mean(views, na.rm = TRUE))

LS16_viz$CommonDate <- ymd(paste0("2000-",str_sub(as.character(LS16_viz$date),-5)))
LS16_viz[, "year"] <- format(LS16_viz$date, "%Y")

ggplot(na.omit(LS16_viz), (aes(CommonDate, mean))) + 
  geom_line() +
  stat_peaks(geom="label", span = 15, ignore_threshold = 0.5, x.label.fmt = "%d-%b" , color="red", angle=0, hjust=-0.1, strict = T) +
  facet_grid(year~., scales="free")+
  scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("1 month"))+
  labs(
    title = "Peaks in Pageviews for the 16th Lok Sabha Session",
    x = "Date",
    y = "Average Daily Pageviews for all Members of Parliament"
  ) +
  theme(text = element_text(family = "Quattrocento Sans"))+
  theme(text = element_text(size = 19))

### 17th LS 

LS17_pv_df <- politicalclaims_all_df %>%  filter_all(any_vars(str_detect(., pattern = "17th")))

LS17_pv_df <- right_join(pv_all_df, LS17_pv_df, by = "label")

LS17_pv_df <- LS17_pv_df[,-c(5:18)]

LS17_pv_df <- LS17_pv_df[LS17_pv_df$date >="2019-04-01" & LS17_pv_df$date <= "2021-12-31", ] 

LS17_pv_df$date <- ymd(LS17_pv_df$date)

LS17_viz <- LS17_pv_df %>%group_by(date)

LS17_viz<-
  summarize(LS17_viz, mean = mean(views, na.rm = TRUE))

LS17_viz$CommonDate <- ymd(paste0("2000-",str_sub(as.character(LS17_viz$date),-5)))
LS17_viz[, "year"] <- format(LS17_viz$date, "%Y")

ggplot(na.omit(LS17_viz), (aes(CommonDate, mean))) + 
  geom_line() +
  stat_peaks(geom="label", span = 9, ignore_threshold = 0.3, x.label.fmt = "%d-%b" , color="red", angle=0, hjust=-0.1, strict = T) +
  facet_grid(year~., scales="free")+
  scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("1 month")) +
  labs(
    title = "Peaks in Pageviews for the 17th Lok Sabha Session",
    x = "Date",
    y = "Average Daily Pageviews for all Members of Parliament"
  ) +
  theme(text = element_text(family = "Quattrocento Sans"))+
  theme(text = element_text(size = 20))
  
  
  