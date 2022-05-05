install.packages("tidyverse")
install.packages("ggpubr")
install.packages("showtext")

library(tidyverse)
library(ggpubr)
library(showtext)

font_add_google("Quattrocento Sans")
showtext_auto()


coreclaims_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//coreclaims_all_df.csv")
revhist_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//revhist_all_df.csv")

editor_df <- left_join(revhist_all_df, coreclaims_all_df, by = "label")

editor_df <- editor_df[,-c(8,10,11,12,13)]

editor_df <- left_join(editor_df, ip_df, by="user")

editor_df$year <- format(as.Date(editor_df$timestamp, format="%Y-%m-%d"),"%Y")

named_editor_df <- editor_df %>% filter(!(userid == 0))

## Jaccard for all editors

bjpeditors_all_df <- editor_df %>% filter(party_affiliation == "Bharatiya Janata Party")

congeditors_all_df <- editor_df %>% filter(party_affiliation == "Indian National Congress")

a <- c(unique(bjpeditors_all_df$user))
b <- c(unique(congeditors_all_df$user))

jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}

jaccard(a,b)

## Jaccard for named editors 

bjpeditors_named_df <- named_editor_df %>% filter(party_affiliation == "Bharatiya Janata Party")

congeditors_named_df <- named_editor_df %>% filter(party_affiliation == "Indian National Congress")

c <- c(unique(bjpeditors_named_df$user))
d <- c(unique(congeditors_named_df$user))

jaccard(c,d)

# simple count / all editors 

editor_count_df <- editor_df %>% count(user)

editor_count_df$percent <- (editor_count_df$n/sum(editor_count_df$n)) * 100

editor_count_df$cut <-   cut(editor_count_df$n,
                             breaks=c(0, 1, 10, 100, 1000, 5000),
                             labels=c('1', '1-10', '10-100', '100-1000','>1000'))

editor_count_df %>% 
  group_by(cut) %>% 
  mutate(contribution = sum(percent)) %>% 
  ungroup() %>% 
  group_by(cut, contribution) %>% 
  tally()

# simple count / named editors 

named_editor_count_df <- editor_df %>% filter(!(userid == 0)) %>% count(user)

named_editor_count_df$percent <- (named_editor_count_df$n/sum(named_editor_count_df$n)) * 100

named_editor_count_df$cut <-   cut(named_editor_count_df$n,
                                   breaks=c(0, 1, 10, 100, 1000, 5000),
                                   labels=c('1', '1-10', '10-100', '100-1000','>1000'))

editor_plot_1 <- named_editor_count_df %>% 
  group_by(cut) %>% 
  mutate(contribution = sum(percent)) %>% 
  ungroup() %>% 
  group_by(cut, contribution) %>% 
  tally()

ed1 <- ggplot(editor_plot_1, aes(x = cut, y = n, fill = cut)) + 
  geom_bar(stat = "identity") +
  labs(title = "Named Editors",
       x = "Number of Contributions (Range)",
       y = "Total Number of Editors") +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Quattrocento Sans"))+
  theme(text = element_text(size = 15)) +
  theme(plot.margin = margin(0.3,0.3,1,1, "cm"))

ed2 <- ggplot(editor_plot_1, aes(x = cut, y = contribution, fill = cut)) + geom_bar(stat = "identity") +
  geom_bar(stat = "identity") +
  labs(title = "Named Editors",
       x = "Number of Contributions (Range)",
       y = "Contribution to Total Edits(%)") +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Quattrocento Sans"))+
  theme(text = element_text(size = 15)) +
  theme(plot.margin = margin(0.3,0.3,1,1, "cm"))


# simplecount / anonymous editors 

anon_editor_count_df <- editor_df %>% filter(userid == 0) %>% count(user)

anon_editor_count_df$percent <- (anon_editor_count_df$n/sum(anon_editor_count_df$n)) * 100

anon_editor_count_df$cut <-   cut(anon_editor_count_df$n,
                                  breaks=c(0, 1, 10, 100, 1000, 5000),
                                  labels=c('1', '1-10', '10-100', '100-1000','>1000'))

editor_plot_2 <- anon_editor_count_df %>% 
  group_by(cut) %>% 
  mutate(contribution = sum(percent)) %>% 
  ungroup() %>% 
  group_by(cut, contribution) %>% 
  tally()

ed3 <- ggplot(editor_plot_2, aes(x = cut, y = n, fill = cut)) + 
  geom_bar(stat = "identity") +
  labs(title = "Anonymous Editors",
       x = "Number of Contributions (Range)",
       y = "Total Number of Editors") +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Quattrocento Sans"))+
  theme(text = element_text(size = 15)) +
  theme(plot.margin = margin(0.3,0.3,1,1, "cm"))

ed4 <- ggplot(editor_plot_2, aes(x = cut, y = contribution, fill = cut)) + geom_bar(stat = "identity") +
  geom_bar(stat = "identity") +
  labs(title = "Anonymous Editors",
       x = "Number of Contributions (Range)",
       y = "Contribution to Total Edits(%)") +
  theme(legend.position = "none") +
  theme(text = element_text(family = "Quattrocento Sans"))+
  theme(text = element_text(size = 15)) +
  theme(plot.margin = margin(0.3,0.3,1,1, "cm"))

ggarrange(ed1, ed2, ed3, ed4, ncol = 2, nrow = 2)

# contribution by year / named editors 

named_editor_contribution_year_df <- editor_df %>% 
  filter(!(userid == 0)) %>% 
  select(user, year) %>% 
  group_by(year) %>% 
  count(user)

named_editor_contribution_year_df$cut <-   cut(named_editor_contribution_year_df$n,
                                               breaks=c(0, 1, 10, 100, 1000, 5000),
                                               labels=c('1', '1-10', '10-100', '100-1000','>1000'))

named_editor_contribution_year_df$percent <- (named_editor_contribution_year_df$n/sum(named_editor_contribution_year_df$n)) * 100

editor_plot_3 <- named_editor_contribution_year_df %>% 
  group_by(cut,year) %>% 
  mutate(contribution = sum(percent)) %>% 
  ungroup() %>% 
  group_by(cut, year, contribution) %>% 
  tally()

editor_plot_3$year <- as.numeric(editor_plot_3$year)

ed5 <- ggplot(editor_plot_3, aes(x = year, y = contribution, fill = cut)) + geom_area()+
  scale_x_continuous(breaks=seq(2008,2021,1)) +
  guides(fill = guide_legend(title = "Number of Edits (Range)")) +
  labs(title = "Named Editor Contribution",
       x = "Year",
       y = "Contribution to Total Edits (%)") +
  theme(text = element_text(family = "Quattrocento Sans"))+
  theme(text = element_text(size = 15)) 

# contribution by year / Anonymous editors 

anon_editor_contribution_year_df <- editor_df %>% 
  filter(userid == 0) %>% 
  select(user, year) %>% 
  group_by(year) %>% 
  count(user)

anon_editor_contribution_year_df$cut <-   cut(anon_editor_contribution_year_df$n,
                                              breaks=c(0, 1, 10, 100, 1000, 5000),
                                              labels=c('1', '1-10', '10-100', '100-1000','>1000'))

anon_editor_contribution_year_df$percent <- (anon_editor_contribution_year_df$n/sum(anon_editor_contribution_year_df$n)) * 100

editor_plot_4 <- anon_editor_contribution_year_df %>% 
  group_by(cut,year) %>% 
  mutate(contribution = sum(percent)) %>% 
  ungroup() %>% 
  group_by(cut, year, contribution) %>% 
  tally()

editor_plot_4$year <- as.numeric(editor_plot_4$year)

ed6 <- ggplot(editor_plot_4, aes(x = year, y = contribution, fill = cut)) + geom_area()+
  scale_x_continuous(breaks=seq(2008,2021,1)) +
  guides(fill = guide_legend(title = "Number of Edits (Range)")) +
  labs(title = "Anonymous Editor Contribution",
       x = "Year",
       y = "Contribution to Total Edits (%)") +
  theme(text = element_text(family = "Quattrocento Sans"))+
  theme(text = element_text(size = 15)) 

ggarrange(ed5, ed6, nrow = 2)
