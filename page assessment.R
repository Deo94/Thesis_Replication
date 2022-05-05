## Required Packages 

install.packages("tidyverse")
install.packages("showtext")
library(tidyverse)
library(showtext)

## Font for visualization 

font_add_google("Quattrocento Sans")
showtext_auto()

pageassessment_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//pageassessment_all_df.csv")

pa_df_1 <- pageassessment_all_df %>% count(class)

ggplot(pa_df_1, aes(x = class, y = n)) + 
  geom_bar(stat = 'identity') +
  labs(
    title = "Page assessment of Wikipedia pages about Indian legislators",
    y = "Number of Articles",
    x = "Wikipedia Class"
  ) +
  labs(caption = "B: The article is mostly complete and without major problems but requires some further work to reach,
C: The article is substantial but is still missing important content or contains much irrelevant material,
GA:The article has attained good article status, having been examined by one or more impartial reviewers,
Redirect: The page does not display any article content and redirects to a related topic,
Start: An article that is developing but still quite incomplete. It may or may not cite adequate reliable sources,
Stub: A very basic description of the topic") +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(text = element_text(family = "Quattrocento Sans")) +
  theme(text = element_text(size = 19)) 

