#Install packages 

install.packages("tidyverse")
install.packages("lubridate")
install.packages("lmtest")
install.packages("performance")
install.packages("see")
install.packages('patchwork')
install.packages("stats")
install.packages("car")
install.packages("coefplot")
install.packages("showtext")

library(tidyverse)
library(lubridate)
library(lmtest)
library(performance)
library(see)
library(patchwork)
library(stats)
library(car)
library(coefplot)
library(showtext)

font_add_google("Quattrocento Sans")
showtext_auto()

#Load Raw Data 

politicalclaims_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//politicalclaims_all_df.csv")
coreclaims_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//coreclaims_all_df.csv")
pv_all_df <- read.csv("/Users/akhildeo/Desktop/Deo_Thesis_Replication/Raw_Data//pv_all_df.csv")

# Format Coreclaims data to include age 

coreclaims_all_df$DoB <- str_sub(coreclaims_all_df$DoB, -20)
coreclaims_all_df$DoB <- as.Date(coreclaims_all_df$DoB)

coreclaims_all_df$DoD<- str_sub(coreclaims_all_df$DoD, -20)
coreclaims_all_df$DoD <- as.Date(coreclaims_all_df$DoD)

coreclaims_all_df$DoB <- ymd(coreclaims_all_df$DoB)

coreclaims_all_df$age <- year(Sys.Date()) - year(coreclaims_all_df$DoB)

## Create new dataframe with DV and IV's for 15th Lok Sabha 

# Create new dataframe extracting only members of the 15th lok sabha from the political claims data 

OLS_LS15_df <- politicalclaims_all_df %>%  filter_all(any_vars(str_detect(., pattern = "15th")))

# Use R's grep functionality to extract various positions, namely, Union Minister, 
# Leader or Deputy Leader of the House, Chief Minister, Speaker or Deputy Speaker,
# Member of the Rajya Sabha, and total number of sessions served

OLS_LS15_df$unionMinister  <- apply(OLS_LS15_df, 1, function(x)as.integer(any(grep("Minister of",x))))
OLS_LS15_df$leader  <- apply(OLS_LS15_df, 1, function(x)as.integer(any(grep("Leader",x))))
OLS_LS15_df$chiefMinister <- apply(OLS_LS15_df, 1, function(x)as.integer(any(grep("Chief",x))))
OLS_LS15_df$MLA <- apply(OLS_LS15_df, 1, function(x)as.integer(any(grep("Legislative",x))))
OLS_LS15_df$speaker <- apply(OLS_LS15_df, 1, function(x)as.integer(any(grep("Speaker",x))))
OLS_LS15_df$RS <- apply(OLS_LS15_df, 1, function(x)as.integer(any(grep("Rajya",x)))) 
OLS_LS15_df$sessions <- rowSums(sapply(OLS_LS15_df, function(i) grepl('Member.*Lok', i)))

# Remove extraneous variables 

OLS_LS15_df <- OLS_LS15_df[,-c(3:15)]

# Create new data frame by merging pageviews data

OLS_LS15_df_pv <- right_join(pv_all_df, OLS_LS15_df, by = "label")

OLS_LS15_df_pv <- OLS_LS15_df_pv[OLS_LS15_df_pv$date >= "2009-04-01" & OLS_LS15_df_pv$date <= "2014-03-31", ]

# Find mean page views
OLS_LS15_df_pv <- OLS_LS15_df_pv %>% 
  group_by(label) %>% 
  summarise(traffic_mean = mean(views, na.rm = T)) %>% 
  ungroup()

# Rejoin previous dataframe because summarising removes information

OLS_LS15_df_pv <- right_join(OLS_LS15_df_pv, OLS_LS15_df, by = "label")

OLS_LS15_df_pv  <- left_join(OLS_LS15_df_pv , coreclaims_all_df, by = "label")

# Remove extranious information

OLS_LS15_df_pv <- OLS_LS15_df_pv[,-c(11,13,15,16)]

# Log the mean traffic variable  

hist(OLS_LS15_df_pv$traffic_mean)

OLS_LS15_df_pv$traffic_mean <- log(OLS_LS15_df_pv$traffic_mean)

OLS_LS15_df_pv[is.na(OLS_LS15_df_pv) | OLS_LS15_df_pv=="-Inf"] = NA

hist(OLS_LS15_df_pv$traffic_mean)

# Fit the Model 

fit1 <- lm(traffic_mean ~ unionMinister +  leader + speaker + chiefMinister + RS + sessions + gender + age + party_affiliation, data = OLS_LS15_df_pv)

# Summary and Checks 

summary(fit1)

## Check for multicolinearity

vif(fit1)

## Check for linearity 

plot(fit1,1)

## Check for normal distribution of residuals

hist(fit1$residuals)
plot(fit1, 2)

## Check for heteroskedasticity 

plot(fit1, 3)
bptest(fit1)

## Check for outliers

plot(fit1, 5)

## Coefplot

coefplot(fit1, 
         coefficients = c("unionMinister","leader", "speaker", "chiefMinister", "RS", "sessions", "age", "gendermale"),
         newNames = c(unionMinister = "Union Minister*", leader = "Leader of House", speaker = "Speaker of House", chiefMinister = "Chief Minister**", RS = "Member of Rajya Sabha***", sessions = "Sessions Served**", age = "Age", gendermale = "Gender(Male)."),
         title = "OLS Estimates of Legislators Characteristics on Log Daily Pageviews (15th Lok Sabha)",
         xlab = "Regression Coefficient ",
         ylab = "Legislator Characteristics") +
  labs(
    caption = "OLS coefficients plotted with 50% and 95% confidence intervals. N = 500. Fit statistics: Adj. R2 = 0.2239. 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’"
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(text = element_text(family = "Quattrocento Sans")) +
  theme(text = element_text(size = 19)) 

## Replicate for 16th Lok Sabha

OLS_LS16_df <- politicalclaims_all_df %>%  filter_all(any_vars(str_detect(., pattern = "16th")))

OLS_LS16_df$unionMinister  <- apply(OLS_LS16_df, 1, function(x)as.integer(any(grep("Minister of",x))))
OLS_LS16_df$leader  <- apply(OLS_LS16_df, 1, function(x)as.integer(any(grep("Leader",x))))
OLS_LS16_df$chiefMinister <- apply(OLS_LS16_df, 1, function(x)as.integer(any(grep("Chief",x))))
OLS_LS16_df$MLA <- apply(OLS_LS16_df, 1, function(x)as.integer(any(grep("Legislative",x))))
OLS_LS16_df$speaker <- apply(OLS_LS16_df, 1, function(x)as.integer(any(grep("Speaker",x))))
OLS_LS16_df$RS <- apply(OLS_LS16_df, 1, function(x)as.integer(any(grep("Rajya",x)))) 
OLS_LS16_df$sessions <- rowSums(sapply(OLS_LS16_df, function(i) grepl('Member.*Lok', i)))

OLS_LS16_df <- OLS_LS16_df[,-c(3:15)]

OLS_LS16_df_pv <- right_join(pv_all_df, OLS_LS16_df, by = "label")

OLS_LS16_df_pv <- OLS_LS16_df_pv[OLS_LS16_df_pv$date >= "2014-04-01" & OLS_LS16_df_pv$date <= "2019-03-31", ]

OLS_LS16_df_pv <- OLS_LS16_df_pv %>% 
  group_by(label) %>% 
  summarise(traffic_mean = mean(views, na.rm = T)) %>% 
  ungroup()

OLS_LS16_df_pv<- right_join(OLS_LS16_df_pv, OLS_LS16_df, by = "label")

OLS_LS16_df_pv <- left_join(OLS_LS16_df_pv, coreclaims_all_df, by = "label")

OLS_LS16_df_pv <- OLS_LS16_df_pv[,-c(11,13,15,16)]

hist(OLS_LS16_df_pv$traffic_mean)

OLS_LS16_df_pv$traffic_mean <- log(OLS_LS16_df_pv$traffic_mean)

OLS_LS16_df_pv[is.na(OLS_LS16_df_pv) | OLS_LS16_df_pv=="-Inf"] = NA

hist(OLS_LS16_df_pv$traffic_mean)

fit2 <- lm(traffic_mean ~ unionMinister +  leader + speaker + chiefMinister + RS + sessions + gender + age + party_affiliation, data = OLS_LS16_df_pv)

summary(fit2)

vif(fit2)

plot(fit2,1)

hist(fit1$residuals)
plot(fit2, 2)

plot(fit2, 3)
bptest(fit2)

plot(fit2, 5)

coefplot(fit2, 
         coefficients = c("unionMinister","leader", "speaker", "chiefMinister", "RS", "sessions", "age", "gendermale"),
         newNames = c(unionMinister = "Union Minister***", leader = "Leader of House***", speaker = "Speaker of House", chiefMinister = "Chief Minister*", RS = "Member of Rajya Sabha**", sessions = "Sessions Served***", age = "Age", gendermale = "Gender(Male)***"),
         title = "OLS Estimates of Legislators Characteristics on Log Daily Pageviews (16th Lok Sabha)",
         xlab = "Regression Coefficient ",
         ylab = "Legislator Characteristics") +
  labs(
    caption = "OLS coefficients plotted with 50% and 95% confidence intervals. N = 573. Fit statistics: Adj. R2 = 0.334. 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’"
  ) +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(text = element_text(family = "Quattrocento Sans")) +
  theme(text = element_text(size = 19)) 

## Replicate for 17th Lok Sabha

OLS_LS17_df <- politicalclaims_all_df %>%  filter_all(any_vars(str_detect(., pattern = "17th")))

OLS_LS17_df$unionMinister  <- apply(OLS_LS17_df, 1, function(x)as.integer(any(grep("Minister of",x))))
OLS_LS17_df$leader  <- apply(OLS_LS17_df, 1, function(x)as.integer(any(grep("Leader",x))))
OLS_LS17_df$chiefMinister <- apply(OLS_LS17_df, 1, function(x)as.integer(any(grep("Chief",x))))
OLS_LS17_df$MLA <- apply(OLS_LS17_df, 1, function(x)as.integer(any(grep("Legislative",x))))
OLS_LS17_df$speaker <- apply(OLS_LS17_df, 1, function(x)as.integer(any(grep("Speaker",x))))
OLS_LS17_df$RS <- apply(OLS_LS17_df, 1, function(x)as.integer(any(grep("Rajya",x)))) 
OLS_LS17_df$sessions <- rowSums(sapply(OLS_LS17_df, function(i) grepl('Member.*Lok', i)))

OLS_LS17_df <- OLS_LS17_df[,-c(3:15)]

OLS_LS17_df_pv <- right_join(pv_all_df, OLS_LS17_df, by = "label")

OLS_LS17_df_pv <- OLS_LS17_df_pv[OLS_LS17_df_pv$date >= "2019-04-01" & OLS_LS17_df_pv$date <= "2021-12-31", ]

OLS_LS17_df_pv <- OLS_LS17_df_pv %>% 
  group_by(label) %>% 
  summarise(traffic_mean = mean(views, na.rm = T)) %>% 
  ungroup()

OLS_LS17_df_pv <- right_join(OLS_LS17_df_pv, OLS_LS17_df, by = "label")

OLS_LS17_df_pv <- left_join(OLS_LS17_df_pv , coreclaims_all_df, by = "label")

OLS_LS17_df_pv  <- OLS_LS17_df_pv [,-c(11,13,15,16)]

hist(OLS_LS17_df_pv $traffic_mean)

OLS_LS17_df_pv $traffic_mean <- log(OLS_LS17_df_pv $traffic_mean)

OLS_LS17_df_pv [is.na(OLS_LS17_df_pv ) | OLS_LS17_df_pv =="-Inf"] = NA

hist(OLS_LS17_df_pv $traffic_mean)

fit3 <- lm(traffic_mean ~ unionMinister +  leader + speaker + chiefMinister + RS + sessions + gender + age + party_affiliation, data = OLS_LS17_df_pv )

summary(fit3)

vif(fit3)

plot(fit3,1)

hist(fit3$residuals)
plot(fit3, 2)

plot(fit3, 3)
bptest(fit3)

plot(fit3, 5)

## Coefplot

coefplot(fit3, 
         coefficients = c("unionMinister","leader", "speaker", "chiefMinister", "RS", "sessions", "age", "gendermale"),
         newNames = c(unionMinister = "Union Minister***", leader = "Leader of House***", speaker = "Speaker of House", chiefMinister = "Chief Minister", RS = "Member of Rajya Sabha***", sessions = "Sessions Served***", age = "Age***", gendermale = "Gender(Male)**"),
         title = "OLS Estimates of Legislators Characteristics on Log Daily Pageviews (17th Lok Sabha)",
         xlab = "Regression Coefficient ",
         ylab = "Legislator Characteristics") +
  labs(
    caption = "OLS coefficients plotted with 50% and 95% confidence intervals. N = 573. Fit statistics: Adj. R2 = 0.3445. 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’"
  ) +
  theme(plot.caption = element_text(hjust = 0))  +
  theme(text = element_text(family = "Quattrocento Sans")) +
  theme(text = element_text(size = 19)) 


