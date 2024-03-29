####################################################################################
#### Twitter data analysis:                                                     ####
#### - Tweets containing "asia/n" ["italy"/"italian"] during coronavirus        ####
####   outbreak early 2020                                                      ####
#### - Sampled 500 tweets each weekend                                          ####
#### - Which sentiments and emotions are reflected in the tweets?               ####
#### - Using common scoring methods and libraries from text analysis            ####
#### - Plotting of sentiments over time                                         ####
####                                                                            ####
#### by Stefan Gehrig (April/May 2020)                                          ####
####################################################################################

#load libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(purrr)
library(tidytext)
library(textdata)
library(ggplot2)
library(patchwork)
library(stringr)
library(scales)
library(sentimentr)
library(extrafont)
loadfonts(device = "win")

###########################
###########################
###### 'Asia'/'Asian' #####
###########################
###########################

#import RData file with tweets in "tw_list"
load("data/tweets_asian_compl.RData")

#remove non-required columns
f1 <- function(x) {
  x %>% select(text,created_at,mentions_screen_name)}
tw_list <- map(tw_list, f1)

#delete URLs from text and replace ampersands
f2 <- function(x){
  x %>%  
    mutate(text = gsub("http.*", "",  text)) %>% 
    mutate(text = gsub("https.*", "",  text)) %>% 
    mutate(text = gsub("&amp;", "&",  text))}
tw_list <- map(tw_list, f2)

#delete @mentions from text
f3 <- function(x){
  mentions <- unlist(x[,"mentions_screen_name"])[!is.na(unlist(x[,"mentions_screen_name"]))]
  mentions <- paste0("@", unique(mentions))
  x %>% 
    mutate(text = str_remove_all(text, paste(mentions, collapse="|")))}
tw_list <- map(tw_list, f3)

#remove punctuation, convert to lowercase, tokenize into words
f4 <- function(x){
  unnest_tokens(x, word, text, to_lower = TRUE)}
tw_list <- map(tw_list, f4)

#remove stop words
data("stop_words")
f5 <- function(x){
  anti_join(x, stop_words)}
tw_list <- map(tw_list, f5)

####################
#### A) Analysis via AFINN scores
####################

#load most recent sentiment scores of the AFINN lexicon, score all words according to AFINN, 0 otherwise
af <- read.table("afinn.txt", header = FALSE, sep = "\t", quote = "")
names(af) <- c("word","afscore")

f6a <- function(x){
  x %>% 
    left_join(., af, by = "word") %>%
    mutate(afscore = ifelse(is.na(afscore), 0, afscore))}
tw_lista <- map(tw_list, f6a)

#get dataframe with weekend date (by sunday) and mean AFINN score
f7a <- function(x){
  x %>% 
    summarise(Avg       = mean(afscore),
              SD        = sd(afscore),
              Nwords    = length(afscore),
              Nneutral  = length(afscore[afscore==0]),
              Date      = ymd(as.Date((tail(created_at,1)))),
              Day       = weekdays(tail(created_at,1)))}
dfa <- map_df(tw_lista, f7a)

#get dataframe with words with highest positive and negative influence
f8a <- function(x){
  x %>% select(word, afscore) %>% 
    filter(afscore != 0) %>% 
    mutate(positive = afscore > 0)}
df_freq <- map_df(tw_lista, f8a)

#plotting time series
plot_afinn <- ggplot(dfa, 
                     aes(x    = Date, 
                         y    = Avg,
                         ymin = Avg-SD/sqrt(Nwords)*qnorm(0.975),
                         ymax = Avg+SD/sqrt(Nwords)*qnorm(0.975)
                         )) +
  geom_line(size= 0.8) + 
  geom_point(size= 2.5) +
  geom_ribbon(alpha = 0.2) +
  theme_minimal(base_size = 14) +
  labs(title = "Sentiments in US tweets which mention\n'asia' or 'asian' over the coronavirus outbreak",
       subtitle = "Sample of 500 English-language tweets each weekend",
       x = "Date",
       y = "Avg. sentiment per word (95% CI)",
       color = "Mentioned",
       caption = "Sentiment scoring following the AFINN lexicon.
       Prior to analysis, stop words, @mentions and URLs were removed."
  ) +
  scale_y_continuous(limits = c(-0.2,0.12)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B %d, %Y") +
  geom_vline(xintercept = ymd("2020-03-16"), linetype = 2, size = 0.4) +
  geom_vline(xintercept = ymd("2020-01-25"), linetype = 2, size = 0.4) +
  annotate(geom = "label",
           x = c(ymd("2020-01-25"),ymd("2020-03-16")), 
           y = 0.1, size = 3,
           label = c("Chinese\nNew Year", "Trump tweets\n'Chinese Virus'"),
           fill = "ivory") +
  theme(text=element_text(family="Segoe UI"),
        panel.grid.minor = element_blank())

png("plots/sent_analysis_afinn_asia.png", width = 2300, height = 1800, res = 350)
print(plot_afinn)
dev.off()

#plotting frequencies of words
set.seed(110)
df_freq %>% 
  group_by(word) %>% 
  summarise(N = n(),
            afscore = first(afscore), 
            positive = first(positive),
            .groups = "drop") %>% 
  group_by(positive) %>% 
  mutate(rank = rank(-N, ties.method = "random")) %>%
  ungroup %>% 
  filter(rank<=15) -> tmp

p_freq <- ggplot() + 
  geom_histogram(data = filter(tmp, positive == TRUE), 
                 aes(x = rank, y = -N, alpha = abs(afscore)), 
                 stat = "identity", fill = "steelblue", width = 0.6, show.legend = FALSE) + 
  geom_histogram(data = filter(tmp, positive == FALSE), 
                 aes(x = rank, y = N, alpha = abs(afscore)), 
                 stat = "identity", fill = "coral", width = 0.6) + 
  geom_text(data = filter(tmp, positive == FALSE), 
            aes(x = rank, y = N + 40, label = word),
            angle = 45, col = "grey30") + 
  geom_text(data = filter(tmp, positive == TRUE), 
            aes(x = rank, y = - N - 40, label = word),
            angle = 315, col = "grey40") + 
  geom_hline(yintercept = 0, col = "grey40") + 
  geom_text(aes(x = 10, y = 220, label = "Negative sentiment"), size = 5, col = "grey20") + 
  geom_text(aes(x = 10, y = -220, label = "Positive sentiment"), size = 5, col = "grey20") + 
  scale_y_continuous(limits = c(-290,290), labels = abs) + 
  scale_x_continuous(breaks = seq(1,15,1), minor_breaks = seq(1,15,1)) + 
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal(base_size = 14) +
  theme(text=element_text(family="Segoe UI"),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        panel.grid.major.x = element_blank()) +
  labs(title = "Most frequent words of negative and positive sentiment in US tweets\nwhich mention 'asia' or 'asian' over the coronavirus outbreak",
       subtitle = "Sample of 500 English-language tweets each weekend",
       y = "N",
       x = "Rank",
       alpha = "Absolute AFINN score\n(opacity gradient is same for blue)", 
       caption = "Sentiment scoring following the AFINN lexicon.
       Prior to analysis, stop words, @mentions and URLs were removed.")

png("plots/sent_analysis_afinn_asia_freq.png", width = 2800, height = 2300, res = 350)
print(p_freq)
dev.off()

##################
#### B) Analysis via NRC scores
##################

#match with words from NRC lexicon, allow non- and multiple matches
f6b <- function(x){
  x %>% 
    mutate(originalN = nrow(x)) %>% 
    left_join(., get_sentiments(("nrc")), by = "word")}

tw_listb <- map(tw_list, f6b)

#count sentiments and retrieve original N (before joining multiplications) to get proportions
f7b <- function(x){
  x %>% 
    group_by(sentiment) %>% 
    summarise(Category = factor(ifelse(sentiment[1] %in% c("positive", "negative"), 
                                       "Sentiment (positive vs. negative)", "Emotion"),
                                ordered = TRUE, levels = c("Sentiment (positive vs. negative)", "Emotion")),
              Date     = ymd(as.Date((tail(created_at,1)))),
              Count    = length(word),
              Nwords   = originalN[1],
              Prop     = Count/Nwords) %>% 
    filter(!is.na(sentiment))}

dfb <- map_df(tw_listb, f7b)

#remove some sentiments before plotting
dfb %>% 
  filter(!sentiment %in% c("anticipation",
                           "surprise",
                           "trust",
                           "disgust")) -> dfb

#plotting
plot_nrc <- ggplot(dfb, 
                   aes(x     = Date, 
                   y     = Prop*100,
                   ymin  = Prop*100 - 100*sqrt(Prop*(1-Prop)/Nwords)*qnorm(0.975),
                   ymax  = Prop*100 + 100*sqrt(Prop*(1-Prop)/Nwords)*qnorm(0.975),
                   col   = sentiment
                   )) +
  geom_line(size = 0.8) + 
  geom_point(size = 2.5) +
  geom_ribbon(alpha = 0.25, aes(fill = sentiment)) +
  guides(col = "none") +
  theme_minimal(base_size = 14) +
  scale_y_continuous(limits = c(0,12.5)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B %d, %Y") +
  facet_wrap(~ Category, nrow = 1) +
  labs(title = "Sentiments and emotions in US tweets which mention\n'asia' or 'asian' over the coronavirus outbreak",
       subtitle = "Sample of 500 English-language tweets each weekend",
       x = "Date",
       y = "Percentage of all words (95% CI)",
       fill = "Emotion/\nSentiment",
       caption = "Emotions and sentiments of words following the NRC lexicon.
  Prior to analysis, stop words, @mentions and URLs were removed.") +
  geom_vline(xintercept = ymd("2020-03-16"), linetype = 2, size = 0.4) +
  geom_vline(xintercept = ymd("2020-01-25"), linetype = 2, size = 0.4) +
  theme(text=element_text(family="Segoe UI"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 60, vjust = 1/2))

png("plots/sent_analysis_nrc_asia.png", width = 3200, height = 2000, res = 390)
print(plot_nrc)
dev.off()

##################
#### C) sentimentr (https://github.com/trinker/sentimentr)
##################

#import RData file with tweets in "tw_list"
load("data/tweets_asian_compl.RData")

#remove non-required columns
f1 <- function(x) {
  x %>% select(text,created_at,mentions_screen_name)}
tw_list <- map(tw_list, f1)

#delete URLs from text and replace ampersands
f2 <- function(x){
  x %>%  
    mutate(text = gsub("http.*", "",  text)) %>% 
    mutate(text = gsub("https.*", "",  text)) %>% 
    mutate(text = gsub("&amp;", "&",  text))}

#delete @mentions from text
f3 <- function(x){
  mentions <- unlist(x[,"mentions_screen_name"])[!is.na(unlist(x[,"mentions_screen_name"]))]
  mentions <- paste0("@", unique(mentions))
  x %>% 
    mutate(text = str_remove_all(text, paste(mentions, collapse="|")))}
tw_list <- map(tw_list, f3)


#assign sentiment scores by sentence and group by tweet
f4sentR <- function(x){
  mytext          <- get_sentences(x$text)
  sentr_by_tweet  <- sentiment_by(mytext)
  x %>% 
    mutate(ave_sentiment = sentr_by_tweet$ave_sentiment)
}

tw_list <- map(tw_list, f4sentR)

#get dataframe with weekend date (by sunday) and mean sentimentr score
f5sentR <- function(x){
  x %>% 
    summarise(Avg       = mean(ave_sentiment),
              SD        = sd(ave_sentiment),
              Ntweets   = length(ave_sentiment),
              Date      = ymd(as.Date((tail(created_at,1)))),
              Day       = weekdays(tail(created_at,1)))}

df <- map_df(tw_list, f5sentR)

#plotting
plot_sentr <- ggplot(df, 
                     aes(x    = Date, 
                         y    = Avg,
                         ymin = Avg-SD/sqrt(Ntweets)*qnorm(0.975),
                         ymax = Avg+SD/sqrt(Ntweets)*qnorm(0.975)
                         )) +
  geom_line(size= 0.8) + 
  geom_point(size= 2.5) +
  geom_ribbon(alpha = 0.2) +
  theme_minimal(base_size = 14) +
  labs(title = "Sentiments in US tweets which mention\n'asia' or 'asian' over the coronavirus outbreak",
       subtitle = "Sample of 500 English-language tweets each weekend",
       x = "Date",
       y = "Avg. sentiment per tweet (95% CI)",
       color = "Mentioned",
       caption = "Sentiment scoring following the sentimentR algorithm.
       Prior to analysis, @mentions and URLs were removed."
  ) +
  scale_y_continuous(limits = c(-0.1,0.15)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B %d, %Y") +
  geom_vline(xintercept = ymd("2020-03-16"), linetype = 2, size = 0.4) +
  geom_vline(xintercept = ymd("2020-01-25"), linetype = 2, size = 0.4) +
  annotate(geom = "label",
           x = c(ymd("2020-01-25"),ymd("2020-03-16")), 
           y = 0.13, size = 3,
           label = c("Chinese\nNew Year", "Trump tweets\n'Chinese Virus'"),
           fill = "ivory") +
  theme(text=element_text(family="Segoe UI"),
        panel.grid.minor = element_blank())

png("plots/sent_analysis_sentr_asia.png", width = 2300, height = 1800, res = 350)
print(plot_sentr)
dev.off()

#############################
#############################
###### 'Italy'/'Italian #####
#############################
#############################

#import RData file with tweets in "tw_list"
load("data/tweets_italian.RData")
tw_list  <- Filter(length, tw_list)

#remove non-required columns
f1 <- function(x) {
  x %>% select(text,created_at,mentions_screen_name)}
tw_list <- map(tw_list, f1)

#delete URLs from text and replace ampersands
f2 <- function(x){
  x %>%  
    mutate(text = gsub("http.*", "",  text)) %>% 
    mutate(text = gsub("https.*", "",  text)) %>% 
    mutate(text = gsub("&amp;", "&",  text))}
tw_list <- map(tw_list, f2)

#delete @mentions from text
f3 <- function(x){
  mentions <- unlist(x[,"mentions_screen_name"])[!is.na(unlist(x[,"mentions_screen_name"]))]
  mentions <- paste0("@", unique(mentions))
  x %>% 
    mutate(text = str_remove_all(text, paste(mentions, collapse="|")))}
tw_list <- map(tw_list, f3)

#remove punctuation, convert to lowercase, tokenize into words
f4 <- function(x){
  unnest_tokens(x, word, text, to_lower = TRUE)}
tw_list <- map(tw_list, f4)

#remove stop words
data("stop_words")
f5 <- function(x){
  anti_join(x, stop_words)}
tw_list <- map(tw_list, f5)

#match with words from NRC lexicon, allow non- and multiple matches
f6b <- function(x){
  x %>% 
    mutate(originalN = nrow(x)) %>% 
    left_join(., get_sentiments(("nrc")), by = "word")}

tw_listb <- map(tw_list, f6b)

#count sentiments and retrieve original N (before joining multiplications) to get proportions
f7b <- function(x){
  x %>% 
    group_by(sentiment) %>% 
    summarise(Category = factor(ifelse(sentiment[1] %in% c("positive", "negative"), 
                                       "Sentiment (positive vs. negative)", "Emotion"),
                                ordered = TRUE, levels = c("Sentiment (positive vs. negative)", "Emotion")),
              Date     = ymd(as.Date((tail(created_at,1)))),
              Count    = length(word),
              Nwords   = originalN[1],
              Prop     = Count/Nwords) %>% 
    filter(!is.na(sentiment))}

dfb <- map_df(tw_listb, f7b)

#remove some sentiments before plotting
dfb %>% 
  filter(!sentiment %in% c("anticipation",
                           "surprise",
                           "trust",
                           "disgust")) -> dfb

#plotting
plot_nrc_ital <- ggplot(dfb, 
                        aes(x     = Date, 
                            y     = Prop*100,
                            ymin  = Prop*100 - 100*sqrt(Prop*(1-Prop)/Nwords)*qnorm(0.975),
                            ymax  = Prop*100 + 100*sqrt(Prop*(1-Prop)/Nwords)*qnorm(0.975),
                            col   = sentiment
                            )) +
  geom_line(size = 0.8) + 
  geom_point(size = 2.5) +
  geom_ribbon(alpha = 0.25, aes(fill = sentiment)) +
  guides(col = "none") +
  theme_minimal(base_size = 14) +
  facet_wrap(~ Category, nrow = 1) +
  scale_y_continuous(limits = c(0,12.5)) + 
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B %d, %Y") +
  labs(title = "Sentiments and emotions in US tweets which mention\n'italy' or 'italian' over the coronavirus outbreak",
       subtitle = "Sample of 500 English-language tweets each weekend",
       x = "Date",
       y = "Percentage of all words (95% CI)",
       fill = "Emotion/\nSentiment",
       caption = "Emotions and sentiments of words following the NRC lexicon.
  Prior to analysis, stop words, @mentions and URLs were removed.") +
  geom_vline(xintercept = ymd("2020-03-16"), linetype = 2, size = 0.4) +
  geom_vline(xintercept = ymd("2020-01-25"), linetype = 2, size = 0.4) +
  theme(text=element_text(family="Segoe UI"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 60, vjust = 1/2))

png("plots/sent_analysis_nrc_italy.png", width = 3200, height = 2000, res = 390)
print(plot_nrc_ital)
dev.off()


#############################
#############################
###### plot covid cases #####
#############################
#############################

#import covid data
df_cov <- read_csv("data/us_covid_data.csv")

plot_cov <- df_cov %>% 
  mutate(Date = mdy(date)) %>% 
  filter(Date <= "2020-04-19") %>% 
  ggplot(aes(x    = Date, 
             y    = cases)) +
  geom_line(size= 0.8) + 
  theme_minimal(base_size = 14) +
  labs(title = "Cumulative count of coronvirus cases in the US",
       x = "Date",
       y = "Cumulative number of cases",
       color = "Mentioned",
       caption = "Data were retrieved from github.com/nytimes/covid-19-data."
  ) +
  scale_y_continuous(trans = "log",
                     labels = trans_format("log10", math_format(10^.x)),
                     breaks = trans_breaks("log10", function(x) 10^x)) +
  scale_x_date(limits = ymd(c("2019-12-28", NA)),
               date_breaks = "1 month",
               date_labels = "%B %d, %Y") +
  theme(text=element_text(family="Segoe UI"),
        panel.grid.minor = element_blank())

# plot covid cases
png("plots/covid_cases_us.png", width = 2300, height = 1800, res = 350)
print(plot_cov)
dev.off()

# plot afinn and covid cases in one figure
png("plots/afinn_and_covid_cases_us.png", width = 2600, height = 3600, res = 400)
plot_afinn/plot_cov
dev.off()