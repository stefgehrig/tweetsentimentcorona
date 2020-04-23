####################################################################################
#### Twitter data analysis:                                                     ####
#### - Tweets containing "asia/n" during coronavirus outbreak early 2020        ####
#### - Sampling 500 tweets each weekend                                         ####
#### - Which sentiments and emotions are reflected in the tweets?               ####
#### - Using common scoring methods from text analysis                          ####
####                                                                            ####
#### by Stefan Gehrig (April 2020)                                              ####
####################################################################################

#empty environment
rm(list = ls(all.names = TRUE))

#load libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(tidytext)
library(textdata)
library(ggplot2)
library(stringr)
library(extrafont)
loadfonts(device = "win")

#import RData file with tweets in "tw_list"
load("data_twt_asian.RData")

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
              Date      = lubridate::ymd(as.Date((tail(created_at,1)))),
              Day       = weekdays(tail(created_at,1)),
              Top1laden = names(sort(table(word[afscore!=0]), decreasing = TRUE)[1]),
              Top2laden = names(sort(table(word[afscore!=0]), decreasing = TRUE)[2]),
              Top3laden = names(sort(table(word[afscore!=0]), decreasing = TRUE)[3]),
              Top4laden = names(sort(table(word[afscore!=0]), decreasing = TRUE)[4]),
              Top5laden = names(sort(table(word[afscore!=0]), decreasing = TRUE)[5]))}
dfa <- map_df(tw_lista, f7a)

#plotting
pa <- ggplot(dfa, 
            aes(x    = Date, 
                y    = Avg,
                ymin = Avg-SD/sqrt(Nwords)*qnorm(0.975),
                ymax = Avg+SD/sqrt(Nwords)*qnorm(0.975))) +
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
  scale_y_continuous(limits = c(-0.2,0.1)) +
  geom_hline(yintercept = 0, linetype = 2, size = 0.5) +
  theme(text=element_text(family="Segoe UI"))

png("sent_analysis_afinn.png", width = 2200, height = 1800, res = 350)
pa
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
              Date     = lubridate::ymd(as.Date((tail(created_at,1)))),
              Count    = length(word),
              Nwords   = originalN[1],
              Prop     = Count/Nwords,
              Top1     = names(sort(table(word), decreasing = TRUE)[1]),
              Top2     = names(sort(table(word), decreasing = TRUE)[2]),
              Top3     = names(sort(table(word), decreasing = TRUE)[3]),
              Top4     = names(sort(table(word), decreasing = TRUE)[4]),
              Top5     = names(sort(table(word), decreasing = TRUE)[5])) %>% 
    filter(!is.na(sentiment))}

dfb <- map_df(tw_listb, f7b)

#plotting
pb <- ggplot(dfb, 
             aes(x     = Date, 
                 y     = Prop*100,
                 ymin  = Prop*100 - 100*sqrt(Prop*(1-Prop)/Nwords)*qnorm(0.975),
                 ymax  = Prop*100 + 100*sqrt(Prop*(1-Prop)/Nwords)*qnorm(0.975),
                 col   = sentiment
             )) +
  geom_line(size = 0.8) + 
  geom_point(size = 2.5) +
  geom_ribbon(alpha = 0.25, aes(fill = sentiment)) +
  guides(col = FALSE) +
  theme_minimal(base_size = 14) +
  facet_wrap(~ Category, nrow = 1) +
  labs(title = "Sentiments and emotions in US tweets which mention\n'asia' or 'asian' over the coronavirus outbreak",
       subtitle = "Sample of 500 English-language tweets each weekend",
       x = "Date",
       y = "Percentage of all words (95% CI)",
       fill = "Emotion/\nSentiment",
       caption = "Emotions and sentiments of words following the NRC lexicon.
  Prior to analysis, stop words, @mentions and URLs were removed.") +
  theme(text=element_text(family="Segoe UI"))

png("sent_analysis_nrc.png", width = 3200, height = 1800, res = 385)
pb
dev.off()


# ...tbc

##################
#### C) Analysis via LIWC
##################

##################
#### D) sentimentr
##################