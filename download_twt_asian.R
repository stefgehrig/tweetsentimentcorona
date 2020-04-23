####################################################################################
#### Twitter data download:                                                     ####
#### - Tweets containing "asia/n" during coronavirus outbreak early 2020        ####
#### - Sampling 500 tweets each weekend                                         ####
####                                                                            ####
#### by Stefan Gehrig (April 2020)                                              ####
####################################################################################

#empty environment
rm(list = ls(all.names = TRUE))

#load libraries
library(rtweet)

#access twitter via API
create_token(app             = "", #NOTE: personal data removed here
             consumer_key    = "", #NOTE: personal data removed here
             consumer_secret = "", #NOTE: personal data removed here
             access_token    = "", #NOTE: personal data removed here
             access_secret   = "") #NOTE: personal data removed here

#get weekend dates and create weekend list
x <- seq(as.Date("2019-12-28"), as.Date("2020-03-31"), by = 1)
wends <- x[weekdays(x) %in% c("Saturday", "Sunday")]
wends_list <- split(wends, ceiling(seq_along(wends)/2))

#create empty lists for twitter data for each weekend
tw_list <- list(NA)

#loop through all weekend list elements for search term "asia" OR "asian" (US and english language only)
#writing results into twitter data list
for (i in seq(1, length(wends_list), 1)){
  tw_list[[i]] <- search_fullarchive(q = "asia lang:en place_country:us OR asian lang:en place_country:us", 
                                     n = 500,
                                     fromDate = wends_list[[i]][1],
                                     toDate   = wends_list[[i]][2],
                                     env_name = "extaccess2")
}

#export RData file
save(tw_list, file = "data_twt_asian.RData")