# Sentiment analysis of twitter data during coronavirus outbreak

## Overview

This repository presnets a simple sentiment and emotion analysis of tweets during the coronavirus outbreak, with focus on xenophobia. **What are sentiments and emotions expressed in tweets that address Asia / Asians? How does this change during the unfolding of the pandemic in 2020?**  For background on xenophobia and racism during the pandemic, see for example this [Wikipedia page](https://en.wikipedia.org/wiki/List_of_incidents_of_xenophobia_and_racism_related_to_the_2019%E2%80%9320_coronavirus_pandemic).

The repository contains R scripts to download data from twitter (a twitter developer account is required) and run the analysis. RData files with dowloaded twitter data are prepared. It also contains the AFINN lexicon for sentiment scoring as a text file (the most recent version can always be retrieved from the developer under https://github.com/fnielsen/afinn) and plots which result from the analysis. I also use the package [sentimentR](https://github.com/trinker/sentimentr).

## Results

![image](https://github.com/stefgehrig/tweetsentimentcorona/blob/master/sent_analysis_afinn_asia.png | width=40)
