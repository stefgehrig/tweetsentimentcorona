# Sentiment analysis of twitter data during coronavirus outbreak

## Overview

This repository presnets a simple sentiment and emotion analysis of US-based tweets during the coronavirus outbreak, with focus on xenophobia. **What are sentiments and emotions expressed in US-based tweets that address Asia / Asians? How does this change during the unfolding of the pandemic in 2020?** For background on xenophobia and racism during the pandemic, see for example this [Wikipedia page](https://en.wikipedia.org/wiki/List_of_incidents_of_xenophobia_and_racism_related_to_the_2019%E2%80%9320_coronavirus_pandemic).

The repository contains R scripts to download data from twitter (a twitter developer account is required) and to run the analysis. RData files with dowloaded twitter data have been prepared (`tweets_asian_compl.RData`, `tweets_italian.RData`). Tweets were sampled at weekends from **December 28, 2019** to **April 19, 2020** (N = 500 per weekend), thereby spanning the initial global spread of the coronavirus after being first reported in China.

The repository also contains the AFINN lexicon for sentiment scoring as a text file (the most recent version can always be retrieved from the developer under https://github.com/fnielsen/afinn), as well as plots produced in the analysis. For text analysis, I use the packages [sentimentr](https://github.com/trinker/sentimentr), [textdata](https://cran.r-project.org/web/packages/textdata/index.html) and [tidytext](https://cran.r-project.org/web/packages/tidytext/index.html).

## Results

Using the AFINN lexicon to assign valence scores to single words (ranging from -5 to 5), a negative trend in average sentiment per word is visible, particularly during the time when COVID-19 was reported to start spreading in the US (March 2020). March 16th, the day Donald Trump tweeted about the "Chinese Virus" (see below), is right before the observed minimum in expressed sentiment over the sampled time period:

<img src="https://github.com/stefgehrig/tweetsentimentcorona/blob/master/plots/sent_analysis_afinn_asia.png" width="550" height="450">

<img src="https://github.com/stefgehrig/tweetsentimentcorona/blob/master/images/trump_tweet.PNG" width="475" height="102">

In comparison to a pure lexicon lookup as used above, `sentimentr` is an algorithm which takes into account syntactical context like valence shifters (e.g., *not*) or valence augmenters (e.g., *very*) on the sentence level. The results look similar, although the average sentiment per Tweet becomes negative only once, at the weekend after Trump's "Chinese Virus" tweet:

<img src="https://github.com/stefgehrig/tweetsentimentcorona/blob/master/plots/sent_analysis_sentr_asia.png" width="550" height="450">

An example tweet from that weekend:



The [NRC lexicon](https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm) provides not only a dictionary for sentiments (positive, negative), but also for particular emotions (e.g., fear, joy). In contrast to AFINN, a word is either in a specific sentiment/emotion class or not. Hence, the analysis is now based on proportions rather than average scores. The trend with decreasing positive sentiment and increasing negative sentiment during the manifestation of COVID-19 in the US is consistent with the previous analyses. Interestingly, negative words are more frequent than positive words only at the weekend after Trump's tweet (but this simple descriptive analysis can of course not establish it as a cause):

<img src="https://github.com/stefgehrig/tweetsentimentcorona/blob/master/plots/sent_analysis_nrc_asia.png" width="800" height="450">

We also see that words associated with joy decrease over time, simultaneously with a slight increase in negative emotions like fear, anger and sadness. It is interesting to compare the sentiments and emotions in tweets concerning Asia with tweets concerning Italy, which arguably was hit by COVID-19 even harder than most countries in Asia during the sampling time. The trends in sentiment over time are indeed surprisingly similar, although we do -- as expected -- not observe the positive spike before the Chinese New Year holidays and the negative spike after Trump's infamous tweet:

<img src="https://github.com/stefgehrig/tweetsentimentcorona/blob/master/plots/sent_analysis_nrc_italy.png" width="800" height="450">

Two important caveats of the simple analysis of "virtual xenophonia" presented here are (i) the difficulty to establish causality and (ii) measurement error. The latter should be huge, given that natural language was transformed into unidimensional metrics -- despite preivous validations of the methods, also in twitter data. Further, a tweet with negative sentiment is of course not necessarily xenophobic, but could simply contain words with negative valence because it expresses sadness or fear. Thus, more advanced methods of text analysis (e.g., neural-network based hate speech detectors) and causal identification (e.g., synthetic controls, difference-in-difference) would be needed for thorough analysis.
