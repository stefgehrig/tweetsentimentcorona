# Sentiment analysis of twitter data during coronavirus outbreak

## Overview

This repository presents a sentiment and emotion text analysis of US-based tweets during the coronavirus outbreak, with focus on xenophobia. **What are sentiments and emotions expressed in US-based tweets that address Asia / Asians? How does this change during the unfolding of the pandemic in 2020?** For background on xenophobia and negative stereotyping during the pandemic, see for example this [Wikipedia page](https://en.wikipedia.org/wiki/List_of_incidents_of_xenophobia_and_racism_related_to_the_2019%E2%80%9320_coronavirus_pandemic).

The repository contains R scripts to download data from twitter (a twitter developer account is required) and to run the analysis. RData files with dowloaded twitter data have been prepared (`tweets_asian_compl.RData`, `tweets_italian.RData`). Tweets were sampled at weekends from **December 28, 2019** to **April 19, 2020**, thereby spanning the initial global spread of the coronavirus after being first reported in China.


## Results

Using the AFINN lexicon to assign valence scores to single words (ranging from -5 to 5), a negative time trend in average sentiment per word is apparent during the time of the initial disease outbreak in the US. While the reported COVID-19 case count sharply increases, sentiment in tweets concering Asia drops on US twitter. After March 16th, the day Donald Trump tweeted about the "Chinese Virus" (see below), sentiment arrives as its observed minimum over the sampled time period:

<p align="center">
<img src="https://github.com/stefgehrig/tweetsentimentcorona/blob/master/plots/afinn_and_covid_cases_us.png" width="600">
</p>

<p align="center">
<img src="https://github.com/stefgehrig/tweetsentimentcorona/blob/master/images/trump_tweet.PNG" width="475" height="102">
</p>

Average scores cannot say anything about *what* people actually tweeted. So, which sentiment-laden words did occur most frequently, pooling tweets across the whole observation period and scoring them according to AFINN?

<p align="center">
<img src="https://github.com/stefgehrig/tweetsentimentcorona/blob/master/plots/sent_analysis_afinn_asia_freq.png" width="600">
</p>

This frequency analysis reveals that the topic of xenophobia is dominating much of the discourse about Asia at the time, with the words "*racist*"/"*racism*" contributing considerably to negative sentiment scores. Importantly, even though the mention of these words might not reveal xenophobia by the tweeter, they suggest presence of xenophobic behavior and speech in society. As an example, these are tweets from the weekend after Trump's "Chinese Virus" tweet (links and \@mentions removed, words with negative sentiment scores in bold):

- "Today in Manhattan I saw a woman absolutely **SCREAMING** her head off at an Asian stranger to 'go back to Japan or China or wherever', the man looked **terrified**. This **racism** is not new but it has sure as **hell** escalated, please call it out when you see it and don’t just walk by."

- "I normally don’t give a **shit** about his **fucking** **racism**, because we all know he’s a **goddamn** **racist**. However, it’s a dog whistle to his mouth breathing inbred backwards knuckle dragging **fuck** faces to **blame** and **attack** innocent Asian people. #RacistRussianCockHolster"

- "I had to step in when a lady was yelling at an Asian woman and her kids, 'It’s your fault we are going through this' Trump isn’t the cause of the **hate**; it was here way before him. BUT his narrative is fueling the **fire**."

In comparison to a pure lexicon lookup as used above, `sentimentr` is an algorithm which systematically takes into account syntactical context like valence shifters (e.g., *not*) or valence augmenters (e.g., *very*) on the sentence level. The results look similar, with sentiment decreasing over the period. The average sentiment per Tweet becomes negative only once, at the weekend after Trump's "Chinese Virus" tweet:

<p align="center">
<img src="https://github.com/stefgehrig/tweetsentimentcorona/blob/master/plots/sent_analysis_sentr_asia.png" width="600" height="475">
</p>

The [NRC lexicon](https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm) provides not only a dictionary for sentiments (positive, negative), but also for particular emotions (e.g., fear, joy, anger). In contrast to AFINN, a word is either in a specific sentiment/emotion class or not. Hence, the analysis is now based on proportions rather than average scores. The trend with decreasing positive sentiment and increasing negative sentiment during the manifestation of COVID-19 in the US is consistent with the previous analyses. Interestingly, negative words are more frequent than positive words only at the weekend after Trump's tweet (but this simple descriptive analysis can of course not establish it as a cause):

<p align="center">
<img src="https://github.com/stefgehrig/tweetsentimentcorona/blob/master/plots/sent_analysis_nrc_asia.png" width="800" height="500">
</p>

We also see that words associated with joy decrease over time, simultaneously with a slight increase in negative emotions like fear, anger and sadness. It is interesting to compare the sentiments and emotions in tweets concerning Asia with tweets concerning Italy, which arguably was hit by COVID-19 even harder than most countries in Asia during the sampling period. Tweets about Italy could potentially serve as a control group: While going through severe negative events that should affect sentiment expressed in US tweets, the country was never called out in public discourse as being to blame for COVID-19. Neither was it adressed in Trump's tweet. 

The trends in sentiment over time are surprisingly similar, although we do - as expected - not observe the positive spike before the Chinese New Year holidays and the negative spike after Trump's infamous tweet. The average frequency of negative words overall is also lower. Still, consistent with the severe health crisis unfolding in Italy at the time, we observe a decrease in the expression of joy and an increase in the expression of sadness, fear and anger over the sampling period among US tweeters:

<p align="center">
<img src="https://github.com/stefgehrig/tweetsentimentcorona/blob/master/plots/sent_analysis_nrc_italy.png" width="800" height="500">
</p>

Important caveats of the simple analysis of "virtual xenophonia" presented here. For example, (i) the difficulty to establish causality and (ii) measurement error. The latter should be huge, given that natural language was transformed into unidimensional metrics (although the scoring methods have previously been validated by other researchers). Further, a tweet with negative sentiment is of course not necessarily xenophobic or dealing with xenophobic events, but could simply contain words with negative valence, for example expressing sadness. Thus, more advanced methods of text analysis (e.g., neural-network based hate speech detection) and causal identification (e.g., synthetic controls, difference-in-difference approaches) would be needed for thorough analysis.
