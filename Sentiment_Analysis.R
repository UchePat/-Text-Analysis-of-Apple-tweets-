# creating Sentiment analysis on tweets in a document file
install.packages("syuzhet")

library(syuzhet)         # dis is d Sentiment Analysis package
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)


# Read d file
mydata <- read.table("apple.txt", header = T, sep = ',')  # using read.table since it is a .txt file

ourdata <- iconv(mydata$text, to = "UTF-8")   # we are only using d text column in d dataset becuz it contains d tweets and we need to convert d text column to UTF-8 format


# Obtain sentiment scores for each tweet
s <- get_nrc_sentiment(ourdata)  # dis will classify d all d indexed tweets(each tweet will have an index number) using value 0,1,2,3...  among multiple sentiment column headers that best fits/describes d tweet
head(s)         # displays numerical values- 0,1,2,3 in multiple sentiment column headers for each tweet (index) that has dat column header sentiment in dat tweet(ie tweet (index) 2 has a positive sentiment in d tweet that why it has value 1 in positive column but has no anger or fear or joy sentiment dats why anger, fear and joy columns have value 0)
# in tweet (index) 4, dere are anger sentiment, disgust sentiment, sadness sentiment and negative sentiment in d tweet dats why dose columns have  value > 0 but no positive or anticipation sentiment dats why there is value 0
ourdata[4]     # lets see d 4th tweet that contains all dis sentiments
ourdata[1]

get_nrc_sentiment('delay')  # dis will classify d stated word (taken from a tweet) using value 0 and 1 among d multiple sentiment column headers that best fits/describes d stated word
get_nrc_sentiment('ugly')


# Create a Bar plot using d sentiment scores
barplot(colSums(s),
        las = 2,             # las = 2 will make d x-axis values to be vertical not horizontal.
        col = rainbow(10),    # since there are 10 columns in d sentiment score
        ylab = 'Count',
        main = 'Sentiment Scores for Apple Tweets')   # we can see negative sentiment is very high in d tweets



we <- 100*colSums(s)/sum(s)   # displays d sentiment scores by % value

barplot(we,   
        las = 2,             # las = 2 will make d x-axis values to be vertical not horizontal.
        col = rainbow(10),    # since there are 10 columns in d sentiment score
        ylab = 'Count',
        main = 'Sentiment Scores for Apple Tweets')   # we can see negative sentiment is very high in d tweets





#------------------------------------------------------------------------------------------------------------------------------------





# Lets read another file
mydata <- read.table("apple2.txt", header = T, sep = ',')  # using read.table since it is a .txt file

ourdata <- iconv(mydata$text, to = "UTF-8")   # we are only using d text column in d dataset becuz it contains d tweets and we need to convert d text column to UTF-8 format


# Obtain sentiment scores for each tweet
s <- get_nrc_sentiment(ourdata)  # dis will classify d all d indexed tweets using value 0,1,2,3...  among multiple sentiment column headers that best fits/describes d tweet
head(s)         # displays numerical values- 0,1,2,3 in multiple sentiment column headers for each tweet (index) that has dat column header sentiment in dat tweet(ie tweet (index) 2 has a positive sentiment in d tweet that why it has value 1 in positive column but has no anger or fear or joy sentiment dats why anger, fear and joy columns have value 0)
# in tweet (index) 4, dere are anger sentiment, disgust sentiment, sadness sentiment and negative sentiment in d tweet dats why dose columns have  value > 0 but no positive or anticipation sentiment dats why there is value 0
ourdata[4]     # lets see d 4th tweet that contains all dis sentiments
ourdata[1]

get_nrc_sentiment('delay')  # dis will classify d stated word using value 0 and 1 among d multiple sentiment column headers that best fits/describes d stated word
get_nrc_sentiment('ugly')


# Bar plot using d sentiment scores
barplot(colSums(s),
        las = 2,             # las = 2 will make d x-axis values to be vertical not horizontal.
        col = rainbow(10),    # since there are 10 columns in d sentiment score
        ylab = 'Count',
        main = 'Sentiment Scores for Apple Tweets')  # we can see positive sentiment is very high in d tweets
