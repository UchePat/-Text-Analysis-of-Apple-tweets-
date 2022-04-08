
# Read d file
mydata <- read.table("apple.txt", header = T, sep = ',')  # using read.table since it is a .txt file
str(mydata) 


# Build Corpus. Corpus is a collection of documents so each tweet will be treated as a document
library(tm)        # tm means text mining

?iconv
mycorpus <- iconv(mydata$text, to = "UTF-8")   # we are only using d text column in d dataset becuz it contains d tweets and we need to convert d text column to UTF-8 format

mycorpus <- Corpus(VectorSource(mycorpus))
inspect(mycorpus[1:5])      # displays d 1st 5 tweets(which is now corpuses)


# Cleaning d tweets/text corpuses
mycorpus <- tm_map(mycorpus, tolower)  # changes all to lower-case
inspect(mycorpus[1:5])


mycorpus <- tm_map(mycorpus, removePunctuation)  # removes puntuation marks
inspect(mycorpus[1:5])


mycorpus <- tm_map(mycorpus, removeNumbers)
inspect(mycorpus[1:5])


# stopwords('english')   # displays all stopwords that will be removed
ourcorpus <- tm_map(mycorpus, removeWords, stopwords('english'))  # removes all stopwords
inspect(ourcorpus[1:5])


removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)   # dis function is used to remove URLs from d corpuses
ourcorpus <- tm_map(ourcorpus, content_transformer(removeURL))  # removes URLs
inspect(ourcorpus[1:5])


#-----------------------------------------------------------------------------------------------
 # dont run dis code. Just move on to stripWhitespace and Term document matrix below. After running Term Document Matrix below, u will see d word that appears in all stated columns(in dis case it is aapl) den come back and run dis code to remove it and den re-run stripWhitespace and Term Document Matrix again 
ourcorpus <- tm_map(ourcorpus, removeWords, c('aapl', 'apple'))   # we will remove aapl and apple words. we are removing apple word becuz all d tweets is about apple

#---------------------------------------------------------------------------------------------------------------------------------
# dont run dis code. Only run dis code after u have plotted d bar chart and you wanto stem/join similar words into 1 word. as such we replace d word stocks with stock. den re-run stripWhitespace and Term Document Matrix and continue.... 
ourcorpus <- tm_map(ourcorpus, gsub, 
                   pattern = 'stocks', 
                   replacement = 'stock')   # as such we are stemming/joining d words- stocks and stock into 1 word stock
#-------------------------------------------------------------------------------------------------

ourcorpus <- tm_map(ourcorpus, stripWhitespace)   # when u remove a word it will be replaced by a space. dis will remove such white space
inspect(ourcorpus[1:5])



# Term document matrix- dis converts d unstructured data into a matrix(rows and columns)
tdm <- TermDocumentMatrix(ourcorpus)
tdm      

tdm <- as.matrix(tdm)
tdm[1:10, 1:20]   # displays 1st 10 rows and 1st 20 columns. we see that aapl appears in all columns (becuz it is present in all d tweets). we need to remove it by going back to Cleaning text/tweets corpuses above


# finding out how often a word appears in d dataset 
w <- rowSums(tdm)    # using rowSums() to find how often a word appears in d dataet
View(w)   # d word is under name column, while its frequency is under value column 

w <- subset(w, w >= 25)   # since d words are too much, lets only look at words that appear >= 25 times
View(w)

# creating a Bar plot
barplot(w, las = 2, col = rainbow(50))  # las = 2 will make d x-axis values to be vertical not horizontal. using rainbow colors
# displays a bar chart showing d frequency of each word. Check to see if there are any similar words among x-axis values- we can see dat stocks and stock are very similar words so let us join/stem them into 1 word(ie we replace d word stocks with stock) by going back to Cleaning text/tweets corpuses above
        
        


# creating a Word-Cloud
library(wordcloud)

w <- sort(rowSums(tdm), decreasing = TRUE)  # we sort in decreasing order

set.seed(222)

wordcloud(words = names(w),    # creates a wordcloud. a bigger word size indicates a bigger frequency in d dataset
          freq = w)


wordcloud(words = names(w),
          freq = w,
          max.words = 150)   # displays only 150 words in d wordcloud


wordcloud(words = names(w),
          freq = w,
          max.words = 200,
          random.order = F,
          min.freq = 5,                    # using min.freq = 5 will add any word that has a frequency of 5 and above to d wordcloud
          colors = brewer.pal(8, 'Dark2'),  # add colors to d words
          scale = c(5, 0.3),               # scale/size range of all words is btw 0.3 and 5(5/7 is scale of biggest word while 0.3 is scale of smallest word)
          rot.per = 0.5)            # rotation percentage will rotate 50% of d words. use 30% or 50% or 70%



# library(devtools)
# devtools::install_github("Lchiffon/wordcloud2")

library(wordcloud2)

w <- data.frame(names(w), w)

colnames(w) <- c('word', 'freq')  # these are d column headers
head(w)


wordcloud2(w,
           size = 0.7,
           shape = 'triangle',   # displays d words in a triangle shape. 
           rotateRatio = 0.5,
           minSize = 1)


wordcloud2(w,
           size = 0.7,
           shape = 'triangle-forward')   # displays d words in a RHS pointing-arrow shape. 
           

wordcloud2(w,
           size = 0.5,
           shape = 'circle',   # displays d words in a circle shape. 
           rotateRatio = 0.5,
           minSize = 1)


wordcloud2(w,
           size = 0.7,
           shape = 'diamond')   # displays d words in a diamond shape. 
           

wordcloud2(w,
           size = 0.5,
           shape = 'star',   # displays d words in a star shape. 
           rotateRatio = 0.5,
           minSize = 1)


wordcloud2(w,
           size = 1.2,
           color = 'random-light',   # displays d words in diff random color with a colored background 
           backgroundColor = '#FFFF99')


wordcloud2(w,
           size = 1.2,
           color = rep_len(c("green", "blue", "red"), nrow(w)))   # displays d words using d 3 stated colors only 


wordcloud2(w,
           size = 1.2,
           color = 'random-dark',        # displays d words in diff random color with a colored background 
           backgroundColor = '#FFFF99')



# using letterCloud() to display words inside a stated letter or words
?letterCloud

letterCloud(w, word="A", size = 1)  # if d lettercloud does not show up in Viewer tab, click Refresh Viewer icon in Viewer tab once or twice. displays the words in d shape of A. 
         

letterCloud(w,
            word = "a",
            wordSize = 1)    # if d lettercloud does not show up in Viewer tab, click Refresh Viewer icon in Viewer tab once or twice


letterCloud(w,
            word = "apple",
            wordSize = 1)     # if d lettercloud does not show up in Viewer tab, click Refresh Viewer icon in Viewer tab once or twice








