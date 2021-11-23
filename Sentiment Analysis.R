library(tm) ## text mining
library(SnowballC) ## stemming
library(wordcloud) ## wordcloud
library(RColorBrewer) ## color palettes
library(syuzhet) ## for sentiment analysis
library(ggplot2) ## for plotting graphs
text=read.csv("Tweets.csv",header = TRUE) ## load the data
text1=iconv(text$text) ## select the data
textdoc=Corpus(VectorSource(text1)) ## load the selected data to corpus
textdoc=tm_map(textdoc,tolower) ## to lower
textdoc=tm_map(textdoc,removePunctuation) ## removing punctuation
textdoc=tm_map(textdoc,removeNumbers) ## remove numbers
textdoc=tm_map(textdoc,removeWords,stopwords('english')) ## remove common english stopwors
textdoc=tm_map(textdoc,removeWords,c('virginamerica')) ## remove customed stopwords
textdoc=tm_map(textdoc,stripWhitespace) ## remove whitespaces
textdoc=tm_map(textdoc,stemDocument) ## stemming the document

textdoc_tdm=TermDocumentMatrix(textdoc) ## term document matrix
tdm_m=as.matrix(textdoc_tdm) ## create a matrix

dtm=sort(rowSums(tdm_m),decreasing = TRUE) ## sorting by decreasing value of frequency
dtm_d=data.frame(word=names(dtm),freq=dtm) ## create a data frame 

head(dtm_d) ## display the first 5 observations

wordcloud(dtm_d$word,freq = dtm_d$freq,min.freq = 1,max.words = 1000,random.order = FALSE,rot.per = 0.40,
          colors = brewer.pal(8,"Dark2")) ## create a wordcloud

sentiment=get_sentiment(textdoc,method = "syuzhet") ## sentiment classification (syuzhet)
head(sentiment)
summary(sentiment)

bing=get_sentiment(textdoc,method = "bing") ## bing method
head(bing)
summary(bing)

afinn=get_sentiment(textdoc,method = "afinn") ## afinn method
head(afinn)
summary(afinn)

emotion=get_nrc_sentiment(text1) ## emotion classification
head(emotion)

t=data.frame(emotion) 
barplot(sort(colSums(prop.table(emotion))),horiz = TRUE,cex.names = 0.7,las=1,
        main = "Emotions in Text",xlab = "Percentage") ## visualization
