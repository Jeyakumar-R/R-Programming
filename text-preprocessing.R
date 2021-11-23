library(tm)
library(SnowballC)
library(textstem)

#Text Pre-processing
m = data.frame(c("Machines   @? understanding / 489language fascinates    me, ",
                 "pandas drop_duplicates to drop3 the duplicate rows", 
                 "Word Clouds make 90it easy to identify #keyword90s. https.cout.in "))
m=data.frame(m)
names(m) = "text"

#Creating Corpus
my_Corpus = Corpus(VectorSource(m$text))

#converting the text to Lower
Corpus_lower <- tm_map(my_Corpus, content_transformer(tolower))
writeLines(as.character(Corpus_lower[[1]])) 


# remove numbers
Corpus_number <- tm_map(Corpus_lower, removeNumbers)
writeLines(as.character(Corpus_number[[1]])) 


# remove punctuation
Corpus_pun <- tm_map(Corpus_number, removePunctuation)
writeLines(as.character(Corpus_pun[[1]]))


# remove whitespace
Corpus_white <-tm_map(Corpus_pun,stripWhitespace)
writeLines(as.character(Corpus_white[[1]]))

# remove Stopwords
Cor_stop <- tm_map(Corpus_white,removeWords,stopwords("english"))
writeLines(as.character(Cor_stop[[1]]))

# remove URLs
removeURL <- function(x) gsub("http[a-z]*", "", x)

### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
Corpus_url <- tm_map(Cor_stop, content_transformer(removeURL))
writeLines(as.character(Corpus_url[[3]]))


#???# keep a copy of corpus to use later as a dictionary for stem completion
myCorpus<- Corpus_url

# stem words
stem_doc<- tm_map(Corpus_url, stemDocument)
writeLines(as.character(stem_doc[[1]]))

#lemmatization

lemma1 <- tm_map(stem_doc,lemmatize_words)
writeLines(as.character(lemma1[[1]]))


# TermDocment matrix
tdm <- TermDocumentMatrix(lemma1)

inspect(tdm)
inspect(tdm[1:5, 1:3])


#finding the frequent items
frequent_terms <- findFreqTerms (tdm,1,2) 
frequent_terms

