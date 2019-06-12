getwd()
install.packages("plyr")
library(plyr)
install.packages("stringr")
library(stringr)
install.packages("tm")
library(tm)
install.packages("ggplot2")
library(ggplot2)
install.packages("NLP")
pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")
starwars = read.csv("C:\\omis 670\\final version  twitter\\vzwsupport1.csv")

score.sentiment<-function(sentences, pos.words, neg.words, .progress='none')
{
  scores <-laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence<-gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence<-gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence<-gsub('\\d+', '', sentence)
                   
                   #convert to lower
                   sentence<-tolower(sentence)
                   
                   
                   # split sentence into words with str_split (stringr package)
                   word.list<- str_split(sentence, "\\s+")
                   words<- unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches<-match(words, pos)
                   neg.matches<- match(words, neg)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches<- !is.na(pos.matches)
                   neg.matches<- !is.na(neg.matches)
                   
                   # final score
                   score<- sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df<- data.frame(text=sentences, score=scores)
  return(scores.df)
}


scores_facebook<-score.sentiment(starwars$text, pos, neg, .progress='text')
View(scores_facebook)

scores_facebook$score_chr<-as.character(scores_facebook$score)

#After looking at the summary(scores_facebook$score) decide on a threshold for the sentiment labels
scores_facebook$score_chr<-gsub("^0$", "Neutral", scores_facebook$score_chr)
scores_facebook$score_chr<-gsub("^1$|^2$|^3$", "Positive", scores_facebook$score_chr)
scores_facebook$score_chr<-gsub("^4$|^5$|^6$|^7$|^8$|^9$|^10$|^11$|^12$", "Very Positive", scores_facebook$score_chr)
scores_facebook$score_chr<-gsub("^-1$|^-2$|^-3$", "Negative", scores_facebook$score_chr)
scores_facebook$score_chr<-gsub("^-4$|^-5$|^-6$|^-7$|^-8$|^-9$|^-10$|^-11$|^-12$", "Very Negative", scores_facebook$score_chr)


#Convert score_chr to factor for visualizations
scores_facebook$score_chr<-as.factor(scores_facebook$score_chr)

#Create a bar chart that shows distributions of sentiment labels
ggplot(scores_facebook, aes(x=score_chr))+geom_bar()+theme_bw()


install.packages("SnowballC") 
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("rvest")
library(rvest)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

comments<-read.csv("C:\\omis 670\\final version  twitter\\tmobilehelp1.csv")
View(comments)

corpus<-Corpus(VectorSource(comments$text))
inspect(corpus)

corpus<-tm_map(corpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))

corpus<- tm_map(corpus, content_transformer(tolower))
corpus<-tm_map(corpus, removeWords, c("tmobile","TMobile","t-mobile","T-Mobile","TMobileHelp","tmobilehelp"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus<- tm_map(corpus, removeNumbers)
corpus<- tm_map(corpus, stemDocument)


dtm<-TermDocumentMatrix(corpus, control = list(weighting=weightTf))
m <- as.matrix(dtm)

v <- sort(rowSums(m),decreasing=TRUE)
df <- data.frame(word = names(v),freq=v)

head(df, 10)
View(df)

wordcloud(words = df$word, freq = df$freq, min.freq = 30, max.words=1000, scale=c(4,0.5), random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
