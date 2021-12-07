#  Predict-Sentiment-Analysis-for-covid19-tweets
My first repository on GitHub.


# Problem Statement: To Predict the sentiment of Covid19 tweets
## Author: "Jahnavi Patchipalu and Wen Yuan"
## Date: 06-12-2021
## Course: Program for Big Data Analytics


```{r}
# To Load Libraries

library(tidyverse)
library(rtweet)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(plotrix)
library(radarchart)
library(frequency)
library(wordcloud)
library(syuzhet)
library(caTools)
library(tm)

```


#  collect data about covid tweets from the TwitterAPI
```{r}
library(rtweet)
#covid19_tweets <- search_tweets("#covid19",n=10000,include_rts=FALSE,retryonratelimit=TRUE)
```


#  Import the data
```{r}
sentimentss <- read.csv("C:/Users/jaanu/Downloads/covid19_tweets.csv",stringsAsFactors = F)
```



#  Remove unnecessary columns
```{r}
sentiment <-sentimentss[,c(3,4,5,6,11,12,13,14,32,34,37,42,43,45,64,65,66,67,68,73,74,77,78,79,82,84)]
```


#  Observe data types
```{r}
str(sentiment)
```



#  Change the data types
```{r}
sentiment$lang <- as.factor(sentiment$lang)
sentiment$quoted_text <- as.factor(sentiment$quoted_text)
sentiment$quoted_location <- as.factor(sentiment$quoted_location)
sentiment$place_name <- as.factor(sentiment$place_name)
sentiment$place_full_name <- as.factor(sentiment$place_full_name)
sentiment$place_type <- as.factor(sentiment$place_type)
sentiment$country <- as.factor(sentiment$country)
sentiment$country_code <- as.factor(sentiment$country_code)
sentiment$name <- as.factor(sentiment$name)
sentiment$location <- as.factor(sentiment$location)

```



# Aggregate top 50 countries
```{r}
library(dplyr)
sentiment %>% 
  group_by(country) %>% 
  summarise(max=max(retweet_count)) %>% 
  top_n(50)
```

#  Remove Missing Values
```{r}
covid19 <- subset(sentiment,lang != "und")
sentiment1 <- subset(sentiment, country != "")
sentiment1
```

#  Exploratory Data Analysis
# Top 10 Retweet count 

```{r}
top_10rc <- sentiment1%>%
  group_by(country) %>% 
  summarise(max = max(retweet_count)) %>% 
  top_n(10)
head(top_10rc)
```

# Maximum Number of tweets from different countries
```{r}
library(ggplot2)

ggplot(sentiment1,aes(country)) + geom_bar(aes(fill=lang)) + labs(title = "Maximum Number of tweets from different countries(With multiple languages)",caption = "Source:Data from Twitter API") + theme(axis.text.x = element_text(angle = 90))
```
![image](https://user-images.githubusercontent.com/95671214/145086966-72e62f19-e143-4a80-886d-74b4d6ee4d08.png)



```{r}
## Top tweeting location
top5cvt_loc <- sentiment1 %>% 
  filter(!is.na(place_full_name)) %>% 
  count(place_full_name, sort = TRUE) %>% 
  top_n(5)
head(top5cvt_loc)

```


```{r}
# Data Visualization for Top tweeting location

ggplot(top5cvt_loc,aes(place_full_name,n)) + geom_bar(stat="identity",fill="darkgreen") + 
  labs(title="Top Tweeting location",x="Location",y="Count")

```

![image](https://user-images.githubusercontent.com/95671214/145087065-d7d38822-d354-4279-a207-49cb44db316c.png)



# Top 10 User Tweets

```{r}
sentiment1 %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10) %>%
  mutate(screen_name = paste0("@", screen_name))
```

```{r}
# Most Liked Tweets

LT <- sentiment1 %>% 
  arrange(-favorite_count)%>%
  top_n(1000,favorite_count) %>%
  select(created_at, screen_name, text, favorite_count,country)
LT
```

```{r}
# Most liked Tweets Graph
ggplot(LT,aes(screen_name,favorite_count)) + geom_bar(stat="identity",aes(fill=country)) + 
  labs(title = "Most liked Tweets Vs Favorite count",x="Screen Name(User Name)",y="Favorite Count") + theme(axis.text.x = element_text(angle = 90))
```

![image](https://user-images.githubusercontent.com/95671214/145087117-fefe112a-dda3-4fc1-8a68-caf8f3e81c23.png)


```{r}
# Most retweeted tweet

 retw <- sentiment1 %>% 
  arrange(-retweet_count) %>%
  top_n(1000,retweet_count) %>% 
  select(created_at, screen_name, text, retweet_count,country)

```

```{r}
#Most Retweeted tweets
ggplot(retw,aes(screen_name,retweet_count)) + geom_bar(stat="identity",aes(fill=country)) + 
  labs(title = "Most Retweeted tweets Vs Retweet count",x="Screen Name(User Name)",y="Retweet Count") + theme(axis.text.x = element_text(angle = 90))

```
![image](https://user-images.githubusercontent.com/95671214/145087163-f8c6c555-fe18-4c7c-b23c-4bad7ab376b7.png)



#  Text mining
```{r}
text = sentiment1$text   # save tweets to another data set "text"

text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",text)         #remove names
text = gsub("http[^[:blank:]]+","",text)                   #remove html links
text = gsub("@\\w+","",text)                               #remove people names
text = gsub("[[:punct:]]","",text)                         #remove punctuation
text = trimws(text, which = c("both", "left", "right"))    # remove white space

text = gsub('[[:digit:]]+', '', text)                      # remove digits
text = gsub("[\r\n]", "", text)                            # remove line breaks
text = iconv(text, to = "ASCII//TRANSLIT")                 # remove not readable standard text
text = iconv(text, "ASCII", "UTF-8", sub="")               # remove not readable standard text
text = tolower(text)                                       # lower case

sentiment1
```



#  Remove stop words
```{r}
library(dplyr)
library(stringr)
library(tidytext)
remove_reg <- "&amp;|&lt;|&gt;" #regular expression
newstops <- c('covid_19','covid-19','covid 19','coronavirus','covid19', '#coronavirus', '#coronavirusoutbreak', '#coronavirusPandemic', '#covid19', '#covid_19', '#epitwitter', '#ihavecorona', '#StayHomeStaySafe', '#TestTraceIsolate','de','la','en','el',"se","las","los","por","con","del","les") #hashtags that need to be removed

tidy_tweets <- sentiment1 %>%  
  mutate(text = str_remove_all(text, remove_reg)) %>%  #remove regular expression
  unnest_tokens(word, text, token = 'tweets',strip_url = TRUE) %>% #work tokenizations
  filter(!word %in% stop_words$word, #remove stopwords
         !word %in% str_remove_all(stop_words$word, "'"),
         !word %in% newstops, #remove those hashtags
         str_detect(word, "[a-z]"))
```

#  Top 10 word frequency
```{r}
library(frequency)
#get words and their frequency
frequency_global <- tidy_tweets %>% count(word, sort=T) 
#get the top 10
frequency_global %>% top_n(10)
```



#  Text analysis
#  data visualization
```{r}
library(wordcloud)
wordcloud(frequency_global$word,frequency_global$n, min.freq = 100,
          scale=c(4.5, .3), random.order = FALSE, random.color = FALSE,
          colors = brewer.pal(8, "Dark2"), res=800)
```

![image](https://user-images.githubusercontent.com/95671214/145087276-12013938-31e9-4588-a15f-e182d7ba9da1.png)



#  Only US tweets and word frequency
```{r}
#get cleaned tweets that are located in the US
tidy_us <- tidy_tweets[is.na(tidy_tweets$country_code)==F & tidy_tweets$country_code == "US", ]

#get words and their frequency
frequency_us <- tidy_us %>% count(word, sort=T)
#get the top 10
frequency_us %>% top_n(10)
```


#  US tweet word frequency visualization
```{r}
wordcloud(frequency_us$word,frequency_us$n, min.freq =50, scale=c(4.5, .2), random.order = FALSE, random.color = FALSE
          ,colors = brewer.pal(8, "Dark2"), res=800)


```
![image](https://user-images.githubusercontent.com/95671214/145087347-eba73730-12f5-4068-9662-044497ada4df.png)




#  By using "bing" get the sentiments and visualize the sentiments
```{r}
tweets_bing<-tidy_tweets%>% 
  # Implement sentiment analysis using the "bing" lexicon`
  inner_join(get_sentiments("bing")) 

perc<-tweets_bing %>% 
  count(sentiment)%>% #count sentiment
  mutate(total=sum(n)) %>% #get sum
  group_by(sentiment) %>% #group by sentiment
  mutate(percent=round(n/total,2)*100) %>% #get the proportion
  ungroup()

label <-c( paste(perc$percent[1],'%',' - ',perc$sentiment[1],sep=''),#create label
     paste(perc$percent[2],'%',' - ',perc$sentiment[2],sep=''))

library(plotrix)
pie3D(perc$percent,labels=label,labelcex=1.1,explode= 0.1, 
      main="Worldwide Sentiment") #create a pie chart
```

![image](https://user-images.githubusercontent.com/95671214/145087410-5a910033-e2c4-45b7-b175-859b2b6c3a35.png)



#  Most Common Positive and Negative words

```{r}
top_words <- tweets_bing %>%
  # Count by word and sentiment
  count(word, sentiment) %>%
  group_by(sentiment) %>% #group ny sentiment
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

#plot the result
ggplot(top_words, aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n, hjust=1), size = 3.5, color = "black") +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip() +
  ggtitle("Most Common Positive and Negative words (Global)") + 
  theme(plot.title = element_text(size = 14, face = "bold",hjust = 0.5))
```

![image](https://user-images.githubusercontent.com/95671214/145087462-0be8ca21-8d26-42ad-8380-2b3c3981208f.png)




#  NRC Plot

```{r}
library(radarchart)
tidy_tweets %>%
  # implement sentiment analysis using the "nrc" lexicon
  inner_join(get_sentiments("nrc")) %>%
  # remove "positive/negative" sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  #get the frequencies of sentiments
  count(sentiment,sort = T) %>% 
  #calculate the proportion
  mutate(percent=100*n/sum(n)) %>%
  select(sentiment, percent) %>%
  #plot the result
  chartJSRadar(showToolTipLabel = TRUE, main = "NRC Radar")
```
![image](https://user-images.githubusercontent.com/95671214/145087602-d6e67be9-d920-4db9-8e18-3bc8873f1e31.png)


#  Sentiment Word Frequency

```{r}
tidy_tweets %>%
  # implement sentiment analysis using the "nrc" lexicon
  inner_join(get_sentiments("nrc")) %>%
  # remove "positive/negative" sentiments
  filter(!sentiment %in% c("positive", "negative")) %>%
  #get the frequencies of sentiments of words
  count(word,sentiment) %>% 
  group_by(sentiment) %>%
  top_n(10) %>% 
  ungroup() %>%
  mutate(word=reorder(word,n)) %>% 
  #plot the sentiment word frequency
  ggplot(aes(x=word,y=n,fill=sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ sentiment, scales = "free") +
    coord_flip() +
  ggtitle(label = "Sentiment Word Frequency (Global)") + 
  theme(plot.title = element_text(size = 14, face = "bold",hjust = 0.5))
```


![image](https://user-images.githubusercontent.com/95671214/145087638-8d687dd2-2a73-4a46-8253-2130183eeb77.png)

#  Create sentiment_score


```{r}
library(tidytext)
library(syuzhet)
w <- get_sentiment(sentiment1$text)
sentiment_score= as.data.frame(w)
colnames(sentiment_score)[1] <- "sentiment_score"
tweets <- cbind(sentiment1$text,sentiment_score)
colnames(tweets)[1] <- "text"
hist(tweets$sentiment_score,breaks = 5)

```

![image](https://user-images.githubusercontent.com/95671214/145087781-c35f78e9-7983-4823-84db-754e7e313618.png)



#  Convert to positive and negative sentiments
# Then we need to construct the outcome variable for these tweets, which means that we have to label them as positive & negative, 
```{r}

tweets$Senti <- ifelse(tweets$sentiment_score >= 0,"Positive","Negative")
colnames(tweets)[3] <- "Senti"
tweets$Senti <- as.factor(tweets$Senti)




#tweets$Negative <- as.factor(tweets$sentiment_score <= -1)
#table(tweets$Negative)
#tweets$Positive <- as.factor(tweets$sentiment_score>=1)
#table(tweets$Positive)
str(tweets)
```














#  CREATING A CORPUS

#A corpus is a collection of documents.

#We will need to convert our tweets to a corpus for pre-processing. Various function in the tm package can be used to create a corpus in many different ways.
#We will create it from the tweet column of our data frame using two functions, VCorpus() and VectorSource(). We feed this to latter the Tweets variable of the tweets data frame.

```{r}
library(tm)
corpus <- VCorpus(VectorSource(tweets$text))
corpus
#corpus[[1]]
```

#  Data pre-processing
```{r}
corpus <- tm_map(corpus, content_transformer(tolower)) #First step is to transform all text to lower case
#corpus[[1]]
corpus <- tm_map(corpus, PlainTextDocument) #converts corpus to a Plain Text Document

corpus <- tm_map(corpus, removePunctuation) #Removing punctuation

stopwords("english")[1:50] #Removing stop words

corpus <- tm_map(corpus, removeWords, c("covid", stopwords("english")))
#corpus[[1]]

corpus <- tm_map(corpus, stemDocument) #Stemming
#corpus[[1]]

library(SnowballC)
DTM <- DocumentTermMatrix(corpus) #Create a Document Term Matrix
DTM
```





#  most popular terms are, or words
```{r}
freq <- findFreqTerms(DTM, lowfreq = 50)
freq 
#Remove sparse terms
sparse_DTM <- removeSparseTerms(DTM, 0.98)
sparse_DTM
#Convert the DTM to a data frame
tweetsSparse <- as.data.frame(as.matrix(sparse_DTM))
#Fix variables names in the data frame
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
```



#  Add the dependent variable

```{r}

tweetsSparse$Senti <- tweets$Senti
tweetsSparse
#tweetsSparse$Negative <- tweets$Negative
#tweetsSparse$Positive <- tweets$Positive


```

# convert true,false to yes,no

```{r}
#tweetsSparse$Negative <- as.factor(ifelse(tweetsSparse$Negative == TRUE, "Yes","No"))
#tweetsSparse$Positive <- as.factor(ifelse(tweetsSparse$Positive == TRUE, "Yes","No"))

```

#   BUILDING MACHINE LEARNING MODEL

#  Split data in training/testing sets

# Negative split with 80%train 

```{r}
library(caTools)
set.seed(123)

split <- sample.split(tweetsSparse$Senti, SplitRatio = 0.8)

trainSparse <- subset(tweetsSparse, split == TRUE)
testSparse <- subset(tweetsSparse, split == FALSE)
```




# Logistic regression

# Predict Positive, Negative sentiment
```{r}
tweetLog <- glm(Senti ~ . , data = trainSparse, family = "binomial")
#PREDICTING SENTIMENT
tweetLog_predict_test <- predict(tweetLog, type = "response", newdata = testSparse)
tweetLog_predict_test <- ifelse(tweetLog_predict_test > 0.5,"Positive","Negative")
#Create confusion matrix
cmat_logRegr <- table(testSparse$Senti, tweetLog_predict_test)
cmat_logRegr
# Accuracy
accu_logRegr <- (cmat_logRegr[1,1] + cmat_logRegr[2,2])/sum(cmat_logRegr)
accu_logRegr

```






# Decision Tree- Ctree model
# predict positive, Negative sentiment
```{r}
library(party)
library(partykit)
tweetCTREE <- ctree(Senti ~ . , data = trainSparse)
#plot
plot(tweetCTREE)
#predict
predictCTREE <- predict(tweetCTREE, newdata = testSparse, type = "response")
#create confusion matrix
cmat_CTREE <- table(testSparse$Senti, predictCTREE)
cmat_CTREE
#Accuracy
accu_CTREE <- (cmat_CTREE[1,1] + cmat_CTREE[2,2])/sum(cmat_CTREE)
accu_CTREE
```
![image](https://user-images.githubusercontent.com/95671214/145087846-f74e2996-17db-4734-abf3-1aa2a9c313fb.png)






# Decision Tree- cart model
# Predict Positive, Negative sentiment

```{r}
library(rpart)
library(rpart.plot)
library(rattle)
tweetCART <- rpart(Senti ~ . , data = trainSparse, method = "class")
#plot
prp(tweetCART)
fancyRpartPlot(tweetCART)
#predict
predictCART <- predict(tweetCART, newdata = testSparse, type = "class")
#create confusion matrix
cmat_CART <- table(testSparse$Senti, predictCART)
cmat_CART 
#Accuracy
accu_CART <- (cmat_CART[1,1] + cmat_CART[2,2])/sum(cmat_CART)
accu_CART
```
![image](https://user-images.githubusercontent.com/95671214/145087884-56aace27-c197-4190-80ab-b0c7492fab38.png)
![image](https://user-images.githubusercontent.com/95671214/145087895-71f69b9e-7884-4277-97ee-7e9f0b530741.png)





# Naive Bayes
# Predict Positive, Negative Sentiment
```{r}
library(e1071)
tweetNB <- naiveBayes(Senti ~ . , data = trainSparse, laplace = 1)
#predict
predictNB <- predict(tweetNB, newdata = testSparse, type = "class")
#create confusion matrix
cmat_NB <- table(testSparse$Senti, predictNB)
cmat_NB 
#Accuracy
accu_NB <- (cmat_NB[1,1] + cmat_NB[2,2])/sum(cmat_NB)
accu_NB
```



# Association Rules



#  Remove stop words
```{r}
library(dplyr)
library(stringr)
library(tidytext)
remove_regg <- "&amp;|&lt;|&gt;" #regular expression
newstopss <- c( '#epitwitter', '#ihavecorona', '#StayHomeStaySafe', '#TestTraceIsolate','de','la','en','el',"se","las","los","por","con","del","les") #hashtags that need to be removed

tidy_tweetss <- sentiment1 %>%  
  mutate(text = str_remove_all(text, remove_regg)) %>%  #remove regular expression
  unnest_tokens(word, text, token = 'tweets',strip_url = TRUE) %>% #work tokenizations
  filter(!word %in% stop_words$word, #remove stopwords
         !word %in% str_remove_all(stop_words$word, "'"),
         !word %in% newstopss, #remove those hashtags
         str_detect(word, "[a-z]"))
```

#  Top 10 word frequency
```{r}
library(frequency)
#get words and their frequency
frequency_globall <- tidy_tweetss %>% count(word, sort=T) 
#get the top 10
frequency_globall %>% top_n(200)
```



```{r}

transcovid19 <- tweetsSparse
library(tidyverse)
transcovid19 = transcovid19 %>% mutate_all(as.logical)
# Clean up names
colnames(transcovid19) <- gsub(x=colnames(transcovid19),
                               pattern="const\\.", replacement="")
order_trans2 <- as(transcovid19,"transactions")
#transcovid19 <- data.frame(transcovid19)
summary(order_trans2)
write.csv(transcovid19,file = "covidtweetsSparse.csv")
```


```{r}
itemFrequencyPlot(order_trans2,topN=30)
```

![image](https://user-images.githubusercontent.com/95671214/145087959-96f9b051-5515-46b6-964d-fa6f0d861ed0.png)


```{r}
inspect(order_trans2[1:15])

```





```{r}
itemFrequency(order_trans2[,1:4])
```


```{r}
itemFrequencyPlot(order_trans2, support = .10)

```

![image](https://user-images.githubusercontent.com/95671214/145087991-0def21b5-0fa6-4ed1-a801-40c559bcc0d8.png)


```{r}
itemFrequencyPlot(order_trans2, support = .15)

```
![image](https://user-images.githubusercontent.com/95671214/145088017-0df129c2-de82-4576-869c-b00aa65897ff.png)


```{r}
itemFrequencyPlot(order_trans2,topN=10,type="absolute")

```
![image](https://user-images.githubusercontent.com/95671214/145088045-d6a12884-3830-4738-a25d-ed7af56c4089.png)


```{r}
itemFrequencyPlot(order_trans2,topN=10,type="relative")

```

![image](https://user-images.githubusercontent.com/95671214/145088068-7f91b13c-ea7f-47de-b485-131fee1bd3d4.png)


# Defining Rules
# Now that we have have an idea of how the data looks. We proceed to create rules. Rules are formed by defining the minimum support and confidence levels. Also the minlen option lets us to set the minimum number of items for both the LHS and RHS.
```{r}
rules <- apriori(order_trans2, parameter = list(supp = 0.005, conf = 0.20, minlen =2)) 
rules

```

# From the summary we can infer the following –

#1044 rules were formed
#Minimum and maximum values for support, confidence and lift are displayed
#Out of the 1044 rules formed the counts of rule length is also displayed


# Once the rules are formed, we can have a look at the some of the formed rules.


```{r}
options(digits=2)
inspect(rules[1:10])
```


#  Just looking at these rules makes little sense, let’s sort the rules based on confidence levels.

```{r}
rules<-sort(rules, by="confidence", decreasing=TRUE)
options(digits=2)
inspect(rules[1:10])
```


# Now things are starting to make sense, looking at the first transaction – there is a high likelihood of covid19 tweets. Similar to sorting rules confidence, they can also be sorted by lift levels.

```{r}
rules<-sort(rules, by="lift", decreasing=TRUE)
options(digits=2)
inspect(rules[1:10])
```


#  Making Product Recommendations
```{r}
rules <- apriori(order_trans2, parameter = list(supp = 0.005, conf = 0.20, minlen =2), appearance = list(default="rhs",lhs="test"))
summary(rules)

```


# Now we sort the rules by lift.

```{r}
rules<-sort(rules, by="lift", decreasing=TRUE)
options(digits=2)
inspect(rules)

```



# The top product recommendation for people who took negative covid test will travel and get vaccinated.


#Conclusion
#As we have discussed in the article, association rules are used extensively in a variety of fields. We have also seen how Association rules analysis can done with the help of the Apriori algorithm. This is quite helpful in getting product recommendations for customers. So, go ahead and start mining your own datasets.






