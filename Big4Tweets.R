library("twitteR")
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

#tweetsDtt <- searchTwitter("@DeloitteNZ")
dttUser<- getUser("DeloitteMX")
dttTweets<-userTimeline(dttUser,n=500)
dfDtt <- twListToDF(dttTweets)
plot(dfDtt$created,dfDtt$retweetCount, type='l')

#tweetsPWC <- searchTwitter("@PwC_NZ", n = 500)
#tweetsPWC <- searchTwitter("@PwC_Mexico")
pwcUser<- getUser("PwC_Mexico")
pwcTweets<-userTimeline(pwcUser,n=500)
dfPWC <- twListToDF(pwcTweets)
points(dfPWC$created,dfPWC$retweetCount, col="red", type = 'l')

#tweetsKPMG <- searchTwitter("@kpmgnzjobs")
#tweetsKPMG <- searchTwitter("@KPMGMEXICO",since = "2015-11-01")
kpmgUser<- getUser("KPMGMEXICO")
kpmgTweets<-userTimeline(kpmgUser,n=500)
dfKPMG <- twListToDF(kpmgTweets)
points(dfKPMG$created,dfKPMG$retweetCount, col="blue", type = 'l')

#tweetsEY <- searchTwitter("@EYnews")
#tweetsEY <- searchTwitter("@EYNewsMexico")
eyUser<- getUser("EYNewsMexico")
eyTweets<-userTimeline(eyUser,n=500)
dfEY <- twListToDF(eyTweets)
points(dfEY$created,dfEY$retweetCount, col="green", type = 'l')


legend("topright", col = c("black","red","blue","green"), cex = 0.8,
       legend = c("Deloitte", "PWC","KPMG","EY"), lty=c(1,1,1,1), lwd=c(3,3,3,3))

retweetCount<-c(dfDtt$retweetCount,dfPWC$retweetCount,dfKPMG$retweetCount,dfEY$retweetCount)
favoriteCount<-c(dfDtt$favoriteCount,dfPWC$favoriteCount,dfKPMG$favoriteCount,dfEY$favoriteCount)
created<-c(dfDtt$created,dfPWC$created,dfKPMG$created,dfEY$created)
company <-c(rep("Deloitte",nrow(dfDtt)),rep("PWC",nrow(dfPWC)),rep("KPMG",nrow(dfKPMG)),rep("EY",nrow(dfEY)))
id <-c(dfDtt$id,dfPWC$id,dfKPMG$id,dfEY$id)
texto <-c(dfDtt$text,dfPWC$text,dfKPMG$text,dfEY$text)
dfTotal <- data.frame(company=company,created=created,retweetCount=retweetCount,
                      favoriteCount=favoriteCount, id=id,texto=texto)
write.csv(dfTotal,file="tweetsBig4.18112015.csv")

dfTotal$Mes<-format(dfTotal$created,"%m-%d")
dfSumretweetCount<- aggregate(dfTotal$retweetCount,list(mes=dfTotal$Mes,company=dfTotal$company),sum)
dfSumfavoriteCount<-aggregate(dfTotal$favoriteCount,list(mes=dfTotal$Mes,company=dfTotal$company),sum)
dfSumID<- aggregate(dfTotal$id,list(mes=dfTotal$Mes,company=dfTotal$company),length)
dfSum<- merge(dfSumretweetCount,dfSumfavoriteCount, by=c("mes","company"))
dfSum<- merge(dfSum,dfSumID, by=c("mes","company"))
names(dfSum)<-c("mes","company","retweetCount","favoriteCount","idCount")
write.csv(dfSum,file="Sum.tweetsBig4.18112015.csv")
tblTotal <- tbl_df(dfTotal)
tdSum <- tblTotal %>% 
  mutate(MESDIA = paste0(as.POSIXlt(tblTotal$created)$mon,as.POSIXlt(tblTotal$created)$mday)) %>%
  group_by(MESDIA,company) %>%
  summarize(tweets=n(),retweetCount=sum(retweetCount))
cor(tdSum$tweets,tdSum$retweetCount)