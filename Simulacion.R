set.seed(1983)
library(dplyr)
library(arules)
library(arulesViz)
tickets <- 1:1000
simulacion <- as.data.frame(tickets)
rm(tickets)
simulacion$items <- rnorm(1000,4,1)
simulacion$items<- round(simulacion$items,0)
simulacion$items[simulacion$items==0]<-1
simulacion$items[simulacion$items==8]<-7

# simulacion$'1'<-round(runif(1000),2)
# simulacion$'2'<-ifelse(simulacion$items>=2,round(runif(sum(simulacion$items>=2,na.rm=T)),2),NA)
# simulacion$'3'<-ifelse(simulacion$items>=3,round(runif(sum(simulacion$items>=3,na.rm=T)),2),NA)
# simulacion$'4'<-ifelse(simulacion$items>=4,round(runif(sum(simulacion$items>=4,na.rm=T)),2),NA)
# simulacion$'5'<-ifelse(simulacion$items>=5,round(runif(sum(simulacion$items>=5,na.rm=T)),2),NA)
# simulacion$'6'<-ifelse(simulacion$items>=6,round(runif(sum(simulacion$items>=6,na.rm=T)),2),NA)
# simulacion$'7'<-ifelse(simulacion$items>=7,round(runif(sum(simulacion$items>=7,na.rm=T)),2),NA)

simulacion$'1'<-round(rnorm(1000,0.5,0.16),2)
simulacion$'2'<-ifelse(simulacion$items>=2,round(rnorm(sum(simulacion$items>=2,na.rm=T),0.5,0.16),2),NA)
simulacion$'3'<-ifelse(simulacion$items>=3,round(rnorm(sum(simulacion$items>=3,na.rm=T),0.5,0.16),2),NA)
simulacion$'4'<-ifelse(simulacion$items>=4,round(rnorm(sum(simulacion$items>=4,na.rm=T),0.5,0.16),2),NA)
simulacion$'5'<-ifelse(simulacion$items>=5,round(rnorm(sum(simulacion$items>=5,na.rm=T),0.5,0.16),2),NA)
simulacion$'6'<-ifelse(simulacion$items>=6,round(rnorm(sum(simulacion$items>=6,na.rm=T),0.5,0.16),2),NA)
simulacion$'7'<-ifelse(simulacion$items>=7,round(rnorm(sum(simulacion$items>=7,na.rm=T),0.5,0.16),2),NA)

simulacion$'1'[simulacion$'1'>1]<-1
simulacion$'2'[simulacion$'2'>1]<-1
simulacion$'3'[simulacion$'3'>1]<-1
simulacion$'4'[simulacion$'4'>1]<-1
simulacion$'5'[simulacion$'5'>1]<-1
simulacion$'6'[simulacion$'6'>1]<-1
simulacion$'7'[simulacion$'7'>1]<-1

simulacion$'1'[simulacion$'1'<0]<-0
simulacion$'2'[simulacion$'2'<0]<-0
simulacion$'3'[simulacion$'3'<0]<-0
simulacion$'4'[simulacion$'4'<0]<-0
simulacion$'5'[simulacion$'5'<0]<-0
simulacion$'6'[simulacion$'6'<0]<-0
simulacion$'7'[simulacion$'7'<0]<-0

itemsAux <- 11111:11211
item <- itemsAux*4
item <-unique(item)
rm(itemsAux)

prob<- seq(0,1,0.01)
probItems<- as.data.frame(cbind(item, prob))
rm(item)
rm(prob)

probItems$prob[probItems$item==44824]<-0.95
probItems$prob[probItems$item==44820]<-0.94
probItems$prob[probItems$item==44776]<-0.83
probItems$prob[probItems$item==44772]<-0.82
probItems$prob[probItems$item==44724]<-0.7
probItems$prob[probItems$item==44720]<-0.69
probItems$prob[probItems$item==44672]<-0.57
probItems$prob[probItems$item==44632]<-0.47
probItems$prob[probItems$item==44608]<-0.41
probItems$prob[probItems$item==44584]<-0.35

simulacion$item1<- probItems$item[match(simulacion$`1`,probItems$prob)]
simulacion$item2<- probItems$item[match(simulacion$`2`,probItems$prob)]
simulacion$item3<- probItems$item[match(simulacion$`3`,probItems$prob)]
simulacion$item4<- probItems$item[match(simulacion$`4`,probItems$prob)]
simulacion$item5<- probItems$item[match(simulacion$`5`,probItems$prob)]
simulacion$item6<- probItems$item[match(simulacion$`6`,probItems$prob)]
simulacion$item7<- probItems$item[match(simulacion$`7`,probItems$prob)]

rm(probItems)

simulacion <- select(simulacion,10:16)

write.csv(simulacion,"demo_basket", row.names = F)
tr <- read.transactions("demo_basket", format = "basket", sep=",",rm.duplicates=T ,skip = 1)
inspect(tr)
itemFrequencyPlot(tr,topN=100,type="absolute")
rules <- apriori(tr, parameter = list(supp = 0.001, conf = 0.9))

summary(rules)


plot(rules[1:10],method="graph",interactive=TRUE,shading=NA )
