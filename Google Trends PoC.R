library(gtrendsR)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
usr <- "xxx"
pwd <- "XXX"
gconnect(usr, pwd)
#langTRend <- gtrends("SAMS CLUB",geo = "MX", start_date = "2013-06-01", end_date = "2016-04-05")
langTRend <- gtrends(c("Deloitte","PWC"),geo = "MX", start_date = "2010-01-01", end_date = "2016-04-05")
plot(langTRend)
trends<-langTRend$trend



ggplot(trends, aes(x = start)) + 
  geom_line(aes(y = Deloitte.MX, color="Deloitte.MX"))+
  geom_line(aes(y = PWC.MX, color="PWC.MX"))

trends14<-filter(trends, year(start)>2014)
trends14$start <- as.Date(trends14$start)

ggplot(trends14, aes(x = start)) + 
  geom_line(aes(y = Deloitte.MX, color="Deloitte.MX"))+
  geom_line(aes(y = PWC.MX, color="PWC.MX"))+
  scale_x_date(breaks= date_breaks(width = "2 months"),
               labels = date_format("%Y-%m"))
