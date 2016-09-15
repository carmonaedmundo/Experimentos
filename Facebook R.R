#install.packages("Rfacebook") 
library(httr)
library(httpuv)
library(Rfacebook)
token <-""

vamos <- getPage("deloittemexico",token, n = 500)
vamos[which.max(vamos$likes_count), ]
## convert Facebook date format to R date format
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}
## aggregate metric counts over month
aggregate.metric <- function(metric) {
  m <- aggregate(vamos[[paste0(metric, "_count")]], list(month = vamos$month), 
                 mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}
# create data frame with average metric counts per month
vamos$datetime <- format.facebook.date(vamos$created_time)
vamos$month <- format(vamos$datetime, "%Y-%m")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df <- do.call(rbind, df.list)
# visualize evolution in metric
#install.packages("ggplot2")
#install.packages("scales")
library(ggplot2)
library(scales)
ggplot(df, aes(x = month, y = x, group = metric)) + geom_line(aes(color = metric)) + 
  scale_x_date(breaks = "months", labels = date_format("%m")) + scale_y_log10("Average count per post", 
                                                                             breaks = c(10,100,200,400,1500)) + theme_bw() + theme(axis.title.x = element_blank())

bestpost <- vamos[which.max(vamos$likes_count), ]
post_id <- head(bestpost$id, n = 1)  ## the most liked post
post <- getPost(post_id, token, n = 1000, likes = TRUE, comments = FALSE)
users <- getUsers(post$likes$from_id, token)
