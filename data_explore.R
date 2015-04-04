train.user <- read.csv("data/tianchi_mobile_recommend_train_user.csv")
train.item <- read.csv("data/tianchi_mobile_recommend_train_item.csv")

sub.train.user <- train.user[1:10000,]
sub.train.user$freq <- 1
sub.train.user$date <- substr(sub.train.user$time,1,10)
View(sub.train.user)
agg <- aggregate(freq ~ date +  behavior_type, sub.train.user, sum)

freq_analysis <- function(train.user){
  train.user$freq <- 1
  train.user$date <- substr(train.user$time,1,10)
  View(sub.train.user)
  agg <- aggregate(freq ~ date +  behavior_type, train.user, sum)
  return(agg)
  
}

tidy_freq <- function(freq){
  browse.idx <- freq$behavior_type == 1;
  favor.idx  <- freq$behavior_type == 2;
  addin.idx  <- freq$behavior_type == 3;
  buy.idx    <- freq$behavior_type == 4;
  df.browse  <- freq[browse.idx,]
  df         <- data.frame(unique(freq$date), freq[browse.idx,3],freq[favor.idx,3],freq[addin.idx,3],freq[buy.idx,3])
  names(df)  <- c("date","bro","fav","add","buy")
  return(df)
}


### TO DO
freq_plot <- function(freq.tidy){
  par(mfrow = c(2,2))
  plot(freq.tidy$bro, type = "l", col = 1, xlab = "date", ylab = "browse times")
  plot(freq.tidy$fav, type = "l", col = 2, xlab = "date", ylab = "favor times")
  plot(freq.tidy$add, type = "l", col = 3, xlab = "date", ylab = "add times")
  plot(freq.tidy$buy, type = "l", col = 4, xlab = "date", ylab = "buy times")
  
}

freq <- freq_analysis(train.user)
save(freq, file = "freq.raw.RData")

load("freq.raw.RData")

freq.tidy <- tidy_freq(freq)

par(mfrow = c(2,2))
plot(freq.tidy$bro, type = "l", col = 1, xlab = "date", ylab = "browse times")
plot(freq.tidy$fav, type = "l", col = 2, xlab = "date", ylab = "favor times")
plot(freq.tidy$add, type = "l", col = 3, xlab = "date", ylab = "add times")
plot(freq.tidy$buy, type = "l", col = 4, xlab = "date", ylab = "buy times")

freq.tidy.ts <- ts(freq.tidy[, c("bro","fav","add","buy")], frequency = 365, start = c(2014,11,18))
xyplot.ts(freq.tidy.ts)

users <- unique(train.user$user_id)
data.1 <- train.user[train.user$user_id == users[1],]

