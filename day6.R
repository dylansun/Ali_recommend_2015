## calculate the duration of chart and buy
ui <- read.csv("data/tianchi_mobile_recommend_train_user.csv")

## select subset

ui.chart <- ui[which(ui$behavior_type == 3),]
ui.buy <- ui[which(ui$behavior_type == 4), ]

## remove duplicate
ui.chart <- unique(ui.chart)
ui.buy <- unique(ui.buy)

## select cols
ui.chart.sub <- data.frame( user_id = ui.chart$user_id, item_id = ui.chart$item_id, start = ui.chart$time)
ui.buy.sub <- data.frame(user_id = ui.buy$user_id, item_id = ui.buy$item_id, end = ui.buy$time)

## merge
com <- merge(ui.chart.sub, ui.buy.sub)

##
date.format <- function(date){
  return (as.POSIXct(date, format = "%Y-%m-%d", tz="utc"))
}

com$delta <-date.format(com$end) - date.format(com$start)  
View(com)

## 分成>0 和 <0
com.plus <- subset(com, com$delta > 0)
com.minus <- subset(com,com$delta < 0)

## 取绝对值？
#com.abs <- com
#com.abs$delta <- abs(com.abs$delta)
#com.abs.sub <- com.abs[which(com.abs$delta > 0)]
View(com.plus)
agg <- aggregate(delta~user_id + item_id, data = com.plus, FUN = min)
names(agg)
agg.2 <- aggregate(delta ~ user_id, data = agg, FUN = mean)
hist(as.numeric(agg.2$delta)/ 86400.00, xlab = "days", main = "when to buy after add chart")

save(agg.2, file =  "data/average_duration.RData") 

bench <- read.csv("data/tianchi_mobile_recommendation_predict.4.2.csv")
tmp <- join(bench, agg.2)
tmp <- tmp[order(tmp$delta, decreasing = F),]
View(tmp)

bench.in.1.day <- tmp[which(tmp$delta == 86400),]
bench.NA <- tmp[which(is.na(tmp$delta)),]
View(bench.NA)

bench.in.1.day.submit <- data.frame(user_id = bench.in.1.day$user_id, item_id = bench.in.1.day$item_id)
bench.NA.submit <- data.frame(user_id = bench.NA$user_id, item_id = bench.NA$item_id)

## remove repeat items
