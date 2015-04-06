## To find which items can be bought multiple times 
item.buy <- subset(ui, ui$behavior_type == 4, select= c("user_id", "item_id"))
item.buy$times <- 1
agg <- aggregate(times ~ user_id+item_id, data = item.buy, sum)
agg <- agg[order(agg$times, decreasing = T),]
agg.sub <- agg[which(agg$times > 1 ), ]
View(agg.sub)
item.sub <-  data.frame(item_id = unique(agg.sub$item_id))

item.categ <- read.csv("data/tianchi_mobile_recommend_train_item.csv")
categ.buy.many.times <- merge(item.sub, item.categ)
categ.buy.many.times <- unique(categ.buy.many.times$item_category)
save(categ.buy.many.times, file = "categ.buy.many.times.RData")
