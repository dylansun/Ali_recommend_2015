candidate_set <- read.csv("data/tianchi_mobile_recommendation_predict.4.2.csv")
ui <- read.csv("data/tianchi_mobile_recommend_train_user.csv")
candidate_record <- merge(candidate_set, ui)
sum(candidate_record$behavior_type == 4)
candidate_record.repeat.buy <- candidate_record[which(candidate_record$behavior_type == 4),]

repeat.buy.item <- data.frame(user_id = candidate_record.repeat.buy$user_id, item_id = candidate_record.repeat.buy$item_id)
repeat.buy.item.unique <- unique(repeat.buy.item)
write.csv(repeat.buy.item.unique, file = "data/repeat.buy.csv",row.names = FALSE, quote = FALSE)

candidate_record$event_weight <- 4^(candidate_record$behavior_type)
rank.candidate <- aggregate(event_weight ~ user_id+item_id, data = candidate_record, sum)
rank.candidate <- rank.candidate[order(rank.candidate$event_weight, decreasing = T),]

rank.candidate.submit.820 <- rank.candidate[1:820,] 
rank.candidate.submit.820 <- unique(data.frame(user_id = rank.candidate.submit.820$user_id, item_id = rank.candidate.submit.820$item_id))
write.csv(rank.candidate.submit.820, file = "data/rank.820.csv",row.names = FALSE, quote = FALSE)
