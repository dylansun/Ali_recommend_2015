## benchmark 
## 
train.user <- read.csv("data/tianchi_mobile_recommend_train_user.csv")
data.12.18 <- subset(train.user, substr(train.user$time,1,10) == "2014-12-18" )
data.12.18.add <- subset(data.12.18, data.12.18$behavior_type == 3) 
data.12.18.buy <- subset(data.12.18, data.12.18$behavior_type == 4) 
user.item.add <- data.frame(user_id = data.12.18.add$user_id, item_id = data.12.18.add$item_id)
user.item.buy <- data.frame(user_id = data.12.18.buy$user_id, item_id = data.12.18.buy$item_id)

remove_common <- function(add, buy){
  n1 <- nrow(add)
  n2 <- nrow(buy)
  idx <- rep(TRUE, n1)
  for(i in 1:n1){
    for(j in 1:n2){
      if((add$item_id[i] == buy$item_id[j] )& (add$user_id[i] == buy$user_id[j]) ){
        idx[i] = FALSE
      }
    }
  }
  add[idx,]
}
left <- remove_common(user.item.add,user.item.buy)
left.unique <- unique(left)
write.csv(left.unique, file = "data/tianchi_mobile_recommendation_predict.csv",row.names = FALSE, quote = FALSE)


## Result:
## recall: 12.581345%
## precision: 0.714110%
## reference size: 461
## hit: 58  