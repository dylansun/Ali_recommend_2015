## 判断用户是否有先放入购物车，然后在接下来的一天购买的情况
library("lubridate")
train.user <- read.csv("data/tianchi_mobile_recommend_train_user.csv")
str(train.user)


before <- function(date.chart, date.buy){
  year.char <- year(date.chart)
  mon.char  <- month(date.chart)
  day.char <- day(date.chart) 
  
  year.buy <- year(date.buy)
  mon.buy  <- month(date.buy)
  day.buy  <- day(date.buy) 
  if(year.char < year.buy){
    return(TRUE)
  }
  else{
    if(mon.char < mon.buy){
      return(TRUE)
    }
    else{
      if(day.char < day.buy){
        return(TRUE)
      }
    }
  }
  
  return(FALSE)
}

bench <- read.csv("data/tianchi_mobile_recommendation_predict.csv")
user.bench <- unique(bench$user_id)
n <- length(user.bench)
not_buy_immediate <- rep(FALSE, n)

for(i in 1: n){
  print(i)
  users.i <- subset(train.user, train.user$user_id == user.bench[i])
  users.i.addchart <- subset(users.i, users.i$behavior_type == 3)
  users.i.buy <- subset(users.i, users.i$behavior_type == 4)
  n.addchar <- nrow(users.i.addchart)
  n.buy <- nrow(users.i.buy)
  
  if(n.addchar != 0 & n.buy != 0){
    for(j in 1: n.addchar){
      if(not_buy_immediate[i] == TRUE){
        break
      }
      
      for(k in 1: n.buy ){
        if(not_buy_immediate[i] == TRUE){
          break
        }
        
        if(users.i.buy$item_id[k] == users.i.addchart$item_id[j]){
          date.chart <- as.POSIXct(users.i.addchart$time[j])
          date.buy   <- as.POSIXct(users.i.buy$time[k])
          if(before(date.chart, date.buy)){
            not_buy_immediate[i] <- TRUE
          }
        }
      }
    }
  }
}

save(not_buy_immediate, user.bench, file = "not_buy_immediate.RData")

user.bench.not.immed <- user.bench[not_buy_immediate]

n <- nrow(bench)
idx <- rep(FALSE, n)
m <- length(user.bench.not.immed)
for(i in 1:n){
  print(i)
  for(j in 1:m){
    if(bench$user_id[i] == user.bench.not.immed[j])
    {
      idx[i] <- TRUE
      break
    }
  }  
}

bench.sub <- bench[idx,]
bench.sub <- unique(bench.sub)
write.csv(bench.sub, file = "data/tianchi_mobile_recommendation_predict.csv",row.names = FALSE, quote = FALSE)

buy_imm <- bench[!idx,]
write.csv(buy_imm, file = "data/tianchi_mobile_recommendation_predict.csv",row.names = FALSE, quote = FALSE)
