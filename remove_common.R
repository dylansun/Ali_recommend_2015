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
