f1_acc_recall <- function(hit, samplesize){
  acc <- hit/samplesize
  recall <- hit/461
  f1 <- 2 * acc * recall / (acc + recall)
  return (data.frame(acc = acc, recall = recall, f1 = f1))
}

result <- f1_acc_recall(58-38, 8211 - 6414)
result2 <- f1_acc_recall(38, 6414)
result2 <- f1_acc_recall(58, 58)
result2
