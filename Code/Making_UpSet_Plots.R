setwd("../")
options(stringsAsFactors = F)
Mad.test <- read.csv("Madison_test_df.csv")
Mad.test <- Mad.test[,-1]
rownames(Mad.test) <- NULL
Mad.sig.pos <- Mad.test[which(Mad.test$enrich_pos_p_value < 0.05),]
Mad.sig.neg <- Mad.test[which(Mad.test$enrich_neg_p_value < 0.05),]

Pitt.test <- read.csv("Pittsburgh_test_df.csv")
Pitt.test <- Pitt.test[,-1]
rownames(Pitt.test) <- NULL
Pitt.sig.pos <- Pitt.test[which(Pitt.test$enrich_pos_p_value < 0.05),]
Pitt.sig.neg <- Pitt.test[which(Pitt.test$enrich_neg_p_value < 0.05),]

Char.test <- read.csv("Charlotte_test_df.csv")
Char.test <- Char.test[,-1]
rownames(Char.test) <- NULL
Char.sig.pos <- Char.test[which(Char.test$enrich_pos_p_value < 0.05),]
Char.sig.neg <- Char.test[which(Char.test$enrich_neg_p_value < 0.05),]

Phoen.test <- read.csv("Phoenix_test_df.csv")
Phoen.test <- Phoen.test[,-1]
rownames(Phoen.test) <- NULL
Phoen.sig.pos <- Phoen.test[which(Phoen.test$enrich_pos_p_value < 0.05),]
Phoen.sig.neg <- Phoen.test[which(Phoen.test$enrich_neg_p_value < 0.05),]

pos.list <- unique(c(Mad.sig.pos$word,Pitt.sig.pos$word,Char.sig.pos$word,Phoen.sig.pos$word))
neg.list <- unique(c(Mad.sig.neg$word,Pitt.sig.neg$word,Char.sig.neg$word,Phoen.sig.neg$word))

pos.sig <- as.data.frame(cbind(pos.list,0,0,0,0))

for(i in 1:57){
  if(pos.sig$pos.list[i] %in% Mad.sig.pos$word)pos.sig$V2[i] <- 1
  if(pos.sig$pos.list[i] %in% Pitt.sig.pos$word)pos.sig$V3[i] <- 1
  if(pos.sig$pos.list[i] %in% Char.sig.pos$word)pos.sig$V4[i] <- 1
  if(pos.sig$pos.list[i] %in% Phoen.sig.pos$word)pos.sig$V5[i] <- 1

}

colnames(pos.sig) <- c("Words","Madison","Pittsburgh","Charlotte","Phoenix")
pos.sig$Madison <- as.numeric(pos.sig$Madison)
pos.sig$Pittsburgh <- as.numeric(pos.sig$Pittsburgh)
pos.sig$Charlotte <- as.numeric(pos.sig$Charlotte)
pos.sig$Phoenix <- as.numeric(pos.sig$Phoenix)


neg.sig <- as.data.frame(cbind(neg.list,0,0,0,0))

for(i in 1:45){
  if(neg.sig$neg.list[i] %in% Mad.sig.neg$word)neg.sig$V2[i] <- 1
  if(neg.sig$neg.list[i] %in% Pitt.sig.neg$word)neg.sig$V3[i] <- 1
  if(neg.sig$neg.list[i] %in% Char.sig.neg$word)neg.sig$V4[i] <- 1
  if(neg.sig$neg.list[i] %in% Phoen.sig.neg$word)neg.sig$V5[i] <- 1

}

colnames(neg.sig) <- c("Words","Madison","Pittsburgh","Charlotte","Phoenix")
neg.sig$Madison <- as.numeric(neg.sig$Madison)
neg.sig$Pittsburgh <- as.numeric(neg.sig$Pittsburgh)
neg.sig$Charlotte <- as.numeric(neg.sig$Charlotte)
neg.sig$Phoenix <- as.numeric(neg.sig$Phoenix)


library(UpSetR)
upset(pos.sig, sets = c("Madison", "Pittsburgh", "Charlotte", "Phoenix"), sets.bar.color = "#56B4E9",
      order.by = "freq", empty.intersections = "on", nintersects = 8)


upset(neg.sig, sets = c("Madison", "Pittsburgh", "Charlotte", "Phoenix"), sets.bar.color = "#56B4E9",
      order.by = "freq", empty.intersections = "on", nintersects = 10)

