options(stringsAsFactors = F)

#readin data
Mad.test <- read.csv("Madison_test_df.csv")
Mad.busi <- read.csv("business_count_Madison.csv")
Mad.busi <- Mad.busi[,-1]
#
Mad.test.sig.pos <- Mad.test[which(Mad.test$enrich_pos_p_value < 0.05),]
Mad.test.sig.neg <- Mad.test[which(Mad.test$enrich_neg_p_value < 0.05),]

row.names(Mad.test.sig.neg) <- NULL
Mad.test.sig.neg <- Mad.test.sig.neg[,-1]

## Make a postivie suggestion reference for customer.

Madison_customer_suggestion_positive <- c("Fries here is good, maybe you could have a try!",
                                  "Chips here is good, maybe you could have a try!",
                                  "Onion rings here is good, it could be good to have some onion rings with drinks!",
                                  "Nachos here is good, try some!",
                                  "Wings here are good, you could have a try!",
                                  "Burger here is good, try to have one if you're hungry!",
                                  "Monzarella stick is good. Have a try!",
                                  "Quesadillas is good. Have a try!",
                                  "Cheese here is good. Try some cheese snacks!",
                                  "Sandwich here is good. Try some if you're hungry!",
                                  "Pizza here is good. Try some!",
                                  "Taco here is good. Try some!",
                                  "Popcorn here is good. Try some!",
                                  "Bacon here is good. Try some food with bacon!",
                                  "Slider here is good. Try some mini-burger!",
                                  "Fried food here is good. Try some fried food here!",
                                  "Shrimp here is good. Try some seafood snacks.",
                                  "Salads here is good. And it's good for health.",
                                  "Dipping source here is good. Try it!",
                                  "Nugget here is good. Try some!",
                                  "Oysters here is good, try some seafood!",
                                  "Peanut here is good, have some snacks while drinking!",
                                  "Chicken here is good, try some if you're hungry!",
                                  "Beer here is good, enjoy it!",
                                  "Wine here is good, try some?",
                                  "Cocktail here is good, enjoy it!",
                                  "Maybe try some spark? Spark here is good.",
                                  "Spirit here is good, try some strong one?",
                                  "Rum here is good, try some and enjoy the tropical style!",
                                  "Whisky here is good, try some? Freedom and Whisky gang thegither!",
                                  "Vodka here is good. Want to try some strong one?",
                                  "Tequila here is good. Have a try?",
                                  "Brandy here is good. Try a shot?",
                                  "Gin here is good. Maybe have a try?",
                                  "Negroni here is good. Do you want to have a try?",
                                  "Daiquiri here is good. Do you want to have a try?",
                                  "Manhattan here is good. Have a try of taste of Manhattan.",
                                  "Mojito here is good. Have a try of taste of Cuba.",
                                  "Margarita here is good. Try a cup and taste the sweetness and bitterness of love.",
                                  "Martini here is good. Do you want to try one of the most famous cocktails?",
                                  "Service here is good. Enjoy it!",
                                  "Service here is good. Enjoy it!",
                                  "There are parking lots. Don't worry.",
                                  "There is Wi-Fi. But remember to talk more with others.",
                                  "Music here is good. Enjoy the atmosphere!",
                                  "Band here is good. Enjoy the atmosphere!")



Postive_Customer_Suggestion_Ref <- as.data.frame(cbind(Mad.test.sig.pos$word[c(1:40,41:44,51,52)],Madison_customer_suggestion_positive))

colnames(Postive_Customer_Suggestion_Ref) <- c("word","Sug_Customer")

## Make a negative suggestion reference for customer.

Madison_customer_suggestion_negative <- c("Fries here is not good. Think twice before order it.",
                                          "Chips here is not good. Think twice before order it.",
                                          "Nacho here is not good. Think twice before order it.",
                                          "Wings here is not good. Think twice before order it.",
                                          "Cheese here is not good. Think twice before order cheese foods.",
                                          "Taco here is not good. Maybe try some other snacks",
                                          "Shrimp here is not good. Think twice before order it.",
                                          "Sauce here is not good. Think twice before order it.",
                                          "Chicken here is not good. Think twice before order it.",
                                          "Beer here is not good. Think twice before order it.",
                                          "Wine here is not good. Think twice before order it.",
                                          "Vodka here is not good. Think twice before order it.",
                                          "Gin here is not good. Think twice before order it.",
                                          "Negroni here is not good. Think twice before order it.",
                                          "Margarita here is not good. Think twice before order it.",
                                          "Service here is not good. Are you sure you want to go here?",
                                          "Service here is not good. Are you sure you want to go here?",
                                          "Service here is not good. Are you sure you want to go here?",
                                          "Price here is not so friendly.",
                                          "Band here seems to be not so good.")

Negative_Customer_Suggestion_Ref <- as.data.frame(cbind(Mad.test.sig.neg$word[c(1:20)],Madison_customer_suggestion_negative))

colnames(Negative_Customer_Suggestion_Ref) <- c("word","Sug_Customer")


## Function for Customer Suggestion Madison

str(Mad.busi)





####################### Make feature tables for Madison
Mad.busi.Sug.Customer <- Mad.busi[,c(1,2,3)]

word.list.Madison <- unique(c(Postive_Customer_Suggestion_Ref$word,Negative_Customer_Suggestion_Ref$word))


k <- length(word.list.Madison)
m <- dim(Mad.busi.Sug.Customer)[1]

empty.keys <- as.data.frame(matrix(0,nrow = m,ncol = k))
colnames(empty.keys) <- word.list.Madison

Mad.busi.Sug.Customer <- cbind(Mad.busi.Sug.Customer,empty.keys)

Mad.busi.filter30 <- Mad.busi.Sug.Customer[which(Mad.busi.Sug.Customer$review_count >= 30),]
Mad.busi.filter30.original <- Mad.busi[which(Mad.busi$review_count >= 30),]

Mad.busi.filter_below30 <- Mad.busi.Sug.Customer[which(Mad.busi.Sug.Customer$review_count < 30),]
Mad.busi.filter_below30.original <- Mad.busi[which(Mad.busi$review_count < 30),]



for(i in 1:49){
  for(j in 1:213){
    if(Mad.busi.filter30.original[j,(3*i+2)] > 3*Mad.busi.filter30.original[j,(3*i+3)]){
      Mad.busi.filter30[j,(i+3)] <- 1
    }
    if(Mad.busi.filter30.original[j,(3*i+3)] > 3*Mad.busi.filter30.original[j,(3*i+2)]){
      Mad.busi.filter30[j,(i+3)] <- -1
    }
  }
}

dim(Mad.busi.filter_below30)

for(i in 1:49){
  for(j in 1:184){
    if(Mad.busi.filter_below30.original[j,(3*i+2)] > Mad.busi.filter_below30.original[j,(3*i+3)]){
      Mad.busi.filter_below30[j,(i+3)] <- 1
    }
    if(Mad.busi.filter_below30.original[j,(3*i+3)] > Mad.busi.filter_below30.original[j,(3*i+2)]){
      Mad.busi.filter_below30[j,(i+3)] <- -1
    }
  }
}


############### Make Feature table for Pittsburg
##Preparation
Pitt.test <- read.csv("Pittsburgh_test_df.csv")
Pitt.test <- Pitt.test[,-1]
Pitt.busi <- read.csv("business_count_Pittsburgh.csv")
Pitt.busi <- Pitt.busi[,-1]
row.names(Pitt.busi) <- NULL

Pitt.test.sig.pos <- Pitt.test[which(Pitt.test$enrich_pos_p_value < 0.05),]
Pitt.test.sig.neg <- Pitt.test[which(Pitt.test$enrich_neg_p_value < 0.05),]

row.names(Pitt.test.sig.neg) <- NULL
row.names(Pitt.test.sig.pos) <- NULL
## Postitive Suggestions



Pittsburg_customer_suggestion_positive <- c("Fries here is good, maybe you could have a try!",
                                          "Chips here is good, maybe you could have a try!",
                                          "Onion rings here is good, it could be good to have some onion rings with drinks!",
                                          "Nachos here is good, try some!",
                                          "Wings here are good, you could have a try!",
                                          "Burger here is good, try to have one if you're hungry!",
                                          "Monzarella stick is good. Have a try!",
                                          "Quesadillas is good. Have a try!",
                                          "Cheese here is good. Try some cheese snacks!",
                                          "Pizza here is good. Try some!",
                                          "Taco here is good. Try some!",
                                          "Popcorn here is good. Try some!",
                                          "Bacon here is good. Try some food with bacon!",
                                          "Slider here is good. Try some mini-burger!",
                                          "Fried food here is good. Try some fried food here!",
                                          "Shrimp here is good. Try some seafood snacks.",
                                          "Salads here is good. And it's good for health.",
                                          "Dipping source here is good. Try it!",
                                          "Nugget here is good. Try some!",
                                          "Oysters here is good, try some seafood!",
                                          "Peanut here is good, have some snacks while drinking!",
                                          "Chicken here is good, try some if you're hungry!",
                                          "Beer here is good, enjoy it!",
                                          "Wine here is good, try some?",
                                          "Cocktail here is good, enjoy it!",
                                          "Spirit here is good, try some strong one?",
                                          "Rum here is good, try some and enjoy the tropical style!",
                                          "Whisky here is good, try some? Freedom and Whisky gang thegither!",
                                          "Vodka here is good. Want to try some strong one?",
                                          "Tequila here is good. Have a try?",
                                          "Gin here is good. Maybe have a try?",
                                          "Daiquiri here is good. Do you want to have a try?",
                                          "Manhattan here is good. Have a try of taste of Manhattan.",
                                          "Mojito here is good. Have a try of taste of Cuba.",
                                          "Margarita here is good. Try a cup and taste the sweetness and bitterness of love.",
                                          "Martini here is good. Do you want to try one of the most famous cocktails?",
                                          "Service here is good. Enjoy it!",
                                          "Service here is good. Enjoy it!",
                                          "There are parking lots. Don't worry.",
                                          "There is Wi-Fi. But remember to talk more with others.",
                                          "Music here is good. Enjoy the atmosphere!",
                                          "Band here is good. Enjoy the atmosphere!",
                                          "Light here is good. Enjoy the atmosphere!")



Pitt_Positive_Customer_Suggestion_Ref <- as.data.frame(cbind(Pitt.test.sig.pos$word[c(1:20,22:41,48:50)],Pittsburg_customer_suggestion_positive))

colnames(Pitt_Positive_Customer_Suggestion_Ref) <- c("word","Sug_Customer")


Pittsburg_customer_suggestion_negative <- c("Fries here is not good. Think twice before order it.",
                                          "Chips here is not good. Think twice before order it.",
                                          "Nachos here is not good. Think twice before order it.",
                                          "Wings here is not good. Think twice before order it.",
                                          "Burgers here is not good. Think twice before order it.",
                                          "Monzarella stick here is not good. Think twice before order it.",
                                          "Cheese here is not good. Think twice before order cheese foods.",
                                          "Sandwich here is not good. Think twice before order it.",
                                          "Pizza here is not good. Think twice before order it.",
                                          "Taco here is not good. Maybe try some other snacks",
                                          "Shrimp here is not good. Think twice before order it.",
                                          "Fried food here is not good. Think twice before order it.",
                                          "Shrimp here is not good. Think twice before order it.",
                                          "Dipping source here is not good. Think twice before order it.",
                                          "Nugget here is not good. Think twice before order it.",
                                          "Peanut here is not good. Think twice before order it.",
                                          "Chicken here is not good. Think twice before order it.",
                                          "Beer here is not good. Think twice before order it.",
                                          "Wine here is not good. Think twice before order it.",
                                          "Cocktail here is not good. Think twice before order it.",
                                          "Gin here is not good. Think twice before order it.",
                                          "Service here is not good. Are you sure you want to go here?",
                                          "Service here is not good. Are you sure you want to go here?",
                                          "Restroom here is not so good.",
                                          "Price here is not so friendly. Be carefull.",
                                          "Band here seems to be not so good.")

Pitt_Negative_Customer_Suggestion_Ref <- as.data.frame(cbind(Pitt.test.sig.neg$word[c(1:14,16:26)],Pittsburg_customer_suggestion_negative))

colnames(Pitt_Negative_Customer_Suggestion_Ref) <- c("word","Sug_Customer")


## Make table
Pitt.busi.Sug.Customer <- Pitt.busi[,c(1,2,3)]

word.list.Pittsburg <- unique(c(Pitt_Positive_Customer_Suggestion_Ref$word,Pitt_Negative_Customer_Suggestion_Ref$word))

k <- length(word.list.Pittsburg)
m <- dim(Pitt.busi.Sug.Customer)[1]

empty.keys <- as.data.frame(matrix(0,nrow = m,ncol = k))
colnames(empty.keys) <- word.list.Pittsburg

Pitt.busi.Sug.Customer <- cbind(Pitt.busi.Sug.Customer,empty.keys)

Pitt.busi.filter30 <- Pitt.busi.Sug.Customer[which(Pitt.busi.Sug.Customer$review_count >= 30),]
Pitt.busi.filter30.original <- Pitt.busi[which(Pitt.busi$review_count >= 30),]

Pitt.busi.filter_below30 <- Pitt.busi.Sug.Customer[which(Pitt.busi.Sug.Customer$review_count < 30),]
Pitt.busi.filter_below30.original <- Pitt.busi[which(Pitt.busi$review_count < 30),]

m1 <- dim(Pitt.busi.filter30)[1]
m2 <- dim(Pitt.busi.filter_below30)[1]

for(i in 1:k){
  for(j in 1:m1){
    if(Pitt.busi.filter30.original[j,(3*i+2)] > 3*Pitt.busi.filter30.original[j,(3*i+3)]){
      Pitt.busi.filter30[j,(i+3)] <- 1
    }
    if(Pitt.busi.filter30.original[j,(3*i+3)] > 3*Pitt.busi.filter30.original[j,(3*i+2)]){
      Pitt.busi.filter30[j,(i+3)] <- -1
    }
  }
}


for(i in 1:k){
  for(j in 1:m2){
    if(Pitt.busi.filter_below30.original[j,(3*i+2)] > Pitt.busi.filter_below30.original[j,(3*i+3)]){
      Pitt.busi.filter_below30[j,(i+3)] <- 1
    }
    if(Pitt.busi.filter_below30.original[j,(3*i+3)] > Pitt.busi.filter_below30.original[j,(3*i+2)]){
      Pitt.busi.filter_below30[j,(i+3)] <- -1
    }
  }
}


############### Make  Tables for Pheonix
##Preparation
Phoen.test <- read.csv("Phoenix_test_df.csv")
Phoen.test <- Phoen.test[,-1]
Phoen.busi <- read.csv("business_count_Phoenix.csv")
Phoen.busi <- Phoen.busi[,-1]
row.names(Phoen.busi) <- NULL

Phoen.test.sig.pos <- Phoen.test[which(Phoen.test$enrich_pos_p_value < 0.05),]
Phoen.test.sig.neg <- Phoen.test[which(Phoen.test$enrich_neg_p_value < 0.05),]

row.names(Phoen.test.sig.neg) <- NULL
row.names(Phoen.test.sig.pos) <- NULL



## Postitive Suggestions list
Phoenix_customer_suggestion_positive <- c("Fries here is good, maybe you could have a try!",
                                            "Chips here is good, maybe you could have a try!",
                                            "Onion rings here is good, it could be good to have some onion rings with drinks!",
                                            "Nachos here is good, try some!",
                                            "Wings here are good, you could have a try!",
                                            "Burger here is good, try to have one if you're hungry!",
                                            "Monzarella stick is good. Have a try!",
                                            "Quesadillas is good. Have a try!",
                                            "Cheese here is good. Try some cheese snacks!",
                                            "Sandwich here is good. Try some cheese snacks!",
                                            "Pizza here is good. Try some!",
                                            "Taco here is good. Try some!",
                                            "Popcorn here is good. Try some!",
                                            "Bacon here is good. Try some food with bacon!",
                                            "Slider here is good. Try some mini-burger!",
                                            "Fried food here is good. Try some fried food here!",
                                            "Shrimp here is good. Try some seafood snacks.",
                                            "Salads here is good. And it's good for health.",
                                            "Dipping source here is good. Try it!",
                                            "Nugget here is good. Try some!",
                                            "Oysters here is good, try some seafood!",
                                            "Peanut here is good, have some snacks while drinking!",
                                            "Chicken here is good, try some if you're hungry!",
                                            "Beer here is good, enjoy it!",
                                            "Wine here is good, try some?",
                                            "Cocktail here is good, enjoy it!",
                                            "Spirit here is good, try some strong one?",
                                            "Rum here is good, try some and enjoy the tropical style!",
                                            "Whisky here is good, try some? Freedom and Whisky gang thegither!",
                                            "Vodka here is good. Want to try some strong one?",
                                            "Tequila here is good. Have a try?",
                                            "Gin here is good. Maybe have a try?",
                                            "Negroni here is good. Maybe have a try?",
                                            "Daiquiri here is good. Do you want to have a try?",
                                            "Manhattan here is good. Have a try of taste of Manhattan.",
                                            "Mojito here is good. Have a try of taste of Cuba.",
                                            "Margarita here is good. Try a cup and taste the sweetness and bitterness of love.",
                                            "Martini here is good. Do you want to try one of the most famous cocktails?",
                                            "Service here is good. Enjoy it!",
                                            "Service here is good. Enjoy it!",
                                            "There are parking lots. Don't worry.",
                                            "There is Wi-Fi. But remember to talk more with others.",
                                            "There is nice restroom.",
                                            "Price here is nice.",
                                            "Music here is good. Enjoy the atmosphere!",
                                            "Band here is good. Enjoy the atmosphere!",
                                            "Light here is good. Enjoy the atmosphere!",
                                            "The environment here is good, enjoy it!",
                                            "The atmosphere here is good, enjoy it!")




## Make_suggestion referrence table
Phoen_Positive_Customer_Suggestion_Ref <- as.data.frame(cbind(Phoen.test.sig.pos$word[c(1:43,45,50,51,52,53,54)],Phoenix_customer_suggestion_positive))
colnames(Phoen_Positive_Customer_Suggestion_Ref) <- c("word","Sug_Customer")

## Negative Suggestions list
Phoenix_customer_suggestion_negative <- c("Fries here is not good. Think twice before order it.",
                                            "Chips here is not good. Think twice before order it.",
                                            "Onion rings here is not good. Think twice before order it.",
                                            "Nachos here is not good. Think twice before order it.",
                                            "Wings here is not good. Think twice before order it.",
                                            "Burgers here is not good. Think twice before order it.",
                                            "Monzarella stick here is not good. Think twice before order it.",
                                            "Quesadillas here is not good. Think twice before order it.",
                                            "Cheese here is not good. Think twice before order cheese foods.",
                                            "Sandwich here is not good. Think twice before order it.",
                                            "Pizza here is not good. Think twice before order it.",
                                            "Taco here is not good. Maybe try some other snacks",
                                            "Bacon here is not good. Maybe try some other snacks",
                                            "Slider here is not good. Maybe try some other snacks",
                                            "Fried food here is not good. Think twice before order it.",
                                            "Shrimp here is not good. Think twice before order it.",
                                            "Dipping source here is not good. Think twice before order it.",
                                            "Oysters here is not good. Think twice before order it.",
                                            "Dipping source here is not good. Think twice before order it.",
                                            "Chicken here is not good. Think twice before order it.",
                                            "Beer here is not good. Think twice before order it.",
                                            "Wine here is not good. Think twice before order it.",
                                            "Spark here is not good. Think twice before order it.",
                                            "Gin here is not good. Think twice before order it.",
                                            "Service here is not good. Are you sure you want to go here?",
                                            "Service here is not good. Are you sure you want to go here?",
                                            "There seems to be not enough parking lot. Maying you could walk here.",
                                            "Restroom here is not so good.",
                                            "Price here is not so friendly. Be carefull.",
                                            "Music here is not so good.",
                                            "Band here seems to be not so good.",
                                            "Light here is not nice.")

## Make_suggestion referrence table
Phoen_Negative_Customer_Suggestion_Ref <- as.data.frame(cbind(Phoen.test.sig.neg$word[c(1:28,30,34:36)],Phoenix_customer_suggestion_negative))
colnames(Phoen_Negative_Customer_Suggestion_Ref) <- c("word","Sug_Customer")






## Make table
Phoen.busi.Sug.Customer <- Phoen.busi[,c(1,2,3)]

word.list.Phoenix <- unique(c(Phoen_Positive_Customer_Suggestion_Ref$word,Phoen_Negative_Customer_Suggestion_Ref$word))

k <- length(word.list.Phoenix)
m <- dim(Phoen.busi.Sug.Customer)[1]

empty.keys <- as.data.frame(matrix(0,nrow = m,ncol = k))
colnames(empty.keys) <- word.list.Phoenix

Phoen.busi.Sug.Customer <- cbind(Phoen.busi.Sug.Customer,empty.keys)

Phoen.busi.filter30 <- Phoen.busi.Sug.Customer[which(Phoen.busi.Sug.Customer$review_count >= 30),]
Phoen.busi.filter30.original <- Phoen.busi[which(Phoen.busi$review_count >= 30),]

Phoen.busi.filter_below30 <- Phoen.busi.Sug.Customer[which(Phoen.busi.Sug.Customer$review_count < 30),]
Phoen.busi.filter_below30.original <- Phoen.busi[which(Phoen.busi$review_count < 30),]

m1 <- dim(Phoen.busi.filter30)[1]
m2 <- dim(Phoen.busi.filter_below30)[1]

for(i in 1:k){
  for(j in 1:m1){
    if(Phoen.busi.filter30.original[j,(3*i+2)] > 3*Phoen.busi.filter30.original[j,(3*i+3)]){
      Phoen.busi.filter30[j,(i+3)] <- 1
    }
    if(Phoen.busi.filter30.original[j,(3*i+3)] > 3*Phoen.busi.filter30.original[j,(3*i+2)]){
      Phoen.busi.filter30[j,(i+3)] <- -1
    }
  }
}


for(i in 1:k){
  for(j in 1:m2){
    if(Phoen.busi.filter_below30.original[j,(3*i+2)] > Phoen.busi.filter_below30.original[j,(3*i+3)]){
      Phoen.busi.filter_below30[j,(i+3)] <- 1
    }
    if(Phoen.busi.filter_below30.original[j,(3*i+3)] > Phoen.busi.filter_below30.original[j,(3*i+2)]){
      Phoen.busi.filter_below30[j,(i+3)] <- -1
    }
  }
}

######################################## Make tables for Charlotte
##Preparation
Char.test <- read.csv("Charlotte_test_df.csv")
Char.test <- Char.test[,-1]
Char.busi <- read.csv("business_count_Charlotte.csv")
Char.busi <- Char.busi[,-1]
row.names(Char.busi) <- NULL

Char.test.sig.pos <- Char.test[which(Char.test$enrich_pos_p_value < 0.05),]
Char.test.sig.neg <- Char.test[which(Char.test$enrich_neg_p_value < 0.05),]

row.names(Char.test.sig.neg) <- NULL
row.names(Char.test.sig.pos) <- NULL











############### Customer Suggestion Fuction
customer_sug_function <- function(ID,city = "Madison"){
  sug <- c()
  if(city == "Madison"){
    city.busi <- Mad.busi
    city.busi.filter30 <- Mad.busi.filter30
    city.busi.filter_below30 <- Mad.busi.filter_below30
    city.Positive_Customer_Suggestion_Ref <- Mad.Positive_Customer_Suggestion_Ref
    city.Negative_Customer_Suggestion_Ref <- Mad.Negative_Customer_Suggestion_Ref
    loop_range <- length(word.list.Madison)
  }
  if(city == "Pittsburg"){
    city.busi <- Pitt.busi
    city.busi.filter30 <- Pitt.busi.filter30
    city.busi.filter_below30 <- Pitt.busi.filter_below30
    city.Positive_Customer_Suggestion_Ref <- Pitt.Positive_Customer_Suggestion_Ref
    city.Negative_Customer_Suggestion_Ref <- Pitt.Negative_Customer_Suggestion_Ref
    loop_range <- length(word.list.Pittsburg)
  }
  if(city == "Phoenix"){
    city.busi <- Phoen.busi
    city.busi.filter30 <- Phoen.busi.filter30
    city.busi.filter_below30 <- Phoen.busi.filter_below30
    city.Positive_Customer_Suggestion_Ref <- Phoen_Positive_Customer_Suggestion_Ref
    city.Negative_Customer_Suggestion_Ref <- Phoen_Negative_Customer_Suggestion_Ref
    loop_range <- length(word.list.Phoenix)
  }

  if(city.busi[which(city.busi$business_id == ID),3] >= 30){
    features <- city.busi.filter30[which(city.busi.filter30$business_id == ID),]
    temp<- paste("Our suggestion is based on ",features[1,3]," reviews.",sep = "")
    sug <- c(sug,temp)
    for(i in 1:loop_range){
      if(features[1,(i+3)] == 1){
        features.name <- colnames(city.busi.filter30)[i+3]
        temp<- paste(city.Positive_Customer_Suggestion_Ref[which(city.Positive_Customer_Suggestion_Ref$word == features.name),2],sep = "")
        sug <- c(sug,temp)
      }
    }
    for(i in 1:loop_range){
      if(features[1,(i+3)] == -1){
        features.name <- colnames(city.busi.filter30)[i+3]
        temp <- paste(city.Negative_Customer_Suggestion_Ref[which(city.Negative_Customer_Suggestion_Ref$word == features.name),2],sep = "")
        sug <- c(sug,temp)
      }
    }
  }
  if(city.busi[which(city.busi$business_id == ID),3] < 30){
    features <- city.busi.filter_below30[which(city.busi.filter_below30$business_id == ID),]
    temp<- paste("Our suggestion is based on ",features[1,3]," reviews, please keep in mind due to the limited number of reviews, the suggestion might not be so accurate.",sep = "")
    sug <- c(sug,temp)
    for(i in 1:loop_range){
      if(features[1,(i+3)] == 1){
        features.name <- colnames(city.busi.filter_below30)[i+3]
        temp<- paste(city.Positive_Customer_Suggestion_Ref[which(city.Positive_Customer_Suggestion_Ref$word == features.name),2],sep = "")
        sug <- c(sug,temp)
      }
    }
    for(i in 1:loop_range){
      if(features[1,(i+3)] == -1){
        features.name <- colnames(Mad.busi.filter_below30)[i+3]
        temp <- paste(city.Negative_Customer_Suggestion_Ref[which(city.Negative_Customer_Suggestion_Ref$word == features.name),2],sep = "")
        sug <- c(sug,temp)
      }
    }
  }
  return(sug)
}

############## Business Suggestion Fuction
business_sug_function <- function(ID, city = "Madison"){
sug <- c()
if(city == "Madison"){
  busi_words_list <- colnames(Mad.busi.filter30)[4:52]
  busi_words_list[c(9,14,16,19)] <- c("cheese food","bacon food","fried food","dipping source")
  if(Mad.busi[which(Mad.busi$business_id == ID),3] >= 30){
    features <- Mad.busi.filter30[which(Mad.busi.filter30$business_id == ID),]
    temp<- paste("Our suggestion is based on ",features[1,3]," reviews.",sep = "")
    sug <- c(sug,temp)
    ##Foods
    if(any(abs(features[1,4:26]) == 1)){
      temp <- paste("Good food can improve your rating star. ")
      sug <- c(sug,temp)}
    food_words <- busi_words_list[1:23]
    if(any(features[1,4:26] == 1)){
      temp <- paste(paste("Many of customers like your foods:"),
                    paste(food_words[which(t(features[1,4:26]) == 1)],
                          sep = ", ", "please keep doing it!"))
      sug <- c(sug,temp)
    }
    if(any(features[1,4:26] == -1)){
      temp1 <- paste("However, some of customer complains about foods:")
      temp2 <- paste(food_words[which(t(features[1,4:26]) == -1)],
                     "and if you can make it better, it would improve your rating star. ",sep = ", ")
      temp <- paste(temp1,temp2)
      sug <- c(sug,temp)
    }

    ##Drinks
    if(any(abs(features[1,27:43]) == 1)){
      temp <- paste("Drinks or alcohols are vital for a bar.")
      sug <- c(sug,temp)
      }
    drinks_words <- busi_words_list[24:40]
    if(any(features[1,27:43] == 1)){
      temp1 <- paste("Many of customers like your ")
      temp2 <- paste(drinks_words[which(t(features[1,27:43]) == 1)],
                     "keep doing it! ",sep = ", ")
      temp <- paste(temp1,temp2)
      sug <- c(sug,temp)
    }
    if(any(features[1,27:43] == -1)){
      temp1 <- paste("However, some of customer complains about")
      temp2 <- paste(drinks_words[which(t(features[1,27:43]) == -1)],
                     "and if you can make it better, it would improve your rating star.",sep = ", ")
      temp <- paste(temp1,temp2)
      sug <- c(sug,temp)
    }
    ##########################Service
    ## waiter/waitress
    if(any(c(any(abs(features[1,c(44:49)]) == -1),any(features[1,44:45] == 1)))){
      temp <- paste("Service is also important for the customer's satisfaction. ")
      sug <- c(sug,temp)
    }
    if(any(features[1,44:45] == 1)){
      temp <- paste("And some customers praised your waiter/ watress. Keep doing it!")
      sug <- c(sug,temp)
    }
    if(any(features[1,44:45] == -1)){
      temp <- paste("Service is also important for the customer's satisfaction")
      sug <- c(sug,temp)
      temp<- paste("But some customers complained about your waiter/watress. So maybe you could improve your management strategy, and make the service better.")
      sug <- c(sug,temp)
    }
    ##
    ## park
    if(any(features[1,46] == -1)){
      temp <- paste("There are some customers complained about your parking lot. If you can improve it, then your rating would be better.")
      sug <- c(sug,temp)
    }
    ## wifi
    if(any(features[1,47] == -1)){
      temp <- paste("There are some customers complained about your Wi-Fi. If you can improve it, then your rating would be better.")
      sug <- c(sug,temp)
    }
    if(any(features[1,47] == 1)){
      temp <- paste("There are some customers praised your Wi-Fi. Please keep doing it, that's good for your rating.")
      sug <- c(sug,temp)
    }
    ## Music
    if(any(features[1,48:49] == -1)){
      temp <- paste("There are some customers complained about your music.")
      sug <- c(sug,temp)
      temp <- paste("Maybe you can conduct a survey or hire a better DJ, and it would help to improve your rating.")
      sug <- c(sug,temp)
    }

  }
  if(Mad.busi[which(Mad.busi$business_id == ID),3] < 30){
    features <- Mad.busi.filter_below30[which(Mad.busi.filter_below30$business_id == ID),]
    temp<- paste("Our suggestion is based on ",features[1,3]," reviews. Please keep in mind due to the limited number of reviews, the suggestion might not be so accurate.",sep = "")
    sug <- c(sug,temp)
    ##Foods
    if(any(abs(features[1,4:26]) == 1)){
      temp <- paste("Good food can improve your rating star. ")
      sug <- c(sug,temp)
      }
    food_words <- busi_words_list[1:23]
    if(any(features[1,4:26] == 1)){
      temp <- paste(paste("Many of customers like your foods:"),
                    paste(food_words[which(t(features[1,4:26]) == 1)],
                          sep = ", ", "please keep doing it!"))
      sug <- c(sug,temp)
    }
    if(any(features[1,4:26] == -1)){
      temp1 <- paste("However, some of customer complains about foods:")
      temp2 <- paste(food_words[which(t(features[1,4:26]) == -1)],
                     "and if you can make it better, it would improve your rating star. ",sep = ", ")
      temp <- paste(temp1,temp2)
      sug <- c(sug,temp)
    }

    ##Drinks
    if(any(abs(features[1,27:43]) == 1)){cat("Drinks or alcohols are vital for a bar. ") }
    drinks_words <- busi_words_list[24:40]
    if(any(features[1,27:43] == 1)){
      temp1 <- paste("Many of customers like your ")
      temp2 <- paste(drinks_words[which(t(features[1,27:43]) == 1)],
                     "keep doing it! ",sep = ", ")
      temp <- paste(temp1,temp2)
      sug <- c(sug,temp)
    }
    if(any(features[1,27:43] == -1)){
      temp1 <- paste("However, some of customer complains about")
      temp2 <- paste(drinks_words[which(t(features[1,27:43]) == -1)],
                     "and if you can make it better, it would improve your rating star.",sep = ", ")
      temp <- paste(temp1,temp2)
      sug <- c(sug,temp)
    }
    ##########################Service
    ## waiter/waitress
    if(any(c(any(abs(features[1,c(44:49)]) == -1),any(features[1,44:45] == 1)))){
      temp <- paste("Service is also important for the customer's satisfaction. ")
      sug <- c(sug,temp)
    }
    if(any(features[1,44:45] == 1)){
      temp <- paste("And some customers praised your waiter/ watress. Keep doing it!")
      sug <- c(sug,temp)
    }
    if(any(features[1,44:45] == -1)){
      temp <- paste("Service is also important for the customer's satisfaction")
      sug <- c(sug,temp)
      temp<- paste("But some customers complained about your waiter/watress. So maybe you could improve your management strategy, and make the service better.")
      sug <- c(sug,temp)
    }
    ##
    ## park
    if(any(features[1,46] == -1)){
      temp <- paste("There are some customers complained about your parking lot. If you can improve it, then your rating would be better.")
      sug <- c(sug,temp)
    }
    ## wifi
    if(any(features[1,47] == -1)){
      temp <- paste("There are some customers complained about your Wi-Fi. If you can improve it, then your rating would be better.")
      sug <- c(sug,temp)
    }
    if(any(features[1,47] == 1)){
      temp <- paste("There are some customers praised your Wi-Fi. Please keep doing it, that's good for your rating.")
      sug <- c(sug,temp)
    }
    ## Music
    if(any(features[1,48:49] == -1)){
      temp <- paste("There are some customers complained about your music.")
      sug <- c(sug,temp)
      temp <- paste("Maybe you can conduct a survey or hire a better DJ, and it would help to improve your rating.")
      sug <- c(sug,temp)
    }

  }
}
if(city == "Pittsburg"){
  busi_words_list <- colnames(Pitt.busi.filter30)[4:49]
  busi_words_list[c(9,13,15,18,44)] <- c("cheese food","bacon food","fried food","dipping source","sandwich")
  if(Pitt.busi[which(Pitt.busi$business_id == ID),3] >= 30){
    features <- Pitt.busi.filter30[which(Pitt.busi.filter30$business_id == ID),]
    temp <- paste("Our suggestion is based on ",features[1,3]," reviews ",sep = "")
    sug <- c(sug,temp)
    ##Foods
    if(any(abs(features[1,c(4:25,47)]) == 1)){
      temp <- paste("Good food can improve your rating star.")
      sug <- c(sug,temp)
      }
    food_words <- busi_words_list[c(1:22,44)]
    if(any(features[1,c(4:25,47)] == 1)){
      temp1 <- paste("Many of customers like your foods: ")
      temp2 <- paste(food_words[which(t(features[1,c(4:25,47)]) == 1)],
          sep = ", ", "keep doing it!")
      temp <- paste(temp1,temp2)
      sug <- c(sug,temp)

    }
    if(any(features[1,c(4:25,47)] == -1)){
      temp1 <- paste("However, some of customer complains about foods: ")
      temp2 <- paste(food_words[which(t(features[1,c(4:25,47)]) == -1)],
          "and if you can make it better, it would improve your rating star.",sep = ", ")
      temp <- paste(temp1,temp2)
      sug <- c(sug,temp)
    }

    ##Drinks
    if(any(abs(features[1,26:39]) == 1)){
      temp <- paste("Good drinks or alcohols are vital for a bar.")
      sug <- c(sug,temp)
      }
    drinks_words <- busi_words_list[23:36]
    if(any(features[1,26:39] == 1)){
      temp1 <- paste("Many of customers like your ")
      temp2 <- paste(drinks_words[which(t(features[1,26:39]) == 1)],
          "keep doing it! ",sep = ", ")
      temp <- paste(temp1,temp2)
      sug <- c(sug,temp)
    }
    if(any(features[1,26:39] == -1)){
      temp1 <- paste("However, some of customer complains about ")
      temp2 <- paste(drinks_words[which(t(features[1,26:39]) == -1)],
          "and if you can make it better, it would improve your rating star.",sep = ", ")
      temp <- paste(temp1,temp2)
      sug <- c(sug,temp)
    }
    ##########################Service
    ## waiter/waitress
    if(any(c(any(abs(features[1,c(40:46,48:49)]) == -1),any(features[1,c(40:41,46)] == 1)))){
      temp <- paste("Service is also important for the customer's satisfaction.")
      sug <- c(sug,temp)
      }
    if(any(features[1,40:41] == 1)){
      temp <- paste("And some customers praised your waiter/watress. Keep doing it! ")
      sug <- c(sug,temp)
    }
    if(any(features[1,40:41] == -1)){
      temp <- paste("Service is also important for the customer's satisfaction. But some customers complained about your waiter/watress.So maybe you could improve your management strategy, and make the service better. ")
      sug <- c(sug,temp)
    }
    ##
    ## park
    if(any(features[1,42] == -1)){
      temp <- paste("There are some customers complained about your parking lot. If you can improve it, then your rating would be better.")
      sug <- c(sug,temp)
    }
    ## wifi
    if(any(features[1,43] == 1)){
      temp <- paste("There are some customers praised your Wi-Fi. That's good for your rating, keep doing it.")
      sug <- c(sug,temp)
    }
    ## Music
    if(any(features[1,44:45] == -1)){
      temp <- paste("There are some customers complained about your music. Maybe you can conduct a survey or hire a better DJ, and it would help to improve your rating.")
      sug <- c(sug,temp)
    }
    if(any(features[1,44:45] == 1)){
      temp <- paste("There are some customers loved your music. Keep doing it, that's good for your rating.")
      sug <- c(sug,temp)
    }
    ## light
    if(any(features[1,46] == 1)){
      temp <- paste("There are some customers loved your light. Keep doing it, that's good for your rating.")
      sug <- c(sug,temp)
    }
    ## restroom
    if(any(features[1,48] == -1)){
      temp <- paste("There are some customers complained about your restroom. Try to keep it clean, that's important for your rating.")
      sug <- c(sug,temp)
    }
  }

  if(Pitt.busi[which(Pitt.busi$business_id == ID),3] < 30){
    features <- Pitt.busi.filter_below30[which(Pitt.busi.filter_below30$business_id == ID),]
    temp <- paste("Our suggestion is based on ",features[1,3]," reviews. Please keep in mind due to the limited number of reviews, the suggestion might not be so accurate.",sep = "")
    sug <- c(sug,temp)
    ##Foods
    if(any(abs(features[1,c(4:25,47)]) == 1)){
      temp <- paste("Good food can improve your rating star.")
      sug <- c(sug,temp)
    }
    food_words <- busi_words_list[c(1:22,44)]
    if(any(features[1,c(4:25,47)] == 1)){
      temp1 <- paste("Many of customers like your foods: ")
      temp2 <- paste(food_words[which(t(features[1,c(4:25,47)]) == 1)],
                     sep = ", ", "keep doing it!")
      temp <- paste(temp1,temp2)
      sug <- c(sug,temp)

    }
    if(any(features[1,c(4:25,47)] == -1)){
      temp1 <- paste("However, some of customer complains about foods: ")
      temp2 <- paste(food_words[which(t(features[1,c(4:25,47)]) == -1)],
                     "and if you can make it better, it would improve your rating star.",sep = ", ")
      temp <- paste(temp1,temp2)
      sug <- c(sug,temp)
    }

    ##Drinks
    if(any(abs(features[1,26:39]) == 1)){
      temp <- paste("Good drinks or alcohols are vital for a bar.")
      sug <- c(sug,temp)
    }
    drinks_words <- busi_words_list[23:36]
    if(any(features[1,26:39] == 1)){
      temp1 <- paste("Many of customers like your ")
      temp2 <- paste(drinks_words[which(t(features[1,26:39]) == 1)],
                     "keep doing it! ",sep = ", ")
      temp <- paste(temp1,temp2)
      sug <- c(sug,temp)
    }
    if(any(features[1,26:39] == -1)){
      temp1 <- paste("However, some of customer complains about ")
      temp2 <- paste(drinks_words[which(t(features[1,26:39]) == -1)],
                     "and if you can make it better, it would improve your rating star.",sep = ", ")
      temp <- paste(temp1,temp2)
      sug <- c(sug,temp)
    }
    ##########################Service
    ## waiter/waitress
    if(any(c(any(abs(features[1,c(40:46,48:49)]) == -1),any(features[1,c(40:41,46)] == 1)))){
      temp <- paste("Service is also important for the customer's satisfaction.")
      sug <- c(sug,temp)
    }
    if(any(features[1,40:41] == 1)){
      temp <- paste("And some customers praised your waiter/watress. Keep doing it! ")
      sug <- c(sug,temp)
    }
    if(any(features[1,40:41] == -1)){
      temp <- paste("Service is also important for the customer's satisfaction. But some customers complained about your waiter/watress.So maybe you could improve your management strategy, and make the service better. ")
      sug <- c(sug,temp)
    }
    ##
    ## park
    if(any(features[1,42] == -1)){
      temp <- paste("There are some customers complained about your parking lot. If you can improve it, then your rating would be better.")
      sug <- c(sug,temp)
    }
    ## wifi
    if(any(features[1,43] == 1)){
      temp <- paste("There are some customers praised your Wi-Fi. That's good for your rating, keep doing it.")
      sug <- c(sug,temp)
    }
    ## Music
    if(any(features[1,44:45] == -1)){
      temp <- paste("There are some customers complained about your music. Maybe you can conduct a survey or hire a better DJ, and it would help to improve your rating.")
      sug <- c(sug,temp)
    }
    if(any(features[1,44:45] == 1)){
      temp <- paste("There are some customers loved your music. Keep doing it, that's good for your rating.")
      sug <- c(sug,temp)
    }
    ## light
    if(any(features[1,46] == 1)){
      temp <- paste("There are some customers loved your light. Keep doing it, that's good for your rating.")
      sug <- c(sug,temp)
    }
    ## restroom
    if(any(features[1,48] == -1)){
      temp <- paste("There are some customers complained about your restroom. Try to keep it clean, that's important for your rating.")
      sug <- c(sug,temp)
    }
  }

}
return(sug)
}


business_sug_function(ID ="M3neMoJoWn4jkMlZJ49aVw",city = "Pittsburg")
customer_sug_function(ID = "vWHKGoI4YnH1_Bju6Fm1OA", city = "Phoenix")
