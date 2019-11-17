options(stringsAsFactors = F)

if(T){
  #readin data
  Mad.test <- read.csv("Madison_test_df.csv")
  Mad.busi <- read.csv("business_count_Madison.csv")
  Mad.busi <- Mad.busi[,-1]
  Mad.test <- Mad.test[,-1]
  row.names(Mad.busi) <- NULL
  row.names(Mad.test) <- NULL
  #
  Mad.test.sig.pos <- Mad.test[which(Mad.test$enrich_pos_p_value < 0.05),]
  Mad.test.sig.neg <- Mad.test[which(Mad.test$enrich_neg_p_value < 0.05),]
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



  Mad_Postive_Customer_Suggestion_Ref <- as.data.frame(cbind(Mad.test.sig.pos$word[c(1:40,41:44,51,52)],Madison_customer_suggestion_positive))

  colnames(Mad_Postive_Customer_Suggestion_Ref) <- c("word","Sug_Customer")

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

  Mad_Negative_Customer_Suggestion_Ref <- as.data.frame(cbind(Mad.test.sig.neg$word[c(1:20)],Madison_customer_suggestion_negative))

  colnames(Mad_Postive_Customer_Suggestion_Ref) <- c("word","Sug_Customer")






  ####################### Make feature tables for Madison
  ## Make table
  Mad.busi.Sug.Customer <- Mad.busi[,c(1,2,3)]

  word.list.Madison <- unique(c(Mad_Positive_Customer_Suggestion_Ref$word,Mad_Negative_Customer_Suggestion_Ref$word))

  k <- length(word.list.Madison)
  m <- dim(Mad.busi.Sug.Customer)[1]

  k_full <- length(Mad.test$word)

  empty.keys <- as.data.frame(matrix(0,nrow = m,ncol = k_full))
  colnames(empty.keys) <- Mad.test$word

  Mad.busi.Sug.Customer <- cbind(Mad.busi.Sug.Customer,empty.keys)

  Mad.busi.filter30 <- Mad.busi.Sug.Customer[which(Mad.busi.Sug.Customer$review_count >= 30),]
  Mad.busi.filter30.original <- Mad.busi[which(Mad.busi$review_count >= 30),]

  Mad.busi.filter_below30 <- Mad.busi.Sug.Customer[which(Mad.busi.Sug.Customer$review_count < 30),]
  Mad.busi.filter_below30.original <- Mad.busi[which(Mad.busi$review_count < 30),]

  m1 <- dim(Mad.busi.filter30)[1]
  m2 <- dim(Mad.busi.filter_below30)[1]


  for(i in 1:k){
    for(j in 1:m1){
      if(Mad.busi.filter30.original[j,(3*i+2)] > 3*Mad.busi.filter30.original[j,(3*i+3)]){
        Mad.busi.filter30[j,(i+3)] <- 1
      }
      if(Mad.busi.filter30.original[j,(3*i+3)] > 3*Mad.busi.filter30.original[j,(3*i+2)]){
        Mad.busi.filter30[j,(i+3)] <- -1
      }
    }
  }


  for(i in 1:k){
    for(j in 1:m2){
      if(Mad.busi.filter_below30.original[j,(3*i+2)] > Mad.busi.filter_below30.original[j,(3*i+3)]){
        Mad.busi.filter_below30[j,(i+3)] <- 1
      }
      if(Mad.busi.filter_below30.original[j,(3*i+3)] > Mad.busi.filter_below30.original[j,(3*i+2)]){
        Mad.busi.filter_below30[j,(i+3)] <- -1
      }
    }
  }

  Mad.busi.filter30 <- Mad.busi.filter30[,c(1:3,which(colnames(Mad.busi.filter30) %in% word.list.Madison))]
  Mad.busi.filter_below30 <- Mad.busi.filter_below30[,c(1:3,which(colnames(Mad.busi.filter_below30) %in% word.list.Madison))]

}
if(T){

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

  k_full <- length(Pitt.test$word)

  empty.keys <- as.data.frame(matrix(0,nrow = m,ncol = k_full))
  colnames(empty.keys) <- Pitt.test$word

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

  Pitt.busi.filter30 <- Pitt.busi.filter30[,c(1:3,which(colnames(Pitt.busi.filter30) %in% word.list.Pittsburg))]
  Pitt.busi.filter_below30 <- Pitt.busi.filter_below30[,c(1:3,which(colnames(Pitt.busi.filter_below30) %in% word.list.Pittsburg))]


}
if(T){

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

  k_full <- length(Phoen.test$word)

  empty.keys <- as.data.frame(matrix(0,nrow = m,ncol = k_full))
  colnames(empty.keys) <- Phoen.test$word

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

  Phoen.busi.filter30 <- Phoen.busi.filter30[,c(1:3,which(colnames(Phoen.busi.filter30) %in% word.list.Phoenix))]
  Phoen.busi.filter_below30 <- Phoen.busi.filter_below30[,c(1:3,which(colnames(Phoen.busi.filter_below30) %in% word.list.Phoenix))]



}
if(T){

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



  ## Postitive Suggestions list
  Charlotte_customer_suggestion_positive <- c("Fries here is good, maybe you could have a try!",
                                              "Chips here is good, maybe you could have a try!",
                                              "Onion rings here is good, it could be good to have some onion rings with drinks!",
                                              "Nachos here is good, try some!",
                                              "Wings here are good, you could have a try!",
                                              "Burger here is good, try to have one if you're hungry!",
                                              "Monzarella stick is good. Have a try!",
                                              "Quesadillas is good. Have a try!",
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
                                              "Beer here is good, enjoy it!",
                                              "Cocktail here is good, enjoy it!",
                                              "Spark here is good, enjoy it!",
                                              "Spirit here is good, try some strong one?",
                                              "Rum here is good, try some and enjoy the tropical style!",
                                              "Whisky here is good, try some? Freedom and Whisky gang thegither!",
                                              "Vodka here is good. Want to try some strong one?",
                                              "Tequila here is good. Have a try?",
                                              "Brandy here is good. Have a try?",
                                              "Gin here is good. Maybe have a try?",
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
  Char_Positive_Customer_Suggestion_Ref <- as.data.frame(cbind(Char.test.sig.pos$word[c(1:17,19:39,41,46:50)],Charlotte_customer_suggestion_positive))
  colnames(Char_Positive_Customer_Suggestion_Ref) <- c("word","Sug_Customer")

  ## Negative Suggestions list
  Char_customer_suggestion_negative <- c("Fries here is not good. Think twice before order it.",
                                         "Chips here is not good. Think twice before order it.",
                                         "Onion rings here is not good. Think twice before order it.",
                                         "Nachos here is not good. Think twice before order it.",
                                         "Wings here is not good. Think twice before order it.",
                                         "Burgers here is not good. Think twice before order it.",
                                         "Monzarella stick here is not good. Think twice before order it.",
                                         "Cheese here is not good. Think twice before order cheese foods.",
                                         "Sandwich here is not good. Think twice before order it.",
                                         "Pizza here is not good. Think twice before order it.",
                                         "Taco here is not good. Maybe try some other snacks",
                                         "Slider here is not good. Maybe try some other snacks",
                                         "Fried food here is not good. Think twice before order it.",
                                         "Shrimp here is not good. Think twice before order it.",
                                         "Dipping source here is not good. Think twice before order it.",
                                         "Chicken here is not good. Think twice before order it.",
                                         "Beer here is not good. Think twice before order it.",
                                         "Rum here is not good. Think twice before order it.",
                                         "Gin here is not good. Think twice before order it.",
                                         "Negroni here is not good. Think twice before order it.",
                                         "Margarita here is not good. Think twice before order it.",
                                         "Service here is not good. Are you sure you want to go here?",
                                         "Service here is not good. Are you sure you want to go here?",
                                         "There seems to be not enough parking lot. Maying you could walk here.",
                                         "Restroom here is not so good.",
                                         "Price here is not so friendly. Be carefull.",
                                         "Band here seems to be not so good.")

  ## Make_suggestion referrence table
  Char_Negative_Customer_Suggestion_Ref <- as.data.frame(cbind(Char.test.sig.neg$word[c(1:15,17:26,28,33)],Char_customer_suggestion_negative))
  colnames(Char_Negative_Customer_Suggestion_Ref) <- c("word","Sug_Customer")


  ## Make table
  Char.busi.Sug.Customer <- Char.busi[,c(1,2,3)]

  word.list.Charlotte <- unique(c(Char_Positive_Customer_Suggestion_Ref$word,Char_Negative_Customer_Suggestion_Ref$word))

  k <- length(word.list.Charlotte)
  m <- dim(Char.busi.Sug.Customer)[1]

  k_full <- length(Char.test$word)

  empty.keys <- as.data.frame(matrix(0,nrow = m,ncol = k_full))
  colnames(empty.keys) <- Char.test$word

  Char.busi.Sug.Customer <- cbind(Char.busi.Sug.Customer,empty.keys)

  Char.busi.filter30 <- Char.busi.Sug.Customer[which(Char.busi.Sug.Customer$review_count >= 30),]
  Char.busi.filter30.original <- Char.busi[which(Char.busi$review_count >= 30),]

  Char.busi.filter_below30 <- Char.busi.Sug.Customer[which(Char.busi.Sug.Customer$review_count < 30),]
  Char.busi.filter_below30.original <- Char.busi[which(Char.busi$review_count < 30),]

  m1 <- dim(Char.busi.filter30)[1]
  m2 <- dim(Char.busi.filter_below30)[1]


  for(i in 1:k){
    for(j in 1:m1){
      if(Char.busi.filter30.original[j,(3*i+2)] > 3*Char.busi.filter30.original[j,(3*i+3)]){
        Char.busi.filter30[j,(i+3)] <- 1
      }
      if(Char.busi.filter30.original[j,(3*i+3)] > 3*Char.busi.filter30.original[j,(3*i+2)]){
        Char.busi.filter30[j,(i+3)] <- -1
      }
    }
  }


  for(i in 1:k){
    for(j in 1:m2){
      if(Char.busi.filter_below30.original[j,(3*i+2)] > Char.busi.filter_below30.original[j,(3*i+3)]){
        Char.busi.filter_below30[j,(i+3)] <- 1
      }
      if(Char.busi.filter_below30.original[j,(3*i+3)] > Char.busi.filter_below30.original[j,(3*i+2)]){
        Char.busi.filter_below30[j,(i+3)] <- -1
      }
    }
  }

  Char.busi.filter30 <- Char.busi.filter30[,c(1:3,which(colnames(Char.busi.filter30) %in% word.list.Charlotte))]
  Char.busi.filter_below30 <- Char.busi.filter_below30[,c(1:3,which(colnames(Char.busi.filter_below30) %in% word.list.Charlotte))]


}



## outputing tables

#Madison

write.csv(Mad.busi,"Mad.busi.csv",row.names = F)
write.csv(Mad.busi.filter30,"Mad.busi.filter30.csv",row.names = F)
write.csv(Mad.busi.filter_below30,"Mad.busi.filter_below30.csv",row.names = F)

write.csv(Mad_Negative_Customer_Suggestion_Ref,"Mad_Negative_Customer_Suggestion_Ref.csv",row.names = F)
write.csv(Mad_Positive_Customer_Suggestion_Ref,"Mad_Positive_Customer_Suggestion_Ref.csv",row.names = F)

#Pittsburgh
write.csv(Pitt.busi,"Pitt.busi.csv",row.names = F)
write.csv(Pitt.busi.filter30,"Pitt.busi.filter30.csv",row.names = F)
write.csv(Pitt.busi.filter_below30,"Pitt.busi.filter_below30.csv",row.names = F)

write.csv(Pitt_Positive_Customer_Suggestion_Ref,"Pitt_Positive_Customer_Suggestion_Ref.csv",row.names = F)
write.csv(Pitt_Negative_Customer_Suggestion_Ref,"Pitt_Negative_Customer_Suggestion_Ref.csv",row.names = F)

#Charlotte
write.csv(Char.busi,"Char.busi.csv",row.names = F)
write.csv(Char.busi.filter30,"Char.busi.filter30.csv",row.names = F)
write.csv(Char.busi.filter_below30,"Char.busi.filter_below30.csv",row.names = F)

write.csv(Char_Positive_Customer_Suggestion_Ref,"Char_Positive_Customer_Suggestion_Ref.csv",row.names = F)
write.csv(Char_Negative_Customer_Suggestion_Ref,"Char_Negative_Customer_Suggestion_Ref.csv",row.names = F)

#Phoenix
write.csv(Phoen.busi,"Phoen.busi.csv",row.names = F)
write.csv(Phoen.busi.filter30,"Phoen.busi.filter30.csv",row.names = F)
write.csv(Phoen.busi.filter_below30,"Phoen.busi.filter_below30.csv",row.names = F)

write.csv(Phoen_Positive_Customer_Suggestion_Ref,"Phoen_Positive_Customer_Suggestion_Ref.csv",row.names = F)
write.csv(Phoen_Negative_Customer_Suggestion_Ref,"Phoen_Negative_Customer_Suggestion_Ref.csv",row.names = F)


save(word.list.Madison,file = "word.list.Madison.Rdata")
save(word.list.Charlotte,file = "word.list.Charlotte.Rdata")
save(word.list.Phoenix,file = "word.list.Phoenix.Rdata")
save(word.list.Pittsburgh,file = "word.list.Pittsburgh.Rdata")

