## 1. Read in the dataset
#Madison
options(stringsAsFactors = F)
Mad.busi <- read.csv("Mad.busi.csv")
Mad.busi.filter30 <- read.csv("Mad.busi.filter30.csv")
Mad.busi.filter_below30 <- read.csv("Mad.busi.filter_below30.csv")

Mad.Negative_Customer_Suggestion_Ref <- read.csv("Mad_Negative_Customer_Suggestion_Ref.csv")
Mad.Positive_Customer_Suggestion_Ref <- read.csv("Mad_Positive_Customer_Suggestion_Ref.csv")

#Pittsburg
Pitt.Positive_Customer_Suggestion_Ref <- read.csv("Pitt.Positive_Customer_Suggestion_Ref.csv")
Pitt.Negative_Customer_Suggestion_Ref <- read.csv("Pitt.Negative_Customer_Suggestion_Ref.csv")
#Fucntion Define

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

  if(city.busi[which(city.busi$business_id == ID),3] >= 30){
    features <- city.busi.filter30[which(city.busi.filter30$business_id == ID),]
    temp<- paste("Our suggestion is based on ",features[1,3]," reviews \n\n",sep = "")
    sug <- c(sug,temp)
    for(i in 1:loop_range){
      if(features[1,(i+3)] == 1){
        features.name <- colnames(city.busi.filter30)[i+3]
        temp<- paste(city.Positive_Customer_Suggestion_Ref[which(city.Positive_Customer_Suggestion_Ref$word == features.name),2],"\n",sep = "")
        sug <- c(sug,temp)
      }
    }
    for(i in 1:loop_range){
      if(features[1,(i+3)] == -1){
        features.name <- colnames(city.busi.filter30)[i+3]
        temp <- paste(city.Negative_Customer_Suggestion_Ref[which(city.Negative_Customer_Suggestion_Ref$word == features.name),2],"\n",sep = "")
        sug <- c(sug,temp)
      }
    }
  }
  if(city.busi[which(city.busi$business_id == ID),3] < 30){
    features <- city.busi.filter_below30[which(city.busi.filter_below30$business_id == ID),]
    temp<- paste("Our suggestion is based on ",features[1,3]," reviews, please keep in mind due to the limited number of reviews, the suggestion might not be so accurate. \n\n",sep = "")
    sug <- c(sug,temp)
    for(i in 1:loop_range){
      if(features[1,(i+3)] == 1){
        features.name <- colnames(city.busi.filter_below30)[i+3]
        temp<- paste(city.Positive_Customer_Suggestion_Ref[which(city.Positive_Customer_Suggestion_Ref$word == features.name),2],"\n",sep = "")
        sug <- c(sug,temp)
      }
    }
    for(i in 1:loop_range){
      if(features[1,(i+3)] == -1){
        features.name <- colnames(Mad.busi.filter_below30)[i+3]
        temp <- paste(city.Negative_Customer_Suggestion_Ref[which(city.Negative_Customer_Suggestion_Ref$word == features.name),2],"\n",sep = "")
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
