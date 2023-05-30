#------------------------------------------------------------#
#------------------KAGGLE SPACESHIP TITANIC------------------#
#-------------------------BEN CROSS--------------------------#
#------------------------------------------------------------#

#----LOAD LIBRARIES----# ####
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(stringr)  # String manipulation
})


#----LOAD DATA----# ####
train <- read.csv("Data/train.csv")
test <- read.csv("Data/test.csv")

# Add a transported column to test data just all true values for transformations
test$Transported = rep(TRUE, nrow(test))

# Check out data structure
str(train)

# Check out column types
glimpse(train)

# Initial data transformations
Initial_Data_Transforamtion <- function(data){
  #' Converts the logical columns to a logical data type, splits
  #' the PassengerID column to get the passenger group, returns the surname
  #' of the passengers, seperated the Cabin column into it's respective Deck,
  #' Num and Side values. It will also combine the Deck and Num values in case
  #' Any missing values can be imputed based on this grouping and finally relocates
  #' created columns to where they fit best.
  #' 
  #' @param data - Either test or train dataframe
  #' 
  #' @return - A dataframe
  data <- data %>%
    mutate(CryoSleep = as.logical(CryoSleep),
           Transported = as.logical(Transported),
           VIP = as.logical(VIP),
           PassengerGroup = str_extract(PassengerId, "^[^_]+"),
           Surname = str_extract(Name, "(?<=\\s)[^\\s]+$")) %>%
    separate(Cabin, into = c("Deck", "Num", "Side"), sep = "/", remove = FALSE) %>%
    mutate(DeckNum = ifelse(is.na(Num), NA, paste0(Deck, "-", Num))) %>%
    relocate(Transported) %>%
    relocate(PassengerGroup, .after = 'PassengerId') %>%
    relocate(DeckNum, .after = 'Cabin') %>%
    suppressWarnings()
  
  return(data)
}

# Preform the transformations
train <- Initial_Data_Transforamtion(train)
test <- Initial_Data_Transforamtion(test)

# For EDA purposes it will be easier to append the data together

train$DataType <-  rep("train", nrow(train))
test$DataType <- rep("test", nrow(test))

eda_data <- rbind(train, test)

eda_data %>%
  group_by(DataType, CryoSleep) %>%
  summarise(across(.cols = c(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck),
                   .fns = ~mean(.x, na.rm = TRUE)))

