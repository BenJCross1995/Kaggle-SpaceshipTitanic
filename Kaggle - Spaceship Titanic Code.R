#------------------------------------------------------------#
#------------------KAGGLE SPACESHIP TITANIC------------------#
#-------------------------BEN CROSS--------------------------#
#------------------------------------------------------------#

#----LOAD LIBRARIES----# ####
suppressPackageStartupMessages({
  library(devtools)
  library(tibble)     # Row names
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(stringr)  # String manipulation
})

#----FUNCTIONS USED FROM GITHUB----#

# One-Hot Encoding function
source_url("https://raw.github.com/BenJCross1995/UsefulFunctions/main/one_hot_ecoding.R")

# Missing Data Function
source_url("https://raw.github.com/BenJCross1995/UsefulFunctions/main/missingData.R")


#----LOAD DATA----# ####
train <- read.csv("Data/train.csv")
test <- read.csv("Data/test.csv")

# Add a transported column to test data just all true values for transformations
test$Transported = rep(TRUE, nrow(test))

#----VIEW DATA----# ####

head(train)

# Check out data structure
str(train)

# Check out column types
glimpse(train)

#----INITIAL TRANSFORMATIONS----# ####

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

#----EDA----# ####

eda_data <- rbind(train, test)

# Now we want to see what percentage of missing values are in each columns.

missing.data(eda_data)

# Examine the relationship between the Cryosleep variable, the shopping totals and 
# the missing Cryosleep data
cryo_vs_spend <- eda_data %>%
  group_by(CryoSleep) %>%
  summarise(across(.cols = c(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck),
                   .fns = ~mean(.x, na.rm = TRUE)))

# We can see here that All Cryosleep data which has a value of TRUE complete no purchases,
# which is to be expected. So, first thing to do is replace any NA values in the shopping
# categories to 0 when CryoSleep = TRUE. Then if the total value spent is > 0 (not including,
# missing values), we will set any missing CryoSleep values to FALSE as the person has spent money.
# If the total spent = 0 (not including missing values) then we know the CryoSleep = TRUE

eda_data <- eda_data %>%
  mutate(across(.cols = c(RoomService, FoodCourt, ShoppingMall, Spa, VRDeck),
                .fns = ~ifelse(is.na(.x) & CryoSleep == TRUE, 0, .x)),
         CryoSleep = ifelse(is.na(CryoSleep) & select(., RoomService:VRDeck) %>% rowSums(na.rm = TRUE) == 0,
                            TRUE, CryoSleep),
         CryoSleep = ifelse(is.na(CryoSleep) & select(., RoomService:VRDeck) %>% rowSums(na.rm = TRUE) > 0,
                            FALSE, CryoSleep),
         TotalSpend = select(., RoomService:VRDeck) %>% rowSums(na.rm = TRUE))

# However now we cannot address the NA values for the spend columns using just the Cryosleep as there
# could be other influences on the column. We need to investigate more.

missing.data(eda_data)

# Now want to check the percentage split between the target values in the training set
ggplot(eda_data %>% filter(DataType == "train"),
       aes(x = Transported)) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))
# Can see they're basically equal

# Steps from book; 1. Distinguish features, 2. Univariate Analysis, 3. Bivariate and Multivariate Analysis
# 4. Detect Aberrant and Missing Values, 5. Detect Outlier, 6. Feature Engineering

# Univariate Analysis
# Firstly want to deal with the character variables, for this i will again use bar/histograms
str(eda_data)

# I'll view CryoSleep and VIP First
ggplot(eda_data,
       aes(x = CryoSleep)) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))

ggplot(eda_data,
       aes(x = VIP)) +
  geom_bar(aes(y = after_stat(count)/sum(after_stat(count))))

#----COMPLETE ONE-HOT ENCODING----# ####
vars_to_onehot <- c("HomePlanet", "CryoSleep", "Deck", "Side", "Destination", "VIP") 
final_data <- one_hot_encoding(eda_data, vars_to_onehot)

