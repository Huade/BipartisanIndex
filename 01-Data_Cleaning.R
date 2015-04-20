# Bipartisan score data visualization
# 01-Data cleaning.R
# Author: Huade Huo <hh561@georgetown.edu>
# License: MIT

library(dplyr)

# Read bipartisan index data
BP <- read.csv("Data/Score.csv", stringsAsFactors = F)
BP$Name <- paste(BP$FirstName, BP$LastName, sep = " ")
State_list <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID",
                "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
                "MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK",
                "OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV",
                "WI","WY")

# ---------------------------------
# 113th House
# ---------------------------------

Hv_113 <- BP %>% 
    filter(State %in% State_list) %>% 
    filter(Chamber == "House") %>% 
    select(-FirstName, -LastName) %>% 
    filter(Session == 113)

# State Mean (House, 113)
Hv_113_Mean <- Hv_113  %>% 
    group_by(State) %>% 
    summarise(MeanScore = mean(Score))

# State Max/Min (House, 113)
Hv_113_Max <- Hv_113  %>% 
    group_by(State) %>% 
    summarise(Score = max(Score))

Hv_113_Min <- Hv_113  %>% 
    group_by(State) %>% 
    summarise(Score = min(Score))

# Max/Min list (House, 113)
House_Max_list <- inner_join(Hv_113, Hv_113_Max, by = c("Score", "State"))
House_Min_list <- inner_join(Hv_113, Hv_113_Min, by = c("Score", "State"))

names(House_Max_list) <- paste("Max",names(House_Max_list),sep = "")
names(House_Max_list)[2] <- "State"
names(House_Min_list) <- paste("Min",names(House_Min_list),sep = "")
names(House_Min_list)[2] <- "State"

# ---------------------------------
# 113th House, Democratic
# ---------------------------------

Hv_113_D <- BP %>% 
    filter(State %in% State_list) %>% 
    filter(Chamber == "House") %>% 
    select(-FirstName, -LastName) %>% 
    filter(Session == 113) %>%
    filter(Party == "D")

# State Mean (House, 113, Democratic)
Hv_113_Mean_D <- Hv_113_D  %>% 
    group_by(State) %>% 
    summarise(MeanScore = mean(Score))

# State Max/Min (House, 113, Democratic)
Hv_113_Max_D <- Hv_113_D  %>% 
    group_by(State) %>% 
    summarise(Score = max(Score))

Hv_113_Min_D <- Hv_113_D  %>% 
    group_by(State) %>% 
    summarise(Score = min(Score))

# Max/Min list (House, 113, Democratic)
House_Max_list_D <- inner_join(Hv_113_D, Hv_113_Max_D, by = c("Score", "State"))
House_Min_list_D <- inner_join(Hv_113_D, Hv_113_Min_D, by = c("Score", "State"))

names(House_Max_list_D) <- paste("Max",names(House_Max_list_D),sep = "")
names(House_Max_list_D)[2] <- "State"
names(House_Min_list_D) <- paste("Min",names(House_Min_list_D),sep = "")
names(House_Min_list_D)[2] <- "State"

# ---------------------------------
# 113th House, Republican
# ---------------------------------

Hv_113_R <- BP %>% 
    filter(State %in% State_list) %>% 
    filter(Chamber == "House") %>% 
    select(-FirstName, -LastName) %>% 
    filter(Session == 113) %>%
    filter(Party == "R")

# State Mean (House, 113, Republican)
Hv_113_Mean_R <- Hv_113_R  %>% 
    group_by(State) %>% 
    summarise(MeanScore = mean(Score))

# State Max/Min (House, 113, Republican)
Hv_113_Max_R <- Hv_113_R  %>% 
    group_by(State) %>% 
    summarise(Score = max(Score))

Hv_113_Min_R <- Hv_113_R  %>% 
    group_by(State) %>% 
    summarise(Score = min(Score))

# Max/Min list (House, 113, Republican)
House_Max_list_R <- inner_join(Hv_113_R, Hv_113_Max_R, by = c("Score", "State"))
House_Min_list_R <- inner_join(Hv_113_R, Hv_113_Min_R, by = c("Score", "State"))

names(House_Max_list_R) <- paste("Max",names(House_Max_list_R),sep = "")
names(House_Max_list_R)[2] <- "State"
names(House_Min_list_R) <- paste("Min",names(House_Min_list_R),sep = "")
names(House_Min_list_R)[2] <- "State"

# ---------------------------------
# 113th Senate
# ---------------------------------
Sv_113 <- BP %>% 
    filter(State %in% State_list) %>% 
    filter(Chamber == "Senate") %>% 
    select(-FirstName, -LastName) %>% 
    filter(Session == 113)

# State Mean (Senate)
Sv_113_Mean <- Sv_113  %>% 
    group_by(State) %>% 
    summarise(MeanScore = mean(Score))

# State Max/Min (Senate, 113)
Sv_113_Max <- Sv_113  %>% 
    group_by(State) %>% 
    summarise(Score = max(Score))

Sv_113_Min <- Sv_113  %>% 
    group_by(State) %>% 
    summarise(Score = min(Score))

# Max/Min list (Senate, 113)
Senate_Max_list <- inner_join(Sv_113, Sv_113_Max, by = c("Score", "State"))
Senate_Min_list <- inner_join(Sv_113, Sv_113_Min, by = c("Score", "State"))

names(Senate_Max_list) <- paste("Max",names(Senate_Max_list),sep = "")
names(Senate_Max_list)[2] <- "State"
names(Senate_Min_list) <- paste("Min",names(Senate_Min_list),sep = "")
names(Senate_Min_list)[2] <- "State"

# ---------------------------------
# Trend data
# ---------------------------------

Hv_all <- BP %>% 
    filter(State %in% State_list) %>% 
    filter(Chamber == "House") %>% 
    select(-FirstName, -LastName)

Sv_all <- BP %>% 
    filter(State %in% State_list) %>% 
    filter(Chamber == "Senate") %>% 
    select(-FirstName, -LastName)

Hv_trend <- Hv_all %>% 
    group_by(Session) %>% 
    summarise(YearMean = mean(Score))

Hv_trend$Chamber <- "House"

Sv_trend <- Sv_all %>% 
    group_by(Session) %>% 
    summarise(YearMean = mean(Score))

Sv_trend$Chamber <- "Senate"

Hv_Sv_tread <- rbind(Hv_trend, Sv_trend)
