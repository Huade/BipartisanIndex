library(ggplot2)
library(ggthemes)
library(dplyr)

Hv <- read.csv("Data/Hv.csv")
Hv$Name <- paste(Hv$NameFirst, Hv$NameLast, sep = " ")
State_list <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID",
                "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS",
                "MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK",
                "OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV",
                "WI","WY")
Hv <- Hv %>% filter(State %in% State_list) %>% 
    select(-NameFirst, -NameLast)
Hv_Mean <- Hv_Max <- Hv  %>% 
    group_by(State) %>% 
    summarise(MeanScore = mean(Score))

Hv_Max <- Hv  %>% 
    group_by(State) %>% 
    summarise(Score = max(Score))

Hv_Min <- Hv  %>% 
    group_by(State) %>% 
    summarise(Score = min(Score))

Max_list <- inner_join(Hv, Hv_Max, by = c("Score", "State"))
Min_list <- inner_join(Hv, Hv_Min, by = c("Score", "State"))

names(Max_list) <- paste("Max",names(Max_list),sep = "")
names(Max_list)[2] <- "State"
names(Min_list) <- paste("Min",names(Min_list),sep = "")
names(Min_list)[2] <- "State"

StateInfo <- Max_list %>% left_join(Min_list) %>% left_join(Hv_Mean)
StateInfo$State <-factor(StateInfo$State, levels=StateInfo[order(StateInfo$MinScore,decreasing = T), "State"])


ggplot(StateInfo,aes(y=State, x= MeanScore))+
    geom_errorbarh(aes(xmin=MinScore, xmax=MaxScore), colour = "#cccccc")+
    geom_text(aes(y=State, x = (MinScore-0.4), label = MinName, color = MinParty))+
    geom_text(aes(y=State, x = (MaxScore+0.4), label = MaxName, color = MaxParty))+
    geom_text(aes(y=State, x= MeanScore, label = State), color = "#8e44ad")+
    geom_vline(aes(xintercept=0), linetype="dashed", colour = "#cccccc")+
    labs(x = "", y = "")+
    scale_color_manual(values=c("#3498db", "#e74c3c"), 
                      name="Party",
                      breaks=c("D", "R"),
                      labels=c("Democratic", "Republican"))+
    theme_few()+
    theme(legend.position="none")+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())