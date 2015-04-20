# Bipartisan score data visualization
# 02-State_bar.R
# Author: Huade Huo <hh561@georgetown.edu>
# License: MIT

source("01-Data_cleaning.R")
library(ggplot2)
library(ggthemes)

# House 113th, both party
House_StateInfo <- House_Max_list %>% left_join(House_Min_list) %>% left_join(Hv_113_Mean)
House_StateInfo$State <-factor(House_StateInfo$State, 
                               levels=House_StateInfo[order(House_StateInfo$MaxScore,
                                                            decreasing = F), 
                                                      "State"])


ggplot(House_StateInfo,aes(y=State, x= MeanScore))+
    geom_errorbarh(aes(xmin=MinScore, xmax=MaxScore), colour = "#cccccc")+
    geom_text(aes(y=State, x = (MinScore-0.4), label = MinName, color = MinParty), size = 3)+
    geom_text(aes(y=State, x = (MaxScore+0.4), label = MaxName, color = MaxParty), size = 3)+
    geom_text(aes(y=State, x= MeanScore, label = State), color = "#8e44ad", size = 4)+
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
          panel.grid.minor=element_blank(),plot.background=element_blank())+
    ggtitle("Bipartisan score, 113th, House, Both party")
ggsave("Figures/State_ErrorBar_House_Overall.png")

# House 113th, Democratic
House_StateInfo_D <- House_Max_list_D %>% left_join(House_Min_list_D) %>% left_join(Hv_113_Mean_D)
House_StateInfo_D$State <-factor(House_StateInfo_D$State, 
                               levels=House_StateInfo_D[order(House_StateInfo_D$MaxScore,
                                                            decreasing = F), 
                                                      "State"])


ggplot(House_StateInfo_D,aes(y=State, x= MeanScore))+
    geom_errorbarh(aes(xmin=MinScore, xmax=MaxScore), colour = "#cccccc")+
    geom_text(aes(y=State, x = (MinScore-0.4), label = MinName, color = MinParty), size = 3)+
    geom_text(aes(y=State, x = (MaxScore+0.4), label = MaxName, color = MaxParty), size = 3)+
    geom_text(aes(y=State, x= MeanScore, label = State), color = "#8e44ad", size = 4)+
    geom_vline(aes(xintercept=0), linetype="dashed", colour = "#cccccc")+
    labs(x = "", y = "")+
    scale_color_manual(values=c("#3498db"), 
                       name="Party",
                       breaks=c("D"),
                       labels=c("Democratic"))+
    theme_few()+
    theme(legend.position="none")+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())+
    ggtitle("Bipartisan score, 113th, Senate, Democratic")
ggsave("Figures/State_ErrorBar_House_D.png")

# House 113th, Republican
House_StateInfo_R <- House_Max_list_R %>% left_join(House_Min_list_R) %>% left_join(Hv_113_Mean_R)
House_StateInfo_R$State <-factor(House_StateInfo_R$State, 
                                 levels=House_StateInfo_R[order(House_StateInfo_R$MaxScore,
                                                                decreasing = F), 
                                                          "State"])

ggplot(House_StateInfo_R,aes(y=State, x= MeanScore))+
    geom_errorbarh(aes(xmin=MinScore, xmax=MaxScore), colour = "#cccccc")+
    geom_text(aes(y=State, x = (MinScore-0.4), label = MinName, color = MinParty), size = 3)+
    geom_text(aes(y=State, x = (MaxScore+0.4), label = MaxName, color = MaxParty), size = 3)+
    geom_text(aes(y=State, x= MeanScore, label = State), color = "#8e44ad", size = 4)+
    geom_vline(aes(xintercept=0), linetype="dashed", colour = "#cccccc")+
    labs(x = "", y = "")+
    scale_color_manual(values=c( "#e74c3c"), 
                       name="Party",
                       breaks=c( "R"),
                       labels=c( "Republican"))+
    theme_few()+
    theme(legend.position="none")+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())+
    ggtitle("Bipartisan score, 113th, Senate, Republican")
ggsave("Figures/State_ErrorBar_House_R.png")

# Senate 113th, both party
Senate_StateInfo <- Senate_Max_list %>% left_join(Senate_Min_list) %>% left_join(Sv_113_Mean)
Senate_StateInfo$State <-factor(Senate_StateInfo$State, 
                               levels=Senate_StateInfo[order(Senate_StateInfo$MaxScore,
                                                            decreasing = F), 
                                                      "State"])


ggplot(Senate_StateInfo,aes(y=State, x= MeanScore))+
    geom_errorbarh(aes(xmin=MinScore, xmax=MaxScore), colour = "#cccccc")+
    geom_text(aes(y=State, x = (MinScore-0.4), label = MinName, color = MinParty), size = 3)+
    geom_text(aes(y=State, x = (MaxScore+0.4), label = MaxName, color = MaxParty), size = 3)+
    geom_text(aes(y=State, x= MeanScore, label = State), color = "#8e44ad", size = 4)+
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
          panel.grid.minor=element_blank(),plot.background=element_blank())+
    ggtitle("Bipartisan score, 113th, Senate, Both party")
ggsave("Figures/State_ErrorBar_Senate_Overall.png")
