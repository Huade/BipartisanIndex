# Bipartisan score data visualization
# 04-Trend_plot.R
# Author: Huade Huo <hh561@georgetown.edu>
# License: MIT

library(ggplot2)
library(ggthemes)

source("01-Data_Cleaning.R")

# House trend
ggplot(Hv_trend,aes(y=YearMean, x= Session))+
    geom_bar(stat="identity", fill="#2ecc71")+
    geom_hline(aes(yintercept=0), linetype="dashed")+
    theme_few()+
    theme(legend.position="none")+
    ggtitle("Bipartisan score trend, all sessions, House")
ggsave("Figures/Trend_Bar_House_Overall.png")

# Senate trend
ggplot(Sv_trend,aes(y=YearMean, x= Session))+
    geom_bar(stat="identity",fill="#f39c12")+
    geom_hline(aes(yintercept=0), linetype="dashed")+
    theme_few()+
    theme(legend.position="none")+
    ggtitle("Bipartisan score trend, all sessions, Senate")
ggsave("Figures/Trend_Bar_Senate_Overall.png")

# House and Senate Trend
ggplot(Hv_Sv_tread, aes(y=YearMean, x = Session, color = Chamber))+
    geom_hline(aes(yintercept=0), linetype="dashed")+
    geom_line()+
    ggtitle("Bipartisan score trend, all sessions")+
    scale_color_manual(values=c("#2ecc71", "#f39c12"), 
                       name="Chamber",
                       breaks=c("House", "Senate"),
                       labels=c("House", "Senate"))+
    scale_x_continuous(breaks=seq(103,113,1))+
    theme_few()
ggsave("Figures/Trend_Bar_Overall.png")
    