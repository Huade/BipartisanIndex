# Bipartisan score data visualization
# 03-Gridmap.R
# Author: Huade Huo <hh561@georgetown.edu>
# License: MIT

# Read griddata
griddata <- read.csv("Data/gridmap.csv")
HvSv <- read.csv("Data/HvSv.csv")

# Gradient of n colors
# startColor = "#CEC7FF", endColor = "#290042" -purple

colorPalettefun <- function(startColor = "#CEC7FF", endColor = "#290042", numColor = 51){
  colfunc <- colorRampPalette(c(startColor, endColor))
  colfunc(numColor)
}

# Merge grid data and users' data
SelectSort <- function(df, state_abb, value){
  data <- df[c(state_abb, value)]
  names(data) <- c("abb", "datavalue")
  data <- merge(griddata, data, by = "abb")
  data <- data[order(data$datavalue),]
  return(data)
}

# Main function
gridMap <- function(title, Subtitle){
  # Assign colors
  data_sc$gridColor <- colorPalettefun(numColor = 51)
  
  # Init plot
  plot(c(0, 12), c(0,8), type = "n", bty="n", 
       xaxt='n', yaxt='n', ann=FALSE,asp=1)
  title(main = title, sub = Subtitle)
  
  for (i in 1:nrow(data_sc)){
    if (!is.na(data_sc$datavalue[i])){
      rect(data_sc$xl[i],data_sc$yb[i],data_sc$xr[i],data_sc$yt[i],col=data_sc$gridColor[i], border="white")
      text(0.5*(data_sc$xl[i]+data_sc$xr[i]),
           0.5*(data_sc$yb[i]+data_sc$yt[i]), 
           labels = data_sc$abb[i],
           col = "white")
    }
    else {
      rect(data_sc$xl[i],data_sc$yb[i],data_sc$xr[i],data_sc$yt[i],  border="white")
      text(0.5*(data_sc$xl[i]+data_sc$xr[i]),
           0.5*(data_sc$yb[i]+data_sc$yt[i]), 
           labels = data_sc$abb[i],
           col = "grey")
    }

  }
}

# Overall
data_sc <- SelectSort(HvSv, state_abb = "abb", value = "AvgScore")
gridMap(title = "Bipartisan Index, 113th Congress \n(Overall)", 
        Subtitle = "Darker color indicates higher bipartisan score")
dev.copy(png,'Figures/Grid_map_overall.png')
dev.off()

# House
data_sc <- SelectSort(HvSv, state_abb = "abb", value = "Hv")
gridMap(title = "Bipartisan Index, 113th Congress \n(House)", 
        Subtitle = "Darker color indicates higher bipartisan score")
dev.copy(png,'Figures/Grid_map_House.png')
dev.off()

# Senate
data_sc <- SelectSort(HvSv, state_abb = "abb", value = "Sv")
gridMap(title = "Bipartisan Index, 113th Congress \n(Senate)", 
        Subtitle = "Darker color indicates higher bipartisan score")
dev.copy(png,'Figures/Grid_map_Senate.png')
dev.off()