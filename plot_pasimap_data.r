##
# this script plots the PaSiMap coordinates 
# exported from Jalview in different scatterplots
# it will output the plots into your project folder. 
#
# go through the script and change annotated lines to your needs
# afterwards, run the whole script 
# by pressing <Alt + Ctrl + r> or <Option + Command + r>
#
# you can run a single line of code 
# by pressing <Ctrl + Enter> or <Command + Enter>
#
# if you want to look at a specific figure inside of
# this editor and not export the graph,
# comment (place a '#' at the beginning) the lines with
# 'svg(...)' and 'dev.off()' of that figure
##

packages <- c("REdaS", "plotly", "ggplot2", "Spectrum")
installed <- packages %in% rownames(installed.packages())
if (any(installed == FALSE)) {
  install.packages(packages[!installed])
}

lapply(packages, library, character.only=TRUE)

# enter the complete path to the main directory
# (has to end with a '\' on windows or a '/' on Mac and GNU/Linux)
setwd("your-path-to/plot_pasimap_data-master/example_data/")

# change "example_data.csv" to the name of your data file
data <- read.csv("example_data.csv")

y <- data$X1
z <- data$X2
x <- data$X3

data$angle <- rad2deg(atan2(y,z))

### colors the datapoints by angle
new <- colorRampPalette(c("red","purple","blue","green","yellow" ,"orange"))
data$Col <- new(45)[as.numeric(cut(data$angle,breaks = 
45))]


### scatterplot of the pasimap datapoints colored by angle
# set the name of the output file (keep the svg extension)
svg("PaSiMap.svg")

plot (y, z, bg= data$Col, pch = 21, cex = 1.3,
xlab="coordinate 2", ylab="coordinate 3", xlim=c(min(y)-0.05,max(y)+0.05), 
ylim=rev(c(min(z)-0.05,max(z)+0.05)))

# comment the line below (by adding a '#' in front of it) to disable labels in the plot
#text(y, z, labels=data$Sequence, cex = 0.8, adj = c(1,1.7), offset = 100)

dev.off()

### calculate the angle distribution grouping (do not change anything here)
angle_bins <- seq(from = -175, to = 175, by = 10) # each bin holds angels -5 +4 (e.g -179 to -170)
angle_counts <- rep(0,36)
angle_distribution <- data.frame(angle_bins, angle_counts)
data[, "binned_angle"] <- 0
for (i in 1:nrow(data))
{
  angle <- data[i,]["angle"]
  angle <- as.integer(angle)
  rounded <- round(angle, digits = -1)
  binned_angle <- if ((angle - rounded) < 0) rounded - 5 else rounded + 5
  row_index = which(angle_distribution$angle_bins == binned_angle)
  data[i,]["binned_angle"] <- binned_angle
  angle_distribution$angle_counts[row_index] <- angle_distribution$angle_counts[row_index] + 1
}
cols_like_old <- new(45)[as.numeric(cut(angle_distribution$angle_bins,breaks = 45))]

### bar plot of the angle distribution
# set the name of the output file (keep the svg extension)
svg("angle-distribution.svg")
barplot (angle_distribution$angle_counts, 0.83, col= cols_like_old, pch = 21, xaxt="n", xlab = "angle", ylab = "count", main="angle distribution per 10° angle intervals")
axis(1, at = seq(from = 0.6, to = 35.6, by = 1), labels = angle_bins)
dev.off()

### group sequences by angle cluster (do not change anything here)
group <- 1
data[, "group"] <- 0
old_n <- 0
for (i in 1:nrow(angle_distribution))
{
  n <- angle_distribution$angle_counts[i]
  anglebin <- angle_distribution$angle_bins[i]
  if (n > 0)
  {
    dataindexes = which(data$binned_angle == anglebin)
    for (k in dataindexes)
    {
      data$group[k] <- group
    }
  } 
  else if (n == 0 && old_n != 0)
  {
    group <- group + 1
  }
  old_n <- n
} 


### (advanced) change the group of a datapoint manually
##
# syntax for this is the following:
# data$group[datapoint number] <- new group
# uncomment (remove the '#' in the beginning) the line below the '##' and enter your values to make it active
# re-run the code until the next 'dev.off' to update your graph
##
# data$group[1] <- 5

### 
# set the name of the output file (keep the svg extension)
svg("PasiMap-by-angle-group.svg")

cols_new <- new(45)[as.numeric(cut(data$group,breaks = 45))]
plot (y, z, bg= cols_new, pch = 21, cex = 1.3,
xlab="coordinate 2", ylab="coordinate 3", xlim=c(min(y)-0.05,max(y)+0.05), 
ylim=rev(c(min(z)-0.05,max(z)+0.05)))

# comment the line below (by adding a '#' in front of it) to disable labels in the plot
text(y, z, labels=data$Sequence, cex = 0.8, adj = c(1,1.7), offset = 100)

dev.off()


### group the data by Spectral Clustering
# do not change anything here
dataForSpectral <- data[,c(2:4)]
dataForSpectral <- t(dataForSpectral)
colnames(dataForSpectral) <- seq(1:ncol(dataForSpectral))
spectral <- Spectrum(dataForSpectral,method=2,showres=FALSE,fontsize=8,dotsize=2)
groups <- spectral[["assignments"]]

for (i in 1:length(groups))
{
  group <- groups[i]
  data$group[i] = group
}


### plot the Spectral Clustering groups
# set the name of the output file (keep the svg extension)
svg("PasiMap-Spectral-Clustering.svg")

cols_new <- new(45)[as.numeric(cut(data$group,breaks = 45))]
plot (y, z, bg= cols_new, pch = 21, cex = 1.3,
xlab="coordinate 2", ylab="coordinate 3", xlim=c(min(y)-0.05,max(y)+0.05), 
ylim=rev(c(min(z)-0.05,max(z)+0.05)))

# comment the line below (by adding a '#' in front of it) to disable labels in the plot
text(y, z, labels=data$Sequence, cex = 0.8, adj = c(1,1.7), offset = 100)

dev.off()
