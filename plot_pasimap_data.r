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
# 'make_figure(...)' and 'dev.off()' of that figure
##

packages <- c("REdaS", "plotly", "ggplot2", "Spectrum")
installed <- packages %in% rownames(installed.packages())
if (any(installed == FALSE)) {
  install.packages(packages[!installed])
}

lapply(packages, library, character.only=TRUE)

# enter the complete path to the main directory
# (has to end with a '\' on windows or a '/' on Mac and GNU/Linux)
#setwd("your-path-to/plot_pasimap_data-master/example_data/")
setwd("/Users/thomasm/Desktop/master_main/R/plot_pasimap_data/git/example_data/")

# change "example_data.csv" to the name of your data file
if (!exists("coordinates"))
{
  coordinates <- read.csv("example_data.csv")
}

# set the output format of your images
# chose either svg, pdf or none
output <- "none"


y <- coordinates$X1
z <- coordinates$X2
x <- coordinates$X3

coordinates$angle <- rad2deg(atan2(y,z))

### colors the datapoints by angle
new <- colorRampPalette(c("red","purple","blue","green","yellow" ,"orange"))
coordinates$Col <- new(45)[as.numeric(cut(coordinates$angle,breaks = 
45))]

######## 
# This code is needed by the script
# to enscure its functionality.
# DO NOT change this
make_figure<-function(f)
{
  if (output == "svg")
    svg(paste(f, ".svg", sep=""))
  else if (output == "pdf")
    pdf(paste(f, ".pdf", sep=""))
}
close<-function()
{
  if (output != "none")
    dev.off()
}
########

### scatterplot of the pasimap datapoints colored by angle
# set the name of the output file (keep the svg extension)
make_figure("PaSiMap")

plot (y, z, bg= coordinates$Col, pch = 21, cex = 1.3,
xlab="coordinate 2", ylab="coordinate 3", xlim=c(min(y)-0.05,max(y)+0.05), 
ylim=rev(c(min(z)-0.05,max(z)+0.05)))

# comment the line below (by adding a '#' in front of it) to disable labels in the plot
#text(y, z, labels=coordinates$Sequence, cex = 0.8, adj = c(1,1.7), offset = 100)

close()

# interactive visualisation of the data
pasimap <- plot_ly(coordinates, x = ~X1, y = ~X2, type = 'scatter', mode = 'markers',
               marker = list(
                 size = 10, color = ~Col, line = list(color = 'rgba(0,0,0,1)', width=1)
               ), hoverinfo = 'text', text = ~Sequence)
pasimap <- pasimap %>%
  layout(yaxis = list(title = "coordinate 3", zeroline = FALSE, range=rev(list(min(z)-0.05, max(z)+0.05)),
             showgrid = FALSE, showline=TRUE, mirror=TRUE, ticks="outside"),
         xaxis = list(title = "coordinate 2", zeroline = FALSE, range=list(min(y)-0.05, max(y)+0.05),
             showgrid = FALSE, showline=TRUE, mirror=TRUE, ticks="outside"))
# to show the plot
pasimap
##

### calculate the angle distribution grouping (do not change anything here)
angle_bins <- seq(from = -175, to = 175, by = 10) # each bin holds angels -5 +4 (e.g -179 to -170)
angle_counts <- rep(0,36)
angle_distribution <- data.frame(angle_bins, angle_counts)
coordinates[, "binned_angle"] <- 0
for (i in 1:nrow(coordinates))
{
  angle <- coordinates[i,]["angle"]
  angle <- as.integer(angle)
  rounded <- round(angle, digits = -1)
  binned_angle <- if ((angle - rounded) < 0) rounded - 5 else rounded + 5
  row_index = which(angle_distribution$angle_bins == binned_angle)
  coordinates[i,]["binned_angle"] <- binned_angle
  angle_distribution$angle_counts[row_index] <- angle_distribution$angle_counts[row_index] + 1
}
cols_like_old <- new(45)[as.numeric(cut(angle_distribution$angle_bins,breaks = 45))]

### bar plot of the angle distribution
# set the name of the output file (keep the svg extension)
make_figure("angle-distribution")
barplot (angle_distribution$angle_counts, 0.83, col= cols_like_old, pch = 21, xaxt="n", xlab = "angle", ylab = "count", main="angle distribution per 10° angle intervals")
axis(1, at = seq(from = 0.6, to = 35.6, by = 1), labels = angle_bins)
close()

### group sequences by angle cluster (do not change anything here)
group <- 1
coordinates[, "group"] <- 0
old_n <- 0
for (i in 1:nrow(angle_distribution))
{
  n <- angle_distribution$angle_counts[i]
  anglebin <- angle_distribution$angle_bins[i]
  if (n > 0)
  {
    dataindexes = which(coordinates$binned_angle == anglebin)
    for (k in dataindexes)
    {
      coordinates$group[k] <- group
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
# coordinates$group[datapoint number] <- new group
# uncomment (remove the '#' in the beginning) the line below the '##' and enter your values to make it active
# re-run the code until the next 'close()' to update your graph
##
# coordinates$group[1] <- 5

### 
# set the name of the output file (keep the svg extension)
make_figure("PasiMap-by-angle-group")

coordinates$ColAngle <- new(45)[as.numeric(cut(coordinates$group,breaks = 45))]
plot (y, z, bg= coordinates$ColAngle, pch = 21, cex = 1.3,
xlab="coordinate 2", ylab="coordinate 3", xlim=c(min(y)-0.05,max(y)+0.05), 
ylim=rev(c(min(z)-0.05,max(z)+0.05)))

# comment the line below (by adding a '#' in front of it) to disable labels in the plot
#text(y, z, labels=coordinates$Sequence, cex = 0.8, adj = c(1,1.7), offset = 100)

close()

# interactive visualisation of the data
pasimap_by_angle <- plot_ly(coordinates, x = ~X1, y = ~X2, type = 'scatter', mode = 'markers',
               marker = list(
                 size = 10, color = ~ColAngle, line = list(color = 'rgba(0,0,0,1)', width=1)
               ), hoverinfo = 'text', text = ~Sequence)
pasimap_by_angle <- pasimap_by_angle %>%
  layout(yaxis = list(title = "coordinate 3", zeroline = FALSE, range=rev(list(min(z)-0.05, max(z)+0.05)),
                   showgrid = FALSE, showline=TRUE, mirror=TRUE, ticks="outside"),
         xaxis = list(title = "coordinate 2", zeroline = FALSE, range=list(min(y)-0.05, max(y)+0.05),
                   showgrid = FALSE, showline=TRUE, mirror=TRUE, ticks="outside"))
# to show the plot
pasimap_by_angle
##

### group the data by Spectral Clustering
# do not change anything here
dataForSpectral <- coordinates[,c(2:4)]
dataForSpectral <- t(dataForSpectral)
colnames(dataForSpectral) <- seq(1:ncol(dataForSpectral))
spectral <- Spectrum(dataForSpectral,method=2,showres=FALSE,fontsize=8,dotsize=2)
groups <- spectral[["assignments"]]

for (i in 1:length(groups))
{
  group <- groups[i]
  coordinates$group[i] = group
}


### plot the Spectral Clustering groups
# set the name of the output file (keep the svg extension)
make_figure("PasiMap-Spectral-Clustering")

coordinates$ColSpectral <- new(45)[as.numeric(cut(coordinates$group,breaks = 45))]
plot (y, z, bg= coordinates$ColSpectral, pch = 21, cex = 1.3,
xlab="coordinate 2", ylab="coordinate 3", xlim=c(min(y)-0.05,max(y)+0.05), 
ylim=rev(c(min(z)-0.05,max(z)+0.05)))

# comment the line below (by adding a '#' in front of it) to disable labels in the plot
#text(y, z, labels=coordinates$Sequence, cex = 0.8, adj = c(1,1.7), offset = 100)

close()

# interactive visualisation of the data
pasimap_spectral <- plot_ly(coordinates, x = ~X1, y = ~X2, type = 'scatter', mode = 'markers',
               marker = list(
                 size = 10, color = ~ColSpectral, line = list(color = 'rgba(0,0,0,1)', width=1)
               ), hoverinfo = 'text', text = ~Sequence)
pasimap_spectral <- pasimap_spectral %>%
  layout(yaxis = list(title = "coordinate 3", zeroline = FALSE, range=rev(list(min(z)-0.05, max(z)+0.05)),
            showgrid = FALSE, showline=TRUE, mirror=TRUE, ticks="outside"),
         xaxis = list(title = "coordinate 2", zeroline = FALSE, range=list(min(y)-0.05, max(y)+0.05),
            showgrid = FALSE, showline=TRUE, mirror=TRUE, ticks="outside"))
# to show the plot
pasimap_spectral
##


