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
setwd("your-path-to/plot_pasimap_data-master/example_data/")

# change "example_data.csv" to the name of your data file
coordinates <- read.csv("example_data.csv")

# set the output format of your images
# chose either svg, pdf or none
output <- "none"


## dimensions to use
# change X1, X2, ... to the dimension you want to use
dim1 <- "X1"
dim2 <- "X2"
dim3 <- "X3"


#######
x <- coordinates[[dim1]]
y <- coordinates[[dim2]]
z <- coordinates[[dim3]]

coordinates$angle <- rad2deg(atan2(x,y))

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
show_interactive<-function(f)
{
    
  interactive <- plot_ly(data = filter(coordinates, Sequence == "origin"), x = ~get(dim1), y = ~get(dim2), z = ~get(dim3),
          name="origin", mode = 'markers', type="scatter3d",
          marker = list( size = 4, color = "#000000", line = list(
            color='rgba(0,0,0,1)', width=1)
        ), hoverinfo='text', text=~Sequence)
  if (f == "angle")
  {
    allCols <- lapply(coordinates, unique)$ColAngle
    for (i in 1:(length(allCols)-1))
  {
      if (allCols[i] == "#000000")
        next 
      interactive <- interactive %>% 
        add_trace(data = filter(coordinates, ColAngle == allCols[i]), x = ~get(dim1), y = ~get(dim2), z = ~get(dim3),
                  name =paste(c("group", i), collapse=" "), mode = 'markers', type="scatter3d",
                  marker = list( size = 4, color = ~ColAngle, line = list(
                    color='rgba(0,0,0,1)', width=1)
                ), hoverinfo='text', text=~Sequence)
    }
  } else if (f == "spectral") {
    allCols <- lapply(coordinates, unique)$ColSpectral
    for (i in 1:(length(allCols)-1))
  {
      if (allCols[i] == "#000000")
        next 
      interactive <- interactive %>% 
        add_trace(data = filter(coordinates, ColSpectral == allCols[i]), x = ~get(dim1), y = ~get(dim2), z = ~get(dim3),
                  name =paste(c("group", i), collapse=" "), mode = 'markers', type="scatter3d",
                  marker = list( size = 4, color = ~ColSpectral, line = list(
                    color='rgba(0,0,0,1)', width=1)
                ), hoverinfo='text', text=~Sequence)
    }
  } else {
    allCols <- lapply(coordinates, unique)$Col
  }
    
  interactive <- interactive %>% 
    layout(scene= list(yaxis = list(title = "coordinate 2", zeroline = FALSE,
              showgrid = TRUE, showline=TRUE, mirror=TRUE, ticks="outside"),
           xaxis = list(title = "coordinate 1", zeroline = FALSE,
              showgrid = TRUE, showline=TRUE, mirror=TRUE, ticks="outside"),
           zaxis = list(title = "coordinate 3", zeroline = FALSE,
              showgrid = TRUE, showline=TRUE, mirror=TRUE, ticks="outside")), showlegend=TRUE)
  
  if (f == "angle")
  {
    htmlwidgets::saveWidget(as_widget(interactive), "PaSiMap_Angle.html")
  } else if (f == "spectral") {
    htmlwidgets::saveWidget(as_widget(interactive), "PaSiMap_Spectral.html")
  } else {
    htmlwidgets::saveWidget(as_widget(interactive), "PaSiMap.html")
  }

  return(interactive)
}
########

### scatterplot of the pasimap datapoints colored by angle
# set the name of the output file (keep the svg extension)
make_figure("PaSiMap")

plot (x, y, bg= coordinates$Col, pch = 21, cex = 1.3,
xlab="coordinate 1", ylab="coordinate 2", xlim=c(min(x)-0.05,max(x)+0.05), 
ylim=c(min(y)-0.05,max(y)+0.05))

# comment the line below (by adding a '#' in front of it) to disable labels in the plot
#text(y, z, labels=coordinates$Sequence, cex = 0.8, adj = c(1,1.7), offset = 100)

close()

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
# coordinates$group[445] <- 9

### 
# set the name of the output file (keep the svg extension)
make_figure("PasiMap-by-angle-group")

coordinates$ColAngle <- new(45)[as.numeric(cut(coordinates$group,breaks = 45))]
plot (x, y, bg= coordinates$ColAngle, pch = 21, cex = 1.3,
xlab="coordinate 1", ylab="coordinate 2", xlim=c(min(x)-0.05,max(x)+0.05), 
ylim=c(min(y)-0.05,max(y)+0.05))

# comment the line below (by adding a '#' in front of it) to disable labels in the plot
#text(y, z, labels=coordinates$Sequence, cex = 0.8, adj = c(1,1.7), offset = 100)

close()

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
plot (x, y, bg= coordinates$ColSpectral, pch = 21, cex = 1.3,
xlab="coordinate 1", ylab="coordinate 2", xlim=c(min(x)-0.05,max(x)+0.05), 
ylim=c(min(y)-0.05,max(y)+0.05))

# comment the line below (by adding a '#' in front of it) to disable labels in the plot
#text(y, z, labels=coordinates$Sequence, cex = 0.8, adj = c(1,1.7), offset = 100)

close()


######
# interactive 3d plot of the data

#add the origin to the plot
if (any(coordinates=="origin"))
{
  coordinates <- filter(coordinates, Sequence != "origin")
}
origin <- c("origin", 0, 0, 0, 0, 0, 0, 0, 0, 0, '#000000', 0, 0, '#000000', '#000000')
coordinates <- rbind(coordinates, origin)
coordinates[[dim1]] <- as.numeric(coordinates[[dim1]])
coordinates[[dim2]] <- as.numeric(coordinates[[dim2]])
coordinates[[dim3]] <- as.numeric(coordinates[[dim3]])
coordinates$binned_angle <- as.numeric(coordinates$binned_angle)
coordinates$angle <- as.numeric(coordinates$angle)
coordinates$group <- as.numeric(coordinates$group)

## shows the interactive 3D plot. Enter the colouring of your choice between the ""
# you can choose between "spectral", "angle" and "default". If anything else is entered here
# the default coloring is used.
# the plot is saved as a html file.
show_interactive("spectral")
