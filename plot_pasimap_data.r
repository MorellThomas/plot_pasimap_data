##
# this script plots the PaSiMap coordinates exported from Jalview in a scatterplot
# datapoints will be colored by angle
#
#  go through the script and change annotated lines to your needs
#  afterwards, run the whole script by pressing <Alt + Ctrl + r>
##

require(REdaS) || install.packages("REdaS")
require(plotly) || install.packages("plotly")

# enter the complete path to the main directory
setwd("/home/thomas/Studium/Master_2/java-hiwi/plot/onGithub/")

# sub-directory containing the data
data_dir <- "example_data/"

# change "example_data.csv" to the name of your data file
data <- read.csv(paste(data_dir, "example_data.csv", sep=""))

y <- data$coordinate_1
z <- data$coordinate_2
x <- data$coordinate_3

data$angle <- rad2deg(atan2(y,z))
new <- colorRampPalette(c("red","purple","blue","green","yellow" ,"orange"))
data$Col <- new(45)[as.numeric(cut(data$angle,breaks = 
45))]


# change the name of the output file
svg("PaSiMap.svg")

plot (y, z, bg= data$Col, pch = 21, cex = 1.3,
xlab="coordinate 2", ylab="coordinate 3", xlim=c(min(y)-0.05,max(y)+0.05), 
ylim=rev(c(min(z)-0.05,max(z)+0.05)))

# comment the line below (by adding a '#' in front of it) to disable labels in the plot
text(y, z, labels=data$Sequence, cex = 0.8, adj = c(1,1.7), offset = 100)

dev.off()
