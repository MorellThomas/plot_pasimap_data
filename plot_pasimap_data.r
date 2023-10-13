require(REdaS) || install.packages("REdaS")
require(plotly) || install.packages("plotly")

# enter the complete path to the directory containing your data to plot
setwd("/home/thomas/Studium/Master_2/java-hiwi/plot/")

input_all <- read.csv("input_all.csv")

A_coordinates <- input_all

x <- A_coordinates$coordinate_1
y <- A_coordinates$coordinate_2
z <- A_coordinates$coordinate_3

A_coordinates$angle <- rad2deg(atan2(y,z))

new <- colorRampPalette(c("red","purple","blue","green","yellow" ,"orange"))

A_coordinates$Col <- new(45)[as.numeric(cut(A_coordinates$angle,breaks = 
45))]

svg("PaSiMap.svg")
plot (y, z, bg= A_coordinates$Col, pch = 21, cex = 1.3,
xlab="coordinate 2", ylab="coordinate 3", xlim=c(min(y)-0.05,max(y)+0.05), 
ylim=c(min(z)-0.05,max(z)+0.05))
#xlab="coordinate 2", ylab="coordinate 3", xlim=c(-0.3,0.5), 
#ylim=c(-0.3,0.45))
text(y, z, labels=A_coordinates$label_alias, cex = 0.8, adj = c(1,1.7), 
offset = 100)
dev.off()


#Plot 2

x <- A_coordinates$coordinate_1
y <- A_coordinates$coordinate_2
z <- A_coordinates$coordinate_3

new <- colorRampPalette(c("orange", "red","purple","blue","green","yellow"))

A_coordinates$Col <- new(45)[as.numeric(cut(A_coordinates$angle,breaks = 
45))]

plot (y, z, bg= A_coordinates$Col, pch = 21, cex = 1.3,
xlab="coordinate 2", ylab="coordinate 3", xlim=c(-0.3,0.5), 
ylim=c(-0.3,0.45))


#Plot 3
x <- A_coordinates$coordinate_1
y <- A_coordinates$coordinate_2
z <- A_coordinates$coordinate_3


plot (y, z, col= "black", pch = 21, cex = 1.3,
xlab="coordinate 2", ylab="coordinate 3", xlim=c(-0.3,0.5), 
ylim=c(-0.3,0.45))


#----
GroupB_lens <- input_all
x <- GroupB_lens$coordinate_1
y <- GroupB_lens$coordinate_2
z <- GroupB_lens$coordinate_3


new <- colorRampPalette(c("red","purple","blue","green","yellow" ,"orange"))

GroupB_lens$Col2 <- new(27)[as.numeric(cut(GroupB_lens$angle,breaks = 27))]


plot (y, z, bg= GroupB_lens$Col2, pch = 21, cex = 1.3,
xlab="coordinate 2", ylab="coordinate 3", xlim=c(-0.40,0.4), 
ylim=c(-0.35,0.32))




#Group B plot 2

x <- GroupB_lens$coordinate_1
y <- GroupB_lens$coordinate_2
z <- GroupB_lens$coordinate_3


new <- colorRampPalette(c("orange", "red","purple","blue","green","yellow"))

GroupB_lens$Col2 <- new(27)[as.numeric(cut(GroupB_lens$angle,breaks = 27))]


plot (y, z, bg= GroupB_lens$Col2, pch = 21, cex = 1.3,
xlab="coordinate 2", ylab="coordinate 3", xlim=c(-0.40,0.4), 
ylim=c(-0.35,0.32))

#Outlines

plot (y, z, col= "black", pch = 21, cex = 1.3,
xlab="coordinate 2", ylab="coordinate 3", xlim=c(-0.40,0.4), 
ylim=c(-0.35,0.32))



#GroupC plot 2

GroupC_len <- input_all

x <- GroupC_len$coordinate_1
y <- GroupC_len$coordinate_2
z <- GroupC_len$coordinate_3


new <- colorRampPalette(c("orange", "red","purple","blue","green","yellow"))

GroupC_len$Col2 <- new(46)[as.numeric(cut(GroupC_len$angle, breaks = 46))]

plot (y, z, bg= GroupC_len$Col2, pch = 21, cex = 1.3,
xlab="coordinate 2", ylab="coordinate 3", xlim=c(-0.22,0.54), 
ylim=c(-0.35,0.4))

plot (y, z, col= "black", pch = 21, cex = 1.3,
xlab="coordinate 2", ylab="coordinate 3", xlim=c(-0.22,0.54), 
ylim=c(-0.35,0.4))


text(y, z, labels=GroupC_len$label_alias, cex = 0.8, adj = c(1,1.7), 
offset = 100)

------

#Igs plot

x <- Igs_group_names$coordinate_1
y <- Igs_group_names$coordinate_2
z <- Igs_group_names$coordinate_3


new <- colorRampPalette(c("orange","purple","blue","green","yellow" 
,"orange"))

Igs_group_names$Col2 <- new(46)[as.numeric(cut(Igs_group_names$angle, 
breaks = 46))]

plot (y, z, bg= Igs_group_names$Col2, pch = 21, cex = 1.3,
xlab="Dimension 2", ylab="Dimension 3", xlim=c(-0.43,0.25), 
ylim=c(-0.4,0.4))

plot (y, z, col= "black", pch = 21, cex = 1.3,
xlab="Dimension 2", ylab="Dimension 3", xlim=c(-0.43,0.25), 
ylim=c(-0.4,0.4))


text(y, z, labels=Igs_group_names$label_alias, cex = 0.8, adj = 
c(1,1.7), offset = 100)



