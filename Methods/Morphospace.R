
#To visualise a nice morphospace
#Read in the whole dataset 
#Note - these data have been Procrusted during the previous stage 

library(factoextra)
#for prcomp (PCA)

#Compute PCA 
#Load landmark data 

landmarks <- read.csv('XXX.csv', header = TRUE, fileEncoding="UTF-8-BOM")


# IF ERRORS #
landmarks[,6] <- as.numeric(as.character(landmarks[,6]))
#na.omit(landmarks)
#any(is.na(landmarks)) #Want this to be FALSE 
#landmarks[is.na(landmarks)] <- 0.015850205 #Or whatever the value is 

myPr <- prcomp(landmarks[1:199,6:62], scale = TRUE) #pull out the columns of data which are nummerical only - i.e. the radii 

#If the above isn't working, it's likely there is a numeric/factor problem
landmarks <- as.numeric(landmarks$X67)

plot(myPr, type = 'b') #boxplot to look at variation 
biplot(myPr, scale = 0) #explore the data 

#extract PC scores 
str(myPr)
myPr$x

summary(myPr) #look at PC variations 
radii <- cbind(landmarks, myPr$x[,1:2]) #pull out PC1 and PC2


library(ggplot2) #plot PCA
library(viridis)
library(ggfortify)
library(RColorBrewer)
library(ggrepel)


a <- ggplot(test2, aes(PC1, PC2, col = suborder, fill = suborder, labels = TRUE)) + 
geom_point(shape = 21) + 
xlab('PC 1 (19.5%)') + 
ylab('PC2 (11.9%)') + 
geom_point(aes(size = sum.radii)) +
theme_classic() +
#geom_text(aes(label= ï..), hjust=1, vjust=2) #with numbers of specimens 
#geom_text_repel(aes(label = test2$id)) #with numbers of specimens repelled 

a 
#a + scale_x_reverse() #If you want to reverse axis
#a + scale_y_reverse() #If you want to reverse axis

a + scale_colour_viridis_d() + 
scale_x_reverse()#viridis
