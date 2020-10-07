

#Plotting the phylogeny with Epoch/timescale data
#Timescales are from Lloyd GT and Slater GJ (in prep) 
#A Total-Group Phylogenetic Metatree of Cetacea and the Importance of Fossil Data in Diversification Analyses.
#Please site the above if using their phylogeny - we trimmed Lloyd and Slater's phylogeny to our specimens (199)

library(plotrix)
library(phytools)


plotTree(tree,ftype="off",ylim=c(-0.2*Ntip(tree),Ntip(tree)),lwd=1,
         xlim=c(max(nodeHeights(tree)),0),direction="leftwards")

#Leg, epoch, and colours are the most important for your own function

#Setting colours 
colors <- setNames(c(rgb(255, 242, 127, 255, maxColorValue = 255), 
                     rgb(255, 230, 25, 255, maxColorValue = 255), 
                     rgb(253, 154, 82, 255, maxColorValue = 255), 
                     rgb(127, 198,78, 255, maxColorValue = 255),
                     rgb(52, 178, 201, 255, maxColorValue = 255),
                     rgb(129, 43, 146, 255, maxColorValue = 255)),
                   c("Holocene", "Pleistocene", "Pliocene", "Miocene", "Oligocene", "Eocene"))


leg <- rbind(c(0.011650, 0), c(2.58, 0.11650), c(5.33, 2.58), c(23.03, 5.33), c(33.9, 23.03), c(49.006, 33.9)) #epoch ages 

rownames(leg) <- c("Holocene", "Pleistocene", "Pliocene", 
                   "Miocene", "Oligocene", "Eocene")

colnames(leg) <- c("start", "end")

object <- list(period = leg, cols = colors)

epochs <- geo.legend(leg=leg, colors=colors, alpha=0.2)                       

obj<-epochs 

bands<-max(epochs$leg[,1])-epochs$leg[,2] #calculates the distance between the two epochs 

plotTree(tree,type="fan",fsize=0.7,lwd=1,ftype="i")
for(i in 1:nrow(epochs$leg)){
  color<-paste(strsplit(epochs$colors[i],"")[[1]][1:7],collapse="")
  draw.circle(0,0,radius=bands[i],col=color,border="transparent") #makes the circles transparent 
}
#par(fg="transparent")
#plotTree(tree,type="fan",fsize=0.7,lwd=1,ftype="i")
par(fg="black")#makes the lettering black over the top
plotTree(tree,type="fan",add=TRUE,fsize=0.7,lwd=1,ftype="i") 



