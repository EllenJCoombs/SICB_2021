

library(Rvcg)
library(rgl)
library(Morpho)
library(rgl)
library(geomorph)
library(paleomorph)

#Reading in landmarks only - these are .pts files
###=========== LOADING DATA SET 1: WHOLE LANDMARKED SKULL 

setwd("X:xxxxxxx/xxxxxx") 

#Mesh + spheres 
ply=ply2mesh(file="Delphinus delphis AMNH 75332 AB.ply")
shade3d(ply, col=bone1)
spheres3d(ptsarray[,,2],radius = 4,col=datcol2) 

#shows the colour spheres 
text3d(dataAB[,,2],text=1:dim(dataAB)[[1]]) #add text numbers 

#lollipop plot 
#Add label = TRUE if you want the numbers 
plotRefToTarget(ptsarray[,,1],MirroredAC[,,1],method="vector")


#=========== Combine datasets (AB and AC) to look at morphospace 


all_array <- abind(ptsarray, MirroredAC, along = 3)

Y.gpa3=gpagen(all_array) #Remove non-shape aspects 
data3=Y.gpa3$coords #Subset out the coords 
size=Y.gpa3$Csize

#plot morphospace
PCA=plotTangentSpace(all_array, axis1=1, axis2=2, label=dimnames(data)[[3]])



#========================================#
#      1. READ IN THE MANUAL LMS         #
#========================================#


###=========== LOADING DATA SET 1: WHOLE LANDMAKRED SKULL 
#Read in landmarks manually placed on the whole skull 

ntaxa<-199 ## number of specimens (extant only) - NB can also put this in the code below (x,y,z) 
#data set .pts from Checkpoint

ptslist<-dir(pattern='.pts',recursive=T)
ptsarray<-array(dim=c(123,3,32)) #dim=c(number of landmarks and semilandmarks, number of dimensions, number of specimens)
for(i in 1:length(ptslist))
{
  ptsarray[,,i]<-as.matrix(read.table(file=ptslist[i],skip=2,header=F,sep="",row.names=1))
}

#Need the .plys for this 
#[3] stays the same 
dimnames(ptsarray)[3]<-list(
  substr(dir("./ply",pattern=".ply"),1,(nchar(dir("./ply",pattern=".ply"))-4)))###donne nom de scan a ptsarray et -4 pour retirer derniere lettre de nom
arraylm<-ptsarray


##### MISSING LANDMARKS #########
#do this first and then rearrange landmarks 
#Landmarks need to be 'reshuffled' because we added 4 extra LMs on the nasal to be reflect that one midline landmark would 
#not work in the asymmetrical odontocetes 

arraylm[which(arraylm==9999)] <- NA
arraylm <- estimate.missing(arraylm,method="TPS")


##### MOVING NEW LANDMARKS INTO THE CORRECT POSITION ##############

#there are 123 landmarks with the last 4 added last (for asymmetry) 
#these last 4 landmarks had to be slotted in to the correct positions 
#120 ---> 67 
#121 ---> 70 
#122 ---> 85 
#123 ---> 85 

#Slot in the landmarks (see above)
#You can use abind to bind datasets eg. the odonts and mysts 
arranged=abind::abind(arraylm[c(1:66),,], arraylm[120,,],arraylm[c(67,68),,],
                      arraylm[121,,], arraylm[c(69:82),,], arraylm[c(122,123),,],
                      arraylm[c(83:119),,],
                      along= 1)

#show a print out with the new landmark numbers to check correct positioning 
#the last number [,,x] is the species number 
#check 
text3d(arranged[,,2], text=1:dim(arranged)[1])

#careful not to have missing data 
text3d(arranged[,,2], text=1:dim(arranged)[1])

#This would be to change the names according to the species (as above with .ply)
#dimnames(arranged)[3]=species

#############################
#                           #
#  PROCRUSTES THE DATA      #
#                           #
#############################

arranged=gpagen(arranged) #Remove non-shape aspects 
arranged_data=arranged$coords #Subset out the coords 
size=arranged$Csize #centroid size
#PlotTangentSpace if you want to see a quick morphospace of the skulls 

PCA <- plotTangentSpace(arranged$coords, legend=TRUE) 

#arranged_data is the Procrusted coords dataset of manually placed landmarks


############################################################
#                                                          #
#     2. Load second dataset - half LM skull to mirror     # 
#                                                          #                            
############################################################

setwd("X:xxxxxxx/xxxxxx") 
#NB the plys have been transformed to ASCII (from binary) so that landmarks can be visualised on the mesh 
#See code 'Binary_ASCII_ply.R" if needed for this step

ntaxa<-199 ## number of specimens (extant only) - NB can also put this in the code below (x,y,z) 
#data set .pts from Checkpoint

ptslist<-dir(pattern='.pts',recursive=T)
ptsarrayAC<-array(dim=c(66,3,34)) #dim=c(number of landmarks and semilandmarks, number of dimensions, number of specimens)
for(i in 1:length(ptslist))
{
  ptsarrayAC[,,i]<-as.matrix(read.table(file=ptslist[i],skip=2,header=F,sep="",row.names=1))
}

#Set to the whole project directory for this - the code wants to look for 'ply' folder 
dimnames(ptsarrayAC)[3]<-list(
  substr(dir("./ply",pattern=".ply"),1,(nchar(dir("./ply",pattern=".ply"))-4)))###donne nom de scan a ptsarray et -4 pour retirer derniere lettre de nom
arraylmAC<-ptsarrayAC
arraylmAC


##### CHANGING MISSING LANDMARKS BEFORE MIRRORING #########
#Otherwise you get weird numbers that aren't 9999 mirroring
#estimate.missing from Geomorph is used for estimating missing landmarks 

arraylmAC[which(arraylmAC==9999)] <- NA
arraylmAC <- estimate.missing(arraylmAC,method="TPS")


#MIRROR THESE LANDMARKS over the central line plane of the skull 

########## SYMETRISATION TO IMPROVE THE SHAPE ANALYSES #########################
#Ellen's code 

midline<-as.integer(c(38,40,48,49,51,54,55,56,61)) # LM that are on the midline + parasphenoid curve points + NO patch point
#got length(midline)= 9 points on the midline

left.lm <- c(1:37,39,41:47,50,52,53,57:60,62:66)
#left.lm <- c(2,3,5:18,21:37,39,41:47,50,52,53,57:60,62:66)
#exclude midline points. Last number = last number of newpts 

lengmatrice=dim(arraylmAC)[1]*2-length(midline)#-length(nasalfrontal) #should be the length with the both sides, 1 is the column and 2 
#just means that we are duplicating the data to be on both sides of the skull 

Matrice=array(NA,dim = c(lengmatrice,3,24)) #3 is the dimensions (x, y, z), 2 is specimen number 
Matrice[1:dim(arraylmAC)[1],,]=arraylmAC

#left.lm <- c(1:37,39,41:47,50,52,53,57:60,62:66)
#left.lm <- c(2,3,5:18,21:37,39,41:47,50,52,53,57:60,62:66)
#exclude midline points. Last number = last number of newpts 

#Check left.lm and midline [left.lm,,x] = species number
spheres3d(arraylmAC[left.lm,,10],radius=4)
spheres3d(arraylmAC[midline,,10],radius=5,col='red')

right.lm <- c(67:123) #left.lm +1:lenmatrice

bilat.landmarks <- cbind(left.lm, right.lm) #one column is the rows of the right side LM and the other column the rows of the left side

MirroredAC=mirrorfill(A=Matrice,  l1=midline, l2=bilat.landmarks) # the NA rows are now filled so that the numbers are the same on both
#sides of the skull 
MirroredAC
#deformGrid3d(MirroredAC[67:123,,2], Matrice[,,2], ngrid=0) #This shows you the new mirroed landmarks 

#These visualisations are done before Procrusted data 
#This shows the original landmarks
spheres3d(MirroredAC[c(1:66),,1],col=2,radius=4)
#check dimensions

#############################
#                           #
#  PROCRUSTES THE DATA      #
#                           #
#############################

MirroredAC=gpagen(MirroredAC) #Remove non-shape aspects 
Mirrored_data=Mirrored_data$coords #Subset out the coords 

#Checking if landmark ordering is correct 
#Comparing landmarks on both sides oft he skull 

col=rainbow(length(1:dim(Mirrored_data)[1]))
shapes3d(Y.gpa$coords[,,1], color=col)

#==========
