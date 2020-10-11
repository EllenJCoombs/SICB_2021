

#This makes nice 3D outputs of the files for publications as well (such as the M.Mirus)
#script for converting binary to ASCII .ply files
#rm(list=ls())

install.packages("Rvcg")
install.packages("rgl")
library(Rvcg)
library(rgl)

#set working directory with ascii or binary ply files
#put the .ply files you want to change (for reading into R) in the .ply binary folder (not the .ply folder) 
setwd("D:/Ply ASCII/ply binary/ binary for individual problem skulls")

#set output folder
#The extra forward slash at the end tells R that that is the output folder
outputfolder<-("D:/Ply ASCII/ply binary/")

meshlist<-dir(pattern='.ply',recursive=F)

for (i in 1:length(meshlist)){
  x<-vcgImport(file=meshlist[i]) #import meshes
  
  vcgPlyWrite(x,filename=paste(outputfolder,meshlist[i],sep=""),binary=FALSE)#export
}


#Remove ply from the filename 
#filelist <- dir(pattern='.ply', recursive=F)
#gsub("ply", "", filelist)

#outputfolder<-("R:/Ellen/WHOLE skull analyses/ply/")



#To visualise using newpts from 2. Ryan's code 
#Set the directory to the one with the .ply in it 
checkLM(newpts,path="./ply/",suffix=".ply",pt.size=1,render="s",alpha=1)


#========================================================================================

#For taking a snapshot for publications 
#nstall.packages('Morpho')
library(Morpho)
library(rgl)


#don't forget .ply ata the end of the file path 

Lissodelphis=ply2mesh(file="D:/Checkpoint - ICVM/Asymmetry project/AB/ply/Lissodelphis borealis USNM 550188.ply")

shade3d(Kentriodon, col="white") #"white" or bone1 (don't use the "" for the latter)
rgl.snapshot(filename = "D:/Checkpoint - ICVM/Asymmetry project/AB/ply/Lissodelphis borealis USNM 550188.png")#the object name and then underscore followed by the new file name 




