

######################################
#                                    #
#  Code adapted from Julien Clavel   #
#                                    #
######################################

#The trees used in this study are from: 
#Lloyd GT and Slater GJ (in prep) 
#A Total-Group Phylogenetic Metatree of Cetacea and the Importance of Fossil Data in Diversification Analyses.


# ------ function to bind an extant species to a tree (possibly non ultrametric) - J. Clavel
tip_bind <- function(tree, sp_names=NULL, interactive=TRUE, position = 0, where = "root"){
  require(ape)
  require(phytools)
  # build a fake tree with one tip
  tip <- list(edge = matrix(c(2, 1), 1, 2), tip.label = sp_names, 
              edge.length = 0., Nnode = 1)
  class(tip) <- "phylo"
  
  #bind the fake tip on the tree
  plot(tree, cex = 0.5)
  tree_temp = bind.tree(tree, tip, interactive = interactive, position = position, where= where)
  
  #Now that we have grafted our fake tree, we want to scale it's branch length so that the tip becomes an extant.
  node <- nodeHeights(tree_temp) # heights of the nodes
  index = which(tree_temp$edge[,2]==which(tree_temp$tip.label==sp_names))
  diff_length = max(node) - node[index,2]
  
  #Now we add this difference (between the tip and present time)
  tree_temp$edge.length[index] = tree_temp$edge.length[index] + diff_length
  
  # we return the transformed tree
  return(tree_temp)
}

#Adding extant species to tree 
library(phytools)
#set.seed(1)
#tree=pbtree(n=20, d = 0.2)
plot(subtree, cex = 0.4) #check
tree2 = tip_bind(subtree, 'Neophocaena_asiaeorientalis', interactive=TRUE)
tree3 = tip_bind(tree2, 'Orcaella_heinsohni', interactive=TRUE)
#tree4 = tip_bind(tree3, 'Tursiops_truncatus_gilli', interactive=TRUE)
tree5 = tip_bind(tree4, 'Berardius_minimus', interactive=TRUE)
tree6 = tip_bind(tree5, 'Cephalorhynchus_hectori_maui', interactive=TRUE)
tree7 = tip_bind(tree6, 'Mesoplodon_hotaula', interactive=TRUE)
tree8 =  tip_bind(tree7, 'Sousa_plumbea', interactive=TRUE)
tree9 = tip_bind(tree8, 'Sousa_teuszii', interactive=TRUE)
tree10 = tip_bind(tree9, 'Sousa_sahulensis', interactive=TRUE)
# plot the new tree
plot(tree10, cex = 0.5)
axisPhylo()

write.tree(tree10, file = 'tree10.tre')
write.nexus(tree10, file = 'tree10.nexus')
#Changing names in the phylo 
#search for the tips you want to change
tree10$tip.label[which(tree10$tip.label=="Berardius_minimus")] = "Berardius_sp"
#check
plot(tree10, cex = 0.4)
