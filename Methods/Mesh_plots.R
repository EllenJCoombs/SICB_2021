

#Code for creating plots of landmarks and curves for the different PC scores

library(Morpho)

#-- Plot with 3D data (example with PC1max)
plotRefToTarget( PCA.ALL$shapes$shapes.comp2$max, PCA.ALL$shapes$shapes.comp2$max,
                 method = "points",axes = F) 

spheres3d(PCA.ALL$shapes$shapes.comp3$min, radius= .0008,color = "grey")
spheres3d(PCA.ALL$shapes$shapes.comp3$max, radius=.0008,color = "grey")
spheres3d(PCA.ALL$shapes$shapes.comp4$min, radius=.0008,color = "grey")
spheres3d(PCA.ALL$shapes$shapes.comp4$max, radius=.0008,color = "grey")


spheres3d(PCA.ALL$shapes$shapes.comp2$min[-nas,], radius=.0008,color = "grey")
spheres3d(PCA.ALL$shapes$shapes.comp2$min[nas,], radius=.0008,color = "firebrick")

#Load ply file
atarfa=ply2mesh(file="D:/Ply ASCII/ply ASCII/Mysts/ply/Diorocetus hiatus USNM 16783.ply")
shade3d(atarfa,col='white')
spheres3d(final_mirrored_mysts[c(1:66),,21], radius = 7, color = 'red')
spheres3d(final_mirrored_mysts[124:1113,,21], radius =4, color = 'blue')
spheres3d(final_mirrored_mysts[459:1115,,21], radius =4, color = 'blue')
spheres3d(final_mirrored_mysts[1114:1448,,21], radius = 2.5, color = 'blue')
spheres3d(final_mirrored_mysts[1449:2028,,21], radius = 2.5, color = 'blue')
