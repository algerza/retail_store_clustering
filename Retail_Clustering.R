 #################################################################
 #####                                                       #####
 #####         STEP 0: LOAD THE NECESSARY PACKAGES           #####
 #####                                                       #####
 #################################################################

 require(readxl)
 require(purrr)
 require(ggplot2)
 require(cluster)
 require(fpc)
 require(dplyr)
 install.packages("factoextra")
 library(factoextra)
 library(gridExtra)
 install.packages("plotly")
 library(plotly)
 packageVersion('plotly')


 
 #################################################################
 #####                                                       #####
 #####              STEP 1: LOAD FINAL DATASET               #####
 #####                                                       #####
 #################################################################

# Load output data from the cleaning and feature engineering phases
data <- read_excel("C:/Users/youruserandpath/data.xlsx", sheet = "data")


 
 #################################################################
 #####                                                       #####
 #####         STEP 2: SELECT NECESSARY VARIABLES            #####
 #####                                                       #####
 ################################################################# 
 


data_cluster = data.frame(
  dimension1_dataset=data[,1],
  variable_1=data[,14],
  variable_2=data[,16],
  variable_3=data[,22],
  variable_4=log(data[,105]+1),
  variable_5=log(data[,110]+1),
  variable_6=data[,121],
  variable_7=data[,123]
  ) 


#------------------------------------------------------------------------#
#                   Exclude already closed stores                        #
#------------------------------------------------------------------------#

closed_stores <- c('23435', '23431', '33432', '43434', '21436', '764350')
data_cluster <- data_cluster %>% filter(!dimension1_dataset %in% closed_stores)


#------------------------------------------------------------------------#
#                       Exclude flagship stores                          #
#------------------------------------------------------------------------#

flagship_stores <- c('13435', '13431', '13432', '13434', '13436', '134350')
data_cluster <- data_cluster %>% filter(dimension1_dataset %in% flagship_stores)



 #################################################################
 #####                                                       #####
 #####         STEP 3: CLUSTERING STORES WITH K-MEANS        #####
 #####                                                       #####
 #################################################################


#------------------------------------------------------------------------#
#             Let's check the correlation between our variables          #
#------------------------------------------------------------------------#

cor_var <- cor(data_cluster)
plot_ly(z = cor_var,type = "heatmap")



#------------------------------------------------------------------------#
#        Optimize K-Means selecting the best performing algorithm        #
#------------------------------------------------------------------------#

# Set random seed
set.seed(123)

# Determine the optimal amount of clusters
fviz_nbclust(data_cluster[c(-1)], kmeans, method = "silhouette")

# Create vector with the different algorithms and a data frame with their iterations

Algorithms         <-c("Hartigan-Wong","Lloyd","Forgy","MacQueen") 
AmountAlgorithms <-length(Algorithms) # guarda la cantidad de Algorithms usados
Iterations        <-data.frame(InterClass=numeric(),Algorithm=character())



#------------------------------------------------------------------------#
#        Iterate k-means 100 times on each algorithm and save on         #              
#          'Iterations' the inter-cluster distance per iteration         #
#------------------------------------------------------------------------#

for (i in 1:AmountAlgorithms) 
{
  for (ii in 1:10) 
  {
    Model      <- kmeans((data_cluster[c(-1)]),4, iter.max = 100, algorithm = Algorithms[i])
    Iterations <- rbind(Iterations,
                         data.frame(InterClass = Model$betweenss,
                                    Algorithm = Algorithms[i]))
  }
}



#------------------------------------------------------------------------#
#        Calculate the average inter-cluster distance for each           #              
#          algorithm and identify the winning one                        #
#------------------------------------------------------------------------#

Results <- tapply(Iterations$InterClass,Iterations$Algorithm,mean) 
Results <-sort(Results,decreasing = T)
WinningAlgo <-names(Results[1])



#------------------------------------------------------------------------#
# K-means for each winning algorithm and assign a cluster to each store  #
#------------------------------------------------------------------------#


results <- kmeans((data_cluster[c(-1)]),4, iter.max = 100, algorithm = WinningAlgo, nstart = 25 )
results


#------------------------------------------------------------------------#
#                   Visualize the results                                #
#------------------------------------------------------------------------#

fviz_cluster(results, geom = "point", choose.vars = c("variable_1", "variable_2"), data_cluster[c(-1)])

# Optional: Visualization of our segmentation per winning algorithm
 plot(data_cluster$variable_1, data_cluster$variable_2,col=results$cluster,cex.axis=.7,cex.lab=.7)
 text(data_cluster$variable_1, data_cluster$variable_2,
      labels=data_cluster$Name,pos=1,col=results$cluster,cex=.7)
 title(main=paste("Winning Algorithm:",WinningAlgo),cex.main=.9)
 points(results$centers,col=results$cluster, pch=15,cex=2)

# Optional: Visualization of our winning algorithms and all variables
with(data_cluster, pairs(data_cluster, col=c(1:10)[results$cluster])) 



#------------------------------------------------------------------------#
#         Asign cluster number to each store and save results            #
#------------------------------------------------------------------------#

res_clus <- data.frame(data_cluster, results$cluster)
res_centers <- data.frame(results$centers)

write.csv(res_clus, file="C:/Users/youruserandpath/Results_clus.csv")  
write.csv(res_centers, file="C:/Users/youruserandpath/Results_cent.csv")  

