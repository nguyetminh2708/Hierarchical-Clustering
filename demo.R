data("USArrests")
options(repos = c(CRAN = "http://cran.rstudio.com"))
library("cluster")
res.agnes <- agnes(x = USArrests, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "average" # Linkage method
)
install.packages("factoextra")
library("factoextra")
fviz_dend(res.agnes, cex = 0.5, k = 4, k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"))
res.coph <- cophenetic(res.agnes)
cor(res.dist, res.coph)
#.............
res.agnes1 <- agnes(x = USArrests, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "complete" # Linkage method
)
cor(res.dist, cophenetic(res.agnes1))
# Cut tree into 4 groups
grp <- cutree(res.agnes, k = 4)
head(grp, n = 4)
# Number of members in each cluster
table(grp)
mylist<-list(data = scale(USArrests), cluster = grp)
fviz_cluster(mylist,
             palette = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"),
             ggtheme = theme_minimal())
# DIvisive ANAlysis Clustering
res.diana <- diana(x = USArrests, # data matrix
                   stand = TRUE, # standardize the data
                   metric = "euclidean", # metric for distance matrix
)
fviz_dend(res.diana, cex = 0.5, k = 4, k_colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A"))
