data("USArrests")
options(repos = c(CRAN = "http://cran.rstudio.com"))
library("cluster")
res.agnes <- agnes(x = USArrests, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "average" # Linkage method
)
grp <- cutree(res.agnes, k = 4)
head(grp, n = 4)
mylist<-list(data = scale(USArrests), cluster = grp)
#.......................
res.agnes1 <- agnes(x = USArrests, # data matrix
                    stand = TRUE, # Standardize the data
                    metric = "manhattan", # metric for distance matrix
                    method = "complete" # Linkage method
)
# Cut tree into 4 groups
grp1 <- cutree(res.agnes1, k = 4)
head(grp1, n = 4)
mylist1<-list(data = scale(USArrests), cluster = grp1)
table(grp)
table(grp1)
#.......................

measurePrecisionRecall <- function(predict, actual_labels){
  precision <- sum(predict & actual_labels) / sum(predict)
  recall <- sum(predict & actual_labels) / sum(actual_labels)
  fmeasure <- 2 * precision * recall / (precision + recall)
  
  cat('precision:  ')
  cat(precision * 100)
  cat('%')
  cat('\n')
  
  cat('recall:     ')
  cat(recall * 100)
  cat('%')
  cat('\n')
  
  cat('f-measure:  ')
  cat(fmeasure * 100)
  cat('%')
  cat('\n')
}
measurePrecisionRecall(grp,grp1)

