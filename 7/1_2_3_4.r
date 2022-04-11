library(ggplot2)
library(factoextra)
library(amap)
library(fpc)

file01 <- read.csv("C:/Users/ivan/Desktop/mag/Applied Statistics in R/7/Datasets/file01.txt", sep="")
file02 <- read.csv("C:/Users/ivan/Desktop/mag/Applied Statistics in R/7/Datasets/file02.txt", sep="")
file07 <- read.csv("C:/Users/ivan/Desktop/mag/Applied Statistics in R/7/Datasets/file07.txt", sep="")
iris <- read.csv("C:/Users/ivan/Desktop/mag/Applied Statistics in R/7/Datasets/iris.data", header=FALSE)

f1 = data.matrix(file01[c(-1)])
rownames(f1) = file01[[1]]
f2 = data.matrix(file02[c(-1)])
rownames(f2) = file02[[1]]
f7 = data.matrix(file07[c(-1)])
rownames(f7) = file07[[1]]

fi = data.matrix(iris[c(-5)])
#rownames(fi) = iris[[5]]

fis = list(f1, f2, f7, fi)

data_names = list("sightings", "milk", "life_expectancy", "iris")

dist_methods = list("euclidean", "maximum", "manhattan")

clust_methods = list("single", "complete", "average", "centroid", "ward.D2")

research_hclust = function(data, data_name, dist_method, clust_method){
  d = dist(data, method = dist_method)
  res.hc = hclust(d, method = clust_method)
  plot(res.hc,
       main = paste("Dendrogram for", data_name, "using", dist_method, "and", clust_method))
}

research_clust = function(data, data_name, dists){
  data = scale(na.omit(data))
  a = fviz_nbclust(x = data, FUNcluster = kmeans, method = "silhouette")
  plot(a)
  a_data = a$data
  opt_n_clust = as.numeric(a_data$clusters[which.max(a_data$y)])
  for (dist_method in dists){
    km.res = Kmeans(x = data, centers = opt_n_clust, method = dist_method)
    plot(fviz_cluster(km.res, data = data, frame.type = "convex",
                      main = paste("Cluster plot for", data_name, "using", dist_method)))
  }
}

for (i in seq(length(fis))){
  for (dist_m in dist_methods)
    research_hclust(fis[[i]], data_names[[i]], dist_m, clust_methods[[4]])
  
  research_clust(fis[[i]], data_names[[i]], dist_methods)
}

build_iris_confusion_m = function(src, data, N, dists){
  d = scale(na.omit(data))
  res = list()
  for (i in seq(length(dists))){
    print(paste("Dist method:", dists[[i]]))
    
    km.res = Kmeans(x = data, centers = N, method = dists[[i]])
    res[[i]] = c(km.res$cluster)
    d = dist(data, method = dists[[i]])
    
    print(cluster.stats(d, src, res[[i]], compareonly = TRUE))
  }
  print(res)
}

for (n in seq(2,3))
  build_iris_confusion_m(as.numeric(iris[[5]]), fi, n, dist_methods)