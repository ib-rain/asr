library(readxl)
library(ggplot2)
library(factoextra)
library(amap)
library(fpc)

t5.dat = na.omit(read_excel("Student 1/Task_5.xls"))

names(t5.dat) = c("Brand", "Calories", "Sodium", "Alcohol", "Price")

t5 = data.matrix(t5.dat[c(-1)])
rownames(t5) = t5.dat[[1]]

research_clust = function(data, data_name, dists){
  data = scale(na.omit(data))
  a = fviz_nbclust(x = data, FUNcluster = kmeans, method = "silhouette")
  plot(a)
  a_data = a$data
  # opt_n_clust = as.numeric(a_data$clusters[which.max(a_data$y)])
  opt_n_clust = 5
  for (dist_method in dists){
    km.res = Kmeans(x = data, centers = opt_n_clust, method = dist_method)
    plot(fviz_cluster(km.res, data = data, frame.type = "convex",
                      main = paste("Cluster plot for", data_name, "using", dist_method)))
  }
}

research_hclust = function(data, data_name, dists, clusts){
    for (dist_method in dists){
      for (clust_method in clusts){
        d = dist(data, method = dist_method)
        res.hc = hclust(d, method = clust_method)
        plot(res.hc, main = paste("Dendrogram for", data_name, "using", dist_method, "and", clust_method))
      }
    }
}

dist_methods = list("euclidean", "maximum", "manhattan")

research_clust(t5, "Brand", dist_methods)

clust_methods = list("single", "complete", "average", "centroid", "ward.D2")

research_hclust(t5, "Brand", dist_methods, clust_methods)