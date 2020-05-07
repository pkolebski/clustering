set.seed(42)
library(factoextra)
library(grid)
library(gridExtra)
library(BBmisc)
library(cluster)
library(clusterSim)
library(clValid)
library(ggplot2)
library(funtimes)

source("plotting.r")
source("scorers.r")

iris <- NULL
glass <- NULL
wine <- NULL
seeds <- NULL

iris$dataset <- read.csv("/home/pkolebski/Projects/Danologia/imad/lab/decision_trees/data/iris/iris.data")
iris$name <- 'Iris Dataset'
glass$dataset <- read.csv("/home/pkolebski/Projects/Danologia/imad/lab/decision_trees/data/glass/glass.data")
glass$name <- 'Glass Dataset'
wine$dataset <- read.csv("/home/pkolebski/Projects/Danologia/imad/lab/decision_trees/data/wine/wine.data")
wine$name <- "Wine Dataset"
seeds$dataset <- read.csv("/home/pkolebski/Projects/Danologia/imad/lab/decision_trees/data/seeds/seeds_dataset2.csv", sep="\t")
seeds$name <- "Seeds Dataset"

clusters_analize <- function(dataset, max_k, method, iter_max = 10, dist_metric = 'euclidean'){
  targets <- dataset$Target
  targets <- as.numeric(factor(targets))
  dataset <- subset(dataset, select = -Target)
  dataset <- normalize(dataset, method = 'range')
  results <- list()
  for (k in seq(1, max_k - 1)) {
    if (method == 'kmeans') {
      res <- kmeans(dataset, centers = k + 1, iter.max = iter_max, nstart = 25)
    }
    else {
      res <- pam(dataset, k + 1, metric = dist_metric, stand = TRUE)
    }
    res$sil <- get_silhouette_score(dataset, k + 1, method, dist_metric, iter_max)
    res$dbi <- get_dbi_score(dataset, k + 1, method, dist_metric, iter_max)
    res$dunn <- get_dunn_score(dataset, k + 1, method, dist_metric, iter_max)
    res$pur <- get_purity_score(dataset, targets, k + 1, method, dist_metric, iter_max)
    results[[k]] <- res
  }
  return(results)
}

analyze_dataset <- function (dataset, max_k) {
  print(dataset$name)
  optimal <- length(unique(dataset$dataset$Target))

  analyze_kmeans <- clusters_analize(dataset$dataset, max_k = max_k, method = 'kmeans')
  plot_results(analyze_kmeans, optimal, max_k, paste(dataset$name, "- kmeans"))

  analyze_pam <- clusters_analize(dataset$dataset, max_k = max_k, method = 'pam')
  plot_results(analyze_pam, optimal, max_k, paste(dataset$name, "- pam"))
}

main <- function () {
  analyze_dataset(iris, 20)
  analyze_dataset(wine, 20)
  analyze_dataset(glass, 20)
  analyze_dataset(seeds, 20)
}

main()


vis_clusters(3, wine$dataset, "Wine Dataset")
vis_clusters(3, seeds$dataset, "Seeds Dataset")
vis_clusters(6, glass$dataset, "Glass Dataset")
