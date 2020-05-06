get_silhouette_score <- function (dataset, k, method, dist_metric, iter_max, iters = 10) {
  mean_sil <- 0
  for (i in seq(1, iters)) {
    cls <- NULL
    if(method == 'kmeans') {
      cls <- kmeans(dataset, k, iter.max = iter_max, nstart = 25)
    }
    else {
      cls <- pam(dataset, k, metric = dist_metric, stand = TRUE)
    }
    sil <- silhouette(cls$cluster, dist(dataset))
    mean_sil <- mean_sil + mean(sil[, 3])
  }
  return(mean_sil / iters)
}

get_dbi_score <- function (dataset, k, method, dist_metric, iter_max, iters = 10) {
    mean_dbi <- 0
  for (i in seq(1, iters)) {
    if(method == 'kmeans') {
      cls <- kmeans(dataset, k, iter.max = iter_max, nstart = 25)
      centrotypes <- "centroids"
      di <- NULL
    }
    else {
      cls <- pam(dataset, k, metric = dist_metric, stand = TRUE)
      centrotypes <- "medoids"
      di <- dist(dataset)
    }
    dbi <- index.DB(dataset, cls$cluster, centrotypes = centrotypes, d = di)
    mean_dbi <- mean_dbi + mean(dbi$DB)
  }
  return(mean_dbi / iters)
}

get_dunn_score <- function (dataset, k, method, dist_metric, iter_max, iters = 10) {
    mean_dunn <- 0
  for (i in seq(1, iters)) {
    if(method == 'kmeans') {
      cls <- kmeans(dataset, k, iter.max = iter_max, nstart = 25)
    }
    else {
      cls <- pam(dataset, k, metric = dist_metric, stand = TRUE)
    }
    dunn <- dunn(clusters = cls$cluster, Data = dataset)
    mean_dunn <- mean_dunn + mean(dunn)
  }
  return(mean_dunn / iters)
}

get_purity_score <- function (dataset, targets, k, method, dist_metric, iter_max, iters = 10) {
    mean_purity <- 0
  for (i in seq(1, iters)) {
    if(method == 'kmeans') {
      cls <- kmeans(dataset, k, iter.max = iter_max, nstart = 25)
    }
    else {
      cls <- pam(dataset, k, metric = dist_metric, stand = TRUE)
    }
    pur <- purity(targets, cls$cluster)
    mean_purity <- mean_purity + mean(pur$pur)
  }
  return(mean_purity / iters)
}