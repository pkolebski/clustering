plot_sil  <- function (report, max_k) {
  df <- data.frame(val=double(), k=integer())
  for (i in seq(1, max_k-1)) {
    df <- rbind(df, data.frame(val = report[[i]]$sil, k=i + 1))
  }
  plot <- ggplot(df, aes(x=k, y=val)) + geom_line() + geom_point() + ggtitle("SIL")
  return(plot)
}
plot_dbi <- function (report, max_k) {
  df <- data.frame(val=double(), k=integer())
  for (i in seq(1, max_k-1)) {
    df <- rbind(df, data.frame(val = report[[i]]$dbi, k=i + 1))
  }
  plot <- ggplot(df, aes(x=k, y=val)) + geom_line() + geom_point() + ggtitle("DBI")
  return(plot)
}
plot_dunn <- function (report, max_k) {
  df <- data.frame(val=double(), k=integer())
  for (i in seq(1, max_k-1)) {
    df <- rbind(df, data.frame(val = report[[i]]$dunn, k=i + 1))
  }
  plot <- ggplot(df, aes(x=k, y=val)) + geom_line() + geom_point() + ggtitle("DUNN")
  return(plot)
}

plot_pur <- function (report, max_k) {
  df <- data.frame(val=double(), k=integer())
  for (i in seq(1, max_k-1)) {
    df <- rbind(df, data.frame(val = report[[i]]$pur, k=i + 1))
  }
  plot <- ggplot(df, aes(x=k, y=val)) + geom_line() + geom_point() + ggtitle("Purity")
  return(plot)
}

plot_results <- function (results, optimal, max_k, name) {
  custom_theme <- theme(
    plot.title = element_text(hjust = 0.5, size = 25),
    axis.text.x = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 20),
  )
  optimal_line <- geom_vline(xintercept = optimal, linetype = 2)
  p_dbi <- plot_dbi(results, max_k) + optimal_line + custom_theme
  p_sil <- plot_sil(results, max_k) + optimal_line + custom_theme
  p_dunn <- plot_dunn(results, max_k) + optimal_line + custom_theme
  p_pur <- plot_pur(results, max_k) + optimal_line + custom_theme
  p <- grid.arrange(
    p_sil,
    p_dbi,
    p_dunn,
    p_pur,
    ncol = 1,
    nrow = 4,
    top = textGrob(name, gp = gpar(fontsize=35, font=1))
  )
  ggsave(
    name,
    p,
    "png",
    path = 'visualizations/',
    width = 210 / 6,
    height = 297 / 6,
    dpi = 700,
    units = 'cm',
    limitsize = FALSE
  )
}