fusion_levels <- function(
  ## Slightly modified from Borcard et al. (2018): Numerical ecology with R
  clustering = qab.dch.single, #hclust object
  cm = cm.abun, #Community matrix
  clusmet = 'Single linkage' #Name of the clustering method
){
  plot(
    clustering[['height']],
    nrow(cm):2,
    type = "S",
    main = paste("Fusion levels", clusmet),
    ylab = "k (number of clusters)",
    xlab = "h (node height)",
    col = "grey"
  )
  text(
    clustering[['height']],
    nrow(cm):2,
    nrow(cm):2,
    col = "red",
    cex = 0.8)
}
