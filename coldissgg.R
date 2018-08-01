coldissgg <- function(dist, vertical=F) {
  require(reshape2)
  require(tidyr)
  require(dplyr)
  require(gclus)
  require(gridExtra)
  dist.g <- melt(as.matrix(dist))
  dist.g$type <- 'Dissimilarity matrix'
  dist.g.o <- dist.g
  dist.g.o$Var1 <- factor(dist.g.o$Var1, levels=levels(dist.g.o$Var1)[order.single(1-dist)])
  dist.g.o$Var2 <- factor(dist.g.o$Var2, levels=levels(dist.g.o$Var2)[order.single(1-dist)])
  dist.g.o$type <- 'Ordered dissimilarity matrix'
  gg1 <- ggplot(dist.g, aes(Var1, Var2)) +
    geom_tile(aes(fill=value), colour = "white") +
    scale_fill_gradient(low = "darkgreen", high = "white", na.value = 'white') +
    geom_text(aes(label=round(value,2))) +
    labs(title='Dissimilarity matrix') +
    theme(
      text = element_text(size = 16),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position = 'none',
      plot.title = element_text(size=20, hjust = 0.5)
      ) +
    coord_equal()
  gg2 <- ggplot(dist.g.o, aes(Var1, Var2)) +
    geom_tile(aes(fill=value), colour = "white") +
    scale_fill_gradient(low = "darkgreen", high = "white", na.value = 'white') +
    geom_text(aes(label=round(value,2))) +
    labs(title='Ordered dissimilarity matrix') +
    theme(
      text = element_text(size = 16),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position = 'none',
      plot.title = element_text(size=20, hjust = 0.5)
    ) +
    coord_equal()
  if(vertical){
    grid.arrange(gg1, gg2, nrow=2)
  } else{
    grid.arrange(gg1, gg2, ncol=2)
  }
}