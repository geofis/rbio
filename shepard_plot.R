shepard_plot <- function(
  # Slightly modified from Borcard et al. (2018): Numerical ecology with R
  d = qab.dch, #Distance matrix
  coph = qab.dch.single.coph, #Cophenetic matrix
  xlab = 'Chord Distance', #Label for x-axis, type of distance
  clusmet = 'UPGMA', #Name of the clustering method
  xlim = c(0, sqrt(2)),
  ylim = c(0, sqrt(2))
){
  plot(
    d,
    coph,
    xlab = xlab,
    ylab = "Cophenetic distance",
    asp = 1,
    xlim = xlim,
    ylim = ylim,
    main = paste(
      clusmet,
      '\n',
      paste0('Cophenetic correlation = '), round(cor(d, coph), 3)
    )
  )
  abline(0, 1)
  lines(lowess(d, coph), col = "red")
}