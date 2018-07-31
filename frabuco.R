frabuco <- function(cm = NULL, n = 0, f = 0){
  # cm    Community matrix
  # n     Abundance cutoff across all the sites
  # f     Frequency cutoff across all the sites
  spn <- names(cm[,colSums(cm)>=n])
  spf <-
    names(
      which(
        sapply(
          cm,
          function(x)
            length(which(x>0))>=f)
        )
      )
  spnf <- intersect(spn, spf)
  z <- list(spn, spf, spnf)
  names(z) <- 
    c(
      paste0('Species with ', n, ' individuals or more across all the sites [', length(spn), ' species]'),
      paste0('Species with ', f, ' occurrences or more across all the sites [', length(spf), ' species]'),
      paste0('Species with ', n, ' individuals or more AND with ', f, ' occurrences or more across all the sites [', length(spnf), ' species]')
    )
  return(z)
}
