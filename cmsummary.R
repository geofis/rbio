cmsummary <- function(cm = NULL, dig = 2, seed = 131, partial = T, alpha = 0.05, pa = F){
  # cm      Data frame containing a community matrix
  # dig     Numeric. Number of significant digits
  # seed    Numeric. Seed for the sample algorithm
  # partial Logical. If TRUE, the function returns partial views
  # alpha   Numeric. Threshold for the null hypothesis of normality test
  # pa      Logical. Is the cm an occurrence matrix?
  require(psych)
  require(dplyr)
  require(tidyr)
  set.seed(seed = seed); selrows <- sample(nrow(cm), 6)
  set.seed(seed = seed); selcols <- sample(ncol(cm),3)
  view <- if(partial) round(cm[selrows, selcols], dig) else round(cm, 2)
  # summ <- t(apply(cm, 2, summary, digits = dig))
  if(pa)
    summ <- stats <- cm %>% gather(species, value) %>% group_by(species) %>% do(describe(.$value)) %>% dplyr::select(-vars) %>% as.data.frame()
    else
    summ <- inner_join(
      stats <- cm %>% gather(species, value) %>% group_by(species) %>% do(describe(.$value)) %>% dplyr::select(-vars),
      norm <- cm %>% gather(species, value) %>% group_by(species) %>% summarise(norm = if(shapiro.test(value)$p.value>=alpha) 'NORM.' else 'NOT NORM.'),
      by='species'
    ) %>% as.data.frame()
  if (partial) summ <- summ[selcols,]
  summ[sapply(summ, is.numeric)] <- round(summ[sapply(summ, is.numeric)], 2)
  # if (partial) summ <- summ %>% dplyr::filter(species %in% selcols) %>% print(width = Inf)
  z <- list(view, summ)
  names(z) <- c(
    paste0(
      if(partial) 'Partial view of ' else 'View of ', 
      'the community matrix'
    ),
    paste0(
      if(partial) 'Partial view of ' else 'View of ',
      'the statistical summary by species'
    )
  )
  return(z)
}
