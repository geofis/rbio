make.cepnames2 <- function (names, seconditem = FALSE)
{
  #Modified from vegan::make.cepnames.
  #Suitable to rename not only species names as colnames,
  #but also species names as vectors in columns.
  #After running, check for possible duplicates
  names <- make.names(names, unique = F)
  names <- gsub("\\.[\\.]+", ".", names)
  names <- gsub("\\.$", "", names)
  names <- lapply(strsplit(names, "\\."), function(x) if (length(x) > 
                                                          1) 
    substring(x, 1, 4)
    else x)
  names <- unlist(lapply(names, function(x) if (length(x) > 
                                                1) 
    paste(x[c(1, if (seconditem) 2 else length(x))], collapse = "")
    else x))
  names <- abbreviate(names, 8)
  names <- make.names(names, unique = F)
  names
}
