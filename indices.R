#VARIOS INDICES DE DIVERSIDAD
dind <- function(mc, nomsitios=NA){
  require(vegan)
  require(BiodiversityR)
  require(fossil)
  zz1 <- cbind(
    ID=rownames(mc),
    as.data.frame(
      sapply(
        c("richness",
          "abundance",
          "Shannon",
          "Simpson",
          "inverseSimpson",
          "Logalpha",
          "Berger",
          "Jevenness",
          "Eevenness" ),
        function(x)
          if(
            package_version(packageVersion("BiodiversityR")) >
            package_version("2.9-1"))
            diversityresult(mc,index=x,method='each site', digits = 4) else
              diversityresult(mc,index=x,method='s', digits = 4)
      )
    )
  )
  chao <- data.frame(ID=rownames(mc), `estimador Chao1`=sapply(rownames(mc), function(x) chao1(mc[x,], taxa.row = F)))
  jack <- data.frame(ID=rownames(mc), `estimador Jackknife`=sapply(rownames(mc), function(x) jack1(mc[x,], taxa.row = F)))
  zz2 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(zz1, chao, jack))
  # zz2 <- base::merge(zz, chao)
  colnames(zz2)[2:ncol(zz2)] <- c('riqueza','abundancia','Shannon','Gini-Simpson','inverso de Simpson','Fisher-alpha o Logalpha','Berger-Parker','equidad de Pielou o J-evenness','equidad de Buzas-Gibson o E-evenness', 'estimador Chao1', 'estimador Jackknife')
  if(!is.na(nomsitios)) colnames(zz2)[1] <- nomsitios
  return(zz2)
}
