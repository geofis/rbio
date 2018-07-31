#FUNCION PARA HACER UN ORDINATION PLOT CON ggplot2
ordiplotgg <- function(mi = d, colsit = 'sitio', colsp = 'especie', env = d.env, filt = c('top','freq'), nfilt = NULL, spl = F, stress = F, colmi = c('growth habit'='fv'), colenv = c('rock type'='derivado_litologia_corta'), pal = brewer.pal(12, 'Set3'), svg = F){
  #mi       Matriz de individuos
  #colsit   Columna de sitios en matriz de individuos
  #colsp    Columna de especies en matriz de individuos
  #env      Matriz ambiental
  #filt     Metodo de seleccion de especies que apareceran en el grafico. 'top' elige la nfilt primeras, y 'freq' las que tengan abundancia >= nfilt
  #nfilt    Numero de especies mas abundantes o abundancia de corte
  #spl      Mostrar o no lista de especies en orden decreciente segun abundancia
  #stress   Mostrar o no el stressplot
  #colmi    Columna para colorear rotulos de especies. Delante del nombre de columna debe aparecer el nombre con el que se desea que aparezca en la leyenda
  #colenv   Columna para colorear rotulos de sitios. Delante del nombre de columna debe aparecer el nombre con el que se desea que aparezca en la leyenda
  #pal      Paleta para colorear especies y sitios
  #svg      Generar o no archivo SVG
  require(reshape2)
  require(dplyr)
  require(ggplot2)
  require(ggrepel)
  require(scales)
  require(vegan)
  require(MASS)
  library(RColorBrewer)
  mi[,'name'] <- gsub("^((\\w+\\W+){1}\\w+).*$","\\1",mi[,colsp])
  mc <- dcast(mi, paste(colsit,'~','name'), fun.aggregate = length, value.var = colsit)
  rownames(mc) <- mc[,1]
  mc <- mc[,2:ncol(mc)]
  dis <- vegdist(mc)
  iso <- isoMDS(dis)
  if(stress) {dev.new();stplot <- stressplot(iso, dis); print(stplot)}
  p <- as.data.frame(iso['points'])
  colnames(p) <- c('Dim1','Dim2')
  p[,'type'] <- rep('sample',nrow(p))
  p[,'name'] <- row.names(p)
  if(!is.null(colenv)){
    p <- merge(p, env[,c(colsit,colenv)], by.x = 'name', by.y = colsit)
    colnames(p)[grep(colenv, colnames(p))] <- names(colenv) 
  }
  s <- as.data.frame(wascores(iso$points, mc, expand = T))
  colnames(s) <- c('Dim1','Dim2')
  s[,'name'] <- row.names(s)
  s[,'type'] <- rep('species',nrow(s))
  if(!is.null(colmi)){
    s <- merge(s,unique(mi[,c('name',colmi)]))
    colnames(s)[grep(colmi, colnames(s))] <- names(colmi)
  }
  spdesc <- mi %>% select_('name') %>% plyr::count() %>% arrange(desc(freq))
  ifelse(filt=='top', spfilt <- spdesc[1:nfilt,'name'], spfilt <- spdesc[spdesc[,'freq']>=nfilt,'name'])
  s <- droplevels(s[s[,'name'] %in% spfilt,])
  nivcolor = c(levels(p[,names(colenv)]), levels(s[,names(colmi)]))
  ifelse(spl, l <- spdesc, l <- "Para desplegar lista completa de abundancia por especies, introduzca argumento spl = T")
  dev.new()
  op <- ggplot() +
    geom_point(data=p, aes(Dim1, Dim2), shape = 16, size = 3) +
    geom_point(data=s, aes(Dim1, Dim2), shape = 3, size = 3) +
    geom_label_repel(data=p,
                     if(is.null(colenv)) aes(Dim1, Dim2, label=name) else aes(Dim1, Dim2, label=name, fill = p[,names(colenv)]),
                     size = 8, colour = 'black', fontface = 'bold', nudge_y = 0.03) +
    geom_label_repel(data=s,
                     if(is.null(colmi)) aes(Dim1, Dim2, label=name) else aes(Dim1, Dim2, label=name, fill = s[,names(colmi)]),
                     size = 5, colour = 'black', fontface = 'bold', nudge_x = 0.03, force = 3) +
                     {if(is.null(colenv)&is.null(colmi)) NULL else {
                       scale_fill_manual(name = {if(is.null(colmi)) names(colenv) else if(is.null(colenv)) names(colmi) else paste(names(colenv),'|',names(colmi))},
                                         breaks = c(nivcolor),
                                         labels = c(nivcolor),
                                         values = pal)}
                     } +
    theme_light() +
    geom_hline(aes(yintercept=0), size = 0.4, linetype="dashed") +
    geom_vline(aes(xintercept=0), size = 0.4, linetype="dashed")
  print(op)
  if(svg) {
    svg(filename="Std_SVG.svg", width = 18, height = 12, pointsize = 24)
    print(op)
    dev.off()
  }
  return(
    list(sites = p,
         species = s,
         l = l,
         colorear = if(is.null(nivcolor)) NULL else sort(nivcolor)
    )
  )
}
