moranlisa <- function(sp, nomel, nomel2, variable, estilo = c('B','W'), mtest = 'none', dmin = 0, dmax = 3, sign = 0.05, colgraf = 6){
  #FUNCION moranlisa
  #REQUISITOS: transectos de 50x2 m en forma de objeto espacial de clase SpatialPointsDataFrame, cada poligono un quadrat 2x2 m...
  ##...con variables a las que interesa evaluar dependencia espacial general y local por Moran, y de las que se haran mapas lisa.
  ##En el caso mas comun, se evalua la densidad de abundancia y de especies, pero tambiéns se puede evaluar area basal.
  ##Los quadrats deben estar identificados por ID unico
  #ARGUMENTOS
  #sp: objeto sp que contiene los transectos
  #nomel: nombre de la columna que identifica la unidad muestral, el transecto
  #nomel2: nombre de la columna que identifica la subunidad muestral, el quadrat
  #variable: la variable a la que se evaluara dependencia
  #estilo: estilo de los pesos para la evaluacion de dependencia global y local
  #mtest: metodo de ajuste del valor de p mediante pruebas multiples (multiple tests). Opciones: 'bonferroni', 'holm', 'hochberg', 'hommel', 'fdr'
  #dmin, dmax: distancia minima y maxima para el objeto de vecindad
  #sign: nivel de significancia para la prueba de hipotesis de aleatoriedad de Moran, tanto la global como la local
  #colgraf: numero de columnas del facet del grafico de Moran
  #VALOR
  #testMoran: prueba de Moran global
  #ml: prueba de Moran local
  #q2sdHHnoedge: quadrats solitarios en bordes
  #print(Grafico de Moran por transecto)
  #print(Grafico de celdas con dependencia espacial)
  #print(Grafico de celdas con variable evaluada)
  require(sp)
  require(spdep)
  require(dplyr)
  require(tidyr)
  require(ggplot2)
  #VERIFICACION DE ARGUMENTOS
  estilo <- match.arg(estilo)
  #COLUMNA 'x' EN sp
  sp@data[,'x'] <- coordinates(sp)[,1]
  #OBJETOS DE VECINDAD Y PESOS
  nb <- sapply(levels(sp@data[,nomel]), function(x) dnearneigh(sp[sp@data[,nomel]==x,], d1 = dmin, d2 = dmax), simplify = F)
  if(estilo=='B') w <- sapply(levels(sp@data[,nomel]), function(x) nb2listw(nb[[x]], style="B"), simplify = F)
  if(estilo=='W') w <- sapply(levels(sp@data[,nomel]), function(x) nb2listw(nb[[x]], style="W"), simplify = F)
  #PRUEBA DE MORAN
  mt <- sapply(levels(sp@data[,nomel]), function(x) p = moran.test(sp@data[sp@data[,nomel]==x,variable], w[[x]])$p.value)
  mt.t <- data.frame(p = mt)
  mt.t[,nomel] <- rownames(mt.t)
  mt.t[,'aemt'] <- ifelse(mt.t[,'p']<sign, 'autocorrelacionado', 'no autocorrelacionado')
  #MORAN LOCAL
  v_lv <- plyr::ldply(
    sapply(
      levels(sp@data[,nomel]), function(x) {
        var <- sp@data[sp@data[,nomel]==x, variable]
        data.frame(
          c1 = sp@data[sp@data[,nomel]==x, nomel2],
          c2 = var,
          c3 = lag.listw(w[[x]], var),
          p = localmoran(var, w[[x]], p.adjust.method = mtest)[,'Pr(z > 0)'],
          x = sp@data[sp@data[,nomel]==x, 'x'],
          Ii = localmoran(var, w[[x]])[,'Ii'], p.adjust.method = mtest)
      },
      simplify = F),
    data.frame, .id = nomel)
  #  colnames(v_lv)[2:4] <- c(nomel2, variable, paste0('l',variable))
  colnames(v_lv)[grep('^c1$', colnames(v_lv))] <- nomel2
  colnames(v_lv)[grep('^c2$', colnames(v_lv))] <- variable
  colnames(v_lv)[grep('^c3$', colnames(v_lv))] <- paste0('l',variable)
  mediaspornomel <- sapply(v_lv[,grep(paste0('^',variable,'$|^l',variable,'$'), colnames(v_lv))], function(x) ave(x, v_lv[,nomel]))
  colnames(mediaspornomel) <- paste0('m', colnames(mediaspornomel))
  v_lv <- cbind(v_lv, mediaspornomel)
  v_lv[,'quadrant'] <- factor(ifelse(v_lv[,'p']>sign, "not signif.",
                                     ifelse(v_lv[,grep(paste0('^',variable,'$'), colnames(v_lv))]>v_lv[,grep(paste0('^','m',variable,'$'), colnames(v_lv))]&v_lv[,grep(paste0('^','l',variable,'$'), colnames(v_lv))]>v_lv[,grep(paste0('^','ml',variable,'$'), colnames(v_lv))], "High-High",
                                            ifelse(v_lv[,grep(paste0('^',variable,'$'), colnames(v_lv))]<v_lv[,grep(paste0('^','m',variable,'$'), colnames(v_lv))]&v_lv[,grep(paste0('^','l',variable,'$'), colnames(v_lv))]<v_lv[,grep(paste0('^','ml',variable,'$'), colnames(v_lv))], "Low-Low",
                                                   ifelse(v_lv[,grep(paste0('^',variable,'$'), colnames(v_lv))]>v_lv[,grep(paste0('^','m',variable,'$'), colnames(v_lv))]&v_lv[,grep(paste0('^','l',variable,'$'), colnames(v_lv))]<v_lv[,grep(paste0('^','ml',variable,'$'), colnames(v_lv))], "High-Low",
                                                          ifelse(v_lv[,grep(paste0('^',variable,'$'), colnames(v_lv))]<v_lv[,grep(paste0('^','m',variable,'$'), colnames(v_lv))]&v_lv[,grep(paste0('^','l',variable,'$'), colnames(v_lv))]>v_lv[,grep(paste0('^','ml',variable,'$'), colnames(v_lv))], "Low-High",'NA'))))))
  #GRAFICO DE MORAN
  dev.new()
  mp <- ggplot(v_lv, aes_string(variable, paste0('l',variable))) +
    geom_vline(aes_string(xintercept = paste0('m',variable)),colour = "black", linetype = "longdash") +
    geom_hline(aes_string(yintercept = paste0('ml',variable)), colour = "black", linetype = "longdash") +
    geom_line(stat = 'smooth', method = "lm", formula = y ~ x, colour = "black", alpha = 0.5, size = 2) +
    geom_point(aes(fill = quadrant), shape=21, size=4) +
    scale_fill_manual(values = c("High-High" = "red", "Low-Low" = "blue", "High-Low" = "lightblue", "Low-High" = "pink", "not signif." = "light grey"))+
    facet_wrap(~get(nomel), ncol = colgraf, scales = 'free') +
    theme(text = element_text(size = 18),
          panel.background = element_rect(fill = 'white', colour = 'black'),
          panel.grid.major = element_line(colour = "grey", linetype = "dashed", size = 0.25),
          strip.background = element_rect(colour = "black", fill = "black"),
          strip.text.x = element_text(colour = "white", face = "bold")) +
    coord_cartesian()
  print(mp)
  #MAPA LISA. APLICA A TRANSECTO, DONDE LA COORDENADA x YA HA SIDO PREVIAMENTE CALCULADA EN EL OBJETO sp
  v_lv[,nomel] <- factor(v_lv[,nomel], rev(levels(v_lv[,nomel])))
  v_lv_rect <- data.frame(c1 = as.integer(rep(1,length(levels(v_lv[,nomel])))), c2 = 1:length(levels(v_lv[,nomel])))
  colnames(v_lv_rect) <- c('x', 'y2')
  dev.new()
  lisa <- ggplot(v_lv %>% dplyr::select_(x='x', y2=nomel, quadrant='quadrant'), aes_string(x = 'x', y = 'y2')) +
    geom_tile(aes(fill = quadrant), colour = 'white', lwd = 0.5) +
    scale_fill_manual(values = c("High-High" = "red", "Low-Low" = "blue", "High-Low" = "lightblue", "Low-High" = "pink", "not signif." = "light grey")) +
    coord_fixed(ratio = 2) +
    theme(text = element_text(size = 18), legend.position =  c(1.10, 0.5)) +
    geom_rect(data = v_lv_rect, size=1, fill=NA, colour="black", aes(xmin = 0, xmax = 50, ymin = y2 - 0.5, ymax = y2 + 0.5)) +
    scale_y_discrete(nomel, expand = c(0,0)) +
    scale_x_continuous(breaks=seq(1,49,2), labels=seq(1,49,2), expand = c(0,0))
  print(lisa)
  dev.new()
  vartile <- ggplot(v_lv %>% dplyr::select_(x='x', y2=nomel, variable), aes_string(x = 'x', y = 'y2')) +
    geom_tile(aes_string(fill = variable), colour = 'white', lwd = 0.5) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) +
    coord_fixed(ratio = 2) +
    theme(text = element_text(size = 18), legend.position = c(1.10, 0.5)) +
    geom_rect(data = v_lv_rect, size=1, fill=NA, colour="black", aes(xmin = 0, xmax = 50, ymin = y2 - 0.5, ymax = y2 + 0.5)) +
    scale_y_discrete(nomel, expand = c(0,0)) +
    scale_x_continuous(breaks=seq(1,49,2), labels=seq(1,49,2), expand = c(0,0))
  print(vartile)
  #QUITAR QUADRATS SOLITARIOS EN BORDES
  q2sdHHnoedge <- droplevels(as.data.frame(v_lv %>% filter(!grepl('not signif.|Low-Low', quadrant)) %>% group_by_(nomel) %>% dplyr::mutate(n=n()) %>% filter(n>1)))
  nomeledge1del <- names(
    which(
      !sapply(
        levels(q2sdHHnoedge[,nomel]), function(x)
          all(any(q2sdHHnoedge[q2sdHHnoedge[,nomel]==x,'x'] %in% 1) & any(q2sdHHnoedge[q2sdHHnoedge[,nomel]==x,'x'] %in% 3)),
        simplify = T)
    )
  )
  nomeledge49del <- names(
    which(
      !sapply(
        levels(q2sdHHnoedge[,nomel]), function(x)
          all(any(q2sdHHnoedge[q2sdHHnoedge[,nomel]==x,'x'] %in% 47) & any(q2sdHHnoedge[q2sdHHnoedge[,nomel]==x,'x'] %in% 49)),
        simplify = T)
    )
  )
  q2sdHHnoedge <- q2sdHHnoedge[!(q2sdHHnoedge[,nomel] %in% nomeledge1del & q2sdHHnoedge[,'x'] %in% 1),]
  q2sdHHnoedge <- q2sdHHnoedge[!(q2sdHHnoedge[,nomel] %in% nomeledge49del & q2sdHHnoedge[,'x'] %in% 49),]
  q2sdHHnoedge <- droplevels(q2sdHHnoedge)
  #sp ACTUALIZADO
  v_lv <- base::merge(sp@data, v_lv[,-grep(paste0(nomel,'|^',variable,'|^x'),colnames(v_lv))], by = nomel2)
  # return(list(vecindad = nb, pesos = w, testMoran = mt.t, varlagged = v_lv, rect=v_lv_rect, q = q2sdHHnoedge))
  return(list(testMoran = mt.t, ml=v_lv, q = q2sdHHnoedge[,nomel2], moranscatterplot=mp, lisamap=lisa))
}
