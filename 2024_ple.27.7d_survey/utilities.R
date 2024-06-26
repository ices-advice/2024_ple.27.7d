# utilities.R - DESC
# /utilities.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
# Modified: Raphael Girardin (Ifremer), 2023 <raphael.girardin@ifremer.fr> to includes
# modified version of surveyIdxPlots function from surveyIndex packages. Allow the plot 
# of maps even if zero obs for a given year and age
#
# Distributed under the terms of the EUPL-1.2


# si2FLIndex {{{

si2FLIndex <- function(si, ...) {
  
  ages <- colnames(si$idx)
  years <- rownames(si$idx)
  dmns <- list(age=ages, year=years)
  
  # index
  idx <- FLQuant(t(si$idx), dimnames=dmns)
  
  # lowq, uppq
  lowq <- FLQuant(t(si$lo), dimnames=dmns)
  uppq <- FLQuant(t(si$up), dimnames=dmns)
  
  # FLIndex(index=FLQuantPoint(mean=idx, lowq=lowq, uppq=uppq))
  FLIndex(index=idx, ...)
} # }}}

# si2FLQuantPoint {{{

si2FLQuantPoint <- function(si, ...) {
  
  ages <- colnames(si$idx)
  years <- rownames(si$idx)
  dmns <- list(age=ages, year=years)
  
  # mean
  mean <- FLQuant(unname(t(si$idx)), dimnames=dmns)
  
  # lowq, uppq
  lowq <- FLQuant(unname(t(si$lo)), dimnames=dmns)
  uppq <- FLQuant(unname(t(si$up)), dimnames=dmns)
  
  FLQuantPoint(mean=mean, lowq=lowq, uppq=uppq)
} # }}}


surveyIdxPlots_Abmaps <- function (x, dat, alt.idx = NULL, myids, cols = 1:length(x$pModels), 
                                 select = c("map", absolutemap), par = list(mfrow = c(3, 3)), 
                                 colors = rev(heat.colors(6)), map.cex = 1, plotByAge = TRUE,
                                 legend = TRUE, predD = NULL, year = NULL, main = NULL, legend.signif = 3, 
                                 legend.pos = "topright", restoreOldPar = FALSE, ...) {
  if (!plotByAge & !is.null(par)) {
    op <- par(par)
    if (restoreOldPar) 
      on.exit(par(op))
  }
  mainwasnull <- is.null(main)
  for (a in cols) {
    print(a)
    if (mainwasnull) 
      main <- paste("Age group", colnames(dat$Nage)[a])
    if (plotByAge & !is.null(par)) {
      op <- par(par)
      if (restoreOldPar) 
        on.exit(par(op))
    }
    if (any(select == "map")) {
      xlims = range(dat$lon, na.rm = TRUE)
      ylims = range(dat$lat, na.rm = TRUE)
      mapvals = NULL
      if (is.null(predD)) {
        tmp = subset(dat, haul.id %in% myids)
      } else {
        tmp = predD
      }
      if (is.null(year)) {
        concT = surveyIndex:::concTransform(log(x$gPreds[[a]]))
        mapvals = x$gPreds[[a]]
      } else {
        y = which(as.numeric(as.character(names(x$gPreds2[[a]]))) == 
                    year)
        if (length(y) == 0) next 
          # stop(paste("Year", year, "age group", a, "not found."))
        concT = surveyIndex:::concTransform(log(x$gPreds2[[a]][[y]]))
        mapvals = x$gPreds2[[a]][[y]]
      }
      if (length(colors) > 1) zFac = cut(concT, 0:length(colors)/length(colors)) else zFac = 1
      if (length(map.cex) > 1) sFac = cut(log(x$gPreds[[a]]), length(map.cex)) else sFac = 1
      myCols = colors
      plot(tmp$lon, y = tmp$lat, col = 1, pch = 1, cex = map.cex[sFac], 
           xlim = xlims, ylim = ylims, xlab = "Longitude", 
           ylab = "Latitude", main = main)
      points(tmp$lon, y = tmp$lat, col = myCols[zFac], 
             pch = 16, cex = map.cex[sFac])
      maps::map("worldHires", xlim = xlims, ylim = ylims, 
                fill = TRUE, plot = TRUE, add = TRUE, col = grey(0.5))
      if (legend) {
        maxcuts = aggregate(mapvals ~ zFac, FUN = max)
        mincuts = aggregate(mapvals ~ zFac, FUN = min)
        mm = mean(mapvals)
        ml = signif(mincuts[, 2]/mm, legend.signif)
        ml[1] = 0
        leg = paste0("[", ml, ",", signif(maxcuts[, 2]/mm, 
                                          legend.signif), "]")
        legend(legend.pos, legend = leg, pch = 16, col = colors, 
               bg = "white")
      }
    }
  }
}

# extract index from surveyIdx object
gather.idx <- function(m, bound=T){
  if(is(m)!="surveyIdx") stop("Should provided a surveyIdx object")
  tmp <- as.data.frame(m$idx)
  tmp$Year <- rownames(tmp)
  idx <- tmp %>% gather(age, index, -Year)
  
  if(bound){
    tmp <- as.data.frame(m$up)
    names(tmp) <- names(as.data.frame(m$idx))
    tmp$Year <- rownames(m$idx)
    tmp <- tmp %>% gather(age, up, -Year)
    idx <- merge(idx, tmp)
    tmp <- as.data.frame(m$lo)
    names(tmp) <- names(as.data.frame(m$idx))
    tmp$Year <- rownames(m$idx)
    tmp <- tmp %>% gather(age, lo, -Year)
    idx <- merge(idx, tmp)
    idx$CV <- (log(idx$up)-log(idx$lo))/4
  }
  return(idx)
}

# extract index of retrospective from surveyIdx object
gather.retro <- function(retro, bound = F){
  retro <- lapply(retro, function(x) {x <- gather.idx(x, bound)
  x$retro <- max(x$Year)
  return(x)})
  retro <- bind_rows(retro)
}

# extract index of sensitivity analysis to lack of data from surveyIdx object
gather.sensi <- function(sensi, bound = F){
  sensi <- mapply(function(x, year_wouk) {
    x <- gather.idx(x, bound = bound)
    return(x)
  }, sensi)
  sensi <- lapply(as.list(1:dim(sensi)[2]), function(x){
    tmp <- as.data.frame(sensi[,x])
    tmp$year_wouk <- dimnames(sensi)[[2]][x]
    return(tmp)})
  sensi <- bind_rows(sensi)
}
