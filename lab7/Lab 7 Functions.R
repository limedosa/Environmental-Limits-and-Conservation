
# Map function ----

map <- function(z, x = 1:dim(z)[1], y = 1:dim(z)[2], 
                mask = NULL, zlim, zlog = FALSE, color = cm.colors(200),
                mask.col="steelblue", zero.col = NULL, geo = NULL,
                outline = NULL, asp = 1, ...){
  
  
  mar <- par()$mar
  par(mar=c(0,0,0,0))
  
  if (missing(zlim)) zlim = range(z, na.rm=1)
  
  if (zlog) {
    z = log(z+1)
    zlim = log(zlim+1)
  }
  
  image(x, y, z, zlim=zlim, col=color, xlab="", ylab="", xaxt="n", yaxt="n", useRaster = TRUE, asp = asp, ...)
  
  #non-null zero color
  if (!is.null(zero.col) & min(z) <= 0){
    z[z>0] <- NA
    image(x,y,z, col=zero.col, add=TRUE, useRaster = TRUE)
  }
  
  if (!is.null(mask)){
    mask[which(mask==1)] <- NA
    image(x,y,mask, col=mask.col, add=TRUE, useRaster = TRUE)
  }
  
  if (!is.null(geo)){
    if (missing(x) | missing(y)){
      warning("Cannot plot geo object without x/y specified as lon/lat")
    } else {
      
      points(geo$Lon, geo$Lat, pch=19)
      text(geo$Lon, geo$Lat+0.5, labels=as.character(geo$Name))
    }
  }
  
  if(!is.null(outline)){
    if (missing(x) | missing(y)){
      warning("Cannot plot outline object without x/y specified as lon/lat")
    } else {
      lines(outline)
    }
  }
  
  box()
  
  par(mar = mar)
  
}


# Animate function ----

map.animate <- function(z, x = 1:dim(z)[1], y = 1:dim(z)[2],
                        span=1:dim(z)[3], dates = NULL, anim.name = "Animated Map.gif",
                        dir.name="GIF Images", name="frame",
                        mask = NULL, geo = NULL, color = cm.colors(200), zlog=FALSE,
                        height = 480, width = 480, zero.col = NULL,
                        outline = NULL, ...){
  
  library(gifski)
  
  if (!dir.exists(dir.name)){
    dir.create(dir.name)
  } else {
    
    files <- list.files(path = dir.name, pattern = ".png", full.names = TRUE)
    
    if (length(files) > 0 ){
      
      go <- readline(prompt = paste("Ok to delete", length(files), "existing images? (y/n): "))
      
      if (go == "y"){
        file.remove(files)
        # print(list.files(path = dir.name, pattern = ".png", full.names = TRUE))
      } else {
        stop("Animation stopped")
      }
      
    }
  }
  
  message("Generating images...")
  
  for (i in span){
    
    if (i < 10) {NAME = paste(name,'0000',i,'.png',sep='')}
    if (i < 100 && i >= 10) {NAME = paste(name,'000',i,'.png', sep='')}
    if (i < 1000 && i >= 100) {NAME = paste(name,'00', i,'.png', sep='')}
    if (i >= 1000) {NAME = paste(name,'0', i,'.png', sep='')}
    
    png(paste(dir.name,NAME,sep="/"), height = height, width = width)
    
    par(mar=c(0,0,0,0))
    
    # print(dim(x))
    # print(dim(y))
    # print(dim(z))
    
    map(x = x, y = y, z = z[,,i], mask = mask, zlim = range(z),
        zlog = zlog, col = color, geo = geo, zero.col = zero.col,
        outline = outline, ...)
    
    if (!is.null(dates)){
      mtext(dates[i], side = 1, line = -1.2, adj = 0.05)
    }
    
    dev.off()
    
  }
  
  par(mar=c(5.1,4.1,4.1,2.1))
  message(paste("Created ",i," images in folder \"", dir.name,"\"", sep=""))
  
  gifski(list.files(dir.name, full.names = TRUE),
         gif_file = anim.name, delay = 0.1)
  
  
  
}

# Get coordinates function ----

get.coord.index <- function(get.lon, get.lat, lon, lat){
  
  ilat = which(abs(get.lat-lat) == min(abs(get.lat-lat)))
  ilon = which(abs(get.lon-lon) == min(abs(get.lon-lon)))
  
  return(c(ilon, ilat))
  
}
