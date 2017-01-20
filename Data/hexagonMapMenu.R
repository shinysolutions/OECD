load("Data/CNT.RData")

## hexagon
hexagon <- function(x, y, r = 1, ...){
  x = x+r*cos(seq(30, 360, by =  60)/180*pi)
  y = y+r*sin(seq(30, 360, by =  60)/180*pi)
  polygon(x, y, ...)
}

svg(filename="Data/hexagonMapMenu.svg", width=8, height=5, pointsize=12)

par(xaxs= "i", bty = "n", xaxt = "n", yaxt = "n", font = 1,  mar = rep(0, 4))
plot(1:2, xlim = xlim, ylim = ylim, type='n', xlab = "", ylab = "")

for(i in 1:length(CNT)) {
  cnt <- CNT[[i]]
  polygon(as.numeric(cnt$x), as.numeric(cnt$y), col = "#58A744", border = rgb(0,0,0,.7), lwd = 2)
  text( mean(cnt$x), mean(cnt$y)-4, names(CNT)[i], col = "white")
}

dev.off()


  
##################

XML <- readLines("Data/hexagonMapMenu.svg")

ID <- grep("<path style=\"fill-rule", XML)

for (i in 1:length(ID)) {
  id <- ID[i]
  cnt <- names(CNT)[i]
  link <- "<a href='../OECD/?cnt=member'>"
  link <- gsub("member", cnt, link)
  
  XML[id] <- paste(link, XML[id]) 
  XML[id+4] <- paste(XML[id+4], "</a>") 
}

writeLines(XML, "Data/hexagonMapMenu2.svg")






