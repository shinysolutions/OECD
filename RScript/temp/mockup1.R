library(jpeg)
worldMap <- readJPEG("Data/worldMap.jpg")


library(XML)
paths <- xmlRoot(xmlTreeParse("Data/hexagonMap.svg"))
shapes <- sapply(xmlApply(paths[[1]], xmlAttrs),"[[", 2)
CNT <- lapply(shapes, FUN = function(x) {
  tmp <- gsub("[[:alpha:]]", "", x)
  tmp <- strsplit(gsub("^ *| *$", "", tmp), " +")[[1]]
  tmp <- as.data.frame(t(matrix(as.numeric(tmp), nrow  = 2)))
  names(tmp) <- c("x", "y")
  row.names(tmp) <- NULL
  tmp
})

cnt <- CNT[[length(CNT)]]
xlim <- range(cnt$x)
ylim <- range(cnt$y)

colGray <- rgb(127,127,127, max = 255)
colGreen <- rgb(169,209,142, max = 255)
colOrange <- rgb(244,177,131, max = 255)

CNT <- unique(CNT)
CNT[[length(CNT)]] <- data.frame(x = CNT[[9]]$x, y = CNT[[6]]$y)

cnt <- c("SK", "GB", "IE", "EE", "FI", "SE", "DE", "LU", "NL", "DK", "BE", "FR", "ES", "PO",
         "CZ", "CH", "AT", "IS", "HU", "SI", "IT", "GR", "TR", "AU", "CA", "CL", "IL", "JP",
         "KR", "MX", "NZ", "US", "LV", "PT", "NO")
names(CNT) <- cnt



par(xaxs= "i", bty = "n", xaxt = "n", yaxt = "n", font = 1, family = "sans", mar = rep(1, 4))
plot(1:2, xlim = xlim, ylim = ylim, type='n', xlab = "", ylab = "")
rasterImage(worldMap, xlim[1], ylim[1], xlim[2], ylim[2])


for(i in 1:length(CNT)){
  cnt <- CNT[[i]]
  polygon(as.numeric(cnt$x), as.numeric(cnt$y), col = colGreen, border = NA)
  text( mean(cnt$x), mean(cnt$y)-1, names(CNT)[i], col = "white", font = 2, family = "sans")
}

rm(list = c("shapes", "i", "paths"))

save.image(file = "Data/CNT.RData")
