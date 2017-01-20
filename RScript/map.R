load("Data/CNT.RData")
load("Data/Q_ls.RData")


for (q in 1:length(Q_ls)) {
  dat <- Q_ls[[q]]
  
  if (class(dat) == "data.frame") {
    print(paste(names(Q_ls)[q], paste(sort(unique(dat$Data)), collapse = ";"), sep = ": "))
    
    par(xaxs= "i", bty = "n", xaxt = "n", yaxt = "n", font = 1, family = "sans", mar = rep(1, 4))
    plot(1:2, xlim = xlim, ylim = ylim, type='n', xlab = "", ylab = "")
    rasterImage(worldMap, xlim[1], ylim[1], xlim[2], ylim[2])
    
    myColor <- c("Yes" = colGreen, "No" = colOrange, " " = colGray)
    
    for(i in 1:length(CNT)){
      cnt <- CNT[[i]]
      polygon(as.numeric(cnt$x), as.numeric(cnt$y), col = myColor[dat$Data[which(dat$CountryCode == names(CNT)[i])]], border = NA)
      text( mean(cnt$x), mean(cnt$y)-1, names(CNT)[i], col = "white", font = 2, family = "sans")
    }
  }
  

}


