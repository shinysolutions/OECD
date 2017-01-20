library( ReporteRs )
load("Data/CNT.RData")
load("Data/Q_ls.RData")


hexagon <- function(x, y, r = 1, ...){
  x = x+r*cos(seq(30, 360, by =  60)/180*pi)
  y = y+r*sin(seq(30, 360, by =  60)/180*pi)
  polygon(x, y, ...)
}


# # Creation of doc, a pptx object (default template)
# mydoc <- pptx(template = "RScript/temp/OECD.pptx")
# slide.layouts(mydoc)
# slide.layouts(mydoc, "Blank2")
# # check my layout names:
# # slide.layouts(mydoc)

colGray <- rgb(234,243,251, max = 255)
colGreen <- rgb(154,201,124, max = 255)
colOrange <- rgb(254,180,11, max = 255)


mydoc <- pptx(template = "RScript/temp/OECD.pptx")
for (q in 1:length(Q_ls)) {
  dat <- Q_ls[[q]]
  if (class(dat) == "data.frame") {
    print(paste(names(Q_ls)[q], paste(sort(unique(dat$Data)), collapse = ";"), sep = ": "))
    mydoc <- addSlide( mydoc, "Blank2" )
    # mydoc <- addTitle( mydoc, topics$Topics[which(topics$TopicsCode == names(Q_ls)[q])])
    mydoc <- addTitle( mydoc, gsub("^Q.*? ", "", topics$Topics[which(topics$TopicsCode == names(Q_ls)[q])]))
    mydoc <- addPageNumber( mydoc )
    mydoc <- addImage(mydoc, "Data/worldMap.jpg")
    mydoc <- addPlot(mydoc, function() {
      par(xaxs= "i", bty = "n", xaxt = "n", yaxt = "n", font = 1,  mar = rep(0, 4))
      plot(1:2, xlim = xlim, ylim = ylim, type='n', xlab = "", ylab = "")
      myColor <- c(colGray, terrain.colors(length(unique(dat$Data))))[1:length(unique(dat$Data))]
      names(myColor) <- sort(unique(dat$Data))
      for(i in 1:length(CNT)){
        cnt <- CNT[[i]]
        polygon(as.numeric(cnt$x), as.numeric(cnt$y), col = myColor[dat$Data[which(dat$CountryCode == names(CNT)[i])]], border = NA)
        
        text( mean(cnt$x), mean(cnt$y)-4, names(CNT)[i], col = "white", font = 2, cex =  1.5)
      }
      
      rd <- 8
      myColor <- myColor[rev(order(nchar(names(myColor))))]
      for (n in 1:length(myColor)) {
        hexagon(15, 470 + 3*(n-1)*rd, r = rd, col = myColor[n], border = NA)
        text( 15+2*rd, 470 + 3*(n-1)*rd, names(myColor)[n],  pos = 4)
      }
    })
  } else if (all(sapply(Q_ls[[q]], class) == "data.frame")) {
    mydoc <- addSlide( mydoc, "Blank2" )
    mydoc <- addTitle( mydoc, gsub("^Q.*? ", "", topics$Topics[which(topics$TopicsCode == names(Q_ls)[q])]))
    mydoc <- addPageNumber( mydoc )
    mydoc <- addPlot(mydoc, function() {
      tmp <- data.frame(CountryCode = dat[[1]]$CountryCode)
      for (m in 1:length(dat)) {
        tmp <- merge(tmp, dat[[m]][, c("CountryCode", "Data")])
        names(tmp)[length(tmp)] <- names(dat)[m]
      }
      
      par(xaxs= "i", bty = "n",  yaxt = "n", font = 1,  mar = c(2,1,1,1))
      barplot(rep(ncol(tmp)-.5, nrow(tmp)), space = 0, border= NA, col = c(colGray, "gray"))
      for (m in 2:ncol(tmp)) {
        # text(0, m-1, topics$Topics[which(topics$TopicsCode == names(tmp)[m])], pos = 4)
        val <- as.numeric(tmp[,m])
        val[is.na(val)] <- 2
        points(seq(1, nrow(tmp)) -0.5, rep(m-1.5, nrow(tmp)), pch = 20, cex = 3, col = c(colGreen, "white")[val])
        text(0, rep(m-1.5 + strheight("A")*1.5, nrow(tmp)), topics$Topics[which(topics$TopicsCode == names(tmp)[m])], pos = 4)
      }
      for (n in 1:nrow(tmp)) {
        axis(1, at = n -0.5, labels = tmp$CountryCode[n], tick = FALSE, line = -1)
      }
    })
  } else if (all(sapply(Q_ls[[q]], class) == "list")) {
    for (n in 1:length(dat)) {
      dat2 <- dat[[n]]
      mydoc <- addSlide( mydoc, "Blank2" )
      mydoc <- addTitle( mydoc, paste(gsub("^Q.*? ", "", topics$Topics[which(topics$TopicsCode == names(Q_ls)[q])]), topics$Topics[which(topics$TopicsCode == names(dat)[n])]))
      mydoc <- addPageNumber( mydoc )
      mydoc <- addPlot(mydoc, function() {
        tmp <- data.frame(CountryCode = dat2[[1]]$CountryCode)
        for (m in 1:length(dat2)) {
          tmp <- merge(tmp, dat2[[m]][, c("CountryCode", "Data")])
          names(tmp)[length(tmp)] <- names(dat2)[m]
        }
        
        par(xaxs= "i", bty = "n",  yaxt = "n", font = 1,  mar = c(2,1,1,1))
        barplot(rep(ncol(tmp)-.5, nrow(tmp)), space = 0, border= NA, col = c(colGray, "gray"))
        for (m in 2:ncol(tmp)) {
          # text(0, m-1, topics$Topics[which(topics$TopicsCode == names(tmp)[m])], pos = 4)
          val <- as.numeric(tmp[,m])
          val[is.na(val)] <- 2
          points(seq(1, nrow(tmp)) -0.5, rep(m-1.5, nrow(tmp)), pch = 20, cex = 3, col = c(colGreen, "white")[val])
          text(0, rep(m-1.5 + strheight("A")*1.5, nrow(tmp)), topics$Topics[which(topics$TopicsCode == names(tmp)[m])], pos = 4)
        }
        for (n in 1:nrow(tmp)) {
          axis(1, at = n -0.5, labels = tmp$CountryCode[n], tick = FALSE, line = -1)
        }
      })
      
    }
  } else {
    for (n in 1:length(dat)) {
      dat2 <- dat[[n]]
      if (class(dat2) == "data.frame") {
        mydoc <- addSlide( mydoc, "Blank2" )
        mydoc <- addTitle( mydoc, gsub("^Q.*? ", "", topics$Topics[which(topics$TopicsCode == names(Q_ls)[q])]))
        mydoc <- addPageNumber( mydoc )
        mydoc <- addImage(mydoc, "Data/worldMap.jpg")
        mydoc <- addPlot(mydoc, function() {
          par(xaxs= "i", bty = "n", xaxt = "n", yaxt = "n", font = 1,  mar = rep(0, 4))
          plot(1:2, xlim = xlim, ylim = ylim, type='n', xlab = "", ylab = "")
          myColor <- c(colGray, terrain.colors(length(unique(dat2$Data))))[1:length(unique(dat2$Data))]
          names(myColor) <- sort(unique(dat2$Data))
          for(i in 1:length(CNT)){
            cnt <- CNT[[i]]
            polygon(as.numeric(cnt$x), as.numeric(cnt$y), col = myColor[dat2$Data[which(dat2$CountryCode == names(CNT)[i])]], border = NA)
            
            text( mean(cnt$x), mean(cnt$y)-4, names(CNT)[i], col = "white", font = 2, cex = 1.5)
          }
          
          rd <- 8
          myColor <- myColor[rev(order(nchar(names(myColor))))]
          for (n in 1:length(myColor)) {
            hexagon(15, 470 + 3*(n-1)*rd, r = rd, col = myColor[n], border = NA)
            text( 15+2*rd, 470 + 3*(n-1)*rd, names(myColor)[n],  pos = 4)
          }
        })
      } else {
        mydoc <- addSlide( mydoc, "Blank2" )
        mydoc <- addTitle( mydoc, paste(gsub("^Q.*? ", "", topics$Topics[which(topics$TopicsCode == names(Q_ls)[q])]), topics$Topics[which(topics$TopicsCode == names(dat)[n])]))
        mydoc <- addPageNumber( mydoc )
        mydoc <- addPlot(mydoc, function() {
          tmp <- data.frame(CountryCode = dat2[[1]]$CountryCode)
          for (m in 1:length(dat2)) {
            tmp <- merge(tmp, dat2[[m]][, c("CountryCode", "Data")])
            names(tmp)[length(tmp)] <- names(dat2)[m]
          }
          
          par(xaxs= "i", bty = "n",  yaxt = "n", font = 1,  mar = c(2,1,1,1))
          barplot(rep(ncol(tmp)-.5, nrow(tmp)), space = 0, border= NA, col = c(colGray, "gray"))
          for (m in 2:ncol(tmp)) {
            # text(0, m-1, topics$Topics[which(topics$TopicsCode == names(tmp)[m])], pos = 4)
            val <- as.numeric(tmp[,m])
            val[is.na(val)] <- 2
            points(seq(1, nrow(tmp)) -0.5, rep(m-1.5, nrow(tmp)), pch = 20, cex = 3, col = c(colGreen, "white")[val])
            text(0, rep(m-1.5 + strheight("A")*1.5, nrow(tmp)), topics$Topics[which(topics$TopicsCode == names(tmp)[m])], pos = 4)
          }
          for (n in 1:nrow(tmp)) {
            axis(1, at = n -0.5, labels = tmp$CountryCode[n], tick = FALSE, line = -1)
          }
          
        })
      }
      
    }
  }
}
writeDoc( mydoc, "Output/demo.pptx" )





