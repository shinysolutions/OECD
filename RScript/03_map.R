library( ReporteRs )
load("Data/CNT.RData")
load("Data/Q_ls.RData")

## hexagon
hexagon <- function(x, y, r = 1, ...){
  x = x+r*cos(seq(30, 360, by =  60)/180*pi)
  y = y+r*sin(seq(30, 360, by =  60)/180*pi)
  polygon(x, y, ...)
}


## binary
col_binary <- colorRampPalette(c("#58A744", "#7A7879"))
## diverging
col_diverging <- colorRampPalette(c("#58A744", "#7A7879", "#B51C2A"))
## categoriesDiverging
col_categoriesDiverging <- colorRampPalette(c("#58A744", "#AED3A7", "#7A7879"))
## categoriesRank
col_categoriesRank <- colorRampPalette(c("#B51C2A", "#EFC4BE", "#D9EBF6", "#469CC9"))
## categoriesSequential
col_categoriesSequential <- colorRampPalette(c("#3F0605", "#B51C2A", "#EFC4BE"))





for (cntHigh in names(CNT)) {
  ## Import template
  mydoc <- pptx(template = "Data/OECD.pptx")
  
  for (q in 1:length(Q_ls)) {
    Dat <- Q_ls[[q]]
    figType <- Dat$meta["figureType"]
    parent  <- Dat$meta["parent"]
    # print(paste(q, figType, sep = ":"))
    if (!is.na(parent)) {
      TitleTxt <- paste(gsub("^ *Q.*? ", "", topics$Topics[which(topics$TopicsCode == parent)]), topics$Topics[which(topics$TopicsCode == names(Q_ls)[q])])
    } else {
      TitleTxt <- gsub("^ *Q.*? ", "", topics$Topics[which(topics$TopicsCode == names(Q_ls)[q])])
    }

    ## Map
    if (figType == "hexgonMap") {
      # print(paste(names(Q_ls)[q], paste(sort(unique(Dat$data)), collapse = ";"), sep = ": "))
      mydoc <- addSlide( mydoc, "Blank2" )
      mydoc <- addTitle( mydoc, TitleTxt)
      mydoc <- addPageNumber( mydoc )
      mydoc <- addImage(mydoc, "Data/worldMap.jpg")
      mydoc <- addPlot(mydoc, function() {
        par(xaxs= "i", bty = "n", xaxt = "n", yaxt = "n", font = 1,  mar = rep(0, 4))
        plot(1:2, xlim = xlim, ylim = ylim, type='n', xlab = "", ylab = "")
        ## Color theme;
        if (Dat$meta["legendType"] == "binary") {
          myColor <- col_binary
        } else if (Dat$meta["legendType"] == "diverging") {
          myColor <- col_diverging
        } else if (Dat$meta["legendType"] == "categoriesDiverging") {
          myColor <- col_categoriesDiverging
        } else if (Dat$meta["legendType"] == "categoriesRank") {
          myColor <- col_categoriesRank
        } else if (Dat$meta["legendType"] == "categoriesSequential") {
          myColor <- col_categoriesSequential
        }
        
        legends <- Dat$meta[grep("legendOrder", names(Dat$meta))]
        cols <- myColor(length(legends))
        names(cols) <- legends
        
        for(i in 1:length(CNT)){
          cnt <- CNT[[i]]
          lgd <- Dat$data[which(Dat$data$CountryCode == names(CNT)[i]), 2]
          if (lgd %in% legends) {
            bgCol <- cols[lgd]
          } else {
            bgCol <- "#E0E0E0"
          }
          if (names(CNT)[i] == cntHigh) {
            polygon(as.numeric(cnt$x), as.numeric(cnt$y), col = bgCol, border = rgb(0,0,0,.7), lwd = 2)
            text( mean(cnt$x), mean(cnt$y)-4, names(CNT)[i], col = "black", font = 2, cex =  1.75)
          } else {
            polygon(as.numeric(cnt$x), as.numeric(cnt$y), col = bgCol, border = NA)
            text( mean(cnt$x), mean(cnt$y)-4, names(CNT)[i], col = "white", font = 1, cex =  1.75)    
          }
        }
        rd <- 8
        cols <- rev(cols)
        for (n in 1:length(cols)) {
          hexagon(15, 470 + 2.5*(n-1)*rd, r = rd, col = cols[n], border = NA)
          text(15+2*rd, 470 + 2.5*(n-1)*rd-3, names(cols)[n],  pos = 4)
        }
        uniVals <- sort(unique(Dat$data[,2]))
        misVals <- uniVals[-which(uniVals %in% legends)]
        if (length(misVals) > 0) {
          if (any(misVals == " ")) {
            misVals[which(misVals == " ")] <- "data not available."
          }
          if (length(misVals) > 1) {
            misVals <- "data not available/others."
          }
          TXT <- paste("Gray hexagon means", misVals, sep = ": ")
          text(10, 440, TXT,  pos = 4, col = "grey")
        }
      })
    }
    
    if (figType == "histgramDataframe") {
      mydoc <- addSlide( mydoc, "Blank2" )
      mydoc <- addTitle( mydoc, TitleTxt)
      mydoc <- addPageNumber( mydoc )
      mydoc <- addPlot(mydoc, function() {
        
        tmp <- Dat$data
        for (m in 2:ncol(tmp)) {
          tmp[which(tmp[,m] == " "),m] <- 2
        }
        tmp <- tmp[do.call(order, tmp[,-1]),]
        tmp <- tmp[, c(1, rev(2:ncol(tmp)))]
        par(xaxs= "i", bty = "n",  yaxt = "n", font = 1,  mar = c(2,1,1,1))
        cols <- rep(c( "#E0E0E0", "white"), ceiling(nrow(tmp)/2))
        cols <- cols[1:nrow(tmp)]
        cols[which(tmp$CountryCode == cntHigh)] <- "gold"
        barplot(rep((ncol(tmp)-1)*(1+7/93), nrow(tmp)), space = 0, border= NA, col = cols)
        for (m in 2:ncol(tmp)) {
          val <- as.numeric(tmp[,m])
          val[is.na(val)] <- 2
          points(seq(1, nrow(tmp)) -0.5, rep(m-1.5, nrow(tmp)), pch = 20, cex = 3, col = c(colGreen, "white")[val])
          text(0, rep(m-1.5 + strheight("A")*1.5, nrow(tmp)), topics$Topics[which(topics$TopicsCode == names(tmp)[m])], pos = 4, col = "gray25")
        }
        for (n in 1:nrow(tmp)) {
          bld <- ifelse(tmp$CountryCode[n] == cntHigh, 2, 1)
          text(n -0.5, ncol(tmp)-1 + strheight("A")*3.5, labels = tmp$CountryCode[n],srt = 45, adj= 1, xpd = TRUE, pos = 1, font = bld)
        }
      })
    }
    
    if (figType == "histgramList") {
      for (l in 1:length(Dat$data)) {
        tmp <- Dat$data[[l]]
        mydoc <- addSlide( mydoc, "Blank2" )
        TitleTxt <- paste(gsub("^ *Q.*? ", "", topics$Topics[which(topics$TopicsCode == parent)]), topics$Topics[which(topics$TopicsCode == names(Dat$data)[l])])
        mydoc <- addTitle( mydoc, TitleTxt)
        mydoc <- addPageNumber( mydoc )
        mydoc <- addPlot(mydoc, function() {
          for (m in 2:ncol(tmp)) {
            tmp[which(tmp[,m] == " "),m] <- 2
          }
          tmp <- tmp[do.call(order, tmp[,-1]),]
          tmp <- tmp[, c(1, rev(2:ncol(tmp)))]
          par(xaxs= "i", bty = "n",  yaxt = "n", font = 1,  mar = c(2,1,1,1))
          cols <- rep(c( "#E0E0E0", "white"), ceiling(nrow(tmp)/2))
          cols <- cols[1:nrow(tmp)]
          cols[which(tmp$CountryCode == cntHigh)] <- "gold"
          barplot(rep((ncol(tmp)-1)*(1+7/93), nrow(tmp)), space = 0, border= NA, col = cols)
          for (m in 2:ncol(tmp)) {
            val <- as.numeric(tmp[,m])
            val[is.na(val)] <- 2
            points(seq(1, nrow(tmp)) -0.5, rep(m-1.5, nrow(tmp)), pch = 20, cex = 3, col = c(colGreen, "white")[val])
            text(0, rep(m-1.5 + strheight("A")*1.5, nrow(tmp)), topics$Topics[which(topics$TopicsCode == names(tmp)[m])], pos = 4, col = "gray25")
          }
          for (n in 1:nrow(tmp)) {
            bld <- ifelse(tmp$CountryCode[n] == cntHigh, 2, 1)
            text(n -0.5, ncol(tmp)-1 + strheight("A")*3.5, labels = tmp$CountryCode[n],srt = 45, adj= 1, xpd = TRUE, pos = 1, font = bld)
          }
        })
      }
    }
  }
  
  writeDoc( mydoc, gsub("cnt", cntHigh, "Output/OECD_cnt.pptx" ) )

}





# 
# 
# for (q in 1:length(Q_ls)) {
#   dat <- Q_ls[[q]]
#   if (class(dat) == "data.frame") {
#     print(paste(names(Q_ls)[q], paste(sort(unique(dat$Data)), collapse = ";"), sep = ": "))
#     mydoc <- addSlide( mydoc, "Blank2" )
#     # mydoc <- addTitle( mydoc, topics$Topics[which(topics$TopicsCode == names(Q_ls)[q])])
#     mydoc <- addTitle( mydoc, gsub("^Q.*? ", "", topics$Topics[which(topics$TopicsCode == names(Q_ls)[q])]))
#     mydoc <- addPageNumber( mydoc )
#     mydoc <- addImage(mydoc, "Data/worldMap.jpg")
#     mydoc <- addPlot(mydoc, function() {
#       par(xaxs= "i", bty = "n", xaxt = "n", yaxt = "n", font = 1,  mar = rep(0, 4))
#       plot(1:2, xlim = xlim, ylim = ylim, type='n', xlab = "", ylab = "")
#       myColor <- c(colGray, terrain.colors(length(unique(dat$Data))))[1:length(unique(dat$Data))]
#       names(myColor) <- sort(unique(dat$Data))
#       for(i in 1:length(CNT)){
#         cnt <- CNT[[i]]
#         polygon(as.numeric(cnt$x), as.numeric(cnt$y), col = myColor[dat$Data[which(dat$CountryCode == names(CNT)[i])]], border = NA)
#         
#         text( mean(cnt$x), mean(cnt$y)-4, names(CNT)[i], col = "white", font = 2, cex =  1.5)
#       }
#       
#       rd <- 8
#       myColor <- myColor[rev(order(nchar(names(myColor))))]
#       for (n in 1:length(myColor)) {
#         hexagon(15, 470 + 3*(n-1)*rd, r = rd, col = myColor[n], border = NA)
#         text( 15+2*rd, 470 + 3*(n-1)*rd, names(myColor)[n],  pos = 4)
#       }
#     })
#   } else if (all(sapply(Q_ls[[q]], class) == "data.frame")) {
#     mydoc <- addSlide( mydoc, "Blank2" )
#     mydoc <- addTitle( mydoc, gsub("^Q.*? ", "", topics$Topics[which(topics$TopicsCode == names(Q_ls)[q])]))
#     mydoc <- addPageNumber( mydoc )
#     mydoc <- addPlot(mydoc, function() {
#       tmp <- data.frame(CountryCode = dat[[1]]$CountryCode)
#       for (m in 1:length(dat)) {
#         tmp <- merge(tmp, dat[[m]][, c("CountryCode", "Data")])
#         names(tmp)[length(tmp)] <- names(dat)[m]
#       }
#       
#       par(xaxs= "i", bty = "n",  yaxt = "n", font = 1,  mar = c(2,1,1,1))
#       barplot(rep(ncol(tmp)-.5, nrow(tmp)), space = 0, border= NA, col = c(colGray, "gray"))
#       for (m in 2:ncol(tmp)) {
#         # text(0, m-1, topics$Topics[which(topics$TopicsCode == names(tmp)[m])], pos = 4)
#         val <- as.numeric(tmp[,m])
#         val[is.na(val)] <- 2
#         points(seq(1, nrow(tmp)) -0.5, rep(m-1.5, nrow(tmp)), pch = 20, cex = 3, col = c(colGreen, "white")[val])
#         text(0, rep(m-1.5 + strheight("A")*1.5, nrow(tmp)), topics$Topics[which(topics$TopicsCode == names(tmp)[m])], pos = 4)
#       }
#       for (n in 1:nrow(tmp)) {
#         axis(1, at = n -0.5, labels = tmp$CountryCode[n], tick = FALSE, line = -1)
#       }
#     })
#   } else if (all(sapply(Q_ls[[q]], class) == "list")) {
#     for (n in 1:length(dat)) {
#       dat2 <- dat[[n]]
#       mydoc <- addSlide( mydoc, "Blank2" )
#       mydoc <- addTitle( mydoc, paste(gsub("^Q.*? ", "", topics$Topics[which(topics$TopicsCode == names(Q_ls)[q])]), topics$Topics[which(topics$TopicsCode == names(dat)[n])]))
#       mydoc <- addPageNumber( mydoc )
#       mydoc <- addPlot(mydoc, function() {
#         tmp <- data.frame(CountryCode = dat2[[1]]$CountryCode)
#         for (m in 1:length(dat2)) {
#           tmp <- merge(tmp, dat2[[m]][, c("CountryCode", "Data")])
#           names(tmp)[length(tmp)] <- names(dat2)[m]
#         }
#         
#         par(xaxs= "i", bty = "n",  yaxt = "n", font = 1,  mar = c(2,1,1,1))
#         barplot(rep(ncol(tmp)-.5, nrow(tmp)), space = 0, border= NA, col = c(colGray, "gray"))
#         for (m in 2:ncol(tmp)) {
#           # text(0, m-1, topics$Topics[which(topics$TopicsCode == names(tmp)[m])], pos = 4)
#           val <- as.numeric(tmp[,m])
#           val[is.na(val)] <- 2
#           points(seq(1, nrow(tmp)) -0.5, rep(m-1.5, nrow(tmp)), pch = 20, cex = 3, col = c(colGreen, "white")[val])
#           text(0, rep(m-1.5 + strheight("A")*1.5, nrow(tmp)), topics$Topics[which(topics$TopicsCode == names(tmp)[m])], pos = 4)
#         }
#         for (n in 1:nrow(tmp)) {
#           axis(1, at = n -0.5, labels = tmp$CountryCode[n], tick = FALSE, line = -1)
#         }
#       })
#       
#     }
#   } else {
#     for (n in 1:length(dat)) {
#       dat2 <- dat[[n]]
#       if (class(dat2) == "data.frame") {
#         mydoc <- addSlide( mydoc, "Blank2" )
#         mydoc <- addTitle( mydoc, gsub("^Q.*? ", "", topics$Topics[which(topics$TopicsCode == names(Q_ls)[q])]))
#         mydoc <- addPageNumber( mydoc )
#         mydoc <- addImage(mydoc, "Data/worldMap.jpg")
#         mydoc <- addPlot(mydoc, function() {
#           par(xaxs= "i", bty = "n", xaxt = "n", yaxt = "n", font = 1,  mar = rep(0, 4))
#           plot(1:2, xlim = xlim, ylim = ylim, type='n', xlab = "", ylab = "")
#           myColor <- c(colGray, terrain.colors(length(unique(dat2$Data))))[1:length(unique(dat2$Data))]
#           names(myColor) <- sort(unique(dat2$Data))
#           for(i in 1:length(CNT)){
#             cnt <- CNT[[i]]
#             polygon(as.numeric(cnt$x), as.numeric(cnt$y), col = myColor[dat2$Data[which(dat2$CountryCode == names(CNT)[i])]], border = NA)
#             
#             text( mean(cnt$x), mean(cnt$y)-4, names(CNT)[i], col = "white", font = 2, cex = 1.5)
#           }
#           
#           rd <- 8
#           myColor <- myColor[rev(order(nchar(names(myColor))))]
#           for (n in 1:length(myColor)) {
#             hexagon(15, 470 + 3*(n-1)*rd, r = rd, col = myColor[n], border = NA)
#             text( 15+2*rd, 470 + 3*(n-1)*rd, names(myColor)[n],  pos = 4)
#           }
#         })
#       } else {
#         mydoc <- addSlide( mydoc, "Blank2" )
#         mydoc <- addTitle( mydoc, paste(gsub("^Q.*? ", "", topics$Topics[which(topics$TopicsCode == names(Q_ls)[q])]), topics$Topics[which(topics$TopicsCode == names(dat)[n])]))
#         mydoc <- addPageNumber( mydoc )
#         mydoc <- addPlot(mydoc, function() {
#           tmp <- data.frame(CountryCode = dat2[[1]]$CountryCode)
#           for (m in 1:length(dat2)) {
#             tmp <- merge(tmp, dat2[[m]][, c("CountryCode", "Data")])
#             names(tmp)[length(tmp)] <- names(dat2)[m]
#           }
#           
#           par(xaxs= "i", bty = "n",  yaxt = "n", font = 1,  mar = c(2,1,1,1))
#           barplot(rep(ncol(tmp)-.5, nrow(tmp)), space = 0, border= NA, col = c(colGray, "gray"))
#           for (m in 2:ncol(tmp)) {
#             # text(0, m-1, topics$Topics[which(topics$TopicsCode == names(tmp)[m])], pos = 4)
#             val <- as.numeric(tmp[,m])
#             val[is.na(val)] <- 2
#             points(seq(1, nrow(tmp)) -0.5, rep(m-1.5, nrow(tmp)), pch = 20, cex = 3, col = c(colGreen, "white")[val])
#             text(0, rep(m-1.5 + strheight("A")*1.5, nrow(tmp)), topics$Topics[which(topics$TopicsCode == names(tmp)[m])], pos = 4)
#           }
#           for (n in 1:nrow(tmp)) {
#             axis(1, at = n -0.5, labels = tmp$CountryCode[n], tick = FALSE, line = -1)
#           }
#           
#         })
#       }
#       
#     }
#   }
# }
# writeDoc( mydoc, "Output/demo.pptx" )
# 
# 
# 
# 
# 
