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




shinyServer(function(input, output, session) {
  ## Initialize reactiveValues:p
  p <- reactiveValues()
  observe({
    p$cnt <-parseQueryString(session$clientData$url_search)$cnt
  })
  

  output$uiTopic <- renderUI({
    Items <- names(Q_ls)
    names(Items) <- merge(data.frame(TopicsCode= Items), topics, all.x = TRUE)$Topics
    selectInput(inputId   = "topic", 
                label     = "Pick variable:", 
                selectize  = FALSE,
                choices   = Items)
    
  })
  
  output$uiCNT <- renderUI({
    HTML(gsub("cnt", p$cnt,  "<div class='col-sm-12'><a href='home.html'><h2 id='home'>cnt</h2></a></div>"))
    
  })
  
  
  
  output$uiSubTopic <- renderUI({
    if (!is.null(input$topic)) {
      Dat <- Q_ls[[input$topic]]
      figType <- Dat$meta["figureType"]
      if(figType == "histgramList") {
        Items <- names(Q_ls[[input$topic]]$data)
        names(Items) <- merge(data.frame(TopicsCode= Items), topics, all.x = TRUE)$Topics
        selectInput(inputId   = "subtopic",
                    label     = "Sub topic:",
                    selectize  = FALSE,
                    choices   = Items)
      }
    }
  })
  
  
  output$uiTitle <- renderUI({
    if (!is.null(input$topic)) {
      Dat <- Q_ls[[input$topic]]
      figType <- Dat$meta["figureType"]
      parent  <- Dat$meta["parent"]
      print(figType)
      if (figType %in% c("hexgonMap", "histgramDataframe")) {
        if (!is.na(parent)) {
          TitleTxt <- paste(gsub("^ *Q.*? ", "", topics$Topics[which(topics$TopicsCode == parent)]),
                            topics$Topics[which(topics$TopicsCode == names(Q_ls)[input$topic])])
        } else {
          TitleTxt <- gsub("^ *Q.*? ", "", topics$Topics[which(topics$TopicsCode == input$topic)])
        }
      } else if (figType == "histgramList") {
        if (!is.null(input$subtopic)) {
          TitleTxt <- paste(gsub("^ *Q.*? ", "", topics$Topics[which(topics$TopicsCode == parent)]), 
                            topics$Topics[which(topics$TopicsCode == input$subtopic)])
        }
      }
      h3(TitleTxt)
      
    }
  })
  
  output$plot1 <- renderPlot({
    if (!is.null(input$topic)) {
      Dat <- Q_ls[[input$topic]]
      figType <- Dat$meta["figureType"]
      parent  <- Dat$meta["parent"]
      cntHigh <- p$cnt
      
      if (figType == "hexgonMap") {
          par(xaxs= "i", bty = "n", xaxt = "n", yaxt = "n", font = 1,  mar = rep(0, 4))
          plot(1:2, xlim = xlim, ylim = ylim, type='n', xlab = "", ylab = "")
          rasterImage(worldMap, xlim[1], ylim[1], xlim[2], ylim[2])
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
            hexagon(15, 480 + 2.5*(n-1)*rd, r = rd, col = cols[n], border = NA)
            text(15+2*rd, 480 + 2.5*(n-1)*rd-3, names(cols)[n],  pos = 4)
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
            text(10, 450, TXT,  pos = 4, col = "grey")
          }
      } else if (figType == "histgramDataframe") {
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
      } else if (figType == "histgramList") {
        if (!is.null(input$subtopic)) {
          tmp <- Dat$data[[input$subtopic]]
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
        }
      }
    }
  })
  


  output$downloadReport <- downloadHandler(
    filename = function() {"my-report.pptx"},

    content = function(file) {
      tempPPTX <- paste(tempfile(), "pptx", sep =".")
      mydoc <- pptx(template = "Data/OECD.pptx")
      
      q <- input$topic
      Dat <- Q_ls[[q]]
      figType <- Dat$meta["figureType"]
      parent  <- Dat$meta["parent"]
      # print(paste(q, figType, sep = ":"))
      if (!is.na(parent)) {
        TitleTxt <- paste(gsub("^ *Q.*? ", "", topics$Topics[which(topics$TopicsCode == parent)]), topics$Topics[which(topics$TopicsCode == q)])
      } else {
        TitleTxt <- gsub("^ *Q.*? ", "", topics$Topics[which(topics$TopicsCode == q)])
      }
      
      ## Map
      if (figType == "hexgonMap") {
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
        if (!is.null(input$subtopic)) {
          tmp <- Dat$data[[input$subtopic]]
          mydoc <- addSlide( mydoc, "Blank2" )
          TitleTxt <- paste(gsub("^ *Q.*? ", "", topics$Topics[which(topics$TopicsCode == parent)]), topics$Topics[which(topics$TopicsCode == input$subtopic)])
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
      
      
      writeDoc( mydoc, tempPPTX)
      file.copy(tempPPTX, file)
    }
  )

  
  
  
  
  output$downloadAll <- downloadHandler(
    filename = function() {gsub("cnt", p$cnt, "OECD_cnt.pptx")},
    content = function(file) {
      file.copy(gsub("cnt", p$cnt, "Output/OECD_cnt.pptx"), file)
    }
  )
  
  
})
