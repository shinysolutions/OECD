
barplot(rep(1, length(cnt)), space = 0, border= NA, col = c("grey", "green"))
text(.5, 0.8, "dfasdfs", pos = 4)
points(.5, 0.7, pch = 20, cex = 3, col = "red")

box()
axis(1)


x <- 1:10
y <- sort(10*runif(10))
z <- runif(10)
z3 <- cbind(z, 2*runif(10), runif(10))

symbols(x, y, circles = rep(.2, 10), inches = FALSE, bg = "red", fg = "red")



colGray <- rgb(234,243,251, max = 255)
colGreen <- rgb(154,201,124, max = 255)
colOrange <- rgb(254,180,11, max = 255)



dat <- Q_ls[["2_1_0"]]
tmp <- data.frame(CountryCode = dat[[1]]$CountryCode)
for (m in 1:length(dat)) {
  tmp <- merge(tmp, dat[[m]][, c("CountryCode", "Data")])
  names(tmp)[length(tmp)] <- names(dat)[m]
}

par(xaxs= "i", bty = "n", xaxt = "n", yaxt = "n", font = 1,  mar = c(4,1,1,1))
barplot(rep(ncol(tmp)-.5, nrow(tmp)), space = 0, border= NA, col = c(colGray, "white"))
for (m in 2:ncol(tmp)) {
  text(0, m-1, topics$Topics[which(topics$TopicsCode == names(tmp)[m])], pos = 4)
  val <- as.numeric(tmp[,m])
  val[is.na(val)] <- 2
  points(seq(1, nrow(tmp)) -0.5, rep(m-1.5, nrow(tmp)), pch = 20, cex = 3, col = c(colGreen, "white")[val])
}

axis(1, at = seq(1, nrow(tmp)) -0.5, labels = tmp$CountryCode)

