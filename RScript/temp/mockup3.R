counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS",space = 1,
        xlab="Number of Gears", col=c("darkblue","red"), horiz=TRUE,
        legend = rownames(counts))


