set.seed(1234)
x <- rnorm(12, mean = rep((1:3), each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = 'blue', pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

dataFrame <- data.frame(x = x, y = y)
kmeansObj <- kmeans(dataFrame, centers = 3)

dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
par(mfrow = c(1, 2))
image(t(dataMatrix)[, nrow(dataMatrix):1], axes = F)
image(t(dataMatrix)[, order(kmeansObj$cluster)], axes = F)
