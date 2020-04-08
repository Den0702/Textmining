scriptsDir <- ".\\scripts"

sourceFile <- paste(
  scriptsDir,
  "\\",
  "script_2.R",
  sep = ""
)
source(sourceFile)
#skalowanie wielowymiarowe
d <- dist(dtmTfidfBoundsMatrix)
fit <- cmdscale(d,eig=TRUE, k=2)
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n")
text(x, y, labels = row.names(dtmTfidfBoundsMatrix), cex=.7)
