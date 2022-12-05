rm(list = ls(all = TRUE))
graphics.off()
data <- read.csv(file.choose())
head(data)
print(data)
data = subset(data, select = -c(X., Name, Type.1, Type.2,Legendary) )
data
data.scaled <- scale(data,
                     center = TRUE,
                     scale = TRUE)
head(data.scaled)
e = eigen(cov(data))
print(e)
e.scaled = eigen(cov(data.scaled))
print(e.scaled)
data.pc = as.matrix(data.scaled) %*% e.scaled$vectors
head(data.pc)
require(stats)
pc <- prcomp(x = data,
             center = TRUE,
             scale. = TRUE)
head(pc$x)
summary(pc)
var_explained = pc$sdev^2 / sum(pc$sdev^2)
var_explained
library(devtools)
library(ggfortify)
autoplot(pc,loadings=TRUE,loadings.label = TRUE, loadings.label.size =
           3,size=0.5)
qplot(c(1:8), var_explained) +
  geom_line() +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0,1)
