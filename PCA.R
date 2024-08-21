install.packages("stats")
install.packages("dplyr")
install.packages("rlang")

library(tidyverse)
library(rlang)
library(stats)
library(dplyr)


data <- read.csv("morphometrics_mouseandhuman.csv")

#remove first column if needed

data <- data[,-1]


#normalize data:



pca <- prcomp(t(data), scale=TRUE) 

## plot pc1 and pc2
plot(pca$x[,1], pca$x[,2])

#plot(pca$x[,1]*-1, pca$x[,2])




##scree plot
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")




pca.data <- data.frame(Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])
pca.data

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("PCA Graph")

pca

install.packages("corrr")
library('corrr')
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("factoextra")

install.packages("FactoMineR")
library("FactoMineR")

install.packages("geomorph")
library(geomorph)

install.packages("ggrepel")
library(ggrepel)

install.packages("factoextra")
library(factoextra)

#normalizing the data
data_normalized <- scale(data)
head(data_normalized)

corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)



data.pca <- princomp(corr_matrix)
summary(data.pca)


corr_matrix


data.pca$loadings[, 1:3]
