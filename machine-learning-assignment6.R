# Q1
# function to simulate data from a multivariate normal distribution
sim_mvd <- function(n = 1, mu, sigma) {
  ndim = length(mu)
  data = matrix(rnorm(n * ndim, 0, 1), nrow = n, ncol = ndim)
  L = chol(sigma)
  cor_data = data %*% L
  cor_data = t(apply(cor_data, 1, function(x) {
    x + mu
  }))
  return(cor_data)
}

# function to simulate data from 3 clusters
sim_clustdata <- function(n = 30,
                          delta = 3,
                          cor = 0.5,
                          p = 4,
                          seed = 65835) {
  cor[1:3] <- cor # make sure cor has 3 elements
  n[1:3] <- n # make sure n has 3 elements
  
  stopifnot(all(cor > -1 / (p - 1))) # enemy of a friend is an enemy principle
  
  mu_cluster1 <- c(-sqrt(2), .5, 0, 0) * delta
  mu_cluster2 <- c(+sqrt(2), .5, 0, 0) * delta
  mu_cluster3 <- c(0, -1, 0, 0) * delta
  
  Sigma_cluster1 <- diag(p) * (1 - cor[1]) + cor[1]
  Sigma_cluster2 <- diag(p) * (1 - cor[2]) + cor[2]
  Sigma_cluster3 <- diag(p) * (1 - cor[3]) + cor[3]
  
  set.seed(seed)
  Y1 <- sim_mvd(n[1], mu_cluster1, Sigma_cluster1)
  Y2 <- sim_mvd(n[2], mu_cluster2, Sigma_cluster2)
  Y3 <- sim_mvd(n[3], mu_cluster3, Sigma_cluster3)
  g <- rep(1:3, n)
  
  Y <- rbind(Y1, Y2, Y3)
  data.frame(Y, g = factor(g))
}

data <- sim_clustdata()

pairs(data, col = data$g)


# Q2
data <- sim_clustdata()

set.seed(65835)
kmeans_TotWSS <- function(k)
  kmeans(data, k, nstart = 20)$tot.withinss

set.seed(65835)
totwss <- sapply(1:40, kmeans_TotWSS)
plot(totwss, type = 'b', ylab = "Total WSS")


# Q3 ???????

v <- c(3, 2, 1.5, 0.5, 0.25, 0)

for (delta in v) {
  loop_data <- sim_clustdata(delta = delta, seed = 65835)
  set.seed(65835)
  kmeans_TotWSS <- function(k)
    kmeans(loop_data, k, nstart = 20)$tot.withinss
  
  set.seed(65835)
  totwss <- sapply(1:40, kmeans_TotWSS)
  totwss
  
  plot(
    totwss,
    type = 'b',
    ylab = "Total WSS",
    main = paste0("Elbow Plot delta = ", delta)
  ) # paste pastes two strings together
}
loop_data

# Q4
for (delta in v) {
  loop_data <- sim_clustdata(delta = delta, seed = 65835, n = 50)
  
  upd_data <- subset(loop_data, select = -c(g))
  upd_data
  
  hclust_complete <- hclust(dist(upd_data, method = "euclidean"), method = "complete")
  hclust_complete
  
  cut <- cutree(hclust_complete, 3)
  table(loop_data$g, cut)
  
  kmeans <- kmeans(upd_data, 3, nstart = 20)
  kmeans
  
}

# at delta = 3
loop_data <- sim_clustdata(delta = 3, seed = 65835, n = 50)

upd_data <- subset(loop_data, select = -c(g))

kmeans <- kmeans(upd_data, 3, nstart = 20)
table(kmeans$cluster, loop_data$g)

hclust_complete <- hclust(dist(upd_data, method = "euclidean"), method = "complete")
cut <- cutree(hclust_complete, 3)
table(loop_data$g, cut)


# Q5
eufood <- read.csv("https://openmv.net/file/food-consumption.csv")
eufood[which(is.na(eufood), arr = T)] <- c(47, 72, 0)
rownames(eufood) <- eufood$Country
eufood <- eufood[-(1:2)]
eufood

n <- nrow(eufood)
n

kmeans_TotWSS <- function(k)
  kmeans(eufood, k, nstart = 20)$tot.withinss

totwss <- sapply(1:15, kmeans_TotWSS)
totwss

plot(totwss, type = 'b', ylab = "Total WSS", )


# Q6
set.seed(822)
kmeans <- kmeans(eufood, 4, nstart = 20)
kmeans


# Q7
# covariance-based pca, centered but not scaled
pca <- prcomp(eufood,
              scale = FALSE,
              center = TRUE,
              rank. = 2)
pca

pred <- predict(pca)
pred

biplot(
  pca,
  cex = .7,
  col = c(1, 0),
  expand = 1.5,
  xlim = c(-.5, .5),
  ylim = c(-.5, .5)
)
points(pred[,1], pred[, 2], col = kmeans$cluster)


# Q8
hclust <- hclust(dist(eufood, method = "euclidean"), method = "complete")
eufood

plot(hclust)

# Q9
cluster1 <- eufood[c("Italy", "Spain", "Portugal", "Austria"), ]
cluster2 <- eufood[c("Germany", "Belgium", "Luxembourg", "France", "Switzerland"), ]
cluster3 <- eufood[c("Finland", "Norway", "Denmark", "Sweden"), ]
cluster4 <- eufood[c("Ireland", "Holland", "England"), ]
cluster1
cluster2
cluster3
cluster4

cov1 <- cov.wt(cluster1)
cov2 <- cov.wt(cluster2)
cov3 <- cov.wt(cluster3)
cov4 <- cov.wt(cluster4)
cov1

mean_fish <- mean(cluster2$Frozen.fish)
mean_orange <- mean(cluster4$Oranges)
mean_orange

# within cluster sums of squares (WSS)
library(psych)
wss1 <- (cov1$n.obs - 1) * tr(cov1$cov)
wss1
wss2 <- (cov2$n.obs - 1) * tr(cov2$cov)
wss2
wss3 <- (cov3$n.obs - 1) * tr(cov3$cov)
wss3
wss4 <- (cov4$n.obs - 1) * tr(cov4$cov)
wss4

wss_tot <- wss1 + wss2 + wss3 + wss4
wss_tot

# total sum of squares:
# sum of all squared differences between the mean of a sample and the individual values in that sample.
orig_cov <- cov.wt(eufood)
tss <- (orig_cov$n.obs - 1) * tr(orig_cov$cov)
tss

# proportion of variance explained/unexplained by the clusters
wss_tot / tss
1 - (wss_tot / tss)

