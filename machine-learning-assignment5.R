# Q1:
set.seed(4354); Y = MASS::mvrnorm(100, mu = c(0,0,0,0,0), Sigma = ability.cov$cov[-1,-1]); A = cov(Y)
E <- eigen(A)
max(E$values)
round(213.897854,2)


# Q2;
humor.zip <- tempfile()
download.file("http://openpsychometrics.org/_rawdata/HSQ.zip", humor.zip)
humor.files <- unzip(humor.zip, list=TRUE)
file.show(unzip(humor.zip, humor.files$Name[1]))
humor <- read.csv(unzip(humor.zip, humor.files$Name[2]), na.string = "-1")
unlink(humor.zip)
humor <- na.omit(humor)

# Without the stars:
psych::corPlot(humor[1:32])

# With the stars
psych::corPlot(humor[1:32], stars=TRUE)


# Q3: 
# testing if the matrix is an identity  matrix:
options(digits = 15)
psych::cortest.bartlett(cor(humor), n = nrow(humor))
round(34887.8689144435, 5)

# testing the sampling adequacy 
options(digits = 15)
KMO <- psych::KMO(cor(humor))
round(KMO$MSA,5)

# Q4:
data <- humor[,1:32]
pca <- princomp(data, cor=T)
summary(pca)

# Plot the variances of the PCA
screeplot(pca, npcs = 32, type = "line")


# Q5: 
pca <- prcomp(data, rank.=4, scale=TRUE) #scale=TRUE, I had forgotten this one. 
biplot(pca, cex = 0.5)


# Q6:
stats:::print.loadings(pca$rotation, cutoff=.2)


# Q8:
R = diag(12); R[upper.tri(R,TRUE)] = c(-1.51, -0.17, -0.88, -0.93, -0.21, -1.2, -0.5, -0.15, -0.38, -1.06, -0.09, 0.13, -0.13, 0.06, -0.98, 0.1, -0.1, -0.05, -0.26, 0.25, -1.4, 0.85, -0.01, 0.22, 0.18, 0.15, -0.38, -1.06, -0.37, 0.3, 0.16, 0.23, -0.34, 1.16, 0.29, -1.12, 0.6, 0.01, 0.19, -0.12, 0.23, -0.15, -0.28, 0.21, -0.88, -0.19, -0.28, -0.1, -0.48, 0.15, -0.76, -0.44, 0.51, -0.01, -1.25, -0.25, -0.13, -0.33, 0.09, -0.09, 0.26, 0.22, -0.34, 0.08, 0.07, -0.84, 0.7, 0.35, 0.65, 0.57, -0.01, 0.47, -0.06, -0.21, -0.04, 0.29, 0.19, 1.01); R
set.seed(66700)
df = data.frame(MASS::mvrnorm(100, rep(0, 12), crossprod(R)))

# Bartlett’s test of sphericity with null hypothesis A = I,(p < .05)
options(scipen = 99)
psych::cortest.bartlett(cor(df), n = nrow(df))
round(570.926199848523, 5)

# Measure of sampling adequacy:
kmo <- psych::KMO(cor(df))
round(kmo$MSA, 5)

pca <- princomp(df, cor=T)
summary(pca)

screeplot(pca, type = "line")


# Q9:
n = 300
mu_set1 = mu_set2 =  rep(0, 5) 
Sigma_set1 = diag(5)*.4 + .6
Sigma_set2 = diag(5)*.5 + .5
set.seed(9162)
Y1 = MASS::mvrnorm(n, mu_set1, Sigma_set1) 
Y2 = MASS::mvrnorm(n, mu_set2, Sigma_set2)
Y = cbind(Y1,Y2)
dat = data.frame(Y)

pca <- princomp(dat, cor=T)
summary(pca)
screeplot(pca, type = "line")

kmo <- psych::KMO(cor(dat))
round(kmo$MSA,5)

