mydata = read.csv('/Users/KavishLeckraz 1/Downloads/seeds_dataset.csv', sep=",")
mydata = mydata[c(-209),]
#names(mydatar)[names(mydatar) == 'X1'] <- 'seeds'
mydata = na.omit(mydata) 
mydata = scale(mydata)

mydatar = read.csv('/Users/KavishLeckraz 1/Downloads/seeds_real.csv', sep=",")
mydatar = na.omit(mydatar) 
mydatar = scale(mydatar)

fit <- kmeans(mydata, 2) # 2 cluster solution
fitr <- kmeans(mydatar, 3) 
Kgroups = fit$cluster
Kgroupsr = fitr$cluster
source('/Users/KavishLeckraz 1/Downloads/WK_R.r')
wkk2 = WK_R(Kgroups, Kgroupsr)
wkk2
plot(mydata, col=Kgroups, main='K-means Output for Seeds Data with k=2')


fit <- kmeans(mydata, 3) # 3 cluster solution
Kgroups = fit$cluster
wkk3 = WK_R(Kgroups, Kgroupsr)
wkk3
plot(mydata, col=Kgroups, main='K-means Output for Seeds Data with k=3')

fit <- kmeans(mydata, 4) # 4 cluster solution
Kgroups = fit$cluster
wkk4 = WK_R(Kgroups, Kgroupsr)
wkk4
plot(mydata, col=Kgroups, main='K-means Output for Seeds Data with k=4')

fit <- kmeans(mydata, 5) # 5 cluster solution
Kgroups = fit$cluster
wkk5 = WK_R(Kgroups, Kgroupsr)
wkk5
plot(mydata, col=Kgroups, main='K-means Output for Seeds Data with k=5')

plot(mydata, col=Kgroupsr, main='True Clusters for Seeds Data') # True cluster plot

# Hierarchical AVERAGE

d <- dist(mydata, method = "euclidean") # distance matrix
dr <- dist(mydatar, method = "euclidean") # distance matrix
fit <- hclust(d, method="average")
fitr <- hclust(dr, method="average")

Hgroups <- cutree(fit, k=2) # cut tree into 2 clusters
Hgroupsr <- cutree(fitr, k=3) # True Clusters
wkha2 = WK_R(Hgroups, Hgroupsr)
wkha2
plot(mydata, col=Hgroups, main='Hierarchical (Average) Output for Seeds Data with k=2')
plot(fit,main="Hierarchical (Average)
Cluster Dendogram for k=2")
rect.hclust(fit, k=2, border="red")

Hgroups <- cutree(fit, k=3) # cut tree into 3 clusters
wkha3 = WK_R(Hgroups, Hgroupsr)
wkha3
plot(mydata, col=Hgroups, main='Hierarchical (Average) Output for Seeds Data with k=3')
plot(fit,main="Hierarchical (Average)
Cluster Dendogram for k=3")
rect.hclust(fit, k=3, border="red")

Hgroups <- cutree(fit, k=4) # cut tree into 4 clusters
wkha4 = WK_R(Hgroups, Hgroupsr)
wkha4
plot(mydata, col=Hgroups, main='Hierarchical (Average) Output for Seeds Data with k=4')
plot(fit,main="Hierarchical (Average)
Cluster Dendogram for k=4")
rect.hclust(fit, k=4, border="red")

Hgroups <- cutree(fit, k=5) # cut tree into 5 clusters
wkha5 = WK_R(Hgroups, Hgroupsr)
wkha5
plot(mydata, col=Hgroups, main='Hierarchical (Average) Output for Seeds Data with k=5')
plot(fit,main="Hierarchical (Average)
Cluster Dendogram for k=5")
rect.hclust(fit, k=5, border="red")

plot(mydata, col=Hgroupsr, main='Hierarchical (Average) True Clustering for Seeds Data') #True Plot

# Hierarchical COMPLETE

fit <- hclust(d, method="complete")
fitr <- hclust(dr, method="complete")

Hgroups <- cutree(fit, k=2) # cut tree into 2 clusters
Hgroupsr <- cutree(fitr, k=3) # True Clusters
wkhc2 = WK_R(Hgroups, Hgroupsr)
wkhc2
plot(mydata, col=Hgroups, main='Hierarchical (Complete) Output for Seeds Data with k=2')
plot(fit,main="Hierarchical (Complete)
Cluster Dendogram for k=2")
rect.hclust(fit, k=2, border="red")

Hgroups <- cutree(fit, k=3) # cut tree into 3 clusters
wkhc3 = WK_R(Hgroups, Hgroupsr)
wkhc3
plot(mydata, col=Hgroups, main='Hierarchical (Complete) Output for Seeds Data with k=3')
plot(fit,main="Hierarchical (Complete)
Cluster Dendogram for k=3")
rect.hclust(fit, k=3, border="red")

Hgroups <- cutree(fit, k=4) # cut tree into 4 clusters
wkhc4 = WK_R(Hgroups, Hgroupsr)
wkhc4
plot(mydata, col=Hgroups, main='Hierarchical (Complete) Output for Seeds Data with k=4')
plot(fit,main="Hierarchical (Complete)
Cluster Dendogram for k=4")
rect.hclust(fit, k=4, border="red")

Hgroups <- cutree(fit, k=5) # cut tree into 5 clusters
wkhc5 = WK_R(Hgroups, Hgroupsr)
wkhc5
plot(mydata, col=Hgroups, main='Hierarchical (Complete) Output for Seeds Data with k=5')
plot(fit,main="Hierarchical (Complete)
Cluster Dendogram for k=5")
rect.hclust(fit, k=5, border="red")

# Hierarchical SINGLE

fit <- hclust(d, method="single")
fitr <- hclust(dr, method="single")

Hgroups <- cutree(fit, k=2) # cut tree into 2 clusters
Hgroupsr <- cutree(fitr, k=3) # True Clusters
wkhs2 = WK_R(Hgroups, Hgroupsr)
wkhs2
plot(mydata, col=Hgroups, main='Hierarchical (Single) Output for Seeds Data with k=2')
plot(fit,main="Hierarchical (Single)
Cluster Dendogram for k=2")
rect.hclust(fit, k=2, border="red")

Hgroups <- cutree(fit, k=3) # cut tree into 3 clusters
wkhs3 = WK_R(Hgroups, Hgroupsr)
wkhs3
plot(mydata, col=Hgroups, main='Hierarchical (Single) Output for Seeds Data with k=3')
plot(fit,main="Hierarchical (Single)
Cluster Dendogram for k=3")
rect.hclust(fit, k=3, border="red")

Hgroups <- cutree(fit, k=4) # cut tree into 4 clusters
wkhs4 = WK_R(Hgroups, Hgroupsr)
wkhs4
plot(mydata, col=Hgroups, main='Hierarchical (Single) Output for Seeds Data with k=4')
plot(fit,main="Hierarchical (Single)
Cluster Dendogram for k=4")
rect.hclust(fit, k=4, border="red")

Hgroups <- cutree(fit, k=5) # cut tree into 5 clusters
wkhs5 = WK_R(Hgroups, Hgroupsr)
wkhs5
plot(mydata, col=Hgroups, main='Hierarchical (Single) Output for Seeds Data with k=5')
plot(fit,main="Hierarchical (Single)
Cluster Dendogram for k=5")
rect.hclust(fit, k=5, border="red")

kvalues=c(2,3,4,5)
WKK=c(wkk2,wkk3,wkk4,wkk5)
WKHA=c(wkha2,wkha3,wkha4,wkha5)
WKHC=c(wkhc2,wkhc3,wkhc4,wkhc5)
WKHS=c(wkhs2,wkhs3,wkhs4,wkhs5)

plot(kvalues, WKK, type='o', col='red', ylab = "Weighted Kappa (WK) Values", xlab = "Number of Clusters (K Values)",
     main='Weghted Kappa of K-means and Hierarchical (Average, Complete & Single) Clustering on Seed Data', ylim = c(0,1))
lines(kvalues, WKHA, type='o', col='blue')
lines(kvalues, WKHC, type='o', col='green')
lines(kvalues, WKHS, type='o', col='purple')
legend(4, 1, legend=c("K-means", "Hierarchical Average", "Hierarchical Complete","Hierarchical Single"),
       col=c("red","blue","green","purple"), lty = 1:1:1:1, cex=0.8)
