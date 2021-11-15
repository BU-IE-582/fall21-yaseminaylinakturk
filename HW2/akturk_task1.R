#IE582 hw2-yasemin aylin akturk
#task1
install.packages("ggfortify")
library(ggfortify)
install.packages("ggpubr")
library(ggpubr)


par(mfrow= c(3,3))
klasor =  "C:/Users/y.akturk/Documents/"
setwd(klasor)

dat=read.csv("IE582_Fall21_HW2_q1_data.csv",header=T)

summary(dat)
#same range, no need to standardize
plot(dat)


dat[1:3,1:3]

factor(dat[,3])
levels (factor(dat[,3]))

lev = as.numeric(factor(dat[,3]))

dat[1:3,1:3]


plot(dat[,1],dat[,2],col=lev,pch=lev,xlab=names(dat)[1],ylab=names(dat)[2])

legend("topleft",paste("Class",levels (factor(dat[,3]))),col=unique(lev), pch= unique(lev))

cor(dat[,-3])

#normality check

qqnorm(dat[,1])
qqline(dat[,1])

qqnorm(dat[,2])
qqline(dat[,2])


pca<-princomp(dat[,1:2], cor=T)
summary(pca, loadings=T)

##autoplot(pca, data = dat, colour = 'class',loadings = TRUE, loadings.colour = 'red',
 ##        loadings.label = TRUE, loadings.label.size = 4)

####


#plot(pca)

#screeplot(pca)

biplot(pca)

#barplot(pca$scores[,1])

pca$loadings[1,1]
pca$loadings[2,1]
par(mfrow= c(2,2))

plot(pca$scores[,1],main= "PCAscores", col = "red")

summary(pca$scores[,1])

plot(pca$scores[,1],main= "PCAscores", col = lev)
legend(160,-.5,paste("Class",levels (factor(dat[,3]))),col=unique(lev), pch= 16,cex= 0.6)

###
par(mfrow= c(2,2))

matEuc <- as.matrix(dist(dat[,1:2]))
dat[1:3,1:2]
matEuc[1:3,1:3]
fitEuc <- stats:::cmdscale(matEuc,eig=TRUE, k=1)
Euc <- fitEuc$points[,1]

plot(Euc,main="Euclidean MDS", col = "red", pch=15)

###
matMan <- as.matrix(dist(dat[,1:2],method = "manhattan"))
dat[1:3,1:2]
matMan [1:3,1:3]

fitMan <- cmdscale(matMan,eig=TRUE, k=1)
Man <- fitMan$points[,1]
plot(Man,col="blue",main="Manhattan MDS",pch=16) 

###
matMax <- as.matrix(dist(dat[,1:2],method = "maximum"))
dat[1:3,1:2]
matMax [1:3,1:3]

fitMax<- cmdscale(matMax,eig=TRUE, k=1)
Max <- fitMax$points[,1]
plot(Max,col="green",main="Max MDS",pch=17) 

###
#all together
plot(Euc,main="MDS", col = "red", pch =15)
points(Man,col="blue",main="Manhattan MDS", pch=16) 
points(Max,col="green",main="Max MDS", pch=17)
#par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
legend(160,0,legend=c("Euc.", "Manh.", "Max." ), pch= c(15,16,17), col=c("red","blue", "green"),bty ="o", horiz = F, cex=0.6, text.font = 18)


###PCA and MDS### for classes
par(mfrow= c(2,2))

plot(pca$scores[,1],pch =16, col=lev, main ="PCA") 
legend(160,-.5,paste("Class",levels (factor(dat[,3]))),col=unique(lev), pch= unique(lev),cex= 0.6)

plot(Euc, pch =17, col=lev,main = "Euc. dist")
legend(160,0,paste("Class",levels (factor(dat[,3]))),col=unique(lev), pch= unique(lev),cex= 0.6)

plot(Man, pch =18, col=lev, main = "Man. dist.")
legend(160,0,paste("Class",levels (factor(dat[,3]))),col=unique(lev), pch= unique(lev),cex= 0.6)

plot(Max, pch =19, col=lev, main = "Max. dist")
legend(160,0,paste("Class",levels (factor(dat[,3]))),col=unique(lev), pch= unique(lev),cex= 0.6)

#pca 2 comp
plot(pca$scores[,1],pca$scores[,2],pch =16, col=lev, main ="PCA") 

###PCA and MDS###

plot(pca$scores[,1], col="blue",pch =16) 
points(Euc, col = "green", pch =17)
points(Man, col = "red", pch =18)
points(Max, col = "black", pch =19)
legend(160,-0.8,legend=c("PCAscore", "Euc.MDS.", "Man. dist", "Max. dist" ), pch= c(16,17,18,19), col=c("blue", "green","red","black"),bty ="o", horiz = F, cex=0.6, text.font = 18)


###PCA and MDS### for classes


plot(pca$scores[,1],pch =16, col=lev) 
points(Euc, pch =17, col=lev)
points(Man, pch =18, col=lev)
points(Max, pch =19, col=lev)
legend(160,-0.8,legend=c("PCAscore", "Euc.MDS.", "Man. dist", "Max. dist" ), pch= c(16,17,18,19),bty ="o", horiz = F, cex=0.6, text.font = 18)




###add columns
#extended data
extdat <-data.frame(dat)

listX1sq = extdat[,1]^2 

extdat$x1sq <- listX1sq 

listX2sq = extdat[,2]^2 

extdat$x2sq <- listX2sq 

listx1X2 = extdat[,1]*extdat[,2]

extdat$x1x2<- listx1X2

extdat[1:3,]


summary(extdat)
#same range, no need to standardize

factor(extdat[,3])
levels (factor(extdat[,3]))

extlev = as.numeric(factor(extdat[,3]))

plot(extdat, col = extlev)

cor(extdat[,-3])

###apply PCA to extended data----

extpca<-princomp(extdat[,-3], cor=T, scores = T)
summary(extpca, loadings=T)

#plot(extpca)

biplot(extpca)
#autoplot(extpca ,pch =15)

extpca$loadings[,1]
extpca$loadings[,2]
extpca$loadings[,3]



#eigenvalues
extpca$loadings

eigen(cor(extdat[,-3]))$vectors

#eigenvectors
extpca$sd^2
eigen(cor(extdat[,-3]))$values

# 
# 
# extcomp1 <-  extpca$loadings[1,1] * myextdat[,1] +
#             extpca$loadings[2,1] * myextdat[,2]+
#            extpca$loadings[3,1] * myextdat[,3]+
#             extpca$loadings[4,1] * myextdat[,4]+
#              extpca$loadings[5,1] * myextdat[,5]
# 
# extcomp2 <-  extpca$loadings[1,2] * myextdat[,1] +
#         extpca$loadings[2,2] * myextdat[,2]+
#        extpca$loadings[3,2] * myextdat[,3]+
#         extpca$loadings[4,2] * myextdat[,4]+
#         extpca$loadings[5,2] * myextdat[,5]
# 
# 
# extcomp3 <-  extpca$loadings[1,3] * myextdat[,1] +
#             extpca$loadings[2,3] * myextdat[,2]+
#             extpca$loadings[3,3] * myextdat[,3]+
#             extpca$loadings[4,3] * myextdat[,4]+
#             extpca$loadings[5,3] * myextdat[,5]
# 




head(extpca$scores)

#########
library(rgl)

#3d plot


plot3d( 
  x=extpca$scores[,1], y=extpca$scores[,2], extpca$scores[,3], 
  col = lev, 
  type = 's', 
  radius = .1,
  xlab="pca1", ylab="pca2", zlab="pca3")


par(mfrow= c(2,2))

plot(extpca$scores[,1], ylab= "PCA 1", col = lev)
legend(2,-2,paste("Class",levels (factor(dat[,3]))),col=unique(lev), pch= c(1,1),cex= 0.6)


plot ( extpca$scores[,1], extpca$scores[,2], col = lev, xlab=paste("PCA", 1, sep=" "), ylab=paste("PCA", 2, sep=" "))
legend(1,1,paste("Class",levels (factor(dat[,3]))),col=unique(lev), pch= c(1,1),cex= 0.6)
