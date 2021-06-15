#write.csv(data1, "~/Documents/classification_problem/data1.csv")

# install.packages("mvtnorm")
library(MASS)
library(class)
library(mvtnorm)

require(mvtnorm)
require(MASS)
require(class)

wdbc <- read.csv("~/Documents/classification_problem/wdbc.data", header=FALSE)

wdbc <- read.csv("~/Documents/classification_problem/wdbc.data", header=FALSE)
data1 <- wdbc[, c('V4' ,'V9', 'V2')]
data1[data1 == "M"] <- 1
data1[data1 == "B"] <- 0
head(data1)

#split into training and test
sample_size = floor(0.8*nrow(data1))
set.seed(777)

picked = sample(seq_len(nrow(data1)),size = sample_size)
data1.tr <- data1[picked,]
data1.te <- data1[-picked,]


########################################################################


#basic-plots


plot(data1.tr[,1:2])
points(data1.tr[data1.tr$V2==1,1:2],col="red")


m <- 120
x <- seq(-1.25,0.87,length=m)
y <- seq(-0.2,1.1,length=m)
gr <- expand.grid(x,y)
colnames(gr) <- c("V4","V9")

tru.dens <- function(x){
  mu11 <- c(-0.7,0.3)
  mu12 <- c(0.3,0.3)
  mu21 <- c(-0.3,0.7)
  mu22 <- c(0.4,0.7)
  sd <- (0.03)
  f1 <- 0.5*(dmvnorm(x,mu11,sd*diag(1,2))+ dmvnorm(x,mu12,sd*diag(1,2)))
  f2 <- 0.5*(dmvnorm(x,mu21,sd*diag(1,2))+ dmvnorm(x,mu22,sd*diag(1,2)))
  f2/(f2+f1)
}

my.gr.pred <- apply(gr,1,tru.dens)
my.gr.pred.mat <- matrix(my.gr.pred,ncol=m)
contour(x,y,my.gr.pred.mat,levels=0.5,add=T,col="red",lty=1,lwd=2,drawlabels=F)

k <- 3

my.knn <-  knn(data1.tr[,1:2],data1.te[,1:2],data1.tr[,3],k=k)
knn.cl <- as.numeric(my.knn)-1

knn.prob <- (attributes(knn(data1.tr[,1:2],data1.te[,1:2],data1.tr[,3],k=k,prob=T)))$prob
my.test.pred <- knn.cl * knn.prob + (1-knn.cl)*(1-knn.prob)
err.rate <- sum((my.test.pred >= 0.5) != data1.te$V2)/nrow(data1.te)


err.rate

my.gr.knn <- knn(data1.tr[,1:2],gr,data1.tr[,3],k=k)
my.gr.knn.cl <- as.numeric(my.gr.knn)-1
my.gr.knn.prob <- (attributes(knn(data1.tr[,1:2],gr,data1.tr[,3],k=k,prob=T)))$prob
my.gr.pred <- my.gr.knn.cl * my.gr.knn.prob + (1-my.gr.knn.cl)*(1-my.gr.knn.prob)
my.gr.pred.mat <- matrix(my.gr.pred,ncol=m)

contour(x,y,my.gr.pred.mat,levels=0.5,add=T,col="orange",lty=2,lwd=2,drawlabels=F)


##################################################################

#LDA


rip.lda <- lda(V2~.,data=data1.tr)

rip.lda.pred <- predict(rip.lda,data1.te)$posterior[,2]

table(data1.te$V2,rip.lda.pred > 0.5)
rip.lda.test.err <- sum(data1.te$V2 != (rip.lda.pred > 0.5))/nrow(data1.te)

rip.qda <- qda(V2~.,data=data1.tr)
rip.qda.pred <- predict(rip.qda,data1.te)$posterior[,2]

table(data1.te$V2,rip.qda.pred > 0.5)
rip.qda.test.err <- sum(data1.te$V2 != (rip.qda.pred > 0.5))/nrow(data1.te)

rip.qda.train.pred <- predict(rip.qda,data1.tr)$posterior[,2]
rip.lda.train.pred <- predict(rip.lda,data1.tr)$posterior[,2]

table(data1.tr$V2,rip.lda.train.pred > 0.5)
rip.lda.train.err <- sum(data1.tr$V2!=(rip.lda.train.pred > 0.5))/nrow(data1.tr)

table(data1.tr$V2,rip.qda.train.pred > 0.5)
rip.qda.train.err <- sum(data1.tr$V2!=(rip.qda.train.pred > 0.5))/nrow(data1.tr)


m <- 120
x <- seq(-1.25,0.87,length=m)
y <- seq(-0.2,1.1,length=m)
gr <- expand.grid(x,y)
colnames(gr) <- c("V4","V9")

my.mod <- rip.lda
my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)

plot(data1.tr[,1:2])
points(data1.tr[data1.tr$V2==1,1:2],col="red")

# plot(data1.tr[,1:2])
# points(data1.tr[tr.cl,1:2],col="red")

contour(x,y,my.gr.pred,levels=0.5,add=T,col="blue")

my.mod <- rip.qda
my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)
contour(x,y,my.gr.pred,levels=0.5,add=T,col="green")

rip.lda.poly <- lda(V2~poly(V4,3)+poly(V9,3),data=data1.tr)
rip.qda.poly <- qda(V2~poly(V4,3)+poly(V9,3),data=data1.tr)

plot(data1.tr[,1:2])
points(data1.tr[data1.tr$V2==1,1:2],col="red")

# plot(data1.tr[,1:2])
# points(data1.tr[tr.cl,1:2],col="red")

my.mod <- rip.lda.poly
my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)
contour(x,y,my.gr.pred,levels=0.5,add=T,col="blue")

my.mod <- rip.qda.poly
my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)
contour(x,y,my.gr.pred,levels=0.5,add=T,col="green")

idx <- c(sample(1:125,10,replace=F),sample(126:250,10,replace=F)  )
rip.lda.poly.subsamp <- lda(V2~poly(V4,3)+poly(V9,3),data=data1.tr,subset=idx)
rip.qda.poly.subsamp <- qda(V2~poly(V4,3)+poly(V9,3),data=data1.tr,subset=idx)

plot(data1.tr[,1:2])
points(data1.tr[data1.tr$V2==1,1:2],col="red")
# plot(data1.tr[,1:2])
# points(data1.tr[tr.cl,1:2],col="red")

my.mod <- rip.lda.poly.subsamp
my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)
contour(x,y,my.gr.pred,levels=0.5,add=T,col="blue")

my.mod <- rip.qda.poly.subsamp
my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)
contour(x,y,my.gr.pred,levels=0.5,add=T,col="green")

reps <- 10
ssize <- 100
plot(data1.tr[,1:2])
points(data1.tr[data1.tr$V2==1,1:2],col="red")
# plot(data1.tr[,1:2])
# points(data1.tr[tr.cl,1:2],col="red")

for (i in 1:reps){
  idx <- c(sample(1:125,ssize,replace=F),sample(126:250,ssize,replace=F)  )
  rip.lda.poly.subsamp <- lda(V2~poly(V4,3)+poly(V9,3),data=data1.tr,subset=idx)
  rip.qda.poly.subsamp <- qda(V2~poly(V4,3)+poly(V9,3),data=data1.tr,subset=idx)
  my.mod <- rip.lda.poly.subsamp
  my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)
  contour(x,y,my.gr.pred,levels=0.5,add=T,col="blue")
  my.mod <- rip.qda.poly.subsamp
  my.gr.pred <- matrix(predict(my.mod,gr)$posterior[,2],nrow=m)
  contour(x,y,my.gr.pred,levels=0.5,add=T,col="green")
}



######################################################################

#logistic regression


# First, logistic regression.
rip.lr <- glm(V2~.,data=data1.tr,family="binomial")   

# compute the test set predictions, on the "response" scale
rip.lr.pred <- predict(rip.lr,data1.te,type="response")

# training set predictions
rip.lr.train.pred <- predict(rip.lr,type="response")

# error rates
rip.lr.err.test <- sum(data1.te$V2 != (rip.lr.pred > 0.5))/nrow(data1.te)
rip.lr.err.train <- sum(data1.tr$V2 != (rip.lr.train.pred > 0.5))/nrow(data1.tr)


# cheat knn on test set
krange <- 2:75
k.test.res <- numeric(length(krange))
for (k in krange){
  my.knn <-  knn(data1.tr[,1:2],data1.te[,1:2],data1.tr[,3],k=k)
  knn.cl <- as.numeric(my.knn)-1
  knn.prob <- (attributes(knn(data1.tr[,1:2],data1.te[,1:2],data1.tr[,3],k=k,prob=T)))$prob
  my.test.pred <- knn.cl * knn.prob + (1-knn.cl)*(1-knn.prob)
  err.rate <- sum((my.test.pred >= 0.5) != data1.te$V2)/nrow(data1.te) 
  k.test.res[k-1] <- err.rate
}

which.min(k.test.res)


plot(krange,k.test.res,xlab="k",ylab="error rate",type="b")


# knn split training set to choose k
tr.size <- 125
tr.idx <- sample(1:nrow(data1.tr),tr.size,replace=F)
valid.idx <- setdiff(1:nrow(data1.tr),tr.idx)

rip.tr <- data1.tr[tr.idx,]
rip.valid <- data1.tr[valid.idx,]

# modification of the code above
krange <- 2:75
k.test.res <- numeric(length(krange))
for (k in krange){
  my.knn <-  knn(rip.tr[,1:2],rip.valid[,1:2],rip.tr[,3],k=k)
  knn.cl <- as.numeric(my.knn)-1
  knn.prob <- (attributes(knn(rip.tr[,1:2],rip.valid[,1:2],rip.tr[,3],k=k,prob=T)))$prob
  my.test.pred <- knn.cl * knn.prob + (1-knn.cl)*(1-knn.prob)
  err.rate <- sum((my.test.pred >= 0.5) != rip.valid$V2)/nrow(rip.valid) 
  k.test.res[k-1] <- err.rate
}

which.min(k.test.res)
plot(krange,k.test.res,xlab="k",ylab="error rate",type="b")


# knn split training set to choose k
# and repeating multiple times
tr.size <- 125
krange <- 2:75
reps <- 30
rip.err.mat <- matrix(0,nrow=length(krange),ncol=reps)

for (j in 1:reps){
  tr.idx <- sample(1:nrow(data1.tr),tr.size,replace=F)
  valid.idx <- setdiff(1:nrow(data1.tr),tr.idx)
  rip.tr <- data1.tr[tr.idx,]
  rip.valid <- data1.tr[valid.idx,]
  k.test.res <- numeric(length(krange))
  for (k in krange){
    my.knn <-  knn(rip.tr[,1:2],rip.valid[,1:2],rip.tr[,3],k=k)
    knn.cl <- as.numeric(my.knn)-1
    knn.prob <- (attributes(knn(rip.tr[,1:2],rip.valid[,1:2],rip.tr[,3],k=k,prob=T)))$prob
    my.test.pred <- knn.cl * knn.prob + (1-knn.cl)*(1-knn.prob)
    err.rate <- sum((my.test.pred >= 0.5) != rip.valid$V2)/nrow(rip.valid) 
    k.test.res[k-1] <- err.rate
  }
  rip.err.mat[,j] <- k.test.res
}

boxplot(t(rip.err.mat),xlab=krange)


k.medians <- apply(rip.err.mat,1,median)
min(k.medians)
which.min(k.medians)




#####################################################

#cross validation



require(MASS)
require(class)
# LDA, train only, 10 fold cross validation. 

V <- 5
V.frac <- nrow(data1.tr)/V    # happily, sample size divides!
v.fold.lda.res <- numeric(V)

for (i in 1:V){
  v.test.idx <- ((i-1)*V.frac+1):(V.frac*i)
  v.train.idx <- setdiff(1:nrow(data1.tr),v.test.idx)
  rip.lda <-  lda(V2~.,data=data1.tr,subset=v.train.idx)
  rip.lda.pred <- predict(rip.lda,data1.tr[v.test.idx,])$posterior[,2]
  err.rate <- sum(data1.tr[v.test.idx,"V2"] != (rip.lda.pred > 0.5))/length(v.test.idx)
  v.fold.lda.res[i] <- err.rate
}
mean(v.fold.lda.res)
sqrt(var(v.fold.lda.res))/sqrt(V)


# QDA, train only, 10 fold cross validation. 

v.fold.qda.res <- numeric(V)

for (i in 1:V){
  v.test.idx <- ((i-1)*V.frac+1):(V.frac*i)
  v.train.idx <- setdiff(1:nrow(data1.tr),v.test.idx)
  rip.qda <-  qda(V2~.,data=data1.tr,subset=v.train.idx)
  rip.qda.pred <- predict(rip.qda,data1.tr[v.test.idx,])$posterior[,2]
  err.rate <- sum(data1.tr[v.test.idx,"V2"] != (rip.qda.pred > 0.5))/length(v.test.idx)
  v.fold.qda.res[i] <- err.rate
}
mean(v.fold.qda.res)
sqrt(var(v.fold.qda.res))/sqrt(V)

# LR, train only, 10 fold cross validation. 

v.fold.lr.res <- numeric(V)

for (i in 1:V){
  v.test.idx <- ((i-1)*V.frac+1):(V.frac*i)
  v.train.idx <- setdiff(1:nrow(data1.tr),v.test.idx)
  rip.lr <-  glm(V2~.,data=data1.tr,subset=v.train.idx,family="binomial")
  rip.lr.pred <- predict(rip.lr,data1.tr[v.test.idx,],type="response")
  err.rate <- sum(data1.tr[v.test.idx,"V2"] != (rip.lr.pred > 0.5))/length(v.test.idx)
  v.fold.lr.res[i] <- err.rate
}
mean(v.fold.lr.res)
sqrt(var(v.fold.lr.res))/sqrt(V)





# V fold cross validation for k-nn. 
# Note, the basic structure is the same, it is the details of the classifier that have changed
k=21
krange <- 2:75
v.fold.knn.mat <- matrix(0,nrow=length(krange),ncol=V)

for (i in 1:V){
  v.test.idx <- ((i-1)*V.frac+1):(V.frac*i)
  v.train.idx <- setdiff(1:nrow(data1.tr),v.test.idx)
  k.test.res <- numeric(length(krange))
  # loop over k
  for (j in krange){
    my.knn <-  knn(data1.tr[v.train.idx,1:2],data1.tr[v.test.idx,1:2],data1.tr[v.train.idx,3],k=j)
    knn.cl <- as.numeric(my.knn)-1
    knn.prob <- (attributes(knn(data1.tr[v.train.idx,1:2],data1.tr[v.test.idx,1:2],data1.tr[v.train.idx,3],k=k,prob=T)))$prob
    my.test.pred <- knn.cl * knn.prob + (1-knn.cl)*(1-knn.prob)
    err.rate <- sum((my.test.pred >= 0.5) != data1.tr$V2[v.test.idx])/length(v.test.idx) 
    k.test.res[j-1] <- err.rate
  }
  v.fold.knn.mat[,i] <- k.test.res 
}

boxplot(t(v.fold.knn.mat),xlab=krange)


k.medians <- apply(v.fold.knn.mat,1,median)
min(k.medians)
which.min(k.medians)


k.means <- apply(v.fold.knn.mat,1,median)
ks.std.errs <- apply(v.fold.knn.mat,1,function(x) sqrt(var(x)))/sqrt(V)

plot(krange,k.means)


# compare the minimum mean choices

min.ks <- which(k.means == min(k.means))
cbind(krange[min.ks],k.means[min.ks],ks.std.errs[min.ks])


