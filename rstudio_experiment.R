
library(MASS)
library(class)

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


#basic-plots

plot(data1[,1:2])
points(data1[data1$V2==1,1:2],col="red")

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
err.rate <- sum((my.test.pred >= 0.5) != data1.te$yc)/nrow(data1.te)


err.rate

my.gr.knn <- knn(data1.tr[,1:2],gr,data1.tr[,3],k=k)
my.gr.knn.cl <- as.numeric(my.gr.knn)-1
my.gr.knn.prob <- (attributes(knn(data1.tr[,1:2],gr,data1.tr[,3],k=k,prob=T)))$prob
my.gr.pred <- my.gr.knn.cl * my.gr.knn.prob + (1-my.gr.knn.cl)*(1-my.gr.knn.prob)
my.gr.pred.mat <- matrix(my.gr.pred,ncol=m)

contour(x,y,my.gr.pred.mat,levels=0.5,add=T,col="orange",lty=2,lwd=2,drawlabels=F)




#LDA




