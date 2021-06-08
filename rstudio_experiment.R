

wdbc <- read.csv("~/Documents/classification_problem/wdbc.data", header=FALSE)

# a1 = wdbc$V3
# a2 = wdbc$V13
# a3 = wdbc$V23

#boxplot(a1,a2,a3, horizontal = TRUE)
# 
# b1 = wdbc$V6
# b2 = wdbc$V16
# b3 = wdbc$V26

#boxplot(b1,b2,b3, horizontal = TRUE)

# radius1 = wdbc$V3
# texture1 = wdbc$V4
# perimeter1 = wdbc$V5
# area1 = wdbc$V6
# smoothness1 = wdbc$V7
# compactness1 = wdbc$V8
# concavity1 = wdbc$V9
# concavepoints1 = wdbc$V10
# symmetry1 = wdbc$V11
# fractaldim1 = wdbc$V12
# 
# 


# plot(radius1, texture1)
# plot(radius1, perimeter1)
# plot(radius1, area1)
# plot(radius1, smoothness1)
# plot(radius1, compactness1)
# plot(radius1, concavity1)
# plot(radius1, concavepoints1)
# plot(radius1, symmetry1)
# plot(radius1, fractaldim1)
# plot(texture1, perimeter1)
# plot(texture1, area1)
# plot(texture1, smoothness1)
# plot(texture1, compactness1)
# plot(texture1, concavity1)
# plot(texture1, concavepoints1)
# plot(texture1, symmetry1)
# plot(texture1, fractaldim1)
# plot(perimeter1, area1)
# plot(perimeter1, smoothness1)
# plot(perimeter1, compactness1)
# plot(perimeter1, concavity1)
# plot(perimeter1, concavepoints1)
# plot(perimeter1, symmetry1)

# 
# datanew <- wdbc[, c('V2' ,'V11' ,'V12')]
# plot(datanew[,2:3])
# points(datanew[datanew$V2=='M',2:3],col="red")
# 


k <- 3

#compute k-nn on the test set

my.knn <-  knn(synth.tr[,1:2],synth.te[,1:2],synth.tr[,3],k=k)
knn.cl <- as.numeric(my.knn)-1

# extract the posterior probabilities, and compute the test set error rate
knn.prob <- (attributes(knn(synth.tr[,1:2],synth.te[,1:2],synth.tr[,3],k=k,prob=T)))$prob
my.test.pred <- knn.cl * knn.prob + (1-knn.cl)*(1-knn.prob)
err.rate <- sum((my.test.pred >= 0.5) != synth.te$yc)/nrow(synth.te) 


err.rate



###


# evalute the predicted probabilites on the grip, extract the probabilities
my.gr.knn <- knn(wdbc[,3:4],gr,wdbc[,2],k=k)
my.gr.knn.cl <- as.numeric(my.gr.knn)-1
my.gr.knn.prob <- (attributes(knn(wdbc[,3:4],gr,wdbc[,2],k=k,prob=T)))$prob
my.gr.pred <- my.gr.knn.cl * my.gr.knn.prob + (1-my.gr.knn.cl)*(1-my.gr.knn.prob)
my.gr.pred.mat <- matrix(my.gr.pred,ncol=m)

# add the contour plot
contour(x,y,my.gr.pred.mat,levels=0.5,add=T,col="orange",lty=2,lwd=2,drawlabels=F)




#me having a go:
datanew <- wdbc[, c('V2' ,'V3' ,'V4')]

# evalute the predicted probabilites on the grip, extract the probabilities
my.gr.knn <- knn(datanew[,2:3],gr,wdbc[,1],k=k)
my.gr.knn.cl <- as.numeric(my.gr.knn)-1
my.gr.knn.prob <- (attributes(knn(datanew[,2:3],gr,datanew[,1],k=k,prob=T)))$prob
my.gr.pred <- my.gr.knn.cl * my.gr.knn.prob + (1-my.gr.knn.cl)*(1-my.gr.knn.prob)
my.gr.pred.mat <- matrix(my.gr.pred,ncol=m)

# add the contour plot
contour(x,y,my.gr.pred.mat,levels=0.5,add=T,col="orange",lty=2,lwd=2,drawlabels=F)
