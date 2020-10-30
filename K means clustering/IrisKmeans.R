library(datasets)
data(iris)
str(iris)
dataset <- iris[,c(3:5)]
names(dataset) <- c("x","y","Species")

# K means Clustering
ocenters <- data.frame(x=c(2.2,4.2,6.5),y=c(0.7,1.7,2),Species=unique(dataset$Species))
par(bg="lightblue",fg="navy blue")
plot(x=dataset$x,y=dataset$y,pch=19,xlab = "Petal Length",ylab="Petal Width"
     ,main="Petal Length vs Petal Width")

for(i in 1:100){
  distance1 <- sqrt((dataset$y-ocenters[1,"y"])^2+(dataset$x-ocenters[1,"x"])^2)
  distance2 <- sqrt((dataset$y-ocenters[2,"y"])^2+(dataset$x-ocenters[2,"x"])^2)
  distance3 <- sqrt((dataset$y-ocenters[3,"y"])^2+(dataset$x-ocenters[3,"x"])^2)
  distance <- cbind.data.frame(distance1,distance2,distance3)
  distance$Species <- apply(distance,1,which.min)
  dataset$cluster <- factor(distance$Species)
  
  ncenters <- data.frame()
  for(i in 1:3){
    subdata <- subset(dataset,cluster==i)
    cx <- mean(subdata$x) ; cy <- mean(subdata$y)
    c <- cbind.data.frame(x=cx,y=cy,cluster=i)
    ncenters <- rbind.data.frame(ncenters,c)
  }
  if(all(ocenters==ncenters)) break
  ocenters=ncenters
}

# Comparison with orginal Species
par(bg="lightblue",fg="navy blue",mfrow=c(1,2),mar=c(4,4,2,2))
plot(dataset$x,dataset$y,col=dataset$cluster,main="Our Clusters",xlab="Petal Length",
     ylab="Petal Width",pch=19)
legend(x=c(1,3.5),y=c(2,2.5),legend=unique(dataset$Species),col=unique(dataset$Species),pch=19,cex=0.6)
plot(dataset$x,dataset$y,col=dataset$Species,main="Original Clusters",xlab="Petal Length",
     ylab="Petal Width",pch=19)
legend(x=c(1,3.5),y=c(2,2.5),legend=unique(dataset$Species),col=unique(dataset$Species),pch=19,cex=0.6)
