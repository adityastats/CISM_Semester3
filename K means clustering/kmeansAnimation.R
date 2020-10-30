library(animation)
gen.norm.data <- function(n,center,sigma,cluster){
  dataset <- data.frame()
  x <- rnorm(n,mean=center[1],sd=sigma[1])
  y <- rnorm(n,mean=center[2],sd=sigma[2])
  data <- cbind.data.frame(x,y)
  data <- cbind.data.frame(data,cluster=factor(cluster))
}
data1 <- gen.norm.data(20,c(5,3),c(1,1),1)
data2 <- gen.norm.data(20,c(7,7),c(1,1),2)
data3 <- gen.norm.data(25,c(9,1),c(1,1),3)
dataset <- rbind.data.frame(data1,data2,data3)

#Visualizing the different clusters initially

par(bg="lightblue",fg="navy blue")
plot(x=dataset$x,y=dataset$y,pch=19,xlab = "x",ylab="y",col=dataset$cluster,
     main="Original Clusters")


# Choosing initial centers
center <- function(group){
    subdata <- subset(dataset,cluster==group)
    cx <- sample(subdata$x,1); cy <- sample(subdata$y,1)
    centers <- cbind.data.frame(x=cx,y=cy,cluster=group)
}

ocenters <- data.frame(x=c(5,9,8),y=c(6,8,4),cluster=c(1,2,3))

# Clustering Animation
for(i in 1:ani.options('nmax')){
  # Raw Plot
  par(bg="lightblue",fg="navy blue",mar=c(4,4,1,2))
  plot(x=dataset$x,y=dataset$y,pch=19,xlab = "x",ylab="y")
  ani.pause(interval=0.5)
  
  #adding centroids to plot
  points(x=ocenters$x,y=ocenters$y,col=ocenters$cluster,pch=17,cex=2)
  ani.pause()
  text(x=5.5,y=0,labels="Assigning clusters",cex=0.7,col="red")

  # animation for new cluster
  for(j in 1:nrow(dataset)){
    ani.pause(interval=0.1)
    distance1 <- sqrt((dataset[j,"y"]-ocenters[1,"y"])^2+(dataset[j,"x"]-ocenters[1,"x"])^2)
    distance2 <- sqrt((dataset[j,"y"]-ocenters[2,"y"])^2+(dataset[j,"x"]-ocenters[2,"x"])^2)  
    distance3 <- sqrt((dataset[j,"y"]-ocenters[3,"y"])^2+(dataset[j,"x"]-ocenters[3,"x"])^2)
    distance <- cbind.data.frame(distance1,distance2,distance3)
    distance$cluster <- apply(distance,1,which.min)
    dataset[j,"cluster"] <- factor(distance$cluster)
    ani.pause(interval=0.1)
    points(x=dataset[j,"x"],y=dataset[j,"y"],pch=19,col=dataset[j,"cluster"])
    ani.pause(interval=0.1)
  }
  
  # Calculation of new centers
  ncenters <- data.frame()
  for(i in 1:3){
    subdata <- subset(dataset,cluster==i)
    cx <- mean(subdata$x) ; cy <- mean(subdata$y)
    c <- cbind.data.frame(x=cx,y=cy,cluster=i)
    ncenters <- rbind.data.frame(ncenters,c)
  }
  if(all(ocenters==ncenters)) break
  ocenters=ncenters
  plot(x=dataset$x,y=dataset$y,pch=19,xlab = "x",ylab="y")
  text(x=5.5,y=0,labels="Calculating new centers",cex=0.7,col="red")
  ani.pause(interval=2)
}

