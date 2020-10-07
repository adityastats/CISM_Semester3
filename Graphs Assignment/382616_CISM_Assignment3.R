#====================================================
# QUESTION 2
#====================================================

distance <- c(0.4,0.7,1.0,1.5,4,5.8,7,8,8.8)
planets <- c("Mercury","Venus","Earth","Mars","Jupiter","Saturn","Neptune","Uranus","Pluto")
diameter <- c(0.5,1.2,1.3,0.7,4,3.5,2.5,2,1)*3
au <- c(0.4,0.7,1,1.5,5.2,9.5,19.2,30,39.3)

xcenter <- 1 
ycenter <- 2 
a <- distance*2 # major axis length
b <- a/2 # minor axis length
phi <- 1 # angle of major axis with x axis

# For generating Orbits
t <- seq(0, 2*pi, 0.0001) 
x <- xcenter + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)
y <- ycenter + a*cos(t)*cos(phi) + b*sin(t)*cos(phi)


# TO GENERATE PLOT

# Generating Planet Positions (Randomly)

planetpos <- sample(-80:20,9)
planetpos <- (pi/180)*planetpos
planetx <- xcenter + a*cos(planetpos)*cos(phi) - b*sin(planetpos)*sin(phi)
planety <- ycenter + a*cos(planetpos)*cos(phi) + b*sin(planetpos)*cos(phi)
planetpos <- data.frame(row.names = planets,x=planetx,y=planety,dist = au)

par(bg="#070710",fg="sky blue",mar=c(3,2,3,1))
plot(x=runif(70,0.5,14),y=runif(70,-8,15),col="white",pch=16,cex=0.1,xlim=c(0.8,14),ylim=c(-8,15))
points(x,y,pch=20,col="black",cex=0.0001)
points(0.5,1.8,col="gold", pch=19,cex=18)
points(0.5,1.8,col="#feab20", pch=19,cex=16)
points(0.5,1.8,col="#fe8a20", pch=19,cex=13)
points(0.5,1.8,col="#fe6420", pch=19,cex=10)
points(0.5,1.8,col="#fe2220", pch=19,cex=5)
points(0.5,1.8,col="#ff5420", pch=19,cex=3)
points(0.5,1.8,col="#fe2220", pch=19,cex=1)
points(planetpos$x,planetpos$y,pch=20,col=c("#595959","brown","blue","#e81c01","#e84e01","#7c4506","#572ff6",
                                            "#10014a","#c0c0c0"),cex=diameter)
with(planetpos[1,],text(y~x,labels = row.names(planetpos[1,]),col = "mistyrose",cex=0.7,pos=3))
with(planetpos[2,],text(y~x,labels = row.names(planetpos[2,]),col = "mistyrose",cex=0.7,pos=1))
with(planetpos[c(3:4,9),],text(y~x,labels = row.names(planetpos[c(3:4,9),]),col = "mistyrose",cex=0.7,pos=4))
with(planetpos[5:8,],text(y~x,labels = row.names(planetpos[5:8,]),col = "mistyrose",cex=0.7,pos=4,offset=1))
text(x=0.5,y=1.8,col="white",labels = "Sun",cex=1,pos=4,offset=1.5,font=2)
legend(x=c(12,14),y=c(-8.5,2),legend=paste(planets,au,sep=" -- "),cex=0.5,title="Distance(AU)",
       pch=16,col=c("#595959","brown","blue","#e81c01","#e84e01","#7c4506","#572ff6","#10014a","#c0c0c0"))


#===================================================
# QUESTION 1
#===================================================

# Cartesian System

ipl <- read.csv("ipl.csv",header = T)
ipl2020 <- subset(ipl,Season==2020)
ipl2020$Price <- ipl2020$Price/10000000
par(bg="#070710",fg="sky blue",mar=c(4,4,3,1))
plot(22,0,xlim=c(22,34),ylim=c(0,15),type="n",col.axis="sky blue",xlab="AGE",ylab="Auction Price",
     col.lab="sky blue",main="Plotting points in Cartesian System",col.main="sky blue")
abline(h=seq(0,15,0.5),v=seq(22,34,0.2),lwd=1)
abline(h=seq(0,15,2.5),v=seq(22,34,1),lwd=2,col="navy blue")
points(ipl2020$Age,ipl2020$Price,pch=20,col="gold",cex=1.5)


# Logarithmic System

ipl <- read.csv("ipl.csv",header = T)
ipl2020 <- subset(ipl,Season==2020)
par(bg="#070710",fg="sky blue",mar=c(4,4,3,1))
plot(22,0,xlim=c(22,34),ylim=c(log10(1),log10(1.5e+08)),type="n",col.axis="sky blue",xlab="AGE",
     ylab="Auction Price(log)",col.lab="sky blue",main="Plotting points in Logarithmic System",
     col.main="sky blue")
abline(v=seq(22,34,0.2))
abline(h=c(log10(1:10),log10(seq(10,100,10)),log10(seq(100,1000,100)),log10(seq(1000,10^4,1000)),
           log10(seq(10^4,10^5,10^4)),log10(seq(10^5,10^6,10^5)),log10(seq(10^6,10^7,10^6)),
           log10(seq(10^7,10^8,10^7))))
abline(v=seq(22,34,1),h=seq(0,8,1),lwd=2,col="navy blue")
points(ipl2020$Age,log10(ipl2020$Price),pch=20,col="gold",cex=1.5)


# Polar System

x <- ipl2020$Age
y <- ipl2020$Price/10000000
r <- sqrt(x^2+y^2) 
theta <- atan2(y,x)
min(r);max(r)
png("polar.png",height=600,width=600)
par(bg="#070710",fg="sky blue",mar=c(4,4,3,1))
plot(x=0,y=0,xlim=c(-40,40),ylim=c(-40,40),main="Plotting points in Polar System",
     col.main="sky blue",col.axis="sky blue",type="n")
abline(v=0,h=0,lwd=2,col="navy blue")
t <- seq(0, 2*pi, 0.0001)
rad <- seq(0,40,2)
x <- rad*cos(t)
y <- rad*sin(t)
points(x,y,col="sky blue",pch=20,cex=0.0001)
points(x=r*cos(theta),y=r*sin(theta),cex=1.5,col="gold",pch=20)
dev.off()