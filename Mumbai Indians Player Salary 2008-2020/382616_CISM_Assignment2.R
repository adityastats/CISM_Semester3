ipl <- read.csv("MI_Players_Salary.csv",header=TRUE,na.strings = "NA")      # Loading complete dataset
str(ipl)
ipl$Season <- as.factor(ipl$Season)                           # Changing into factor variable
ipl$Player_Category <- as.factor(ipl$Player_Category)

#===============================================================================================================
# QUESTION 1
#===============================================================================================================

auction2020 <- subset(ipl,Season=="2020")                     # Extracting data for 2020
par(bg="navy blue",fg="gold",mar=c(6,4,4,2))
plot(auction2020$Age,auction2020$Price,xlab = "AGE",ylab = "Auction Price(Rs.)",
     pch = 16,col=c("light blue","red")[auction2020$Player_Category],col.main="gold",col.lab="gold",
     col.axis="gold",main = "Scatter Plot of Auction Price vs Age",sub="TEAM : MUMBAI INDIANS (2020)",
     col.sub="gold",cex.sub=0.8,font.sub=13,cex=1.2)
grid(ny=6,col="gold",lwd=1.5)
legend("topleft",pch=16,legend=levels(auction2020$Player_Category),col = c("light blue","red"))  

#===============================================================================================================
# QUESTION 2
#===============================================================================================================

# Extracting most paid 15 players for each season
top15 <- data.frame(sl=1:15)
for (i in levels(ipl$Season)){
  seasondata <- ipl[ipl$Season==i,c("Price")]
  dataordered <- seasondata[order(seasondata,decreasing = TRUE)]
  top15 <- cbind(top15,i=dataordered[1:15])
}
top15 <- top15[,2:14]
names(top15) <- levels(ipl$Season)

# Plotting box plots
par(bg="navy blue",fg="gold",mar=c(6,4,4,1))
boxplot(top15,col="gold",border="black",main="Comparison of Auction Prices of 15 most paid players",
        col.axis="gold",ylab="Auction Price(Rs.)",col.lab="gold",col.main="gold",pch=16,cex=1.3,
        outcol="light blue",sub="TEAM : MUMBAI INDIANS",col.sub="gold",cex.sub=0.8,font.sub=13,cex=1.2)

#===============================================================================================================
# QUESTION 3
#===============================================================================================================

# Extracting data of highest price for each IPL Seasons
highest <- data.frame()
for (i in levels(ipl$Season)){
  seasondata <- ipl[ipl$Season==i,c("Player.Name","Season","Price")]
  dataordered <- seasondata[order(seasondata[,"Price"],decreasing = TRUE),]
  highest <- rbind(highest,dataordered[1,])
}
highest$Season <- as.integer(as.character(highest$Season))

# Line plot of highest price for all IPL Seasons
par(bg="navy blue",fg="gold",mar=c(5,4,4,2),mfrow=c(1,1))
plot(highest$Season,highest$Price,type="o",lwd=2,pch=16,col="gold",col.axis="gold",col.main="gold",
     col.lab="gold",main="Highest Auction Price in all IPL Seasons",sub="TEAM : MUMBAI INDIANS",
     col.sub="gold",cex.sub=0.8,font.sub=13,cex=1.2,xlab="IPL Season",ylab="Auction Price(Rs.)",
     ylim=c(4.0e+07,1.6e+08))
grid(col="gold",lwd=1.5)

#===============================================================================================================
# END
#===============================================================================================================
