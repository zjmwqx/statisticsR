dose<-c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
plot(dose, drugA, type='b')

opar <- par(no.readonly = T)
par(lty = 2, pch = 17)
plot(dose, drugA, type = "b")
par(opar)

plot(dose, drugA, type="b", lty = 3, lwd = 3, pch = 15, cex = 3)
colors()

n<- 10
mycolors <- rainbow(n)
pie(rep(1, n), labels=mycolors, col=mycolors)
mygreys <- grey(0:n /n)
pie(rep(1, n), labels=mygreys, col=mygreys)

opar<-par(no.readonly = T)
par(mfrow=c(1, 2))
par(pin=c(2, 3))
par(lwd=2, cex=1.5)
par(cex.axis = .75, font.axis=3)
plot(dose, drugA, type = 'b', pch=19, lty=2, col="red")
plot(dose, drugB, type = 'b', pch=23, lty=6, col="blue", bg="green")
par(opar)

library(Hmisc)
x<-c(1:10)
y<-x
z<-10/x
opar<-par(no.readonly = T)
par(mar = c(5,4,3,8) + 0.1)
plot(x, y , type="b", pch=21, col="red", yaxt="n",lty=3, ann=FALSE)
lines(x, z, type="b", pch=22, col="blue", lty = 2)
axis(2, at=y, labels=y, col.axis="red", las=2)
axis(4, at=z, labels=round(z, digits = 2), col.axis="blue", las = 2, 
     cex.axis= 0.75, tck = -0.01)
mtext("z=10/x", side=4, line=3, cex.lab=1, las=2, col="blue")
title("An example of Creative Axes", xlab = "x values", ylab="y=x")
minor.tick(nx=3, ny=1, tick.ratio = 0.5)
par(opar)

opar<- par(no.readonly = T)
par(lwd=2, cex=1.5, font.lab=2)
plot(dose, drugA, type="b", pch=15, lty=1, col="red", ylim=c(0, 60))
lines(dose, drugB, type="b", pch=17, lty=2, col="blue")
abline(h=c(30), lwd=1.5, lty=2, col="blue")
minor.tick(nx=3, ny=3, tick.ratio = 0.5)
legend("topleft", inset = 0.05, title="Drug Type",
       c("A", "B"), lty = c(1,2), pch=c(15, 17), 
       col=c("red", "blue"), cex=0.5)
par(opar)

attach(mtcars)
plot(wt, mpg, main="Mileage vs. Car Weight", xlab="weight", ylab="Mileage",
     pch=18, col="blue")
text(wt, mpg, row.names(mtcars), cex=0.6, pos=4, col='red')
detach(mtcars)
