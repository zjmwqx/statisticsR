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
#pin:画的宽，高

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

attach(mtcars)
opar<- par(no.readonly = T)
par(mfrow=c(2, 2))
plot(wt, mpg,main="scatterplot of wt vs. mpg")
plot(wt, disp, main="scatterplot of wt vs. disp")
hist(wt, main="Histgram of wt")
boxplot(wt, main="Boxplot of wt")
par(opar)
detach(mtcars)

attach(mtcars)
opar<-par(no.readonly = T)
par(mfrow=c(3,1 ))
hist(wt)
hist(disp)
hist(mpg)
par(opar)
detach(mtcars)

attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
hist(wt)
hist(disp)
hist(mpg)
detach(mtcars)

attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow=T))
hist(wt)
hist(disp)
hist(mpg)
detach(mtcars)

attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow=T), widths = c(3, 1), heights = c(1, 2))
hist(wt)
hist(disp)
hist(mpg)
detach(mtcars)

opar<-par(no.readonly = T)
par(fig=c(0, 0.8, 0, 0.8))
plot(mtcars$wt, mtcars$mpg, xlab="Miles per Gallon",
     ylab = "Car Weight")
par(fig=c(0, 0.8, 0.55, 1), new=T)
boxplot(mtcars$wt, horizontal = T, axes=F)
par(fig=c(0.65, 1, 0, 0.8), new=T)
boxplot(mtcars$mpg, horizontal = T,axes=F)
mtext("Enhanced Scatterplot", side =3, outer=T, line=-3)
par(opar)

library('vcd')
library('Hmisc')
counts<-table(Arthritis$Improved)

opar <- par(no.readonly = T)
par(mfrow = c(1,1))
barplot(counts, main='Simple Bar Plot', xlab="Improved", ylab = "Frequence")
barplot(counts, main='Simple Bar Plot', ylab="Improved", xlab = "Frequence", horiz = T)
counts<-table(Arthritis$Improved, Arthritis$Treatment)
plot(Arthritis$Improved)
barplot(counts, main="Stacked Bar Plot", col=c("red", "yellow", "green"), legend = rownames(counts))
barplot(counts, main="Grouped Bar Plot", col=c("red", "yellow", "green"), legend = rownames(counts), beside = T)
states<-data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, by=list(state.region), FUN=mean)
means <- means[order(means$x),]
barplot(means$x, names.arg = means$Group.1)
title("Mean Illiteracy Rate")
par(mar = c(5, 8, 4, 2))
par(las=2)
barplot(means$x, names.arg = means$Group.1, cex.names = 0.8, horiz = T)
title("Mean Illiteracy Rate")

attach(Arthritis)
counts<-table(Treatment, Improved)
spine(counts, main = "Spinogram Example")
detach(Arthritis)


opar<-par(no.readonly = T)
par(mfrow=c(2, 2))
slices = c(10, 12, 4, 16, 8)
lbls<- c("US", "UK", "AUS", "Ger", "France")
pie(slices, labels = lbls, main="Simple Pie Chart")
pct<-round(slices/sum(slices)* 100)
lbls2<-paste(lbls, " ", pct, "%", sep="")
pie(slices, lbls2, col=rainbow(length(slices)))

library(plotrix)
pie3D(slices, labels = lbls, explode = 0.1, labelcex=0.8, main="3D pie chart")

fan.plot(slices, labels = lbls, explode = 0.1, main="fan chart")

par(opar)
par(mfrow=c(2, 2))
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 12, col="red", xlab="miles per gallon")
rug(jitter(mtcars$mpg))
hist(mtcars$mpg, freq=FALSE, breaks = 12, col="red", xlab="miles per gallon")
lines(density(mtcars$mpg), col="blue", lwd=2)
x<-mtcars$mpg
h<-hist(x, breaks = 12, col="red", xlab="Miles per gallon",
        main="Histogram with normal curve and box")
xfit <- seq(min(x), max(x), length=40)
yfit <-dnorm(xfit, mean=mean(x), sd=sd(x))
yfit<- yfit*length(x)
lines(xfit, yfit,col="blue",lwd=2)
box()

par(mfrow=c(2, 1))
d<- density(mtcars$mpg)
plot(d)
polygon(d, col="red", border="blue")
rug(mtcars$mpg, col="brown")
library(sm)
attach(mtcars)
cyl.f<- factor(cyl, levels=c(4,6, 8), labels = c("4 cylinder",
                                                 "6 cylinder",
                                                 "8 cylinder"))
sm.density.compare(mpg, cyl, xlab="Miles per Gallon")
colfill <- c(2:(1+length(levels(cyl.f))))
legend(locator(1), levels(cyl.f), fill = colfill, cex = 0.3)
detach(mtcars)
par(opar)
boxplot(mtcars$mpg, main="Box plot", ylab="Miles per Gallon")
boxplot.stats(mtcars$mpg)
attach(mtcars)
boxplot(mpg~cyl)
boxplot(mpg~cyl, notch=TRUE,
        varwidth=TRUE,
        col="red")
cyl.f <- factor(mtcars$cyl, levels = c(4,6, 8), labels=c("4","6","8"))
am.f<- factor(mtcars$am, levels=c(0, 1), labels=c("auto", "standard"))
boxplot(mpg~am.f*cyl.f, data=mtcars, varwidth=T,
        col=c("gold", "darkgreen"))
library(vioplot)
x1<-mtcars$mpg[mtcars$cyl == 4]
x2<-mtcars$mpg[mtcars$cyl == 6]
x3<-mtcars$mpg[mtcars$cyl == 8]
vioplot(x1, x2, x3, names=c("4 cyl", "6 cyl", "8 cyl"), col="gold")

dotchart(mtcars$mpg,  labels = row.names(mtcars), cex=0.7)
x<-mtcars[order(mtcars$mpg),]
x$color[x$cyl == 4] <- "red"
x$color[x$cyl == 6] <- "blue"
x$color[x$cyl == 8] <- "darkgreen"
dotchart(x$mpg, color = x$color, groups = x$cyl, labels = row.names(x),
         cex=0.7, pch=19)

library(RODBC)
library(xlsx)
data<-read.xlsx("ch1_homework.xls", 1, encoding = "gbk")
names(data) <-paste(c("id", "name", "date", "quality", 
                      "price", "pack_price"), sep=" ")
hist(data$quality, breaks = 12, col = "red", main="product-quality",
     xlab = "quality", cex.axis=0.7)

plot(data$quality, pch=16,col="blue", main="quality plot", ylab="quality")
