#单变量数据分析
attach(mtcars)
stem(mpg)
EDA<-function(x)
{
  par(mfrow = c(2, 2))
  hist(x)
  dotchart(x)
  boxplot(x, horizontal = T)
  qqnorm(x);qqline(x)
  par(mfrow=c(1,1))
}
EDA(mpg)

x <- runif(1000)
EDA(x)
x <- rnorm(1000)
EDA(x)
x <- rt(1000, 10)
EDA(x)
x <- rf(100, 10, 10)
EDA(x)
x <- r
x <- abs(rnorm(1000))
EDA(x)
x <- rexp(200)
EDA(x)

#类目型数据
 x  <- c("是" ,"否" ,"否" ,"是","是","否","否","是","是")
table(x)
 
barplot(table(x))
mean(mtcars$mpg)
median((mtcars$mpg))
var(mtcars$mpg)
sd(mtcars$mpg)
fivenum(mtcars$mpg)
summary(mtcars$mpg)
IQR(mtcars$mpg)
mad(mtcars$mpg)
stem(mtcars$mpg)
mpgg <- cut(mtcars$mpg, breaks=c(10,20, 30, max(mtcars$mpg)))
mpgg
table(mpgg)

#多变量数据分析
smoke=c("Y","N", "N", "Y", "N", "Y", "Y", "Y", "N", "Y")
study=c("<5h", "5-10h", "5-10h", ">10h", ">10h","<5h", "5-10h", "<5h", ">10h", "5-10h")
tab = table(smoke, study)
prop.table(tab, 1)
prop.table(tab, 2)
par(mfrow=c(1,3 ))
barplot(tab)
barplot(t(tab))
barplot(t(tab), beside=T)

levels(iris$Species)
iris.lab <- rep(c("1", "2", "3"), rep(50, 3))
par(mfrow=c(1,2))
plot(iris[, 1], iris[,3], type="n")                
text(iris[, 1], iris[, 3], iris.lab,cex=0.6)
pairs(iris)
pairs(iris, pch=21, bg=iris.lab) #一目了然

#参数估计
num<-c(rep(0:5, c(1532, 581, 179, 41, 10, 4)))
lambda <-mean(num)
lambda

k<-0:5
ppois = dpois(k, lambda)
poisnum <- ppois* length(num)
plot(k, poisnum, ylim = c(0, 1600))
samplenum<-as.vector((table(num)))
points(k, samplenum, type = "p", col=2)

library(rootSolve)
x <- c(4, 5,4, 3, 9,9, 5,7,9,8,0,3,8,0,8,7,2,1,1,2)
m1 <- mean(x)
m2 <- var(x)
model <- function(x, m1, m2)
{
  c(f1=x[1] + x[2] -2* m1, f2=((x[2]-x[1]) ^ 2) / 12 - m2)
}
multiroot(f=model, start=c(0, 10), m1=m1, m2=m2)

