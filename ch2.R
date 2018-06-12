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

#MLE
library(MASS)
attach(geyser)
hist(waiting)

ll<-function(para)
{
  f1=dnorm(waiting, para[2], para[3])
  f2=dnorm(waiting, para[4], para[5])
  f = para[1] * f1 + (1-para[1])* f2
  ll = sum(log(f))
  return (-ll)
}
geyser.est = nlminb(c(0.5, 50,
                      10, 80, 10), ll,
                    lower=c(0.0001, -Inf, 0.0001, -Inf,
                            0.0001), upper = c(0.9999, Inf, Inf, Inf, Inf))
options(digits=3)
geyser.est$par
p<-geyser.est$par[1]
mu1<-geyser.est$par[2]
sigma1<-geyser.est$par[3]
mu2<-geyser.est$par[4]
sigma2<-geyser.est$par[5]

x<-seq(40, 120)
f<-p*dnorm(x, mu1, sigma1) + (1 - p) * dnorm(x, mu2, sigma2)
hist(waiting, freq=F)
lines(x, f)

num<-c(rep(0:5, c(1532, 581, 179, 41, 10, 4)))
install.packages("maxLik")
logLik<-function(para)
{
  f=dnbinom(num,para[1],1/(1 + para[2]))
  ll=sum(log(f))
  return(ll)
}
library(maxLik)
para <- maxLik(logLik, start = c(0.5, 0.4))$estimate
r <- para[1];
beta<-para[2];
l <-  length(num)
nbinomnum <-  dnbinom(0:5, r, 1/(1 + beta))* l
plot(0:5, nbinomnum, ylim=c(0, 1600))
points(0:5, c(1532, 581, 179, 41, 10, 4), type = 'p', col=2)
legend(3, 1000, legend=c("num", "poisson"), col=1:2, lty=1)

#均值区间估计
#大样本：用标准正太分布反求,中心极限，均值近似服从正太
a<-0.05
ua<-qnorm(c(a/2, 1-a/2))
x<- seq(-3, 3, 0.1)
curve(dnorm(x), -3, 3)
lines(ua, dnorm(ua), xlim = c(-3, 3), ylim = c(0, 0.5), type="h")
legend(-0.5, 0.3, "a=0.95", bty="n")

u.conf.int<-function(x, sigma, conf.level=0.95)
{
  n<- length(x)
  xbar<-mean(x)
  a<- 1-conf.level
  ua<-qnorm(1 - a/2)
  Se <- sigma/sqrt(n) #均值方差是样本方差的sqrt(n)^-1
  xbar + c(-ua* Se, ua* Se)
}
x <- c(7.2, 3.5, 4.3, 6.2, 10.1, 5.4, 6.8, 4.5, 5.1, 6.6, 3.8, 8.2)
u.conf.int(x, 2)

#小样本均值区间估计, t分布
a<- 0.05
ta<-qnorm(c(a/2, 1-a/2))
x<-seq(-4, 4, 0.1)
curve(dt(x,12), -4, 4)
lines(ta, dt(ta,12), xlim = c(-4, 4), ylim=c(0, 0.5), type="h")
t.conf.int <- function(x, conf.level = 0.95)
{
  n <- length(x)
  xbar <- mean(x)
  a <- 1 - conf.level
  ta <- qt(1-a/2, n -1 )
  s <- sd(x)
  Se <- s / sqrt(n)
  xbar + c( -ta * Se, ta * Se)
}
x <- c(7.2, 3.5, 4.3, 6.2, 10.1, 5.4, 6.8, 4.5, 5.1, 6.6, 3.8, 8.2)
t.conf.int(x)
t.test(x)

x <- rnorm(100, mean=10); y<-rt(100, 10)
par(mfrow=c(1, 3))
boxplot(x, y)
hist(x)
hist(y)

#比较检验
twosample.ci<- function(x, y, alpha, sigma1, sigma2)
{
  n1 <- length(x); n2 <- length(y)
  xbar <- mean(x) - mean(y)
  z <- qnorm(1 - alpha/2)* sqrt(sigma1 ^ 2/n1 + sigma2 ^ 2 / n2)
  c(xbar - z, xbar + z)
}

t.test(x, y, var.equal = TRUE)$conf.int
t.test(x, y, var.equal = TRUE)

twosample.ci2 <- function(x, y, alpha)
{
  n1 <- length(x); n2 <- length(y)
  xbar <- mean(x) - mean(y)
  s1 <- var(x); s2 <- var(y)
  nu <- (s1/n1 + s2/n2)^2/(s1^2/n1^2/(n1 -1) + s2^2/n2^2/(n2 - 1))
  z<- qt(1 - alpha/2, nu) * sqrt(s1/n1 + s2/n2)
  c(xbar - z, xbar + z)
}

#中位数的区间估计
x<- c(21240, 4632, 22838, 5484, 5052, 5064, 6972, 7596, 14760, 15012,
      18720, 9480, 4728, 67200, 52788)
EDA<-function(x)
{
  par(mfrow = c(2,2))
  hist(x)
  dotchart(x)
  boxplot(x, horizontal = T)
  qqnorm(x);qqline(x)
  par(mfrow=c(1,1))
}
EDA(x)

wilcox.test(x, conf.int = T)

#比例的区间估计
prop.test(47, 100)
prop.test(47, 100, conf.level = 0.9)
?prop.test
#置信区间的模拟比较
m <- 100; n <- 20; p<-0.5
phat <-rbinom(m, n, p)/n
se <- sqrt(phat *(1-phat) / n)
a<-0.05;zstar<-qnorm(1 - a/2)
matplot(rbind(phat - zstar * se, phat + zstar * se), rbind(1:m, 1:m), type="l", lty = 1)
abline(v=p)

#homework
data <- rnorm(20, 10, 2)
EDA(data)
library(BSDA)
z.test(data, sigma.x =2,conf.level = 0.95)

qqnorm(data)
t.test(data,conf.level = 0.95)
