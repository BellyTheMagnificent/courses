attach(mtcars)

# Q1 
mtcars$cyl = as.factor(cyl)
lm1 = lm(mpg ~ as.factor(cyl) + wt)
lm2 = lm(mpg ~ as.factor(cyl))
summary(lm1)
summary(lm2)
install.packages("swirl")
fit2<-lm(mpg ~ cyl +wt, data=mtcars)
mycol=rainbow(8)
plot(mtcars$wt, mtcars$mpg, pch=19, col=mycol[mtcars$cyl])
abline(c(fit2$coeff[1],fit2$coeff[4]),col="red",lwd=3)
abline(c(fit2$coeff[1] + fit2$coeff[2] ,fit2$coeff[4]),col="blue",lwd=3)
abline(c(fit2$coeff[1] + fit2$coeff[3] ,fit2$coeff[4]),col="black",lwd=3)


# Q3
fit<-lm(mpg~factor(cyl)+wt,data=mtcars)
fit1<-lm(mpg~factor(cyl)+wt+interaction(cyl,wt),data=mtcars)
anova(fit1, fit2)

# Q4
fit3 = lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(fit3)


# Q5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fitQ5 = lm(y ~ x)
plot(fitQ5)

round(hatvalues(fitQ5), 3)


# Q6
round(dfbetas(fitQ5)[1 : 5, 2 ], 3)

