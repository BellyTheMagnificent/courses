# Q1 & Q2
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

model1 = lm(y ~ x)
summary(model1)
coef(model1)


# Q3 Hints 
sumCoef<-summary(fit)$coefficients sumCoef[1,1]+c(-1,1)*qt(.975,df=fit$df)*sumCoef[1,2]

# Q3
data(mtcars)
str(mtcars)
model2 = lm(mpg~I(I(wt-mean(wt))), data = mtcars)
summary(model2)
sumCoef<-summary(model2)$coefficients 
## Intercept Coeff
sumCoef[1,1]+c(-1,1)*qt(.975,df=model2$df)*sumCoef[1,2]
## Weight Coeff
sumCoef[2,1]+c(-1,1)*qt(.975,df=model2$df)*sumCoef[2,2]

# Q5 
predict(model1, newdata = data.frame(wt=3), interval="prediction")

# Q6 
## Weight Coeff
(sumCoef[2,1]+c(-1,1)*qt(.975,df=model1$df)*sumCoef[2,2]) * 2


# Q9
model1 = lm(mtcars$mpg)
