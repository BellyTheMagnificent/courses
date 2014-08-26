# Q1

x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
sum(w * (x - 0.3)^2)
sum(w * (x - 0.0025)^2)
sum(w * (x - 0.1471)^2)
sum(w * (x - 0.1077)^2)


# Q2 Hints
lm(I(child-mean(child))~I(parent-mean(parent))-1,data=galton)

# Q2 
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

# Centralize
lm(I(y-mean(y))~I(x-mean(x))-1)
# uncentralize
lm(I(y~I(x)-1))

# Q3
data(mtcars)
str(mtcars)
lm(mpg ~ wt, data = mtcars)

# Q4 Hints
y<-galton$child
x<-galton$parent 
beta1<-cor(y,x)* sd(y)/sd(x) 
beta0<-mean(y)-beta1*mean(x) r
bind(c(beta0,beta1),coef(lm(y~x)))

# Q4
beta1<- 0.5 * 1/0.5
beta0<-mean(y)-beta1*mean(x) 
rbind(c(beta0,beta1),coef(lm(y~x)))

# Q6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
s = sd(x)
sum((x-8.86)/sd(x))
sum((x-8.58)/sd(x))
sum((x-9.31)/sd(x))
sum((x-(-0.9719))/sd(x))

# Q7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y~x)

# Q9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mu = 0.573
mu = 0.8
mu = 0.36
mu = 0.44
sum((mu - x)^2)
