library(MASS)
data(shuttle)
str(shuttle)
head(shuttle)
data = shuttle
data$auto = as.numeric(shuttle$use=="auto")
data$headwind <- as.numeric(shuttle$wind=="head")

fit.glm1 = glm(use ~ wind, data = shuttle, family=binomial)
exp(fit.glm1$coefficients)
fit.glm2 = glm(use ~ wind * magn, data = shuttle, family=binomial)

#Q2
landing <- glm(use ~ wind + magn, family="binomial", data=shuttle)
summary(landing)
landing$coef
exp(landing$coef)
exp(landing$coef[1])/exp(landing$coef[1] + landing$coef[2])

# Q3 Solution obtain from course forum
shuttle$auto <- as.numeric(shuttle$use=="auto")
shuttle$auto <- as.numeric(shuttle$use=="auto")
fit2 <- glm(auto ~ wind + magn -1, binomial, shuttle)
fit4 <- glm(1-auto ~ wind + magn -1, binomial, shuttle)
fit2$coefficients
fit4$coefficients

#Q4
data(InsectSprays)

## With Intercept
fit.glm.p0 <- glm(count ~ spray , data = InsectSprays, family = "poisson")
exp(fit.glm.p0$coeff[1])/exp(fit.glm.p0$coeff[1]+fit.glm.p0$coeff[2])
## Remove intercept
fit.glm.p1 <- glm(count ~ spray -1, data = InsectSprays, family = "poisson")
summary(fit.glm.p1)
exp(fit.glm.p1$coeff[1])/exp(fit.glm.p1$coeff[2])

# Q5
## Using hits data set from swirl()
hits$date = as.integer(hits[,'date'])
glm(simplystats ~ date , family = poisson, data = hits)
glm(simplystats ~ date + offset(visits) , family = poisson, data = hits)
glm(simplystats ~ date + offset(log(10)+visits), family = poisson, data = hits)

#Q6 
## formula is obtain from Coursera course assignment forum
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
lhs <- function(x) ifelse(x < 0,0-x,0) # basis function 1 (lhs = left hockey stick)
rhs <- function(x) ifelse(x > 0,x-0,0) # basis function 2 (rhs = right hockey stick)
gb <- lm(y ~ lhs(x) + rhs(x))
x <- seq(-5,5,by=1)
py <- gb$coef[1]+gb$coef[2]*lhs(x)+gb$coef[3]*rhs(x)
plot(x,py)
