# Hints for Question 1

mn <- mean(mtcars$mpg)
s <- sd(mtcars$mpg)
z <- qnorm(.05)
mu0 <- mn - z * s / sqrt(nrow(mtcars))

## Q1
mn <- 12
s <- 4
z <- qnorm(.05)
mu0 <- mn - z * s / sqrt(100)
mu1 <- mn + z * s / sqrt(100)

## Hints for Question 2

m4 <- mtcars$mpg[mtcars$cyl == 4]
m6 <- mtcars$mpg[mtcars$cyl == 6]
p <- t.test(m4, m6, paired = FALSE, alternative="two.sided", var.equal=FALSE)$p.value

## Q2

set1 = c(140,138,150,148,135)
set2 = c(132,135,151,146,130)
p <- t.test(set1, set2, paired = TRUE, alternative="two.sided", var.equal=FALSE)$p.value


## Q3 Hints
mn <- 3.0
s <- 1.1
z <- qnorm(.025)
mu0 <- mn - z * s / sqrt(100)
mu1 <- mn + z * s / sqrt(100)

## Q3 - T Test
mn <- 1100
s <- 30
z <- qt(0.025, 8, lower.tail=TRUE)
z = 2.31
mu0 <- mn - z * s / sqrt(9)
mu1 <- mn + z * s / sqrt(9)

## Q4 Hints
ans <- round(pbinom(54, prob = .5, size = 100, lower.tail = FALSE),4)

## Q4
ans <- round(pbinom(2, prob = .5, size = 4, lower.tail = FALSE),4)

## Q5
ans <- round(pbinom(10, prob = 1/100, size = 1787, lower.tail = TRUE),4)

## Q6
power.t.test(n = 9, delta = -4, sd = se, type = "two.sample", alt = "two")$power


## Q8 Hints
power <- pnorm(10 + qnorm(.95) * .4, mean = 11, sd = .4, lower.tail = FALSE)

pnorm(100, mean = 0.01, sd = 0.04, lower.tail=TRUE)
## Q8

power.t.test(n = 100,  sd = 0.04, delta=0.01, type = "one.sample", alternative = "one")$power

## Q9
power.t.test(power = 0.9, delta = 0.01, sd = 0.04,sig.level = 0.05, type = "one.sample", alt = "one.sided")$n

## Q11 
m1 <- 44; m2 <- 42.04
n1 <- n2 <- 288
s <- 12
se <- s * sqrt(1 / n1 + 1 / n2)
ts <- (m2 - m1) / se
pv <- 2 * pnorm(-abs(ts))
