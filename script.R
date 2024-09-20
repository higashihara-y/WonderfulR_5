
# section1 ----------------------------------------------------------------

library(tidyverse)
library(dplyr)

x1 <- c(63, 56, 51, 50, 42)
y1 <- c(63, 66, 61, 60, 52)
df1 <- tibble(x1, y1)
df1
df1 |>
  summarise(
    mean_x1 = across(x1, mean),
    sd_x1 = across(x1, sd),
    mean_y1 = across(y1, mean),
    sd_y1 = across(y1, sd))

output <- tibble(
  mean = rep(NA, 2),
  sd = rep(NA, 2))

for(j in 1:2) {
  output[j, 1] <- mean(as_vector(df1[, j]))
  output[j ,2] <- sd(as_vector(df1[, j]))
}

output



# section2 ----------------------------------------------------------------

library(haven)

rm(list = ls())

data02 <- read_csv("causality-main/data02.csv")
glimpse(data02)
summary(data02)

attach(data02)
y1t - y0t
y3 - x1
y1 - y0

mean(y1t) - mean(y0t)
mean(y3) - mean (x1)
m1 <- mean(y1, na.rm = TRUE)
m0 <- mean(y0, na.rm = TRUE)
m1 - m0

mt1 <- mean(y1t[t1 == 1])
mt0 <- mean(y0t[t1 == 1])
mt1 - mt0

set.seed(1)
r0 <- runif(20, 0, 1)
r1 <- round(r0)
y2 <- NULL
y2[r1 == 1] <- y1t[r1 == 1]
y2[r1 == 0] <- y0t[r1 == 0]

mr1 <- mean(y2[r1 == 1])
mr0 <- mean(y2[r1 == 0])
data02
mr1 - mr0

t.test(y2[r1 == 1], y2[r1 == 0], var.equal = TRUE)


rm(list = ls())
data03 <- read_csv("causality-main/data03.csv")
summary(data03)

mean(data03$y3[data03$t1 == 1]) - mean(data03$y3[data03$t1 == 0])
mean(data03$y1t) - mean(data03$y0t) 


model1 <- lm(y3 ~ x1 + t1)
summary(model1)
confint(model1, level = 0.95)

glimpse(data03)
data03 |> 
  ggplot(aes(x = x1, y = y3, group = t1)) +
  geom_point(aes(shape = factor(t1)),
             size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(limits = c(60, 100)) +
  scale_y_continuous(limits = c(60, 100))



# section4 ----------------------------------------------------------------

rm(list = lst())
N1 <- 4; n1 <- 2
x1 <- c(165, 166, 171, 180)
mu <- mean(x1)
hensa <- x1 - mu
hensa2 <- hensa^2
sigma2 <- sum(hensa2) / N1
sigma <- sqrt(sigma2)

xs <- combn(x1, n1)
xbars <- apply(xs, 2, mean)b
mean(xbars)
hensab <- xbars - mu
hensa2b <- hensab^2
sigma2b <- sum(hensa2b) / 6
sigmab <- sqrt(sigma2b)
sigma
(sigma / sqrt(2)) * sqrt((4 - 2) / (4 - 1))

se0 <- sigma / sqrt(n1)
correct <- sqrt((N1 - n1) / (N1 - 1))
se1 <- se0 * correct

qt(0.025, 80 - 1, lower.tail = FALSE)
qt(0.975, 80 - 1)


data04 <- read.csv("causality-main/data04.csv")
summary(data04)
n1 <- nrow(data04)
diff <- data04$y1t - data04$y0t
m1 <- mean(diff)
s1 <- sd(diff)
talpha <- qt(0.025, n1 - 1, lower.tail = FALSE)
m1 + talpha * s1 / sqrt(n1)
m1 - talpha * s1 / sqrt(n1)
t.test(diff)

y0obs <- na.omit(data04$y0)
y1obs <- na.omit(data04$y1)
n0 <- length(y0obs)
n1 <- length(y1obs)
s0 <- sd(y0obs)
s1 <- sd(y1obs)
num <- (s1^2/n1 + s0^2/n0)^2
denom <- ((s1^2/n1)^2)/(n1-1) + ((s0^2/n0)^2)/(n0-1)
df1 <- num / denom
xbar <- mean(y1obs) - mean(y0obs)
se1 <- sqrt((s1^2/n1) + (s0^2/n0))
talpha <- qt(0.025, df1, lower.tail = F)
xbar + talpha * se1
xbar - talpha * se1
t.test(y1obs, y0obs, var.equal = FALSE)


# section5 ----------------------------------------------------------------

rm(list = ls())
y1 <- c(40, 20, 50, 10)
x1 <- c(5, 1, 3, 2)
yhat1 <- 11.143 + 6.857 * x1
e1 <- y1 - yhat1
yhat2 <- 10.909 * x1
e2 <- y1 - yhat2
sum(e1)
sum(e2)
e1b <- e1^2
e2b <- e2^2
sum(e1b)
sum(e2b)

xbar <- mean(x1)
ybar <- mean(y1)
hensax <- x1 - xbar
hensay <- y1 - ybar
hensaxy <- hensax * hensay
num <- sum(hensaxy)
hensax2 <- hensax^2
denom <- sum(hensax2)
b1 <- num / denom
b0 <- ybar - b1 * xbar

model1 <- lm(y1 ~ x1)
summary(model1)
b1b <- ybar / xbar

rm(list = ls())
y1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
x1 <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
mean(y1)
model2 <- lm(y1 ~ x1)
summary(model2)
-1 + 3 * 1
-1 + 3 * 2
-1 + 3 * 3
plot(x1, y1)
abline(model2)

rm(list = ls())
x1 <- c(5, 1, 3, 2)
y1 <- c(40, 20, 50, 10)
model3 <- lm(y1 ~ x1)
bOLS <- summary(model3)$coefficient[2, 1]
ussOLS <- sum(resid(model3)^2)

b1 <- NULL; uss <- NULL
set.seed(1)
for(i in 1:10000) {
  a1 <- 11.14286
  b1[i] <- runif(1, -50, 100)
  yhat <- a1 + b1[i] * x1
  uss[i] <- sum((y1 - yhat)^2)
}

summary(uss)
plot(b1, uss, col = 7, cex = 0.1, pch = 20)
abline(v = bOLS, lty = 2)
abline(h = ussOLS)


# section6 ----------------------------------------------------------------

rm(list = ls())
data06 <- read_csv("causality-main/data06.csv")
data06
n1 <- nrow(data06)

hensa <- data06$y1 - mean(data06$y1)
hensa2 <- hensa^2
tss <- sum(hensa2)
tss / (n1 - 1)
var(data06$y1)

model1 <- lm(data06$y1 ~ data06$x1)
summary(model1)
confint(model1, level = 0.95)
yhat1 <- predict(model1)
yhat2 <- (yhat1 - mean(data06$y1))^2
ess <- sum(yhat2)
e1 <- resid(model1)
e2 <- e1^2
uss <- sum(e2)
1- uss / tss
ls(summary(model1))
summary(model1)$r.squared

model1 <- lm(data06$x1 ~ data06$x2)
ex1 <- resid(model1)
model2 <- lm(data06$y1 ~ ex1)
model3 <- lm(data06$y1 ~ data06$x1 + data06$x2)
summary(model2)
summary(model3)
confint(model3, level = 0.95)

rm(list = ls())
data03 <- read_csv("causality-main/data03.csv")
model1 <- lm(t1 ~ x1, data = data03)
et1 <- resid(model1)
model2 <- lm(y3 ~ et1, data = data03)
summary(model2)
lm(y3 ~ x1 + t1, data = data03)










