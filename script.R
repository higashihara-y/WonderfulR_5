
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
