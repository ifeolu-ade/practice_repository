# Adegbohungbe Ifeoluwa
# 29th of October
# ALY 6000

cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) # disables scientific notation for entire R session

#Packages
library("pacman")
library("janitor")
library("tidyverse")
library("lubridate")
library("ggplot2")
library(testthat)

pWin <- 0.65
pLoss <- 1- pWin
Combo <- factorial(7) / (factorial(7-5) * factorial(5))
prob1_result <- (pWin ^ 5) * (pLoss ^ 2) * Combo

ComboZ <- Combo7 <- factorial(7) / (factorial(7-0) * factorial(0))
Combo1 <- Combo6 <- factorial(7) / (factorial(7-1) * factorial(1))
Combo2 <- Combo5 <- factorial(7) / (factorial(7-2) * factorial(2))
Combo3 <- Combo4 <- factorial(7) / (factorial(7-3) * factorial(3))

p0 <- (pWin ^ 0) * (pLoss ^ 7)
p1 <- (pWin ^ 1) * (pLoss ^ 6)
p2 <- (pWin ^ 2) * (pLoss ^ 5)
p3 <- (pWin ^ 3) * (pLoss ^ 4)
p4 <- (pWin ^ 4) * (pLoss ^ 3)
p5 <- (pWin ^ 5) * (pLoss ^ 2)
p6 <- (pWin ^ 6) * (pLoss ^ 1)
p7 <- (pWin ^ 7) * (pLoss ^ 0)

prob2_result<- tibble(wins = 0:7, probability = c(p0*ComboZ,p1*Combo1,p2*Combo2,
                                                  p3*Combo3,p4*Combo4,p5*Combo5,
                                                  p6*Combo6,p7*Combo7))

prob3_result <- 1 - sum(prob2_result$probability[6:8])

prob4_result <- sum(prob2_result$probability[4:6])

prob5_result <- 1 - prob3_result

meanZ <- prob2_result$probability[1]*prob2_result$wins[1]
mean1 <- prob2_result$probability[2]*prob2_result$wins[2]
mean2 <- prob2_result$probability[3]*prob2_result$wins[3]
mean3 <- prob2_result$probability[4]*prob2_result$wins[4]
mean4 <- prob2_result$probability[5]*prob2_result$wins[5]
mean5 <- prob2_result$probability[6]*prob2_result$wins[6]
mean6 <- prob2_result$probability[7]*prob2_result$wins[7]
mean7 <- prob2_result$probability[8]*prob2_result$wins[8]


prob6_result <- sum(meanZ,mean1,mean2,mean3,mean4,mean5,mean6,mean7)
               
prob7_result <- 7*pWin*pLoss

set.seed(10)
random_values <- rnorm(n=1000, mean = prob6_result, sd = sqrt(prob7_result))
prob9_result <- mean(sample(random_values))
prob10_result <- sd(sample(random_values))

################################################################################
e <- exp(1)

lamda <- 7
X <- 6
prob11_result <- ((e ^ -lamda) * (lamda ^ X)) / factorial(X)

lamda <- 7*8
y <- ((e ^ -lamda) * (lamda ^ 1)) / factorial(1)
z <- 0
for (z in 0:40) {
  y <- y + (((e ^ -lamda) * (lamda ^ z)) / factorial(z))
  z <- z + 1
  }

prob12_result <- y

lamda <- 7*8
y <- ((e ^ -lamda) * (lamda ^ 1)) / factorial(1)
z <- 0
for (z in 0:(274/5)) {
  y <- y + (((e ^ -lamda) * (lamda ^ z)) / factorial(z))
  z <- z + 1
}

prob13_result <- 1 - (y ^ 5)
                      
prob14_result <- 1 - ((y ^ 5) * (factorial(5) / (factorial(5-4) * factorial(4))))

set.seed(15)
random_poisson <- rpois(n = 1000, lambda = 7*8)
prob17_result <- mean(random_poisson)
prob18_result <- var(random_poisson)

################################################################################

meanlife <- 2000
sdlife <- 100

X1 <- 1800
X2 <- 2200

Z2 <- (X2 - meanlife) / sdlife
Z2value <- 0.97725
Z1 <- (X1 - meanlife) / sdlife
Z1value <- 0.02275
prob19_result <- (Z2value - Z1value) 

X3 <- 2500
Z3 <- (X3 - meanlife) / sdlife
Z3value <- 0.9999
prob20_result <- 1 - Z3value

Z4value <- 0.1
Z4 <- -1.28
X4 <- (sdlife * Z4) + meanlife
prob21_result <- X4

set.seed(25)
random_normal <- rnorm(n=10000,mean=meanlife, sd=sdlife)
prob23_result <- mean(sample(random_normal))
prob24_result <- sd(sample(random_normal))

set.seed(1)
samples <- 1
z <- 1
for (z in 1:1000) {
  samples[z] <- mean(sample(random_normal,100))
  z = z + 1
}

hist(samples)
prob27_result <- mean(samples)
################################################################################

library("palmerpenguins")
glimpse(penguins)
adelie_flippers <- penguins %>% filter(species == "Adelie") %>% 
  select(flipper_length_mm) %>% drop_na()
hist(adelie_flippers$flipper_length_mm, xlab = "Flipper Length", main = paste("Histogram of Adelie Flippers"), col = "green") 

gentoo_penguins <- penguins %>% filter(species == "Gentoo") %>% 
  select(flipper_length_mm, bill_length_mm) %>% drop_na()
plot(gentoo_penguins$flipper_length_mm, gentoo_penguins$bill_length_mm)

plot(gentoo_penguins$bill_length_mm, gentoo_penguins$flipper_length_mm)
