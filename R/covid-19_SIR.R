library(RCurl)
library(rlist)
library(lattice)
library(latticeExtra)
library(directlabels)
library(RColorBrewer)
library(readr)
library(dplyr)
library(tidyr)
library(wbstats)
library(data.table)
library(prophet)
library(ggplot2)
library(zoo)
library(deSolve)
library(DEoptim)


### https://github.com/lilywang1988/eSIR
#library(devtools)
#install_github("lilywang1988/eSIR")
library(eSIR)

#https://www.kaggle.com/jordimas/differential-evolution-to-fit-covid-19-sir-model
set.seed(2020)

#
# Runge-Kutta
#
rk4ApproxSys <- function(f, p, t_0, x_0, t_f, h) {
  N <- (t_f - t_0) / h
  t <- numeric(N + 1)
  x <- matrix(nrow=N+1, ncol=length(x_0))
  
  t[1] <- t_0
  x[1,] <- x_0
  
  for (i in 1:N) {
    t[i + 1] <- t[i] + h
    
    k1 <- f(t[i], x[i,], p)
    k2 <- f(t[i] + h / 2, x[i,] + h * k1 / 2, p)
    k3 <- f(t[i] + h / 2, x[i,] + h * k2 / 2, p)
    k4 <- f(t[i + 1], x[i,] + h * k3, p)
    k <- (k1 + 2 * k2 + 2 * k3 + k4) / 6
    
    x[i + 1,] <- x[i,] + h * k
  }
  
  data.frame(t, x)
}

#
# SIR Model
#
sirModel <- function(t, x, p) {
  S <- x[1]
  I <- x[2]
  R <- x[3]
  beta  <- p[1]
  gamma <- p[2]
  
  dS <- -beta * I * S
  dI <- beta * I * S - gamma * I
  dR <- gamma * I
  
  c(dS, dI, dR)
}

#
# Solves a SIR model
#
solveSIR <- function(p, days) {
  # parameters
  N  <- p[1]
  D  <- p[2]
  R0 <- p[3]
  I0 <- p[4]
  gamma <- 1 / D
  beta <- R0 * gamma / N
  
  # independent variable
  t_0 <- 0
  t_f <- days - 1
  
  # initial condition
  I_0 <- I0
  S_0 <- N - I_0
  R_0 <- 0
  
  # solve
  rk4ApproxSys(sirModel, c(beta, gamma), t_0, c(S_0, I_0, R_0), t_f, 1)
}

#
# Solves a SIR model and compares to actual data
#
lifespan <- function(p, regionData) {
  m <- solveSIR(p, nrow(regionData))
  
  # RMSE to evaluate model over actual data
  mseI <- mean((m$X2 - regionData$realI)^2)
  mseR <- mean((m$X3 - regionData$realR)^2)
  
  sqrt(mseI + mseR)
}

setwd("D:/OneDrive/data/covid19/data")

data <- read.csv(sprintf("covid19_italy_df_%s.csv", Sys.Date()))

data$date <- as.Date(data$date, format="%Y-%m-%d")

# due to lack of tests, confirmed cases are x2 more than reported
data$confirmed <- data$confirmed * 2
# due to asymptomatic cases, recovereds are x6 more than reported
data$recovered <- data$recovered * 6

data$realR <- data$recovered + data$death
data$realI <- data$confirmed - data$realR



# params:      N    D   R0    I0
lower <- c(    1,   1,   1,    0)
upper <- c( 60E6,  90,  10,  100)

evo <- DEoptim(lifespan, data, lower=lower, upper=upper, control=list(itermax=700, trace=100))
summary(evo)
plot(evo, plot.type="bestvalit", type="l")


best <- evo$optim$bestmem

# solve SIR for this best parameters, for 120 days
m <- solveSIR(best, 120)

# assign a date to t
m$date <- as.Date(m$t, origin=min(data$date))

plot(m$date, m$X1, type="l", col="blue", main="Fitted SIR Model for Italy", xlab="t", ylab="cases")
lines(m$date, m$X2, type="l", col="red")
lines(m$date, m$X3, type="l", col="green")
lines(m$date, m$X2 + m$X3, type="l", col="black")
points(data$date, data$confirmed, pch=4)
points(data$date, data$realR, pch=1)

legend("left", bty="n",
       legend=c("Fitted S", "Fitted I", "Fitted R", "Fitted I cumulative", "Confirmed Cases", "Recovered + Death"),
       col=c("blue", "red", "green", "black", "black", "black"),
       lty=c(1, 1, 1, 1, 0, 0), pch=c(-1, -1, -1, -1, 4, 1))

