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


setwd("D:/OneDrive/data/covid19/data")

useEstimateN <- TRUE

data <- read.csv(sprintf("covid19_italy_df_%s.csv", Sys.Date()))

data$date <- as.Date(data$date, format="%Y-%m-%d")

# due to lack of tests, confirmed cases are x2 more than reported
# data$confirmed <- data$confirmed *2
# due to asymptomatic cases, recovereds are x6 more than reported
# data$recovered <- data$recovered *6

data$realR <- data$recovered + data$death
data$realI <- data$confirmed - data$realR



#####


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




# params:      N    D   R0    I0
lower <- c(  1  ,   1,   1,    0)
upper <- c( 60E6,  90,  10,  100)

if(!useEstimateN) {
  lower[1]=60E6
  
}

evo <- DEoptim(lifespan, data, lower=lower, upper=upper, control=list(itermax=700, trace=100))
summary(evo)
plot(evo, plot.type="bestvalit", type="l")


best <- evo$optim$bestmem


# solve SIR for this best parameters, for 120 days
m <- solveSIR(best, 160)

# assign a date to t
m$date <- as.Date(m$t, origin=min(data$date))

plot(m$date, m$X1, type="l", col="blue", main="Fitted SIR Model for Italy", xlab="t", ylab="cases",ylim=c(0,best[1]))
lines(m$date, m$X2, type="l", col="red")
lines(m$date, m$X3, type="l", col="green")
lines(m$date, m$X2 + m$X3, type="l", col="black")
points(data$date, data$realI, pch=2)
points(data$date, data$realR, pch=1)
points(data$date, data$death, pch=4)



legend("left", bty="n",cex=0.8,
       legend=c("Fitted S", "Fitted I", "Fitted R", "Fitted I cumulative", "Confirmed Cases", "Recovered + Death","Death"),
       col=c("blue", "red", "green", "black", "black", "black", "black"),
       lty=c(1, 1, 1, 1, 0, 0,0), pch=c(-1, -1, -1, -1, 2, 1, 4))


s<- summary(evo)

N <-  s$optim$bestmem[1] 
D <-  s$optim$bestmem[2] 
R0 <- s$optim$bestmem[3]

gamma <- 1 / D
beta  <- R0 * gamma / N

message(sprintf("beta: %f, gamma: %f, R0: %f", beta,gamma,R0))

legend("right", bty="n",cex=0.8,inset=0.05,
       legend=c(sprintf("Nr: %s",N),  sprintf("R0: %s",R0), sprintf("gm: %e",gamma), sprintf("bt: %e",beta)))




#################
### Fitting a simple SIR model with actual data in order to calculate R0
# https://blog.ephorie.de/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov

startDate <- min(data$date, na.rm = TRUE) # as.Date("2020-02-27")


Infected  <- data %>% filter(date > startDate) %>% pull(realI)
Recovered <- data %>% filter(date > startDate) %>% pull(realR)

Day <- 1:(length(Infected))

if(!useEstimateN) {
  N <- data %>% filter (date == max(data$date, na.rm = TRUE) - 1) %>% pull(max(population, na.rm = TRUE)) # population
} else {
  N <- as.numeric(N)
}




country <- data %>% filter (date == max(data$date, na.rm = TRUE) - 1) %>% pull(country)

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta / N * I * S
    dI <- beta / N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

init <- c(S = N - Infected[1],
          I = Infected[1],
          R = 0)

RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <-
    ode(
      y = init,
      times = Day,
      func = SIR,
      parms = parameters
    )
  Ifit <- out[, 3]
  Rfit <- out[, 4]
  sqrt(mean((Infected - Ifit) ^ 2) + mean((Recovered - Rfit) ^ 2))
}

# RSS <- function(parameters) {
#   names(parameters) <- c("beta", "gamma")
#   out <-
#     ode(
#       y = init,
#       times = Day,
#       func = SIR,
#       parms = parameters
#     )
#   Ifit <- out[, 3]
#   sum((Infected - fit)^2)
# }

Opt <-
  optim(
    c(0.5, 0.5),
    RSS,
    method = "L-BFGS-B",
    lower = c(0, 0),
    upper = c(1, 1)
  ) # optimize with some sensible conditions

message(Opt$message)
## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
##      beta     gamma
## 0.6427585 0.3572415

beta <- Opt_par[1]
gamma <- Opt_par[2]

t <- 1:160 # time in days

fit <-
  data.frame(ode(
    y = init,
    times = t,
    func = SIR,
    parms = Opt_par
  ))
col <- 1:3 # colour



m <- fit %>% mutate(date = startDate+time) %>% mutate(X1 = S, X2= I, X3 = R)



plot(m$date, m$X1, type="l", col="blue", main="Fitted SIR Model for Italy", xlab="t", ylab="cases",ylim=c(0,N))
lines(m$date, m$X2, type="l", col="red")
lines(m$date, m$X3, type="l", col="green")
lines(m$date, m$X2 + m$X3, type="l", col="black")
points(data$date, data$realI, pch=2)
points(data$date, data$realR, pch=1)
points(data$date, data$death, pch=4)



legend("left", bty="n",cex=0.8,
       legend=c("Fitted S", "Fitted I", "Fitted R", "Fitted I cumulative", "Confirmed Cases", "Recovered + Death","Death"),
       col=c("blue", "red", "green", "black", "black", "black", "black"),
       lty=c(1, 1, 1, 1, 0, 0,0), pch=c(-1, -1, -1, -1, 2, 1, 4))



#matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
#matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")

## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
## omitted from logarithmic plot

#points(Day, Infected)
#legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"), lty = 1, lwd = 2, col = col, inset = 0.05)
#title(sprintf("SIR model COVID-19  %s",country), outer = TRUE, line = -2)


#par(old)

R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")
##       R0
## 1.799227

message(sprintf("beta: %f, gamma: %f, R0: %f", beta,gamma,R0))

legend("right", bty="n",cex=0.8,inset=0.05,
       legend=c(sprintf("Nr: %s",N),  sprintf("R0: %s",R0), sprintf("gm: %e",gamma), sprintf("bt: %e",beta)))

###
library(lubridate)
# time in days for predictions
t <- 1:as.integer(today()+60 - ymd(startDate))

# get the fitted values from our SIR model
fitted_cumulative_incidence <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))

Infected <- data %>% filter(date > startDate) %>% pull(realI)

# add a Date column and join the observed incidence data
fitted_cumulative_incidence <- fitted_cumulative_incidence %>% 
  mutate(date = ymd(startDate) + days(t - 1), country = "Italy") %>% 
  left_join(data %>% ungroup() %>% filter(country ==  "Italy") %>% select(date, realI))

# plot the data
fitted_cumulative_incidence  %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = I), colour = "red") +
  geom_line(aes(y = S), colour = "black") + 
  geom_line(aes(y = R), colour = "green") +
  geom_point(aes(y = realI), colour = "orange") + 
  labs(y = "Comulative incidence", title = "COVID-19 fitted vs observed cumulative incidence, Italy", 
       subtitle = "(red=fitted incidence from SIR model, orange=observed incidence)") +
  theme_light()  
