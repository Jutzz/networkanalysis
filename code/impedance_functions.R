library(ggplot2)
library(dplyr)
library(extrafont)

negexp <- function(C) exp(-0.054 * C)
negexp_norm <- function(C) exp(-0.001 * C^2)
negexp_sqrt <- function(C) exp(-0.3 * sqrt(C))
negexp_gener <- function(C) exp(-0.002 * (C^1.8))
power <- function(C) 3*C^-1.835
tanner <- function(C) (C^-0.63) * exp(-0.005 * C)
lognorm <- function(C) exp(-0.17 * (log(C)^2))
logistic <- function(C) 1-1/(1+exp(-0.3*(C-30)))


step <- data.frame(y = c(1,0.8,0.6,0.4,0.2,0), x = c(0,10,20,30,40,50))
cumulative <- data.frame(y = c(1,0,0), x = c(0,30,60))

P <- ggplot(data.frame(C = c(0, 60)), aes(x = C)) +
  geom_function(aes(color = "Exponential"), fun = negexp, linewidth = 1.5) +
  geom_function(aes(color = "Exponential-Normal"), fun = negexp_norm, linewidth = 1.5) +
  geom_function(aes(color = "Exponential-Quadrat"), fun = negexp_sqrt, linewidth = 1.5) +
  geom_function(aes(color = "Generalisiert"), fun = negexp_gener, linewidth = 1.5) +
  geom_function(aes(color = "Power"), fun = power, linewidth = 1.5) +
  geom_function(aes(color = "Tanner"), fun = tanner, linewidth = 1.5) +
  geom_function(aes(color = "Logarithmisch"), fun = lognorm, linewidth = 1.5, xlim = c(1,60)) +
  geom_function(aes(color = "Logistisch"), fun = logistic, linewidth = 1.5) +
  geom_step(data = step, aes(color = "Abgestuft", x = x, y =y), direction = "hv", linewidth = 1.5) +
  geom_step(data = cumulative, aes(color = "Kumulativ", x = x, y =y), direction = "hv", linewidth = 1.5) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Wegaufwand (C)",
    y = "Impedanz",
    color = "Impedanzfunktion"
  ) +
  theme(legend.position = "bottom", text = element_text(family = windowsFont("Source Sans 3")))
  
ggsave(plot = P)
