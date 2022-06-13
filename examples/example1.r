# Example 1: estimate the 95% confidence interval of the mean.

library("mosaic")
library("readxl")
library("dplyr")

# Stature for males

# sheet of biometric data for males
table <- read_excel("/home/caballero/Datasets/Anthropometric/20642-FSMA Anthropometric data.xlsx", sheet = "Male")

# visualize the table
# View(table)

# maximum 240 months = 20 years
# minimum 24 months = 2 years

#sample between 2 and 4 years
sample <- filter(table, `Age (months)` <= 48 )

remove(table)

stature <- unlist(sample[3])

remove(sample) 

n <- length(stature)

sample_size <- 50

iterations <- 10000

# bootstrapping

mu_boot <- vector(mode = "list", length = 0)

for (i in 1:iterations) {
  sample <- stature[ sample(n, sample_size, replace = TRUE)  ]
  mu_boot <- append(mu_boot, mean(sample)) 
}

mu_boot <- unlist(mu_boot)

h <- hist(x = mu_boot, breaks = 90)

mu <- mean(mu_boot)

sigma <- sd(mu_boot)

xfit <- seq(min(mu_boot), max(mu_boot), length = 50)

yfit <- dnorm(x = xfit, mean = mu, sd = sigma)

yfit <- yfit * diff(h$mids[1:2]) * length(mu_boot) 

lines(xfit, yfit, col = "red", lwd = 2)

f <- function(mu, sigma, x, bound_p) {
  return ( pnorm(mu+x, mean = mu, sd = sigma) - bound_p)  
}

p <- 0.95

bound_p <- 0.5*(1 + p)

remove(p)

t <- findZeros(f(mu, sigma, t, bound_p) ~ t, near = 10, within = 100, iterate = 5)[[1]]

remove(bound_p)

cat( c("95% confidence interval = [", mu-t, ", ", mu+t,"]")  )

