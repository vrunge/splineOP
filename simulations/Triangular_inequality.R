

library(splineOP)

n <- 200
# Example with 5 segments and given accelerations
K <- 2
segments <- c(n/2,n/2)
accelerations <- rnorm(K)
result <- generate_Qsplines(segments, accelerations, max1 = TRUE)
#'
# Inspect results
result$p  # positions
result$v  # velocities
result$a  # accelerations


sdNoise <- 0.05
signal <- generate_Qspline_signal(result, segments, noise_sd = sdNoise)


######################################


# Plot positions along the spline

s <- 60
t <- 120
u <- n

nbState <- 5

m <- 20
# change if your sampling interval is not 1
tvec <- 0:(m-1)

# extract the segment of the signal
y <- signal[s + tvec]

# fit quadratic y = a + b*t + c*t^2
fit <- lm(y ~ tvec + I(tvec^2))

# extract initial speed at t = 0 (slope at start)
v_s <- coef(fit)["tvec"]

v_s


M <- sapply(1:n, function(t)
  c(signal[t], rnorm(nbState-1, signal[t], sdNoise))
)


#############

fit_quadratic <- function(s, t, y_s, y_t, v_s)
{
  Delta <- t - s
  d <- (y_t - y_s - v_s * Delta) / (Delta^2)
  b <- v_s - 2 * d * s
  a <- y_s - b * s - d * s^2
  list(a = a, b = b, d = d,
       q = function(u) a + b*u + d*u^2)
}

#############

y_s <- M[sample(nbState,1),s]
y_u <- M[sample(nbState,1),u]
y_s <- M[1,s]
y_t <- M[1,t]
y_u <- M[1,u]

Qst <- (fit_quadratic(s, t, y_s, y_t, v_s))$q
Qtu <- fit_quadratic(t, u, y_t, y_u, 2*(y_t - y_s)/(t-s)- v_s)$q
Qsu <- fit_quadratic(s, u, y_s, y_u, v_s)$q

yCC <- c(Qst(s:(t-1)),Qtu(t:u))
yC <- Qsu(s:u)
y_signal <- signal[s:u]   # extract the part of the signal corresponding to s:u

# compute y-limits to include all curves and points
ymin <- min(yCC, yC, y_signal)
ymax <- max(yCC, yC, y_signal)

# base plot with first curve


plot(signal, col = "blue", pch = 16, ylim = c(ymin,ymax))
abline(v = s, col = "blue")
abline(v = n/2, col = "blue")
abline(v = t, col = "orange", lwd = 2)

lines(s:u, yC, col = "red", lwd = 3)
lines(s:u, yCC, type = "l", col = "orange", lwd = 3)



sum(yCC-signal[s:u])^2
sum(yC-signal[s:u])^2
sum(yCC-signal[s:u])^2 <= sum(yC-signal[s:u])^2




##############################################################################

y_s <- M[sample(nbState,1),s]
y_u <- M[sample(nbState,1),u]

comptage <- 0

for(i in 1:nbState)
{
  y_t <- M[i,t]
  Qst <- (fit_quadratic(s, t, y_s, y_t, v_s))$q
  Qtu <- fit_quadratic(t, u, y_t, y_u, 2*(y_t - y_s)/(t-s)- v_s)$q
  Qsu <- fit_quadratic(s, u, y_s, y_u, v_s)$q

  yCC <- c(Qst(s:(t-1)),Qtu(t:u))
  yC <- Qsu(s:u)
  y_signal <- signal[s:u]   # extract the part of the signal corresponding to s:u

  # compute y-limits to include all curves and points
  ymin <- min(yCC, yC, y_signal)
  ymax <- max(yCC, yC, y_signal)

  # base plot with first curve


  plot(signal, col = "blue", pch = 16, ylim = c(ymin,ymax))
  abline(v = s, col = "blue")
  abline(v = n/2, col = "blue")
  abline(v = t, col = "orange", lwd = 2)

  lines(s:u, yC, col = "red", lwd = 3)
  lines(s:u, yCC, type = "l", col = "orange", lwd = 3)



  sum(yCC-signal[s:u])^2
  sum(yC-signal[s:u])^2
  comptage <- comptage + (sum((yCC-signal[s:u])^2) <= sum((yC-signal[s:u])^2))
}

comptage
