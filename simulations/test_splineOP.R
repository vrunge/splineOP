

library(splineOP)


# Example with 5 segments and given accelerations
K <- 2
segments <- generate_segment_lengths(400, K,alpha = rep(100,K))
accelerations <- rnorm(K)
result <- generate_Qsplines(segments, accelerations, max1 = TRUE)
#'
# Inspect results
result$p  # positions
result$v  # velocities
result$a  # accelerations
#'
# Plot positions along the spline

plot_Qspline(result, segments)

signal <- generate_Qspline_signal(result, segments, noise_sd = 0.005)
plot(signal, type = "l")



#


# Example with 5 segments and given accelerations
K <- 3
segments <- generate_segment_lengths(400, K,alpha = rep(100,K))
accelerations <- rnorm(K)
result <- generate_Qsplines(segments, accelerations, max1 = TRUE)
#'
# Inspect results
result$p  # positions
result$v  # velocities
result$a  # accelerations
#'
# Plot positions along the spline

plot_Qspline(result, segments)

signal <- generate_Qspline_signal(result, segments, noise_sd = 0.005)
plot(signal, type = "l")


cumsum(segments)
splineOP_Rcpp(signal, beta = 200, S = 12, data_var = 0.01)











# Example with 5 segments and given accelerations
n <- 10^3
K <- 2
segments <- generate_segment_lengths(n, K,alpha = rep(100,K))
accelerations <- c(-1,1)
result <- generate_Qsplines(segments, accelerations, max1 = TRUE)

plot_Qspline(result, segments)

signal <- generate_Qspline_signal(result, segments, noise_sd = 0.1)
plot(signal, type = "l")


length(signal)
system.time(res <- splineOP_Rcpp(signal, beta = 2000, S = 5, data_var = 0.1))[[1]]
res





###########


# Example with 5 segments and given accelerations
n <- 10^5
K <- 2
segments <- generate_segment_lengths(n, K,alpha = rep(100,K))
accelerations <- c(-1,1)
result <- generate_Qsplines(segments, accelerations, max1 = TRUE)

plot_Qspline(result, segments)

signal <- generate_Qspline_signal(result, segments, noise_sd = 0.1)
plot(signal, type = "l")


length(signal)
system.time(res <- splineOP_Rcpp(signal, beta = 2000, S = 5, data_var = 0.1))[[1]]
res






### Time complexity test for splineOP_Rcpp ###

# Define test function
test_time_complexity <- function(ns = c(1:10 * 10^3), K = 2, beta = 2000, S = 5, data_var = 0.1) {
  times <- numeric(length(ns))

  for (i in seq_along(ns)) {
    n <- ns[i]
    cat("\n--- Running test with n =", n, "---\n")

    # Generate data
    segments <- generate_segment_lengths(n, K, alpha = rep(100, K))
    accelerations <- c(-1, 1)
    result <- generate_Qsplines(segments, accelerations, max1 = TRUE)
    signal <- generate_Qspline_signal(result, segments, noise_sd = sqrt(data_var))

    # Measure runtime
    runtime <- system.time({
      res <- splineOP_Rcpp(signal, beta = beta, S = S, data_var = data_var)
    })[["elapsed"]]

    cat("Time (s):", runtime, "\n")
    times[i] <- runtime
  }

  data.frame(n = ns, time_sec = times)
}

# Run experiment
set.seed(123)
results <- test_time_complexity()

# Display results
print(results)

# Plot scaling behavior
plot(results$n, results$time_sec, type = "b", log = "xy",
     xlab = "Signal length (n, log scale)",
     ylab = "Execution time (s, log scale)",
     main = "Empirical Time Complexity of splineOP_Rcpp")
abline(lm(log(time_sec) ~ log(n), data = results), col = "red", lwd = 2)

# Estimate complexity exponent
complexity <- coef(lm(log(time_sec) ~ log(n), data = results))[2]
cat("\nEstimated time complexity exponent:", round(complexity, 2), "\n")

