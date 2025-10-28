#' Generate a Quadratic Spline Signal with Additive Noise
#'
#' @param result List output from `generate_Qsplines`, containing vectors `p`, `v`, and `a`
#'   for the position, velocity, and acceleration of each segment at its start.
#' @param segments Numeric vector of segment lengths used to generate the spline.
#' @param noise_sd Standard deviation of additive Gaussian noise (default 0).
#'
#' @return A numeric vector representing the continuous spline signal with noise.
#' @examples
#' segments <- c(30, 20, 40)
#' accelerations <- c(1,-1,1)
#' result <- generate_Qsplines(segments, accelerations)
#' signal <- generate_Qspline_signal(result, segments, noise_sd = 0.01)
#' plot(signal, type = "l")
generate_Qspline_signal <- function(result, segments, noise_sd = 1)
{
  x_full <- c()
  p_full <- c()
  cum_len <- c()

  for (i in seq_along(segments))
  {
    seg_len <- segments[i]
    t <- seq(0, seg_len-1, length.out = seg_len)

    # Quadratic interpolation within segment
    p_seg <- result$p[i] + result$v[i] * t + 0.5 * result$a[i] * t^2

    x_full <- c(x_full, cum_len + t)
    p_full <- c(p_full, p_seg)
    cum_len <- cum_len + seg_len
  }

  noise <- rnorm(length(p_full), mean = 0, sd = noise_sd)
  p_full <- p_full + noise

  return(p_full)
}
