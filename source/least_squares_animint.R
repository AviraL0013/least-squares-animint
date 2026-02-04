library(animint2)

# setup some messy fake data
set.seed(123)
n <- 15
x <- 1:n
y <- 2 + 0.5 * x + rnorm(n, 0, 2)

# find the actual "perfect" fit
fit <- lm(y ~ x)
true_intercept <- coef(fit)[1]
true_slope <- coef(fit)[2]

# prep a range of slopes to test
nmax <- 50
slopes <- seq(true_slope - 1, true_slope + 1, length.out = nmax)
points_data <- data.frame(x = x, y = y)

# calculate the line positions for the animation
lines_data <- data.frame()
for (i in 1:nmax) {
  lines_data <- rbind(lines_data, data.frame(
    iteration = i, slope = slopes[i], intercept = true_intercept,
    x_start = min(x), x_end = max(x),
    y_start = true_intercept + slopes[i] * min(x),
    y_end = true_intercept + slopes[i] * max(x)
  ))
}

# math for the error curve (rss) and those red dashed lines
rss_data <- data.frame()
residuals_data <- data.frame()
for (i in 1:nmax) {
  y_pred <- true_intercept + slopes[i] * x
  rss_data <- rbind(rss_data, data.frame(iteration = i, slope = slopes[i], rss = sum((y - y_pred)^2)))
  for (j in 1:n) {
    residuals_data <- rbind(residuals_data, data.frame(
      iteration = i, x = x[j], y_actual = y[j], y_pred = y_pred[j]
    ))
  }
}

# plot 1: the main scatter view
p1 <- ggplot() +
  geom_point(data = points_data, aes(x = x, y = y), size = 3) +
  geom_segment(data = lines_data, aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               clickSelects = "iteration", color = "black", size = 1.2) +
  geom_segment(data = lines_data, aes(x = min(x), y = true_intercept + true_slope * min(x),
                                       xend = max(x), yend = true_intercept + true_slope * max(x)),
               color = "gray", size = 1, alpha = 0.5) +
  geom_segment(data = residuals_data, aes(x = x, y = y_actual, xend = x, yend = y_pred),
               showSelected = "iteration", color = "red", linetype = "dashed") +
  theme_bw()

# plot 2: the "bowl" showing the error drop
p2 <- ggplot() +
  geom_line(data = rss_data, aes(x = slope, y = rss), size = 1) +
  geom_point(data = rss_data, aes(x = slope, y = rss), clickSelects = "iteration", size = 3, color = "red") +
  geom_vline(aes(xintercept = true_slope), color = "gray", linetype = "dashed") +
  theme_bw()

# stitch it all together
animint_list <- list(
  scatter = p1, rss = p2,
  time = list(variable = "iteration", ms = 2000),
  duration = list(iteration = 2000)
)

# export it
animint2dir(animint_list, out.dir = "least_squares_animint")