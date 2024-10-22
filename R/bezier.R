bezier_curve <- function(x0, y0, x1, y1, curvature, angle) {
  ## convert angle from degrees to radians
  angle_rad <- angle * pi / 180

  ## calculate control point based on midpoint and skew
  xm <- (x0 + x1) / 2
  ym <- (y0 + y1) / 2

  # skew direction vector
  skew_vector_x <- cos(angle_rad)
  skew_vector_y <- sin(angle_rad)

  # control point
  x_ctrl <- xm + curvature * skew_vector_x
  y_ctrl <- ym + curvature * skew_vector_y

  ## calculate bezier curve
  t <- seq(0, 1, length.out = 100)
  x <- (1 - t)^2 * x0 + 2 * (1 - t) * t * x_ctrl + t^2 * x1
  y <- (1 - t)^2 * y0 + 2 * (1 - t) * t * y_ctrl + t^2 * y1

  points <- data.frame(x = x, y = y)

  return(points)
}

curve <- bezier_curve(x0 = 1, y0 = 0, x1 = 1, y1 = 1, curvature = 1, angle = 180)

# plot
ggplot(curve, aes(x = x, y = y)) +
  geom_path() +
  scale_x_continuous(limits = c(0,2)) +
  scale_y_continuous(limits = c(-1.5,1.5)) +
  theme_bw()


