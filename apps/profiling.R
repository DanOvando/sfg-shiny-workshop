library(profvis)
library(shiny)
profvis({
  data(diamonds, package = "ggplot2")
  
  plot(price ~ carat, data = diamonds)
  m <- lm(price ~ carat, data = diamonds)
  abline(m, col = "red")
})


profvis({
  runApp("apps/app_08.R", display.mode = "normal")
})
