## code to prepare `hex_logo` dataset goes here

# Latex code:
# R_{\;\;ci}^{ic}

# svg generated with https://viereck.ch/latex-to-svg/
# hex plot and svg text merged with inkscape

library(hexSticker)
library(ggplot2)

bg <- "white"

plot <-
  ggplot() +
  theme_void() +
  theme(legend.position = "none")

plot

sticker(
  plot,
  s_x = 1,
  s_y = 1,
  s_width = 1.6,
  s_height = 1.6,
  h_fill = bg,
  h_color = "black",
  package = "",
  filename = "ricci.svg"
)
