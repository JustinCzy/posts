my_theme <- theme_minimal() +
  theme(plot.title = element_text(hjust = .5), plot.caption = element_text(face = 3), legend.position = "bottom")
gg_plt <- function (comp) {
  ggplot(comp, aes(x = date, y = value, color = asset)) +
    geom_line() +
    scale_x_date(name = NULL, date_labels = "%Y") +
    my_theme
}
#Color for labels
lab_col <- wesanderson::wes_palettes$GrandBudapest1[3]
path_col <- "grey20"
#Brown GGPLOT theme
back_col <- "cornsilk"
back_col_2 <- "cornsilk2"
line_col <- "burlywood3"
text_col <- "grey20"
brn_theme <-  theme(
  plot.background = element_rect(fill = back_col, color = line_col, linetype = "solid"),
  plot.title = element_text(hjust = .5, color = text_col), 
  plot.subtitle = element_text(hjust = .5, color = text_col), 
  plot.caption = element_text(face = 3, color = text_col), 
  panel.background = element_rect(fill = back_col, color = line_col, linetype = "solid"),
  panel.grid = element_line(color = line_col, linetype = "solid", size = .5),
  strip.background = element_rect(fill = back_col_2),
  axis.line = element_line(color = line_col),
  axis.text = element_text(color = text_col),
  axis.title = element_text(color = text_col),
  legend.background = element_rect(fill = back_col),
  legend.key = element_rect(fill = back_col),
  legend.text = element_text(color = text_col),
  legend.title = element_text(color = text_col),
  legend.position = c(.005, .91), 
  legend.justification = c("left", "top"))
