library(stylehaven)
library(showtext)
showtext_auto()
showtext_opts(dpi = 170)

font_add_weights("Barlow Semi Condensed", semibold = 500)

base_col <- "#1F449C"

palx_colors <- palx(base_col, n_shades = 5, plot = F)

qual_pal <- palx_colors$shade03

cb_pal <- c("#F05039", "#E57A77", "#EEBAB4", "#1F449C", "#3D65A5", "#7CA1CC", "#A8B6CC", "#FFC132", "#FFD97F", "#FFF0CB")

gradient_pal <- c("#1F449C", "#4161A8", "#647DB4", "#869AC0", "#A8B6CC")

tf_pal <- c("TRUE" = palx_colors[["shade02"]][["gray"]], "FALSE" = palx_colors[["shade05"]][["gray"]])


theme_bar <- function(x, xgrid = FALSE, ygrid = FALSE, ...) {
  camiller::theme_din(base_family = "Barlow Semi Condensed", xgrid = xgrid, ygrid = ygrid) +
    theme(plot.caption.position = "plot",
          strip.text = element_text(family = "Barlow Semi Condensed Semibold", face = "plain"),
          axis.text = element_text(family = "Barlow Semi Condensed", color = "black"),
          ...)
}
theme_set(theme_bar())
update_geom_defaults("col", list(fill = base_col))
update_geom_defaults("text", list(size = 4.5, fontface = "bold", family = "Barlow Semi Condensed", color = "black"))
