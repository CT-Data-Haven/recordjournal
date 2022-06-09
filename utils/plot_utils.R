library(stylehaven)
library(showtext)
showtext_auto()
showtext_opts(dpi = 170)

font_add_weights("Barlow Semi Condensed", semibold = 500)

base_col <- "#5e3da7"

palx_colors <- palx(base_col, n_shades = 5, plot = F)

qual_pal <- palx_colors$shade03
opinion_pal <- c(palx_colors$shade02[["violet"]], palx_colors$shade03[["violet"]], palx_colors$shade04[["violet"]])

tf_pal <- c("TRUE" = palx_colors[["shade02"]][["gray"]], "FALSE" = palx_colors[["shade05"]][["gray"]])

theme_bar <- function(x, xgrid = FALSE, ygrid = FALSE, ...) {
  camiller::theme_din(base_family = "Barlow Semi Condensed", xgrid = xgrid, ygrid = ygrid) +
    theme(plot.caption.position = "plot",
          strip.text = element_text(family = "Barlow Semi Condensed Semibold", face = "plain"),
          ...)
}
theme_set(theme_bar())
update_geom_defaults("col", list(fill = base_col))
update_geom_defaults("text", list(size = 3.5, fontface = "bold", family = "Barlow Semi Condensed", color = tf_pal[["FALSE"]]))
