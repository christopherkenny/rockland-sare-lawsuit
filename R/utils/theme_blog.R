theme_blog <- function(...) {
  theme_bw() + 
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = 'bold', size = 14),
      ...
    )
}