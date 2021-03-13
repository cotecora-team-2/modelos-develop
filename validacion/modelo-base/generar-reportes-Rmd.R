library(tidyverse)

params <- tibble(estado = c("NAYARIT", "CHIHUAHUA", "COLIMA", "ZACATECAS"),
                 frac = c(0.12, 0.06, 0.15, 0.15),
                 adapt_delta = c(0.95, 0.95, 0.95, 0.95),
                 simular = c(FALSE, FALSE, FALSE, FALSE))

render_reporte <- function(estado, frac, adapt_delta, simular, rmd_file) {
  print(estado)
  rmd_name <- basename(rmd_file) %>%
    str_remove(".Rmd")
  html_name <- paste0(rmd_name,"-",estado, "-", frac, ".html")
  print(html_name)
  rmarkdown::render(rmd_file,
                    output_file = html_name,
                    params = list(estado = estado, frac = frac,
                                  adapt_delta = adapt_delta)
  )
}

safely_render <- purrr::safely(render_reporte)

# Generar checks de inicial
pwalk(params, safely_render,
      rmd_file = "modelo-base-calibracion-inicial.Rmd")

# Generar calibración 2018
