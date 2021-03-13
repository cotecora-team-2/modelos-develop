library(tidyverse)

render_reporte <- function(estado, frac, adapt_delta, rmd_file) {
  print(estado)
  rmarkdown::render(rmd_file,
                    output_file = stringr::str_c(id_07, ext),
                    output_dir = stringr::str_c("reportes/", dir_save),
                    params = list(mi_anp = anp)
  )
}
safely_render <- purrr::safely(render_reporte)

render_reporte(anp = "anp_terrestres_2017_NOMBRE_La_Encrucijada")

params <- tibble(estado = c("NAYARIT", "CHIHUAHUA", "COLIMA", "ZACATECAS"),
       frac = c(0.15, 0.06, 0.15, 0.15),
       adapt_delta = c(0.95, 0.95, 0.95, 0.95))

pwalk(params, safely_render, rmd_file = "validacion/modelo-base/modelo-base-calibracion-inicial.Rmd")
