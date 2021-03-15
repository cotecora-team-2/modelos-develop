library(tidyverse)

## Bajar tiempos de llegada
if(FALSE){
  library(pins)
  library(dotenv)
  dotenv_file <- '.env'
  path <- './datos'
  load_dot_env(dotenv_file)

  board_register_github(name = "mygithub",
                      repo = "cotecora-team-2/quickcountmx-data",
                      branch = 'main', token=Sys.getenv("token"))

  sims <- pin_get("tiempos_simulaciones", board = "mygithub")
  readr::write_rds(sims, paste(path, "simulaciones_completo.rds", sep = "/"))
  rm(sims)
}

## parámetros de reportes
params <- tibble(estado = c("NAYARIT", "CHIHUAHUA", "COLIMA", "ZACATECAS"),
                 frac = c(0.12, 0.06, 0.15, 0.15),
                 adapt_delta = c(0.95, 0.95, 0.95, 0.95),
                 simular = FALSE)

render_reporte <- function(estado, frac, adapt_delta, simular, rmd_file) {
  print(estado)
  print(frac)
  print(simular)
  rmd_name <- basename(rmd_file) %>%
    str_remove(".Rmd")
  html_name <- paste0(rmd_name,"-",estado, "-", frac, ".html")
  print(html_name)
  rmarkdown::render(rmd_file,
                    output_file = html_name,
                    params = list(estado = estado, frac = frac,
                                  adapt_delta = adapt_delta, simular = simular)
  )
}

safely_render <- purrr::safely(render_reporte)

# Generar descripción del modelo
rmarkdown::render("modelo-base-construccion.Rmd")

# Generar checks de inicial
pwalk(params, safely_render,
      rmd_file = "modelo-base-calibracion-inicial.Rmd")

# Generar calibración 2018
pwalk(params, safely_render,
      rmd_file = "modelo-base-calibracion-posterior-2018.Rmd")
