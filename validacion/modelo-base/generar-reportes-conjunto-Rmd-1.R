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
                 frac = c(0.20, 0.07, 0.20, 0.15),
                 adapt_delta = c(0.98, 0.98, 0.98, 0.98),
                 simular = TRUE, n_reps = 100,
                 prop_observado = 0.80)

#params <- filter(params, estado %in% c("COLIMA", "ZACATECAS"))

render_reporte <- function(estado, frac, adapt_delta,
                           simular, n_reps, prop_observado, rmd_file) {
  print(estado)
  print(frac)
  print(simular)
  print(prop_observado)
  rmd_name <- basename(rmd_file) %>%
    str_remove(".Rmd")
  html_name <- paste0(rmd_name,"-",estado, "-", prop_observado, ".html")
  print(html_name)
  rmarkdown::render(rmd_file,
                    output_file = html_name,
                    params = list(estado = estado, frac = frac,
                                  adapt_delta = adapt_delta, simular = simular,
                                  n_reps = n_reps,
                                  prop_observado = prop_observado)
  )
}

safely_render <- purrr::safely(render_reporte)

# Generar calibración 2018
pwalk(params, render_reporte,
      rmd_file = "validacion/modelo-base/modelo-conjunto-calibracion-posterior-2018.Rmd")

# Generar calibración censura 2018
