#devtools::install_github("cotecora-team-2/quickcountmx")
library(cmdstanr)
library(posterior)
library(tidyverse)
library(patchwork)
library(quickcountmx)
theme_set(theme_minimal())
cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



datos_ent <- conteo_2018 %>% filter(NOMBRE_ESTADO == estado) %>%
  # usar estratificacion anterior por ahora
  mutate(estrato_df = as.numeric(factor(estrato))) %>%
  select(CLAVE_CASILLA, AMLO, RAC, JAMK, CAND_IND_02, estrato_df,
         ln = LISTA_NOMINAL_CASILLA, componente = .fittedPC1, TIPO_CASILLA.x) %>%
  mutate(in_sample = 0, y_f = 0) %>%
  mutate(ln = ifelse(ln == 0, 1200, ln)) %>%
  mutate(casilla_esp = ifelse(TIPO_CASILLA.x == "S", 1, 0)) %>%
  mutate(no_casilla = row_number())
sim_datos_lista <- list()
x <- datos_ent %>%
  select(ln, componente, casilla_esp) %>%
  as.matrix()
x_f <- scale(x)
sim_datos_lista$x_f <- x_f
sim_datos_lista$N_f <- nrow(x_f)
sim_datos_lista$n_covariates_f <- ncol(x_f)
sim_datos_lista$n_strata_f <- datos_ent$estrato_df %>% unique %>% length()
sim_datos_lista$n_f <- datos_ent$ln
sim_datos_lista$stratum_f <- datos_ent$estrato_df
jsonlite::write_json(sim_datos_lista, "datos/datos_prueba.json")

sim_datos <- jsonlite::read_json("./datos/datos_prueba.json", simplifyVector = TRUE)
parametros <- jsonlite::read_json("./datos/datos_inicial.json", simplifyVector = TRUE)
