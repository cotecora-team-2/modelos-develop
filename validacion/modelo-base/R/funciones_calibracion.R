obtener_muestra <- function(num_draw, frac= 0.04, seed = NA){
  sims_1 <- sims_casillas %>% filter(.draw == num_draw)
  muestra_1 <- quickcountmx::select_sample_prop(sims_1, stratum = estrato_df, frac = frac,
                                                seed = seed)
  list(y = muestra_1$y_f, N = length(muestra_1$y_f), stratum = muestra_1$estrato_df,
       n = muestra_1$ln, x = x_f[muestra_1$no_casilla, , drop = FALSE])
}

ajustar_diagnosticos <- function(rep, frac = 0.04, modelo, datos, params,
                                 adapt_delta = 0.9, max_treedepth = 10,
                                 iter_sampling=2000, iter_warmup = 2000){

  datos_muestra <- obtener_muestra(rep, frac = frac, seed = NA)
  datos_1 <- c(datos, datos_muestra)
  print(rep)
  ajuste <- modelo$sample(data = datos_1,
                          seed = 2210,
                          iter_sampling = iter_sampling, iter_warmup = iter_warmup,
                          parallel_chains = 4,
                          adapt_delta = adapt_delta,
                          max_treedepth = max_treedepth,
                          refresh = 0,
                          show_messages = FALSE)


  suppressMessages(diagnostico <- ajuste$cmdstan_diagnose())
  suppressMessages(resumen <- ajuste$summary())

  # diagnosticar parámetros
  sims_tbl <- ajuste$draws(names(params)) %>% as_draws_df() %>% as_tibble()
  sbc_tbl <- sbc_rank(params, sims_tbl)
  tibble(rep = rep, params = list(params), sbc_rank = list(sbc_tbl),
         resumen = list(resumen), diagnosticos = list(diagnostico))
}

sbc_rank <- function(params_tbl, sims_tbl){
  params_nom <- names(params_tbl)
  sims_tbl_larga <- sims_tbl %>%
    filter((row_number() %% 10) == 0) %>% # adelgazar la cadena
    pivot_longer(cols = any_of(params_nom), names_to = "parametro", values_to = "valor")
  params_tbl_larga <- params_tbl %>%
    pivot_longer(cols = any_of(params_nom), names_to = "parametro", values_to = "valor_real")
  sbc_tbl <- sims_tbl_larga %>%
    left_join(params_tbl_larga, by = "parametro") %>%
    group_by(parametro) %>%
    summarise(sbc_rank = mean(valor_real < valor))
  sbc_tbl %>% pivot_wider( names_from = "parametro", values_from ="sbc_rank")
}
