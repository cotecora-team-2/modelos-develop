histogramas_estaturas <- function(sims, sim_data, k = 10){
  draws_df <- as_draws_df(sims$draws()) %>%
    nest(y_1 = contains("y_1"), y_2 = contains("y_2")) %>%
    mutate(y_1 = map(y_1, as.numeric), y_2 = map(y_2, as.numeric)) %>%
    filter(.draw <= k) %>%
    select(.draw, y_1, y_2) %>%
    pivot_longer(cols = any_of(c("y_1", "y_2")), names_to = "tipo", values_to = "estatura") %>%
    unnest(cols = estatura)
  ggplot(draws_df, aes(x = estatura, fill = tipo)) +
    geom_histogram() + facet_wrap(~.draw)
}

ajustar_diagnosticos <- function(rep, modelo, datos, params,
                                 iter_sampling=2000, iter_warmup = 2000){

  ajuste <- modelo$sample(data = datos,
                          seed = 2210,
                          iter_sampling = iter_sampling, iter_warmup = iter_warmup,
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
