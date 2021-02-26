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
