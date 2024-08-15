rm(list = ls()) # removes everything in your global environment

gw_rate <- function(site)  {
  if(!site %in% c("mountain", "prarie", "desert", "beach")) {
    warning("site is not included")
  }
  gw_depths <- data.frame(sitename = c("mountain",
                                       "prarie",
                                       "desert",
                                       "beach"),
                          depth = c(32, 41, 63, 2),
                          slope = c(11.2, 0.4, 0.8, 2.6))
  
  site_select <- filter(gw_depths, sitename == site)
  
  transport_rate <- 1.4* site_select$slope + 3.6 * site_select$depth
  
  return(transport_rate)
}

gw_rate("mountain")


rm(list =ls())

logistic_growth <- function(N0, K, r, time){
  Nt <- K / (1 + (K - N0) / N0 * exp(-r * time))
  return(Nt)
}

logistic_growth(N0 = 100, K = 6000, r = 0.27, time = 40)

time_vec <- seq(from = 0, to = 50, by = 0.1)

pop_1 <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec)

pop_1_vec <- vector(mode = "numeric", length = length(time_vec))

for (i in seq_along(time_vec)) {
  population <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec[i])
  pop_1_vec[i] <- population
}


pop_time_1 <- data.frame(time_vec, pop_1_vec)

ggplot(data = pop_time_1, aes(x = time_vec, y = pop_1_vec))+
  geom_line()

r_seq <- seq(from = 0.2, to = 0.4, by = 0.01)
out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_seq))

for (i in seq_along(r_seq)){ # outer loop of growth rates
  for (j in seq_along(time_vec)) { # inner loop of time steps
    population <- logistic_growth(N0 = 100, K = 6000, r = r_seq[i], time = time_vec[j])
    pop_1_vec[i] <- population
    out_matrix[j, i] <- population
  }
}

out_df <- data.frame(out_matrix, time = time_vec)
colnames(out_df) <- c(paste0("growth_rate_", r_seq), "time")

out_df_long <- out_df |>
  pivot_longer(cols = -time, names_to = "growth_rate", values_to = "population_size")

ggplot(data = out_df_long, aes(x = time, y = population_size))+
  geom_line(aes(color = growth_rate), show.legend = FALSE)+
  theme_minimal()
















