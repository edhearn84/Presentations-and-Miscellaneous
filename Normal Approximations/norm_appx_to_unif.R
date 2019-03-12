
#################################
# Normal approximation to uniform
#################################

library(tidyverse)

unif_draws = function(nums , sims = 1000){

  unif_draws = array(NA , c(nums , sims))

  for ( i in 1:nrow(unif_draws) ) {
    unif_draws[i,] = runif(sims , min = 0 , max = i)
  }
  return( tibble( number = 1:6 , n = apply(unif_draws,1,mean) ) )
}

unifs = lapply( rep(6 , 1000) , function(x){ unif_draws(x) } )

all_draws_df = bind_rows( unifs , .id = "sim_num" )

all_draws_df %>%
  ggplot( aes(x = n) ) +
  geom_histogram() +
  facet_wrap(~ number , scales = "free_x") +
  theme_bw()
