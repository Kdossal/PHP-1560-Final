sim <- function(pop){
  adm_rate <- pop*.0002
  admits <- c()
  daily_total <- c()
  
  for(i in 1:100){
    num_new_admits <- rpois(1, adm_rate)
    admits <- append(admits, rpois(num_new_admits, lambda = 4.6)) # 4.6 is national average LoS
    daily_total <- append(daily_total, sum(admits > 0))
    admits <- admits - 1
  }
  return(c(daily_total))
}


plot(seq(1:100),sim(1000000))