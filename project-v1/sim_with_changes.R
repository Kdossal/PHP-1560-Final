simulation <- function(pop, sex, beds, output){
  
  # Separating Populations into sex groups
  fem_pop <- sex*pop
  male_pop <- (1 - sex)*pop
  
  # Average Number of New Male Admits is .024% of the Male Population
  m_adm_rate <- male_pop*.00024
  # Average Number of New Female Admits is .024% of the Female Population
  f_adm_rate <- fem_pop*.00032
  
  # Tracks daily Number of New Admits 
  new_daily_adm <- c()
  
  # Used to track patients and their changing length of stays
  all_admits <- c()
  
  # Contains all LoS Data
  LoS <- c()
  
  # Daily Total Number of Patients in Hospital
  daily_total <- c()
  
  # Patients Waiting in Line
  queue <- c()
  
  # Daily Length of Queue 
  q_len <- c()
  
  # Used to track wait times
  wait_times <- c()
  n_queue_adm <- 1
  
  for(i in 1:100){
    # Generates random number of new admits (n) given an adm_rate for M/F
    m_new_admits <- rpois(1, m_adm_rate)
    f_new_admits <- rpois(1, f_adm_rate)
    
    # Generates n number of new M/F admits with given LoS Averages 
    m_los <- rpois(m_new_admits, 5) 
    f_los <-  rpois(f_new_admits, 4.3)
    
    new_adm <- c(m_los, f_los)
    
    while (sum(all_admits > 0) < beds & length(queue) > 0){
      # Tracks and Updates all Patients Data
      all_admits <- append(all_admits, queue[1])
      
      # Tracks all patient's LoS
      LoS <- append(LoS, queue[1])
      
      queue <- queue[-1]
      n_queue_adm = n_queue_adm + 1
    }
    
    while (sum(all_admits > 0) < beds & length(new_adm) > 0){
      # Tracks and Updates all Patients Data
      all_admits <- append(all_admits, new_adm[1])
      
      # Tracks all patient's LoS
      LoS <- append(LoS, new_adm[1])
      
      new_adm <- new_adm[-1]
    }
    
    # Add People that did not admitted to the Queue
    if (length(new_adm) > 0){
      queue <- append(queue, new_adm)
      wait_times <- append(wait_times, rep(0, length(new_adm)))
    }
    
    # Updates q_len with number of people currently in queue
    q_len <- append(q_len, length(queue))
    
    # Checks how many people in hospital with days remaining
    daily_total <- append(daily_total, sum(all_admits > 0))
    
    # Simulates Next Day 
    all_admits <- all_admits - 1
    
    # Adds one day to everyone still waiting in queue
    wait_times <- replace(wait_times, 
                          n_queue_adm:length(wait_times), 
                          wait_times[n_queue_adm:length(wait_times)]+1)
  }
  
  if (output == 'Daily Totals'){
    return(
      data.frame(
        days = seq(1:100),
        daily_total,
        q_len
      )
    )}
  else if (output == 'LoS'){return(LoS)}
  else if (output == 'Wait Times'){if (length(wait_times > 0)){return(wait_times)}
                                   else {c(0)}}
}
