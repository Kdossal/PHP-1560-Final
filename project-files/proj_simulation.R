simulation <- function(pop, sex, beds, inc, output){
  #@param: pop, an integer from 25000-1000000 representing the population size
  #@param: sex, an float representing the ratio of female to male in population
  #@param: beds, an integer from 0-2000 representing the number of available beds
  #@param: inc, an string representing the population income quartile (1=lowest, 4=highest)
  #@param: output, an string representing the either length of stay or wait time
  #@returns: ---
  # Updates admission rates and LoS rates based on income range 
  # of the community around a hospital
  if (inc == "Quartile 1 (lowest income)"){inc_adm_rate = .00034
      inc_los = 4.8}
  else if (inc == "Quartile 2"){inc_adm_rate = .0003
      inc_los = 4.6}
  else if (inc == "Quartile 3"){inc_adm_rate = .00026
    inc_los = 4.5}
  else if (inc == "Quartile 4 (highest income)"){inc_adm_rate = .00023
    inc_los = 4.5}
  
  # Separating Populations into sex groups
  fem_pop <- sex*pop
  male_pop <- (1 - sex)*pop
  
  # Number of New Male Admits is the average between .024% for adm. rate for 
  # males and the rate for a specific income bracket
  m_adm_rate <- male_pop*(.00024+inc_adm_rate)/2
  # Number of New Female Admits is the average between .032% for adm. rate for 
  # females and the rate for a specific income bracket
  f_adm_rate <- fem_pop*(.00032+inc_adm_rate)/2
  
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
  n_queue_adm <- 0
  
  # Daily Loss of Not having every bed full or people waiting
  daily_loss <- c()
  
  for(i in 1:100){
    # Updates Number of current patients
    curr_adm <- sum(all_admits > 0)
    
    # Generates random number of new admits (n) given an adm_rate for M/F
    m_new_admits <- rpois(1, m_adm_rate)
    f_new_admits <- rpois(1, f_adm_rate)
    
    # Generates n number of new M/F admits with given LoS Averages 
    # based on sex and income quartile
    m_los <- rpois(m_new_admits, (5+inc_los)/2) 
    f_los <-  rpois(f_new_admits, (4.3+inc_los)/2) 
    
    new_adm <- c(m_los, f_los)
    
    while (curr_adm < beds & length(queue) > 0){
      # Tracks and Updates all Patients Data
      all_admits <- append(all_admits, queue[1])
      curr_adm = curr_adm + 1
      
      # Tracks all patient's LoS
      LoS <- append(LoS, queue[1])
      
      queue <- queue[-1]
      n_queue_adm = n_queue_adm + 1
    }
    
    while (curr_adm < beds & length(new_adm) > 0){
      # Tracks and Updates all Patients Data
      all_admits <- append(all_admits, new_adm[1])
      curr_adm = curr_adm + 1
      
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
    daily_total <- append(daily_total, curr_adm)
    
    # Simulates Next Day 
    all_admits <- all_admits - 1
    
    # Adds one day to everyone still waiting in queue
    wait_times <- ifelse(seq_along(wait_times) > n_queue_adm, wait_times + 1, wait_times)
    
    # Average daily cost of a patient is $11,700 each day
    daily_loss <- append(daily_loss, (beds - curr_adm)*11.7 + length(queue)*11.7)
  }
  
  if (output == 'Daily Totals'){
    return(
      data.frame(
        days = seq(1:100),
        daily_total,
        q_len, 
        daily_loss
      )
    )}
  else if (output == 'LoS'){return(LoS)}
  else if (output == 'Wait Times'){if (length(wait_times > 0)){return(wait_times)}
    else {c(0)}}
}

