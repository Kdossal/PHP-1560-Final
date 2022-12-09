simulation <- function(pop, sex, output){
  
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
  
  for(i in 1:100){
    # Generates random number of new admits (n) given an adm_rate for M/F
    m_new_admits <- rpois(1, m_adm_rate)
    f_new_admits <- rpois(1, f_adm_rate)
    
    # Generates n number of new M/F admits with given LoS Averages 
    m_los <- rpois(m_new_admits, 5) 
    f_los <-  rpois(f_new_admits, 4.3)
    
    new_adm <- c(m_los, f_los)
    
    # Tracks Number of New Daily Admits
    new_daily_adm <- append(new_daily_adm, length(new_adm))
    # Tracks and Updates all Patients Data
    all_admits <- append(all_admits, new_adm)
    # Tracks all patient's LoS
    LoS <- append(LoS, new_adm)
    
    # Checks how many people in hospital with days remaining
    daily_total <- append(daily_total, sum(all_admits > 0))
    
    # Simulates Next Day 
    all_admits <- all_admits - 1
  }
  
  # 1:100 are Daily Total Number People in the Hospital
  # 101:200 are Daily Number of New Patients
  # 201: are the LoS for every Patient
  if (output == 'Daily Total'){return(daily_total)}
  else if (output == 'LoS'){return(LoS)}
}
