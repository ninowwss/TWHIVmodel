# this is a R script for running TW HIV stochastic model 


sum_infected <- 0
sum_contact <- 0
acute_infected <- 0
zz <- vector()
for (number in 1:10000) {
  
  CD4 <- rnorm(1,1000,20)
  Z <- (CD4-1000)/20           
  Z_new <- function(Z){
    if(Z<0)
      dice <- -Z
    if(Z>=0)
      dice <- -Z
    return(dice)
  }
  Z_final <- Z_new(Z)
  #time to AIDS
  month_AIDS <- 0
  while (CD4>200) {           
    if (CD4<=199)
      CD4 <- CD4-((Z_final*0.5)+6.9)
    else if (CD4<=349)
      CD4 <- CD4-((Z_final*0.3)+6.1)
    else if (CD4<=499)
      CD4 <- CD4-((Z_final*0.3)+6.4)
    else if (CD4<=699)
      CD4 <- CD4-((Z_final*0.4)+10.0)
    else if (CD4<=899)
      CD4 <- CD4-((Z_final*0.7)+13.3)
    else 
      CD4 <- CD4-((Z_final*1.5)+22.9)
    end
    month_AIDS <- month_AIDS+1
  }
  day_AIDS <- month_AIDS*30.4375
  
  #Parameter Setting
  contact_rate <- 0.136892539356605
  prep_efficacy <- 0.86                  
  compliance <- 1                         
  prep <- 0.5                             
  no_prep <- (1-prep)
  testing_rate <- 0.006570841889117       
  T_C_rate <- 0.00597349262647004         
  C_A_rate <- 0.0328542094455852          
  I6_testing_rate <- 0.0328542094455852   
  I6_T_C_rate <- 0.0657030223390276     
  I6_C_A_rate <- 0.131406044678055      
  acute_chronic <- 26                     
  death_withoutT <- 0.00000736           
  death_T_to_C_to_A <- 0.00000925       
  death_after_A <- 0.00000819            
  death_I6_after_A <- 0.00004435       
  
  #Simulation
  #acute stage 0-30 day (acute1)
  count_acute1 <- 1
  contact_acute1 <- 0
  testing_acute1 <- 0
  death_acute1 <- 0
  nothing_acute1 <- 0
  infected_acute1 <- 0
  probability <- runif(30,0,1)
  P_Have_Sex <- contact_rate                  
  P_Test <- 0                          
  P_death <- death_withoutT
  P_Nothing <- (1-P_Have_Sex-P_Test-P_death)
  P_Oral_Sex <- (1/2)
  P_Anal_Sex <- (1/2)
  P_Oral_noprep_infected <- (0.0004*acute_chronic)          
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- (0.0004*acute_chronic*(1-prep_efficacy*compliance))
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)
  P_Anal_noprep_infected <- (0.008*(1-0.3)*acute_chronic)   
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- (0.008*(1-0.3)*acute_chronic*(1-prep_efficacy*compliance))
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_acute1<=30) {
    if (probability[count_acute1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_acute1 <- contact_acute1+1
    infected_acute1 <- infected_acute1+1}
    else if (probability[count_acute1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_acute1 <- contact_acute1+1
    else if (probability[count_acute1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_acute1 <- contact_acute1+1
    infected_acute1 <- infected_acute1+1}
    else if (probability[count_acute1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_acute1 <- contact_acute1+1
    else if (probability[count_acute1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_acute1 <- contact_acute1+1
    infected_acute1 <- infected_acute1+1}
    else if (probability[count_acute1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_acute1 <- contact_acute1+1
    else if (probability[count_acute1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_acute1 <- contact_acute1+1
    infected_acute1 <- infected_acute1+1}
    else if (probability[count_acute1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_acute1 <- contact_acute1+1
    else if (probability[count_acute1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Test)
      testing_acute1 <- testing_acute1+1
    else if (probability[count_acute1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Test+P_death)
    {death_acute1 <- death_acute1+1
    count_acute1 <-30}
    else
      nothing_acute1 <- nothing_acute1+1
    end
    count_acute1 <- count_acute1+1
  }
  
  #acute stage 31-90 day (acute2)
  count_acute2_1 <- 1
  contact_acute2_1 <- 0
  testing_acute2_1 <- 0
  death_acute2_1 <- 0
  nothing_acute2_1 <- 0
  infected_acute2_1 <- 0
  probability_acute2_1 <- runif(60,0,1)
  P_Have_Sex <- ifelse(death_acute1==0, contact_rate, 0)
  P_Test <- ifelse(death_acute1==0, testing_rate, 0)
  P_death <- ifelse(death_acute1==0, death_withoutT, 0)
  P_Nothing <- ifelse(death_acute1==0, (1-P_Have_Sex-P_Test-P_death),0)
  P_Oral_Sex <- (1/2)         
  P_Anal_Sex <- (1/2)           
  P_Oral_noprep_infected <- (0.0004*acute_chronic)         
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- (0.0004*acute_chronic*(1-prep_efficacy*compliance))
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)
  P_Anal_noprep_infected <- (0.008*(1-0.3)*acute_chronic)   
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- (0.008*(1-0.3)*acute_chronic*(1-prep_efficacy*compliance))
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_acute2_1<=60) {
    if (probability_acute2_1[count_acute2_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_acute2_1 <- contact_acute2_1+1
    infected_acute2_1 <- infected_acute2_1+1}
    else if (probability_acute2_1[count_acute2_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_acute2_1 <- contact_acute2_1+1
    else if (probability_acute2_1[count_acute2_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_acute2_1 <- contact_acute2_1+1
    infected_acute2_1 <- infected_acute2_1+1}
    else if (probability_acute2_1[count_acute2_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_acute2_1 <- contact_acute2_1+1
    else if (probability_acute2_1[count_acute2_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_acute2_1 <- contact_acute2_1+1
    infected_acute2_1 <- infected_acute2_1+1}
    else if (probability_acute2_1[count_acute2_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_acute2_1 <- contact_acute2_1+1
    else if (probability_acute2_1[count_acute2_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_acute2_1 <- contact_acute2_1+1
    infected_acute2_1 <- infected_acute2_1+1}
    else if (probability_acute2_1[count_acute2_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_acute2_1 <- contact_acute2_1+1
    else if (probability_acute2_1[count_acute2_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Test)
    {testing_acute2_1 <- testing_acute2_1+1
    count_acute2_1 <- 60} 
    else if (probability_acute2_1[count_acute2_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Test+P_death)
    {death_acute2_1 <- death_acute2_1+1
    count_acute2_1 <-60}
    else
      nothing_acute2_1 <- nothing_acute2_1+1
    end
    count_acute2_1 <- count_acute2_1+1
  }
  
  a <- (contact_acute2_1+testing_acute2_1+death_acute2_1+nothing_acute2_1)
  
  acute2_test_day <- function(testing_acute2_1){
    if(testing_acute2_1>0)
      dice <- a
    if(testing_acute2_1<=0)
      dice <- NA
    return(dice)
  }
  acute2_test_day <- acute2_test_day(testing_acute2_1)
  
  count_acute2_2 <- 1
  contact_acute2_2 <- 0
  care_acute2_2 <- 0
  death_acute2_2 <- 0
  nothing_acute2_2 <- 0
  infected_acute2_2 <- 0
  probability_acute2_2 <- runif((60-a),0,1)
  P_Have_Sex <- ifelse((death_acute1+death_acute2_1)==0, (contact_rate*0.8),0)                
  P_Care <- ifelse((death_acute1+death_acute2_1)==0, T_C_rate,0)
  P_death <- ifelse((death_acute1+death_acute2_1)==0, death_T_to_C_to_A,0)
  P_Nothing <- ifelse((death_acute1+death_acute2_1)==0, (1-P_Have_Sex-P_Care-P_death),0)     
  P_Oral_Sex <- (1/2)            
  P_Anal_Sex <- (1/2)           
  P_Oral_noprep_infected <- (0.0004*acute_chronic)        
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- (0.0004*acute_chronic*(1-prep_efficacy*compliance))
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)
  P_Anal_noprep_infected <- (0.008*(1-0.3)*acute_chronic)  
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- (0.008*(1-0.3)*acute_chronic*(1-prep_efficacy*compliance))
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_acute2_2<=(60-a)) {
    if (probability_acute2_2[count_acute2_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_acute2_2 <- contact_acute2_2+1
    infected_acute2_2 <- infected_acute2_2+1}
    else if (probability_acute2_2[count_acute2_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_acute2_2 <- contact_acute2_2+1
    else if (probability_acute2_2[count_acute2_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_acute2_2 <- contact_acute2_2+1
    infected_acute2_2 <- infected_acute2_2+1}
    else if (probability_acute2_2[count_acute2_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_acute2_2 <- contact_acute2_2+1
    else if (probability_acute2_2[count_acute2_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_acute2_2 <- contact_acute2_2+1
    infected_acute2_2 <- infected_acute2_2+1}
    else if (probability_acute2_2[count_acute2_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_acute2_2 <- contact_acute2_2+1
    else if (probability_acute2_2[count_acute2_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_acute2_2 <- contact_acute2_2+1
    infected_acute2_2 <- infected_acute2_2+1}
    else if (probability_acute2_2[count_acute2_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_acute2_2 <- contact_acute2_2+1
    else if (probability_acute2_2[count_acute2_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Care)
    {care_acute2_2 <- care_acute2_2+1
    count_acute2_2 <- (60-a)} 
    else if (probability_acute2_2[count_acute2_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Care+P_death)
    {death_acute2_2 <- death_acute2_2+1
    count_acute2_2 <- (60-a)}
    else
      nothing_acute2_2 <- nothing_acute2_2+1
    end
    count_acute2_2 <- count_acute2_2+1
  }
  
  b <- (a+contact_acute2_2+care_acute2_2+death_acute2_2+nothing_acute2_2)
  acute2_care_day <- function(care_acute2_2){
    if(care_acute2_2>0)
      dice <- b
    if(care_acute2_2<=0)
      dice <- NA
    return(dice)
  }
  acute2_care_day <- acute2_care_day(care_acute2_2)
  
  count_acute2_3 <- 1
  contact_acute2_3 <- 0
  ART_acute2_3 <- 0
  death_acute2_3 <- 0
  nothing_acute2_3 <- 0
  infected_acute2_3 <- 0
  probability_acute2_3 <- runif((60-b),0,1)
  P_Have_Sex <- ifelse((death_acute1+death_acute2_1+death_acute2_2)==0,(contact_rate*0.8),0)                  
  P_ART <- ifelse((death_acute1+death_acute2_1+death_acute2_2)==0,C_A_rate,0)                  
  P_death <- ifelse((death_acute1+death_acute2_1+death_acute2_2)==0, death_T_to_C_to_A,0)
  P_Nothing <- ifelse((death_acute1+death_acute2_1+death_acute2_2)==0,(1-P_Have_Sex-P_ART-P_death),0)      
  P_Oral_Sex <- (1/2)           
  P_Anal_Sex <- (1/2)          
  P_Oral_noprep_infected <- (0.0004*acute_chronic)        
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- (0.0004*acute_chronic*(1-prep_efficacy*compliance))
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)
  P_Anal_noprep_infected <- (0.008*(1-0.4875)*acute_chronic)   
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- (0.008*(1-0.4875)*acute_chronic*(1-prep_efficacy*compliance))
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_acute2_3<=(60-b)) {
    if (probability_acute2_3[count_acute2_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_acute2_3 <- contact_acute2_3+1
    infected_acute2_3 <- infected_acute2_3+1}
    else if (probability_acute2_3[count_acute2_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_acute2_3 <- contact_acute2_3+1
    else if (probability_acute2_3[count_acute2_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_acute2_3 <- contact_acute2_3+1
    infected_acute2_3 <- infected_acute2_3+1}
    else if (probability_acute2_3[count_acute2_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_acute2_3 <- contact_acute2_3+1
    else if (probability_acute2_3[count_acute2_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_acute2_3 <- contact_acute2_3+1
    infected_acute2_3 <- infected_acute2_3+1}
    else if (probability_acute2_3[count_acute2_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_acute2_3 <- contact_acute2_3+1
    else if (probability_acute2_3[count_acute2_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_acute2_3 <- contact_acute2_3+1
    infected_acute2_3 <- infected_acute2_3+1}
    else if (probability_acute2_3[count_acute2_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_acute2_3 <- contact_acute2_3+1
    else if (probability_acute2_3[count_acute2_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_ART)
    {ART_acute2_3 <- ART_acute2_3+1
    count_acute2_3 <- (60-b)}
    else if (probability_acute2_3[count_acute2_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_ART+P_death)
    {death_acute2_3 <- death_acute2_3+1
    count_acute2_3 <- (60-b)}
    else
      nothing_acute2_3 <- nothing_acute2_3+1
    end
    count_acute2_3 <- count_acute2_3+1
  }
  
  c <- (b+contact_acute2_3+ART_acute2_3+death_acute2_3+nothing_acute2_3)
  acute2_ART_day <- function(ART_acute2_3){
    if(ART_acute2_3>0)
      dice <- c
    if(ART_acute2_3<=0)
      dice <- NA
    return(dice)
  }
  acute2_ART_day <- acute2_ART_day(ART_acute2_3)
  
  contact_acute2 <- (contact_acute2_1+contact_acute2_2+contact_acute2_3)
  infected_acute2 <- (infected_acute2_1+infected_acute2_2+infected_acute2_3)
  
  d <- (testing_acute2_1+care_acute2_2+ART_acute2_3)
  ab <- (death_acute1+death_acute2_1+death_acute2_2+death_acute2_3)
  
  #chronic stage
  count_chronic_1 <- 1
  contact_chronic_1 <- 0
  testing_chronic_1 <- 0
  death_chronic_1 <- 0
  nothing_chronic_1 <- 0
  infected_chronic_1 <- 0
  probability_chronic_1 <- runif((day_AIDS-90),0,1)
  P_Have_Sex <- ifelse(d==0 & ab==0, contact_rate, 0)
  P_Test <- ifelse(d==0 & ab==0, testing_rate, 0)
  P_death <- ifelse(d==0 & ab==0, death_withoutT,0)
  P_Nothing <- ifelse(d==0 & ab==0,(1-P_Have_Sex-P_Test-P_death),0)
  P_Oral_Sex <- (1/2)
  P_Anal_Sex <- (1/2)
  P_Oral_noprep_infected <- ifelse(d==0, (0.0004*1), 0)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- ifelse(d==0, (0.0004*1*(1-prep_efficacy*compliance)), 0)
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- ifelse(d==0, (0.008*(1-0.3)*1), 0)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- ifelse(d==0, (0.008*(1-0.3)*1*(1-prep_efficacy*compliance)), 0)
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_chronic_1<=(day_AIDS-90)) {
    if (probability_chronic_1[count_chronic_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_chronic_1 <- contact_chronic_1+1
    infected_chronic_1 <- infected_chronic_1+1}
    else if (probability_chronic_1[count_chronic_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_chronic_1 <- contact_chronic_1+1
    else if (probability_chronic_1[count_chronic_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_chronic_1 <- contact_chronic_1+1
    infected_chronic_1 <- infected_chronic_1+1}
    else if (probability_chronic_1[count_chronic_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_chronic_1 <- contact_chronic_1+1
    else if (probability_chronic_1[count_chronic_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_chronic_1 <- contact_chronic_1+1
    infected_chronic_1 <- infected_chronic_1+1}
    else if (probability_chronic_1[count_chronic_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_chronic_1 <- contact_chronic_1+1
    else if (probability_chronic_1[count_chronic_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_chronic_1 <- contact_chronic_1+1
    infected_chronic_1 <- infected_chronic_1+1}
    else if (probability_chronic_1[count_chronic_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_chronic_1 <- contact_chronic_1+1
    else if (probability_chronic_1[count_chronic_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Test)
    {testing_chronic_1 <- testing_chronic_1+1
    count_chronic_1 <- (day_AIDS-90)}
    else if (probability_chronic_1[count_chronic_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Test+P_death)
    {death_chronic_1 <- death_chronic_1+1
    count_chronic_1 <- (day_AIDS-90)}
    else
      nothing_chronic_1 <- nothing_chronic_1+1
    end
    count_chronic_1 <- count_chronic_1+1
  }
  
  e <- (contact_chronic_1+testing_chronic_1+death_chronic_1+nothing_chronic_1)
  chronic_test_day_NO_TCA <- ifelse(testing_chronic_1==1, e, NA)
  
  count_chronic_2 <- 1
  contact_chronic_2 <- 0
  care_chronic_2 <- 0
  death_chronic_2 <- 0
  nothing_chronic_2 <- 0
  infected_chronic_2 <- 0
  probability_chronic_2 <- runif((day_AIDS-90-e),0,1)
  P_Have_Sex <- ifelse(d==0 & (ab+death_chronic_1)==0, (contact_rate*0.8), 0)
  P_Care <- ifelse(d==0 & (ab+death_chronic_1)==0, T_C_rate, 0)
  P_death <- ifelse(d==0 & (ab+death_chronic_1)==0, death_T_to_C_to_A, 0)
  P_Nothing <- ifelse(d==0 & (ab+death_chronic_1)==0, (1-P_Have_Sex-P_Care-P_death),0)
  P_Oral_Sex <- (1/2)
  P_Anal_Sex <- (1/2)
  P_Oral_noprep_infected <- ifelse(d==0, (0.0004*1), 0)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- ifelse(d==0, (0.0004*1*(1-prep_efficacy*compliance)), 0)
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- ifelse(d==0, (0.008*(1-0.3)*1), 0)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- ifelse(d==0, (0.008*(1-0.3)*1*(1-prep_efficacy*compliance)), 0)
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_chronic_2<=(day_AIDS-90-e)) {
    if (probability_chronic_2[count_chronic_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_chronic_2 <- contact_chronic_2+1
    infected_chronic_2 <- infected_chronic_2+1}
    else if (probability_chronic_2[count_chronic_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_chronic_2 <- contact_chronic_2+1
    else if (probability_chronic_2[count_chronic_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_chronic_2 <- contact_chronic_2+1
    infected_chronic_2 <- infected_chronic_2+1}
    else if (probability_chronic_2[count_chronic_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_chronic_2 <- contact_chronic_2+1
    else if (probability_chronic_2[count_chronic_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_chronic_2 <- contact_chronic_2+1
    infected_chronic_2 <- infected_chronic_2+1}
    else if (probability_chronic_2[count_chronic_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_chronic_2 <- contact_chronic_2+1
    else if (probability_chronic_2[count_chronic_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_chronic_2 <- contact_chronic_2+1
    infected_chronic_2 <- infected_chronic_2+1}
    else if (probability_chronic_2[count_chronic_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_chronic_2 <- contact_chronic_2+1
    else if (probability_chronic_2[count_chronic_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Care)
    {care_chronic_2 <- care_chronic_2+1
    count_chronic_2 <- (day_AIDS-90-e)}
    else if (probability_chronic_2[count_chronic_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Care+P_death)
    {death_chronic_2 <- death_chronic_2+1
    count_chronic_2 <- (day_AIDS-90-e)}
    else
      nothing_chronic_2 <- nothing_chronic_2+1
    end
    count_chronic_2 <- count_chronic_2+1
  }
  
  f <- (e+contact_chronic_2+care_chronic_2+death_chronic_2+nothing_chronic_2)
  chronic_care_day_NO_TCA <- ifelse(care_chronic_2==1, f, NA)
  
  count_chronic_3 <- 1
  contact_chronic_3 <- 0
  ART_chronic_3 <- 0
  death_chronic_3 <- 0
  nothing_chronic_3 <- 0
  infected_chronic_3 <- 0
  probability_chronic_3 <- runif((day_AIDS-90-f),0,1)
  P_Have_Sex <- ifelse(d==0 & (ab+death_chronic_1+death_chronic_2)==0, (contact_rate*0.8), 0)
  P_ART <- ifelse(d==0 & (ab+death_chronic_1+death_chronic_2)==0, C_A_rate, 0)
  P_death <- ifelse(d==0 & (ab+death_chronic_1+death_chronic_2)==0, death_T_to_C_to_A, 0)
  P_Nothing <- ifelse(d==0 & (ab+death_chronic_1+death_chronic_2)==0,(1-P_Have_Sex-P_ART-P_death),0)
  P_Oral_Sex <- (1/2)
  P_Anal_Sex <- (1/2)
  P_Oral_noprep_infected <- ifelse(d==0, (0.0004*1), 0)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- ifelse(d==0, (0.0004*1*(1-prep_efficacy*compliance)), 0)
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- ifelse(d==0, (0.008*(1-0.4875)*1), 0)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- ifelse(d==0, (0.008*(1-0.4875)*1*(1-prep_efficacy*compliance)), 0)
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_chronic_3<=(day_AIDS-90-f)) {
    if (probability_chronic_3[count_chronic_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_chronic_3 <- contact_chronic_3+1
    infected_chronic_3 <- infected_chronic_3+1}
    else if (probability_chronic_3[count_chronic_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_chronic_3 <- contact_chronic_3+1
    else if (probability_chronic_3[count_chronic_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_chronic_3 <- contact_chronic_3+1
    infected_chronic_3 <- infected_chronic_3+1}
    else if (probability_chronic_3[count_chronic_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_chronic_3 <- contact_chronic_3+1
    else if (probability_chronic_3[count_chronic_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_chronic_3 <- contact_chronic_3+1
    infected_chronic_3 <- infected_chronic_3+1}
    else if (probability_chronic_3[count_chronic_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_chronic_3 <- contact_chronic_3+1
    else if (probability_chronic_3[count_chronic_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_chronic_3 <- contact_chronic_3+1
    infected_chronic_3 <- infected_chronic_3+1}
    else if (probability_chronic_3[count_chronic_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_chronic_3 <- contact_chronic_3+1
    else if (probability_chronic_3[count_chronic_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_ART)
    {ART_chronic_3 <- ART_chronic_3+1
    count_chronic_3 <- (day_AIDS-90-f)}
    else if (probability_chronic_3[count_chronic_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_ART+P_death)
    {death_chronic_3 <- death_chronic_3+1
    count_chronic_3 <- (day_AIDS-90-f)}
    else
      nothing_chronic_3 <- nothing_chronic_3+1
    end
    count_chronic_3 <- count_chronic_3+1
  }
  
  g <- (f+contact_chronic_3+ART_chronic_3+death_chronic_3+nothing_chronic_3)
  chronic_ART_day_NO_TCA <- ifelse(ART_chronic_3==1, g, NA)
  
  #chronic stage
  count_chronic_4 <- 1
  contact_chronic_4 <- 0
  care_chronic_4 <- 0
  death_chronic_4 <- 0
  nothing_chronic_4 <- 0
  infected_chronic_4 <- 0
  probability_chronic_4 <- runif((day_AIDS-90),0,1)
  P_Have_Sex <- ifelse(d==1 & ab==0, (contact_rate*0.8), 0)
  P_Care <- ifelse(d==1 & ab==0, T_C_rate, 0)
  P_death <- ifelse(d==1 & ab==0, death_T_to_C_to_A, 0)
  P_Nothing <- ifelse(d==1 & ab==0, (1-P_Have_Sex-P_Care-P_death),0)
  P_Oral_Sex <- (1/2)
  P_Anal_Sex <- (1/2)
  P_Oral_noprep_infected <- ifelse(d==1, (0.0004*1), 0)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- ifelse(d==1, (0.0004*1*(1-prep_efficacy*compliance)), 0)
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- ifelse(d==1, (0.008*(1-0.3)*1), 0)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- ifelse(d==1, (0.008*(1-0.3)*1*(1-prep_efficacy*compliance)), 0)
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_chronic_4<=(day_AIDS-90)) {
    if (probability_chronic_4[count_chronic_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_chronic_4 <- contact_chronic_4+1
    infected_chronic_4 <- infected_chronic_4+1}
    else if (probability_chronic_4[count_chronic_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_chronic_4 <- contact_chronic_4+1
    else if (probability_chronic_4[count_chronic_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_chronic_4 <- contact_chronic_4+1
    infected_chronic_4 <- infected_chronic_4+1}
    else if (probability_chronic_4[count_chronic_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_chronic_4 <- contact_chronic_4+1
    else if (probability_chronic_4[count_chronic_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_chronic_4 <- contact_chronic_4+1
    infected_chronic_4 <- infected_chronic_4+1}
    else if (probability_chronic_4[count_chronic_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_chronic_4 <- contact_chronic_4+1
    else if (probability_chronic_4[count_chronic_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_chronic_4 <- contact_chronic_4+1
    infected_chronic_4 <- infected_chronic_4+1}
    else if (probability_chronic_4[count_chronic_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_chronic_4 <- contact_chronic_4+1
    else if (probability_chronic_4[count_chronic_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Care)
    {care_chronic_4 <- care_chronic_4+1
    count_chronic_4 <- (day_AIDS-90)}
    else if (probability_chronic_4[count_chronic_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Care+P_death)
    {death_chronic_4 <- death_chronic_4+1
    count_chronic_4 <- (day_AIDS-90)}
    else
      nothing_chronic_4 <- nothing_chronic_4+1
    end
    count_chronic_4 <- count_chronic_4+1
  }
  
  h <- (contact_chronic_4+care_chronic_4+death_chronic_4+nothing_chronic_4)
  chronic_care_day_NO_CA <- ifelse(care_chronic_4==1, h, NA)
  
  count_chronic_5 <- 1
  contact_chronic_5 <- 0
  ART_chronic_5 <- 0
  death_chronic_5 <- 0
  nothing_chronic_5 <- 0
  infected_chronic_5 <- 0
  probability_chronic_5 <- runif((day_AIDS-90-h),0,1)
  P_Have_Sex <- ifelse(d==1 & (ab+death_chronic_4)==0, (contact_rate*0.8), 0)
  P_ART <- ifelse(d==1 & (ab+death_chronic_4)==0, C_A_rate, 0)
  P_death <- ifelse(d==1 & (ab+death_chronic_4)==0, death_T_to_C_to_A, 0)
  P_Nothing <- ifelse(d==1 & (ab+death_chronic_4)==0,(1-P_Have_Sex-P_ART-P_death),0)
  P_Oral_Sex <- (1/2)
  P_Anal_Sex <- (1/2)
  P_Oral_noprep_infected <- ifelse(d==1, (0.0004*1), 0)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- ifelse(d==1, (0.0004*1*(1-prep_efficacy*compliance)), 0)
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- ifelse(d==1, (0.008*(1-0.4875)*1), 0)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- ifelse(d==1, (0.008*(1-0.4875)*1*(1-prep_efficacy*compliance)), 0)
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_chronic_5<=(day_AIDS-90-h)) {
    if (probability_chronic_5[count_chronic_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_chronic_5 <- contact_chronic_5+1
    infected_chronic_5 <- infected_chronic_5+1}
    else if (probability_chronic_5[count_chronic_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_chronic_5 <- contact_chronic_5+1
    else if (probability_chronic_5[count_chronic_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_chronic_5 <- contact_chronic_5+1
    infected_chronic_5 <- infected_chronic_5+1}
    else if (probability_chronic_5[count_chronic_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_chronic_5 <- contact_chronic_5+1
    else if (probability_chronic_5[count_chronic_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_chronic_5 <- contact_chronic_5+1
    infected_chronic_5 <- infected_chronic_5+1}
    else if (probability_chronic_5[count_chronic_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_chronic_5 <- contact_chronic_5+1
    else if (probability_chronic_5[count_chronic_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_chronic_5 <- contact_chronic_5+1
    infected_chronic_5 <- infected_chronic_5+1}
    else if (probability_chronic_5[count_chronic_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_chronic_5 <- contact_chronic_5+1
    else if (probability_chronic_5[count_chronic_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_ART)
    {ART_chronic_5 <- ART_chronic_5+1
    count_chronic_5 <- (day_AIDS-90-h)}
    else if (probability_chronic_5[count_chronic_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_ART+P_death)
    {death_chronic_5 <- death_chronic_5+1
    count_chronic_5 <- (day_AIDS-90-h)}
    else
      nothing_chronic_5 <- nothing_chronic_5+1
    end
    count_chronic_5 <- count_chronic_5+1
  }
  
  i <- (h+contact_chronic_5+ART_chronic_5+death_chronic_5+nothing_chronic_5)
  chronic_ART_day_NO_CA <- ifelse(ART_chronic_5==1, i, NA)
  
  #chronic stage
  count_chronic_6 <- 1
  contact_chronic_6 <- 0
  ART_chronic_6 <- 0
  death_chronic_6 <- 0
  nothing_chronic_6 <- 0
  infected_chronic_6 <- 0
  probability_chronic_6 <- runif((day_AIDS-90),0,1)
  P_Have_Sex <- ifelse(d==2 & ab==0, (contact_rate*0.8), 0)
  P_ART <- ifelse(d==2 & ab==0, C_A_rate, 0)
  P_death <- ifelse(d==2 & ab==0, death_T_to_C_to_A, 0)
  P_Nothing <- ifelse(d==2 & ab==0, (1-P_Have_Sex-P_ART-P_death),0)
  P_Oral_Sex <- (1/2)
  P_Anal_Sex <- (1/2)
  P_Oral_noprep_infected <- ifelse(d==2, (0.0004*1), 0)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- ifelse(d==2, (0.0004*1*(1-prep_efficacy*compliance)), 0)
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- ifelse(d==2, (0.008*(1-0.4875)*1), 0)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- ifelse(d==2, (0.008*(1-0.4875)*1*(1-prep_efficacy*compliance)), 0)
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_chronic_6<=(day_AIDS-90)) {
    if (probability_chronic_6[count_chronic_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_chronic_6 <- contact_chronic_6+1
    infected_chronic_6 <- infected_chronic_6+1}
    else if (probability_chronic_6[count_chronic_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_chronic_6 <- contact_chronic_6+1
    else if (probability_chronic_6[count_chronic_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_chronic_6 <- contact_chronic_6+1
    infected_chronic_6 <- infected_chronic_6+1}
    else if (probability_chronic_6[count_chronic_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_chronic_6 <- contact_chronic_6+1
    else if (probability_chronic_6[count_chronic_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_chronic_6 <- contact_chronic_6+1
    infected_chronic_6 <- infected_chronic_6+1}
    else if (probability_chronic_6[count_chronic_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_chronic_6 <- contact_chronic_6+1
    else if (probability_chronic_6[count_chronic_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_chronic_6 <- contact_chronic_6+1
    infected_chronic_6 <- infected_chronic_6+1}
    else if (probability_chronic_6[count_chronic_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_chronic_6 <- contact_chronic_6+1
    else if (probability_chronic_6[count_chronic_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_ART)
    {ART_chronic_6 <- ART_chronic_6+1
    count_chronic_6 <- (day_AIDS-90)}
    else if (probability_chronic_6[count_chronic_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_ART+P_death)
    {death_chronic_6 <- death_chronic_6+1
    count_chronic_6 <- (day_AIDS-90)}
    else
      nothing_chronic_6 <- nothing_chronic_6+1
    end
    count_chronic_6 <- count_chronic_6+1
  }
  
  j <- (contact_chronic_6+ART_chronic_6+death_chronic_6+nothing_chronic_6)
  chronic_ART_day_NO_A <- ifelse(ART_chronic_6==1, j, NA)
  
  contact_chronic <- (contact_chronic_1+contact_chronic_2+contact_chronic_3+contact_chronic_4+contact_chronic_5+contact_chronic_6)
  infected_chronic <- (infected_chronic_1+infected_chronic_2+infected_chronic_3+infected_chronic_4+infected_chronic_5+infected_chronic_6)
  
  chronic_test_day <- 
    if(d==0){
      chronic_test_day <- chronic_test_day_NO_TCA
    }else{
      chronic_test_day <- NA
    }
  
  chronic_care_day <- 
    if(d==0){
      chronic_care_day <- chronic_care_day_NO_TCA
    }else if(d==1){
      chronic_care_day <- chronic_care_day_NO_CA
    }else{
      chronic_care_day <- NA
    }
  
  chronic_ART_day <- 
    if(d==0){
      chronic_ART_day <- chronic_ART_day_NO_TCA
    }else if(d==1){
      chronic_ART_day <- chronic_ART_day_NO_CA
    }else if(d==2){
      chronic_ART_day <- chronic_ART_day_NO_A
    }else{
      chronic_ART_day <- NA
    }
  
  k <- (d+testing_chronic_1+care_chronic_2+ART_chronic_3+care_chronic_4+ART_chronic_5+ART_chronic_6)
  ac <- (ab+death_chronic_1+death_chronic_2+death_chronic_3+death_chronic_4+death_chronic_5+death_chronic_6)
  
  #I5 stage
  count_I5_1 <- 1
  contact_I5_1 <- 0
  testing_I5_1 <- 0
  death_I5_1 <- 0
  nothing_I5_1 <- 0
  infected_I5_1 <- 0
  probability_4 <- runif((1.6*365.25),0,1)
  P_Have_Sex <- ifelse(k==0 & ac==0, contact_rate, 0)
  P_Test <- ifelse(k==0 & ac==0, testing_rate, 0)
  P_death <- ifelse(k==0 & ac==0,death_withoutT ,0)
  P_Nothing <- ifelse(k==0 & ac==0,(1-P_Have_Sex-P_Test-P_death),0)
  P_Oral_Sex <- (1/2)
  P_Anal_Sex <- (1/2)
  P_Oral_noprep_infected <- ifelse(k==0, (0.0004*4), 0)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- ifelse(k==0, (0.0004*4*(1-prep_efficacy*compliance)), 0)
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- ifelse(k==0, (0.008*(1-0.3)*4), 0)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- ifelse(k==0, (0.008*(1-0.3)*4*(1-prep_efficacy*compliance)), 0)
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_I5_1<=(1.6*365.25)) {
    if (probability_4[count_I5_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_I5_1 <- contact_I5_1+1
    infected_I5_1 <- infected_I5_1+1}
    else if (probability_4[count_I5_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_I5_1 <- contact_I5_1+1
    else if (probability_4[count_I5_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_I5_1 <- contact_I5_1+1
    infected_I5_1 <- infected_I5_1+1}
    else if (probability_4[count_I5_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_I5_1 <- contact_I5_1+1
    else if (probability_4[count_I5_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_I5_1 <- contact_I5_1+1
    infected_I5_1 <- infected_I5_1+1}
    else if (probability_4[count_I5_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_I5_1 <- contact_I5_1+1
    else if (probability_4[count_I5_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_I5_1 <- contact_I5_1+1
    infected_I5_1 <- infected_I5_1+1}
    else if (probability_4[count_I5_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_I5_1 <- contact_I5_1+1
    else if (probability_4[count_I5_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Test)
    {testing_I5_1 <- testing_I5_1+1
    count_I5_1 <- (1.6*365.25)}
    else if (probability_4[count_I5_1]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Test+P_death)
    {death_I5_1 <- death_I5_1+1
    count_I5_1 <- (1.6*365.25)}
    else
      nothing_I5_1 <- nothing_I5_1+1
    end
    count_I5_1 <- count_I5_1+1
  }
  
  l <- (contact_I5_1+testing_I5_1+death_I5_1+nothing_I5_1)
  I5_test_day_NO_TCA <- ifelse(testing_I5_1==1, l, NA)
  
  count_I5_2 <- 1
  contact_I5_2 <- 0
  care_I5_2 <- 0
  death_I5_2 <- 0
  nothing_I5_2 <- 0
  infected_I5_2 <- 0
  probability_5 <- runif((1.6*365.25-l),0,1)
  P_Have_Sex <- ifelse(k==0 & (ac+death_I5_1)==0, (contact_rate*0.8), 0)
  P_Care <- ifelse(k==0 & (ac+death_I5_1)==0, T_C_rate, 0)
  P_death <- ifelse(k==0 & (ac+death_I5_1)==0, death_T_to_C_to_A, 0)
  P_Nothing <- ifelse(k==0 & (ac+death_I5_1)==0, (1-P_Have_Sex-P_Care-P_death),0)
  P_Oral_Sex <- (1/2)
  P_Anal_Sex <- (1/2)
  P_Oral_noprep_infected <- ifelse(k==0, (0.0004*4), 0)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- ifelse(k==0, (0.0004*4*(1-prep_efficacy*compliance)), 0)
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- ifelse(k==0, (0.008*(1-0.3)*4), 0)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- ifelse(k==0, (0.008*(1-0.3)*4*(1-prep_efficacy*compliance)), 0)
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_I5_2<=(1.6*365.25-l)) {
    if (probability_5[count_I5_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_I5_2 <- contact_I5_2+1
    infected_I5_2 <- infected_I5_2+1}
    else if (probability_5[count_I5_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_I5_2 <- contact_I5_2+1
    else if (probability_5[count_I5_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_I5_2 <- contact_I5_2+1
    infected_I5_2 <- infected_I5_2+1}
    else if (probability_5[count_I5_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_I5_2 <- contact_I5_2+1
    else if (probability_5[count_I5_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_I5_2 <- contact_I5_2+1
    infected_I5_2 <- infected_I5_2+1}
    else if (probability_5[count_I5_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_I5_2 <- contact_I5_2+1
    else if (probability_5[count_I5_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_I5_2 <- contact_I5_2+1
    infected_I5_2 <- infected_I5_2+1}
    else if (probability_5[count_I5_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_I5_2 <- contact_I5_2+1
    else if (probability_5[count_I5_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Care)
    {care_I5_2 <- care_I5_2+1
    count_I5_2 <- (1.6*365.25-l)}
    else if (probability_5[count_I5_2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Care+P_death)
    {death_I5_2 <- death_I5_2+1
    count_I5_2 <- (1.6*365.25-l)}
    else
      nothing_I5_2 <- nothing_I5_2+1
    end
    count_I5_2 <- count_I5_2+1
  }
  
  m <- (l+contact_I5_2+care_I5_2+death_I5_2+nothing_I5_2)
  I5_care_day_NO_TCA <- ifelse(care_I5_2==1, m, NA)
  
  count_I5_3 <- 1
  contact_I5_3 <- 0
  ART_I5_3 <- 0
  death_I5_3 <- 0
  nothing_I5_3 <- 0
  infected_I5_3 <- 0
  probability_6 <- runif((1.6*365.25-m),0,1)
  P_Have_Sex <- ifelse(k==0 & (ac+death_I5_1+death_I5_2)==0, (contact_rate*0.8), 0)
  P_ART <- ifelse(k==0 & (ac+death_I5_1+death_I5_2)==0, C_A_rate, 0)
  P_death <- ifelse(k==0 & (ac+death_I5_1+death_I5_2)==0, death_T_to_C_to_A, 0)
  P_Nothing <- ifelse(k==0 & (ac+death_I5_1+death_I5_2)==0,(1-P_Have_Sex-P_ART-P_death),0)
  P_Oral_Sex <- (1/2)
  P_Anal_Sex <- (1/2)
  P_Oral_noprep_infected <- ifelse(k==0, (0.0004*4), 0)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- ifelse(k==0, (0.0004*4*(1-prep_efficacy*compliance)), 0)
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- ifelse(k==0, (0.008*(1-0.4875)*4), 0)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- ifelse(k==0, (0.008*(1-0.4875)*4*(1-prep_efficacy*compliance)), 0)
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_I5_3<=(1.6*365.25-m)) {
    if (probability_6[count_I5_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_I5_3 <- contact_I5_3+1
    infected_I5_3 <- infected_I5_3+1}
    else if (probability_6[count_I5_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_I5_3 <- contact_I5_3+1
    else if (probability_6[count_I5_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_I5_3 <- contact_I5_3+1
    infected_I5_3 <- infected_I5_3+1}
    else if (probability_6[count_I5_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_I5_3 <- contact_I5_3+1
    else if (probability_6[count_I5_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_I5_3 <- contact_I5_3+1
    infected_I5_3 <- infected_I5_3+1}
    else if (probability_6[count_I5_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_I5_3 <- contact_I5_3+1
    else if (probability_6[count_I5_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_I5_3 <- contact_I5_3+1
    infected_I5_3 <- infected_I5_3+1}
    else if (probability_6[count_I5_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_I5_3 <- contact_I5_3+1
    else if (probability_6[count_I5_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_ART)
    {ART_I5_3 <- ART_I5_3+1
    count_I5_3 <- (1.6*365.25-m)}
    else if (probability_6[count_I5_3]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_ART+P_death)
    {death_I5_3 <- death_I5_3+1
    count_I5_3 <- (1.6*365.25-m)}
    else
      nothing_I5_3 <- nothing_I5_3+1
    end
    count_I5_3 <- count_I5_3+1
  }
  
  n <- (m+contact_I5_3+ART_I5_3+death_I5_3+nothing_I5_3)
  I5_ART_day_NO_TCA <- ifelse(ART_I5_3==1, n, NA)
  
  #I5 stage
  count_I5_4 <- 1
  contact_I5_4 <- 0
  care_I5_4 <- 0
  death_I5_4 <- 0
  nothing_I5_4 <- 0
  infected_I5_4 <- 0
  probability_7 <- runif((1.6*365.25),0,1)
  P_Have_Sex <- ifelse(k==1 & ac==0, (contact_rate*0.8), 0)
  P_Care <- ifelse(k==1 & ac==0, T_C_rate, 0)
  P_death <- ifelse(k==1 & ac==0, death_T_to_C_to_A, 0)
  P_Nothing <- ifelse(k==1 & ac==0, (1-P_Have_Sex-P_Care-P_death),0)
  P_Oral_Sex <- (1/2)
  P_Anal_Sex <- (1/2)
  P_Oral_noprep_infected <- ifelse(k==1, (0.0004*4), 0)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- ifelse(k==1, (0.0004*4*(1-prep_efficacy*compliance)), 0)
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- ifelse(k==1, (0.008*(1-0.3)*4), 0)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- ifelse(k==1, (0.008*(1-0.3)*4*(1-prep_efficacy*compliance)), 0)
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_I5_4<=(1.6*365.25)) {
    if (probability_7[count_I5_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_I5_4 <- contact_I5_4+1
    infected_I5_4 <- infected_I5_4+1}
    else if (probability_7[count_I5_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_I5_4 <- contact_I5_4+1
    else if (probability_7[count_I5_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_I5_4 <- contact_I5_4+1
    infected_I5_4 <- infected_I5_4+1}
    else if (probability_7[count_I5_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_I5_4 <- contact_I5_4+1
    else if (probability_7[count_I5_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_I5_4 <- contact_I5_4+1
    infected_I5_4 <- infected_I5_4+1}
    else if (probability_7[count_I5_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_I5_4 <- contact_I5_4+1
    else if (probability_7[count_I5_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_I5_4 <- contact_I5_4+1
    infected_I5_4 <- infected_I5_4+1}
    else if (probability_7[count_I5_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_I5_4 <- contact_I5_4+1
    else if (probability_7[count_I5_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Care)
    {care_I5_4 <- care_I5_4+1
    count_I5_4 <- (1.6*365.25)}
    else if (probability_7[count_I5_4]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_Care+P_death)
    {death_I5_4 <- death_I5_4+1
    count_I5_4 <- (1.6*365.25)}
    else
      nothing_I5_4 <- nothing_I5_4+1
    end
    count_I5_4 <- count_I5_4+1
  }
  
  o <- (contact_I5_4+care_I5_4+death_I5_4+nothing_I5_4)
  I5_care_day_NO_CA <- ifelse(care_I5_4==1, o, NA)
  
  count_I5_5 <- 1
  contact_I5_5 <- 0
  ART_I5_5 <- 0
  death_I5_5 <- 0
  nothing_I5_5 <- 0
  infected_I5_5 <- 0
  probability_8 <- runif((1.6*365.25-o),0,1)
  P_Have_Sex <- ifelse(k==1 & (ac+death_I5_4)==0, (contact_rate*0.8), 0)
  P_ART <- ifelse(k==1 & (ac+death_I5_4)==0, C_A_rate, 0)
  P_death <- ifelse(k==1 & (ac+death_I5_4)==0, death_T_to_C_to_A, 0)
  P_Nothing <- ifelse(k==1 & (ac+death_I5_4)==0, (1-P_Have_Sex-P_ART-P_death),0)
  P_Oral_Sex <- (1/2)
  P_Anal_Sex <- (1/2)
  P_Oral_noprep_infected <- ifelse(k==1, (0.0004*4), 0)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- ifelse(k==1, (0.0004*4*(1-prep_efficacy*compliance)), 0)
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- ifelse(k==1, (0.008*(1-0.4875)*4), 0)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- ifelse(k==1, (0.008*(1-0.4875)*4*(1-prep_efficacy*compliance)), 0)
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_I5_5<=(1.6*365.25-o)) {
    if (probability_8[count_I5_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_I5_5 <- contact_I5_5+1
    infected_I5_5 <- infected_I5_5+1}
    else if (probability_8[count_I5_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_I5_5 <- contact_I5_5+1
    else if (probability_8[count_I5_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_I5_5 <- contact_I5_5+1
    infected_I5_5 <- infected_I5_5+1}
    else if (probability_8[count_I5_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_I5_5 <- contact_I5_5+1
    else if (probability_8[count_I5_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_I5_5 <- contact_I5_5+1
    infected_I5_5 <- infected_I5_5+1}
    else if (probability_8[count_I5_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_I5_5 <- contact_I5_5+1
    else if (probability_8[count_I5_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_I5_5 <- contact_I5_5+1
    infected_I5_5 <- infected_I5_5+1}
    else if (probability_8[count_I5_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_I5_5 <- contact_I5_5+1
    else if (probability_8[count_I5_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_ART)
    {ART_I5_5 <- ART_I5_5+1
    count_I5_5 <- (1.6*365.25-o)}
    else if (probability_8[count_I5_5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_ART+P_death)
    {death_I5_5 <- death_I5_5+1
    count_I5_5 <- (1.6*365.25-o)}
    else
      nothing_I5_5 <- nothing_I5_5+1
    end
    count_I5_5 <- count_I5_5+1
  }
  
  p <- (o+contact_I5_5+ART_I5_5+death_I5_5+nothing_I5_5)
  I5_ART_day_NO_CA <- ifelse(ART_I5_5==1, p, NA)
  
  #I5 stage
  count_I5_6 <- 1
  contact_I5_6 <- 0
  ART_I5_6 <- 0
  death_I5_6 <- 0
  nothing_I5_6 <- 0
  infected_I5_6 <- 0
  probability_9 <- runif((1.6*365.25),0,1)
  P_Have_Sex <- ifelse(k==2 & ac==0, (contact_rate*0.8), 0)
  P_ART <- ifelse(k==2 & ac==0, C_A_rate, 0)
  P_death <- ifelse(k==2 & ac==0, death_T_to_C_to_A, 0)
  P_Nothing <- ifelse(k==2 & ac==0,(1-P_Have_Sex-P_ART-P_death),0)
  P_Oral_Sex <- (1/2)
  P_Anal_Sex <- (1/2)
  P_Oral_noprep_infected <- ifelse(k==2, (0.0004*4), 0)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- ifelse(k==2, (0.0004*4*(1-prep_efficacy*compliance)), 0)
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- ifelse(k==2, (0.008*(1-0.4875)*4), 0)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- ifelse(k==2, (0.008*(1-0.4875)*4*(1-prep_efficacy*compliance)), 0)
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_I5_6<=(1.6*365.25)) {
    if (probability_9[count_I5_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_I5_6 <- contact_I5_6+1
    infected_I5_6 <- infected_I5_6+1}
    else if (probability_9[count_I5_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_I5_6 <- contact_I5_6+1
    else if (probability_9[count_I5_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_I5_6 <- contact_I5_6+1
    infected_I5_6 <- infected_I5_6+1}
    else if (probability_9[count_I5_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_I5_6 <- contact_I5_6+1
    else if (probability_9[count_I5_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_I5_6 <- contact_I5_6+1
    infected_I5_6 <- infected_I5_6+1}
    else if (probability_9[count_I5_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_I5_6 <- contact_I5_6+1
    else if (probability_9[count_I5_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_I5_6 <- contact_I5_6+1
    infected_I5_6 <- infected_I5_6+1}
    else if (probability_9[count_I5_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_I5_6 <- contact_I5_6+1
    else if (probability_9[count_I5_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_ART)
    {ART_I5_6 <- ART_I5_6+1
    count_I5_6 <- (1.6*365.25)}
    else if (probability_9[count_I5_6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_ART+P_death)
    {death_I5_6 <- death_I5_6+1
    count_I5_6 <- (1.6*365.25)}
    else
      nothing_I5_6 <- nothing_I5_6+1
    end
    count_I5_6 <- count_I5_6+1
  }
  
  q <- (contact_I5_6+ART_I5_6+death_I5_6+nothing_I5_6)
  I5_ART_day_NO_A <- ifelse(ART_I5_6==1, q, NA)
  
  contact_I5 <- (contact_I5_1+contact_I5_2+contact_I5_3+contact_I5_4+contact_I5_5+contact_I5_6)
  infected_I5 <- (infected_I5_1+infected_I5_2+infected_I5_3+infected_I5_4+infected_I5_5+infected_I5_6)
  
  I5_test_day <- 
    if(k==0){
      I5_test_day <- I5_test_day_NO_TCA
    }else{
      I5_test_day <- NA
    }
  
  I5_care_day <- 
    if(k==0){
      I5_care_day <- I5_care_day_NO_TCA
    }else if(k==1){
      I5_care_day <- I5_care_day_NO_CA
    }else{
      I5_care_day <- NA
    }
  
  I5_ART_day <- 
    if(k==0){
      I5_ART_day <- I5_ART_day_NO_TCA
    }else if(k==1){
      I5_ART_day <- I5_ART_day_NO_CA
    }else if(k==2){
      I5_ART_day <- I5_ART_day_NO_A
    }else{
      I5_ART_day <- NA
    }
  
  r <- (k+testing_I5_1+care_I5_2+ART_I5_3+care_I5_4+ART_I5_5+ART_I5_6)
  ad <- (ac+death_I5_1+death_I5_2+death_I5_3+death_I5_4+death_I5_5+death_I5_6)
  
  #I6
  count_I6_1 <- 1
  testing_I6_1 <- 0
  nothing_I6_1 <- 0
  probability_10 <- runif((1*365.25),0,1)
  P_Test <- ifelse(r==0 & ad==0, I6_testing_rate, 0)
  P_Nothing <- ifelse(r==0 & ad==0, (1-P_Test),0)
  while (count_I6_1<=(1*365.25)) {
    if (probability_10[count_I6_1]<P_Test)
    {testing_I6_1 <- testing_I6_1+1
    count_I6_1 <- (1*365.25)}
    else
      nothing_I6_1 <- nothing_I6_1+1
    end
    count_I6_1 <- count_I6_1+1
  }
  
  s <- (testing_I6_1+nothing_I6_1)
  I6_test_day_NO_TCA <- ifelse(testing_I6_1==1, s, NA)
  
  count_I6_2 <- 1
  care_I6_2 <- 0
  nothing_I6_2 <- 0
  probability_11 <- runif((1*365.25-s),0,1)
  P_Care <- ifelse(r==0 & ad==0, I6_T_C_rate, 0)
  P_Nothing <- ifelse(r==0 & ad==0, (1-P_Care),0)
  while (count_I6_2<=(1*365.25-s)) {
    if (probability_11[count_I6_2]<P_Care)
    {care_I6_2 <- care_I6_2+1
    count_I6_2 <- (1*365.25-s)}
    else
      nothing_I6_2 <- nothing_I6_2+1
    end
    count_I6_2 <- count_I6_2+1
  }
  
  t <- (s+care_I6_2+nothing_I6_2)
  I6_care_day_NO_TCA <- ifelse(care_I6_2==1, t, NA)
  
  count_I6_3 <- 1
  ART_I6_3 <- 0
  nothing_I6_3 <- 0
  probability_12 <- runif((1*365.25-t),0,1)
  P_ART <- ifelse(r==0 & ad==0, I6_C_A_rate, 0)
  P_Nothing <- ifelse(r==0 & ad==0, (1-P_ART),0)
  while (count_I6_3<=(1*365.25-t)) {
    if (probability_12[count_I6_3]<P_ART)
    {ART_I6_3 <- ART_I6_3+1
    count_I6_3 <- (1*365.25-t)}
    else
      nothing_I6_3 <- nothing_I6_3+1
    end
    count_I6_3 <- count_I6_3+1
  }
  
  u <- (t+ART_I6_3+nothing_I6_3)
  I6_ART_day_NO_TCA <- ifelse(ART_I6_3==1, u, NA)
  
  #I6
  count_I6_4 <- 1
  care_I6_4 <- 0
  nothing_I6_4 <- 0
  probability_13 <- runif((1*365.25),0,1)
  P_Care <- ifelse(r==1 & ad==0, I6_T_C_rate, 0)
  P_Nothing <- ifelse(r==1 & ad==0, (1-P_Care),0)
  while (count_I6_4<=(1*365.25)) {
    if (probability_13[count_I6_4]<P_Care)
    {care_I6_4 <- care_I6_4+1
    count_I6_4 <- (1*365.25)}
    else
      nothing_I6_4 <- nothing_I6_4+1
    end
    count_I6_4 <- count_I6_4+1
  }
  
  v <- (care_I6_4+nothing_I6_4)
  I6_care_day_NO_CA <- ifelse(care_I6_4==1, v, NA)
  
  count_I6_5 <- 1
  ART_I6_5 <- 0
  nothing_I6_5 <- 0
  probability_14 <- runif((1*365.25-v),0,1)
  P_ART <- ifelse(r==1 & ad==0, I6_C_A_rate, 0)
  P_Nothing <- ifelse(r==1 & ad==0, (1-P_ART),0)
  while (count_I6_5<=(1*365.25-v)) {
    if (probability_14[count_I6_5]<P_ART)
    {ART_I6_5 <- ART_I6_5+1
    count_I6_5 <- (1*365.25-v)}
    else
      nothing_I6_5 <- nothing_I6_5+1
    end
    count_I6_5 <- count_I6_5+1
  }
  
  w <- (v+ART_I6_5+nothing_I6_5)
  I6_ART_day_NO_CA <- ifelse(ART_I6_5==1, w, NA)
  
  #I6
  count_I6_6 <- 1
  ART_I6_6 <- 0
  nothing_I6_6 <- 0
  probability_15 <- runif((1*365.25),0,1)
  P_ART <- ifelse(r==2 & ad==0, I6_C_A_rate, 0)
  P_Nothing <- ifelse(r==2 & ad==0,(1-P_ART),0)
  while (count_I6_6<=(1*365.25)) {
    if (probability_15[count_I6_6]<P_ART)
    {ART_I6_6 <- ART_I6_6+1
    count_I6_6 <- (1*365.25)}
    else
      nothing_I6_6 <- nothing_I6_6+1
    end
    count_I6_6 <- count_I6_6+1
  }
  
  x <- (ART_I6_6+nothing_I6_6)
  I6_ART_day_NO_A <- ifelse(ART_I6_6==1, x, NA)
  
  I6_test_day <- 
    if(r==0){
      I6_test_day <- I6_test_day_NO_TCA
    }else{
      I6_test_day <- NA
    }
  
  I6_care_day <- 
    if(r==0){
      I6_care_day <- I6_care_day_NO_TCA
    }else if(r==1){
      I6_care_day <- I6_care_day_NO_CA
    }else{
      I6_care_day <- NA
    }
  
  I6_ART_day <- 
    if(r==0){
      I6_ART_day <- I6_ART_day_NO_TCA
    }else if(r==1){
      I6_ART_day <- I6_ART_day_NO_CA
    }else if(r==2){
      I6_ART_day <- I6_ART_day_NO_A
    }else{
      I6_ART_day <- NA
    }
  
  #ART to 45 years old ---acute2
  ART_time <- ifelse(ART_acute2_3==1, (20*365.25+30+acute2_ART_day), (45*365.25))
  count_ART_acute2 <- 1
  contact_ART_acute2 <- 0
  nothing_ART_acute2 <- 0
  infected_ART_acute2 <- 0
  death_ART_acute2 <- 0
  probability_16 <- runif((45*365.25-ART_time),0,1)
  P_Have_Sex <- (contact_rate*0.8)
  P_death <- death_after_A
  P_Nothing <- (1-P_Have_Sex-P_death)   
  P_Oral_Sex <- (1/2)           
  P_Anal_Sex <- (1/2) 
  P_Oral_noprep_infected <- (0.0004*0.04)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- (0.0004*0.04*(1-prep_efficacy*compliance))
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- (0.008*(1-0.4875)*0.04)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- (0.008*(1-0.4875)*0.04*(1-prep_efficacy*compliance))
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_ART_acute2<=(45*365.25-ART_time)) {
    if (probability_16[count_ART_acute2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_ART_acute2 <- contact_ART_acute2+1
    infected_ART_acute2 <- infected_ART_acute2+1}
    else if (probability_16[count_ART_acute2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_ART_acute2 <- contact_ART_acute2+1
    else if (probability_16[count_ART_acute2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_ART_acute2 <- contact_ART_acute2+1
    infected_ART_acute2 <- infected_ART_acute2+1}
    else if (probability_16[count_ART_acute2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_ART_acute2 <- contact_ART_acute2+1
    else if (probability_16[count_ART_acute2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_ART_acute2 <- contact_ART_acute2+1
    infected_ART_acute2 <- infected_ART_acute2+1}
    else if (probability_16[count_ART_acute2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_ART_acute2 <- contact_ART_acute2+1
    else if (probability_16[count_ART_acute2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_ART_acute2 <- contact_ART_acute2+1
    infected_ART_acute2 <- infected_ART_acute2+1}
    else if (probability_16[count_ART_acute2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_ART_acute2 <- contact_ART_acute2+1
    else if (probability_16[count_ART_acute2]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_death)
    {death_ART_acute2 <- death_ART_acute2+1
    count_ART_acute2 <- (45*365.25-ART_time)}
    else
      nothing_ART_acute2 <- nothing_ART_acute2+1
    end
    count_ART_acute2 <- count_ART_acute2+1
  }
  
  #ART to 45 years old ---chronic
  ART_time_chronic_3 <- ifelse(ART_chronic_3==1, (20*365.25+90+chronic_ART_day_NO_TCA), 0)
  ART_time_chronic_5 <- ifelse(ART_chronic_5==1, (20*365.25+90+chronic_ART_day_NO_CA), 0)
  ART_time_chronic_6 <- ifelse(ART_chronic_6==1, (20*365.25+90+chronic_ART_day_NO_A), 0)
  ART_time_chronic <- (ART_time_chronic_3+ART_time_chronic_5+ART_time_chronic_6)
  ART_time <- 
    if(ART_time_chronic==0){
      ART_time <- (45*365.25)
    }else{
      ART_time <- ART_time_chronic
    }
  count_ART_chronic <- 1
  contact_ART_chronic <- 0
  nothing_ART_chronic <- 0
  infected_ART_chronic <- 0
  death_ART_chronic <- 0
  probability_16 <- runif((45*365.25-ART_time),0,1)
  P_Have_Sex <- (contact_rate*0.8)
  P_death <- death_after_A
  P_Nothing <- (1-P_Have_Sex-P_death)
  P_Oral_Sex <- (1/2)           
  P_Anal_Sex <- (1/2) 
  P_Oral_noprep_infected <- (0.0004*0.04)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- (0.0004*0.04*(1-prep_efficacy*compliance))
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- (0.008*(1-0.4875)*0.04)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- (0.008*(1-0.4875)*0.04*(1-prep_efficacy*compliance))
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_ART_chronic<=(45*365.25-ART_time)) {
    if (probability_16[count_ART_chronic]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_ART_chronic <- contact_ART_chronic+1
    infected_ART_chronic <- infected_ART_chronic+1}
    else if (probability_16[count_ART_chronic]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_ART_chronic <- contact_ART_chronic+1
    else if (probability_16[count_ART_chronic]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_ART_chronic <- contact_ART_chronic+1
    infected_ART_chronic <- infected_ART_chronic+1}
    else if (probability_16[count_ART_chronic]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_ART_chronic <- contact_ART_chronic+1
    else if (probability_16[count_ART_chronic]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_ART_chronic <- contact_ART_chronic+1
    infected_ART_chronic <- infected_ART_chronic+1}
    else if (probability_16[count_ART_chronic]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_ART_chronic <- contact_ART_chronic+1
    else if (probability_16[count_ART_chronic]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_ART_chronic <- contact_ART_chronic+1
    infected_ART_chronic <- infected_ART_chronic+1}
    else if (probability_16[count_ART_chronic]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_ART_chronic <- contact_ART_chronic+1
    else if (probability_16[count_ART_chronic]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_death)
    {death_ART_chronic <- death_ART_chronic+1
    count_ART_chronic <- (45*365.25-ART_time)}
    else
      nothing_ART_chronic <- nothing_ART_chronic+1
    end
    count_ART_chronic <- count_ART_chronic+1
  }
  
  #ART to 45 years old ---I5
  ART_time_I5_3 <- ifelse(ART_I5_3==1, (20*365.25+day_AIDS+I5_ART_day_NO_TCA), 0)
  ART_time_I5_5 <- ifelse(ART_I5_5==1, (20*365.25+day_AIDS+I5_ART_day_NO_CA), 0)
  ART_time_I5_6 <- ifelse(ART_I5_6==1, (20*365.25+day_AIDS+I5_ART_day_NO_A), 0)
  ART_time_I5 <- (ART_time_I5_3+ART_time_I5_5+ART_time_I5_6)
  ART_time <- 
    if(ART_time_I5==0){
      ART_time <- (45*365.25)
    }else{
      ART_time <- ART_time_I5
    }
  count_ART_I5 <- 1
  contact_ART_I5 <- 0
  nothing_ART_I5 <- 0
  infected_ART_I5 <- 0
  death_ART_I5 <- 0
  probability_17 <- runif((45*365.25-ART_time),0,1)
  P_Have_Sex <- (contact_rate*0.8)
  P_death <- death_after_A
  P_Nothing <- (1-P_Have_Sex-P_death)             
  P_Oral_Sex <- (1/2)           
  P_Anal_Sex <- (1/2)
  P_Oral_noprep_infected <- (0.0004*0.04)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- (0.0004*0.04*(1-prep_efficacy*compliance))
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- (0.008*(1-0.4875)*0.04)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- (0.008*(1-0.4875)*0.04*(1-prep_efficacy*compliance))
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_ART_I5<=(45*365.25-ART_time)) {
    if (probability_17[count_ART_I5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_ART_I5 <- contact_ART_I5+1
    infected_ART_I5 <- infected_ART_I5+1}
    else if (probability_17[count_ART_I5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_ART_I5 <- contact_ART_I5+1
    else if (probability_17[count_ART_I5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_ART_I5 <- contact_ART_I5+1
    infected_ART_I5 <- infected_ART_I5+1}
    else if (probability_17[count_ART_I5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_ART_I5 <- contact_ART_I5+1
    else if (probability_17[count_ART_I5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_ART_I5 <- contact_ART_I5+1
    infected_ART_I5 <- infected_ART_I5+1}
    else if (probability_17[count_ART_I5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_ART_I5 <- contact_ART_I5+1
    else if (probability_17[count_ART_I5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_ART_I5 <- contact_ART_I5+1
    infected_ART_I5 <- infected_ART_I5+1}
    else if (probability_17[count_ART_I5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_ART_I5 <- contact_ART_I5+1
    else if (probability_17[count_ART_I5]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_death)
    {death_ART_I5 <- death_ART_I5+1
    count_ART_I5 <- (45*365.25-ART_time)}
    else
      nothing_ART_I5 <- nothing_ART_I5+1
    end
    count_ART_I5 <- count_ART_I5+1
  }
  
  #ART to 45 years old ---I6
  ART_time_I6_3 <- ifelse(ART_I6_3==1, (20*365.25+day_AIDS+1.6*365.25+I6_ART_day_NO_TCA), 0)
  ART_time_I6_5 <- ifelse(ART_I6_5==1, (20*365.25+day_AIDS+1.6*365.25+I6_ART_day_NO_CA), 0)
  ART_time_I6_6 <- ifelse(ART_I6_6==1, (20*365.25+day_AIDS+1.6*365.25+I6_ART_day_NO_A), 0)
  ART_time_I6 <- (ART_time_I6_3+ART_time_I6_5+ART_time_I6_6)
  ART_time <- 
    if(ART_time_I6==0){
      ART_time <- (45*365.25)
    }else{
      ART_time <- ART_time_I6
    }
  count_ART_I6 <- 1
  contact_ART_I6 <- 0
  nothing_ART_I6 <- 0
  infected_ART_I6 <- 0
  death_ART_I6 <- 0
  probability_18 <- runif((45*365.25-ART_time),0,1)
  P_Have_Sex <- (contact_rate*0.8)
  P_death <- death_I6_after_A
  P_Nothing <- (1-P_Have_Sex-P_death)
  P_Oral_Sex <- (1/2)           
  P_Anal_Sex <- (1/2)
  P_Oral_noprep_infected <- (0.0004*0.04)
  P_Oral_noprep_uninfected <- (1-P_Oral_noprep_infected)
  P_Oral_prep_infected <- (0.0004*0.04*(1-prep_efficacy*compliance))
  P_Oral_prep_uninfected <- (1-P_Oral_prep_infected)                               
  P_Anal_noprep_infected <- (0.008*(1-0.4875)*0.04)
  P_Anal_noprep_uninfected <- (1-P_Anal_noprep_infected)
  P_Anal_prep_infected <- (0.008*(1-0.4875)*0.04*(1-prep_efficacy*compliance))
  P_Anal_prep_uninfected <- (1-P_Anal_prep_infected)
  while (count_ART_I6<=(45*365.25-ART_time)) {
    if (probability_18[count_ART_I6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected)
    {contact_ART_I6 <- contact_ART_I6+1
    infected_ART_I6 <- infected_ART_I6+1}
    else if (probability_18[count_ART_I6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected)
      contact_ART_I6 <- contact_ART_I6+1
    else if (probability_18[count_ART_I6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected)
    {contact_ART_I6 <- contact_ART_I6+1
    infected_ART_I6 <- infected_ART_I6+1}
    else if (probability_18[count_ART_I6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected)
      contact_ART_I6 <- contact_ART_I6+1
    else if (probability_18[count_ART_I6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected)
    {contact_ART_I6 <- contact_ART_I6+1
    infected_ART_I6 <- infected_ART_I6+1}
    else if (probability_18[count_ART_I6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected)
      contact_ART_I6 <- contact_ART_I6+1
    else if (probability_18[count_ART_I6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected)
    {contact_ART_I6 <- contact_ART_I6+1
    infected_ART_I6 <- infected_ART_I6+1}
    else if (probability_18[count_ART_I6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected)
      contact_ART_I6 <- contact_ART_I6+1
    else if (probability_18[count_ART_I6]<P_Have_Sex*P_Oral_Sex*no_prep*P_Oral_noprep_infected+ P_Have_Sex*P_Oral_Sex*no_prep* P_Oral_noprep_uninfected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_infected+ P_Have_Sex*P_Oral_Sex* prep* P_Oral_prep_uninfected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_infected+ P_Have_Sex*P_Anal_Sex* no_prep* P_Anal_noprep_uninfected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_infected+ P_Have_Sex*P_Anal_Sex* prep* P_Anal_prep_uninfected+P_death)
    {death_ART_I6 <- death_ART_I6+1
    count_ART_I6 <- (45*365.25-ART_time)}
    else
      nothing_ART_I6 <- nothing_ART_I6+1
    end
    count_ART_I6 <- count_ART_I6+1
  }
  
  contact_ART <- (contact_ART_acute2+contact_ART_chronic+contact_ART_I5+contact_ART_I6)
  infected_ART <- (infected_ART_acute2+infected_ART_chronic+infected_ART_I5+infected_ART_I6)
  
  #sum
  contact <- (contact_acute1+contact_acute2+contact_chronic+contact_I5+contact_ART)
  infected <- (infected_acute1+infected_acute2+infected_chronic+infected_I5+infected_ART)
  
  outcome <- c(number,contact, infected)
  print(outcome)
  sum_contact <- sum_contact+contact
  sum_infected <- sum_infected+infected
  acute_infected <- acute_infected+infected_acute1+infected_acute2
  
  zz[number] <- infected
}

(R0 <- (sum_infected/number))
(acute_mean_infected <- (acute_infected/number))

print(zz)
sd(zz)
(lower <- (R0-1.96*sd(zz)/sqrt(number)))
(upper <- (R0+1.96*sd(zz)/sqrt(number)))

