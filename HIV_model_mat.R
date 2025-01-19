HIV_mat <- function(times, y, parms, t1){ 
  id <- min(which(times < t1$times))-1
  
  parms <- t1[id, ]
 
  with(as.list(c(y,parms)), { 
    S <-   matrix(y[1:9], nrow = 9, ncol=1)
    P0 <-  matrix(y[10:18], nrow = 9, ncol=1)
    I1 <-  matrix(y[19:27], nrow = 9, ncol=1)
    I1c <- matrix(y[28:36], nrow = 9, ncol=1)
    I2 <-  matrix(y[37:45], nrow = 9, ncol=1)
    I3 <-  matrix(y[46:54], nrow = 9, ncol=1)
    I4 <-  matrix(y[55:63], nrow = 9, ncol=1)
    I5 <-  matrix(y[64:72], nrow = 9, ncol=1)
    I6 <-  matrix(y[73:81], nrow = 9, ncol=1)
    
    P1 <-  matrix(y[82:90], nrow = 9, ncol=1)
    P1c <- matrix(y[91:99], nrow = 9, ncol=1)
    P2 <-  matrix(y[100:108], nrow = 9, ncol=1)
    P3 <-  matrix(y[109:117], nrow = 9, ncol=1)
    P4 <-  matrix(y[118:126], nrow = 9, ncol=1)
    P5 <-  matrix(y[127:135], nrow = 9, ncol=1)
    P6 <-  matrix(y[136:144], nrow = 9, ncol=1)
    
    T1 <-  matrix(y[145:153], nrow = 9, ncol=1)
    T1c <- matrix(y[154:162], nrow = 9, ncol=1)
    T2 <-  matrix(y[163:171], nrow = 9, ncol=1)
    T3 <-  matrix(y[172:180], nrow = 9, ncol=1)
    T4 <-  matrix(y[181:189], nrow = 9, ncol=1)
    T5 <-  matrix(y[190:198], nrow = 9, ncol=1)
    T6 <-  matrix(y[199:207], nrow = 9, ncol=1)
    
    C1 <-  matrix(y[208:216], nrow = 9, ncol=1)
    C1c <- matrix(y[217:225], nrow = 9, ncol=1)
    C2 <-  matrix(y[226:234], nrow = 9, ncol=1)
    C3 <-  matrix(y[235:243], nrow = 9, ncol=1)
    C4 <-  matrix(y[244:252], nrow = 9, ncol=1)
    C5 <-  matrix(y[253:261], nrow = 9, ncol=1)
    C6 <-  matrix(y[262:270], nrow = 9, ncol=1)
    
    A1 <-  matrix(y[271:279], nrow = 9, ncol=1)
    A1c <- matrix(y[280:288], nrow = 9, ncol=1)
    A2 <-  matrix(y[289:297], nrow = 9, ncol=1)
    A3 <-  matrix(y[298:306], nrow = 9, ncol=1)
    A4 <-  matrix(y[307:315], nrow = 9, ncol=1)
    A5 <-  matrix(y[316:324], nrow = 9, ncol=1)
    A6 <-  matrix(y[325:333], nrow = 9, ncol=1)
    
    
    
    #aggegrate infection by risk and age groups 

    sum_I <- I1 + I1c + I2 + I3 + I4 + I5 + I6
    
    sum_P <- P1 + P1c + P2 + P3 + P4 + P5 + P6
    
    sum_T <- T1 + T1c + T2 + T3 + T4 + T5 + T6
    
    sum_C <- C1 + C1c + C2 + C3 + C4 + C5 + C6
    
    sum_A <- A1 + A1c + A2 + A3 + A4 + A5 + A6 
    
    # total number living with HIV  
    ## aged 15-44 
    sum_infection <- sum_I + sum_P + sum_T + sum_C + sum_A 
    
    # flate the matrices 
    # I
    ## aged 15-44 
    sum_I_H <- I1_HH + I1c_HH + I2_HH  + I3_HH + I4_HH + I5_HH + I6_HH
    sum_I_L <- I1_LL + I1c_LL + I2_LL  + I3_LL + I4_LL + I5_LL + I6_LL
    sum_I_HL <- I1_HL + I1c_HL + I2_HL  + I3_HL + I4_HL + I5_HL + I6_HL
    
    ## aged 45-64 
    sum_Ia_H <- I1a_HH + I1ca_HH + I2a_HH  + I3a_HH + I4a_HH + I5a_HH + I6a_HH
    sum_Ia_L <- I1a_LL + I1ca_LL + I2a_LL  + I3a_LL + I4a_LL + I5a_LL + I6a_LL
    sum_Ia_HL <- I1a_HL + I1ca_HL + I2a_HL  + I3a_HL + I4a_HL + I5a_HL + I6a_HL
    
    ## aged 65-
    sum_Iaa_H <- I1aa_HH + I1caa_HH + I2aa_HH  + I3aa_HH + I4aa_HH + I5aa_HH + I6aa_HH
    sum_Iaa_L <- I1aa_LL + I1caa_LL + I2aa_LL  + I3aa_LL + I4aa_LL + I5aa_LL + I6aa_LL
    sum_Iaa_HL <- I1aa_HL + I1caa_HL + I2aa_HL  + I3aa_HL + I4aa_HL + I5aa_HL + I6aa_HL
    
    # T
    ## aged 15-44 
    sum_T_H <- T1_HH + T1c_HH + T2_HH  + T3_HH + T4_HH + T5_HH + T6_HH
    sum_T_L <- T1_LL + T1c_LL + T2_LL  + T3_LL + T4_LL + T5_LL + T6_LL
    sum_T_HL <- T1_HL + T1c_HL + T2_HL  + T3_HL + T4_HL + T5_HL + T6_HL
    
    ## aged 45-64 
    sum_Ta_H <- T1a_HH + T1ca_HH + T2a_HH  + T3a_HH + T4a_HH + T5a_HH + T6a_HH
    sum_Ta_L <- T1a_LL + T1ca_LL + T2a_LL  + T3a_LL + T4a_LL + T5a_LL + T6a_LL
    sum_Ta_HL <- T1a_HL + T1ca_HL + T2a_HL  + T3a_HL + T4a_HL + T5a_HL + T6a_HL
    
    ## aged 65- 
    sum_Taa_H <- T1aa_HH + T1caa_HH + T2aa_HH  + T3aa_HH + T4aa_HH + T5aa_HH + T6aa_HH
    sum_Taa_L <- T1aa_LL + T1caa_LL + T2aa_LL  + T3aa_LL + T4aa_LL + T5aa_LL + T6aa_LL
    sum_Taa_HL <- T1aa_HL + T1caa_HL + T2aa_HL  + T3aa_HL + T4aa_HL + T5aa_HL + T6aa_HL
    
    # C
    ## aged 15-44 
    sum_C_H <- C1_HH + C1c_HH + C2_HH  + C3_HH + C4_HH + C5_HH + C6_HH
    sum_C_L <- C1_LL + C1c_LL + C2_LL  + C3_LL + C4_LL + C5_LL + C6_LL
    sum_C_HL <- C1_HL + C1c_HL + C2_HL  + C3_HL + C4_HL + C5_HL + C6_HL
    
    ## aged 45-64 
    sum_Ca_H <- C1a_HH + C1ca_HH + C2a_HH  + C3a_HH + C4a_HH + C5a_HH + C6a_HH
    sum_Ca_L <- C1a_LL + C1ca_LL + C2a_LL  + C3a_LL + C4a_LL + C5a_LL + C6a_LL
    sum_Ca_HL <- C1a_HL + C1ca_HL + C2a_HL  + C3a_HL + C4a_HL + C5a_HL + C6a_HL
    
    ## aged 65-
    sum_Caa_H <- C1aa_HH + C1caa_HH + C2aa_HH  + C3aa_HH + C4aa_HH + C5aa_HH + C6aa_HH
    sum_Caa_L <- C1aa_LL + C1caa_LL + C2aa_LL  + C3aa_LL + C4aa_LL + C5aa_LL + C6aa_LL
    sum_Caa_HL <- C1aa_HL + C1caa_HL + C2aa_HL  + C3aa_HL + C4aa_HL + C5aa_HL + C6aa_HL
    
    # A
    ## aged 15-44 
    sum_A_H <- A1_HH + A1c_HH + A2_HH  + A3_HH + A4_HH + A5_HH + A6_HH
    sum_A_L <- A1_LL + A1c_LL + A2_LL  + A3_LL + A4_LL + A5_LL + A6_LL
    sum_A_HL <- A1_HL + A1c_HL + A2_HL  + A3_HL + A4_HL + A5_HL + A6_HL
    
    ## aged 45-64 
    sum_Aa_H <- A1a_HH + A1ca_HH + A2a_HH  + A3a_HH + A4a_HH + A5a_HH + A6a_HH
    sum_Aa_L <- A1a_LL + A1ca_LL + A2a_LL  + A3a_LL + A4a_LL + A5a_LL + A6a_LL
    sum_Aa_HL <- A1a_HL + A1ca_HL + A2a_HL  + A3a_HL + A4a_HL + A5a_HL + A6a_HL
    
    ## aged 65-
    sum_Aaa_H <- A1aa_HH + A1caa_HH + A2aa_HH  + A3aa_HH + A4aa_HH + A5aa_HH + A6aa_HH
    sum_Aaa_L <- A1aa_LL + A1caa_LL + A2aa_LL  + A3aa_LL + A4aa_LL + A5aa_LL + A6aa_LL
    sum_Aaa_HL <- A1aa_HL + A1caa_HL + A2aa_HL  + A3aa_HL + A4aa_HL + A5aa_HL + A6aa_HL
    
    # P_infection 
    ## aged 15-44 
    sum_P_infection_H <- P1_HH + P1c_HH + P2_HH  + P3_HH + P4_HH + P5_HH + P6_HH
    sum_P_infection_L <- P1_LL + P1c_LL + P2_LL  + P3_LL + P4_LL + P5_LL + P6_LL
    sum_P_infection_HL <- P1_HL + P1c_HL + P2_HL  + P3_HL + P4_HL + P5_HL + P6_HL
    
    ## aged 45-64 
    sum_P_infection_a_H <- P1a_HH + P1ca_HH + P2a_HH  + P3a_HH + P4a_HH + P5a_HH + P6a_HH
    sum_P_infection_a_L <- P1a_LL + P1ca_LL + P2a_LL  + P3a_LL + P4a_LL + P5a_LL + P6a_LL
    sum_P_infection_a_HL <- P1a_HL + P1ca_HL + P2a_HL  + P3a_HL + P4a_HL + P5a_HL + P6a_HL
    
    ## aged 65- 
    sum_P_infection_aa_H <- P1aa_HH + P1caa_HH + P2aa_HH  + P3aa_HH + P4aa_HH + P5aa_HH + P6aa_HH
    sum_P_infection_aa_L <- P1aa_LL + P1caa_LL + P2aa_LL  + P3aa_LL + P4aa_LL + P5aa_LL + P6aa_LL
    sum_P_infection_aa_HL <- P1aa_HL + P1caa_HL + P2aa_HL  + P3aa_HL + P4aa_HL + P5aa_HL + P6aa_HL
    
    # total number living with HIV  
    ## aged 15-44 
    sum_infection_H <- sum_I_H + sum_T_H + sum_C_H + sum_A_H + sum_P_infection_H  
    sum_infection_L <- sum_I_L + sum_T_L + sum_C_L + sum_A_L + sum_P_infection_L  
    sum_infection_HL <- sum_I_HL + sum_T_HL + sum_C_HL + sum_A_HL + sum_P_infection_HL  
    
    ## aged 45-64 
    sum_infection_a_H <- sum_Ia_H + sum_Ta_H + sum_Ca_H + sum_Aa_H + sum_P_infection_a_H  
    sum_infection_a_L <- sum_Ia_L + sum_Ta_L + sum_Ca_L + sum_Aa_L + sum_P_infection_a_L  
    sum_infection_a_HL <- sum_Ia_HL + sum_Ta_HL + sum_Ca_HL + sum_Aa_HL + sum_P_infection_a_HL  
    
    ## aged 65-
    sum_infection_aa_H <- sum_Iaa_H + sum_Taa_H + sum_Caa_H + sum_Aaa_H + sum_P_infection_aa_H  
    sum_infection_aa_L <- sum_Iaa_L + sum_Taa_L + sum_Caa_L + sum_Aaa_L + sum_P_infection_aa_L  
    sum_infection_aa_HL <- sum_Iaa_HL + sum_Taa_HL + sum_Caa_HL + sum_Aaa_HL + sum_P_infection_aa_HL 
    
    
    fifteen <- population_15y*porprotion 
    beta_HL <- 0.5*beta_HH
    beta_LL <- 0.1*beta_HH 
    beta_aHaH <- 0.4*initial_Beta
    beta_aHaL <- 0.2*initial_Beta
    beta_aLaL <- 0.04*initial_Beta
    beta_aaHaaH <- 0.01*beta_fixed
    beta_aaHaaL <- 0.01*beta_fixed
    beta_aaLaaL <- 0.01*beta_fixed
    #### flow #### 
    ##### Entry #####
    Enter_H <- 0.01*fifteen*high_risk
    Enter_L <- fifteen*(1-high_risk)
    Enter_HL <- 0.99*fifteen*high_risk
    ##### PrEP off #####
    out_of_PrEP_H <- P0_HH*duration_PrEP_H*PrEP_START
    out_of_PrEP_L <- P0_LL*duration_PrEP_L*PrEP_START
    out_of_PrEP_HL <- P0_HL*duration_PrEP_HL*PrEP_START
    
    ##### PrEP on #####
    Enrolled_to_PrEP_H <- S_HH*enrolled_PrEP_H*PrEP_START
    Enrolled_to_PrEP_L <- S_LL*enrolled_PrEP_L*PrEP_START
    Enrolled_to_PrEP_HL <- S_HL*enrolled_PrEP_HL*PrEP_START
    
    
    ##### death  #####
    ###### S & PrEP0 ######
    death_S_H <- S_HH*u
    death_S_L <- S_LL*u
    death_S_HL <- S_HL*u
    
    death_Sa_H <- Sa_HH*ua
    death_Sa_L <- Sa_LL*ua
    death_Sa_HL <- Sa_HL*ua
    
    death_Saa_H <- Saa_HH*uaa
    death_Saa_L <- Saa_LL*uaa
    death_Saa_HL <- Saa_HL*uaa 
    
    death_P0_H <- P0_HH*u
    death_P0_L <- P0_LL*u
    death_P0_HL <- P0_HL*u
    
    death_P0a_H <- P0a_HH*ua
    death_P0a_L <- P0a_LL*ua
    death_P0a_HL <- P0a_HL*ua
    
    death_P0aa_H <- P0aa_HH*uaa
    death_P0aa_L <- P0aa_LL*uaa
    death_P0aa_HL <- P0aa_HL*uaa 
    
    # death 
    death_I12 <- matrix(c(Rdeath_I1_I2, Rdeath_I1_I2, Rdeath_I1_I2, 
                           Rdeath_I1a_I2a, Rdeath_I1a_I2a, Rdeath_I1a_I2a, 
                           Rdeath_I1aa_I2aa, Rdeath_I1aa_I2aa, Rdeath_I1aa_I2aa), 
                        nrow = 9, ncol=1)

    death_I3 <- matrix(c(Rdeath_I3, Rdeath_I3, Rdeath_I3, 
                         Rdeath_I3a, Rdeath_I3a, Rdeath_I3a, 
                         Rdeath_I3aa, Rdeath_I3aa, Rdeath_I3aa), 
                       nrow = 9, ncol=1)
    
    death_I45 <- matrix(c(Rdeath_I4_I5, Rdeath_I4_I5, Rdeath_I4_I5, 
                            Rdeath_I4a_I5a, Rdeath_I4a_I5a, Rdeath_I4a_I5a, 
                            Rdeath_I4aa_I5aa, Rdeath_I4aa_I5aa, Rdeath_I4aa_I5aa), 
                       nrow = 9, ncol=1)
    
    death_I6 <- matrix(c(Rdeath_I6, Rdeath_I6, Rdeath_I6, 
                         Rdeath_I6a, Rdeath_I6a, Rdeath_I6a, 
                         Rdeath_I6aa, Rdeath_I6aa, Rdeath_I6aa), 
                       nrow = 9, ncol=1)
    
    death_T12C12 <- matrix(c(Rdeath_T1C1_T2C2, Rdeath_T1C1_T2C2, Rdeath_T1C1_T2C2, 
                             Rdeath_T1aC1a_T2aC2a, Rdeath_T1aC1a_T2aC2a, Rdeath_T1aC1a_T2aC2a, 
                             Rdeath_T1aaC1aa_T2aaC2aa, Rdeath_T1aaC1aa_T2aaC2aa, Rdeath_T1aaC1aa_T2aaC2aa), 
                           nrow = 9, ncol=1)
    
    death_T3C3 <- matrix(c(Rdeath_T3C3, Rdeath_T3C3, Rdeath_T3C3, 
                           Rdeath_T3aC3a, Rdeath_T3aC3a, Rdeath_T3aC3a, 
                           Rdeath_T3aaC3aa, Rdeath_T3aaC3aa, Rdeath_T3aaC3aa), 
                         nrow = 9, ncol=1)
    
    death_T45C45 <- matrix(c(Rdeath_T4C4_T5C5, Rdeath_T4C4_T5C5, Rdeath_T4C4_T5C5, 
                             Rdeath_T4aC4a_T5aC5a, Rdeath_T4aC4a_T5aC5a, Rdeath_T4aC4a_T5aC5a, 
                             Rdeath_T4aaC4aa_T5aaC5aa, Rdeath_T4aaC4aa_T5aaC5aa, Rdeath_T4aaC4aa_T5aaC5aa), 
                           nrow = 9, ncol=1)
    
    death_T6C6 <- matrix(c(Rdeath_T6C6, Rdeath_T6C6, Rdeath_T6C6, 
                           Rdeath_T6aC6a, Rdeath_T6aC6a, Rdeath_T6aC6a, 
                           Rdeath_T6aaC6aa, Rdeath_T6aaC6aa, Rdeath_T6aaC6aa), 
                         nrow = 9, ncol=1) 
    
    
    death_A12 <- matrix(c(Rdeath_A1A2, Rdeath_A1A2, Rdeath_A1A2, 
                          Rdeath_A1aA2a, Rdeath_A1aA2a, Rdeath_A1aA2a, 
                          Rdeath_A1aaA2aa, Rdeath_A1aaA2aa, Rdeath_A1aaA2aa), 
                        nrow = 9, ncol=1)
    
    death_A3 <- matrix(c(Rdeath_A3, Rdeath_A3, Rdeath_A3, 
                         Rdeath_A3a, Rdeath_A3a, Rdeath_A3a, 
                         Rdeath_A3aa, Rdeath_A3aa, Rdeath_A3aa), 
                       nrow = 9, ncol=1)
    
    death_A45 <- matrix(c(Rdeath_A4A5, Rdeath_A4A5, Rdeath_A4A5, 
                          Rdeath_A4aA5a, Rdeath_A4aA5a, Rdeath_A4aA5a, 
                          Rdeath_A4aaA5aa, Rdeath_A4aaA5aa, Rdeath_A4aaA5aa), 
                        nrow = 9, ncol=1)
    
    death_A6 <- matrix(c(Rdeath_A6, Rdeath_A6, Rdeath_A6, 
                         Rdeath_A6a, Rdeath_A6a, Rdeath_A6a, 
                         Rdeath_A6aa, Rdeath_A6aa, Rdeath_A6aa), 
                       nrow = 9, ncol=1)
    
    
    
    ###### I ######
    death_I1_H <- I1_HH*Rdeath_I1_I2
    death_I1_HL <- I1_HL*Rdeath_I1_I2 
    death_I1_L <- I1_LL*Rdeath_I1_I2
    
    death_I1a_H <- I1a_HH*Rdeath_I1a_I2a
    death_I1a_HL <- I1a_HL*Rdeath_I1a_I2a
    death_I1a_L <- I1a_LL*Rdeath_I1a_I2a
    
    death_I1aa_H <- I1aa_HH*Rdeath_I1aa_I2aa
    death_I1aa_HL <- I1aa_HL*Rdeath_I1aa_I2aa
    death_I1aa_L <- I1aa_LL*Rdeath_I1aa_I2aa
    
    death_P1_H <- P1_HH*Rdeath_I1_I2
    death_P1_HL <- P1_HL*Rdeath_I1_I2
    death_P1_L <- P1_LL*Rdeath_I1_I2
    
    death_P1a_H <- P1a_HH*Rdeath_I1a_I2a
    death_P1a_HL <- P1a_HL*Rdeath_I1a_I2a
    death_P1a_L <- P1a_LL*Rdeath_I1a_I2a
    
    death_P1aa_H <- P1aa_HH*Rdeath_I1aa_I2aa
    death_P1aa_HL <- P1aa_HL*Rdeath_I1aa_I2aa
    death_P1aa_L <- P1aa_LL*Rdeath_I1aa_I2aa
    
    
    # aging 
    aging_S_H <- S_HH*aging1
    aging_S_HL <- S_HL*aging1
    aging_S_L <- S_LL*aging1
    
    aging_Sa_H <- Sa_HH*aging2
    aging_Sa_HL <- Sa_HL*aging2
    aging_Sa_L <- Sa_LL*aging2
    
    aging_P0_H <- P0_HH*aging1
    aging_P0_HL <- P0_HL*aging1
    aging_P0_L <- P0_LL*aging1
    
    aging_P0a_H <- P0a_HH*aging2
    aging_P0a_HL <- P0a_HL*aging2
    aging_P0a_L <- P0a_LL*aging2
    
    ###### I ###### 
    aging_I1_H <- I1_HH*aging1
    aging_I1_HL <- I1_HL*aging1
    aging_I1_L <- I1_LL*aging1
    
    aging_I1a_H <- I1a_HH*aging2
    aging_I1a_HL <- I1a_HL*aging2
    aging_I1a_L <- I1a_LL*aging2
    
    ###### P ###### 
    aging_P1_H <- P1_HH*aging1
    aging_P1_HL <- P1_HL*aging1
    aging_P1_L <- P1_LL*aging1
    
    aging_P1a_H <- P1a_HH*aging2
    aging_P1a_HL <- P1a_HL*aging2
    aging_P1a_L <- P1a_LL*aging2
    
    # rest of aging in the formula 
    aging_in <- matrix(c(0,0,0,aging1, aging1, aging1, aging2, aging2, aging2), 
                       nrow = 9, ncol=1)
    
    aging_out <- matrix(c(aging1, aging1, aging1, aging2, aging2, aging2, 
                          0,0,0), 
                       nrow = 9, ncol=1)
    
    # disease progression 
    ##### disease progression ##### 
    ###### I ###### 
    I1_to_I1c_H <- I1_HH*zoe_1
    I1_to_I1c_HL <- I1_HL*zoe_1
    I1_to_I1c_L <- I1_LL*zoe_1
    
    I1a_to_I1ca_H <-  I1a_HH*zoe_1a
    I1a_to_I1ca_HL <-  I1a_HL*zoe_1a
    I1a_to_I1ca_L <-  I1a_LL*zoe_1a
    
    I1aa_to_I1caa_H <-  I1aa_HH*zoe_1aa
    I1aa_to_I1caa_HL <-  I1aa_HL*zoe_1aa
    I1aa_to_I1caa_L <-  I1aa_LL*zoe_1aa
    
    ###### P ###### 
    P1_to_P1c_H <- P1_HH*zoe_p1
    P1_to_P1c_HL <- P1_HL*zoe_p1
    P1_to_P1c_L <- P1_LL*zoe_p1
    
    P1a_to_P1ca_H <-  P1a_HH*zoe_p1
    P1a_to_P1ca_HL <-  P1a_HL*zoe_p1
    P1a_to_P1ca_L <-  P1a_LL*zoe_p1
    
    P1aa_to_P1caa_H <-  P1aa_HH*zoe_p1
    P1aa_to_P1caa_HL <-  P1aa_HL*zoe_p1
    P1aa_to_P1caa_L <-  P1aa_LL*zoe_p1
    
    disprog1 <- matrix(c(zoe_1, zoe_1, zoe_1, zoe_1a, zoe_1a, zoe_1a, 
                          zoe_1aa, zoe_1aa, zoe_1aa), 
                        nrow = 9, ncol=1)
    
    disprog1c <- matrix(c(zoe_1c, zoe_1c, zoe_1c, zoe_1ca, zoe_1ca, zoe_1ca, 
                         zoe_1caa, zoe_1caa, zoe_1caa), 
                       nrow = 9, ncol=1)
    
    disprog2 <- matrix(c(zoe_2, zoe_2, zoe_2, zoe_2a, zoe_2a, zoe_2a, 
                          zoe_2aa, zoe_2aa, zoe_2aa), 
                        nrow = 9, ncol=1)
    
    disprog3 <- matrix(c(zoe_3, zoe_3, zoe_3, zoe_3a, zoe_3a, zoe_3a, 
                         zoe_3aa, zoe_3aa, zoe_3aa), 
                       nrow = 9, ncol=1)
    
    disprog4 <- matrix(c(zoe_4, zoe_4, zoe_4, zoe_4a, zoe_4a, zoe_4a, 
                         zoe_4aa, zoe_4aa, zoe_4aa), 
                       nrow = 9, ncol=1)
    
    disprog5 <- matrix(c(zoe_5, zoe_5, zoe_5, zoe_5a, zoe_5a, zoe_5a, 
                         zoe_5aa, zoe_5aa, zoe_5aa), 
                       nrow = 9, ncol=1)

    # cascade 
    #### test ####
    test_1_H <- tau1*I1_HH
    test_1_HL <- tau1*I1_HL
    test_1_L <- tau1*I1_LL
    
    test_1a_H <- tau1*I1a_HH
    test_1a_HL <- tau1*I1a_HL
    test_1a_L <- tau1*I1a_LL
    
    test_1aa_H <- tau1*I1aa_HH
    test_1aa_HL <- tau1*I1aa_HL 
    test_1aa_L <- tau1*I1aa_LL
    
    # testing flows of HIV+ and on PrEP 
    test_P1_H <- P1_HH*tau_p1 
    test_P1_HL <- P1_HL*tau_p1
    test_P1_L <- P1_LL*tau_p1
    
    test_P1a_H <- P1a_HH*tau_p1 
    test_P1a_HL <- P1a_HL*tau_p1
    test_P1a_L <- P1a_LL*tau_p1
    
    test_P1aa_H <- P1aa_HH*tau_p1 
    test_P1aa_HL <- P1aa_HL*tau_p1
    test_P1aa_L <- P1aa_LL*tau_p1
    
    t_1 <- matrix(c(test_1_H, test_1_HL, test_1_L,
                       test_1a_H, test_1a_HL, test_1a_L,
                       test_1aa_H, test_1aa_HL, test_1aa_L), 
                     nrow = 9, ncol=1) 
    test_1 <- matrix(c(tau1, tau1, tau1,
                       tau1, tau1, tau1,
                       tau1, tau1, tau1), nrow = 9, ncol=1) 
    
    test_1c <- matrix(c((0.95*tau1cp+0.05)*tau2_H, (0.95*tau1cp+0.05)*tau2_H, 
                        (0.95*tau1cp+0.05)*tau2_L,
                        tau1ca_H, tau1ca_L, tau1ca_L,
                        tau1caa_H, tau1caa_L, tau1caa_L), 
                      nrow = 9, ncol=1) 
    
    test_AP1c <- matrix(c(test_Aptau_H*proportion, test_Aptau_H*proportion, 
                          test_Aptau_L*proportion, 
                          0, 0, 0,
                          0, 0, 0), 
                        nrow = 9, ncol=1) 
    
    test_2 <- matrix(c(tau2_H, tau2_H, tau2_L,
                       tau_a_H, tau_a_L, tau_a_L,
                       tau_aa_H, tau_aa_L, tau_aa_L), 
                     nrow = 9, ncol=1)
    
    test_AP <- matrix(c(test_Aptau_H, test_Aptau_H, 
                         test_Aptau_L, 
                          0, 0, 0,
                          0, 0, 0), 
                        nrow = 9, ncol=1) 
    
    test_3 <- matrix(c(tau3_H, tau3_H, tau3_L,
                       tau_a_H, tau_a_L, tau_a_L,
                       tau_aa_H, tau_aa_L, tau_aa_L), 
                     nrow = 9, ncol=1)
    
    test_4 <- matrix(c(tau4_H, tau4_H, tau4_L,
                       tau_a_H, tau_a_L, tau_a_L,
                       tau_aa_H, tau_aa_L, tau_aa_L), 
                     nrow = 9, ncol=1)
    
    test_5 <- matrix(c(tau5_H, tau5_H, tau5_L,
                       tau_a_H, tau_a_L, tau_a_L,
                       tau_aa_H, tau_aa_L, tau_aa_L), 
                     nrow = 9, ncol=1)
    
    test_6 <- matrix(c(tau_6, tau_6, tau_6, tau_6, tau_6, tau_6, tau_6, tau_6, tau_6), 
                     nrow = 9, ncol=1) 
    
    test_P1 <- matrix(c(tau_p1, tau_p1, tau_p1, tau_p1, tau_p1, tau_p1, 
                        tau_p1, tau_p1, tau_p1), 
                       nrow = 9, ncol=1)
    
    test_P1c <- matrix(c(tau_p1c, tau_p1c, tau_p1c, tau1ca_H, tau1ca_L, tau1ca_L, 
                      tau1caa_H, tau1caa_L, tau1caa_L), 
                    nrow = 9, ncol=1)
    
    test_P2 <- matrix(c(tau_p2, tau_p2 , tau_p2, 
                        tau_a_H, tau_a_L, tau_a_L,
                        tau_aa_H, tau_aa_L, tau_aa_L),nrow = 9, ncol=1)
    
    test_P3 <- matrix(c(tau_p3, tau_p3, tau_p3, 
                        tau_a_H, tau_a_L, tau_a_L,
                        tau_aa_H, tau_aa_L, tau_aa_L),nrow = 9, ncol=1)
    
    test_P4 <- matrix(c(tau_p4, tau_p4 , tau_p4, 
                        tau_a_H, tau_a_L, tau_a_L,
                        tau_aa_H, tau_aa_L, tau_aa_L),nrow = 9, ncol=1)
    
    test_P5 <- matrix(c(tau_p5, tau_p5 , tau_p5, 
                        tau_a_H, tau_a_L, tau_a_L,
                        tau_aa_H, tau_aa_L, tau_aa_L),nrow = 9, ncol=1)
    
    test_P6 <- matrix(c(tau_p6, tau_p6 , tau_p6, 
                        tau_p6, tau_p6 , tau_p6, 
                        tau_p6, tau_p6 , tau_p6),nrow = 9, ncol=1)
    
    
    
    
    ###### link to care flow ######
    care_T1 <- matrix(c(sigma1, sigma1, sigma1,
                        sigma1, sigma1, sigma1, 
                        sigma1, sigma1, sigma1), nrow = 9, ncol=1) 
    
    care_T1c <- matrix(c(sigma1c, sigma1c, sigma1c,
                        sigma1c, sigma1c, sigma1c, 
                        sigma1c, sigma1c, sigma1c), nrow = 9, ncol=1) 
    
    care_T2 <- matrix(c(sigma2, sigma2, sigma2,
                         sigma2, sigma2, sigma2, 
                         sigma2, sigma2, sigma2), nrow = 9, ncol=1) 
    
    care_T3 <- matrix(c(sigma3, sigma3, sigma3,
                        sigma3, sigma3, sigma3, 
                        sigma3, sigma3, sigma3), nrow = 9, ncol=1) 
    
    care_T4 <- matrix(c(sigma4, sigma4, sigma4,
                        sigma4, sigma4, sigma4, 
                        sigma4, sigma4, sigma4), nrow = 9, ncol=1) 
    
    care_T5 <- matrix(c(sigma5, sigma5, sigma5,
                        sigma5, sigma5, sigma5, 
                        sigma5, sigma5, sigma5), nrow = 9, ncol=1) 
    
    care_T6 <- matrix(c(sigma6, sigma6, sigma6,
                        sigma6, sigma6, sigma6, 
                        sigma6, sigma6, sigma6), nrow = 9, ncol=1)
    
    
    
    ###### ART initiation ######
    treatment_C1 <- matrix(c(gamma1*ARTstart, gamma1*ARTstart, gamma1*ARTstart,
                             gamma1*ARTstart, gamma1*ARTstart, gamma1*ARTstart, 
                             gamma1*ARTstart, gamma1*ARTstart, gamma1*ARTstart), 
                           nrow = 9, ncol=1) 
    
    treatment_C1c <- matrix(c(gamma1c*ARTstart, gamma1c*ARTstart, gamma1c*ARTstart,
                             gamma1c*ARTstart, gamma1c*ARTstart, gamma1c*ARTstart, 
                             gamma1c*ARTstart, gamma1c*ARTstart, gamma1c*ARTstart), 
                           nrow = 9, ncol=1)
    
    treatment_C2 <- matrix(c(gamma2*ARTstart, gamma2*ARTstart, gamma2*ARTstart,
                             gamma2*ARTstart, gamma2*ARTstart, gamma2*ARTstart, 
                             gamma2*ARTstart, gamma2*ARTstart, gamma2*ARTstart), 
                           nrow = 9, ncol=1)
    
    treatment_C3 <- matrix(c(gamma3*ARTstart, gamma3*ARTstart, gamma3*ARTstart,
                             gamma3*ARTstart, gamma3*ARTstart, gamma3*ARTstart, 
                             gamma3*ARTstart, gamma3*ARTstart, gamma3*ARTstart), 
                           nrow = 9, ncol=1)
    
    treatment_C4 <- matrix(c(gamma4*ARTstart, gamma4*ARTstart, gamma4*ARTstart,
                             gamma4*ARTstart, gamma4*ARTstart, gamma4*ARTstart, 
                             gamma4*ARTstart, gamma4*ARTstart, gamma4*ARTstart), 
                           nrow = 9, ncol=1)
    
    treatment_C5 <- matrix(c(gamma5*ARTstart, gamma5*ARTstart, gamma5*ARTstart,
                             gamma5*ARTstart, gamma5*ARTstart, gamma5*ARTstart, 
                             gamma5*ARTstart, gamma5*ARTstart, gamma5*ARTstart), 
                           nrow = 9, ncol=1)
    
    treatment_C6 <- matrix(c(gamma6*ARTstart, gamma6*ARTstart, gamma6*ARTstart,
                             gamma6*ARTstart, gamma6*ARTstart, gamma6*ARTstart, 
                             gamma6*ARTstart, gamma6*ARTstart, gamma6*ARTstart), 
                           nrow = 9, ncol=1)
    
    
    #### Infection flow ####  
    H_dy <- ((Bacute*(I1_HH + I1c_HH + I1_HL + I1c_HL) + 
                I2_HH + I3_HH + I4_HH + kI5k*I5_HH + kI6k*I6_HH + 
                I2_HL + I3_HL + I4_HL + kI5k*I5_HL + kI6k*I6_HL)  + 
               eTT*(Bacute*(T1_HH + T1c_HH + T1_HL + T1c_HL)  + 
                      T2_HH + T3_HH + T4_HH + kI5k*T5_HH + kI6k*T6_HH + 
                      T2_HL + T3_HL + T4_HL + kI5k*T5_HL + kI6k*T6_HL)  + 
               eTC*(Bacute*(C1_HH + C1c_HH + C1_HL + C1c_HL) + 
                      C2_HH + C3_HH + C4_HH + kI5k*C5_HH + kI6k*C6_HH + 
                      C2_HL + C3_HL + C4_HL + kI5k*C5_HL  + kI6k*C6_HL)  + 
               eA*eTC*(A1_HH + A1c_HH + A2_HH + A3_HH + A4_HH + A5_HH + A6_HH + 
                         A1_HL + A1c_HL + A2_HL + A3_HL + A4_HL + A5_HL + A6_HL)  + 
               psP*(Bacute*(P1_HH + P1c_HH + P1_HL + P1c_HL) + 
                      P2_HH + P3_HH + P4_HH + kI5k*P5_HH + kI6k*P6_HH + 
                      P2_HL + P3_HL + P4_HL + kI5k*P5_HL + kI6k*P6_HL))
    
    L_dy <- ((Bacute*( I1_LL + I1c_LL) + I2_LL + I3_LL + I4_LL + kI5k*I5_LL + kI6k*I6_LL)  + 
               eTT*(Bacute*(T1_LL + T1c_LL) + T2_LL + T3_LL + T4_LL + kI5k*T5_LL + kI6k*T6_LL)  + 
               eTC*(Bacute*(C1_LL + C1c_LL) + C2_LL + C3_LL + C4_LL + kI5k*C5_LL + kI6k*C6_LL)  + 
               eA*eTC*(A1_LL + A1c_LL + A2_LL + A3_LL + A4_LL + A5_LL + A6_LL)  + 
               psP*(Bacute*(P1_LL + P1c_LL) + P2_LL + P3_LL + P4_LL + kI5k*P5_LL + kI6k*P6_LL)) 
    
    
    aH_dy <- ((Bacute*(I1a_HH + I1ca_HH) + I2a_HH + I3a_HH + I4a_HH + kI5k*I5a_HH + kI6k*I6a_HH)  + 
                eTT*(Bacute*(T1a_HH + T1ca_HH) + T2a_HH + T3a_HH + T4a_HH + kI5k*T5a_HH + kI6k*T6a_HH)  + 
                eTC*(Bacute*(C1a_HH + C1ca_HH) + C2a_HH + C3a_HH + C4a_HH + kI5k*C5a_HH + kI6k*C6a_HH) + 
                eA*eTC*(A1a_HH + A1ca_HH + A2a_HH + A3a_HH + A4a_HH + A5a_HH + A6a_HH)  + 
                psP*(Bacute*(P1a_HH + P1ca_HH) + P2a_HH + P3a_HH + P4a_HH + kI5k*P5a_HH + kI6k*P6a_HH))  
    
    aL_dy <-   ((Bacute*(I1a_HL + I1ca_HL + I1a_LL + I1ca_LL) + 
                   I2a_HL + I3a_HL + I4a_HL + kI5k*I5a_HL + kI6k*I6a_HL + 
                   I2a_LL + I3a_LL + I4a_LL + kI5k*I5a_LL + kI6k*I6a_LL) + 
                  eTT*(Bacute*(T1a_HL + T1ca_HL + T1a_LL + T1ca_LL) + 
                         T2a_HL + T3a_HL + T4a_HL + kI5k*T5a_HL + kI6k*T6a_HL + 
                         T2a_LL + T3a_LL + T4a_LL + kI5k*T5a_LL + kI6k*T6a_LL)  + 
                  eTC*(Bacute*(C1a_HL + C1ca_HL + C1a_LL + C1ca_LL) + 
                         C2a_HL + C3a_HL + C4a_HL + kI5k*C5a_HL + kI6k*C6a_HL + 
                         C2a_LL + C3a_LL + C4a_LL + kI5k*C5a_LL + kI6k*C6a_LL)  + 
                  eA*eTC*(A1a_HL + A1ca_HL + A2a_HL + A3a_HL + A4a_HL + A5a_HL + A6a_HL + 
                          A1a_LL + A1ca_LL + A2a_LL + A3a_LL + A4a_LL + A5a_LL + A6a_LL)  + 
                  psP*(Bacute*(P1a_HL + P1ca_HL + P1a_LL + P1ca_LL) + 
                         P2a_HL + P3a_HL + P4a_HL + kI5k*P5a_HL + kI6k*P6a_HL + 
                         P2a_LL + P3a_LL + P4a_LL + kI5k*P5a_LL + kI6k*P6a_LL))  
    
    
    aaH_dy <- ((Bacute*(I1aa_HH + I1caa_HH) + I2aa_HH + I3aa_HH + I4aa_HH + kI5k*I5aa_HH + kI6k*I6aa_HH)  + 
                 eTC*(Bacute*(T1aa_HH + T1caa_HH) + T2aa_HH + T3aa_HH + T4aa_HH + kI5k*T5aa_HH + kI6k*T6aa_HH) + 
                 eTC*(Bacute*(C1aa_HH + C1caa_HH) + C2aa_HH + C3aa_HH + C4aa_HH + kI5k*C5aa_HH + kI6k*C6aa_HH) + 
                 eA*eTC*(A1aa_HH + A1caa_HH + A2aa_HH + A3aa_HH + A4aa_HH + A5aa_HH + A6aa_HH) + 
                 psP*(Bacute*(P1aa_HH + P1caa_HH) + P2aa_HH + P3aa_HH + P4aa_HH + kI5k*P5aa_HH + kI6k*P6aa_HH)) 
    
    aaL_dy <- ((Bacute*(I1aa_HL + I1caa_HL + I1aa_LL + I1caa_LL) + 
                  I2aa_HL + I3aa_HL + I4aa_HL + kI5k*I5aa_HL + kI6k*I6aa_HL + 
                  I2aa_LL + I3aa_LL + I4aa_LL + kI5k*I5aa_LL + kI6k*I6aa_LL) + 
                 eTT*(Bacute*(T1aa_HL + T1caa_HL + T1aa_LL + T1caa_LL) + 
                        T2aa_HL + T3aa_HL + T4aa_HL + kI5k*T5aa_HL + kI6k*T6aa_HL + 
                        T2aa_LL + T3aa_LL + T4aa_LL + kI5k*T5aa_LL + kI6k*T6aa_LL) + 
                 eTC*(Bacute*(C1aa_HL + C1caa_HL + C1aa_LL + C1caa_LL) + 
                        C2aa_HL + C3aa_HL + C4aa_HL + kI5k*C5aa_HL + kI6k*C6aa_HL + 
                        C2aa_LL + C3aa_LL + C4aa_LL + kI5k*C5aa_LL + kI6k*C6aa_LL) + 
                 eA*eTC*(A1aa_HL + A1caa_HL + A2aa_HL + A3aa_HL + A4aa_HL + A5aa_HL + A6aa_HL + 
                         A1aa_LL + A1caa_LL + A2aa_LL + A3aa_LL + A4aa_LL + A5aa_LL + A6aa_LL) + 
                 psP*(Bacute*(P1aa_HL + P1caa_HL + P1aa_LL + P1caa_LL) + 
                        P2aa_HL + P3aa_HL + P4aa_HL + kI5k*P5aa_HL + kI6k*P6aa_HL + 
                        P2aa_LL + P3aa_LL + P4aa_LL + kI5k*P5aa_LL + kI6k*P6aa_LL))
    
    
    ##### aged 15-44 ##### 
    ###### H ###### 
    Infection_H <- 
    beta_HH*(S_HH/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* H_dy + 
      beta_HL*(S_HH/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* L_dy + 
      age_gap12*beta_aHaH*(S_HH/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aH_dy + 
      age_gap12*beta_aHaL*(S_HH/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aL_dy + 
      age_gap13*beta_aaHaaH*(S_HH/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aaH_dy + 
      age_gap13*beta_aaHaaL*(S_HH/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aaH_dy
    
    ###### L ######
    Infection_L <- 
      beta_HL*(S_LL/(sum_infection_L + S_LL + P0_LL))* H_dy + 
      beta_LL*(S_LL/(sum_infection_L + S_LL + P0_LL))* L_dy +
      age_gap12*beta_aHaL*(S_LL/(sum_infection_L + S_LL + P0_LL))* aH_dy + 
      age_gap12*beta_aLaL*(S_LL/(sum_infection_L + S_LL + P0_LL))* aL_dy + 
      age_gap13*beta_aaHaaL*(S_LL/(sum_infection_L + S_LL + P0_LL))* aaH_dy + 
      age_gap13*beta_aaLaaL*(S_LL/(sum_infection_L + S_LL + P0_LL))* aaL_dy 
    
    ###### HL ######
    Infection_HL <- 
      beta_HH*(S_HL/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* H_dy +
      beta_HL*(S_HL/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* L_dy +
      age_gap12*beta_aHaH*(S_HL/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aH_dy + 
      age_gap12*beta_aHaL*(S_HL/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aL_dy + 
      age_gap13*beta_aaHaaH*(S_HL/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aaH_dy + 
      age_gap13*beta_aaHaaL*(S_HL/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aaL_dy
    
    
    ##### aged 45-64 #####
    ###### H ######
    Infection_a_H <- 
      age_gap12*beta_aHaH*(Sa_HH/(sum_infection_a_H + Sa_HH + P0a_HH))* H_dy + 
      age_gap12*beta_aHaL*(Sa_HH/(sum_infection_a_H + Sa_HH + P0a_HH))* L_dy + 
      beta_aHaH*(Sa_HH/(sum_infection_a_H + Sa_HH + P0a_HH))* aH_dy +
      beta_aHaL*(Sa_HH/(sum_infection_a_H + Sa_HH + P0a_HH))* aL_dy +
      age_gap23*beta_aaHaaH*(Sa_HH/(sum_infection_a_H + Sa_HH + P0a_HH))* aaH_dy +
      age_gap23*beta_aaHaaL*(Sa_HH/(sum_infection_a_H + Sa_HH + P0a_HH))* aaL_dy 
    
    
    ###### L ######
    Infection_a_L <- 
      age_gap12*beta_aHaL*(Sa_LL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* H_dy +
      age_gap12*beta_aLaL*(Sa_LL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* L_dy+
      beta_aHaL*(Sa_LL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aH_dy +
      beta_aLaL*(Sa_LL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aL_dy +
      age_gap23*beta_aaHaaL*(Sa_LL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aaH_dy + 
      age_gap23*beta_aaLaaL*(Sa_LL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aaL_dy 
    
    
    ###### HL ######
    Infection_a_HL <- 
      age_gap12*beta_aHaL*(Sa_HL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* H_dy + 
      age_gap12*beta_aLaL*(Sa_HL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* L_dy + 
      beta_aHaL*(Sa_HL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aH_dy + 
      beta_aLaL*(Sa_HL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aL_dy + 
      age_gap23*beta_aaHaaL*(Sa_HL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aaH_dy + 
      age_gap23*beta_aaLaaL*(Sa_HL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aaL_dy
    
    
    ##### aged 65- #####
    ###### H ###### 
    Infection_aa_H <- 
      age_gap13*beta_aaHaaH*(Saa_HH/(sum_infection_aa_H + Saa_HH + P0aa_HH))* H_dy +
      age_gap13*beta_aaHaaL*(Saa_HH/(sum_infection_aa_H + Saa_HH + P0aa_HH))* L_dy + 
      age_gap23*beta_aaHaaH*(Saa_HH/(sum_infection_aa_H + Saa_HH + P0aa_HH))* aH_dy + 
      age_gap23*beta_aaHaaL*(Saa_HH/(sum_infection_aa_H + Saa_HH + P0aa_HH))* aL_dy + 
      beta_aaHaaH*(Saa_HH/(sum_infection_aa_H + Saa_HH + P0aa_HH))* aaH_dy + 
      beta_aaHaaL*(Saa_HH/(sum_infection_aa_H + Saa_HH + P0aa_HH))* aaL_dy
    
    
    ###### L ######
    Infection_aa_L <-  
      age_gap13*beta_aaHaaL*(Saa_LL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* H_dy +
      age_gap13*beta_aaLaaL*(Saa_LL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* L_dy + 
      age_gap23*beta_aaHaaL*(Saa_LL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* aH_dy + 
      age_gap23*beta_aaLaaL*(Saa_LL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* aL_dy + 
      beta_aaHaaL*(Saa_LL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* aaH_dy + 
      beta_aaLaaL*(Saa_LL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* aaL_dy
    
    
    ###### HL ######
    Infection_aa_HL <- 
      age_gap13*beta_aaHaaL*(Saa_HL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* H_dy +
      age_gap13*beta_aaLaaL*(Saa_HL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* L_dy + 
      age_gap23*beta_aaHaaL*(Saa_HL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* aH_dy + 
      age_gap23*beta_aaLaaL*(Saa_HL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* aL_dy +   
      beta_aaHaaL*(Saa_HL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* aaH_dy + 
      beta_aaLaaL*(Saa_HL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* aaL_dy  
    
    
    #### Force of infeciton: PrEP infection ####
    ##### aged 15-44 ##### 
    ###### H ######
    Infection_P_H <-  
      beta_PP*beta_HH*(P0_HH/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* H_dy +
      beta_PP*beta_HL*(P0_HH/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* L_dy +
      beta_PP*age_gap12*beta_aHaH*(P0_HH/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aH_dy +
      beta_PP*age_gap12*beta_aHaL*(P0_HH/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aL_dy +   
      beta_PP*age_gap13*beta_aaHaaH*(P0_HH/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aaH_dy +
      beta_PP*age_gap13*beta_aaHaaL*(P0_HH/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aaL_dy
    
    
    ###### L ######
    Infection_P_L <-  
      beta_PP*beta_HL*(P0_LL/(S_LL + sum_infection_L + P0_LL))* H_dy +
      beta_PP*beta_LL*(P0_LL/(S_LL + sum_infection_L + P0_LL))* L_dy +
      beta_PP*age_gap12*beta_aHaL*(P0_LL/(S_LL + sum_infection_L + P0_LL))* aH_dy +
      beta_PP*age_gap12*beta_aLaL*(P0_LL/(S_LL + sum_infection_L + P0_LL))* aL_dy +
      beta_PP*age_gap13*beta_aaHaaL*(P0_LL/(S_LL + sum_infection_L + P0_LL))* aaH_dy +
      beta_PP*age_gap13*beta_aaLaaL*(P0_LL/(S_LL + sum_infection_L + P0_LL))* aaL_dy
    
    
    ###### HL ######
    Infection_P_HL <-  
      beta_PP*beta_HH*(P0_HL/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* H_dy +
      beta_PP*beta_HL*(P0_HL/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* L_dy +
      beta_PP*age_gap12*beta_aHaH*(P0_HL/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aH_dy +
      beta_PP*age_gap12*beta_aHaL*(P0_HL/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aL_dy +
      beta_PP*age_gap13*beta_aaHaaH*(P0_HL/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aaH_dy +
      beta_PP*age_gap13*beta_aaHaaL*(P0_HL/(sum_infection_H + sum_infection_HL + S_HH + S_HL + P0_HH + P0_HL))* aaL_dy
    
    
    ##### aged 45-64 #####
    ###### H ######
    Infection_Pa_H <-  
      age_gap12*beta_aHaH*(P0a_HH/(sum_infection_a_H + Sa_HH + P0a_HH))* H_dy +
      age_gap12*beta_aHaL*(P0a_HH/(sum_infection_a_H + Sa_HH + P0a_HH))* L_dy +
      beta_aHaH*(P0a_HH/(sum_infection_a_H + Sa_HH + P0a_HH))* aH_dy +
      beta_aHaL*(P0a_HH/(sum_infection_a_H + Sa_HH + P0a_HH))* aL_dy +
      age_gap23*beta_aaHaaH*(P0a_HH/(sum_infection_a_H + Sa_HH + P0a_HH))* aaH_dy +
      age_gap23*beta_aaHaaL*(P0a_HH/(sum_infection_a_H + Sa_HH + P0a_HH))* aaL_dy
    
    
    ###### L ######
    Infection_Pa_L <- 
      age_gap12*beta_aHaL*(P0a_LL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* H_dy +
      age_gap12*beta_aLaL*(P0a_LL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* L_dy +
      beta_aHaL*(P0a_LL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aH_dy +
      beta_aLaL*(P0a_LL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aL_dy +
      age_gap23*beta_aaHaaL*(P0a_LL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aaH_dy +
      age_gap23*beta_aaLaaL*(P0a_LL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aaL_dy
    
    ###### HL ######
    Infection_Pa_HL <- 
      age_gap12*beta_aHaL*(P0a_HL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* H_dy +
      age_gap12*beta_aLaL*(P0a_HL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* L_dy +
      beta_aHaL*(P0a_HL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aH_dy +
      beta_aLaL*(P0a_HL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aL_dy +
      age_gap23*beta_aaHaaL*(P0a_HL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aaH_dy +
      age_gap23*beta_aaLaaL*(P0a_HL/(sum_infection_a_L + sum_infection_a_HL + Sa_HL + Sa_LL + P0a_HL + P0a_LL))* aaL_dy
    
    
    ##### aged 65- #####
    ###### H ######
    Infection_Paa_H <- 
      age_gap13*beta_aaHaaH*(P0aa_HH/(sum_infection_aa_H + Saa_HH + P0aa_HH))* H_dy +
      age_gap13*beta_aaHaaL*(P0aa_HH/(sum_infection_aa_H + Saa_HH + P0aa_HH))* L_dy +
      age_gap23*beta_aaHaaH*(P0aa_HH/(sum_infection_aa_H + Saa_HH + P0aa_HH))* aH_dy +
      age_gap23*beta_aaHaaL*(P0aa_HH/(sum_infection_aa_H + Saa_HH + P0aa_HH))* aL_dy +
      beta_aaHaaH*(P0aa_HH/(sum_infection_aa_H + Saa_HH + P0aa_HH))* aaH_dy +
      beta_aaHaaL*(P0aa_HH/(sum_infection_aa_H + Saa_HH + P0aa_HH))* aaL_dy
    
    ###### L ######
    Infection_Paa_L <- 
      age_gap13*beta_aaHaaL*(P0aa_LL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* H_dy +
      age_gap13*beta_aaLaaL*(P0aa_LL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* L_dy +
      age_gap23*beta_aaHaaL*(P0aa_LL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* aH_dy +
      age_gap23*beta_aaLaaL*(P0aa_LL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* aL_dy +
      beta_aaHaaL*(P0aa_LL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* aaH_dy +
      beta_aaLaaL*(P0aa_LL/(sum_infection_aa_L + sum_infection_aa_HL + Saa_LL + Saa_HL + P0aa_LL + P0aa_HL))* aaL_dy
    
    
    ###### HL ######
    Infection_Paa_HL <- 
      age_gap13*beta_aaHaaL*(P0aa_HL/(sum_infection_aa_HL + sum_infection_aa_L + Saa_HL + Saa_LL + P0aa_HL + P0aa_LL))* H_dy + 
      age_gap13*beta_aaLaaL*(P0aa_HL/(sum_infection_aa_HL + sum_infection_aa_L + Saa_HL + Saa_LL + P0aa_HL + P0aa_LL))* L_dy +
      age_gap23*beta_aaHaaL*(P0aa_HL/(sum_infection_aa_HL + sum_infection_aa_L + Saa_HL + Saa_LL + P0aa_HL + P0aa_LL))* aH_dy +
      age_gap23*beta_aaLaaL*(P0aa_HL/(sum_infection_aa_HL + sum_infection_aa_L + Saa_HL + Saa_LL + P0aa_HL + P0aa_LL))* aL_dy +
      beta_aaHaaL*(P0aa_HL/(sum_infection_aa_HL + sum_infection_aa_L + Saa_HL + Saa_LL + P0aa_HL + P0aa_LL))* aaH_dy +
      beta_aaLaaL*(P0aa_HL/(sum_infection_aa_HL + sum_infection_aa_L + Saa_HL + Saa_LL + P0aa_HL + P0aa_LL))* aaL_dy
    
    
    
    
    
    
    #### Equations  ####   
    
    ##### S & P0 ##### 
    dS_HH <- Enter_H  +  out_of_PrEP_H - Enrolled_to_PrEP_H - death_S_H - aging_S_H - Infection_H
    dS_HL <- Enter_HL  +  out_of_PrEP_HL - Enrolled_to_PrEP_HL - death_S_HL - aging_S_HL - Infection_HL
    dS_LL <- Enter_L  +  out_of_PrEP_L - Enrolled_to_PrEP_L - death_S_L - aging_S_L - Infection_L
    
    dSa_HH <- aging_S_H - aging_Sa_H - death_Sa_H - Infection_a_H
    dSa_HL <- aging_S_HL - aging_Sa_HL - death_Sa_HL - Infection_a_HL
    dSa_LL <- aging_S_L - aging_Sa_L - death_Sa_L - Infection_a_L
    
    
    dSaa_HH <- aging_Sa_H - death_Saa_H - Infection_aa_H
    dSaa_HL <- aging_Sa_HL - death_Saa_HL - Infection_aa_HL 
    dSaa_LL <- aging_Sa_L - death_Saa_L - Infection_aa_L
    
    dP0_HH <- Enrolled_to_PrEP_H - out_of_PrEP_H  - aging_P0_H - death_P0_H - Infection_P_H
    dP0_HL <- Enrolled_to_PrEP_HL - out_of_PrEP_HL  - aging_P0_HL - death_P0_HL - Infection_P_HL
    dP0_LL <- Enrolled_to_PrEP_L - out_of_PrEP_L  - aging_P0_L - death_P0_L - Infection_P_L
    
    dP0a_HH <- aging_P0_H - aging_P0a_H - death_P0a_H - Infection_Pa_H
    dP0a_HL <- aging_P0_HL - aging_P0a_HL - death_P0a_HL - Infection_Pa_HL
    dP0a_LL <- aging_P0_L - aging_P0a_L - death_P0a_L - Infection_Pa_L
    
    dP0aa_HH <- aging_P0a_H - death_P0aa_H - Infection_Paa_H
    dP0aa_HL <- aging_P0a_HL - death_P0aa_HL - Infection_Paa_HL
    dP0aa_LL <- aging_P0a_L - death_P0aa_L - Infection_Paa_L
    
    
    ##### I ##### 
    dI1_HH <- Infection_H - I1_to_I1c_H - test_1_H - death_I1_H - aging_I1_H 
    dI1_HL <- Infection_HL - I1_to_I1c_HL - test_1_HL - death_I1_HL - aging_I1_HL
    dI1_LL <- Infection_L - I1_to_I1c_L - test_1_L - death_I1_L - aging_I1_L
    
    dI1a_HH <- Infection_a_H + aging_I1_H + Infection_Pa_H - I1a_to_I1ca_H - test_1a_H - death_I1a_H - aging_I1a_H
    dI1a_HL <- Infection_a_HL + aging_I1_HL + Infection_Pa_HL - I1a_to_I1ca_HL - test_1a_HL - death_I1a_HL - aging_I1a_HL
    dI1a_LL <- Infection_a_L + aging_I1_L + Infection_Pa_L - I1a_to_I1ca_L - test_1a_L - death_I1a_L - aging_I1a_L
    
    dI1aa_HH <- Infection_aa_H + aging_I1a_H + Infection_Paa_H - I1aa_to_I1caa_H - test_1aa_H - death_I1aa_H
    dI1aa_HL <- Infection_aa_HL + aging_I1a_HL + Infection_Paa_HL - I1aa_to_I1caa_HL - test_1aa_HL - death_I1aa_HL
    dI1aa_LL <- Infection_aa_L + aging_I1a_L + Infection_Paa_L - I1aa_to_I1caa_L - test_1aa_L - death_I1aa_L
    
    dI1c <- disprog1*I1 - 
      I1c*(disprog1c - aging_in + aging_out + death_I12 + test_1c + test_AP1c) 
    
    dI2 <- disprog1c*I1c - 
      I2*(disprog2 - aging_in + aging_out + death_I12 + test_2 + test_AP) 
    
    dI3 <- disprog2*I2 - 
      I3*(disprog3 - aging_in + aging_out + death_I3 + test_3 + test_AP) 
    
    dI4 <- disprog3*I3 - 
      I4*(disprog4 - aging_in + aging_out + death_I45 + test_4 + test_AP) 
    
    dI5 <- disprog4*I4 - 
      I5*(disprog5 - aging_in + aging_out + death_I45 + test_5 + test_AP) 
    
    dI6 <- disprog5*I5 - 
      I6*(aging_out - aging_in + death_I6 + test_6) 
    
  
    
    ##### P ##### 
    dP1_HH <- Infection_P_H - P1_to_P1c_H - aging_P1_H - death_P1_H - test_P1_H 
    dP1_LL <- Infection_P_L - P1_to_P1c_L - aging_P1_L - death_P1_L - test_P1_L
    dP1_HL <- Infection_P_HL - P1_to_P1c_HL - aging_P1_HL - death_P1_HL - test_P1_HL
    
    dP1a_HH <- aging_P1_H - P1a_to_P1ca_H - aging_P1a_H - death_P1a_H - test_P1a_H 
    dP1a_LL <- aging_P1_L - P1a_to_P1ca_L - aging_P1a_L - death_P1a_L - test_P1a_L
    dP1a_HL <- aging_P1_HL - P1a_to_P1ca_HL - aging_P1a_HL - death_P1a_HL - test_P1a_HL 
    
    dP1aa_HH <- aging_P1a_H - P1aa_to_P1caa_H - death_P1aa_H - test_P1aa_H
    dP1aa_LL <- aging_P1a_L - P1aa_to_P1caa_L - death_P1aa_L - test_P1aa_L 
    dP1aa_HL <- aging_P1a_HL - P1aa_to_P1caa_HL - death_P1aa_HL - test_P1aa_HL
    
    dP1c <- disprog1*P1 - 
      P1c*(disprog1c - aging_in + aging_out + death_I12 + test_P1c)
    
    dP2 <- disprog1c*P1c - 
      P2*(disprog2 - aging_in + aging_out + death_I12 + test_P2)
    
    dP3 <- disprog2*P2 - 
      P3*(disprog3 - aging_in + aging_out + death_I3 + test_P3)
    
    dP4 <- disprog3*P3 - 
      P4*(disprog4 - aging_in + aging_out + death_I45 + test_P4)
    
    dP5 <- disprog4*P4 - 
      P5*(disprog5 - aging_in + aging_out + death_I45 + test_P5)
    
    dP6 <- disprog5*P5 - 
      P6*(- aging_in + aging_out + death_I6 + test_P6) 
    
    t_P1 <- matrix(c(test_P1_H, test_P1_HL, test_P1_L,
                     test_P1a_H, test_P1a_HL, test_P1a_L,
                     test_P1aa_H, test_P1aa_HL, test_P1aa_L), 
                   nrow = 9, ncol=1) 
                               
    ##### T ##### 
    
    dT1 <- t_1 + t_P1 - 
      T1*( disprog1 - aging_in + aging_out + death_T12C12 + care_T1)
    
    dT1c <- disprog1*T1 + (test_1c + test_AP1c)*I1c + (test_P1c)*P1c - 
      T1c*(disprog1c - aging_in + aging_out + death_T12C12 + care_T1c)
    
    dT2 <- disprog1c*T1c + (test_2 + test_AP)*I2 + (test_P2)*P2 - 
      T2*(disprog2 - aging_in + aging_out + death_T12C12 + care_T2)
    
    dT3 <- disprog2*T2 + (test_3 + test_AP)*I3 + (test_P3)*P3 - 
      T3*(disprog3 - aging_in + aging_out + death_T3C3 + care_T3)
    
    dT4 <- disprog3*T3 + (test_4 + test_AP)*I4 + (test_P4)*P4 - 
      T4*(disprog4 - aging_in + aging_out + death_T45C45 + care_T4)
    
    dT5 <- disprog4*T4 + (test_5 + test_AP)*I5 + (test_P5)*P5 - 
      T5*(disprog5 - aging_in + aging_out + death_T45C45 + care_T5)
    
    dT6 <- disprog5*T5 + (test_6)*I6 + (test_P6)*P6 - 
      T6*(- aging_in + aging_out + death_T6C6 + care_T6) 
    
    
    
    ##### C ##### 
    
    dC1 <- T1*care_T1 - 
      C1*(disprog1 - aging_in + aging_out + death_T12C12 + treatment_C1)
    
    dC1c <- T1c*care_T1c + C1*(disprog1) - 
      C1c*(disprog1c - aging_in + aging_out + death_T12C12 + treatment_C1c)
    
    dC2 <- T2*care_T2 + C1c*(disprog1c) -
      C2*(disprog2 - aging_in + aging_out + death_T12C12 + treatment_C2)
    
    dC3 <- T3*care_T3 + C2*(disprog2) -
      C3*(disprog3 - aging_in + aging_out + death_T3C3 + treatment_C3)
    
    dC4 <- T4*care_T4 + C3*(disprog3) -
      C4*(disprog4 - aging_in + aging_out + death_T45C45 + treatment_C4)
    
    dC5 <- T5*care_T5 + C4*(disprog4) -
      C5*(disprog5 - aging_in + aging_out + death_T45C45 + treatment_C5)
    
    dC6 <- T6*care_T6 + C5*(disprog5) -
      C6*(- aging_in + aging_out + death_T6C6 + treatment_C6)
    
    
    ##### A ##### 
    
    dA1 <- C1*treatment_C1 - A1*(-aging_in + aging_out + death_A12)
    dA1c <- C1c*treatment_C1c - A1c*(-aging_in + aging_out + death_A12)
    dA2 <- C2*treatment_C2 - A2*(-aging_in + aging_out + death_A12) 
    dA3 <- C3*treatment_C3 - A3*(-aging_in + aging_out + death_A3) 
    dA4 <- C4*treatment_C4 - A4*(-aging_in + aging_out + death_A45) 
    dA5 <- C5*treatment_C5 - A5*(-aging_in + aging_out + death_A45) 
    dA6 <- C6*treatment_C6 - A6*(-aging_in + aging_out + death_A6) 
    
    
    
    # aggegrate 
    # death 
    d_I1c_all <- matrix(c(I1c*death_I12), nrow = 9, ncol = 1)
    d_I2_all <- matrix(c(I2*death_I12), nrow = 9, ncol = 1)
    d_I3_all <- matrix(c(I3*death_I3), nrow = 9, ncol = 1)
    d_I4_all <- matrix(c(I4*death_I45), nrow = 9, ncol = 1)
    d_I5_all <- matrix(c(I5*death_I45), nrow = 9, ncol = 1)
    d_I6_all <- matrix(c(I6*death_I6), nrow = 9, ncol = 1)
    
    d_P1c_all <- matrix(c(P1c*death_I12), nrow = 9, ncol = 1)
    d_P2_all <- matrix(c(P2*death_I12), nrow = 9, ncol = 1)
    d_P3_all <- matrix(c(P3*death_I3), nrow = 9, ncol = 1)
    d_P4_all <- matrix(c(P4*death_I45), nrow = 9, ncol = 1)
    d_P5_all <- matrix(c(P5*death_I45), nrow = 9, ncol = 1)
    d_P6_all <- matrix(c(P6*death_I6), nrow = 9, ncol = 1)
    
    d_T1_all <- matrix(c(T1*death_T12C12), nrow = 9, ncol = 1)
    d_T1c_all <- matrix(c(T1c*death_T12C12), nrow = 9, ncol = 1)
    d_T2_all <- matrix(c(T2*death_T12C12), nrow = 9, ncol = 1)
    d_T3_all <- matrix(c(T3*death_T3C3), nrow = 9, ncol = 1)
    d_T4_all <- matrix(c(T4*death_T45C45), nrow = 9, ncol = 1)
    d_T5_all <- matrix(c(T5*death_T45C45), nrow = 9, ncol = 1)
    d_T6_all <- matrix(c(T6*death_T6C6), nrow = 9, ncol = 1)
    
    d_C1_all <- matrix(c(C1*death_T12C12), nrow = 9, ncol = 1)
    d_C1c_all <- matrix(c(C1c*death_T12C12), nrow = 9, ncol = 1)
    d_C2_all <- matrix(c(C2*death_T12C12), nrow = 9, ncol = 1)
    d_C3_all <- matrix(c(C3*death_T3C3), nrow = 9, ncol = 1)
    d_C4_all <- matrix(c(C4*death_T45C45), nrow = 9, ncol = 1)
    d_C5_all <- matrix(c(C5*death_T45C45), nrow = 9, ncol = 1)
    d_C6_all <- matrix(c(C6*death_T6C6), nrow = 9, ncol = 1)
    
    d_A1_all <- matrix(c(A1*death_A12), nrow = 9, ncol = 1)
    d_A1c_all <- matrix(c(A1c*death_A12), nrow = 9, ncol = 1)
    d_A2_all <- matrix(c(A2*death_A12), nrow = 9, ncol = 1)
    d_A3_all <- matrix(c(A3*death_A3), nrow = 9, ncol = 1)
    d_A4_all <- matrix(c(A4*death_A45), nrow = 9, ncol = 1)
    d_A5_all <- matrix(c(A5*death_A45), nrow = 9, ncol = 1)
    d_A6_all <- matrix(c(A6*death_A6), nrow = 9, ncol = 1)
    
    # test flows 
    T1_all <- matrix(c((test_1*I1) + (test_P1*I1)), nrow = 9, ncol=1) 
    T1c_all <- matrix(c((test_1c + test_AP1c)*I1c + (test_P1c)*P1c), nrow = 9, ncol = 1)
    T2_all <- matrix(c((test_2 + test_AP)*I2 + (test_P2)*P2), nrow = 9, ncol = 1)
    T3_all <- matrix(c((test_3 + test_AP)*I3 + (test_P3)*P3), nrow = 9, ncol = 1)
    T4_all <- matrix(c((test_4 + test_AP)*I4 + (test_P4)*P4), nrow = 9, ncol = 1)
    T5_all <- matrix(c((test_5 + test_AP)*I5 + (test_P5)*P5), nrow = 9, ncol = 1)
    T6_all <- matrix(c((test_6)*I6 + (test_P6)*P6), nrow = 9, ncol = 1)
    
    # care 
    Care1_all <- matrix(c(T1*care_T1), nrow = 9, ncol=1) 
    Care1c_all <- matrix(c(T1c*care_T1c), nrow = 9, ncol=1) 
    Care2_all <- matrix(c(T2*care_T2), nrow = 9, ncol=1)
    Care3_all <- matrix(c(T3*care_T3), nrow = 9, ncol=1)
    Care4_all <- matrix(c(T4*care_T4), nrow = 9, ncol=1)
    Care5_all <- matrix(c(T5*care_T5), nrow = 9, ncol=1)
    Care6_all <- matrix(c(T6*care_T6), nrow = 9, ncol=1)
    
    # treatment
    Treat1_all <- matrix(c(C1*treatment_C1), nrow = 9, ncol=1) 
    Treat1c_all <- matrix(c(C1c*treatment_C1c), nrow = 9, ncol=1) 
    Treat2_all <- matrix(c(C2*treatment_C2), nrow = 9, ncol=1)
    Treat3_all <- matrix(c(C3*treatment_C3), nrow = 9, ncol=1)
    Treat4_all <- matrix(c(C4*treatment_C4), nrow = 9, ncol=1)
    Treat5_all <- matrix(c(C5*treatment_C5), nrow = 9, ncol=1)
    Treat6_all <- matrix(c(C6*treatment_C6), nrow = 9, ncol=1)
    
    
    #### #### 
    return(list(c(dS_HH, dS_HL, dS_LL, 
                  dSa_HH, dSa_HL, dSa_LL,
                  dSaa_HH, dSaa_HL, dSaa_LL, 
                  
                  dP0_HH, dP0_HL, dP0_LL, 
                  dP0a_HH, dP0a_HL, dP0a_LL,
                  dP0aa_HH, dP0aa_HL, dP0aa_LL,
                  
                  dI1_HH, dI1_HL, dI1_LL, 
                  dI1a_HH, dI1a_HL, dI1a_LL,
                  dI1aa_HH, dI1aa_HL, dI1aa_LL,
                  
                  dI1c, dI2, dI3, dI4, dI5, dI6,
                  
                  dP1_HH, dP1_HL, dP1_LL, 
                  dP1a_HH, dP1a_HL, dP1a_LL,
                  dP1aa_HH, dP1aa_HL, dP1aa_LL,
                  
                  dP1c, dP2, dP3, dP4, dP5, dP6,
                  dT1, dT1c, dT2, dT3, dT4, dT5, dT6,
                  dC1, dC1c, dC2, dC3, dC4, dC5, dC6,
                  dA1, dA1c, dA2, dA3, dA4, dA5, dA6
    ),
    #####flow results #####
    Enter_H = Enter_H, 
    Enter_L = Enter_L, 
    Enter_HL = Enter_HL, 
    
    out_of_PrEP_H = out_of_PrEP_H, 
    out_of_PrEP_L = out_of_PrEP_L, 
    out_of_PrEP_HL = out_of_PrEP_HL, 
    
    Enrolled_to_PrEP_H = Enrolled_to_PrEP_H, 
    Enrolled_to_PrEP_L = Enrolled_to_PrEP_L, 
    Enrolled_to_PrEP_HL = Enrolled_to_PrEP_HL, 
    
    death_S_H = death_S_H,
    death_S_L = death_S_L, 
    death_S_HL = death_S_HL, 
    death_Sa_H = death_Sa_H,
    death_Sa_L = death_Sa_L, 
    death_Sa_HL = death_Sa_HL, 
    death_Saa_H = death_Saa_H,
    death_Saa_L = death_Saa_L, 
    death_Saa_HL = death_Saa_HL, 
    
    death_P0_H = death_P0_H,
    death_P0_L = death_P0_L,
    death_P0_HL = death_P0_HL,
    death_P0a_H = death_P0a_H,
    death_P0a_L = death_P0a_L,
    death_P0a_HL = death_P0a_HL,
    death_P0aa_H = death_P0aa_H,
    death_P0aa_L = death_P0aa_L,
    death_P0aa_HL = death_P0aa_HL,
    
    death_I1_H = death_I1_H,
    death_I1_L = death_I1_L,
    death_I1_HL = death_I1_HL,
    death_I1a_H = death_I1a_H,
    death_I1a_L = death_I1a_L,
    death_I1a_HL = death_I1a_HL,
    death_I1aa_H = death_I1aa_H,
    death_I1aa_L = death_I1aa_L,
    death_I1aa_HL = death_I1aa_HL,
    
    death_I1c = d_I1c_all,
    death_I2 = d_I2_all,
    death_I3 = d_I3_all,
    death_I4 = d_I4_all,
    death_I5 = d_I5_all,
    death_I6 = d_I6_all,
    
    death_P1_H = death_P1_H,
    death_P1_L = death_P1_L,
    death_P1_HL = death_P1_HL,
    death_P1a_H = death_P1a_H,
    death_P1a_L = death_P1a_L,
    death_P1a_HL = death_P1a_HL,
    death_P1aa_H = death_P1aa_H,
    death_P1aa_L = death_P1aa_L,
    death_P1aa_HL = death_P1aa_HL,
    
    death_P1c = d_P1c_all,
    death_P2 = d_P2_all,
    death_P3 = d_P3_all,
    death_P4 = d_P4_all,
    death_P5 = d_P5_all,
    death_P6 = d_P6_all,
    death_T1 = d_T1_all,
    death_T1c = d_T1c_all,
    death_T2 = d_T2_all,
    death_T3 = d_T3_all,
    death_T4 = d_T4_all,
    death_T5 = d_T5_all,
    death_T6 = d_T6_all,
    
    death_C1 = d_C1_all,
    death_C1c = d_C1c_all,
    death_C2 = d_C2_all,
    death_C3 = d_C3_all,
    death_C4 = d_C4_all,
    death_C5 = d_C5_all,
    death_C6 = d_C6_all,
    death_A1 = d_A1_all,
    death_A1c = d_A1c_all,
    death_A2 = d_A2_all,
    death_A3 = d_A3_all,
    death_A4 = d_A4_all,
    death_A5 = d_A5_all,
    death_A6 = d_A6_all,
    
    # testing flow 
    ## test_ 
    test_1 = T1_all, 
    test_1c = T1c_all, 
    test_2 = T2_all, 
    test_3 = T3_all, 
    test_4 = T4_all, 
    test_5 = T5_all, 
    test_6 = T6_all, 
    
    # link to care 
    LK1 = Care1_all, 
    LK1c = Care1c_all, 
    LK2 = Care2_all, 
    LK3 = Care3_all, 
    LK4 = Care4_all, 
    LK5 = Care5_all, 
    LK6 = Care6_all, 

    # treatment 
    Treat1 = Treat1_all,
    Treat1c = Treat1c_all,
    Treat2 = Treat2_all,
    Treat3 = Treat3_all,
    Treat4 = Treat4_all,
    Treat5 = Treat5_all,
    Treat6 = Treat6_all,
    # infections 
    Infection_H = Infection_H, 
    Infection_L = Infection_L, 
    Infection_HL = Infection_HL, 
    
    Infection_a_H = Infection_a_H, 
    Infection_a_L = Infection_a_L,
    Infection_a_HL = Infection_a_HL, 
    
    Infection_aa_H = Infection_aa_H, 
    Infection_aa_L = Infection_aa_L,
    Infection_aa_HL = Infection_aa_HL, 
    
    # infections on PrEP 
    Infection_P_H = Infection_P_H, 
    Infection_P_L = Infection_P_L, 
    Infection_P_HL = Infection_P_HL, 
    
    Infection_Pa_H = Infection_Pa_H, 
    Infection_Pa_L = Infection_Pa_L, 
    Infection_Pa_HL = Infection_Pa_HL, 
    
    Infection_Paa_H = Infection_Paa_H, 
    Infection_Paa_L = Infection_Paa_L, 
    Infection_Paa_HL = Infection_Paa_HL))
    
  })
}
