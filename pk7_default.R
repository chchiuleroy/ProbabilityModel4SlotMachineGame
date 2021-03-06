rm(list = ls())

#### payout ####

pay = c(0, 1, 2, 3, 5, 7, 50, 120, 200, 500)

#### pass levels ####

levels = 1 : 4

prob_level = c(0.3, 0.3, 0.2, 0.1)

#### bet ####

bet = 20

#### choose the models of pass levels ####

models = 1 : 16

prob_models = c(0.10, 0.10, 0.10, 0.10, 
                0.10, 0.15, 0.15, 0.10, 
                0.05, 0.05, 0.00, 0.00, 
                0.00, 0.00, 0.00, 0.00)

#### settings of level models #####

#### the rewards of probabilities in level 1 ####

##### part 1 exp = 0.41, 0.49, 0.562, 0.654 ####

l_1_1 = c(0.70, 0.24, 0.040, 0.010, 0.005, 0.005, 0.00, 0.00, 0.00, 0.00)

l_1_2 = c(0.70, 0.20, 0.070, 0.010, 0.010, 0.010, 0.00, 0.00, 0.00, 0.00)

l_1_3 = c(0.70, 0.18, 0.064, 0.023, 0.023, 0.010, 0.00, 0.00, 0.00, 0.00)

l_1_4 = c(0.70, 0.15, 0.070, 0.035, 0.028, 0.017, 0.00, 0.00, 0.00, 0.00)

##### part 2 exp = 0.755, 0.8885, 0.955, 1.0573 ####

l_1_5 = c(0.68, 0.179, 0.090, 0.0350, 0.0100, 0.0030, 0.0020, 0.0010, 0.0000, 0.0000)

l_1_6 = c(0.68, 0.130, 0.115, 0.0410, 0.0269, 0.0050, 0.0010, 0.0008, 0.0002, 0.0001)

l_1_7 = c(0.68, 0.113, 0.114, 0.0570, 0.0259, 0.0075, 0.0015, 0.0008, 0.0002, 0.0001)

l_1_8 = c(0.68, 0.125, 0.112, 0.0451, 0.0248, 0.0090, 0.0040, 0.0008, 0.0002, 0.0001)

#### part 3 exp = 2.05, 2.82, 3.555, 5.2 ####

l_1_9  = c(0.00, 0.55, 0.17, 0.170, 0.06, 0.050, 0.000, 0.00, 0.00, 0.00)

l_1_10 = c(0.00, 0.36, 0.18, 0.215, 0.13, 0.115, 0.000, 0.00, 0.00, 0.00)

l_1_11 = c(0.00, 0.38, 0.17, 0.180, 0.12, 0.135, 0.015, 0.00, 0.00, 0.00)

l_1_12 = c(0.00, 0.00, 0.27, 0.290, 0.29, 0.120, 0.030, 0.00, 0.00, 0.00)

#### part 4 exp = 5.93, 7.64, 11.25, 25.4 ####

l_1_13 = c(0.00, 0.00, 0.30, 0.300, 0.26, 0.09, 0.050, 0.00, 0.00, 0.00)

l_1_14 = c(0.00, 0.00, 0.29, 0.290, 0.23, 0.12, 0.060, 0.01, 0.00, 0.00)

l_1_15 = c(0.00, 0.00, 0.00, 0.055, 0.25, 0.58, 0.115, 0.00, 0.00, 0.00)

l_1_16 = c(0.00, 0.00, 0.00, 0.000, 0.49, 0.35, 0.070, 0.05, 0.03, 0.01)

#### the rewards of probabilities in level 2 ####

##### part 1 exp = 0.40, 0.48, 0.551, 0.642 ####

l_2_1 = c(0.71, 0.230, 0.040, 0.010, 0.005, 0.005, 0.00, 0.00, 0.00, 0.00)

l_2_2 = c(0.71, 0.190, 0.070, 0.010, 0.010, 0.010, 0.00, 0.00, 0.00, 0.00)

l_2_3 = c(0.70, 0.183, 0.065, 0.021, 0.021, 0.010, 0.00, 0.00, 0.00, 0.00)

l_2_4 = c(0.70, 0.152, 0.072, 0.033, 0.027, 0.016, 0.00, 0.00, 0.00, 0.00)

##### part 2 exp = 0.740, 0.8785, 0.943, 1.0403 ####

l_2_5 = c(0.690, 0.174, 0.085, 0.0350, 0.0100, 0.0030, 0.0020, 0.0010, 0.0000, 0.0000)

l_2_6 = c(0.690, 0.125, 0.105, 0.0460, 0.0269, 0.0050, 0.0010, 0.0008, 0.0002, 0.0001)

l_2_7 = c(0.684, 0.115, 0.110, 0.0550, 0.0259, 0.0075, 0.0015, 0.0008, 0.0002, 0.0001)

l_2_8 = c(0.683, 0.129, 0.109, 0.0401, 0.0248, 0.0090, 0.0040, 0.0008, 0.0002, 0.0001)

#### part 3 exp = 1.95, 2.72, 3.505, 5.1 ####

l_2_9  = c(0.00, 0.60, 0.15, 0.150, 0.05, 0.050, 0.000, 0.00, 0.00, 0.00)

l_2_10 = c(0.00, 0.38, 0.18, 0.210, 0.13, 0.100, 0.000, 0.00, 0.00, 0.00)

l_2_11 = c(0.00, 0.38, 0.18, 0.180, 0.12, 0.125, 0.015, 0.00, 0.00, 0.00)

l_2_12 = c(0.00, 0.00, 0.29, 0.290, 0.29, 0.100, 0.030, 0.00, 0.00, 0.00)

#### part 4 exp = 5.77, 7.55, 11.131, 25.4 ####

l_2_13 = c(0.00, 0.00, 0.34, 0.300, 0.24, 0.07, 0.050, 0.00, 0.00, 0.00)

l_2_14 = c(0.00, 0.00, 0.30, 0.300, 0.23, 0.10, 0.060, 0.01, 0.00, 0.00)

l_2_15 = c(0.00, 0.00, 0.00, 0.057, 0.25, 0.58, 0.113, 0.00, 0.00, 0.00)

l_2_16 = c(0.00, 0.00, 0.00, 0.000, 0.49, 0.35, 0.070, 0.05, 0.03, 0.01)

#### the rewards of probabilities in level 3 ####

##### part 1 exp = 0.39, 0.47, 0.54, 0.63 ####

l_3_1 = c(0.72, 0.220, 0.040, 0.010, 0.005, 0.005, 0.00, 0.00, 0.00, 0.00)

l_3_2 = c(0.71, 0.200, 0.060, 0.010, 0.010, 0.010, 0.00, 0.00, 0.00, 0.00)

l_3_3 = c(0.70, 0.190, 0.060, 0.020, 0.020, 0.010, 0.00, 0.00, 0.00, 0.00)

l_3_4 = c(0.69, 0.170, 0.070, 0.030, 0.025, 0.015, 0.00, 0.00, 0.00, 0.00)

##### part 2 exp = 0.725, 0.8655, 0.9387, 1.0212 ####

l_3_5 = c(0.690, 0.184, 0.080, 0.0300, 0.0100, 0.0030, 0.0020, 0.0010, 0.0000, 0.0000)

l_3_6 = c(0.690, 0.122, 0.116, 0.0420, 0.0229, 0.0050, 0.0010, 0.0008, 0.0002, 0.0001)

l_3_7 = c(0.684, 0.120, 0.113, 0.0451, 0.0275, 0.0077, 0.0016, 0.0008, 0.0002, 0.0001)

l_3_8 = c(0.683, 0.128, 0.109, 0.0410, 0.0248, 0.0096, 0.0035, 0.0008, 0.0002, 0.0001)

#### part 3 exp = 1.9, 2.6, 3.445, 4.98 ####

l_3_9  = c(0.00, 0.60, 0.20, 0.100, 0.050, 0.050, 0.000, 0.00, 0.00, 0.00)

l_3_10 = c(0.00, 0.40, 0.20, 0.200, 0.100, 0.100, 0.000, 0.00, 0.00, 0.00)

l_3_11 = c(0.00, 0.40, 0.15, 0.200, 0.125, 0.110, 0.015, 0.00, 0.00, 0.00)

l_3_12 = c(0.00, 0.00, 0.29, 0.300, 0.330, 0.050, 0.030, 0.00, 0.00, 0.00)

#### part 4 exp = 5.65, 7.31, 10.91, 25.4 ####

l_3_13 = c(0.00, 0.00, 0.27, 0.270, 0.32, 0.10, 0.040, 0.00, 0.00, 0.00)

l_3_14 = c(0.00, 0.00, 0.27, 0.270, 0.27, 0.13, 0.050, 0.01, 0.00, 0.00)

l_3_15 = c(0.00, 0.00, 0.00, 0.050, 0.31, 0.53, 0.110, 0.00, 0.00, 0.00)

l_3_16 = c(0.00, 0.00, 0.00, 0.000, 0.49, 0.35, 0.070, 0.05, 0.03, 0.01)

#### the rewards of probabilities in level 4 ####

##### part 1 exp = 0.38, 0.47, 0.54, 0.62 ####

l_4_1 = c(0.73, 0.210, 0.040, 0.010, 0.005, 0.005, 0.00, 0.00, 0.00, 0.00)

l_4_2 = c(0.71, 0.200, 0.060, 0.010, 0.010, 0.010, 0.00, 0.00, 0.00, 0.00)

l_4_3 = c(0.70, 0.190, 0.060, 0.020, 0.020, 0.010, 0.00, 0.00, 0.00, 0.00)

l_4_4 = c(0.70, 0.169, 0.070, 0.030, 0.025, 0.015, 0.00, 0.00, 0.00, 0.00)

##### part 2 exp = 0.715, 0.8515, 0.921, 1.017 ####

l_4_5 = c(0.700, 0.1740, 0.080, 0.0300, 0.0100, 0.0030, 0.0020, 0.0010, 0.0000, 0.0000)

l_4_6 = c(0.690, 0.1300, 0.110, 0.0420, 0.0209, 0.0050, 0.0010, 0.0008, 0.0002, 0.0001)

l_4_7 = c(0.684, 0.1250, 0.110, 0.0460, 0.0249, 0.0075, 0.0015, 0.0008, 0.0002, 0.0001)

l_4_8 = c(0.683, 0.1295, 0.109, 0.0400, 0.0244, 0.0095, 0.0035, 0.0008, 0.0002, 0.0001)

#### part 3 exp = 1.85, 2.5, 3.335, 4.87 ####

l_4_9  = c(0.00, 0.65, 0.15, 0.100, 0.050, 0.050, 0.000, 0.00, 0.00, 0.00)

l_4_10 = c(0.00, 0.45, 0.20, 0.150, 0.100, 0.100, 0.000, 0.00, 0.00, 0.00)

l_4_11 = c(0.00, 0.43, 0.18, 0.150, 0.115, 0.110, 0.015, 0.00, 0.00, 0.00)

l_4_12 = c(0.00, 0.00, 0.32, 0.310, 0.290, 0.050, 0.030, 0.00, 0.00, 0.00)

#### part 4 exp = 5.56, 7.22, 10.2, 25.4 ####

l_4_13 = c(0.00, 0.00, 0.29, 0.285, 0.285, 0.10, 0.040, 0.00, 0.00, 0.00)

l_4_14 = c(0.00, 0.00, 0.28, 0.280, 0.270, 0.11, 0.050, 0.01, 0.00, 0.00)

l_4_15 = c(0.00, 0.00, 0.00, 0.100, 0.350, 0.45, 0.100, 0.00, 0.00, 0.00)

l_4_16 = c(0.00, 0.00, 0.00, 0.000, 0.490, 0.35, 0.070, 0.05, 0.03, 0.01)

##### summary probabilities for levels' setting #####

l_1 = rbind(l_1_1, l_1_2, l_1_3, l_1_4, l_1_5, l_1_6, l_1_7, l_1_8, l_1_9, l_1_10, l_1_11, l_1_12, l_1_13, l_1_14, l_1_15, l_1_16)

l_2 = rbind(l_2_1, l_2_2, l_2_3, l_2_4, l_2_5, l_2_6, l_2_7, l_2_8, l_2_9, l_2_10, l_2_11, l_2_12, l_2_13, l_2_14, l_2_15, l_2_16)

l_3 = rbind(l_3_1, l_3_2, l_3_3, l_3_4, l_3_5, l_3_6, l_3_7, l_3_8, l_3_9, l_3_10, l_3_11, l_3_12, l_3_13, l_3_14, l_3_15, l_3_16)

l_4 = rbind(l_4_1, l_4_2, l_4_3, l_4_4, l_4_5, l_4_6, l_4_7, l_4_8, l_4_9, l_4_10, l_4_11, l_4_12, l_4_13, l_4_14, l_4_15, l_4_16)

##### simulations #####

pk7 = function(levels, prob_level, bet, models, prob_models, pay) {
  
  level_choice = sample(levels, 1, prob = prob_level)
  
  total_bet = bet*level_choice
  
  models_choice = sample(models, 1, prob = prob_models)
  
  reward_choice_level = switch(level_choice, l_1, l_2, l_3, l_4)
  
  reward_choice_model_prob = reward_choice_level[models_choice, ]
  
  reward_choice = sample(1:length(pay), 1, prob = reward_choice_model_prob)
  
  money_player = total_bet*pay[reward_choice]
  
  results = c(level_choice, models_choice, total_bet, money_player)
  
  return(results)
  
}

run = 100000

results_machines = t(replicate(run, pk7(levels, prob_level, bet, models, prob_models, pay)))

totally_machines = sum(results_machines[, 3])

totally_player = sum(results_machines[, 4])

expectation = totally_player/totally_machines

bank = cumsum(results_machines[, 3]) - cumsum(results_machines[, 4])

plot.ts(bank, type = "l")

