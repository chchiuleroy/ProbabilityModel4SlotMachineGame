rm(list = ls())

##### parrellel computing ####

if(require("RevoUtilsMath")){
  setMKLthreads(2)
}

##### basic setting ####

lines_com = 20  ## 總線數

bet = 5

total_bet = lines_com*bet

average_time = 3 ## seconds

fg_run = 5

##### simulation setting ####

l = 70

m = 15000 # mode 切換次數

##### payoff ####

pay = rbind(c(0, 0, 005, 015, 0045), 
            c(0, 0, 005, 015, 0045),
            c(0, 0, 005, 015, 0045),
            c(0, 0, 005, 015, 0045),
            c(0, 0, 005, 015, 0045),
            c(0, 0, 010, 045, 0150),
            c(0, 0, 010, 045, 0180),
            c(0, 0, 015, 060, 0240),
            c(0, 0, 015, 060, 0300),
            c(0, 0, 020, 080, 0500),
            c(0, 0, 000, 000, 0000),
            c(0, 0, 000, 000, 0000),
            c(0, 0, 000, 000, 0000),
            c(0, 0, 000, 000, 0000))

##### function ####

## a = index of wild

condi_sum = function(x) {
  
  if (x[2] == x[1] && x[3] == x[1] && x[4] != x[1]) {y = 3} 
  else if (x[2] == x[1] && x[3] == x[1] && x[4] == x[1] && x[5] != x[1]) {y = 4}
  else if (x[2] == x[1] && x[3] == x[1] && x[4] == x[1] && x[5] == x[1]) {y = 5}
  else if (x[2] == x[1] && x[3] != x[1]) {y = 2}
  else {y = 1}
  
  return (y)
  
}

sampling_ng = function(p, n1, n) {
  
  w1 = rbinom(1, n1, p[1]) + 1
  w2 = rbinom(1, n, p[2]) + 1
  w3 = rbinom(1, n, p[3]) + 1
  w4 = rbinom(1, n, p[4]) + 1
  w5 = rbinom(1, n, p[5]) + 1
  
  w6 = rbinom(1, n1, p[6]) + 1
  w7 = rbinom(1, n, p[7]) + 1
  w8 = rbinom(1, n, p[8]) + 1
  w9 = rbinom(1, n, p[9]) + 1
  w10 = rbinom(1, n, p[10]) + 1
  
  w11 = rbinom(1, n1, p[11]) + 1
  w12 = rbinom(1, n, p[12]) + 1
  w13 = rbinom(1, n, p[13]) + 1
  w14 = rbinom(1, n, p[14]) + 1
  w15 = rbinom(1, n, p[15]) + 1
  
  u = rbind(c(w1, w2, w3, w4, w5), c(w6, w7, w8, w9, w10), c(w11, w12, w13, w14, w15))
  
  a = wild
  
  l1 = c(1, 
         ifelse(u[1, 2] == u[1, 1] || u[1, 2] == a, 1, 0), 
         ifelse(u[1, 3] == u[1, 1] || u[1, 3] == a, 1, 0), 
         ifelse(u[1, 4] == u[1, 1] || u[1, 4] == a, 1, 0),
         ifelse(u[1, 5] == u[1, 1] || u[1, 5] == a, 1, 0))
  
  l2 = c(1, 
         ifelse(u[2, 2] == u[1, 1] || u[2, 2] == a, 1, 0), 
         ifelse(u[3, 3] == u[1, 1] || u[3, 3] == a, 1, 0), 
         ifelse(u[2, 4] == u[1, 1] || u[2, 4] == a, 1, 0),
         ifelse(u[1, 5] == u[1, 1] || u[1, 5] == a, 1, 0))
  
  l3 = c(1, 
         ifelse(u[1, 2] == u[1, 1] || u[1, 2] == a, 1, 0), 
         ifelse(u[2, 3] == u[1, 1] || u[2, 3] == a, 1, 0), 
         ifelse(u[3, 4] == u[1, 1] || u[3, 4] == a, 1, 0),
         ifelse(u[3, 5] == u[1, 1] || u[3, 5] == a, 1, 0))
  
  l4 = c(1, 
         ifelse(u[2, 2] == u[1, 1] || u[2, 2] == a, 1, 0), 
         ifelse(u[2, 3] == u[1, 1] || u[2, 3] == a, 1, 0), 
         ifelse(u[2, 4] == u[1, 1] || u[2, 4] == a, 1, 0),
         ifelse(u[1, 5] == u[1, 1] || u[1, 5] == a, 1, 0))
  
  l5 = c(1, 
         ifelse(u[2, 2] == u[1, 1] || u[2, 2] == a, 1, 0), 
         ifelse(u[1, 3] == u[1, 1] || u[1, 3] == a, 1, 0), 
         ifelse(u[2, 4] == u[1, 1] || u[2, 4] == a, 1, 0),
         ifelse(u[1, 5] == u[1, 1] || u[1, 5] == a, 1, 0))
  
  l6 = c(1, 
         ifelse(u[1, 2] == u[1, 1] || u[1, 2] == a, 1, 0), 
         ifelse(u[3, 3] == u[1, 1] || u[3, 3] == a, 1, 0), 
         ifelse(u[1, 4] == u[1, 1] || u[1, 4] == a, 1, 0),
         ifelse(u[1, 5] == u[1, 1] || u[1, 5] == a, 1, 0))
  
  l7 = c(1, 
         ifelse(u[3, 2] == u[1, 1] || u[3, 2] == a, 1, 0), 
         ifelse(u[3, 3] == u[1, 1] || u[3, 3] == a, 1, 0), 
         ifelse(u[3, 4] == u[1, 1] || u[3, 4] == a, 1, 0),
         ifelse(u[1, 5] == u[1, 1] || u[1, 5] == a, 1, 0))
  
  l8 = c(1, 
         ifelse(u[2, 2] == u[2, 1] || u[2, 2] == a, 1, 0), 
         ifelse(u[2, 3] == u[2, 1] || u[2, 3] == a, 1, 0), 
         ifelse(u[2, 4] == u[2, 1] || u[2, 4] == a, 1, 0),
         ifelse(u[2, 5] == u[2, 1] || u[2, 5] == a, 1, 0))
  
  l9 = c(1, 
         ifelse(u[1, 2] == u[2, 1] || u[1, 2] == a, 1, 0), 
         ifelse(u[1, 3] == u[2, 1] || u[1, 3] == a, 1, 0), 
         ifelse(u[1, 4] == u[2, 1] || u[1, 4] == a, 1, 0),
         ifelse(u[2, 5] == u[2, 1] || u[2, 5] == a, 1, 0))
  
  l10 = c(1, 
          ifelse(u[3, 2] == u[2, 1] || u[3, 2] == a, 1, 0), 
          ifelse(u[3, 3] == u[2, 1] || u[3, 3] == a, 1, 0), 
          ifelse(u[3, 4] == u[2, 1] || u[3, 4] == a, 1, 0),
          ifelse(u[2, 5] == u[2, 1] || u[2, 5] == a, 1, 0))
  
  l11 = c(1, 
          ifelse(u[3, 2] == u[2, 1] || u[3, 2] == a, 1, 0), 
          ifelse(u[2, 3] == u[2, 1] || u[2, 3] == a, 1, 0), 
          ifelse(u[1, 4] == u[2, 1] || u[1, 4] == a, 1, 0),
          ifelse(u[1, 5] == u[2, 1] || u[1, 5] == a, 1, 0))
  
  l12 = c(1, 
          ifelse(u[1, 2] == u[2, 1] || u[1, 2] == a, 1, 0), 
          ifelse(u[2, 3] == u[2, 1] || u[2, 3] == a, 1, 0), 
          ifelse(u[3, 4] == u[2, 1] || u[3, 4] == a, 1, 0),
          ifelse(u[3, 5] == u[2, 1] || u[3, 5] == a, 1, 0))
  
  l13 = c(1, 
          ifelse(u[2, 2] == u[2, 1] || u[2, 2] == a, 1, 0), 
          ifelse(u[1, 3] == u[2, 1] || u[1, 3] == a, 1, 0), 
          ifelse(u[2, 4] == u[2, 1] || u[2, 4] == a, 1, 0),
          ifelse(u[2, 5] == u[2, 1] || u[2, 5] == a, 1, 0))
  
  l14 = c(1, 
          ifelse(u[2, 2] == u[2, 1] || u[2, 2] == a, 1, 0), 
          ifelse(u[3, 3] == u[2, 1] || u[3, 3] == a, 1, 0), 
          ifelse(u[2, 4] == u[2, 1] || u[2, 4] == a, 1, 0),
          ifelse(u[2, 5] == u[2, 1] || u[2, 5] == a, 1, 0))
  
  l15 = c(1, 
          ifelse(u[3, 2] == u[3, 1] || u[3, 2] == a, 1, 0), 
          ifelse(u[3, 3] == u[3, 1] || u[3, 3] == a, 1, 0), 
          ifelse(u[3, 4] == u[3, 1] || u[3, 4] == a, 1, 0),
          ifelse(u[3, 5] == u[3, 1] || u[3, 5] == a, 1, 0))
  
  l16 = c(1, 
          ifelse(u[2, 2] == u[3, 1] || u[2, 2] == a, 1, 0), 
          ifelse(u[1, 3] == u[3, 1] || u[1, 3] == a, 1, 0), 
          ifelse(u[2, 4] == u[3, 1] || u[2, 5] == a, 1, 0),
          ifelse(u[3, 5] == u[3, 1] || u[3, 5] == a, 1, 0))
  
  l17 = c(1, 
          ifelse(u[3, 2] == u[3, 1] || u[3, 2] == a, 1, 0), 
          ifelse(u[2, 3] == u[3, 1] || u[2, 3] == a, 1, 0), 
          ifelse(u[1, 4] == u[3, 1] || u[1, 4] == a, 1, 0),
          ifelse(u[1, 5] == u[3, 1] || u[1, 5] == a, 1, 0))
  
  l18 = c(1, 
          ifelse(u[2, 2] == u[3, 1] || u[2, 2] == a, 1, 0), 
          ifelse(u[2, 3] == u[3, 1] || u[2, 3] == a, 1, 0), 
          ifelse(u[2, 4] == u[3, 1] || u[2, 4] == a, 1, 0),
          ifelse(u[3, 5] == u[3, 1] || u[3, 5] == a, 1, 0))
  
  l19 = c(1, 
          ifelse(u[2, 2] == u[3, 1] || u[2, 2] == a, 1, 0), 
          ifelse(u[3, 3] == u[3, 1] || u[3, 3] == a, 1, 0), 
          ifelse(u[2, 4] == u[3, 1] || u[2, 4] == a, 1, 0),
          ifelse(u[3, 5] == u[3, 1] || u[2, 5] == a, 1, 0))
  
  l20 = c(1, 
          ifelse(u[3, 2] == u[3, 1] || u[3, 2] == a, 1, 0), 
          ifelse(u[1, 3] == u[3, 1] || u[1, 3] == a, 1, 0), 
          ifelse(u[3, 4] == u[3, 1] || u[3, 4] == a, 1, 0),
          ifelse(u[3, 5] == u[3, 1] || u[3, 5] == a, 1, 0))
  
  r1  = condi_sum(l1)  
  r2  = condi_sum(l2)	
  r3	=	condi_sum(l3)	
  r4	=	condi_sum(l4)	
  r5	=	condi_sum(l5)	
  r6	=	condi_sum(l6)	
  r7	=	condi_sum(l7)	
  r8	=	condi_sum(l8)	
  r9	=	condi_sum(l9)	
  r10	=	condi_sum(l10)	
  r11	=	condi_sum(l11)	
  r12	=	condi_sum(l12)	
  r13	=	condi_sum(l13)	
  r14	=	condi_sum(l14)	
  r15	=	condi_sum(l15)	
  r16	=	condi_sum(l16)	
  r17	=	condi_sum(l17)	
  r18	=	condi_sum(l18)	
  r19	=	condi_sum(l19)	
  r20	=	condi_sum(l20)
  
  r = c(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20)
  
  money = sum(
    bet*(pay[u[1, 1], r[1]] + pay[u[1, 1], r[2]] + pay[u[1, 1], r[3]] + 
           pay[u[1, 1], r[4]] + pay[u[1, 1], r[5]] + pay[u[1, 1], r[6]] + pay[u[1, 1], r[7]] + 
           pay[u[2, 1], r[8]] + pay[u[2, 1], r[9]] + pay[u[2, 1], r[10]] + pay[u[2, 1], r[11]] + 
           pay[u[2, 1], r[12]] + pay[u[2, 1], r[13]] + pay[u[2, 1], r[14]] + pay[u[3, 1], r[15]] + 
           pay[u[3, 1], r[16]] + pay[u[3, 1], r[17]] + pay[u[3, 1], r[18]] + pay[u[3, 1], r[19]] + 
           pay[u[3, 1], r[20]]))
  
  FG_1 = ifelse(sum(u == 11) == 3, 1, 0)
  FG_2 = ifelse(sum(u == 11) == 4, 1, 0)
  FG_3 = ifelse(sum(u == 11) == 5, 1, 0)
  
  BG_1 = ifelse(sum(u == 12) == 3, 1, 0)
  BG_2 = ifelse(sum(u == 12) == 4, 1, 0)
  BG_3 = ifelse(sum(u == 12) == 5, 1, 0)
  
  results = c(money, FG_1, FG_2, FG_3, BG_1, BG_2, BG_3)
  
  return(results)
  
}

nowinlength = function(z) {
  
  s1 = 1
  s2 = 0
  
  g = {}
  
  for (i in 1 : length(z[, 1])) {
    
    if (z[i, 1] == 0 && z[i, 2] == 0 && z[i, 3] == 0 && z[i, 4] == 0 && z[i, 5] == 0 && z[i, 6] == 0 && z[i, 7] == 0) {
      
      g[s1] = s2 + 1
      s2 = s2 + 1
      
    }
    else {
      
      s2 = 0
      s1 = s1 + 1
    }
    
  }
  
  q1 = na.omit(g)
  
  q_max = max(q1)
  
  q_mean = mean(q1)
  
  q_median = median(q1)
  
  q_sd = sd(q1)
  
  q2 = c(q_max)
  
  return(q2)
  
}

##### random p ####

wild = 14

p = matrix(NA, l, 15)

exp.base = {}

fg.freq = matrix(NA, l, 6)

zero = {}

for(i in 1 : l) {
  
  p.hat = seq(0.55, 0.75, by = 0.01)
  
  p[i, ] = c(sample(p.hat, 1), sample(p.hat, 1), sample(p.hat, 1), sample(p.hat, 1), sample(p.hat, 1),
             sample(p.hat, 1), sample(p.hat, 1), sample(p.hat, 1), sample(p.hat, 1), sample(p.hat, 1),
             sample(p.hat, 1), sample(p.hat, 1), sample(p.hat, 1), sample(p.hat, 1), sample(p.hat, 1))
  
  outcome = t(replicate(m, sampling_ng(p[i, ], n = 13, n1 = 12)))
  
  summary.outcome = apply(outcome, 2, sum)
  
  exp.base[i] = summary.outcome[1]/(lines_com*bet*m)
  
  fg.freq[i, ] = summary.outcome[2:7]/m
  
  zero[i] = nowinlength(outcome)
  
}

summary(exp.base)

summary(zero)

which(exp.base>0.5)

