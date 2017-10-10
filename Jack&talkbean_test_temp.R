rm(list = ls())

####### 傑克與魔豆數值模型 #######

#### 低表個數 #####

w1_L = c(1, 1, 2, 3, 2, 1, 1, 2, 2, 0)

w2_L = c(2, 3, 2, 1, 2, 1, 2, 2, 2, 0)

w3_L = c(5, 3, 4, 4, 4, 1, 3, 2, 2, 0)

w4_L = c(3, 3, 3, 1, 2, 1, 1, 2, 2, 0)

w5_L = c(1, 2, 1, 3, 2, 1, 1, 2, 3, 0)

w_L = cbind(w1_L, w2_L, w3_L, w4_L, w5_L)

N_L = apply(w_L, 2, sum)

#### 高表個數 #####

w1_H = c(2, 2, 1, 2, 2, 1, 1, 0, 2, 0)

w2_H = c(2, 2, 3, 2, 3, 1, 2, 5, 2, 0)

w3_H = c(3, 3, 3, 2, 1, 1, 2, 0, 3, 1)

w4_H = c(2, 3, 3, 3, 3, 1, 2, 5, 2, 0)

w5_H = c(2, 1, 1, 2, 2, 1, 1, 0, 2, 0)

w_H = cbind(w1_H, w2_H, w3_H, w4_H, w5_H)

N_H = apply(w_H, 2, sum)

#### FREE SPIN 表個數 #####

w1_FS = c(2, 2, 2, 1, 2, 0, 1, 0, 1, 0)

w2_FS = c(3, 3, 3, 3, 2, 0, 0, 2, 2, 0)

w3_FS = c(3, 2, 1, 5, 4, 0, 2, 0, 1, 0)

w4_FS = c(3, 3, 3, 2, 2, 0, 1, 3, 0, 0)

w5_FS = c(1, 2, 3, 1, 2, 0, 0, 0, 2, 0)

w_FS = cbind(w1_FS, w2_FS, w3_FS, w4_FS, w5_FS)

N_FS = apply(w_FS, 2, sum)

#### BONUS game 3 表個數 #####

w1_B3 = c(00, 00, 00, 00, 00, 0, 0, 3, 0, 0)

w2_B3 = c(20, 18, 16, 14, 12, 0, 0, 1, 0, 0)

w3_B3 = c(00, 00, 00, 00, 00, 0, 0, 5, 0, 0)

w4_B3 = c(20, 18, 16, 14, 12, 0, 0, 1, 0, 0)

w5_B3 = c(00, 00, 00, 00, 00, 0, 0, 3, 0, 0)

w_B3 = cbind(w1_B3, w2_B3, w3_B3, w4_B3, w5_B3)

N_B3 = apply(w_B3, 2, sum)

#############################

gt = 10 ## 平均遊戲時間 (second)

bet = 100 ## 玩家下注

fr = 10 ## 送轉數

E_general = 0.8751 ### 一般遊戲之期望值

############################

#### 賠率表

### 從出現5個開始賠

payoff = rbind(c(0, 2.15, 3.00, 4.00, 6.00, 10, 40, 100, 200), 
               c(0, 2.18, 3.25, 4.25, 6.25, 11, 45, 110, 350), 
               c(0, 2.21, 3.50, 4.50, 6.50, 12, 50, 120, 300), 
               c(0, 2.22, 3.75, 4.75, 7.00, 13, 55, 130, 350), 
               c(0, 2.24, 4.00, 5.00, 7.50, 15, 60, 150, 400))

### 賠率圖案在低表之機率

### 圖案一

p1_L = {}

n_L_sum_1 = {}

a1 = 1

j_L_1 = 1

for (i_p1_L_1 in 0 : w_L[j_L_1, 1]+w_L[10, 1])
  for (i_p1_L_2 in 0 : w_L[j_L_1, 2]+w_L[10, 2])
    for (i_p1_L_3 in 0 : w_L[j_L_1, 3]+w_L[10, 3])
      for (i_p1_L_4 in 0 : w_L[j_L_1, 4]+w_L[10, 4])
        for (i_p1_L_5 in 0 : w_L[j_L_1, 5]+w_L[10, 5])
        {
{
{
{
{
  p1_L[a1] = choose(w_L[j_L_1, 1]+w_L[10, 1], i_p1_L_1)*choose(N_L[1]-w_L[j_L_1, 1]-w_L[10, 1], 3 - i_p1_L_1)/choose(N_L[1], 3)*
    choose(w_L[j_L_1, 2]+w_L[10, 2], i_p1_L_2)*choose(N_L[2]-w_L[j_L_1, 2]-w_L[10, 2], 4 - i_p1_L_2)/choose(N_L[2], 4)*
    choose(w_L[j_L_1, 3]+w_L[10, 3], i_p1_L_3)*choose(N_L[3]-w_L[j_L_1, 3]-w_L[10, 3], 5 - i_p1_L_3)/choose(N_L[3], 5)*
    choose(w_L[j_L_1, 4]+w_L[10, 4], i_p1_L_4)*choose(N_L[4]-w_L[j_L_1, 4]-w_L[10, 4], 4 - i_p1_L_4)/choose(N_L[4], 4)*
    choose(w_L[j_L_1, 5]+w_L[10, 5], i_p1_L_5)*choose(N_L[5]-w_L[j_L_1, 5]-w_L[10, 5], 3 - i_p1_L_5)/choose(N_L[5], 3)
  
  n_L_sum_1[a1] = i_p1_L_1 + i_p1_L_2 + i_p1_L_3 + i_p1_L_4 + i_p1_L_5
  
  a1 = a1 + 1
  
}
}
}
}
        }

p1_L_pay = c(sum(p1_L[which(n_L_sum_1 == 4)]), sum(p1_L[which(n_L_sum_1 == 5)]), 
             sum(p1_L[which(n_L_sum_1 == 6)]), sum(p1_L[which(n_L_sum_1 == 7)]), sum(p1_L[which(n_L_sum_1 == 8)]),
             sum(p1_L[which(n_L_sum_1 == 9)]), sum(p1_L[which(n_L_sum_1 == 10)]), sum(p1_L[which(n_L_sum_1 == 11)]), 
             sum(p1_L[which(n_L_sum_1 == 12)]))

###### 圖案二

p2_L = {}

n_L_sum_2 = {}

a2 = 1

j_L_2 = 2

for (i_p2_L_1 in 0 : w_L[j_L_2, 1]+w_L[10, 1])
  for (i_p2_L_2 in 0 : w_L[j_L_2, 2]+w_L[10, 2])
    for (i_p2_L_3 in 0 : w_L[j_L_2, 3]+w_L[10, 3])
      for (i_p2_L_4 in 0 : w_L[j_L_2, 4]+w_L[10, 4])
        for (i_p2_L_5 in 0 : w_L[j_L_2, 5]+w_L[10, 5])
        {
{
{
{
{
  p2_L[a2] = choose(w_L[j_L_2, 1]+w_L[10, 1], i_p2_L_1)*choose(N_L[1]-w_L[j_L_2, 1]-w_L[10, 1], 3 - i_p2_L_1)/choose(N_L[1], 3)*
    choose(w_L[j_L_2, 2]+w_L[10, 2], i_p2_L_2)*choose(N_L[2]-w_L[j_L_2, 2]-w_L[10, 2], 4 - i_p2_L_2)/choose(N_L[2], 4)*
    choose(w_L[j_L_2, 3]+w_L[10, 3], i_p2_L_3)*choose(N_L[3]-w_L[j_L_2, 3]-w_L[10, 3], 5 - i_p2_L_3)/choose(N_L[3], 5)*
    choose(w_L[j_L_2, 4]+w_L[10, 4], i_p2_L_4)*choose(N_L[4]-w_L[j_L_2, 4]-w_L[10, 4], 4 - i_p2_L_4)/choose(N_L[4], 4)*
    choose(w_L[j_L_2, 5]+w_L[10, 5], i_p2_L_5)*choose(N_L[5]-w_L[j_L_2, 5]-w_L[10, 5], 3 - i_p2_L_5)/choose(N_L[5], 3)
  
  n_L_sum_2[a2] = i_p2_L_1 + i_p2_L_2 + i_p2_L_3 + i_p2_L_4 + i_p2_L_5
  
  a2 = a2 + 1
  
}
}
}
}
        }

p2_L_pay = c(sum(p2_L[which(n_L_sum_2 == 4)]), sum(p2_L[which(n_L_sum_2 == 5)]), 
             sum(p2_L[which(n_L_sum_2 == 6)]), sum(p2_L[which(n_L_sum_2 == 7)]), sum(p2_L[which(n_L_sum_2 == 8)]),
             sum(p2_L[which(n_L_sum_2 == 9)]), sum(p2_L[which(n_L_sum_2 == 10)]),sum(p2_L[which(n_L_sum_2 == 11)]), 
             sum(p2_L[which(n_L_sum_2 == 12)]))

##### 圖案三

p3_L = {}

n_L_sum_3 = {}

a3 = 1

j_L_3 = 3

for (i_p3_L_1 in 0 : w_L[j_L_3, 1]+w_L[10, 1])
  for (i_p3_L_2 in 0 : w_L[j_L_3, 2]+w_L[10, 2])
    for (i_p3_L_3 in 0 : w_L[j_L_3, 3]+w_L[10, 3])
      for (i_p3_L_4 in 0 : w_L[j_L_3, 4]+w_L[10, 4])
        for (i_p3_L_5 in 0 : w_L[j_L_3, 5]+w_L[10, 5])
        {
{
{
{
{
  p3_L[a3] = choose(w_L[j_L_3, 1]+w_L[10, 1], i_p3_L_1)*choose(N_L[1]-w_L[j_L_3, 1]-w_L[10, 1], 3 - i_p3_L_1)/choose(N_L[1], 3)*
    choose(w_L[j_L_3, 2]+w_L[10, 2], i_p3_L_2)*choose(N_L[2]-w_L[j_L_3, 2]-w_L[10, 2], 4 - i_p3_L_2)/choose(N_L[2], 4)*
    choose(w_L[j_L_3, 3]+w_L[10, 3], i_p3_L_3)*choose(N_L[3]-w_L[j_L_3, 3]-w_L[10, 3], 5 - i_p3_L_3)/choose(N_L[3], 5)*
    choose(w_L[j_L_3, 4]+w_L[10, 4], i_p3_L_4)*choose(N_L[4]-w_L[j_L_3, 4]-w_L[10, 4], 4 - i_p3_L_4)/choose(N_L[4], 4)*
    choose(w_L[j_L_3, 5]+w_L[10, 5], i_p3_L_5)*choose(N_L[5]-w_L[j_L_3, 5]-w_L[10, 5], 3 - i_p3_L_5)/choose(N_L[5], 3)
  
  n_L_sum_3[a3] = i_p3_L_1 + i_p3_L_2 + i_p3_L_3 + i_p3_L_4 + i_p3_L_5
  
  a3 = a3 + 1
  
}
}
}
}
        }

p3_L_pay = c(sum(p3_L[which(n_L_sum_3 == 4)]), sum(p3_L[which(n_L_sum_3 == 5)]),
             sum(p3_L[which(n_L_sum_3 == 6)]), sum(p3_L[which(n_L_sum_3 == 7)]), sum(p3_L[which(n_L_sum_3 == 8)]),
             sum(p3_L[which(n_L_sum_3 == 9)]), sum(p3_L[which(n_L_sum_3 == 10)]), sum(p3_L[which(n_L_sum_3 == 11)]), 
             sum(p3_L[which(n_L_sum_3 == 12)]))

###### 圖案四

p4_L = {}

n_L_sum_4 = {}

a4 = 1

j_L_4 = 4

for (i_p4_L_1 in 0 : w_L[j_L_4, 1]+w_L[10, 1])
  for (i_p4_L_2 in 0 : w_L[j_L_4, 2]+w_L[10, 2])
    for (i_p4_L_3 in 0 : w_L[j_L_4, 3]+w_L[10, 3])
      for (i_p4_L_4 in 0 : w_L[j_L_4, 4]+w_L[10, 4])
        for (i_p4_L_5 in 0 : w_L[j_L_4, 5]+w_L[10, 5])
        {
{
{
{
{
  p4_L[a4] = choose(w_L[j_L_4, 1]+w_L[10, 1], i_p4_L_1)*choose(N_L[1]-w_L[j_L_4, 1]-w_L[10, 1], 3 - i_p4_L_1)/choose(N_L[1], 3)*
    choose(w_L[j_L_4, 2]+w_L[10, 2], i_p4_L_2)*choose(N_L[2]-w_L[j_L_4, 2]-w_L[10, 2], 4 - i_p4_L_2)/choose(N_L[2], 4)*
    choose(w_L[j_L_4, 3]+w_L[10, 3], i_p4_L_3)*choose(N_L[3]-w_L[j_L_4, 3]-w_L[10, 3], 5 - i_p4_L_3)/choose(N_L[3], 5)*
    choose(w_L[j_L_4, 4]+w_L[10, 4], i_p4_L_4)*choose(N_L[4]-w_L[j_L_4, 4]-w_L[10, 4], 4 - i_p4_L_4)/choose(N_L[4], 4)*
    choose(w_L[j_L_4, 5]+w_L[10, 5], i_p4_L_5)*choose(N_L[5]-w_L[j_L_4, 5]-w_L[10, 5], 3 - i_p4_L_5)/choose(N_L[5], 3)
  
  n_L_sum_4[a4] = i_p4_L_1 + i_p4_L_2 + i_p4_L_3 + i_p4_L_4 + i_p4_L_5
  
  a4 = a4 + 1
  
}
}
}
}
        }

p4_L_pay = c(sum(p4_L[which(n_L_sum_4 == 4)]), sum(p4_L[which(n_L_sum_4 == 5)]), 
             sum(p4_L[which(n_L_sum_4 == 6)]), sum(p4_L[which(n_L_sum_4 == 7)]), sum(p4_L[which(n_L_sum_4 == 8)]),
             sum(p4_L[which(n_L_sum_4 == 9)]), sum(p4_L[which(n_L_sum_4 == 10)]), sum(p4_L[which(n_L_sum_4 == 11)]), 
             sum(p4_L[which(n_L_sum_4 == 12)]))

##### 圖案五

p5_L = {}

n_L_sum_5 = {}

a5 = 1

j_L_5 = 5

for (i_p5_L_1 in 0 : w_L[j_L_5, 1]+w_L[10, 1])
  for (i_p5_L_2 in 0 : w_L[j_L_5, 2]+w_L[10, 2])
    for (i_p5_L_3 in 0 : w_L[j_L_5, 3]+w_L[10, 3])
      for (i_p5_L_4 in 0 : w_L[j_L_5, 4]+w_L[10, 4])
        for (i_p5_L_5 in 0 : w_L[j_L_5, 5]+w_L[10, 5])
        {
{
{
{
{
  p5_L[a5] = choose(w_L[j_L_5, 1]+w_L[10, 1], i_p5_L_1)*choose(N_L[1]-w_L[j_L_5, 1]-w_L[10, 1], 3 - i_p5_L_1)/choose(N_L[1], 3)*
    choose(w_L[j_L_5, 2]+w_L[10, 2], i_p5_L_2)*choose(N_L[2]-w_L[j_L_5, 2]-w_L[10, 2], 4 - i_p5_L_2)/choose(N_L[2], 4)*
    choose(w_L[j_L_5, 3]+w_L[10, 3], i_p5_L_3)*choose(N_L[3]-w_L[j_L_5, 3]-w_L[10, 3], 5 - i_p5_L_3)/choose(N_L[3], 5)*
    choose(w_L[j_L_5, 4]+w_L[10, 4], i_p5_L_4)*choose(N_L[4]-w_L[j_L_5, 4]-w_L[10, 4], 4 - i_p5_L_4)/choose(N_L[4], 4)*
    choose(w_L[j_L_5, 5]+w_L[10, 5], i_p5_L_5)*choose(N_L[5]-w_L[j_L_5, 5]-w_L[10, 5], 3 - i_p5_L_5)/choose(N_L[5], 3)
  
  n_L_sum_5[a5] = i_p5_L_1 + i_p5_L_2 + i_p5_L_3 + i_p5_L_4 + i_p5_L_5
  
  a5 = a5 + 1
  
}
}
}
}
        }

p5_L_pay = c(sum(p5_L[which(n_L_sum_5 == 4)]), sum(p5_L[which(n_L_sum_5 == 5)]), 
             sum(p5_L[which(n_L_sum_5 == 6)]), sum(p5_L[which(n_L_sum_5 == 7)]), sum(p5_L[which(n_L_sum_5 == 8)]),
             sum(p5_L[which(n_L_sum_5 == 9)]), sum(p5_L[which(n_L_sum_5 == 10)]), sum(p5_L[which(n_L_sum_5 == 11)]), 
             sum(p5_L[which(n_L_sum_5 == 12)]))

p_L_pay = rbind(p1_L_pay, p2_L_pay, p3_L_pay, p4_L_pay, p5_L_pay)

######### 賠率圖案在高表之機率

#### 圖案一

p1_H = {}

n_H_sum_1 = {}

b1 = 1

j_H_1 = 1

for (i_p1_H_1 in 0 : w_H[j_H_1, 1]+w_H[10, 1])
  for (i_p1_H_2 in 0 : w_H[j_H_1, 2]+w_H[10, 2])
    for (i_p1_H_3 in 0 : w_H[j_H_1, 3]+w_H[10, 3])
      for (i_p1_H_4 in 0 : w_H[j_H_1, 4]+w_H[10, 4])
        for (i_p1_H_5 in 0 : w_H[j_H_1, 5]+w_H[10, 5])
        {
{
{
{
{
  p1_H[b1] = choose(w_H[j_H_1, 1]+w_H[10, 1], i_p1_H_1)*choose(N_H[1]-w_H[j_H_1, 1]-w_H[10, 1], 3 - i_p1_H_1)/choose(N_H[1], 3)*
    choose(w_H[j_H_1, 2]+w_H[10, 2], i_p1_H_2)*choose(N_H[2]-w_H[j_H_1, 2]-w_H[10, 2], 4 - i_p1_H_2)/choose(N_H[2], 4)*
    choose(w_H[j_H_1, 3]+w_H[10, 3], i_p1_H_3)*choose(N_H[3]-w_H[j_H_1, 3]-w_H[10, 3], 5 - i_p1_H_3)/choose(N_H[3], 5)*
    choose(w_H[j_H_1, 4]+w_H[10, 4], i_p1_H_4)*choose(N_H[4]-w_H[j_H_1, 4]-w_H[10, 4], 4 - i_p1_H_4)/choose(N_H[4], 4)*
    choose(w_H[j_H_1, 5]+w_H[10, 5], i_p1_H_5)*choose(N_H[5]-w_H[j_H_1, 5]-w_H[10, 5], 3 - i_p1_H_5)/choose(N_H[5], 3)
  
  n_H_sum_1[b1] = i_p1_H_1 + i_p1_H_2 + i_p1_H_3 + i_p1_H_4 + i_p1_H_5
  
  b1 = b1 + 1
  
}
}
}
}
        }

p1_H_pay = c(sum(p1_H[which(n_H_sum_1 == 4)]), sum(p1_H[which(n_H_sum_1 == 5)]), 
             sum(p1_H[which(n_H_sum_1 == 6)]), sum(p1_H[which(n_H_sum_1 == 7)]), sum(p1_H[which(n_H_sum_1 == 8)]),
             sum(p1_H[which(n_H_sum_1 == 9)]), sum(p1_H[which(n_H_sum_1 == 10)]), sum(p1_H[which(n_H_sum_1 == 11)]), 
             sum(p1_H[which(n_H_sum_1 == 12)]))

###### 圖案二

p2_H = {}

n_H_sum_2 = {}

b2 = 1

j_H_2 = 2

for (i_p2_H_1 in 0 : w_H[j_H_2, 1]+w_H[10, 1])
  for (i_p2_H_2 in 0 : w_H[j_H_2, 2]+w_H[10, 2])
    for (i_p2_H_3 in 0 : w_H[j_H_2, 3]+w_H[10, 3])
      for (i_p2_H_4 in 0 : w_H[j_H_2, 4]+w_H[10, 4])
        for (i_p2_H_5 in 0 : w_H[j_H_2, 5]+w_H[10, 5])
        {
{
{
{
{
  p2_H[b2] = choose(w_H[j_H_2, 1]+w_H[10, 1], i_p2_H_1)*choose(N_H[1]-w_H[j_H_2, 1]-w_H[10, 1], 3 - i_p2_H_1)/choose(N_H[1], 3)*
    choose(w_H[j_H_2, 2]+w_H[10, 2], i_p2_H_2)*choose(N_H[2]-w_H[j_H_2, 2]-w_H[10, 2], 4 - i_p2_H_2)/choose(N_H[2], 4)*
    choose(w_H[j_H_2, 3]+w_H[10, 3], i_p2_H_3)*choose(N_H[3]-w_H[j_H_2, 3]-w_H[10, 3], 5 - i_p2_H_3)/choose(N_H[3], 5)*
    choose(w_H[j_H_2, 4]+w_H[10, 4], i_p2_H_4)*choose(N_H[4]-w_H[j_H_2, 4]-w_H[10, 4], 4 - i_p2_H_4)/choose(N_H[4], 4)*
    choose(w_H[j_H_2, 5]+w_H[10, 5], i_p2_H_5)*choose(N_H[5]-w_H[j_H_2, 5]-w_H[10, 5], 3 - i_p2_H_5)/choose(N_H[5], 3)
  
  n_H_sum_2[b2] = i_p2_H_1 + i_p2_H_2 + i_p2_H_3 + i_p2_H_4 + i_p2_H_5
  
  b2 = b2 + 1
  
}
}
}
}
        }

p2_H_pay = c(sum(p2_H[which(n_H_sum_2 == 4)]), sum(p2_H[which(n_H_sum_2 == 5)]), 
             sum(p2_H[which(n_H_sum_2 == 6)]), sum(p2_H[which(n_H_sum_2 == 7)]), sum(p2_H[which(n_H_sum_2 == 8)]),
             sum(p2_H[which(n_H_sum_2 == 9)]), sum(p2_H[which(n_H_sum_2 == 10)]),sum(p2_H[which(n_H_sum_2 == 11)]), 
             sum(p2_H[which(n_H_sum_2 == 12)]))

###### 圖案三

p3_H = {}

n_H_sum_3 = {}

b3 = 1

j_H_3 = 3

for (i_p3_H_1 in 0 : w_H[j_H_3, 1]+w_H[10, 1])
  for (i_p3_H_2 in 0 : w_H[j_H_3, 2]+w_H[10, 2])
    for (i_p3_H_3 in 0 : w_H[j_H_3, 3]+w_H[10, 3])
      for (i_p3_H_4 in 0 : w_H[j_H_3, 4]+w_H[10, 4])
        for (i_p3_H_5 in 0 : w_H[j_H_3, 5]+w_H[10, 5])
        {
{
{
{
{
  p3_H[b3] = choose(w_H[j_H_3, 1]+w_H[10, 1], i_p3_H_1)*choose(N_H[1]-w_H[j_H_3, 1]-w_H[10, 1], 3 - i_p3_H_1)/choose(N_H[1], 3)*
    choose(w_H[j_H_3, 2]+w_H[10, 2], i_p3_H_2)*choose(N_H[2]-w_H[j_H_3, 2]-w_H[10, 2], 4 - i_p3_H_2)/choose(N_H[2], 4)*
    choose(w_H[j_H_3, 3]+w_H[10, 3], i_p3_H_3)*choose(N_H[3]-w_H[j_H_3, 3]-w_H[10, 3], 5 - i_p3_H_3)/choose(N_H[3], 5)*
    choose(w_H[j_H_3, 4]+w_H[10, 4], i_p3_H_4)*choose(N_H[4]-w_H[j_H_3, 4]-w_H[10, 4], 4 - i_p3_H_4)/choose(N_H[4], 4)*
    choose(w_H[j_H_3, 5]+w_H[10, 5], i_p3_H_5)*choose(N_H[5]-w_H[j_H_3, 5]-w_H[10, 5], 3 - i_p3_H_5)/choose(N_H[5], 3)
  
  n_H_sum_3[b3] = i_p3_H_1 + i_p3_H_2 + i_p3_H_3 + i_p3_H_4 + i_p3_H_5
  
  b3 = b3 + 1
  
}
}
}
}
        }

p3_H_pay = c(sum(p3_H[which(n_H_sum_3 == 4)]), sum(p3_H[which(n_H_sum_3 == 5)]),
             sum(p3_H[which(n_H_sum_3 == 6)]), sum(p3_H[which(n_H_sum_3 == 7)]), sum(p3_H[which(n_H_sum_3 == 8)]),
             sum(p3_H[which(n_H_sum_3 == 9)]), sum(p3_H[which(n_H_sum_3 == 10)]), sum(p3_H[which(n_H_sum_3 == 11)]), 
             sum(p3_H[which(n_H_sum_3 == 12)]))

###### 圖案四

p4_H = {}

n_H_sum_4 = {}

b4 = 1

j_H_4 = 4

for (i_p4_H_1 in 0 : w_H[j_H_4, 1]+w_H[10, 1])
  for (i_p4_H_2 in 0 : w_H[j_H_4, 2]+w_H[10, 2])
    for (i_p4_H_3 in 0 : w_H[j_H_4, 3]+w_H[10, 3])
      for (i_p4_H_4 in 0 : w_H[j_H_4, 4]+w_H[10, 4])
        for (i_p4_H_5 in 0 : w_H[j_H_4, 5]+w_H[10, 5])
        {
{
{
{
{
  p4_H[b4] = choose(w_H[j_H_4, 1]+w_H[10, 1], i_p4_H_1)*choose(N_H[1]-w_H[j_H_4, 1]-w_H[10, 1], 3 - i_p4_H_1)/choose(N_H[1], 3)*
    choose(w_H[j_H_4, 2]+w_H[10, 2], i_p4_H_2)*choose(N_H[2]-w_H[j_H_4, 2]-w_H[10, 2], 4 - i_p4_H_2)/choose(N_H[2], 4)*
    choose(w_H[j_H_4, 3]+w_H[10, 3], i_p4_H_3)*choose(N_H[3]-w_H[j_H_4, 3]-w_H[10, 3], 5 - i_p4_H_3)/choose(N_H[3], 5)*
    choose(w_H[j_H_4, 4]+w_H[10, 4], i_p4_H_4)*choose(N_H[4]-w_H[j_H_4, 4]-w_H[10, 4], 4 - i_p4_H_4)/choose(N_H[4], 4)*
    choose(w_H[j_H_4, 5]+w_H[10, 5], i_p4_H_5)*choose(N_H[5]-w_H[j_H_4, 5]-w_H[10, 5], 3 - i_p4_H_5)/choose(N_H[5], 3)
  
  n_H_sum_4[b4] = i_p4_H_1 + i_p4_H_2 + i_p4_H_3 + i_p4_H_4 + i_p4_H_5
  
  b4 = b4 + 1
  
}
}
}
}
        }

p4_H_pay = c(sum(p4_H[which(n_H_sum_4 == 4)]), sum(p4_H[which(n_H_sum_4 == 5)]), 
             sum(p4_H[which(n_H_sum_4 == 6)]), sum(p4_H[which(n_H_sum_4 == 7)]), sum(p4_H[which(n_H_sum_4 == 8)]),
             sum(p4_H[which(n_H_sum_4 == 9)]), sum(p4_H[which(n_H_sum_4 == 10)]), sum(p4_H[which(n_H_sum_4 == 11)]), 
             sum(p4_H[which(n_H_sum_4 == 12)]))

###### 圖案五

p5_H = {}

n_H_sum_5 = {}

b5 = 1

j_H_5 = 5

for (i_p5_H_1 in 0 : w_H[j_H_5, 1]+w_H[10, 1])
  for (i_p5_H_2 in 0 : w_H[j_H_5, 2]+w_H[10, 2])
    for (i_p5_H_3 in 0 : w_H[j_H_5, 3]+w_H[10, 3])
      for (i_p5_H_4 in 0 : w_H[j_H_5, 4]+w_H[10, 4])
        for (i_p5_H_5 in 0 : w_H[j_H_5, 5]+w_H[10, 5])
        {
{
{
{
{
  p5_H[b5] = choose(w_H[j_H_5, 1]+w_H[10, 1], i_p5_H_1)*choose(N_H[1]-w_H[j_H_5, 1]-w_H[10, 1], 3 - i_p5_H_1)/choose(N_H[1], 3)*
    choose(w_H[j_H_5, 2]+w_H[10, 2], i_p5_H_2)*choose(N_H[2]-w_H[j_H_5, 2]-w_H[10, 2], 4 - i_p5_H_2)/choose(N_H[2], 4)*
    choose(w_H[j_H_5, 3]+w_H[10, 3], i_p5_H_3)*choose(N_H[3]-w_H[j_H_5, 3]-w_H[10, 3], 5 - i_p5_H_3)/choose(N_H[3], 5)*
    choose(w_H[j_H_5, 4]+w_H[10, 4], i_p5_H_4)*choose(N_H[4]-w_H[j_H_5, 4]-w_H[10, 4], 4 - i_p5_H_4)/choose(N_H[4], 4)*
    choose(w_H[j_H_5, 5]+w_H[10, 5], i_p5_H_5)*choose(N_H[5]-w_H[j_H_5, 5]-w_H[10, 5], 3 - i_p5_H_5)/choose(N_H[5], 3)
  
  n_H_sum_5[b5] = i_p5_H_1 + i_p5_H_2 + i_p5_H_3 + i_p5_H_4 + i_p5_H_5
  
  b5 = b5 + 1
  
}
}
}
}
        }

p5_H_pay = c(sum(p5_H[which(n_H_sum_5 == 4)]), sum(p5_H[which(n_H_sum_5 == 5)]), 
             sum(p5_H[which(n_H_sum_5 == 6)]), sum(p5_H[which(n_H_sum_5 == 7)]), sum(p5_H[which(n_H_sum_5 == 8)]),
             sum(p5_H[which(n_H_sum_5 == 9)]), sum(p5_H[which(n_H_sum_5 == 10)]),sum(p5_H[which(n_H_sum_5 == 11)]), 
             sum(p5_H[which(n_H_sum_5 == 12)]))

p_H_pay = rbind(p1_H_pay, p2_H_pay, p3_H_pay, p4_H_pay, p5_H_pay)

##### 由低表進入Free spin & bonus games 的機率

#### Free spin 

p6_L = {}

n_L_sum_6 = {}

a6 = 1

j_L_6 = 6

for (i_p6_L_1 in 0 : w_L[j_L_6, 1])
  for (i_p6_L_2 in 0 : w_L[j_L_6, 2])
    for (i_p6_L_3 in 0 : w_L[j_L_6, 3])
      for (i_p6_L_4 in 0 : w_L[j_L_6, 4])
        for (i_p6_L_5 in 0 : w_L[j_L_6, 5])
        {
{
{
{
{
  p6_L[a6] = choose(w_L[j_L_6, 1], i_p6_L_1)*choose(N_L[1]-w_L[j_L_6, 1], 3 - i_p6_L_1)/choose(N_L[1], 3)*
    choose(w_L[j_L_6, 2], i_p6_L_2)*choose(N_L[2]-w_L[j_L_6, 2], 4 - i_p6_L_2)/choose(N_L[2], 4)*
    choose(w_L[j_L_6, 3], i_p6_L_3)*choose(N_L[3]-w_L[j_L_6, 3], 5 - i_p6_L_3)/choose(N_L[3], 5)*
    choose(w_L[j_L_6, 4], i_p6_L_4)*choose(N_L[4]-w_L[j_L_6, 4], 4 - i_p6_L_4)/choose(N_L[4], 4)*
    choose(w_L[j_L_6, 5], i_p6_L_5)*choose(N_L[5]-w_L[j_L_6, 5], 3 - i_p6_L_5)/choose(N_L[5], 3)
  
  n_L_sum_6[a6] = i_p6_L_1 + i_p6_L_2 + i_p6_L_3 + i_p6_L_4 + i_p6_L_5
  
  a6 = a6 + 1
  
}
}
}
}
        }

p6_L_enter_FS = c(sum(p6_L[which(n_L_sum_6 == 1)]), sum(p6_L[which(n_L_sum_6 == 2)]), 
                  sum(p6_L[which(n_L_sum_6 == 3)]), sum(p6_L[which(n_L_sum_6 == 4)]),
                  sum(p6_L[which(n_L_sum_6 == 5)]))

##### Bonus game 1

p7_L = {}

n_L_sum_7 = {}

a7 = 1

j_L_7 = 7

for (i_p7_L_1 in 0 : w_L[j_L_7, 1])
  for (i_p7_L_2 in 0 : w_L[j_L_7, 2])
    for (i_p7_L_3 in 0 : w_L[j_L_7, 3])
      for (i_p7_L_4 in 0 : w_L[j_L_7, 4])
        for (i_p7_L_5 in 0 : w_L[j_L_7, 5])
        {
{
{
{
{
  p7_L [a7] = choose(w_L[j_L_7, 1], i_p7_L_1)*choose(N_L[1]-w_L[j_L_7, 1], 3 - i_p7_L_1)/choose(N_L[1], 3)*
    choose(w_L[j_L_7, 2], i_p7_L_2)*choose(N_L[2]-w_L[j_L_7, 2], 4 - i_p7_L_2)/choose(N_L[2], 4)*
    choose(w_L[j_L_7, 3], i_p7_L_3)*choose(N_L[3]-w_L[j_L_7, 3], 5 - i_p7_L_3)/choose(N_L[3], 5)*
    choose(w_L[j_L_7, 4], i_p7_L_4)*choose(N_L[4]-w_L[j_L_7, 4], 4 - i_p7_L_4)/choose(N_L[4], 4)*
    choose(w_L[j_L_7, 5], i_p7_L_5)*choose(N_L[5]-w_L[j_L_7, 5], 3 - i_p7_L_5)/choose(N_L[5], 3)
  
  n_L_sum_7[a7] = i_p7_L_1 + i_p7_L_2 + i_p7_L_3 + i_p7_L_4 + i_p7_L_5
  
  a7 = a7 + 1
  
}
}
}
}
        }

p7_L_enter_B1 = c(sum(p7_L[which(n_L_sum_7 == 1)]), sum(p7_L[which(n_L_sum_7 == 2)]), 
                  sum(p7_L[which(n_L_sum_7 == 3)]), sum(p7_L[which(n_L_sum_7 == 4)]),
                  sum(p7_L[which(n_L_sum_7 == 5)]), sum(p7_L[which(n_L_sum_7 == 6)]),
                  sum(p7_L[which(n_L_sum_7 == 7)]), sum(p7_L[which(n_L_sum_7 == 8)]))

###### Bonus game 2

p8_L = {}

n_L_sum_8 = {}

a8 = 1

j_L_8 = 8

for (i_p8_L_1 in 0 : w_L[j_L_8, 1])
  for (i_p8_L_2 in 0 : w_L[j_L_8, 2])
    for (i_p8_L_3 in 0 : w_L[j_L_8, 3])
      for (i_p8_L_4 in 0 : w_L[j_L_8, 4])
        for (i_p8_L_5 in 0 : w_L[j_L_8, 5])
        {
{
{
{
{
  p8_L [a8] = choose(w_L[j_L_8, 1], i_p8_L_1)*choose(N_L[1]-w_L[j_L_8, 1], 3 - i_p8_L_1)/choose(N_L[1], 3)*
    choose(w_L[j_L_8, 2], i_p8_L_2)*choose(N_L[2]-w_L[j_L_8, 2], 4 - i_p8_L_2)/choose(N_L[2], 4)*
    choose(w_L[j_L_8, 3], i_p8_L_3)*choose(N_L[3]-w_L[j_L_8, 3], 5 - i_p8_L_3)/choose(N_L[3], 5)*
    choose(w_L[j_L_8, 4], i_p8_L_4)*choose(N_L[4]-w_L[j_L_8, 4], 4 - i_p8_L_4)/choose(N_L[4], 4)*
    choose(w_L[j_L_8, 5], i_p8_L_5)*choose(N_L[5]-w_L[j_L_8, 5], 3 - i_p8_L_5)/choose(N_L[5], 3)
  
  n_L_sum_8[a8] = i_p8_L_1 + i_p8_L_2 + i_p8_L_3 + i_p8_L_4 + i_p8_L_5
  
  a8 = a8 + 1
  
}
}
}
}
        }

p8_L_enter_B2 = c(sum(p8_L[which(n_L_sum_8 == 3)]), sum(p8_L[which(n_L_sum_8 == 4)]), 
                  sum(p8_L[which(n_L_sum_8 == 5)]), sum(p8_L[which(n_L_sum_8 == 6)]),
                  sum(p8_L[which(n_L_sum_8 == 7)]), sum(p8_L[which(n_L_sum_8 == 8)]),
                  sum(p8_L[which(n_L_sum_8 == 9)]), sum(p8_L[which(n_L_sum_8 == 10)]))



##### Bonus game 3

p9_L = {}

n_L_sum_9 = {}

a9 = 1

j_L_9 = 9

for (i_p9_L_1 in 0 : w_L[j_L_9, 1])
  for (i_p9_L_2 in 0 : w_L[j_L_9, 2])
    for (i_p9_L_3 in 0 : w_L[j_L_9, 3])
      for (i_p9_L_4 in 0 : w_L[j_L_9, 4])
        for (i_p9_L_5 in 0 : w_L[j_L_9, 5])
        {
{
{
{
{
  p9_L [a9] = choose(w_L[j_L_9, 1], i_p9_L_1)*choose(N_L[1]-w_L[j_L_9, 1], 3 - i_p9_L_1)/choose(N_L[1], 3)*
    choose(w_L[j_L_9, 2], i_p9_L_2)*choose(N_L[2]-w_L[j_L_9, 2], 4 - i_p9_L_2)/choose(N_L[2], 4)*
    choose(w_L[j_L_9, 3], i_p9_L_3)*choose(N_L[3]-w_L[j_L_9, 3], 5 - i_p9_L_3)/choose(N_L[3], 5)*
    choose(w_L[j_L_9, 4], i_p9_L_4)*choose(N_L[4]-w_L[j_L_9, 4], 4 - i_p9_L_4)/choose(N_L[4], 4)*
    choose(w_L[j_L_9, 5], i_p9_L_5)*choose(N_L[5]-w_L[j_L_9, 5], 3 - i_p9_L_5)/choose(N_L[5], 3)
  
  n_L_sum_9[a9] = i_p9_L_1 + i_p9_L_2 + i_p9_L_3 + i_p9_L_4 + i_p9_L_5
  
  a9 = a9 + 1
  
}
}
}
}
        }

p9_L_enter_B3 = c(sum(p9_L[which(n_L_sum_9 == 4)]), sum(p9_L[which(n_L_sum_9 == 5)]), 
                  sum(p9_L[which(n_L_sum_9 == 6)]), sum(p9_L[which(n_L_sum_9 == 7)]), 
                  sum(p9_L[which(n_L_sum_9 == 8)]), sum(p9_L[which(n_L_sum_9 == 9)]), 
                  sum(p9_L[which(n_L_sum_9 == 10)]), sum(p9_L[which(n_L_sum_9 == 11)]))

##### 由高表進入Free spin & bonus games 的機率

#### Free spin

p6_H = {}

n_H_sum_6 = {}

b6 = 1

j_H_6 = 6

for (i_p6_H_1 in 0 : w_H[j_H_6, 1])
  for (i_p6_H_2 in 0 : w_H[j_H_6, 2])
    for (i_p6_H_3 in 0 : w_H[j_H_6, 3])
      for (i_p6_H_4 in 0 : w_H[j_H_6, 4])
        for (i_p6_H_5 in 0 : w_H[j_H_6, 5])
        {
{
{
{
{
  p6_H[b6] = choose(w_H[j_H_6, 1], i_p6_H_1)*choose(N_H[1]-w_H[j_H_6, 1], 3 - i_p6_H_1)/choose(N_H[1], 3)*
    choose(w_H[j_H_6, 2], i_p6_H_2)*choose(N_H[2]-w_H[j_H_6, 2], 4 - i_p6_H_2)/choose(N_H[2], 4)*
    choose(w_H[j_H_6, 3], i_p6_H_3)*choose(N_H[3]-w_H[j_H_6, 3], 5 - i_p6_H_3)/choose(N_H[3], 5)*
    choose(w_H[j_H_6, 4], i_p6_H_4)*choose(N_H[4]-w_H[j_H_6, 4], 4 - i_p6_H_4)/choose(N_H[4], 4)*
    choose(w_H[j_H_6, 5], i_p6_H_5)*choose(N_H[5]-w_H[j_H_6, 5], 3 - i_p6_H_5)/choose(N_H[5], 3)
  
  n_H_sum_6[b6] = i_p6_H_1 + i_p6_H_2 + i_p6_H_3 + i_p6_H_4 + i_p6_H_5
  
  b6 = b6 + 1
  
}
}
}
}
        }

p6_H_enter_FS = c(sum(p6_H[which(n_H_sum_6 == 1)]), sum(p6_H[which(n_H_sum_6 == 2)]), 
                  sum(p6_H[which(n_H_sum_6 == 3)]), sum(p6_H[which(n_H_sum_6 == 4)]),
                  sum(p6_H[which(n_H_sum_6 == 5)]))

##### Bonus game 1

p7_H = {}

n_H_sum_7 = {}

b7 = 1

j_H_7 = 7

for (i_p7_H_1 in 0 : w_H[j_H_7, 1])
  for (i_p7_H_2 in 0 : w_H[j_H_7, 2])
    for (i_p7_H_3 in 0 : w_H[j_H_7, 3])
      for (i_p7_H_4 in 0 : w_H[j_H_7, 4])
        for (i_p7_H_5 in 0 : w_H[j_H_7, 5])
        {
{
{
{
{
  p7_H [b7] = choose(w_H[j_H_7, 1], i_p7_H_1)*choose(N_H[1]-w_H[j_H_7, 1], 3 - i_p7_H_1)/choose(N_H[1], 3)*
    choose(w_H[j_H_7, 2], i_p7_H_2)*choose(N_H[2]-w_H[j_H_7, 2], 4 - i_p7_H_2)/choose(N_H[2], 4)*
    choose(w_H[j_H_7, 3], i_p7_H_3)*choose(N_H[3]-w_H[j_H_7, 3], 5 - i_p7_H_3)/choose(N_H[3], 5)*
    choose(w_H[j_H_7, 4], i_p7_H_4)*choose(N_H[4]-w_H[j_H_7, 4], 4 - i_p7_H_4)/choose(N_H[4], 4)*
    choose(w_H[j_H_7, 5], i_p7_H_5)*choose(N_H[5]-w_H[j_H_7, 5], 3 - i_p7_H_5)/choose(N_H[5], 3)
  
  n_H_sum_7[b7] = i_p7_H_1 + i_p7_H_2 + i_p7_H_3 + i_p7_H_4 + i_p7_H_5
  
  b7 = b7 + 1
  
}
}
}
}
        }

p7_H_enter_B1 = c(sum(p7_H[which(n_H_sum_7 == 1)]), sum(p7_H[which(n_H_sum_7 == 2)]), 
                  sum(p7_H[which(n_H_sum_7 == 3)]), sum(p7_H[which(n_H_sum_7 == 4)]),
                  sum(p7_H[which(n_H_sum_7 == 5)]), sum(p7_H[which(n_H_sum_7 == 6)]),
                  sum(p7_H[which(n_H_sum_7 == 7)]), sum(p7_H[which(n_H_sum_7 == 8)]))

###### Bonus game 2

p8_H = {}

n_H_sum_8 = {}

b8 = 1

j_H_8 = 8

for (i_p8_H_1 in 0 : w_H[j_H_8, 1])
  for (i_p8_H_2 in 0 : w_H[j_H_8, 2])
    for (i_p8_H_3 in 0 : w_H[j_H_8, 3])
      for (i_p8_H_4 in 0 : w_H[j_H_8, 4])
        for (i_p8_H_5 in 0 : w_H[j_H_8, 5])
        {
{
{
{
{
  p8_H [b8] = choose(w_H[j_H_8, 1], i_p8_H_1)*choose(N_H[1]-w_H[j_H_8, 1], 3 - i_p8_H_1)/choose(N_H[1], 3)*
    choose(w_H[j_H_8, 2], i_p8_H_2)*choose(N_H[2]-w_H[j_H_8, 2], 4 - i_p8_H_2)/choose(N_H[2], 4)*
    choose(w_H[j_H_8, 3], i_p8_H_3)*choose(N_H[3]-w_H[j_H_8, 3], 5 - i_p8_H_3)/choose(N_H[3], 5)*
    choose(w_H[j_H_8, 4], i_p8_H_4)*choose(N_H[4]-w_H[j_H_8, 4], 4 - i_p8_H_4)/choose(N_H[4], 4)*
    choose(w_H[j_H_8, 5], i_p8_H_5)*choose(N_H[5]-w_H[j_H_8, 5], 3 - i_p8_H_5)/choose(N_H[5], 3)
  
  n_H_sum_8[b8] = i_p8_H_1 + i_p8_H_2 + i_p8_H_3 + i_p8_H_4 + i_p8_H_5
  
  b8 = b8 + 1
  
}
}
}
}
        }

p8_H_enter_B2 = c(sum(p8_H[which(n_H_sum_8 == 3)]), sum(p8_H[which(n_H_sum_8 == 4)]), 
                  sum(p8_H[which(n_H_sum_8 == 5)]), sum(p8_H[which(n_H_sum_8 == 6)]),
                  sum(p8_H[which(n_H_sum_8 == 7)]), sum(p8_H[which(n_H_sum_8 == 8)]), 
                  sum(p8_H[which(n_H_sum_8 == 9)]), sum(p8_H[which(n_H_sum_8 == 10)]))

##### Bonus game 3

p9_H = {}

n_H_sum_9 = {}

b9 = 1

j_H_9 = 9

for (i_p9_H_1 in 0 : w_H[j_H_9, 1])
  for (i_p9_H_2 in 0 : w_H[j_H_9, 2])
    for (i_p9_H_3 in 0 : w_H[j_H_9, 3])
      for (i_p9_H_4 in 0 : w_H[j_H_9, 4])
        for (i_p9_H_5 in 0 : w_H[j_H_9, 5])
        {
{
{
{
{
  p9_H [b9] = choose(w_H[j_H_9, 1], i_p9_H_1)*choose(N_H[1]-w_H[j_H_9, 1], 3 - i_p9_H_1)/choose(N_H[1], 3)*
    choose(w_H[j_H_9, 2], i_p9_H_2)*choose(N_H[2]-w_H[j_H_9, 2], 4 - i_p9_H_2)/choose(N_H[2], 4)*
    choose(w_H[j_H_9, 3], i_p9_H_3)*choose(N_H[3]-w_H[j_H_9, 3], 5 - i_p9_H_3)/choose(N_H[3], 5)*
    choose(w_H[j_H_9, 4], i_p9_H_4)*choose(N_H[4]-w_H[j_H_9, 4], 4 - i_p9_H_4)/choose(N_H[4], 4)*
    choose(w_H[j_H_9, 5], i_p9_H_5)*choose(N_H[5]-w_H[j_H_9, 5], 3 - i_p9_H_5)/choose(N_H[5], 3)
  
  n_H_sum_9[b9] = i_p9_H_1 + i_p9_H_2 + i_p9_H_3 + i_p9_H_4 + i_p9_H_5
  
  b9 = b9 + 1
  
}
}
}
}
        }

p9_H_enter_B3 = c(sum(p9_H[which(n_H_sum_9 == 4)]), sum(p9_H[which(n_H_sum_9 == 5)]), 
                  sum(p9_H[which(n_H_sum_9 == 6)]), sum(p9_H[which(n_H_sum_9 == 7)]), 
                  sum(p9_H[which(n_H_sum_9 == 8)]), sum(p9_H[which(n_H_sum_9 == 9)]), 
                  sum(p9_H[which(n_H_sum_9 == 10)]), sum(p9_H[which(n_H_sum_9 == 11)]))

##############################################

#### 賠率圖案在 bonus game 3 之機率

#### 圖案一

p1_B3 = {}

n_B3_sum_1 = {}

c1 = 1

j_B3_1 = 1

for (i_p1_B3_1 in 0 : w_B3[j_B3_1, 1]+w_B3[10, 1])
  for (i_p1_B3_2 in 0 : w_B3[j_B3_1, 2]+w_B3[10, 2])
    for (i_p1_B3_3 in 0 : w_B3[j_B3_1, 3]+w_B3[10, 3])
      for (i_p1_B3_4 in 0 : w_B3[j_B3_1, 4]+w_B3[10, 4])
        for (i_p1_B3_5 in 0 : w_B3[j_B3_1, 5]+w_B3[10, 5])
        {
{
{
{
{
  p1_B3[c1] = choose(w_B3[j_B3_1, 1]+w_B3[10, 1], i_p1_B3_1)*choose(N_B3[1]-w_B3[j_B3_1, 1]-w_B3[10, 1], 3 - i_p1_B3_1)/choose(N_B3[1], 3)*
    choose(w_B3[j_B3_1, 2]+w_B3[10, 2], i_p1_B3_2)*choose(N_B3[2]-w_B3[j_B3_1, 2]-w_B3[10, 2], 4 - i_p1_B3_2)/choose(N_B3[2], 4)*
    choose(w_B3[j_B3_1, 3]+w_B3[10, 3], i_p1_B3_3)*choose(N_B3[3]-w_B3[j_B3_1, 3]-w_B3[10, 3], 5 - i_p1_B3_3)/choose(N_B3[3], 5)*
    choose(w_B3[j_B3_1, 4]+w_B3[10, 4], i_p1_B3_4)*choose(N_B3[4]-w_B3[j_B3_1, 4]-w_B3[10, 4], 4 - i_p1_B3_4)/choose(N_B3[4], 4)*
    choose(w_B3[j_B3_1, 5]+w_B3[10, 5], i_p1_B3_5)*choose(N_B3[5]-w_B3[j_B3_1, 5]-w_B3[10, 5], 3 - i_p1_B3_5)/choose(N_B3[5], 3)
  
  n_B3_sum_1[c1] = i_p1_B3_1 + i_p1_B3_2 + i_p1_B3_3 + i_p1_B3_4 + i_p1_B3_5
  
  c1 = c1 + 1
  
}
}
}
}
        }

p1_B3_pay = c(sum(p1_B3[which(n_B3_sum_1 == 4)]), sum(p1_B3[which(n_B3_sum_1 == 5)]), 
              sum(p1_B3[which(n_B3_sum_1 == 6)]), sum(p1_B3[which(n_B3_sum_1 == 7)]), sum(p1_B3[which(n_B3_sum_1 == 8)]),
              sum(p1_B3[which(n_B3_sum_1 == 9)]), sum(p1_B3[which(n_B3_sum_1 == 10)]), sum(p1_B3[which(n_B3_sum_1 == 11)]), 
              sum(p1_B3[which(n_B3_sum_1 == 12)]))

###### 圖案二

p2_B3 = {}

n_B3_sum_2 = {}

c2 = 1

j_B3_2 = 2

for (i_p2_B3_1 in 0 : w_B3[j_B3_2, 1]+w_B3[10, 1])
  for (i_p2_B3_2 in 0 : w_B3[j_B3_2, 2]+w_B3[10, 2])
    for (i_p2_B3_3 in 0 : w_B3[j_B3_2, 3]+w_B3[10, 3])
      for (i_p2_B3_4 in 0 : w_B3[j_B3_2, 4]+w_B3[10, 4])
        for (i_p2_B3_5 in 0 : w_B3[j_B3_2, 5]+w_B3[10, 5])
        {
{
{
{
{
  p2_B3[c2] = choose(w_B3[j_B3_2, 1]+w_B3[10, 1], i_p2_B3_1)*choose(N_B3[1]-w_B3[j_B3_2, 1]-w_B3[10, 1], 3 - i_p2_B3_1)/choose(N_B3[1], 3)*
    choose(w_B3[j_B3_2, 2]+w_B3[10, 2], i_p2_B3_2)*choose(N_B3[2]-w_B3[j_B3_2, 2]-w_B3[10, 2], 4 - i_p2_B3_2)/choose(N_B3[2], 4)*
    choose(w_B3[j_B3_2, 3]+w_B3[10, 3], i_p2_B3_3)*choose(N_B3[3]-w_B3[j_B3_2, 3]-w_B3[10, 3], 5 - i_p2_B3_3)/choose(N_B3[3], 5)*
    choose(w_B3[j_B3_2, 4]+w_B3[10, 4], i_p2_B3_4)*choose(N_B3[4]-w_B3[j_B3_2, 4]-w_B3[10, 4], 4 - i_p2_B3_4)/choose(N_B3[4], 4)*
    choose(w_B3[j_B3_2, 5]+w_B3[10, 5], i_p2_B3_5)*choose(N_B3[5]-w_B3[j_B3_2, 5]-w_B3[10, 5], 3 - i_p2_B3_5)/choose(N_B3[5], 3)
  
  n_B3_sum_2[c2] = i_p2_B3_1 + i_p2_B3_2 + i_p2_B3_3 + i_p2_B3_4 + i_p2_B3_5
  
  c2 = c2 + 1
  
}
}
}
}
        }

p2_B3_pay = c(sum(p2_B3[which(n_B3_sum_2 == 4)]), sum(p2_B3[which(n_B3_sum_2 == 5)]), 
              sum(p2_B3[which(n_B3_sum_2 == 6)]), sum(p2_B3[which(n_B3_sum_2 == 7)]), sum(p2_B3[which(n_B3_sum_2 == 8)]),
              sum(p2_B3[which(n_B3_sum_2 == 9)]), sum(p2_B3[which(n_B3_sum_2 == 10)]),sum(p2_B3[which(n_B3_sum_2 == 11)]), 
              sum(p2_B3[which(n_B3_sum_2 == 12)]))

######## 圖案三

p3_B3 = {}

n_B3_sum_3 = {}

c3 = 1

j_B3_3 = 3

for (i_p3_B3_1 in 0 : w_B3[j_B3_3, 1]+w_B3[10, 1])
  for (i_p3_B3_2 in 0 : w_B3[j_B3_3, 2]+w_B3[10, 2])
    for (i_p3_B3_3 in 0 : w_B3[j_B3_3, 3]+w_B3[10, 3])
      for (i_p3_B3_4 in 0 : w_B3[j_B3_3, 4]+w_B3[10, 4])
        for (i_p3_B3_5 in 0 : w_B3[j_B3_3, 5]+w_B3[10, 5])
        {
{
{
{
{
  p3_B3[c3] = choose(w_B3[j_B3_3, 1]+w_B3[10, 1], i_p3_B3_1)*choose(N_B3[1]-w_B3[j_B3_3, 1]-w_B3[10, 1], 3 - i_p3_B3_1)/choose(N_B3[1], 3)*
    choose(w_B3[j_B3_3, 2]+w_B3[10, 2], i_p3_B3_2)*choose(N_B3[2]-w_B3[j_B3_3, 2]-w_B3[10, 2], 4 - i_p3_B3_2)/choose(N_B3[2], 4)*
    choose(w_B3[j_B3_3, 3]+w_B3[10, 3], i_p3_B3_3)*choose(N_B3[3]-w_B3[j_B3_3, 3]-w_B3[10, 3], 5 - i_p3_B3_3)/choose(N_B3[3], 5)*
    choose(w_B3[j_B3_3, 4]+w_B3[10, 4], i_p3_B3_4)*choose(N_B3[4]-w_B3[j_B3_3, 4]-w_B3[10, 4], 4 - i_p3_B3_4)/choose(N_B3[4], 4)*
    choose(w_B3[j_B3_3, 5]+w_B3[10, 5], i_p3_B3_5)*choose(N_B3[5]-w_B3[j_B3_3, 5]-w_B3[10, 5], 3 - i_p3_B3_5)/choose(N_B3[5], 3)
  
  n_B3_sum_3[c3] = i_p3_B3_1 + i_p3_B3_2 + i_p3_B3_3 + i_p3_B3_4 + i_p3_B3_5
  
  c3 = c3 + 1
  
}
}
}
}
        }

p3_B3_pay = c(sum(p3_B3[which(n_B3_sum_3 == 4)]), sum(p3_B3[which(n_B3_sum_3 == 5)]),
              sum(p3_B3[which(n_B3_sum_3 == 6)]), sum(p3_B3[which(n_B3_sum_3 == 7)]), sum(p3_B3[which(n_B3_sum_3 == 8)]),
              sum(p3_B3[which(n_B3_sum_3 == 9)]), sum(p3_B3[which(n_B3_sum_3 == 10)]), sum(p3_B3[which(n_B3_sum_3 == 11)]), 
              sum(p3_B3[which(n_B3_sum_3 == 12)]))

###### 圖案四

p4_B3 = {}

n_B3_sum_4 = {}

c4 = 1

j_B3_4 = 4

for (i_p4_B3_1 in 0 : w_B3[j_B3_4, 1]+w_B3[10, 1])
  for (i_p4_B3_2 in 0 : w_B3[j_B3_4, 2]+w_B3[10, 2])
    for (i_p4_B3_3 in 0 : w_B3[j_B3_4, 3]+w_B3[10, 3])
      for (i_p4_B3_4 in 0 : w_B3[j_B3_4, 4]+w_B3[10, 4])
        for (i_p4_B3_5 in 0 : w_B3[j_B3_4, 5]+w_B3[10, 5])
        {
{
{
{
{
  p4_B3[c4] = choose(w_B3[j_B3_4, 1]+w_B3[10, 1], i_p4_B3_1)*choose(N_B3[1]-w_B3[j_B3_4, 1]-w_B3[10, 1], 3 - i_p4_B3_1)/choose(N_B3[1], 3)*
    choose(w_B3[j_B3_4, 2]+w_B3[10, 2], i_p4_B3_2)*choose(N_B3[2]-w_B3[j_B3_4, 2]-w_B3[10, 2], 4 - i_p4_B3_2)/choose(N_B3[2], 4)*
    choose(w_B3[j_B3_4, 3]+w_B3[10, 3], i_p4_B3_3)*choose(N_B3[3]-w_B3[j_B3_4, 3]-w_B3[10, 3], 5 - i_p4_B3_3)/choose(N_B3[3], 5)*
    choose(w_B3[j_B3_4, 4]+w_B3[10, 4], i_p4_B3_4)*choose(N_B3[4]-w_B3[j_B3_4, 4]-w_B3[10, 4], 4 - i_p4_B3_4)/choose(N_B3[4], 4)*
    choose(w_B3[j_B3_4, 5]+w_B3[10, 5], i_p4_B3_5)*choose(N_B3[5]-w_B3[j_B3_4, 5]-w_B3[10, 5], 3 - i_p4_B3_5)/choose(N_B3[5], 3)
  
  n_B3_sum_4[c4] = i_p4_B3_1 + i_p4_B3_2 + i_p4_B3_3 + i_p4_B3_4 + i_p4_B3_5
  
  c4 = c4 + 1
  
}
}
}
}
        }

p4_B3_pay = c(sum(p4_B3[which(n_B3_sum_4 == 4)]), sum(p4_B3[which(n_B3_sum_4 == 5)]), 
              sum(p4_B3[which(n_B3_sum_4 == 6)]), sum(p4_B3[which(n_B3_sum_4 == 7)]), sum(p4_B3[which(n_B3_sum_4 == 8)]),
              sum(p4_B3[which(n_B3_sum_4 == 9)]), sum(p4_B3[which(n_B3_sum_4 == 10)]), sum(p4_B3[which(n_B3_sum_4 == 11)]), 
              sum(p4_B3[which(n_B3_sum_4 == 12)]))

##### 圖案五

p5_B3 = {}

n_B3_sum_5 = {}

c5 = 1

j_B3_5 = 5

for (i_p5_B3_1 in 0 : w_B3[j_B3_5, 1]+w_B3[10, 1])
  for (i_p5_B3_2 in 0 : w_B3[j_B3_5, 2]+w_B3[10, 2])
    for (i_p5_B3_3 in 0 : w_B3[j_B3_5, 3]+w_B3[10, 3])
      for (i_p5_B3_4 in 0 : w_B3[j_B3_5, 4]+w_B3[10, 4])
        for (i_p5_B3_5 in 0 : w_B3[j_B3_5, 5]+w_B3[10, 5])
        {
{
{
{
{
  p5_B3[c5] = choose(w_B3[j_B3_5, 1]+w_B3[10, 1], i_p5_B3_1)*choose(N_B3[1]-w_B3[j_B3_5, 1]-w_B3[10, 1], 3 - i_p5_B3_1)/choose(N_B3[1], 3)*
    choose(w_B3[j_B3_5, 2]+w_B3[10, 2], i_p5_B3_2)*choose(N_B3[2]-w_B3[j_B3_5, 2]-w_B3[10, 2], 4 - i_p5_B3_2)/choose(N_B3[2], 4)*
    choose(w_B3[j_B3_5, 3]+w_B3[10, 3], i_p5_B3_3)*choose(N_B3[3]-w_B3[j_B3_5, 3]-w_B3[10, 3], 5 - i_p5_B3_3)/choose(N_B3[3], 5)*
    choose(w_B3[j_B3_5, 4]+w_B3[10, 4], i_p5_B3_4)*choose(N_B3[4]-w_B3[j_B3_5, 4]-w_B3[10, 4], 4 - i_p5_B3_4)/choose(N_B3[4], 4)*
    choose(w_B3[j_B3_5, 5]+w_B3[10, 5], i_p5_B3_5)*choose(N_B3[5]-w_B3[j_B3_5, 5]-w_B3[10, 5], 3 - i_p5_B3_5)/choose(N_B3[5], 3)
  
  n_B3_sum_5[c5] = i_p5_B3_1 + i_p5_B3_2 + i_p5_B3_3 + i_p5_B3_4 + i_p5_B3_5
  
  c5 = c5 + 1
  
}
}
}
}
        }

p5_B3_pay = c(sum(p5_B3[which(n_B3_sum_5 == 4)]), sum(p5_B3[which(n_B3_sum_5 == 5)]), 
              sum(p5_B3[which(n_B3_sum_5 == 6)]), sum(p5_B3[which(n_B3_sum_5 == 7)]), sum(p5_B3[which(n_B3_sum_5 == 8)]),
              sum(p5_B3[which(n_B3_sum_5 == 9)]), sum(p5_B3[which(n_B3_sum_5 == 10)]),sum(p5_B3[which(n_B3_sum_5 == 11)]), 
              sum(p5_B3[which(n_B3_sum_5 == 12)]))

p_B3_pay = rbind(p1_B3_pay, p2_B3_pay, p3_B3_pay, p4_B3_pay, p5_B3_pay)

######### 賠率圖案在 free spin之機率

##### 圖案一

p1_FS = {}

n_FS_sum_1 = {}

d1 = 1

j_FS_1 = 1

for (i_p1_FS_1 in 0 : w_FS[j_FS_1, 1]+w_FS[10, 1])
  for (i_p1_FS_2 in 0 : w_FS[j_FS_1, 2]+w_FS[10, 2])
    for (i_p1_FS_3 in 0 : w_FS[j_FS_1, 3]+w_FS[10, 3])
      for (i_p1_FS_4 in 0 : w_FS[j_FS_1, 4]+w_FS[10, 4])
        for (i_p1_FS_5 in 0 : w_FS[j_FS_1, 5]+w_FS[10, 5])
        {
{
{
{
{
  p1_FS[d1] = choose(w_FS[j_FS_1, 1]+w_FS[10, 1], i_p1_FS_1)*choose(N_FS[1]-w_FS[j_FS_1, 1]-w_FS[10, 1], 3 - i_p1_FS_1)/choose(N_FS[1], 3)*
    choose(w_FS[j_FS_1, 2]+w_FS[10, 2], i_p1_FS_2)*choose(N_FS[2]-w_FS[j_FS_1, 2]-w_FS[10, 2], 4 - i_p1_FS_2)/choose(N_FS[2], 4)*
    choose(w_FS[j_FS_1, 3]+w_FS[10, 3], i_p1_FS_3)*choose(N_FS[3]-w_FS[j_FS_1, 3]-w_FS[10, 3], 5 - i_p1_FS_3)/choose(N_FS[3], 5)*
    choose(w_FS[j_FS_1, 4]+w_FS[10, 4], i_p1_FS_4)*choose(N_FS[4]-w_FS[j_FS_1, 4]-w_FS[10, 4], 4 - i_p1_FS_4)/choose(N_FS[4], 4)*
    choose(w_FS[j_FS_1, 5]+w_FS[10, 5], i_p1_FS_5)*choose(N_FS[5]-w_FS[j_FS_1, 5]-w_FS[10, 5], 3 - i_p1_FS_5)/choose(N_FS[5], 3)
  
  n_FS_sum_1[d1] = i_p1_FS_1 + i_p1_FS_2 + i_p1_FS_3 + i_p1_FS_4 + i_p1_FS_5
  
  d1 = d1 + 1
  
}
}
}
}
        }

p1_FS_pay = c(sum(p1_FS[which(n_FS_sum_1 == 4)]), sum(p1_FS[which(n_FS_sum_1 == 5)]), 
              sum(p1_FS[which(n_FS_sum_1 == 6)]), sum(p1_FS[which(n_FS_sum_1 == 7)]), sum(p1_FS[which(n_FS_sum_1 == 8)]),
              sum(p1_FS[which(n_FS_sum_1 == 9)]), sum(p1_FS[which(n_FS_sum_1 == 10)]), sum(p1_FS[which(n_FS_sum_1 == 11)]), 
              sum(p1_FS[which(n_FS_sum_1 == 12)]))

###### 圖案二

p2_FS = {}

n_FS_sum_2 = {}

d2 = 1

j_FS_2 = 2

for (i_p2_FS_1 in 0 : w_FS[j_FS_2, 1]+w_FS[10, 1])
  for (i_p2_FS_2 in 0 : w_FS[j_FS_2, 2]+w_FS[10, 2])
    for (i_p2_FS_3 in 0 : w_FS[j_FS_2, 3]+w_FS[10, 3])
      for (i_p2_FS_4 in 0 : w_FS[j_FS_2, 4]+w_FS[10, 4])
        for (i_p2_FS_5 in 0 : w_FS[j_FS_2, 5]+w_FS[10, 5])
        {
{
{
{
{
  p2_FS[d2] = choose(w_FS[j_FS_2, 1]+w_FS[10, 1], i_p2_FS_1)*choose(N_FS[1]-w_FS[j_FS_2, 1]-w_FS[10, 1], 3 - i_p2_FS_1)/choose(N_FS[1], 3)*
    choose(w_FS[j_FS_2, 2]+w_FS[10, 2], i_p2_FS_2)*choose(N_FS[2]-w_FS[j_FS_2, 2]-w_FS[10, 2], 4 - i_p2_FS_2)/choose(N_FS[2], 4)*
    choose(w_FS[j_FS_2, 3]+w_FS[10, 3], i_p2_FS_3)*choose(N_FS[3]-w_FS[j_FS_2, 3]-w_FS[10, 3], 5 - i_p2_FS_3)/choose(N_FS[3], 5)*
    choose(w_FS[j_FS_2, 4]+w_FS[10, 4], i_p2_FS_4)*choose(N_FS[4]-w_FS[j_FS_2, 4]-w_FS[10, 4], 4 - i_p2_FS_4)/choose(N_FS[4], 4)*
    choose(w_FS[j_FS_2, 5]+w_FS[10, 5], i_p2_FS_5)*choose(N_FS[5]-w_FS[j_FS_2, 5]-w_FS[10, 5], 3 - i_p2_FS_5)/choose(N_FS[5], 3)
  
  n_FS_sum_2[d2] = i_p2_FS_1 + i_p2_FS_2 + i_p2_FS_3 + i_p2_FS_4 + i_p2_FS_5
  
  d2 = d2 + 1
  
}
}
}
}
        }

p2_FS_pay = c(sum(p2_FS[which(n_FS_sum_2 == 4)]), sum(p2_FS[which(n_FS_sum_2 == 5)]), 
              sum(p2_FS[which(n_FS_sum_2 == 6)]), sum(p2_FS[which(n_FS_sum_2 == 7)]), sum(p2_FS[which(n_FS_sum_2 == 8)]),
              sum(p2_FS[which(n_FS_sum_2 == 9)]), sum(p2_FS[which(n_FS_sum_2 == 10)]),sum(p2_FS[which(n_FS_sum_2 == 11)]), 
              sum(p2_FS[which(n_FS_sum_2 == 12)]))

######## 圖案三

p3_FS = {}

n_FS_sum_3 = {}

d3 = 1

j_FS_3 = 3

for (i_p3_FS_1 in 0 : w_FS[j_FS_3, 1]+w_FS[10, 1])
  for (i_p3_FS_2 in 0 : w_FS[j_FS_3, 2]+w_FS[10, 2])
    for (i_p3_FS_3 in 0 : w_FS[j_FS_3, 3]+w_FS[10, 3])
      for (i_p3_FS_4 in 0 : w_FS[j_FS_3, 4]+w_FS[10, 4])
        for (i_p3_FS_5 in 0 : w_FS[j_FS_3, 5]+w_FS[10, 5])
        {
{
{
{
{
  p3_FS[d3] = choose(w_FS[j_FS_3, 1]+w_FS[10, 1], i_p3_FS_1)*choose(N_FS[1]-w_FS[j_FS_3, 1]-w_FS[10, 1], 3 - i_p3_FS_1)/choose(N_FS[1], 3)*
    choose(w_FS[j_FS_3, 2]+w_FS[10, 2], i_p3_FS_2)*choose(N_FS[2]-w_FS[j_FS_3, 2]-w_FS[10, 2], 4 - i_p3_FS_2)/choose(N_FS[2], 4)*
    choose(w_FS[j_FS_3, 3]+w_FS[10, 3], i_p3_FS_3)*choose(N_FS[3]-w_FS[j_FS_3, 3]-w_FS[10, 3], 5 - i_p3_FS_3)/choose(N_FS[3], 5)*
    choose(w_FS[j_FS_3, 4]+w_FS[10, 4], i_p3_FS_4)*choose(N_FS[4]-w_FS[j_FS_3, 4]-w_FS[10, 4], 4 - i_p3_FS_4)/choose(N_FS[4], 4)*
    choose(w_FS[j_FS_3, 5]+w_FS[10, 5], i_p3_FS_5)*choose(N_FS[5]-w_FS[j_FS_3, 5]-w_FS[10, 5], 3 - i_p3_FS_5)/choose(N_FS[5], 3)
  
  n_FS_sum_3[d3] = i_p3_FS_1 + i_p3_FS_2 + i_p3_FS_3 + i_p3_FS_4 + i_p3_FS_5
  
  d3 = d3 + 1
  
}
}
}
}
        }

p3_FS_pay = c(sum(p3_FS[which(n_FS_sum_3 == 4)]), sum(p3_FS[which(n_FS_sum_3 == 5)]),
              sum(p3_FS[which(n_FS_sum_3 == 6)]), sum(p3_FS[which(n_FS_sum_3 == 7)]), sum(p3_FS[which(n_FS_sum_3 == 8)]),
              sum(p3_FS[which(n_FS_sum_3 == 9)]), sum(p3_FS[which(n_FS_sum_3 == 10)]), sum(p3_FS[which(n_FS_sum_3 == 11)]), 
              sum(p3_FS[which(n_FS_sum_3 == 12)]))

###### 圖案四

p4_FS = {}

n_FS_sum_4 = {}

d4 = 1

j_FS_4 = 4

for (i_p4_FS_1 in 0 : w_FS[j_FS_4, 1]+w_FS[10, 1])
  for (i_p4_FS_2 in 0 : w_FS[j_FS_4, 2]+w_FS[10, 2])
    for (i_p4_FS_3 in 0 : w_FS[j_FS_4, 3]+w_FS[10, 3])
      for (i_p4_FS_4 in 0 : w_FS[j_FS_4, 4]+w_FS[10, 4])
        for (i_p4_FS_5 in 0 : w_FS[j_FS_4, 5]+w_FS[10, 5])
        {
{
{
{
{
  p4_FS[d4] = choose(w_FS[j_FS_4, 1]+w_FS[10, 1], i_p4_FS_1)*choose(N_FS[1]-w_FS[j_FS_4, 1]-w_FS[10, 1], 3 - i_p4_FS_1)/choose(N_FS[1], 3)*
    choose(w_FS[j_FS_4, 2]+w_FS[10, 2], i_p4_FS_2)*choose(N_FS[2]-w_FS[j_FS_4, 2]-w_FS[10, 2], 4 - i_p4_FS_2)/choose(N_FS[2], 4)*
    choose(w_FS[j_FS_4, 3]+w_FS[10, 3], i_p4_FS_3)*choose(N_FS[3]-w_FS[j_FS_4, 3]-w_FS[10, 3], 5 - i_p4_FS_3)/choose(N_FS[3], 5)*
    choose(w_FS[j_FS_4, 4]+w_FS[10, 4], i_p4_FS_4)*choose(N_FS[4]-w_FS[j_FS_4, 4]-w_FS[10, 4], 4 - i_p4_FS_4)/choose(N_FS[4], 4)*
    choose(w_FS[j_FS_4, 5]+w_FS[10, 5], i_p4_FS_5)*choose(N_FS[5]-w_FS[j_FS_4, 5]-w_FS[10, 5], 3 - i_p4_FS_5)/choose(N_FS[5], 3)
  
  n_FS_sum_4[d4] = i_p4_FS_1 + i_p4_FS_2 + i_p4_FS_3 + i_p4_FS_4 + i_p4_FS_5
  
  d4 = d4 + 1
  
}
}
}
}
        }

p4_FS_pay = c(sum(p4_FS[which(n_FS_sum_4 == 4)]), sum(p4_FS[which(n_FS_sum_4 == 5)]), 
              sum(p4_FS[which(n_FS_sum_4 == 6)]), sum(p4_FS[which(n_FS_sum_4 == 7)]), sum(p4_FS[which(n_FS_sum_4 == 8)]),
              sum(p4_FS[which(n_FS_sum_4 == 9)]), sum(p4_FS[which(n_FS_sum_4 == 10)]), sum(p4_FS[which(n_FS_sum_4 == 11)]), 
              sum(p4_FS[which(n_FS_sum_4 == 12)]))

###### 圖案五

p5_FS = {}

n_FS_sum_5 = {}

d5 = 1

j_FS_5 = 5

for (i_p5_FS_1 in 0 : w_FS[j_FS_5, 1]+w_FS[10, 1])
  for (i_p5_FS_2 in 0 : w_FS[j_FS_5, 2]+w_FS[10, 2])
    for (i_p5_FS_3 in 0 : w_FS[j_FS_5, 3]+w_FS[10, 3])
      for (i_p5_FS_4 in 0 : w_FS[j_FS_5, 4]+w_FS[10, 4])
        for (i_p5_FS_5 in 0 : w_FS[j_FS_5, 5]+w_FS[10, 5])
        {
{
{
{
{
  p5_FS[d5] = choose(w_FS[j_FS_5, 1]+w_FS[10, 1], i_p5_FS_1)*choose(N_FS[1]-w_FS[j_FS_5, 1]-w_FS[10, 1], 3 - i_p5_FS_1)/choose(N_FS[1], 3)*
    choose(w_FS[j_FS_5, 2]+w_FS[10, 2], i_p5_FS_2)*choose(N_FS[2]-w_FS[j_FS_5, 2]-w_FS[10, 2], 4 - i_p5_FS_2)/choose(N_FS[2], 4)*
    choose(w_FS[j_FS_5, 3]+w_FS[10, 3], i_p5_FS_3)*choose(N_FS[3]-w_FS[j_FS_5, 3]-w_FS[10, 3], 5 - i_p5_FS_3)/choose(N_FS[3], 5)*
    choose(w_FS[j_FS_5, 4]+w_FS[10, 4], i_p5_FS_4)*choose(N_FS[4]-w_FS[j_FS_5, 4]-w_FS[10, 4], 4 - i_p5_FS_4)/choose(N_FS[4], 4)*
    choose(w_FS[j_FS_5, 5]+w_FS[10, 5], i_p5_FS_5)*choose(N_FS[5]-w_FS[j_FS_5, 5]-w_FS[10, 5], 3 - i_p5_FS_5)/choose(N_FS[5], 3)
  
  n_FS_sum_5[d5] = i_p5_FS_1 + i_p5_FS_2 + i_p5_FS_3 + i_p5_FS_4 + i_p5_FS_5
  
  d5 = d5 + 1
  
}
}
}
}
        }

p5_FS_pay = c(sum(p5_FS[which(n_FS_sum_5 == 4)]), sum(p5_FS[which(n_FS_sum_5 == 5)]), 
              sum(p5_FS[which(n_FS_sum_5 == 6)]), sum(p5_FS[which(n_FS_sum_5 == 7)]), sum(p5_FS[which(n_FS_sum_5 == 8)]),
              sum(p5_FS[which(n_FS_sum_5 == 9)]), sum(p5_FS[which(n_FS_sum_5 == 10)]),sum(p5_FS[which(n_FS_sum_5 == 11)]), 
              sum(p5_FS[which(n_FS_sum_5 == 12)]))

p_FS_pay = rbind(p1_FS_pay, p2_FS_pay, p3_FS_pay, p4_FS_pay, p5_FS_pay)

############################################

#### 高低表的期望值

E_L = p_L_pay*payoff

E_H = p_H_pay*payoff

E_L_sum = sum(E_L)

E_H_sum = sum(E_H)

#########################################

##### 高低表之存在時間比

duration = (E_general - E_H_sum)/(E_L_sum - E_general)

##### 進入Free spin & bonus games 的平均機率

##### 圖案六(巨人)需出現5個方可進入 Free spin

p6_enter_FS_average = sum(((p6_H_enter_FS + p6_L_enter_FS*duration)/(1 + duration))[5:5])

##### 圖案七(金蛋)需出現5個以上方可進入 Bonus game 1

p7_enter_B1_average = sum(((p7_H_enter_B1 + p7_L_enter_B1*duration)/(1 + duration))[5:8])

##### 圖案八(戰錘)需出現6以上個方可進入 Bonus game 2

p8_enter_B2_average = sum(((p8_H_enter_B2 + p8_L_enter_B2*duration)/(1 + duration))[4:8])

##### 圖案九(公主)需出現7以上個方可進入 Bonus game 3

p9_enter_B3_average = sum(((p9_H_enter_B3 + p9_L_enter_B3*duration)/(1 + duration))[4:8])

#####

p_enter_smallgame = c(p6_enter_FS_average, p7_enter_B1_average, p8_enter_B2_average, 
                      p9_enter_B3_average)

##### 進入Free spin & bonus games 的平均迴轉數

rp_enter_smallgame = c(1/p6_enter_FS_average, 1/p7_enter_B1_average, 1/p8_enter_B2_average, 
                       1/p9_enter_B3_average)

##### Free spin & bonus games 的平均出現時間

time_enter_samllgame = rp_enter_smallgame*gt/60

##### Bonus game 期望值 

E_b1 = 2.5 # 給予玩家投注金額之期望倍數 

E_b2 = 5   # 給予玩家投注金額之期望倍數

Q = 50    # 賠率圖案乘與 100倍賠率在bonus game 3

E_b3 = sum(Q*payoff*p_B3_pay)

##### Free spin 期望值

E_fs = sum(payoff*p_FS_pay)*fr

##### 一般遊戲期望值 + Bonus game 期望值 + Free spin 期望值

E_fs_b = c(E_fs, E_b2, E_b1, E_b3)

E_samllgame = p_enter_smallgame*E_fs_b

E_part_1 = E_general + sum(E_samllgame) ### 目標 = 0.95

#### 在 三個 bonus game 中拉下JP的機率 ####

p_b1_jp_mini = (3/9)*(2/8)*(1/7)

p_b2_jp_mega = (3/9)^5

p_b3_jp_grand = 1*choose(w_B3[8, 2], 1)*choose(N_B3[2] - w_B3[8, 2], 4 - 1)/choose(N_B3[2], 4)*1*
  choose(w_B3[8, 4], 1)*choose(N_B3[4] - w_B3[8, 4], 4 - 1)/choose(N_B3[4], 4)*1

p_b_jp = c(p_b1_jp_mini, p_b2_jp_mega, p_b3_jp_grand)

#### 三個 Jackpot 的出現機率

p_jp_mini = p_enter_smallgame[2]*p_b_jp[1]

p_jp_mega = p_enter_smallgame[3]*p_b_jp[2]

p_jp_grand = p_enter_smallgame[4]*p_b_jp[3]

#### 三個 Jackpot 的迴轉數

rp_jp_mini = 1/p_jp_mini

rp_jp_mega = 1/p_jp_mega

rp_jp_grand = 1/p_jp_grand

#########################

#### 三個 Jackpot 出現的平均時間(天數)

jp_time_jp_mini = rp_jp_mini*10/(60*60*24) 

jp_time_jp_mega = rp_jp_mega*10/(60*60*24)

jp_time_jp_grand = rp_jp_grand*10/(60*60*24)

########################

#### Jackpot 獎金

return_jp = 0.03       ## 提撥率 mini = 0.015; mega = 0.01; grand = 0.005

subs_jp_mini = 0.015   ## 提撥率 mini

subs_jp_mega = 0.01    ## 提撥率 mega

subs_jp_grand = 0.005   ## 提撥率 grand

cum_jp_mini = 0.0125   ## mini累積率

fix_mini = 0.0025      ## mini補底率

cum_jp_mega = 0.0075   ## mega累積率

fix_mega = 0.0025      ## mega補底率

cum_jp_grand = 0.004   ## grand累積率

fix_grand = 0.001      ## grand補底率

money_jp_mini = rp_jp_mini*bet*(cum_jp_mini + fix_mini)     ## JP mini獎金

money_jp_mega = rp_jp_mega*bet*(cum_jp_mega + fix_mega)     ## JP mega獎金

money_jp_grand = rp_jp_grand*bet*(cum_jp_grand + fix_grand) ## JP grand獎金

fix_money_jp_mini = rp_jp_mini*bet*fix_mini     ## JP mini補底

fix_money_jp_mega = rp_jp_mega*bet*fix_mega     ## JP mega補底

fix_money_jp_grand = rp_jp_grand*bet*fix_grand  ## JP grand補底

#### JP期望值

E_jp_mini = money_jp_mini*1/(rp_jp_mini*bet)

E_jp_mega = money_jp_mega*1/(rp_jp_mega*bet)

E_jp_grand = money_jp_grand*1/(rp_jp_grand*bet)

#### 總體遊戲期望值

E_total = E_part_1 + E_jp_mini + E_jp_mega + E_jp_grand

####### 報表輸出

library(xlsx)

LT = rbind(w_L, N_L)

colnames(LT) = c("輪軸1", "輪軸2", "輪軸3", "輪軸4", "輪軸5")

rownames(LT) = c("圖案1", "圖案2", "圖案3", "圖案4", "圖案5", "圖案6", "圖案7", "圖案8", "圖案9", "圖案0", "總計")

HT = rbind(w_H, N_H)

colnames(HT) = c("輪軸1", "輪軸2", "輪軸3", "輪軸4", "輪軸5")

rownames(HT) = c("圖案1", "圖案2", "圖案3", "圖案4", "圖案5", "圖案6", "圖案7", "圖案8", "圖案9", "圖案0", "總計")

B3 = rbind(w_B3, N_B3)

colnames(B3) = c("輪軸1", "輪軸2", "輪軸3", "輪軸4", "輪軸5")

rownames(B3) = c("圖案1", "圖案2", "圖案3", "圖案4", "圖案5", "圖案6", "圖案7", "圖案8", "圖案9", "圖案0", "總計")

FS = rbind(w_FS, N_FS)

colnames(FS) = c("輪軸1", "輪軸2", "輪軸3", "輪軸4", "輪軸5")

rownames(FS) = c("圖案1", "圖案2", "圖案3", "圖案4", "圖案5", "圖案6", "圖案7", "圖案8", "圖案9", "圖案0", "總計")

enter_bonus = rbind(p_enter_smallgame[2:4], rp_enter_smallgame[2:4], time_enter_samllgame[2:4])

colnames(enter_bonus) = c("Bonus game 1", "Bonus game 2", "Bonus game 3")

rownames(enter_bonus) = c("進入遊戲之機率", "平均出現回轉數", "平均出現時間(分鐘)")

got_jp = rbind(c(p_jp_mini, p_jp_mega, p_jp_grand), c(rp_jp_mini, rp_jp_mega, rp_jp_grand), 
               c(jp_time_jp_mini, jp_time_jp_mega, jp_time_jp_grand))

colnames(got_jp) = c("Jackpot mini", "Jackpot mega", "Jackpot grand")

rownames(got_jp) = c("拉下JP之機率", "JP平均出現回轉數", "JP平均出現時間(天)")

jp_reward = rbind(c(subs_jp_mini, subs_jp_mega, subs_jp_grand), c(cum_jp_mini, cum_jp_mega, cum_jp_grand), 
                  c(fix_mini, fix_mega, fix_grand), c(fix_money_jp_mini, fix_money_jp_mega, fix_money_jp_grand), 
                  c(money_jp_mini, money_jp_mega, money_jp_grand))

colnames(jp_reward) = c("Jackpot mini", "Jackpot mega", "Jackpot grand")

rownames(jp_reward) = c("JP提撥率", "JP累積率", "JP補底率", "JP補底金額", "JP中獎金額")

expectation = as.matrix(c(E_L_sum, E_H_sum, duration, E_general, E_samllgame, E_jp_mini, E_jp_mega, E_jp_grand, E_total), 12, 1)

dimnames(expectation) = list(c("高表期望值", "低表期望值'", "高低表之存在時間比", "一般遊戲總期望值", "Free spin期望值", "Bonus game 1 期望值", 
                               "Bonus game 2 期望值", "Bonus game 3 期望值", "JP mini 期望值", "JP mega 期望值", "JP grand 期望值", 
                               "總計整體遊戲期望值"), c("遊戲期望值"))

setting = matrix(c(gt, bet, fr, E_general, E_b1, E_b2, Q), 1, 7)

colnames(setting) = c("平均遊戲時間 (second)", "玩家下注金額", "Free spin 送轉數", "一般遊戲之期望值", 
                      "在Bonus game 1 給予玩家投注金額之期望倍數(期望金額除以玩家下注金額)", "在Bonus game =2 給予玩家投注金額之期望倍", 
                      "在Bonus game 3 賠率圖案乘與倍數之賠率")

rownames(setting, do.NULL = T) 

colnames(payoff) = c("出現4個", "出現5個", "出現6個", "出現7個", "出現8個", "出現9個", "出現10個", "出現11個", "出現12個")

rownames(payoff) = c("圖案1", "圖案2", "圖案3", "圖案4", "圖案5")

Jackmodel <- createWorkbook()

Jack0 <- createSheet(wb = Jackmodel, sheetName="賠率表")

Jack1 <- createSheet(wb = Jackmodel, sheetName="遊戲設定")

Jack2 <- createSheet(wb = Jackmodel, sheetName="遊戲低表個數")

Jack3 <- createSheet(wb = Jackmodel, sheetName="遊戲高表個數")

Jack4 <- createSheet(wb = Jackmodel, sheetName="遊戲Bonusgame3個數")

Jack5 <- createSheet(wb = Jackmodel, sheetName="遊戲Freespin個數")

Jack6 <- createSheet(wb = Jackmodel, sheetName="進入bonus game之概況")

Jack7 <- createSheet(wb = Jackmodel, sheetName="拉下JP之概況")

Jack8 <- createSheet(wb = Jackmodel, sheetName="JP金額之概況")

Jack9 <- createSheet(wb = Jackmodel, sheetName="遊戲總計期望值")

addDataFrame(x = payoff, sheet = Jack0)

addDataFrame(x = setting, sheet = Jack1)

addDataFrame(x = LT, sheet = Jack2)

addDataFrame(x = HT, sheet = Jack3)

addDataFrame(x = B3, sheet = Jack4)

addDataFrame(x = FS, sheet = Jack5)

addDataFrame(x = enter_bonus, sheet = Jack6)

addDataFrame(x = got_jp, sheet = Jack7)

addDataFrame(x = jp_reward, sheet = Jack8)

addDataFrame(x = expectation, sheet = Jack9)


saveWorkbook(Jackmodel, "傑克與魔豆數值模型建立表_temp.xlsx")
