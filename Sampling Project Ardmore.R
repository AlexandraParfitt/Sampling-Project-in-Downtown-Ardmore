#Alexandra Parfitt
#Coding for Sampling Project
#Randomly selecting 20 blocks to sample
sort(sample(2:72, 20))# 2 3 10 14 19 29 30 32 36 38 39 41 45 48 51 60 61 63 67 72
#Calculating the Results using Ratio calculation with the number of businesses as my x variable
tx<-c(1, 0, 12, 26, 1, 0, 2, 4, 3, 0, 7, 5, 2, 1, 0, 1, 1, 4, 1, 3)
mean(tx)
sum(tx)

#Estimating the total number of businesses in Ardmore
tot_all_businesses<-71*mean(tx)#262.7
s_sq_all<-((sum(tx^2))-((3.7^2)/20))/19
se_all<-sqrt((71^2)*(1-(20/71))*(s_sq_all/20))#95.51026
CI_all=tot_all_businesses+1.96*c(-se_all, se_all)

#Estimating the variance of the mean number of businesses in Ardmore

#se2 calculations
#food
yi_food <-c(1, 0, 4, 4, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0)
se2_food<-sum((yi_food-(12/74)*tx)^2)/19
#retail=
yi_retail<-c(0,0,2,4,0,0,0,0,0,0,1,0,0,1,0,0,0,1,0,0)
se2_retail<-sum((yi_retail-(9/74)*tx)^2)/19
#personal service
yi_pers<-c(0, 0, 4, 1, 0, 0, 0, 0, 1, 0, 3, 0, 0, 0, 0, 0, 0, 2, 0, 1)
se2_pers<-sum((yi_pers-(12/74)*tx)^2)/19
#professional services
yi_prof<-c(0, 0, 1, 4, 0, 0, 0, 2, 1, 0, 2, 3, 0, 0, 0, 0, 0, 1, 0, 0)
se2_prof<-sum((yi_pers-(14/74)*tx)^2)/19
#healthcare
yi_hc<-c(0,0,0, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
se2_hc<-sum((yi_hc-(12/74)*tx)^2)/19
#other
yi_other<-c(0, 0, 1, 1, 1, 0, 2, 2, 0, 0, 1, 1, 2, 0, 0, 0, 1, 0, 1, 2)
se2_other<-sum((yi_other-(15/74)*tx)^2)/19

#Standard Error and CI Calculations based on formula 4.10 with the number of businesses in a block as x, tx=74
#Food businesses
#proportion
prop_food=sum(yi_food)/sum(tx)#0.1621622
SE_prop_food=sqrt((51/71)*(se2_food)/(20*3.7*3.7))# 0.03456288
CI_prop_food=prop_food+1.96*c(-SE_prop_food, SE_prop_food)#0.09441893 0.22990540
#total
tot_food=71*prop_food#11.51351
SE_tot_food=71*sqrt((51/71)*(se2_food/20))
CI_tot_food=tot_food+1.96*c(-SE_tot_food, SE_tot_food)

#retail
#proportion
prop_retail=sum(yi_retail)/sum(tx)
SE_prop_retail=sqrt((51/71)*(se2_retail)/(20*3.7*3.7))#0.02074419
CI_prop_retail=prop_retail+1.96*c(-SE_prop_retail, SE_prop_retail)# 0.0809630 0.1622802
#total
tot_retail=71*prop_retail# 8.635135
SE_tot_retail=71*sqrt((51/71)*(se2_retail/20))
CI_tot_retail=tot_retail+1.96*c(-SE_tot_retail, SE_tot_retail)

#personal services
prop_pers=sum(yi_pers)/sum(tx)#0.1621622
SE_prop_pers=sqrt((51/71)*(se2_pers)/(20*3.7*3.7))#0.05491567
CI_prop_pers=prop_pers+1.96*c(-SE_prop_pers, SE_prop_pers)# 0.05452746 0.26979687
#total
tot_pers=71*prop_pers
SE_tot_pers=71*sqrt((51/71)*(se2_pers/20))
CI_tot_pers=tot_pers+1.96*c(-SE_tot_pers, SE_tot_pers)#-16.76212  39.78915

#professional services
prop_prof=sum(yi_prof)/sum(tx)#0.1891892
SE_prop_prof=sqrt((51/71)*(se2_prof)/(20*3.7*3.7))#0.05880737
CI_prop_prof=prop_prof+1.96*c(-SE_prop_prof, SE_prop_prof)# 0.07392675 0.30445163
#total
tot_prof=71*prop_prof#13.43243
SE_tot_prof=71*sqrt((51/71)*(se2_prof/20))
CI_tot_prof=tot_prof+1.96*c(-SE_tot_prof, SE_tot_prof)#-16.84701  43.71188
#health care
prop_hc=sum(yi_hc)/sum(tx)#0.1621622
SE_prop_hc=sqrt((51/71)*(se2_hc)/(20*3.7*3.7))#0.09690054
CI_prop_hc=prop_hc+1.96*c(-SE_prop_hc, SE_prop_hc)#-0.02776289  0.35208721
#total
tot_hc=71*prop_hc#11.51351
SE_tot_hc=71*sqrt((51/71)*(se2_hc/20))#25.45577
CI_tot_hc=tot_hc+1.96*c(-SE_tot_hc, SE_tot_hc)

#other
prop_other=sum(yi_other)/sum(tx)# 0.2027027
SE_prop_other=sqrt((51/71)*(se2_other)/(20*3.7*3.7))
CI_prop_other=prop_prof+1.96*c(-SE_prop_other, SE_prop_other)# 0.0589915 0.3193869
#total
tot_other=71*prop_other#14.39189
SE_tot_other=71*sqrt((51/71)*(se2_other/20))#17.45048
CI_tot_other=tot_other+1.96*c(-SE_tot_other, SE_tot_other)#-19.81104  48.59483
#Chi-Square Test
all_ti<-c(12, 9, 12, 14, 12, 15)
chisq.test(all_ti)#p-value is 0.8851...can't tell if there is a difference between the groups...

#Calculating Design Effect# 
all<-c(yi_food, yi_retail, yi_pers, yi_prof, yi_other)
mean(all)#0.62
E_x_squared<-((mean(yi_food))^2+var(yi_food)+(mean(yi_retail))^2+var(yi_retail)+(mean(yi_pers))^2+var(yi_pers)+(mean(yi_prof))^2+var(yi_prof)+(mean(yi_other))^2+var(yi_other))/5#1.558
V_mean_SRS<-E_x_squared-(mean(all)^2)
V_t_SRS<-(71^2)*V_mean_SRS#by formula 2.16, 5916
V_t_cluster<-se_all^2#9122.2
design_effect<-V_t_cluster/V_t_SRS

#Using bootstrapping to calculate the number of bars in the first categorization scheme
#treating bars the same
x<-c(2/12, 1/5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
B=10000 
store1=double(B) #For bootstrap sample means.
store2=double(B) #For bootstrap sample medians.
for (run in 1:B){
  samp=sample(x,length(x),replace=T)
  store1[run]=mean(samp)
  store2[run]=median(samp)}

hist(store1)
sd(store1)
mean(store1)

#treating bars as restaurants
x<-c(0, 4/12, 4/26, 0, 1/3, 1/5, 1/1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
B=10000 
store1=double(B) #For bootstrap sample means.
store2=double(B) #For bootstrap sample medians.
for (run in 1:B){
  samp=sample(x,length(x),replace=T)
  store1[run]=mean(samp)
  store2[run]=median(samp)}

hist(store1)
sd(store1)
mean(store1)