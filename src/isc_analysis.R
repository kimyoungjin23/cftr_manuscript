
ussing<-read_csv("C:/Users/kimyo/Dropbox/CSHL/r/cftr_manuscript/data/16hbe/ussing_chamber/isc_measurements.csv")
tx<-unique(ussing$tx)
exp<-unique(ussing$exp)[order(unique(ussing$exp))]
ussing$tx<-factor(ussing$tx,levels = tx)
ussing$exp<-factor(ussing$exp, levels = exp)

# ==================
# calculate mean and sd of Isc and save in `summary_isc`
summary_isc<-ussing %>% group_by(time_min, tx,exp) %>% summarise(mean(isc_uA_per_cm2, na.rm=TRUE),sd(isc_uA_per_cm2, na.rm=TRUE))
colnames(summary_isc) = c("time_min", "tx", "exp", "mean_isc", "sd_isc")
# ==================

#    ***********

# ==================
# calculate area under the curve

source("C:/Users/kimyo/Dropbox/CSHL/r/func/auc_ussing_chamber.R")
exp_range<-filter(ussing, time_min>20 & time_min<40 ) %>% group_by(tx, replicate)

# sort by experiments and select time range
e<-exp_range %>%group_by(exp, tx, replicate) %>% nest() %>% arrange(exp)

e1<-filter(exp_range, exp == "exp19-27") %>% nest()
e2<-filter(exp_range, exp == "exp19-35") %>% nest() 
e3<-filter(exp_range, exp == "exp19-38") %>% nest() 
e4<-filter(ussing, c(time_min>20 & time_min<50) ) %>% group_by(tx, replicate) %>% filter(exp == "exp19-40") %>% nest() 

# define auc data
auc_exp19_27<-data.frame(exp="exp19-27",tx = rep(unique(e1$tx), each=3), total_current = rep(0,6))
auc_exp19_35<-data.frame(exp="exp19-35",tx = rep(unique(e2$tx), each=3), total_current = rep(0,6))
auc_exp19_38<-data.frame(exp="exp19-38",tx = rep(unique(e3$tx), each=3), total_current = rep(0,6))
auc_exp19_40<-data.frame(exp="exp19-40",tx = rep(unique(e4$tx), each=3), total_current = rep(0,6))

# calculate auc of each replicate
for (n in 1:6){
  auc_exp19_27[n,3]<-desc_integrate(e1[,3][[1]][[n]]$time_min, e1[,3][[1]][[n]]$isc_uA_per_cm2)
}

for (n in 1:6){
  auc_exp19_35[n,3]<-desc_integrate(e2[,3][[1]][[n]]$time_min, e2[,3][[1]][[n]]$isc_uA_per_cm2)
}


for (n in 1:6){
  auc_exp19_38[n,3]<-desc_integrate(e3[,3][[1]][[n]]$time_min, e3[,3][[1]][[n]]$isc_uA_per_cm2)
}


for (n in 1:6){
  auc_exp19_40[n,3]<-desc_integrate(e4[,3][[1]][[n]]$time_min, e4[,3][[1]][[n]]$isc_uA_per_cm2)
}

auc<-rbind(auc_exp19_27, auc_exp19_35, auc_exp19_38, auc_exp19_40)
# ==================