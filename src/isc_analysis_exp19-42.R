# ==================================
# ---clean up the raw Isc reading---

c<-read_csv("C:/Users/kimyo/Dropbox/CSHL/r/cftr_manuscript/data/16hbe/ussing_chamber/Isc_exp19-42_(unsure)_raw.csv", col_names = FALSE)

time<-unlist(c[,1])%>% as.numeric()/60
time<-rep(time, 6)
base<-unlist(c[490,2:7]) %>% as.numeric() %>% matrix(nrow=1)

# function to subtract baseline values
baseline<-function(data){
  data - base
}   
# subtract isc baseline
isc<-apply(c[,2:7],1,baseline) %>% matrix(654,6, byrow=TRUE) %>% matrix(654*6,1)

# fill in the rep, exp, tx information
reps<-rep(c("r1","r2","r3","r1","r2","r3"),each=654)
exp<-rep("exp19-42(ns)", 654*6)
tx<-rep(c("ctrl_20uM_vx809_vx770_g418_200uM", "ejc24-25-26-2_20uM_vx809_vx770_g418_200uM"), each= 654*3)

# write the new cleaned up csv file
ussing_1942<-tibble(exp,tx,reps,time,`isc`=isc)
write.csv(tibble(exp,tx,reps,time,isc), "C:/Users/kimyo/Dropbox/CSHL/r/cftr_manuscript/data/16hbe/ussing_chamber/isc_exp19-42_(unsure).csv")

# ======================

# =============================
# ---factorize the variables---

tx<-unique(ussing_1942$tx)
exp<-unique(ussing_1942$exp)[order(unique(ussing_1942$exp))]
ussing_1942$tx<-factor(ussing_1942$tx,levels = tx)
ussing_1942$exp<-factor(ussing_1942$exp, levels = exp)
# ======================


# ======================
# ---calculate mean and sd of Isc and save in `summary_isc`---
summary_isc_1942<-ussing_1942 %>% group_by(time, tx,exp) %>% summarise(mean(isc, na.rm=TRUE),sd(isc, na.rm=TRUE))
colnames(summary_isc_1942) = c("time_min", "tx", "exp", "mean_isc", "sd_isc")
# ======================


ggplot(summary_isc_1942, aes(x=time_min, y=mean_isc)) + 
  geom_point(aes(col=tx)) +
  facet_grid(~tx) +
  lims(x = c(10, 47), y = c(-0.5,5)) + 
  geom_errorbar(aes(ymin=mean_isc-sd_isc, ymax=mean_isc+sd_isc, col = tx), width=.2) +
  geom_segment(aes(x = 20, y = 0, xend = 20, yend = 3), linetype = "dashed") + # or geom_vline(xintercept=20)
  geom_segment(aes(x = 30, y = 0, xend = 30, yend = 4), linetype = "dashed") +
  geom_segment(aes(x = 40, y = 0, xend = 40, yend = 2.5), linetype = "dashed") +
  geom_text(x=20, y=3.2, label= paste("fsk 10",paste0("\u03BC","M")))+           # unicode for mu is \u03BC
  geom_text(x=30, y=4.2, label= paste("VX770 10",paste0("\u03BC","M"))) +
  geom_text(x=40, y=3, label= paste("Inh-172 \n 20",paste0("\u03BC","M")))


