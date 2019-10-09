source("C:/Users/kimyo/Dropbox/CSHL/r/cftr_manuscript/src/isc_analysis.R")
e1<-filter(summary_isc,exp=="exp19-27")

ggplot(summary_isc, aes(x=time_min, y=mean_isc)) + 
  geom_point(aes(col=tx)) +
  facet_grid( exp~tx) +                              # if changed to facet_grid(tx~.), the facet is arranged vertically
  geom_errorbar(aes(ymin=mean_isc-sd_isc, ymax=mean_isc+sd_isc, col = tx), width=.2,
                position=position_dodge(.9)) +
  lims(x = c(10, 47), y = c(-0.5,5)) + 
  geom_segment(aes(x = 20, y = 0, xend = 20, yend = 3), linetype = "dashed") + # or geom_vline(xintercept=20)
  geom_segment(aes(x = 30, y = 0, xend = 30, yend = 4), linetype = "dashed") +
  geom_segment(aes(x = 40, y = 0, xend = 40, yend = 2.5), linetype = "dashed") +
  geom_text(x=20, y=3.2, label= paste("fsk 10",paste0("\u03BC","M")))+           # unicode for mu is \u03BC
  geom_text(x=30, y=4.2, label= paste("VX770 10",paste0("\u03BC","M"))) +
  geom_text(x=40, y=3, label= paste("Inh-172 \n 20",paste0("\u03BC","M")))
  
  
