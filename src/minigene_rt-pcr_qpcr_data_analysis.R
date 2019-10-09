source("C:/Users/kimyo/Dropbox/CSHL/r/func/ggplot_library.R")
#load rt-pcr data
minigene_mrna<-read_csv("C:/Users/kimyo/Dropbox/CSHL/r/cftr_manuscript/data/minigene/minigene_rt-pcr_qpcr.csv")

# subset the screening results
screening<-grep("screen", minigene_mrna$exp_type)
screening<-minigene_mrna[screening,]

# make grouped data
screening<-screening %>%
  group_by(exp_type, exp) %>%     # group screening data by each screening type
  nest() %>%                 # makes a list of each grouped data and stores it in a big tibble
  arrange(exp_type)          # sorts exp_type in ascending order

print(screening[[3]])        # show the nested screening data 

# recalculate levels for the treatments
for (n in 1:4){
  screening[[3]][[n]]$treatment<-factor(screening[[3]][[n]]$treatment, 
                                        levels = unique(screening[[3]][[n]]$treatment[order(screening[[3]][[n]]$levels)]))
}                    # using the unique() function good if there are multiple replicate treatments

