source("C:/Users/kimyo/Dropbox/CSHL/r/cftr_manuscript/src/minigene_rt-pcr_qpcr_data_analysis.R")
a<-screening[[3]]
sc24<-ggplot(a[[1]], aes(treatment, fc)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle=45, hjust =1))+
  labs(title = "ASO screening exon 24, minigene IVS24", x= "Treatment", y="minigene fold change")
sc25<-ggplot(a[[2]], aes(treatment, fc)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle=45, hjust =1))+
  labs(title = "ASO screening exon 25, minigene IVS25", x= "Treatment", y="minigene fold change")

sc26<-ggplot(a[[3]], aes(treatment, fc)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle=45, hjust =1))+
  labs(title = "ASO screening exon 26, minigene IVS26", x= "Treatment", y="minigene fold change")

grid.arrange(
  arrangeGrob(top=textGrob("minigene ASO screening"),sc24,sc25, sc26, ncol=1),
  arrangeGrob(p3, bottom=textGrob(
    "E16-E17 AI injection (late injection compared to E13-13.5),
assay age unknown, too few data points for MOE400",
    gp=gpar(fontsize=15, col="grey")))
)