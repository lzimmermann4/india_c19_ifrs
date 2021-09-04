source("/Users/soumikp/Box/COVIND19_IFR/submission_microeconomics/code/ifr_forest_wave1.R")
source("/Users/soumikp/Box/COVIND19_IFR/submission_microeconomics/code/ifr_forest_wave2.R")

require(ggpubr)

plot.list <- lapply(list(p.top, p.bottom), 
                    function(p) p + theme(plot.background = element_rect(color = "black")))

p <- ggarrange(plotlist = plot.list, ncol = 1)

ggsave("/Users/soumikp/Box/COVIND19_IFR/submission_microeconomics/ifr_forest.pdf",
       p,
       device = "pdf",
       width = 18,
       height = 12,
       units = "in",
       dpi = 300,
       limitsize = FALSE)
