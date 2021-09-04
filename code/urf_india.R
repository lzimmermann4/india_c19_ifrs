require(tidyverse)
require(gsheet)
require(latex2exp)
require(ggrepel)
require(ggsci)
require(glue)
require(ggpubr)

data.obs <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv")

data.pred1 <- read_csv("/Users/soumikp/Box/COVIND19_IFR/submission_microeconomics/data/bmc_supple_1.csv") %>% 
  filter(Place == "India")

data.pred2 <- read_csv("/Users/soumikp/Box/COVIND19_IFR/submission_microeconomics/data/bmc_supple_2.csv") %>% 
  filter(Place == "India")

data <- read_csv("/Users/soumikp/Box/COVIND19_IFR/submission_microeconomics/data/URF_2.csv") %>% 
  filter(Place == "India")

wave1_case <- data.obs %>% filter(Date_YMD == "2021-01-31") %>% pull("Total Confirmed")
wave1_death <- data.obs %>% filter(Date_YMD == "2021-01-31") %>% pull("Total Deceased")

comb_case <- data.obs %>% filter(Date_YMD == "2021-07-01") %>% pull("Total Confirmed")
comb_death <- data.obs %>% filter(Date_YMD == "2021-07-01") %>% pull("Total Deceased")

wave2_case <- comb_case - wave1_case
wave2_death <- comb_death - wave1_death

plotdata <- as_tibble(rbind(c(wave1_case, 119636170, 11.1), 
      c(wave1_death, 551180.2, 3.58179), 
      c(wave2_case, 13.3*wave2_case, 13.3), 
      c(wave2_death, 3.46*wave2_death, 3.46), 
      c(comb_case, 119636170 + 13.3*wave2_case, 19.99689), 
      c(comb_death, 551180.2 + 3.46*wave2_death, 4.542223))) %>% 
  add_column(wave = factor(rep(c("Wave 1", "Wave 2", "Combined"), each = 2),
                           levels = c("Wave 1", "Wave 2", "Combined")),
             count = rep(c("Cases", "Deaths"), times = 3)) %>% 
  rename(Observed = V1, 
         Total = V2, 
         UR = V3) %>% 
  pivot_longer(cols = -c(wave, count)) %>% 
  mutate(name = ifelse(name == "Total", "Total (reported + unreported)", name))

title <- "(A) Case counts"
subtitle <- glue("Based on data from April 1, 2020 - June 30, 2021.\nWave 1: April 1, 2020 - January 31, 2021.\nWave 2: February 1, 2021 - June 30, 2021.")
# keep - unless the data is not through May 18 for whatever reason
x_lab    <- ""
#y_lab    <- "Estimated underreporting factor"                                                      # this will need to change
caption  <- glue("**\uA9 COV-IND-19 Study Group**<br>",              # keep
                 "**Source:** covid19india.org<br>",                 # keep
                 "**Note:**<br>",                                    # these lines may need to change
                # " - Owing to lack of sufficient data, estimates from states with less than 40,000 cumulative cases (as of May 15, 2021) have been left out.<br>", 
                 #" - Cumulative cases are presented in log-scale.<br>", 
                 " - Values in <span style='color:#008000'>green</span> represent undereporting factors (URF) (95% CI) for cases.<br>", 
                 " - Values in <span style='color:#ee2600'>red</span> represent observed (reported) cases (in millions).<br>",
                 " - Values in <span style='color:#3b4992'>blue</span> represent total (reported + unreported) cases (in millions).")


cases <- plotdata %>% 
  filter(count == "Cases") %>% 
  filter(name != "UR") %>%
  ggplot(aes(x = wave, y = value, fill = factor(name, levels = c("Total (reported + unreported)",
                                                                 "Observed")))) + 
  geom_bar(stat = "identity",
           #position = position_dodge(), 
           width = 0.5) +
  ylab("Cumulative cases (in millions)") + 
  xlab("") +
  annotate("text", 
           x = factor("Wave 1", levels = c("Wave 1", "Wave 2", "Combined")),  
           y = (119636170 + 1e+08), 
           label = "URF: 11.11\n(10.71 - 11.47)", 
           size = 6, 
           fontface = "bold", 
           color = "#008000") + 
  annotate("text", 
           x = factor("Wave 2", levels = c("Wave 1", "Wave 2", "Combined")),  
           y = (13.3*wave2_case + wave2_case + 1e+08), 
           label = "URF: 13.3\n(11.4 - 14.6)",
           size = 6, 
           fontface = "bold", 
           color = "#008000")+ 
  geom_label(aes(y = c(wave1_case + 1e+07, 119636170 + wave1_case+ 1e+07, 
                       wave2_case + 1e+07, 13.3*wave2_case + wave2_case+ 1e+07,
                       comb_case + 1e+07, 13.3*wave2_case +119636170 + comb_case+ 1e+07),
                 x = wave, 
                 fill = name,
                 label = paste0(format(round(value/1000000, 2), digits = 2, nsmall = 1), " M")), 
             size = 5, 
             color = "white",
             nudge_x = 0, #c(-0.125, 0.125, -0.125, 0.125, -0.125, 0.125), 
             nudge_y = 0, 
             show.legend = FALSE) + 
  scale_fill_aaas() +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(face = "bold", size = 14),
        legend.title = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 14),
        #axis.text.y = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 14),
        #axis.text.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #text               = element_text(family = "Helvetica Neue"),
        plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
        plot.subtitle      = element_text(size = 14, color = "#36454f"),
        plot.caption       = ggtext::element_markdown(hjust = 0, size = 10, lineheight = 1.1)
  ) +
  labs(
    title    = title,
    subtitle = subtitle,
    x        = x_lab,
    caption  = caption,
    fill = "Case count"
  )

title <- "(B) Death counts"
#subtitle <- glue("Based on data from April 1, 2020 - May 15, 2021.\nWave 1: April 1, 2020 - January 31, 2021.\nWave 2: February 1, 2021 - May 15, 2021.")
# keep - unless the data is not through May 18 for whatever reason
x_lab    <- ""
#y_lab    <- "Estimated underreporting factor"                                                      # this will need to change
caption  <- glue("**\uA9 COV-IND-19 Study Group**<br>",              # keep
                 "**Source:** covid19india.org<br>",                 # keep
                 "**Note:**<br>",                                    # these lines may need to change
               #  " - Owing to lack of sufficient data, estimates from states with less than 500 cumulative deaths (as of May 15, 2021) have been left out.<br>", 
                 #" - Cumulative deaths are presented in log-scale.<br>", 
                 " - Values in <span style='color:#008000'>green</span> represent undereporting factors (URF) (95% CI) for deaths.<br>", 
                 " - Values in <span style='color:#ee2600'>red</span> represent observed (reported) deaths (in thousands).<br>",
                 " - Values in <span style='color:#3b4992'>blue</span> represent total (reported + unreported) deaths (in thousands).")



deaths <- plotdata %>% 
  filter(count == "Deaths") %>% 
  filter(name != "UR") %>%
  ggplot(aes(x = wave, y = value, fill = factor(name, levels = c("Total (reported + unreported)",
                                                                 "Observed")))) + 
  geom_bar(stat = "identity", 
           #position = position_dodge(), 
           width = 0.5) +
  ylab("Cumulative deaths (in thousands)") + 
  xlab("") +
  annotate("text", 
           x = factor("Wave 1", levels = c("Wave 1", "Wave 2", "Combined")),  
           y = (551180 + 5e5), 
           label = "URF: 3.56\n(3.48 - 3.64)", 
           size = 6, 
           fontface = "bold", 
           color = "#008000") + 
  annotate("text", 
           x = factor("Wave 2", levels = c("Wave 1", "Wave 2", "Combined")),  
           y = (3.46*wave2_death + 551180), 
           label = "URF: 3.46\n(3.13 - 3.69)",
           size = 6, 
           fontface = "bold", 
           color = "#008000")+ 
  geom_label(aes(y = c(wave1_death + 2.65e+04, wave1_death + 551180 + 2.65e+04, 
                       wave2_death + 2.65e+04, wave2_death + 3.46*wave2_death + 2.65e+04,
                       comb_death + 2.65e+04, comb_death + 3.46*wave2_death + 551180 + 2.65e+04), 
                 x = wave,
                 fill = name,
                 label = paste0(format(round(value/1000, 2), digits = 2, nsmall = 1), " th")), 
             size = 5, 
             color = "white",
             nudge_x = 0,#c(-0.125, 0.125, -0.125, 0.125, -0.125, 0.125), 
             nudge_y = 2e4, 
             show.legend = FALSE) + 
  scale_fill_aaas() +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(face = "bold", size = 14),
        legend.title = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 14),
        #axis.text.y = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold", size = 14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #text               = element_text(family = "Helvetica Neue"),
        plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
        plot.subtitle      = element_text(size = 14, color = "#36454f"),
        plot.caption       = ggtext::element_markdown(hjust = 0, size = 10, lineheight = 1.1)
  ) +
  labs(
    title    = title,
    subtitle = subtitle,
    x        = x_lab,
    caption  = caption,
    fill = "Death count"
  )



p <- annotate_figure(ggarrange(cases, deaths, ncol = 2), 
               top = text_grob("Observed and estimated case (A) and death (B) counts associated with SARS-CoV-2 in India for waves 1 and 2.", 
                               color = "black", 
                               face = "bold", 
                               size = 18))

ggsave("/Users/soumikp/Box/COVIND19_IFR/submission_microeconomics/urf_india.pdf",
       p, 
       device = "pdf", 
       width = 18, 
       height = 12, 
       units = "in", 
       dpi = 300, 
       limitsize = FALSE)
         