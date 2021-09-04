library('ggplot2') 
require(glue)
require(ggrepel)
require(tidyverse)
require(ggsci)

states <- c("Andhra Pradesh", "Assam", "Bihar", "Chattisgarh", 
            "Delhi", "Goa", "Gujarat", "Haryana", 
            "Jharkhand", "Karnataka", "Kerala", "Maharashtra",
            "Madhya Pradesh", "Odisha", "Punjab", "Rajasthan", 
            "Telangana", "Tamil Nadu", "Uttar Pradesh", "Uttarakhand", 
            "West Bengal")

data <- read_csv("~/Box/COVIND19_IFR/results/RR_results/CI.csv") %>% select(-X1)

data.fp <- as_tibble(rbind(cbind(data$Place, "URF(C)", data$UF_cases_mean, data$UF_cases_low, data$UF_cases_high),
                           cbind(data$Place, "URF(D)", data$UF_deaths_mean, data$UF_deaths_low, data$UF_deaths_high))) %>% 
  rename(Place = V1, 
         Type = V2, 
         pe = V3,
         l = V4, 
         u = V5) %>% 
  mutate(pe = as.numeric(pe), 
         l = as.numeric(l), 
         u = as.numeric(u))

data.fp.text <- data.fp %>% 
  mutate(Type = factor(Type)) %>% 
  mutate(tt = paste0(Type, ": ", format(round(pe, 2), nsmall = 2), " (", format(round(l, 2), nsmall = 2), "-",format(round(u, 2), nsmall = 2),")")) %>% 
  select(Place, Type, tt) %>%
  pivot_wider(values_from = tt, names_from = Type) 


data.final <- full_join(data.fp %>% 
                          mutate(Type = as.factor(Type)), data.fp.text)


ord.level = data.final %>% 
  filter(Place!="India") %>%
  filter(Type == "URF(C)") %>% 
  arrange(pe) %>% 
  pull(Place)


title    <- "Estimated first-wave (top)  and second-wave (bottom) underreporting factors associated with SARS-CoV-2 for states in India"       # change
subtitle <- "Wave 1: April 1, 2020 - January 31, 2021. Wave 2: February 1, 2021 - June 30, 2021."                                        # keep - unless the data is not through May 18 for whatever reason
x_lab    <- "State/Union territory"
y_lab    <- "Estimated underreporting factor"                                                      # this will need to change
caption  <- glue("**\uA9 COV-IND-19 Study Group**<br>",              # keep
                 "**Source:** covid19india.org<br>",                 # keep
                 "**Note:**<br>",                                    # these lines may need to change
                 " - Owing to lack of sufficient data, estimates for only twenty states with highest case counts have been presented.<br>",
                 " - Coloured <span style='color:#3B4992FF'>blue for URF(C)</span> and <span style='color:#EE0000FF'>red for URF(D)</span> values.")

india.urf <- data.final %>% 
  filter(Place=="India")



p.top <-  ggplot(data.final %>% 
               filter(Place!="India") %>% 
               filter((Place %in%  states)) %>% 
               mutate(Place = factor(Place, levels = ord.level)), 
             aes(x = Place, y = pe, ymin = l, ymax = u, col = Type)) + 
  geom_linerange(size=1) + 
  geom_point(size=1, 
             shape=21, 
             aes(fill = Type)) + 
  geom_hline(yintercept = 11.1, color = "#3B4992FF", linetype = "dotted") + 
  geom_hline(yintercept = 3.56, color = "#EE0000FF", linetype = "dashed") + 
  #geom_line(aes(x = Place, y = pe, group = Type)) +
  #theme_minimal() + 
  coord_flip() + 
  scale_color_aaas() +
  scale_fill_aaas() + 
  ylab("Estimated underreporting factors") + 
  geom_hline(yintercept = 45) +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(face = "bold", size = 14),
        legend.title = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold.italic", size = 12),
        axis.text.x = element_text(face = "bold", size = 12),
        #text               = element_text(family = "Helvetica Neue"),
        plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
        plot.subtitle      = element_text(size = 14, color = "#36454f"),
        plot.caption       = ggtext::element_markdown(hjust = 0, size = 10, lineheight = 1.1)
  ) + 
  geom_text(aes(x = Place, y = 61, label= `URF(C)`), vjust = 0.5, hjust = 1, size = 3.5, col = "#3B4992FF") + 
  geom_text(aes(x = Place, y = 46, label= `URF(D)`), vjust = 0.5, hjust = 0, size = 3.5, col = "#EE0000FF") + 
  #geom_text(aes(x = Place, y = 6.1, label= CFR), vjust = 0.5, hjust = 1, size = 3.5, col = "#008B45FF") + 
  scale_x_discrete(expand = c(0.01, 0.00)) + 
  scale_y_continuous(breaks = seq(0, 45, 5), limits = c(0, NA)) + 
  labs(
    title    = title,
    subtitle = subtitle,
    x        = x_lab,
    y        = y_lab#,
    #caption  = caption
  ) +
  annotate("text", label = glue("URF(C) for India\n11.1 (10.7 - 11.5)"),
           x = "Maharashtra", y = 12, hjust = 0, vjust = -0.95, color = "#3B4992FF", 
           fontface = "bold", angle = 90, size = 5) +
  annotate("text", label = glue("URF(D) for India\n3.56 (3.48-3.64)"),
           x = "Maharashtra", y = 3, hjust = 0, vjust = -0.95, color = "#EE0000FF", 
           fontface = "bold", angle = 90, size = 5) 


