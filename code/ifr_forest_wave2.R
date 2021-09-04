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


data <- read_csv("/Users/soumikp/Box/COVIND19_IFR/submission_microeconomics/data/URF_2.csv") %>% select(-X1)

#data <- read_csv("URF_N.csv")%>% select(-X1)

data.fp <- as_tibble(rbind(cbind(data$Place, "IFR1", data$ifr1_mean, data$ifr1_low, data$ifr1_high),
                           cbind(data$Place, "IFR2", data$ifr2_mean, data$ifr2_low, data$ifr2_high),
                           cbind(data$Place, "CFR", data$cfr_mean,  data$cfr_low, data$cfr_high))) %>%
  rename(Place = V1,
         Type = V2,
         pe = V3,
         l = V4,
         u = V5) %>%
  mutate(pe = 100*as.numeric(pe),
         l = 100*as.numeric(l),
         u = 100*as.numeric(u))

# data.fp %>%
#   ggplot(aes(x = pe, y = Place, group = Place)) +
#   geom_point(aes(color = Type))

# data.fp2<-data.fp %>%
#   filter(Place %in% c("Maharashtra", #
# "Kerala",#
# "Karnataka",#
# #"Andhra Pradesh",
# "Tamil Nadu",#
# "Uttar Pradesh",#
# "West Bengal",#
# "Odisha",#
# "Rajasthan",#
# "Chattisgarh",
# "India")) %>% #
#   mutate(Place = factor(Place, levels = c("Maharashtra", "West Bengal", "Uttar Pradesh",
#                                           "Tamil Nadu", "Karnataka", "Rajasthan",
#                                           "India", "Odisha", "Chattisgarh", "Kerala")))

#show_col(pal_aaas()(6))

fill <- c("#B24745FF", "#79AF97FF", "#DF8F44FF")





data.fp.text <- data.fp %>%
  mutate(Type = factor(Type)) %>%
  #filter(Place != "Arunachal Pradesh", Place != "Dadra and Nagar Haveli") %>%
  mutate(tt = paste0(Type, ": ", format(round(pe, 3), nsmall = 3), " (", format(round(l, 3), nsmall = 3), "-", format(round(u, 3), nsmall = 3),")")) %>%
  select(Place, Type, tt) %>%
  pivot_wider(values_from = tt, names_from = Type)


data.final <- full_join(data.fp %>%
                          mutate(Type = factor(Type, levels = c("IFR1", "IFR2", "CFR"))) # %>%
                        #filter(Place != "Arunachal Pradesh",
                        # Place != "Dadra and Nagar Haveli")
                        ,data.fp.text)

ord.level = data.final %>%
  filter(Place!="India") %>%
  filter(Type == "IFR1") %>%
  arrange(pe) %>%
  pull(Place)

india.ifr <- data.final %>%
  filter(Place=="India")

title    <- "Estimated first-wave (top) and second-wave (bottom) fatality rates associated with SARS-CoV-2 for states in India"       # change
subtitle <- "Wave 1: April 1, 2020 - January 31, 2021. Wave 2: February 1, 2021 - ??? (ask Lauren)"                                        # keep - unless the data is not through May 18 for whatever reason
x_lab    <- "State/Union territory"
y_lab    <- "Estimated fatality rate (%)"                                                      # this will need to change
caption  <- glue("**\uA9 COV-IND-19 Study Group**<br>",              # keep
                 "**Source:** covid19india.org<br>",                 # keep
                 "**Note:**<br>",                                    # these lines may need to change
                 " - Owing to lack of sufficient data, estimates for only twenty states with highest case counts have been presented.<br>",
                 " - Coloured <span style='color:#3B4992FF'> blue for IFR1</span>, <span style='color:#EE0000FF'>red for IFR2</span> and <span style='color:#008B45FF'>green for CFR</span> values.")


p.bottom <- ggplot(data.final %>%
                  filter(Place!="India") %>%
                  filter(Place %in% states) %>% 
                  mutate(Place = factor(Place, levels = ord.level)),
                aes(x = Place, y = pe, ymin = l, ymax = u, col = Type)) +
  geom_linerange(size=1) +
  geom_point(size=1,
             shape=21,
             aes(fill = Type)) +
  geom_hline(yintercept = 0.095, color = "#3B4992FF", linetype = "dotted") +
  geom_hline(yintercept = 0.326, color = "#EE0000FF", linetype = "dashed") +
  geom_hline(yintercept = 1.25, color = "#008B45FF", linetype = "dotdash") +
  #geom_line(aes(x = Place, y = pe, group = Type)) +
  #theme_minimal() +
  coord_flip() +
  scale_color_aaas() +
  scale_fill_aaas() +
  ylab("Estimated fatality rate (%)") +
  geom_hline(yintercept = 3.8) +
  theme(legend.position = "bottom",
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
  geom_text(aes(x = Place, y = 3.9, label= IFR1), vjust = 0.5, hjust = 0, size = 3.5, col = "#3B4992FF") +
  geom_text(aes(x = Place, y = 5.0, label= IFR2), vjust = 0.5, hjust = 0.5, size = 3.5, col = "#EE0000FF") +
  geom_text(aes(x = Place, y = 6.1, label= CFR), vjust = 0.5, hjust = 1, size = 3.5, col = "#008B45FF") +
  scale_x_discrete(expand = c(0.01, 0.00)) +
  scale_y_continuous(breaks = seq(0, 3.8, 0.2)) +
  labs(
    #title    = title,
    #subtitle = subtitle,
    x        = x_lab,
    y        = y_lab,
    caption  = caption
  ) +
  annotate("text", label = glue("IFR1 for India\n0.095 (0.086-0.109)"),
           x = "Madhya Pradesh", y =0.07, hjust = 0, vjust = -0.2, color = "#3B4992FF", 
           fontface = "bold", angle = 90, size = 5) +
  annotate("text", label = glue("IFR2 for India\n0.326 (0.316-0.342)"),
           x = "Tamil Nadu", y = 0.65, hjust = 0, vjust = -0.2, color = "#EE0000FF",
           fontface = "bold", angle = 90, size = 5) +
  annotate("text", label = glue("CFR for India\n1.248 (1.243-1.253)"),
           x = "West Bengal", y = 1.5, hjust = 0, vjust = -0.2, color = "#008B45FF", 
           fontface = "bold", angle = 90, size = 5)



# 
# ggsave("fig1_wave1.pdf", p,
#        device = "pdf",
#        width = 18,
#        height = 12,
#        units = "in",
#        dpi = 300,
#        limitsize = FALSE)
