

library(tidyverse)



tab <- read_csv("C:/Users/msassac6/Dropbox (The University of Manchester)/Papers/Joe S/UCLMM/UCLMM_R/data/Table_A11_data_v2.csv")


pd <- position_dodge(-.5)

tab %>% 
  mutate(variable = as.factor(variable),
         mode2 = factor(mode, labels = c("Face-to-face",
                                         "Telephone",
                                         "Web")) %>% 
           fct_rev(),
         type2 = str_to_title(type) %>% 
           as.factor() %>% 
           fct_relevel("Refrence", "Unadjested"),
         variable = as.factor(variable) %>% 
           fct_inorder() %>% 
           fct_rev(),
         type3 = factor(type2, labels = c("W1", "W8", "W8 adjust"))) %>%
  ggplot(aes(est, variable, color = type3)) +
  geom_point(size = 0.9, position = pd) +
  geom_errorbarh(aes(xmin = lb, xmax = ub, height = 0),
                 position = pd) +
  facet_wrap(~mode2) +
  theme_bw() +
  labs(x = "Percent",
       y = "Variable",
       color = "Estimate type")


ggsave("C:/Users/msassac6/Dropbox (The University of Manchester)/Papers/Joe S/UCLMM/UCLMM_R/output/Table_A11_data_v2.png", 
       dpi = 300,
       width = 9)
