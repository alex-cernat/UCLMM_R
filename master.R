#############################################################
#
# Project looking at how surveys influence behaviour
#
#
#
#############################################################

# clear working space
rm(list = ls())



# Admin -------------------------------------------------------------------

# folders for installed packages
# .libPaths(c(paste0("C:/Users/", Sys.getenv("USERNAME"), "/Dropbox (The University of Manchester)/R/package"),  .libPaths()))



# create folders and unzip
# dir.create("./mplus")
# dir.create("./output")
# dir.create("./functions")
# dir.create("./data")
#


# install packages and load

# use packrat to install packages localy

pkg <- c("tidyverse", "ggthemes", "haven", "MplusAutomation",
         "rmarkdown", "reshape2")

sapply(pkg, library, character.only = T)

# load local functions
map(str_c("./functions/",
          list.files("./functions/")),
    source)


# Import data -------------------------------------------------------------

data <- read_dta("./data/paper4data.dta")

data2 <- data %>%
  rename_all(~str_remove_all(., "w8|type|ure")) %>%
  mutate_all(~as.numeric(.))


# Make Mplus syntax -------------------------------------------------------



audit_nm <- str_subset(names(data2), "audit")
bully_nm <- str_subset(names(data2), "bully")
adult_nm <- str_subset(names(data2), "adult")
leisurea_nm <- str_subset(names(data2), "leisa")
leisureb_nm <- str_subset(names(data2), "leisb")
locus_nm <- str_subset(names(data2), "locus")
ghq_nm <- str_subset(names(data2), "ghq")

all_vars <- list(adult_nm, leisurea_nm,
                 leisureb_nm, locus_nm, ghq_nm)
names(all_vars) <- c("adult_nm", "leisurea_nm",
                     "leisureb_nm", "locus_nm", "ghq_nm")

# we treat bully and audit differently

groups <- c("Web", "Tel", "Ftf")

MplusAutomation::prepareMplusData(data2,
                                  filename = "./mplus/data.dta")


# get configural syntax for all scales
conf_syntax <- map(all_vars, function(x) {
  mh_config_cat(data2, x, "mode", groups)
})

# get metric syntax
metric_syntax <- map(all_vars, function(x) {
  mh_metric_cat(data2, x, "mode", groups)
})


# get scalar syntax
scalar_syntax <- map(all_vars, function(x) {
  mh_scalar_cat(data2, x, "mode", groups)
})

cat(conf_syntax[[1]])
cat(metric_syntax[[1]])
cat(scalar_syntax[[1]])


names(conf_syntax) <- str_c("configural_", names(all_vars))
names(metric_syntax) <- str_c("metric_", names(all_vars))
names(scalar_syntax) <- str_c("scalar_", names(all_vars))

all_syntaxes <- list(conf_syntax, metric_syntax, scalar_syntax) %>%
  unlist()


i <- 1
map(all_syntaxes, function(x) {
  write.table(all_syntaxes[i],
              str_c("./mplus/", names(all_syntaxes)[i], ".inp"),
              quote = F,
              row.names = F,
              col.names = F)
  i <<- i + 1
})





runModels(target = "C:/Users/msassac6/Dropbox (The University of Manchester)/Papers/Joe S/UCLMM/UCLMM_R/mplus/",
          showOutput = T, replaceOutfile = "always")




# Import Mplus ------------------------------------------------------------

mfiles <- list.files("./mplus/",
           pattern = ".out",
           full.names = T)

sfiles <- list.files("./mplus/special/",
                     pattern = ".out",
                     full.names = T)



# get fit indices
most_fit <- mplus_read_fit_list(mfiles)
special_fit <- mplus_read_fit_list(sfiles)

all_fit <- bind_rows(most_fit, special_fit) %>%
  select(-Comp)


View(all_fit)

# audit does not run. try continous

# get coefficients for loadingds and thresholds
# all_conf_ind <- mplus_ind_coef(mfiles)


write.csv(all_fit, file = "./output/fit_table_all.csv")






diff_thresholds <- rbind(
  mplus_ind_coef(mfiles[8])[[2]],
  mplus_ind_coef(mfiles[9])[[2]]
)


diff_thresholds <- diff_thresholds %>%
  mutate(max_dif = pmax(abs(ftf_tel),
                        abs(ftf_web),
                        abs(tel_web)))


diff_thresholds <- diff_thresholds %>%
  mutate_at(c("FTF", "TEL", "WEB"),
            ~round(pnorm(.), 2))

diff_thresholds <- diff_thresholds %>%
  separate(param, into = c("var", "cat"), sep = "\\$")



# Graph with differences in predicted probs -------------------------------



thr <- tibble(var = unique(diff_thresholds$var),
       cat = 0,
       FTF = 0,
       TEL = 0,
       WEB = 0) %>%
  rbind(select(diff_thresholds, var:WEB)) %>%
  arrange(var, cat) %>%
  mutate(prob_f2f = lead(FTF) - FTF,
         prob_f2f = ifelse(prob_f2f < 0, 1 + prob_f2f, prob_f2f),
         prob_web = lead(WEB) - WEB,
         prob_web = ifelse(prob_web < 0, 1 + prob_web, prob_web),
         prob_tel = lead(TEL) - TEL,
         prob_tel = ifelse(prob_tel < 0, 1 + prob_tel, prob_tel),
         cat = as.numeric(cat) + 1) %>%
  dplyr::select(var, cat, matches("prob"))


# deal with missing last row
thr <- thr %>%
  mutate_at(.vars = c("prob_f2f", "prob_web", "prob_tel"),
            ~ifelse(is.na(.), 1 - lag(.), .))


leis_info <- select(data, matches("leisure")) %>%
  map(attributes)

leis_names <- map(leis_info, function(x) x$label) %>%
  unlist() %>%
  str_remove("How often do activity:") %>%
  str_trim() %>%
  str_split(";", simplify = T) %>%
  .[, 1] %>% unlist()


leis_cat <- map(leis_info, function(x) x$labels) %>%
  unlist() %>%
  tibble(names = names(.),
         value = .) %>%
  filter(value > 0) %>%
  separate(names, into = c("var", "cat"), sep =  "\\.")



thr <- thr %>%
  mutate(Label = rep(leis_names, each = 4),
         Category = leis_cat$cat)

thr <- thr %>%
  gather(key = Mode,
         value = Value,
         -var, -cat, -Label, -Category) %>%
  mutate(Mode = str_remove(Mode, "prob_"))

thr %>%
  mutate(Mode2 = case_when(Mode == "f2f" ~ "FTF",
                           Mode == "tel" ~ "TEL",
                           Mode == "web" ~ "WEB")) %>%
  ggplot(aes(fct_rev(Category), Value, fill = Mode2)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Label, ncol = 2) +
  coord_flip() +
  theme_bw(base_size = 8) +
  labs(y = "Predicted probability to answer category",
       x = "Category",
       fill = "Mode")


ggsave("./output/leisure_pred_prob.png", dpi = 800)









# Means and variances in final models -------------------------------------

sfiles_means <- list.files("./mplus/special/means/",
                           pattern = ".out",
                           full.names = T)


nc_files_means <- list.files("./mplus/mean no correction/",
                           pattern = ".out",
                           full.names = T)


all_links <- c(mfiles, sfiles_means)

scal_link <- str_subset(all_links, "scalar")

scal_nms <- str_split(scal_link, "_", simplify = T)[, 2]
scal_nms_nc <- str_split(nc_files_means, "_", simplify = T)[, 2]

lat_stats <- map_df(scal_link, mplus_lat_stats)
lat_stats_nc <- map_df(nc_files_means, mplus_lat_stats)


lat_stats <- lat_stats %>%
  mutate(Variable = rep(scal_nms, each = 6),
         lci = est - (1.96 * se),
         uci = est + (1.96 * se),
         Model = "Yes")

lat_stats_nc <- lat_stats_nc %>%
  mutate(Variable = rep(scal_nms_nc, each = 6),
         lci = est - (1.96 * se),
         uci = est + (1.96 * se),
         Model = "No")





rbind(lat_stats, lat_stats_nc) %>%
  filter(Statistic == "Means", Model == "Yes") %>%
  mutate(Variable = str_to_title(Variable),
         Variable = str_replace(Variable, "Ghq","GHQ"),
         Variable = str_replace(Variable, "Leisure","Leisure ")) %>%
  ggplot(aes(Variable, est, color = Group)) +
  geom_point(position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                width = 0,
                position = position_dodge(0.7)) +
  theme_bw(base_size = 18) +
  labs(x = "Scale",
       y = "Mean estimate compared to Web",
       shape = "Selection correct",
       linetype = "Selection correct")

ggsave("./output/means_lv.png", dpi = 300)







rbind(lat_stats, lat_stats_nc) %>%
  filter(Statistic == "Means") %>%
  mutate(Variable = str_to_title(Variable),
         Variable = str_replace(Variable, "Ghq","GHQ"),
         Variable = str_replace(Variable, "Leisure","Leisure ")) %>%
  ggplot(aes(Variable, est, color = Group,
             shape = Model,
             linetype = Model)) +
  geom_point(position = position_dodge(0.7)) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                width = 0,
                position = position_dodge(0.7)) +
  theme_bw(base_size = 10) +
  labs(x = "Scale",
       y = "Mean estimate compared to Web",
       shape = "Selection correct",
       linetype = "Selection correct") +
  coord_flip()

ggsave("./output/means_lv2.png", dpi = 300)




# Partial equivalence -----------------------------------------------------


## do partial equivalence by hand with laisure A and B



pqfiles <- list.files("./mplus/partial eq/",
                     pattern = ".out",
                     full.names = T)



# get fit indices
pq_fit <- mplus_read_fit_list(pqfiles)


coefs_free <- c(NA, NA, NA,
                "leisa0f$3", "leisa0b$3", "leisa0a$1", "leisa0e$1",
                NA, NA, NA, "leisb0b$2")

pq_fit <- pq_fit %>%
  mutate(Comp = coefs_free)



write.csv(pq_fit, file = "./output/fit_table_pq.csv")

# do graphs of categories based on partial equivalence






diff_thresholds2 <- rbind(
  mplus_ind_coef(pqfiles[9])[[2]],
  mplus_ind_coef(pqfiles[11])[[2]]
)


diff_thresholds2 <- diff_thresholds2 %>%
  mutate_at(c("FTF", "TEL", "WEB"),
            ~round(pnorm(.), 2))

diff_thresholds2 <- diff_thresholds2 %>%
  separate(param, into = c("var", "cat"), sep = "\\$")


thr <- tibble(var = unique(diff_thresholds2$var),
              cat = 0,
              FTF = 0,
              TEL = 0,
              WEB = 0) %>%
  rbind(select(diff_thresholds2, var:WEB)) %>%
  arrange(var, cat) %>%
  mutate(prob_f2f = lead(FTF) - FTF,
         prob_f2f = ifelse(prob_f2f < 0, 1 + prob_f2f, prob_f2f),
         prob_web = lead(WEB) - WEB,
         prob_web = ifelse(prob_web < 0, 1 + prob_web, prob_web),
         prob_tel = lead(TEL) - TEL,
         prob_tel = ifelse(prob_tel < 0, 1 + prob_tel, prob_tel),
         cat = as.numeric(cat) + 1) %>%
  dplyr::select(var, cat, matches("prob"))


# deal with missing last row
thr <- thr %>%
  mutate_at(.vars = c("prob_f2f", "prob_web", "prob_tel"),
            ~ifelse(is.na(.), 1 - lag(.), .))


leis_info <- select(data, matches("leisure")) %>%
  map(attributes)

leis_names <- map(leis_info, function(x) x$label) %>%
  unlist() %>%
  str_remove("How often do activity:") %>%
  str_trim() %>%
  str_split(";", simplify = T) %>%
  .[, 1] %>% unlist()


leis_cat <- map(leis_info, function(x) x$labels) %>%
  unlist() %>%
  tibble(names = names(.),
         value = .) %>%
  filter(value > 0) %>%
  separate(names, into = c("var", "cat"), sep =  "\\.")



thr <- thr %>%
  mutate(Label = rep(leis_names, each = 4),
         Category = leis_cat$cat)

thr <- thr %>%
  gather(key = Mode,
         value = Value,
         -var, -cat, -Label, -Category) %>%
  mutate(Mode = str_remove(Mode, "prob_"))




thr %>%
  filter(var %in% c("LEISA0F", "LEISA0B", "LEISA0A", "LEISA0E", "LEISB0B")) %>%
  mutate(Mode2 = case_when(Mode == "f2f" ~ "FTF",
                           Mode == "tel" ~ "TEL",
                           Mode == "web" ~ "WEB")) %>%
  ggplot(aes(fct_rev(Category), Value, fill = Mode2)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Label, ncol = 2) +
  coord_flip() +
  theme_bw(base_size = 8) +
  labs(y = "Predicted probability to answer category",
       x = "Category",
       fill = "Mode")


ggsave("./output/leisure_pred_prob_pq.png", dpi = 600)




# graph with means with and without partial equivalence

scal_link <- str_subset(pqfiles[c(5, 10, 9, 11)], "scalar")

scal_nms <- str_split(scal_link, "_", simplify = T)[, 2] %>%
  str_c(rep(c("_full", "_pq"), each = 2))

lat_stats <- map_df(scal_link, mplus_lat_stats)


lat_stats <- lat_stats %>%
  mutate(Variable = rep(scal_nms, each = 6),
         lci = est - (1.96 * se),
         uci = est + (1.96 * se))



lat_stats %>%
  filter(Statistic == "Means") %>%
  select(-Statistic, -se) %>%
  separate(Variable, into = c("Scale", "Model"), sep = "_") %>%
  mutate(Scale = str_to_title(Scale),
         Scale = str_replace(Scale, "Leisure","Leisure "),
         Model = case_when(Model == "full" ~ "Full scalar",
                           Model == "pq" ~ "Partial equivalence")) %>%
  ggplot(aes(Scale, est, color = Model,
             lintype = Group, shape = Group)) +
  geom_point(position = position_dodge(0.4)) +
  geom_errorbar(aes(ymin = lci, ymax = uci),
                width = 0,
                position = position_dodge(0.4)) +
  theme_bw() +
  labs(x = "Scale",
       y = "Mean estimate compared to Web")

ggsave("./output/means_lv_pq.png", dpi = 300)
