# Mplus functions





# read fit indices
read_fit <- function(model) {

  fit <- readModels(model)$summaries

  indices <- c("Filename", "ChiSqM_Value", "ChiSqM_DF",
               "CFI", "TLI","RMSEA_Estimate")

  if ("ChiSqDiffTest_Value" %in% names(fit)) {
    indices <- c(indices, "ChiSqDiffTest_Value", "ChiSqDiffTest_DF",
                 "ChiSqDiffTest_PValue")
  }


  fit[indices]


}




# list multiple models

mplus_read_fit_list <- function(list) {

fit_table_a <- map_df(list, read_fit)

separate_vector <- c("Scale", "Model")


# if group comparison add variable
if (length(str_extract_all(fit_table_a$Filename[1], "_")[[1]]) == 2) {

  separate_vector <- c(separate_vector, "Comp")
}


fit_table_a2 <- fit_table_a %>%
  tbl_df() %>%
  separate(Filename, into = separate_vector) %>%
  rename(Chi2 = ChiSqM_Value, df = ChiSqM_DF,
         RMSEA = RMSEA_Estimate,
         Chi2_diff = ChiSqDiffTest_Value,
         Chi2_diff_df = ChiSqDiffTest_DF,
         Chi2_dfff_p = ChiSqDiffTest_PValue)

if ("Comp" %in% names(fit_table_a2)) {
  fit_table_a2 <- fit_table_a2 %>%
    arrange(Comp, Model)
} else {
  fit_table_a2 <- fit_table_a2 %>%
    arrange(Model)
}


fit_table_a2
}



# get loadings and intercepts
mplus_ind_coef <- function(model) {

  test <- readModels(model)

  # select loadings
  loadings <- test$parameters$unstandardized %>%
    filter(str_detect(paramHeader, "BY")) %>%
    select(param, est, Group) %>%
    mutate(param = factor(param,
                          levels = unique(param)),
           est = round(est, 2)) %>%
    spread(key = Group, value = est)

  # do differences by groups if present

  groups <- unique(test$parameters$unstandardized$Group)

  if (all(c("FTF", "TEL") %in% groups)) {
    loadings <- mutate(loadings, ftf_tel = FTF - TEL)
  }

  if (all(c("FTF", "WEB") %in% groups)) {
    loadings <- mutate(loadings, ftf_web = FTF - WEB)
  }

  if (all(c("TEL", "WEB") %in% groups)) {
    loadings <- mutate(loadings, tel_WEB = TEL - WEB)
  }


  thresholds <- test$parameters$unstandardized %>%
    tbl_df() %>%
    filter(str_detect(paramHeader, "Thresholds")) %>%
    select(param, est, Group) %>%
    mutate(param = factor(param,
                          levels = unique(param)),
           est = round(est, 2)) %>%
    spread(key = Group, value = est)

  # do differences by groups if present

  if (all(c("FTF", "TEL") %in% groups)) {
    thresholds <- mutate(thresholds, ftf_tel = round(FTF - TEL, 2))
  }

  if (all(c("FTF", "WEB") %in% groups)) {
    thresholds <- mutate(thresholds, ftf_web = round(FTF - WEB, 2))
  }

  if (all(c("TEL", "WEB") %in% groups)) {
    thresholds <- mutate(thresholds, tel_web = round(TEL - WEB, 2))
  }

  list(loadings = loadings,
       thresholds = thresholds)

}




# funciton to get mean and variances of models

mplus_lat_stats <- function(file) {
  readModels(file)$parameters$std.standardized %>%
    tbl_df() %>%
    filter(param == "F1") %>%
    select(paramHeader, est, se, Group) %>%
    rename(Statistic = paramHeader)

}
