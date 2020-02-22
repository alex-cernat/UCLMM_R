# function to write syntax for categorical CFA

mh_metric_cat <- function(df_use,
                          vars_use,
                          group_var,
                          group_nm) {


  base_text <- MplusAutomation::prepareMplusData(df_use,
                                                 filename = "data.dta")

  base_text <- str_remove_all(base_text, '\\"') %>%
    str_c(collapse = "\n")

  syn_use1 <- str_c(str_c(vars_use, collapse = " \n"), ";\n")
  syn_use <- str_c("USEVARIABLES ARE ", syn_use1)
  syn_cat <- str_c("CATEGORICAL ARE ", syn_use1)

  # get the levels of variable
  # need to adapt this to be useble in function
  group_level <- order(unique(df_use[[group_var]]))

  syn_group <- str_c("GROUPING IS ", group_var, " (",
                     str_c(group_level, "=", group_nm, collapse = " "),
                     ");\n")

  syn_weights <- "WEIGHT IS designweight_combin;\n
                STRATIFICATION IS sampstratum;\n
                CLUSTER IS samppsu;\n\n"

  syn_analysis <- "ANALYSIS: TYPE IS COMPLEX;\n
                ESTIMATOR = WLSMV;\n
                ITERATIONS = 100000;\n
                PARAMETERIZATION = THETA;\n\n"

  syn_analysis <- str_c(syn_analysis,
        "DIFFTEST = ",
        vars_use[1],
        "_configural.dat;\n", collapse = "\n")


  syn_model <- "Model:\n\n"

  nr_vars <- length(vars_use)

  nr_cat <- map(vars_use, function(x) length(unique(na.omit(df_use[[x]])))) %>%
    unlist()






  metric_cat_syntax <- function(vars_use, nr_cat, grp) {





    syn_loading <- str_c("f1 BY ",
                         str_c(vars_use[1], "@1\n"))

    for (i in 2:length(vars_use)) {
      syn_loading <- str_c(syn_loading,
                           vars_use[i], " (L", i,
                           ")\n")
    }

    syn_loading <- str_c(syn_loading, ";\n\n")




    # threshold function with group
    syn_unique_threshold <- function(var_use, nr_cat, group) {
      out <- ""
      for (i in 1:(as.numeric(nr_cat[1]) - 1)) {

        # make exception for first variable as it is fixed
        if (i == 1) {
          out <- str_c(out,
                       str_c("[", var_use, "$", i, "] (t_", var_use,
                             "_", i,
                             ");\n"))
        } else {
          out <- str_c(out,
                       str_c("[", var_use, "$", i, "] (t_", var_use,
                             "_", i,
                             "_", group,
                             ");\n"))
        }

      }
      out
    }

    # threshold function without group
    syn_unique_threshold_nogroup <- function(var_use, nr_cat) {
      out <- ""
      for (i in 1:(as.numeric(nr_cat[1]) - 1)) {
        out <- str_c(out,
                     str_c("[", var_use, "$", i, "] (t_", var_use,
                           "_", i,
                           ");\n"))

      }
      out
    }




    # make thresholds for all variables within group
    # no group restrictions for the first variable and firs thresholds

    syn_thresholds <- ""

    for (i in seq_along(vars_use)) {
      if (i == 1) {
        syn_thresholds <- str_c(syn_thresholds, "\n",
                                syn_unique_threshold_nogroup(vars_use[i],
                                                             nr_cat[i]))
      } else {
        syn_thresholds <- str_c(syn_thresholds, "\n",
                                syn_unique_threshold(vars_use[i],
                                                     nr_cat[i],
                                                     grp))
      }



    }

    syn_resid <- str_c(vars_use, "@1;\n", collapse = "")

    # bring everything together
    str_c(str_c("Model ", grp, ":\n\n", collapse = ""),
          syn_loading, syn_thresholds, syn_resid, collapse = "\n")

  }



  syn_model_full <- map(group_nm, function(x)
    metric_cat_syntax(vars_use, nr_cat, x)) %>%
    unlist() %>% str_c(collapse = "\n")

  # remove first line to have a general model
  syn_model_full <- str_remove(syn_model_full, "Model.+\n")


  # syn_lat_var <- str_c("f1@1; [f1@0];\n\n")


  syn_save <- str_c("SAVEDATA: DIFFTEST IS ",
                    vars_use[1],
                    "_metric.dat;", collapse = "")


  syn_output <- str_c("\nOUTPUT: SAMPSTAT; \n
MODINDICES; \n
STD; \n
Plot: type = plot3;\n")



  # bring everything together
  out <- str_c(str_c(base_text, collapse = "\n"),
               syn_use, syn_cat, syn_group, syn_weights,
               syn_analysis, syn_model,
               syn_model_full, syn_save, syn_output, collapse = "\n")

}
