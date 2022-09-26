# Measurement Equivalence in Sequential Mixed-Mode Surveys
[![DOI](https://doi.org/10.18148/srm/2022.v16i1.7811)](https://doi.org/10.18148/srm/2022.v16i1.7811)

This is the code associated with the paper: Sakshaug, J., Cernat, A., Silverwood, R. J., Calderwood, L., & Ploubidis, G. B. (2022). Measurement Equivalence in Sequential Mixed-Mode Surveys. Survey Research Methods, 16(1), 29-43.


# Process

1. data was initially cleaned in Stata (see "statacode.do" in the Stata subfolder)
3. R and MPLUS were used to efficiently run the equivalence testing for all the scales ("master.R"). The R code created the Mplus syntax (in the mplus folder), estimated the models and then imported them back. Helper functions used for that can be found in the "functions" folder.
4. Graphs and tables were saved in the "output" folder as well as in "./data/clean"


# Data used

We started the analysis using restricted data of Next Steps Waves 1-8 provided internally by the UCL Next Steps team. Most Next Steps data are available through the UK Data Service. Visit the UK Data Service study page for Next Steps [SN 2000030]: https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000030

# Software

We used three different software for this paper:

- Stata 15.1 for the alternative data quality indicators
- R 3.6.2 for data cleaning and batch analysis of equivalence testing
- Mplus 8.6 for equivalence testing

`R` session info:

```
R version 3.6.2 (2019-12-12)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19042)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252   
[3] LC_MONETARY=English_United Kingdom.1252 LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] reshape2_1.4.3        rmarkdown_2.1         MplusAutomation_0.7-3
 [4] haven_2.2.0           ggthemes_4.2.0        forcats_0.4.0        
 [7] stringr_1.4.0         dplyr_0.8.4           purrr_0.3.3          
[10] readr_1.3.1           tidyr_1.0.2           tibble_2.1.3         
[13] ggplot2_3.2.1         tidyverse_1.3.0      

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.3        lubridate_1.7.4   lattice_0.20-38   assertthat_0.2.1 
 [5] digest_0.6.24     packrat_0.5.0     R6_2.4.1          cellranger_1.1.0 
 [9] plyr_1.8.5        backports_1.1.5   reprex_0.3.0      coda_0.19-3      
[13] evaluate_0.14     httr_1.4.1        pillar_1.4.3      rlang_0.4.4      
[17] lazyeval_0.2.2    readxl_1.3.1      rstudioapi_0.11   data.table_1.12.8
[21] texreg_1.36.23    gsubfn_0.7        proto_1.0.0       pander_0.6.3     
[25] munsell_0.5.0     broom_0.5.4       xfun_0.12         compiler_3.6.2   
[29] modelr_0.1.5      pkgconfig_2.0.3   htmltools_0.4.0   tidyselect_1.0.0 
[33] fansi_0.4.1       crayon_1.3.4      dbplyr_1.4.2      withr_2.1.2      
[37] grid_3.6.2        nlme_3.1-142      jsonlite_1.6.1    xtable_1.8-4     
[41] gtable_0.3.0      lifecycle_0.1.0   DBI_1.1.0         magrittr_1.5     
[45] scales_1.1.0      cli_2.0.1         stringi_1.4.6     fs_1.3.1         
[49] xml2_1.2.2        vctrs_0.2.2       generics_0.0.2    boot_1.3-23      
[53] tools_3.6.2       glue_1.3.1        hms_0.5.3         parallel_3.6.2   
[57] colorspace_1.4-1  rvest_0.3.5       knitr_1.28   
```
