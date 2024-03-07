# Grilches-Mairesse_Analysis

Hi there! Thank you for checking out my repository!

## Overview

This project uses data from
the Grilches-Mairesse 1995 paper to conduct statistical analysis. This project
is concerned with identifying production function parameters using between,
fixed, random effect, and differences estimators,
as well as constructing an Olley-Pakes like estimator.

## Table of Contents

- [Grilches-Mairesse_Analysis](#project-name)
  - [Overview](#overview)
  - [Table of Contents](#table-of-contents)
  - [Getting Started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Installation](#installation)
  - [Dataset](#dataset)

## Getting Started

Below are some instructions on how to get the project up and running.

### Prerequisites

Main dependencies:

```R
R version 4.3.1 (2023-06-16 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 11 x64 (build 22631)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
[3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

time zone: America/Phoenix
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] stargazer_5.2.3   plm_2.6-3         data.table_1.15.0 texreg_1.39.3    
[5] haven_2.5.3       dplyr_1.1.3      

loaded via a namespace (and not attached):
 [1] compiler_4.3.1    tidyselect_1.2.0  Rcpp_1.0.11       collapse_2.0.9   
 [5] parallel_4.3.1    lattice_0.21-8    readr_2.1.4       R6_2.5.1         
 [9] maxLik_1.5-2      generics_0.1.3    lmtest_0.9-40     Formula_1.2-5    
[13] rbibutils_2.2.16  MASS_7.3-60       forcats_1.0.0     bdsmatrix_1.3-6  
[17] tibble_3.2.1      tzdb_0.4.0        pillar_1.9.0      rlang_1.1.1      
[21] utf8_1.2.4        cli_3.6.1         magrittr_2.0.3    Rdpack_2.6       
[25] digest_0.6.33     grid_4.3.1        rstudioapi_0.15.0 hms_1.1.3        
[29] sandwich_3.0-2    nlme_3.1-162      lifecycle_1.0.3   miscTools_0.6-28
[33] vctrs_0.6.3       glue_1.6.2        zoo_1.8-12        fansi_1.0.5      
[37] pacman_0.5.1      httr_1.4.7        tools_4.3.1       pkgconfig_2.0.3  
```

```R
# Install required packages
install.packages(c("stargazer", "plm", "data.table", "texreg", "haven", "dplyr"))
```

### Installation

```bash
# Clone the repository
git clone https://github.com/willbrasic/Grilches-Mairesse_Analysis.git

# Navigate to the project directory
cd Grilches-Mairesse_Analysis

```

## Dataset

The dataset is named ECON_696Q_HW1_Data.dta. There are nine variables:
index (firm ID), sic3 (3 digit SIC), yr (1973, 1978, 183, 1988),
ldsal (log of deflated sales), lemp (log of employment),
ldnpt (log of deflated capital), ldrst (log of deflated R&D),
ldrnd (log of deflated R&D), and ldinv (log of deflated investment).

The goal is identification of the causal of effect of lemp, ldnpt, and ldrst
on ldsal.
