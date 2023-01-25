===== Steps to install RaschKit and set up ====

1.  Create a project file 'xxx.Rproj' in a working directory and open it.
2.  Run code below to install packages.\
    lapply(\
        c('plyr', 'conquestr', 'rlang','bookdown',\
          'ggthemes', 'ggrepel', 'patchwork','rmarkdown',\
          'gdata', 'janitor', 'data.table','knitr',\
          'RColorBrewer', 'fs', 'here', 'lazyeval',\
          'writexl', 'ggpubr', 'qpdf', 'tidyverse'), \
        function(x){\
          if (!require(x, character.only=TRUE)){\
             install.packages(x)\
             library(x, character.only=TRUE)}}\
    )
3.	Software compatibility.
    - ACER ConQuest: 5.27.0\
      conquestr: 1.0.5\
      Raschkit: 1.0.2
    - Install ACER ConQuest 5.27.0.
        - From left bottom corner of Windows, 
            - Type in 'software centre' and click.
            - click 'Applications'.
            - click 'ACER ConQuest v5.27.0' and install.
    - Install 'conquestr' 1.0.5 (packageVersion('conquestr')).
        - From bottom right pane of 'xxx.Rproj' (Step 1).    
            - click 'Packages' (3rd button after 'Files', 'Plots').
            - click 'Install'. From 'Install from', select 'Package Archive File...'.
            - Click 'Browse'. Enter into 'File name' file path (if Windows)\
                P:\ACER ConQuest\Admin\InHouse - ConQuest Latest versions\conquestr\Win
            - Select 'conquestr_1.0.5.zip'. 
            - Click 'Open'. 
            - Click 'Install'.
    - Install 'RaschKit' 1.0.2 (packageVersion('RaschKit')).
        - From bottom right pane of 'xxx.Rproj' (Step 1).    
            - click 'Packages' (3rd button after 'Files', 'Plots').
            - click 'Install'. From 'Install from', select 'Package Archive File...'.
            - Click 'Browse'. Enter into 'File name' file path\
                T:\Xiaoliang Zhou
            - Select 'RaschKit_1.0.2.tar.gz'. 
            - Click 'Open'. 
            - Click 'Install'.
4.  In 'xxx.Rproj', run code below and explore .Rmd files in ‘rCode’ folder for 
    data exploration, calibration, equating, and DIF. 
    - library(RaschKit)
    - install_packages_ls()
    - create_folders()

===== v1.0.2 Updates =====
1. Suppressed ConQuest, Rmd, and other unnecessary printing in R console.
2. install_packages_ls() included packages needed to knit Rmd files.
3. freq_resps_cat() ordered in numerical/alphabetic order score categories in column names 
   of wide form. It also added response total 'N' in output and argument 'prop' 
   for proportion.
4. calibrate() supports case weights with argument 'pweight'.
5. calibrate() accepts dataframe with covariates after response columns. Need 
   specify 'n_dims'.
6. Used .txt extension for input and output text files.
7. Inside functions, changed some %>% to |>, removed some dependencies, and 
   added some package names to functions (ongoing).
8. 'facet' method of DIF analysis for polytomous variable shows DIF variable 
   name in both table column name and plot legend. Also, a note is shown beside 
   table to interpret results.

===== v1.0.1 Updates =====

1. section_model() sets up correct model terms for group model used in DIF 
   analysis on polytomous variable.
2. DIF_poly_shw() extracts from .shw files delta columns of all numeric.
3. plot_DIF_group() plots polytomous items.
4. plot_DIF_poly() allows for 30 overlapping labels.
5. DIF_dich() reads facilities from collapsed columns in .its file (CQ 5.27.0).