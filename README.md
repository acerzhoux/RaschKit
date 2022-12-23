===== Steps to install RaschKit and set up ====

1.	Create a project file 'xxx.Rproj' in a working directory and open it.
2.	Software compatibility.
    - ACER ConQuest: 5.27.0\
      conquestr: 1.0.1\
      Raschkit: 1.0.1
    - Install ACER ConQuest 5.27.0.
        - From left bottom corner of Windows, 
            - Type in 'software centre' and click.
            - click 'Applications'.
            - click 'ACER ConQuest v5.27.0' and install.
    - Install 'conquestr' 1.0.1 (packageVersion('conquestr')).
        - From bottom right pane of 'xxx.Rproj' (Step 1).    
            - click 'Packages' (3rd button after 'Files', 'Plots').
            - click 'Install'. From 'Install from', select 'Package Archive File...'.
            - Click 'Browse'. Enter into 'File name' file path (if Windows)\
                P:\ACER ConQuest\Admin\InHouse - ConQuest Latest versions\conquestr\Win
            - Select 'conquestr_1.0.1.zip'. 
            - Click 'Open'. 
            - Click 'Install'.
    - Install 'RaschKit' 1.0.1 (packageVersion('RaschKit')).
        - From bottom right pane of 'xxx.Rproj' (Step 1).    
            - click 'Packages' (3rd button after 'Files', 'Plots').
            - click 'Install'. From 'Install from', select 'Package Archive File...'.
            - Click 'Browse'. Enter into 'File name' file path\
                T:\Xiaoliang Zhou
            - Select 'RaschKit_1.0.1.tar.gz'. 
            - Click 'Open'. 
            - Click 'Install'.
5.  In 'xxx.Rproj', run code below and explore .Rmd files in ‘rCode’ folder for 
    data exploration, calibration, equating, and DIF. 
    - library(RaschKit)
    - install_packages_ls()
    - create_folders()

===== v1.0.1 Updates =====

1. section_model() sets up correct model terms for group model used in DIF analysis on polytomous variable.
2. DIF_poly_shw() extracts from .shw files delta columns of all numeric.
3. plot_DIF_group() plots polytomous items.
4. plot_DIF_poly() allows for 30 overlapping labels.
5. DIF_dich() reads facilities from collapsed columns in .its file (CQ 5.27.0).