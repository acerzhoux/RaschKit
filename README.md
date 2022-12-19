1.	Create a project file 'xxx.Rproj' in a working directory and open it.
2.	In the opened file, run code below. (If unable to install RaschKit, 
    disconnect VPN. If prompted to update packages, select ‘3: None’.)
    install.packages(‘devtools’)\
    library(devtools)\
    devtools::install_github("acerzhoux/RaschKit")\
    library(RaschKit)\
    install_packages_ls()\
    create_folders()
3.	Connect VPN if needed. Explore .Rmd files in ‘rCode’ folder for data 
    exploration, calibration, equating, and DIF.
4.  Software compatibility:
    ACER ConQuest: 5.27.0\
    conquestr: 1.0.1\
    Raschkit: 1.0.1
5.  Install 'conquestr' 1.0.1 (Check version: packageVersion('conquestr'))
    - Create a project file 'xxx.Rproj' in a working directory.
    - Open 'xxx.Rproj'. From bottom right pane.    
        - click 'Packages' (3rd button after 'Files', 'Plots').
        - click 'Install'. From 'Install from', select 'Package Archive File...'.
        - Click 'Browse'. Enter into 'File name' file path 
          'P:\ACER ConQuest\Admin\InHouse - ConQuest Latest versions\conquestr\Win'.
        - Select 'conquestr_1.0.1.zip'. 
        - Click 'Open'. 
        - Click 'Install'.
