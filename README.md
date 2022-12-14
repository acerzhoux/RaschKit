1.	Create a project file 'xxx.Rproj' in a working directory and open it
2.	In the opened file, run code below
library(devtools)\
# if unable to install, disconnect VPN and retry\
# if prompted to update packages, select ‘3: None’\
devtools::install_github("acerzhoux/RaschKit")\
library(RaschKit)\
install_packages_ls()\
create_folders()
3.	Explore .Rmd files in ‘rCode’ folder
