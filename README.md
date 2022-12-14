1.	Create a project file 'xxx.Rproj' in a working directory and open it.
2.	In the opened file, run code below. If unable to install RaschKit, disconnect VPN. If prompted to update packages, select ‘3: None’.
library(devtools)\
devtools::install_github("acerzhoux/RaschKit")\
library(RaschKit)\
install_packages_ls()\
create_folders()
3.	Connect VPN if needed. Explore .Rmd files in ‘rCode’ folder for data exploration, calibration, equating, and DIF.
