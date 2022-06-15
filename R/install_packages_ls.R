#' install_packages_ls
#'
#' This function installs packages if not yet installed.
#'
#' @param packages Vector of package names (character). Default is the vector of packages needed for Package RaschKit.
#' @examples
#' install_packages_ls(packages='openxlsx')

install_packages_ls <- function(packages=c('tidyverse', 'conquestr', 'rlang', 'ggthemes', 'ggrepel', 'patchwork', 'gdata', 'janitor', 'data.table', 'RColorBrewer', 'fs')){
    lapply(packages, function(x){
        if (!require(x, character.only=TRUE)){
            install.packages(x)
            library(x, character.only=TRUE)
        }
    })
}
