#' install_packages_ls
#'
#' This function installs packages if not yet installed.
#'
#' @param packages Vector of package names (character). Default is the vector
#' of packages needed for Package RaschKit.
#' @examples
#' install_packages_ls(packages='openxlsx')
#' @export

install_packages_ls <- function(packages=c('plyr', 'conquestr', 'rlang', 'bookdown',
                                           'ggthemes', 'ggrepel', 'patchwork', 'rmarkdown',
                                           'gdata', 'janitor', 'data.table', 'knitr',
                                           'RColorBrewer', 'fs', 'here', 'lazyeval',
                                           'writexl', 'ggpubr', 'qpdf', 'tidyverse',
                                           'openxlsx')){
    lapply(packages, function(x){
        if (!require(x, character.only=TRUE)){
            install.packages(x)
            library(x, character.only=TRUE)
        }
    })
}
