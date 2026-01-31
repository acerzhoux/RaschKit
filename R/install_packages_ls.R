#' install_packages_ls
#'
#' This function installs packages if not yet installed.
#'
#' @param packages Vector of package names (character). Default is the vector
#' of packages needed for Package RaschKit.
#' @examples
#' install_packages_ls(packages='openxlsx')
#' @export

install_packages_ls <- function(packages=c('conquestr', 'rlang', 'bookdown',
                                           'ggthemes', 'ggrepel', 'patchwork', 'rmarkdown',
                                           'gdata', 'janitor', 'knitr',
                                           'RColorBrewer', 'fs', 'lazyeval',
                                           'writexl', 'ggpubr', 'qpdf', 'tidyverse',
                                           'openxlsx','flextable')){
    lapply(packages, function(x){
        if (!require(x, character.only=TRUE)){
            install.packages(x)
            library(x, character.only=TRUE)
        }
    })
}
