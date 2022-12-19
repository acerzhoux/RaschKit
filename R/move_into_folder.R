#' move_into_folder
#'
#' This function checks whether files related to 'test' exist. If so, 
#' it establishes a new folder and moves files inside. This is associated 
#' with test named 'test'.
#'
#' @param folder Place to find CQ output files. Default is 'output' folder.
#' @param test Name of the test.
#' @examples
#' move_into_folder(test='randomData1')
#' @export

move_into_folder <- function(folder=here::here('output'), test){
    # get folder and file names in folder
    file_frd <- list.files(folder)
    file_frd_test <- list.files(folder, pattern=test)
    file_test <- file_frd_test[str_detect(file_frd_test, '\\.')]

    if (!any(str_detect(file_test, test))){
        # No test files: Return
        return()
    } else {
        # set new folder name
        folder_test <- file_frd_test[!str_detect(file_frd_test, '\\.')]
        folder_test_run <- folder_test[str_detect(folder_test, '_Run_')]
        if (length(folder_test_run)==0){
            frd_test_new <- here::here(folder, paste0(test, '_Run_1'))
        } else {
            frd_ls <- folder_test_run %>%
                    strsplit('_')
            frd_last <- frd_ls[[length(frd_ls)]]
            run_recent <- frd_last[[length(frd_last)]] %>%
                as.numeric()
            frd_test_new <- here::here(folder, paste0(test, '_Run_', run_recent + 1))
        }
        # create folder
        dir.create(frd_test_new)

        # get full file names related to 'test'
        file_frd_test_full <- list.files(folder, pattern=test, full.names=TRUE)
        file_test_full <- file_frd_test_full[str_detect(file_frd_test_full, '\\.')]

        # move files
        map(file_test_full, ~fs::file_move(path=.x, new_path=frd_test_new))
    }
}
