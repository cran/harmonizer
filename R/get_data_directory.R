###############################################################################
### harmonize CN8
### 12-11-2021
### Christoph Baumgartner & Janette Walde
###############################################################################

#' @export get_data_directory

get_data_directory <- function(path = TRUE, open_explorer = FALSE, show_data = NULL) {

   # get path
   org_path <- system.file("extdata", package = "harmonizer")
   mod_path <- gsub(pattern = "/", replacement = paste("\\\\"), x = org_path)

   # print path in console
   if (path == TRUE) {
      print(org_path)
   }

   # open explorer
   if (sum(grep(" ", mod_path)) > 0) {
      stop("The path contains blanks, therefore no explorer can be opened. Please use the path provided in the console.")
   } else if (open_explorer == TRUE) {
      system2("open", mod_path)
   }

   # show available data
   if (is.null(show_data)) {
      show_data <- NULL
   } else if (show_data == "CN8") {
      list.files(paste0(org_path, "/CN8"))
   } else if (show_data == "HS6") {
      list.files(paste0(org_path, "/HS6"))
   } else if (show_data == "PC8") {
      list.files(paste0(org_path, "/PC8"))
   } else if (show_data == "HS6toBEC") {
      list.files(paste0(org_path, "/HS6toBEC"))
   } else {
      stop("Unkown command for 'show_data'. Please use one of the following values:
           'CN8', 'HS6', 'PC8' or 'HS6toBEC'.")
   }

   return(org_path)
}


