#############################################################
### Translate CN8/HS6 into BEC
### 12-11-2021
### Christoph Baumgartner & Janette Walde
#############################################################

#' @importFrom stats na.omit setNames
#' @importFrom utils read.csv read.table
#' @export cn8_to_bec

cn8_to_bec <- function(b, e, historymatrix = NULL, progress = TRUE) {
  #########################
  ### input check
  #########################
  if(b >= e) {
    stop("The entered last year of interest (e) is smaller than the first year of interest (b). Please correct.")
  }
  if (length(b) != 1 | !b%%1 == 0 | b > as.integer(substr(date(), start = 21, stop = 24))) {
    stop(paste0("The entered first year of interest (b) has to be a single integer value, which has to be smaller than ", as.integer(substr(date(), start = 21, stop = 24)), ". Please correct."))
  }
  if (length(e) != 1 | !e%%1 == 0 | e > as.integer(substr(date(), start = 21, stop = 24))) {
    stop(paste0("The entered last year of interest (e) has to be a single integer value, which has to be smaller than ", as.integer(substr(date(), start = 21, stop = 24)), ". Please correct."))
  }
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/CN8/CN8_", b, ".rds"))) {
    stop("There is no data avilable for the first year of interest (b). Consider changing the time periode, or alter data (-> get.data.directory()).")
  }
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/CN8/CN8_", e, ".rds"))) {
    stop("There is no data avilable for the first year of interest (e). Consider changing the time periode, or alter data (-> get.data.directory()).")
  }

  #########################
  ### history matrix check
  #########################
  if (is.null(historymatrix)) {
    mod_part <- 3
    CN8_over_time <- history_matrix_cn8(b = b, e = e, progress = progress)
  } else {
    if (!is.data.frame(historymatrix)) {
      stop("The entered history matrix is not a dataframe. Please correct.")
    }
    if (!any(grep("CN8_", colnames(historymatrix)))) {
      stop("Column names of the history matrix are not correct. Please correct.")
    }
    if (ncol(historymatrix) != (e - b + 3)) {
      stop(paste0("The entered history matrix has the wrong number of columns. It has ", ncol(historymatrix), ", while ", (e - b + 3), " are needed for the entered time period. Please correct."))
    }
    if (length(grep("CN8_", colnames(historymatrix))) != (e - b + 1)) {
      stop("The entered history matrix does not have enough columns which contain CN8 codes. It has ", length(grep("CN8_", colnames(historymatrix))), " while ", (e - b + 1), " are needed for the entered time period. Please correct.")
    }
    if (!all(grep("CN8_", colnames(historymatrix[1:(e - b + 1)])) == seq(from = 1, to = (e - b + 1)))) {
      stop("The structure of the entered history matrix is not correct. The first columns have to containt the CN8 codes. Please correct.")
    }
    if (!all(seq(from = b, to = e) == as.integer(substr(colnames(historymatrix[1:(ncol(historymatrix) - 2)]), start = 5, stop = 8)))) {
      stop("The entered history matrix does not match the given time period. Please correct.")
    }
    mod_part <- 1
    CN8_over_time <- historymatrix
  }

  #############################################################
  ### get data
  #############################################################

  # get all changes between HS6 and BEC as separate dataframes
  filenames <- list.files(paste0(system.file("extdata", package = "harmonizer"), "/HS6toBEC"), pattern="*.csv", full.names=TRUE)
  correspondence_lists <- lapply(filenames, read.csv)
  for(i in 1:length(correspondence_lists)) {
    # unlist as dataframe
    assign(paste0("correspondence_", i), as.data.frame(correspondence_lists[i], responseName = c("HS6", "BEC")))
    # split up in two cols
    assign(paste0("correspondence_", i),
           data.frame(do.call("rbind", strsplit(as.character(eval(parse(text = paste0("correspondence_", i, "[[1]]")))),
                                                ";", fixed = TRUE))), )
    # rename cols
    assign(paste0("correspondence_", i), setNames(eval(parse(text = paste0("correspondence_", i))), c("HS", "BEC")))
  }

  # create one dataframe which contains all unique HS6 codes
  assign("correspondence_BEC", eval(parse(text = "correspondence_1")))
  for (i in 2:length(correspondence_lists)) {
    correspondence_BEC <- rbind(correspondence_BEC, eval(parse(text = paste0("correspondence_", i))))
  }

  correspondence_BEC$HS <- gsub(".", "", correspondence_BEC$HS, fixed = TRUE)
  correspondence_BEC$HS <- gsub(" ", "", correspondence_BEC$HS, fixed = TRUE)
  correspondence_BEC$HS[nchar(correspondence_BEC$HS) == 5] <- paste0("0", correspondence_BEC$HS[nchar(correspondence_BEC$HS) == 5])
  correspondence_BEC <- correspondence_BEC[!duplicated(correspondence_BEC$HS),]

  #############################################################
  ### main function
  #############################################################

  ### get unique CN8 codes
  if (progress) {
    print(paste0("Work in progress... Part ", mod_part, "/" , mod_part + 1,": 0%"))
  }
  # Get all unique product codes over time
  unique_codes <- unique(CN8_over_time[[1]])
  for (i in 2:(e - b + 1)) {
    x <- unique(CN8_over_time[[i]])
    x <- x[!x %in% unique_codes]
    unique_codes <- c(x, unique_codes)
  }
  unique_codes <- na.omit(unique_codes)
  if (progress) {
    print(paste0("Work in progress... Part ", mod_part, "/" , mod_part + 1,": 100%"))
  }

  ### extract HS6 from CN8
  HS6_codes <- substr(unique_codes, start = 1, stop = 6)

  CN8_to_BEC <- data.frame(
    "CN8" = unique_codes,
    "HS6" = HS6_codes,
    "BEC" = rep(NA, times = length(unique_codes)),
    "BEC_agr" = rep(NA, times = length(unique_codes))
  )

  if (progress) {
    print(paste0("Work in progress... Part ", mod_part + 1, "/" , mod_part + 1,": 0%"))
  }
  ### get the corresponding BEC from the list
  for(i in 1:nrow(CN8_to_BEC)) {
    if(CN8_to_BEC[i, "HS6"] %in% correspondence_BEC$HS) {
      CN8_to_BEC[i, "BEC"] <- correspondence_BEC$BEC[CN8_to_BEC[i, "HS6"] == correspondence_BEC$HS]
    } else {
      CN8_to_BEC[i, "BEC"] <- NA
    }
  }

  BEC_agr <- substr(CN8_to_BEC$BEC, start = 1, stop = 1)
  CN8_to_BEC$BEC_agr <- BEC_agr
  rownames(CN8_to_BEC) <- NULL

  if (progress) {
    print(paste0("Work in progress... Part ", mod_part + 1, "/" , mod_part + 1,": 100%"))
  }

  return(CN8_to_BEC)
}
