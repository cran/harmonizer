#############################################################
### translate PC8 into BEC
### 12-11-2021
### Christoph Baumgartner & Janette Walde
#############################################################

#' @importFrom stats na.omit setNames complete.cases
#' @importFrom utils read.csv2
#' @export pc8_to_bec

pc8_to_bec <- function(b, e, historymatrix = NULL, progress = TRUE) {
  #########################
  ### input check
  #########################
  if(b >= e) {
    stop("The entered last year of interest (e) is smaller than the first year
            of interest (b). Please correct.")
  }
  if (length(b) != 1 | !b%%1 == 0 | b > as.integer(substr(date(), start = 21, stop = 24))) {
    stop(paste0("The entered first year of intrest (b) has to be a single integer value, which has to be smaller than ", as.integer(substr(date(), start = 21, stop = 24)), ". Please correct."))
  }
  if (length(e) != 1 | !e%%1 == 0 | e > as.integer(substr(date(), start = 21, stop = 24))) {
    stop(paste0("The entered last year of intrest (e) has to be a single integer value, which has to be smaller than ", as.integer(substr(date(), start = 21, stop = 24)), ". Please correct."))
  }
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/PC8/PC8_", b, ".rds"))) {
    stop("There is no data avilable for the first year of intrest (b). Consider changing the time periode, or alter data (-> get.data.directory()).")
  }
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/PC8/PC8_", e, ".rds"))) {
    stop("There is no data avilable for the first year of intrest (e). Consider changing the time periode, or alter data (-> get.data.directory()).")
  }
  #########################
  ### history matrix check
  #########################
  if (is.null(historymatrix)) {
    mod_part <- 3
    PC8_over_time <- history_matrix_pc8(b = b, e = e, progress = progress)
  } else {
    if (!is.data.frame(historymatrix)) {
      stop("The entered history matrix is not a dataframe. Please correct.")
    }
    if (!any(grep("PC8_", colnames(historymatrix)))) {
      stop("Column names of the history matrix are not correct. Please correct.")
    }
    if (ncol(historymatrix) != (e - b + 3)) {
      stop(paste0("The entered history matrix has the wrong number of colums. It has ", ncol(historymatrix), ", while ", (e - b + 3), " are needed for the entered time period. Please correct."))
    }
    if (length(grep("PC8_", colnames(historymatrix))) != (e - b + 1)) {
      stop("The entered history matrix does not have enough columns which contain PC8 codes. It has ", length(grep("PC8_", colnames(historymatrix))), " while ", (e - b + 1), " are needed for the entered time period. Please correct.")
    }
    if (!all(grep("PC8_", colnames(historymatrix[1:(e - b + 1)])) == seq(from = 1, to = (e - b + 1)))) {
      stop("The structure of the entered history matrix is not correct. The first columns have to containt the PC8 codes. Please correct.")
    }
    if (!all(seq(from = b, to = e) == as.integer(substr(colnames(historymatrix[1:(ncol(historymatrix) - 2)]), start = 5, stop = 8)))) {
      stop("The entered history matrix does not match the given time period. Please correct.")
    }
    mod_part <- 1
    PC8_over_time <- historymatrix
  }

  #############################################################
  ### get data
  #############################################################

  # get all changes between HS6 and BEC as separate dataframes
  filenames <- list.files(paste0(system.file("extdata", package = "harmonizer"), "/HS6toBEC"), pattern="*.csv", full.names=TRUE)
  # check for which years files exist
  available_years <- substr(filenames,(nchar(filenames)+1)-15,nchar(filenames))
  available_years <- regmatches(available_years, gregexpr("[[:digit:]]+", available_years))
  available_years <- unlist(available_years)
  # select only needed HStoBEC files
  needed_files <- paste0("HS", b:e, "toBEC")
  needed_files <- sapply(needed_files, FUN = function(x) {length(grep(x, filenames)) > 0})
  needed_files <- names(needed_files)[needed_files]
  needed_files <- sapply(needed_files, FUN = function(x) {grep(x, filenames)})
  # at least one file has to be selected
  # workaround if b > max(available_years) or e < min(available_years)
  if(length(needed_files) == 0) {
    if(e < min(available_years)) {
      needed_files <- 1
    } else if(b > max(available_years)) {
      needed_files <- length(filenames)
    } else {
      available_years <- as.integer(available_years)
      needed_files <- available_years > b & available_years > e
      needed_files <- unique(c(which(needed_files) - 1, which(needed_files)))
    }
  }

  # check if only Rev. 5 of BEC, that is year >= 2012 is needed
  # if so reduce to all files >= 2012
  if (any(as.integer(available_years[needed_files]) >= 2012 & e >= 2012)) {
    needed_files <- needed_files[as.integer(available_years[needed_files]) >= 2012]
  } else if (any(as.integer(available_years[needed_files]) >= 2012) & e < 2012) {
    needed_files <- needed_files[as.integer(available_years[needed_files]) < 2012]
  }

  correspondence_lists <- lapply(filenames[needed_files], read.csv2)
  for(i in 1:length(correspondence_lists)) {
    # unlist as dataframe
    assign(paste0("correspondence_", i), as.data.frame(correspondence_lists[i]))
    # # split up in two cols
    # assign(paste0("correspondence_", i),
    #        data.frame(do.call("rbind", strsplit(as.character(eval(parse(text = paste0("correspondence_", i, "[[1]]")))),
    #                                             ";", fixed = TRUE))) )
    # # rename cols
    # assign(paste0("correspondence_", i), setNames(eval(parse(text = paste0("correspondence_", i))), c("HS", "BEC")))
  }

  # create one dataframe which contains all unique HS6 codes
  assign("correspondence_BEC", eval(parse(text = "correspondence_1")))
  if(exists("correspondence_2")) {
    for (i in 2:length(correspondence_lists)) {
      correspondence_BEC <- rbind(correspondence_BEC, eval(parse(text = paste0("correspondence_", i))))
    }
  }

  correspondence_BEC$HS <- gsub(".", "", correspondence_BEC$HS, fixed = TRUE)
  correspondence_BEC$HS <- gsub(" ", "", correspondence_BEC$HS, fixed = TRUE)
  correspondence_BEC$HS[nchar(correspondence_BEC$HS) == 5] <- paste0("0", correspondence_BEC$HS[nchar(correspondence_BEC$HS) == 5])
  correspondence_BEC <- correspondence_BEC[!duplicated(correspondence_BEC$HS),]

  # import 'PC8_to_CN8' lists for the years of interest
  # this lists are needed to translate PC8 codes into HS6 codes

  correspondence_CN8 <- data.frame()
  for(i in 0:(e - b)) {
    assign(paste0("PC8_CN8_", b + i), readRDS(paste0(system.file("extdata", package = "harmonizer"), "/PC8/PC8_CN8_", b + i, ".rds")))
    correspondence_CN8 <- rbind(correspondence_CN8, eval(parse(text = paste0("PC8_CN8_", b + i))))
  }

  correspondence_CN8 <- correspondence_CN8[!duplicated(correspondence_CN8),]
  # drop all NAs, because those codes can not be translated anyways
  correspondence_CN8 <- correspondence_CN8[which(complete.cases(correspondence_CN8)), ]
  rownames(correspondence_CN8) <- NULL



  #############################################################
  ### main function
  #############################################################

  # Get all unique product codes over time
  unique_codes <- unique(PC8_over_time[[1]])

  if (progress) {
    print(paste0("Work in progress... Part ", mod_part, "/" , mod_part + 1,": 0%"))
  }

  for (i in 2:(e - b + 1)) {
    x <- unique(PC8_over_time[[i]])
    x <- x[!x %in% unique_codes]
    unique_codes <- c(x, unique_codes)
  }
  unique_codes <- na.omit(unique_codes)

  PC8_to_BEC <- data.frame(
    PC8 = unique_codes,
    HS6 = rep(NA, times = length(unique_codes)),
    BEC = rep(NA, times = length(unique_codes)),
    BEC_agr = rep(NA, times = length(unique_codes))
  )

  if (progress) {
    print(paste0("Work in progress... Part ", mod_part, "/" , mod_part + 1,": 100%"))
  }
  # create dataframe for product codes which appeared multiple times (more than one PC8 correspond to the same CN8)
  multiple_codes <- data.frame()

  if (progress) {
    print(paste0("Work in progress... Part ", mod_part + 1, "/" , mod_part + 1,": 0%"))
  }

  # 'i' is indicating the current row number while lopping over data
  for(i in 1:nrow(PC8_to_BEC)) {
    # get current code of row i
    current_PC8 <- PC8_to_BEC[i, "PC8"]

    # extract HS6 from PC8
    current_CN8 <- correspondence_CN8$CNCODE[which(correspondence_CN8$PRCCODE == current_PC8)]
    PC8_to_BEC$HS6[i] <- substr(current_CN8[1], start = 1, stop = 6)

    n_codes <- length(current_CN8)

    if(n_codes > 1) {
      multiple_codes_temp <- data.frame(
        PC8 = rep(current_PC8, times = n_codes - 1),
        HS6 = substr(current_CN8[-1], start = 1, stop = 6),
        BEC = rep(NA, times = n_codes - 1),
        BEC_agr = rep(NA, times = n_codes - 1)
      )

      multiple_codes <- rbind(multiple_codes, multiple_codes_temp)
    }
  }

  PC8_to_BEC <- rbind(PC8_to_BEC, multiple_codes)

  # erase same rows
  PC8_to_BEC <- PC8_to_BEC[!duplicated(PC8_to_BEC), ]

  # erase PC8 codes associated with different BECs
  PC8_to_BEC <- PC8_to_BEC[!PC8_to_BEC$PC8 %in% PC8_to_BEC$PC8[duplicated(PC8_to_BEC$PC8)],]

  ### get the corresponding BEC from the list
  for(i in 1:nrow(PC8_to_BEC)) {
    if(PC8_to_BEC[i, "HS6"] %in% correspondence_BEC$HS) {
      PC8_to_BEC[i, "BEC"] <- correspondence_BEC$BEC[PC8_to_BEC[i, "HS6"] == correspondence_BEC$HS]
    } else {
      PC8_to_BEC[i, "BEC"] <- NA
    }
  }

  PC8_to_BEC$BEC_agr <- substr(PC8_to_BEC$BEC, start = 1, stop = 1)
  rownames(PC8_to_BEC) <- NULL

  if (progress) {
    print(paste0("Work in progress... Part ", mod_part + 1, "/" , mod_part + 1,": 100%"))
  }
  return(PC8_to_BEC)
}
