###############################################################################
### harmonise CN8
### 12-11-2021
### Christoph Baumgartner & Janette Walde
###############################################################################

#' @importFrom stats na.omit
#' @importFrom utils read.table
#' @export history_matrix_cn8

history_matrix_cn8 <- function(b, e, c1 = 1988, c2 = 2022, progress = TRUE) {
  #########################
  ### input check
  #########################
  if(e > c2) {
    stop("The entered last year of interest (e) is greater than the last year of the concordance list (c2). Please correct.")
  }
  if(b >= e) {
    stop("The entered last year of interest (e) is smaller than the first year of interest (b). Please correct.")
  }
  if(b < c1) {
    stop("The entered first year of interest (b) is smaller than the first year of the concordance list (c1). Please correct.")
  }
  if (length(b) != 1 | !b%%1 == 0 | b > as.integer(substr(date(), start = 21, stop = 24))) {
    stop(paste0("The entered first year of interest (b) has to be a single integer value, which has to be smaller than ", as.integer(substr(date(), start = 21, stop = 24)), ". Please correct."))
  }
  if (length(e) != 1 | !e%%1 == 0 | e > as.integer(substr(date(), start = 21, stop = 24))) {
    stop(paste0("The entered last year of interest (e) has to be a single integer value, which has to be smaller than ", as.integer(substr(date(), start = 21, stop = 24)), ". Please correct."))
  }
  if (length(c1) != 1 | !c1%%1 == 0 | c1 > as.integer(substr(date(), start = 21, stop = 24))) {
    stop(paste0("The entered first year of year of the concordance list (c1) has to be a single integer value, which has to be smaller than ", as.integer(substr(date(), start = 21, stop = 24)), ". Please correct."))
  }
  if (length(c2) != 1 | !c2%%1 == 0 | c2 > as.integer(substr(date(), start = 21, stop = 24))) {
    stop(paste0("The entered last year of year of the concordance list (c2) has to be a single integer value, which has to be smaller than ", as.integer(substr(date(), start = 21, stop = 24)), ". Please correct."))
  }
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/CN8/CN8_", b, ".rds"))) {
    stop("There is no data avilable for the first year of interest (b). Consider changing the time periode, or alter data (-> get_data_directory()).")
  }
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/CN8/CN8_", e, ".rds"))) {
    stop("There is no data avilable for the last year of interest (e). Consider changing the time periode, or alter data (-> get_data_directory()).")
  }
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/CN8/CN8_concordances_", c1,"_", c2, ".csv"))) {
    stop("There is no data avilable for the chosen years for the concordance list (c1, c2). Consider changing the time periode, or alter data (-> get_data_directory()).")
  }

  ###############################################################################
  ### get data
  ###############################################################################

  # was the function called by another fct already
  fcalls <- sys.nframe()
  if(fcalls == 2) {
    mod_part <- 4
  } else if (fcalls == 3) {
    mod_part <- 5
  } else {
    mod_part <- 2
  }

  numb_years <- e - b  # length of period of interest = numb_years + 1

  # load all lists of product codes in the respective years
  for(i in 0:numb_years) {
    assign("CN_temp", readRDS(paste0(system.file("extdata", package = "harmonizer"), "/CN8/CN8_", b + i, ".rds")))
    CN_temp$group <- gsub(" ", "", CN_temp$group, fixed = TRUE)
    colnames(CN_temp) <- b + i
    assign(paste0("CN_", b + i), CN_temp)
    rm(CN_temp)
  }

  # load the concordance list from c1 to c2
  updates <- read.table(paste0(system.file("extdata", package = "harmonizer"), "/CN8/CN8_concordances_", c1,"_", c2,".csv"),
                        header = TRUE, sep = ";", colClasses = "character", na.strings = c("", " ", "NA", "-"))
  updates$from <- as.integer(updates$from)
  updates$to <- as.integer(updates$to)
  updates$obsolete <- gsub(" ", "", updates$obsolete, fixed = TRUE)
  updates$new <- gsub(" ", "", updates$new, fixed = TRUE)

  updates$obsolete[nchar(updates$obsolete) == 0] <- NA
  updates$new[nchar(updates$new) == 0] <- NA
  # Remove all white spaces
  # updates$obsolete <- replace(updates$obsolete, grepl("^\\s*$", updates$obsolete) == TRUE, NA)
  # updates$new <- replace(updates$new, grepl("^\\s*$", updates$new) == TRUE, NA)

  ###############################################################################
  ### main function
  ###############################################################################

  # create a dataframe for capturing all CN8 codes
  # as first column put the codes of the year b
  CN_over_time <- eval(parse(text = paste0("CN_", b)))
  colnames(CN_over_time) <- paste0("CN8_", b)

  # loop over all years
  # compare all years pairwise where x is at time t
  # and y at time t + 1
  # x is from the dataframe "CN_over_time"
  j <- 0
  if (progress) {
   print(paste0("Work in progress... Part 1/", mod_part,": 0%"))
  }
  while(j < numb_years) {

    assign("x", eval(parse(text = paste0("CN_over_time[", j + 1, "]"))))
    assign("y", eval(parse(text = paste0("CN_", b + j + 1))))

    # create dataframe for all "regular" changed codes
    CN_over_time_temp <- data.frame(
      "CN8" = rep(NA, times = nrow(eval(parse(text = paste0("CN_", b)))))
    )

    # create dataframe with product codes which appeared multiple times in the correspondence list (split/merge codes)
    multiple_codes <- data.frame(
      "CN8" <- character()
    )


    # loop over all codes, keep year fixed
    for(i in 1:nrow(x)) {

      current_CN8 <- x[i, 1]

      # check NA (check whether a product code was dropped)
      if (is.na(current_CN8) & (j > 0)) {
        # k is the previous col
        k <- j
        obs_NA <- current_CN8
        while(is.na(obs_NA)) { # Check whether the code is available in previous years?
          # Take the former code if available and
          # look whether it pops up again.
          obs_NA <- CN_over_time[i, k]
          k <- k - 1
        }
        current_CN8 <- obs_NA
      }

      # Case A: check if the code did not change
      if (current_CN8 %in% y[[1]]) {
        CN_over_time_temp[i, 1] <- current_CN8

        # Case A1: check if the code merged or split in addition
        if (current_CN8 %in% updates$obsolete[updates$from == (b + j)]) {

          idx_new_codes <- which(current_CN8 == updates$obsolete & updates$from == b + j)
          n_new_codes <- length(idx_new_codes)
          new_codes <- updates$new[idx_new_codes]

          # if there are multiple replacements of one code,
          # n new rows are added for the new codes

          previous_codes <- CN_over_time[i, ]
          rownames(previous_codes) <- NULL
          new_df <- cbind(previous_codes, new_codes)
          colnames(new_df)[1:(j + 1)] <- colnames(as.data.frame(CN_over_time))
          colnames(new_df)[j + 2] <- paste0("CN8_", b + j + 1)

          multiple_codes <- rbind(multiple_codes, new_df, make.row.names = FALSE)
        }
        next
      }

      # Case B: check if the code was changed
      if (current_CN8 %in% updates$obsolete[updates$from == (b + j)]) {

        idx_new_codes <- which(current_CN8 == updates$obsolete & updates$from == b + j)
        n_new_codes <- length(idx_new_codes)
        new_codes <- updates$new[idx_new_codes]

        # Case B1: if there are multiple replacements of one code (n > 1),
        # n - 1 new rows are added for new codes
        if (n_new_codes > 1) {
          CN_over_time_temp[i, 1] <- new_codes[1]
          previous_codes <- CN_over_time[i, ]
          rownames(previous_codes) <- NULL
          new_df <- cbind(previous_codes, new_codes[-1])
          colnames(new_df)[1:(j + 1)] <- colnames(as.data.frame(CN_over_time))
          colnames(new_df)[j + 2] <- paste0("CN8_", b + j + 1)

          multiple_codes <- rbind(multiple_codes, new_df, make.row.names = FALSE)

        } else {
          # Case B2: the code was not split and has only one replacement
          CN_over_time_temp[i, 1] <- new_codes
        }
        next
      }

      # Case C: the code was dropped completely
      CN_over_time_temp[i, 1] <- NA
    }

    # add all "regular" changed codes
    CN_over_time <- cbind(CN_over_time, CN_over_time_temp)
    colnames(CN_over_time)[j + 2] <- paste0("CN8_", b + j + 1)

    # add all codes which appeared multiple times in the obsolete list
    CN_over_time <- rbind(CN_over_time, multiple_codes, make.row.names = FALSE)

    # Case D: add codes which were created in t + 1
    # two possibilities to create new codes:  1 - exists in all codes list in t + 1
    #                                         2 - created only according to change-list
    # and fill all previous years with NA
    # 1
    assign(paste0("CN8f_", b + j + 1), y[!y[[1]] %in%  CN_over_time[[j + 2]], 1])
    created_codes_df <- as.data.frame(eval(parse(text = paste0("CN8f_", b + j + 1))))
    current_changelist <- updates[updates$from == b + j & updates$to == b + j + 1, c("new","obsolete")]
    colnames(created_codes_df)[1] <- paste(b + j)
    old_col <- which(colnames(current_changelist) == "obsolete")
    colnames(current_changelist)[old_col] <- paste(b + j)
    # 2
    #All new codes according to the change list
    temp1 <- na.omit(current_changelist[is.na(current_changelist[[old_col]]), "new"])
    temp <- data.frame(temp1[!(temp1 %in% CN_over_time[[j + 2]])])
    rm(temp1)

    colnames(temp) <- paste(b + j)
    created_codes_df <- rbind(created_codes_df, temp)

    colnames(created_codes_df)[1] <- paste0("CN8_", b + j + 1)
    for (r in 1:(j + 1)) {
      assign(paste0("CN8f_", b + r - 1), rep(NA, times = nrow(created_codes_df)))
      created_codes_df <- as.data.frame(cbind(created_codes_df, eval(parse(text = paste0("CN8f_", b + r - 1)))))
      colnames(created_codes_df)[r + 1] <- paste0("CN8_", b + r - 1)
    }
    CN_over_time <- rbind(CN_over_time, created_codes_df, make.row.names = FALSE)

    nacheck <- which(rowSums(is.na(CN_over_time)) == ncol(CN_over_time))
    if (length(nacheck) != 0) {
      CN_over_time <- CN_over_time[-nacheck,]
    }
    rm(nacheck)

    j <- j + 1
    if (progress) {
      print(paste0("Work in progress... Part 1/", mod_part,": ", round(j/numb_years, 3) * 100, "%"))
    }
  }

  CN_over_time <- CN_over_time[!duplicated(CN_over_time), ]

  ###############################################################################
  ### add variable "flag" & "flag_year"
  ###############################################################################

  if (progress) {
    print(paste0("Work in progress... Part 2/", mod_part,": 0%"))
  }

  # flag == 1 indicates that this code did not change in terms of CN8 notation
  # but was split or merged at least once in the time record (Case A+A1)
  # flag_year indicates the year this split/merge appeared
  # flag == 2 indicates that this code is new or was dropped

  CN_over_time$flag <- rep(0, times = nrow(CN_over_time))
  CN_over_time$flagyear <- rep(NA, times = nrow(CN_over_time))

  ### flag == 1
  # j indicates the loop variable over the years (starting at t = 0)
  # i indicates the current row of the dataframe

  # get all rows where first and last year codes do not differ
  # flag_rows_d <- which(CN_over_time[[1]] == CN_over_time[[numb_years + 1]])
  flag_rows_d <- apply(CN_over_time[1:(numb_years + 1)], 1, function(x) all(x[1] == x))
  names(flag_rows_d) <- NULL
  flag_rows_d <- which(flag_rows_d)
  # get all rows where codes appear multiple times
  flag_rows_m <- apply(CN_over_time[1:(numb_years + 1)], 2, function(x) unique(c(which(duplicated(x)), which(duplicated(x, fromLast = TRUE)))))
  flag_rows_m <- unique(unlist(flag_rows_m))
  # check for which rows both conditions hold
  flag_rows <- intersect(flag_rows_m, flag_rows_d)

  # only continue if any exist
  if(length(flag_rows) > 0) {
    # set flag == 1
    CN_over_time$flag[flag_rows] <- 1
    ## find flag years
    # distinguish between merges and splits
    # splits have to appear more than once in the first year
    multiple_rows1 <- unique(c(which(duplicated(CN_over_time[, 1])),
                               which(duplicated(CN_over_time[, 1], fromLast = TRUE))))
    # splits are not allowed to appear than once in the last year
    multiple_rows2 <- unique(intersect(which(!duplicated(CN_over_time[, (numb_years + 1)])),
                                       which(!duplicated(CN_over_time[, (numb_years + 1)], fromLast = TRUE))))
    multiple_rows <- intersect(multiple_rows1, multiple_rows2)
    # merges
    merges <- CN_over_time[setdiff(flag_rows, multiple_rows), 1]
    if(length(merges) > 0) {
      # get all rows with the merged codes
      for(i in 1:length(merges)) {
        row_nbr <- 1
        j <- 0
        # for merges do it as long as codes differ
        while(row_nbr == 1) {
          j <- j + 1
          row_nbr <- nrow(CN_over_time[which(merges[i] == CN_over_time[[j]] & merges[i] == CN_over_time[[numb_years + 1]]), ])
        }
        CN_over_time$flagyear[setdiff(flag_rows, multiple_rows)[i]] <- as.numeric(substr(colnames(CN_over_time[j]), start = 5, stop = 8))
      }
    }
    # splits
    splits <- CN_over_time[intersect(flag_rows, multiple_rows), 1]
    if(length(splits) > 0) {
      # get all rows with the split codes
      for(i in 1:length(splits)) {
        row_nbr <- 2
        j <- 0
        # for splits do it as long as codes differ no longer
        while(row_nbr != 1) {
          j <- j + 1
          row_nbr <- nrow(CN_over_time[which(splits[i] == CN_over_time[[j]] & splits[i] == CN_over_time[[1]]), ])
        }
        CN_over_time$flagyear[intersect(flag_rows, multiple_rows)[i]] <- as.numeric(substr(colnames(CN_over_time[j]), start = 5, stop = 8))
      }
    }
  }

  if (progress) {
    print(paste0("Work in progress... Part 2/", mod_part,": 50%"))
  }

  ### flag == 2

  ## find all history cols
  hist_cols <- 1:(numb_years + 1)
  ## find any NA in the history cols
  na_rows <- apply(CN_over_time[, hist_cols], 1, anyNA)
  na_rows_new <- is.na(CN_over_time[[1]])
  na_rows_dropped <- is.na(CN_over_time[[(numb_years + 1)]])
  ## only continue if any NAs exist
  if(any(na_rows)) {
    ## set flag == 2
    CN_over_time$flag[na_rows] <- 2
    ## find flag years
    flag_cols_new <- !apply(CN_over_time[na_rows_new, hist_cols], 2, is.na)
    flag_cols_dropped <- apply(CN_over_time[na_rows_dropped, hist_cols], 2, is.na)
    # check how many NAs there are
    # 8 cases:
    # I:    multiple drops    and     multiple news
    # II:   multiple drops    and     one new
    # III:  one drop          and     multiple news
    # IV:   one drop          and     one new
    # V:    one drop          and     no new
    # VI:   no drop           and     one new
    # VII:  multiple drops    and     no new
    # VIII: no drop           and     multiple new

    # case I
    if(length(flag_cols_dropped) > (numb_years + 1) & length(flag_cols_new) > (numb_years + 1)) {
      # find only cols which do not have a NA
      flag_cols_new <- apply(flag_cols_new, 1, which, simplify = FALSE)
      flag_cols_dropped <- apply(flag_cols_dropped, 1, which, simplify = FALSE)
      # take only first element, i.e. first year where the code poped up
      flag_cols_new <- lapply(flag_cols_new, min)
      flag_cols_dropped <- lapply(flag_cols_dropped, min)
      # transform list into vector
      flag_cols_new <- unlist(flag_cols_new)
      flag_cols_dropped <- unlist(flag_cols_dropped)
      # correct with - 1, since the finds first NA, i.e. the code dropped in the previous year
      flag_cols_dropped <- unlist(flag_cols_dropped) - 1

      # get colnames and therefore years
      flag_cols_new <- as.numeric(substr(colnames(CN_over_time[flag_cols_new]), start = 5, stop = 8))
      flag_cols_dropped <- as.numeric(substr(colnames(CN_over_time[flag_cols_dropped]), start = 5, stop = 8))
      ## set flag years
      CN_over_time$flagyear[na_rows_new] <- flag_cols_new
      CN_over_time$flagyear[na_rows_dropped] <- flag_cols_dropped
    }

    # case II
    else if(length(flag_cols_dropped) > (numb_years + 1) & length(flag_cols_new) == (numb_years + 1)) {
      # find only cols which do not have a NA
      flag_cols_new <- which(flag_cols_new)
      flag_cols_dropped <- apply(flag_cols_dropped, 1, which, simplify = FALSE)
      # take only first element, i.e. first year where the code poped up
      flag_cols_new <- flag_cols_new[1]
      flag_cols_dropped <- lapply(flag_cols_dropped, min)
      # transform list into vector
      flag_cols_dropped <- unlist(flag_cols_dropped)
      # correct with - 1, since the finds first NA, i.e. the code dropped in the previous year
      flag_cols_dropped <- unlist(flag_cols_dropped) - 1

      # get colnames and therefore years
      flag_cols_new <- as.numeric(substr(colnames(CN_over_time[flag_cols_new]), start = 5, stop = 8))
      flag_cols_dropped <- as.numeric(substr(colnames(CN_over_time[flag_cols_dropped]), start = 5, stop = 8))
      ## set flag years
      CN_over_time$flagyear[na_rows_new] <- flag_cols_new
      CN_over_time$flagyear[na_rows_dropped] <- flag_cols_dropped
    }

    # case III
    else if(length(flag_cols_dropped) == (numb_years + 1) & length(flag_cols_new) > (numb_years + 1)) {
      # find only cols which do not have a NA
      flag_cols_dropped <- which(flag_cols_dropped)
      flag_cols_new <- apply(flag_cols_new, 1, which, simplify = FALSE)
      # take only first element, i.e. first year where the code poped up
      flag_cols_dropped <- flag_cols_dropped[1]
      flag_cols_new <- lapply(flag_cols_new, min)
      # transform list into vector
      flag_cols_new <- unlist(flag_cols_new)
      # correct with - 1, since the finds first NA, i.e. the code dropped in the previous year
      flag_cols_dropped <- flag_cols_dropped - 1

      # get colnames and therefore years
      flag_cols_new <- as.numeric(substr(colnames(CN_over_time[flag_cols_new]), start = 5, stop = 8))
      flag_cols_dropped <- as.numeric(substr(colnames(CN_over_time[flag_cols_dropped]), start = 5, stop = 8))
      ## set flag years
      CN_over_time$flagyear[na_rows_new] <- flag_cols_new
      CN_over_time$flagyear[na_rows_dropped] <- flag_cols_dropped
    }

    # case IV
    else if(length(flag_cols_dropped) == (numb_years + 1) & length(flag_cols_new) == (numb_years + 1)) {
      # find only cols which do not have a NA
      flag_cols_dropped <- which(flag_cols_dropped)
      flag_cols_new <- which(flag_cols_new)
      # take only first element, i.e. first year where the code poped up
      flag_cols_dropped <- flag_cols_dropped[1]
      flag_cols_new <- flag_cols_new[1]
      # correct with - 1, since the finds first NA, i.e. the code dropped in the previous year
      flag_cols_dropped <- flag_cols_dropped - 1

      # get colnames and therefore years
      flag_cols_new <- as.numeric(substr(colnames(CN_over_time[flag_cols_new]), start = 5, stop = 8))
      flag_cols_dropped <- as.numeric(substr(colnames(CN_over_time[flag_cols_dropped]), start = 5, stop = 8))
      ## set flag years
      CN_over_time$flagyear[na_rows_new] <- flag_cols_new
      CN_over_time$flagyear[na_rows_dropped] <- flag_cols_dropped
    }

    # case IV
    else if(length(flag_cols_dropped) == (numb_years + 1) & length(flag_cols_new) == 0) {
      # find only cols which do not have a NA
      flag_cols_dropped <- which(flag_cols_dropped)
      # take only first element, i.e. first year where the code poped up
      flag_cols_dropped <- flag_cols_dropped[1]
      # correct with - 1, since the finds first NA, i.e. the code dropped in the previous year
      flag_cols_dropped <- flag_cols_dropped - 1

      # get colnames and therefore years
      flag_cols_dropped <- as.numeric(substr(colnames(CN_over_time[flag_cols_dropped]), start = 5, stop = 8))
      ## set flag years
      CN_over_time$flagyear[na_rows_dropped] <- flag_cols_dropped
    }

    # case VI
    else if(length(flag_cols_dropped) == 0 & length(flag_cols_new) == (numb_years + 1)) {
      # find only cols which do not have a NA
      flag_cols_new <- which(flag_cols_new)
      # take only first element, i.e. first year where the code poped up
      flag_cols_new <- flag_cols_new[1]

      # get colnames and therefore years
      flag_cols_new <- as.numeric(substr(colnames(CN_over_time[flag_cols_new]), start = 5, stop = 8))
      ## set flag years
      CN_over_time$flagyear[na_rows_new] <- flag_cols_new
    }


    # case VII
    if(length(flag_cols_dropped) > (numb_years + 1) & length(flag_cols_new) == 0) {
      # find only cols which do not have a NA
      flag_cols_dropped <- apply(flag_cols_dropped, 1, which, simplify = FALSE)
      # take only first element, i.e. first year where the code poped up
      flag_cols_dropped <- lapply(flag_cols_dropped, min)
      # transform list into vector
      flag_cols_dropped <- unlist(flag_cols_dropped)
      # correct with - 1, since the finds first NA, i.e. the code dropped in the previous year
      flag_cols_dropped <- unlist(flag_cols_dropped) - 1

      # get colnames and therefore years
      flag_cols_dropped <- as.numeric(substr(colnames(CN_over_time[flag_cols_dropped]), start = 5, stop = 8))
      ## set flag years
      CN_over_time$flagyear[na_rows_dropped] <- flag_cols_dropped
    }

    # case VIII
    if(length(flag_cols_dropped) == 0 & length(flag_cols_new) > (numb_years + 1)) {
      # find only cols which do not have a NA
      flag_cols_new <- apply(flag_cols_new, 1, which, simplify = FALSE)
      # take only first element, i.e. first year where the code poped up
      flag_cols_new <- lapply(flag_cols_new, min)
      # transform list into vector
      flag_cols_new <- unlist(flag_cols_new)

      # get colnames and therefore years
      flag_cols_new <- as.numeric(substr(colnames(CN_over_time[flag_cols_new]), start = 5, stop = 8))
      ## set flag years
      CN_over_time$flagyear[na_rows_new] <- flag_cols_new
    }
  }

  if (progress) {
    print(paste0("Work in progress... Part 2/", mod_part,": 100%"))
  }

  return(CN_over_time)
}



