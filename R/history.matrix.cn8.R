###############################################################################
### harmonise CN8
### 12-11-2021
### Christoph Baumgartner & Janette Walde
###############################################################################

#' @importFrom stats na.omit
#' @importFrom utils read.table
#' @export history.matrix.cn8

history.matrix.cn8 <- function(b, e, c1 = 1988, c2 = 2020, progress = TRUE) {
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
    stop("There is no data avilable for the first year of interest (b). Consider changing the time periode, or alter data (-> get.data.directory()).")
  }
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/CN8/CN8_", e, ".rds"))) {
    stop("There is no data avilable for the last year of interest (e). Consider changing the time periode, or alter data (-> get.data.directory()).")
  }
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/CN8/CN8_concordances_", c1,"_", c2, ".csv"))) {
    stop("There is no data avilable for the chosen years for the concordance list (c1, c2). Consider changing the time periode, or alter data (-> get.data.directory()).")
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
                        header = TRUE, sep = ";")
  updates$obsolete <- gsub(" ", "", updates$obsolete, fixed = TRUE)
  updates$new <- gsub(" ", "", updates$new, fixed = TRUE)

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
    # and fill all previous years with NA
    assign(paste0("CN8_", b + j + 1), y[!y[[1]] %in%  CN_over_time[[j + 2]], 1])
    created_codes_df <- as.data.frame(eval(parse(text = paste0("CN8_", b + j + 1))))
    colnames(created_codes_df)[1] <- paste0("CN8_", b + j + 1)
    for (r in 1:(j + 1)) {
      assign(paste0("CN8_", b + r - 1), rep(NA, times = nrow(created_codes_df)))
      created_codes_df <- as.data.frame(cbind(created_codes_df, eval(parse(text = paste0("CN8_", b + r - 1)))))
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

  # flag == 1 indicates that this code did not change in terms of CN8 notation
  # but was split or merged at least once in the time record (Case A+A1)
  # flag_year indicates the year this split/merge appeared

  CN_over_time$flag <- rep(0, times = nrow(CN_over_time))
  CN_over_time$flagyear <- rep(NA, times = nrow(CN_over_time))

  # j indicates the loop variable over the years (starting at t = 0)
  # i indicates the current row of the dataframe

  j <- 0
  while (j <= (numb_years)) {

    multiple <- na.omit(CN_over_time[, j + 1])
    multiple <- multiple[duplicated(multiple)] # Which codes appear more than once?
    tplusone_codes <- CN_over_time[[j + 2]]  # (j+2)th column

    for (i in 1:nrow(CN_over_time)) {
      current_code <- CN_over_time[i, j + 1]
      # Two arguments must hold in order to set flag == 1
      # 1) the code has to appear more than once in the current year
      # 2) the code has to remain (in terms of notation) the same on the whole time period

      ####### ERROR!!!
      if (current_code %in% multiple &
          all(apply(CN_over_time[i, 1:(numb_years + 1)], 2, function(x) paste(CN_over_time[i, 1]) %in% x))) {
        CN_over_time$flag[i] <- 1
        if (is.na(CN_over_time$flagyear[i])) {CN_over_time$flagyear[i] <- b+j}
      }
    }

    if (progress) {
      print(paste0("Work in progress... Part 2/", mod_part,": ", round(j/numb_years, 3) * 100, "%"))
    }
    j <- j + 1
  }

  return(CN_over_time)
}



