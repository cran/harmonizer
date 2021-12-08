##############################################################################
### harmonize PC8
### 12-11-2021
### Christoph Baumgartner & Janette Walde
##############################################################################

#' @importFrom stats na.omit
#' @export history.matrix.pc8

history.matrix.pc8 <- function(b, e, progress = TRUE) {
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
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/PC8/PC8_", b, ".rds"))) {
    stop("There is no data avilable for the first year of interest (b). Consider changing the time periode, or alter data (-> get.data.directory()).")
  }
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/PC8/PC8_", e, ".rds"))) {
    stop("There is no data avilable for the first year of interest (e). Consider changing the time periode, or alter data (-> get.data.directory()).")
  }
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/PC8/PC8_", e - 1, "_", e, ".rds"))) {
    stop("There is no data avilable for the concordance between the last year of interest (e) and the year before. Consider changing the time periode, or add data (-> get.data.directory()).")
  }
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/PC8/PC8_", b, "_", b + 1, ".rds"))) {
    stop("There is no data avilable for the concordance between the first year of interest (b) and the year after. Consider changing the time periode, or add data (-> get.data.directory()).")
  }

  ###############################################################################
  ### get data
  ###############################################################################

  # was the function called by another fct already
  fcalls <- sys.nframe()
  if(fcalls == 2) {
    mod_part <- 4
  } else if(fcalls == 3) {
    mod_part <- 5
  } else {
    mod_part <- 2
  }

  numb_years <- e - b # length of period of interest = numb_years + 1

  # load all lists of codes in the respective year
  for(i in 0:(e - b)) {
    assign(paste0("PC8_", b + i), readRDS(paste0(system.file("extdata", package = "harmonizer"), "/PC8/PC8_", b + i, ".rds")))
  }

  # load the concordance lists
  for(i in 0:(e - b - 1)) {
    assign(paste0("PC8_", b + i, "_", b + i + 1),
           readRDS(paste0(system.file("extdata", package = "harmonizer"), "/PC8/PC8_", b + i, "_", b + i + 1, ".rds")))
    assign("temp", eval(parse(text = paste0("PC8_", b + i, "_", b + i + 1))))
    # all changed product codes
    temp1 <- temp[temp[, 1] != temp[, 2], ]
    # plus new or dropped product codes -> concordance list
    temp2 <- rbind(temp1, temp[is.na(temp[, 1]) | is.na(temp[, 2]), ])
    assign(paste0("PC8_", b + i, "_", b + i + 1), temp2)
    rm(temp, temp1, temp2)
  }

  ###############################################################################
  ### main function
  ###############################################################################

  # create a dataframe for all PC8 codes in each year
  # each row provides the history of a PC8 code over the years

  # start with the codes of the year b
  PC8_over_time <- eval(parse(text = paste0("PC8_", b)))
  colnames(PC8_over_time) <- paste0("PC8_", b)

  # loop over all years
  # compare all years pairwise where x is at time t
  # and y is the concordance list of t and t + 1
  # x is from the dataframe "PC8_over_time"
  j <- 0
  if (progress) {
    print(paste0("Work in progress... Part 1/", mod_part,": 0%"))
  }
  while(j < numb_years) {

    assign("x", eval(parse(text = paste0("PC8_over_time[", j + 1, "]"))))
    assign("y", eval(parse(text = paste0("PC8_", b + j, "_", b + j + 1))))

    # create dataframe for all "regular" changed codes
    PC8_over_time_temp <- data.frame(
      "PC8" = rep(NA, times = nrow(eval(parse(text = paste0("PC8_", b)))))
    )

    # create dataframe which appeared multiple times in the obsolete list (split/merged codes)
    multiple_codes <- data.frame(
      "PC8" <- character()
    )

    # all codes of t + 1
    next_year <- eval(parse(text = paste0("PC8_", b + j + 1)))

    # loop over rows = codes
    for(i in 1:nrow(x)) {

      current_PC8 <- x[i, 1]

      # check NA (check whether a product code was dropped)
      if (is.na(current_PC8) & (j > 0)) {
        # k is the previous col
        k <- j
        obs_NA <- current_PC8
        while(is.na(obs_NA)) { # Check whether there is a code available in previous years?
          # Take the former code if available and
          # look whether it pops up again.
          obs_NA <- PC8_over_time[i, k]
          k <- k - 1
        }
        current_PC8 <- obs_NA
      }

      # Case A: check if the code did not change
      if (current_PC8 %in% next_year[,1]) {
        PC8_over_time_temp[i, 1] <- current_PC8

        # Case A1: check if the code merged or split in addition
        if (current_PC8 %in% y$old) {
          # check if the code
          idx_new_codes <- which(current_PC8 == y$old)
          n_new_codes <- length(idx_new_codes)
          new_codes <- y$new[idx_new_codes]

          # if there are multiple replacements of one code,
          # n new rows are added for new codes

          previous_codes <- PC8_over_time[i, ]
          rownames(previous_codes) <- NULL
          new_df <- cbind(previous_codes, new_codes)
          colnames(new_df)[1:(j + 1)] <- colnames(as.data.frame(PC8_over_time))
          colnames(new_df)[j + 2] <- paste0("PC8_", b + j + 1)

          multiple_codes <- rbind(multiple_codes, new_df, make.row.names = FALSE)
        }
        next
      }

      # Case B: check if the code was changed
      if (current_PC8 %in% y$old) {
        # check if the code was split
        idx_new_codes <- which(current_PC8 == y$old)
        n_new_codes <- length(idx_new_codes)
        new_codes <- y$new[idx_new_codes]

        # Case B1: if there are multiple replacements of one code (n > 1),
        # n - 1 new rows are added for the new codes
        if (n_new_codes > 1) {
          PC8_over_time_temp[i, 1] <- new_codes[1]
          previous_codes <- PC8_over_time[i, ]
          rownames(previous_codes) <- NULL
          new_df <- cbind(previous_codes, new_codes[-1])
          colnames(new_df)[1:(j + 1)] <- colnames(as.data.frame(PC8_over_time))
          colnames(new_df)[j + 2] <- paste0("PC8_", b + j + 1)

          multiple_codes <- rbind(multiple_codes, new_df, make.row.names = FALSE)

        } else {
          # Case B2: the code was not split and has only one replacement
          PC8_over_time_temp[i, 1] <- new_codes
        }
        next
      }

      # Case C: the code was dropped completely
      PC8_over_time_temp[i, 1] <- NA
    }

    # add all "regular" changed codes
    PC8_over_time <- cbind(PC8_over_time, PC8_over_time_temp)
    colnames(PC8_over_time)[j + 2] <- paste0("PC8_", b + j + 1)

    # add all codes which appeared multiple times in the obsolete list
    PC8_over_time <- rbind(PC8_over_time, multiple_codes, make.row.names = FALSE)

    # Case D: add codes which were created in t + 1
    # and fill all previous years with NA
    assign(paste0("PC8f_", b + j + 1), next_year[!next_year[[1]] %in%  PC8_over_time[[j + 2]], 1])
    created_codes_df <- as.data.frame(eval(parse(text = paste0("PC8f_", b + j + 1))))
    colnames(created_codes_df)[1] <- paste0("PC8_", b + j + 1)
    for (r in 1:(j + 1)) {
      assign(paste0("PC8f_", b + r - 1), rep(NA, times = nrow(created_codes_df)))
      created_codes_df <- as.data.frame(cbind(created_codes_df, eval(parse(text = paste0("PC8f_", b + r - 1)))))
      colnames(created_codes_df)[r + 1] <- paste0("PC8_", b + r - 1)
    }
    PC8_over_time <- rbind(PC8_over_time, created_codes_df, make.row.names = FALSE)

    nacheck <- which(rowSums(is.na(PC8_over_time)) == ncol(PC8_over_time))
    if (length(nacheck) != 0) {
      PC8_over_time <- PC8_over_time[-nacheck,]
    }
    rm(nacheck)

    j <- j + 1
    if (progress) {
      print(paste0("Work in progress... Part 1/", mod_part,": ", round(j/numb_years, 3) * 100, "%"))
    }
  }

  PC8_over_time <- PC8_over_time[!duplicated(PC8_over_time), ]

  ###############################################################################
  ### add variable "flag" & "flag_year"
  ###############################################################################

  # flag == 1 indicates that this code did not change in terms of PC8 notation
  # but was split or merged at least once in the time record
  # flag_year indicates the year this split/merge appeared

  PC8_over_time$flag <- rep(0, times = nrow(PC8_over_time))
  PC8_over_time$flagyear <- rep(NA, times = nrow(PC8_over_time))

  # j indicates the loop variable over the years (starting at t = 0)
  # i indicates the current row of the dataframe

  j <- 0
  while (j <= (numb_years)) {

    multiple <- na.omit(PC8_over_time[, j + 1])
    multiple <- multiple[duplicated(multiple)] # Which codes are more than once?
    tplusone_codes <- PC8_over_time[[j + 2]]  # (j+2)th col

    for (i in 1:nrow(PC8_over_time)) {
      current_code <- PC8_over_time[i, j + 1]
      # Two arguments must hold in order to set flag == 1
      # 1) the code has to appear more than once in the current year
      # 2) the code has to remain (in terms of notation) the same on the whole time period

      if (current_code %in% multiple &
          all(apply(PC8_over_time[i, 1:(numb_years + 1)], 2, function(x) paste(PC8_over_time[i, 1]) %in% x))) {
        PC8_over_time$flag[i] <- 1
        if (is.na(PC8_over_time$flagyear[i])) {PC8_over_time$flagyear[i] <- b + j}
      }
    }

    if (progress) {
      print(paste0("Work in progress... Part 2/", mod_part,": ", round(j/numb_years, 3) * 100, "%"))
    }
    j <- j + 1
  }
  return(PC8_over_time)
}



