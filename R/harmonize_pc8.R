###############################################################################
### Get PC8+ code
### 24-11-2021
### Christoph Baumgartner & Janette Walde
###############################################################################

#' @importFrom stats na.omit setNames
#' @importFrom utils read.csv2
#' @export harmonize_pc8

harmonize_pc8 <- function(b, e, historymatrix = NULL, harmonize.to = "e",
                          HS6breaks = c(1992, 1996, 2002, 2007, 2012, 2017),
                          progress = TRUE) {

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
    stop("There is no data avilable for the first year of interest (b). Consider changing the time periode, or alter data (-> get_data_directory()).")
  }
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/PC8/PC8_", e, ".rds"))) {
    stop("There is no data avilable for the last year of interest (e). Consider changing the time periode, or alter data (-> get_data_directory()).")
  }

  #########################
  ### define harmonization start/end
  #########################

  if (!(harmonize.to %in% c("b", "e"))) {
    stop("Invalid value of 'harmonize.to', 'harmonize.to' must be set to 'e' or 'b'.")
  }

  if(harmonize.to == "e") {
    harm_year <- e
  } else {
    harm_year <- b
  }

  #########################
  ### history matrix check
  #########################
  fcalls <- sys.nframe()

  if (is.null(historymatrix)) {
    PC8_over_time <- history_matrix_pc8(b = b, e = e, progress = progress)

  } else {
    if (!is.data.frame(historymatrix)) {
      stop("The entered history matrix is not a dataframe. Please correct.")
    }
    if (!any(grep("PC8_", colnames(historymatrix)))) {
      stop("Column names of the history matrix are not correct. Please correct.")
    }
    if (ncol(historymatrix) != (e - b + 3)) {
      stop(paste0("The entered history matrix has the wrong number of columns. It has ", ncol(historymatrix), ", while ", (e - b + 3), " are needed for the entered time period. Please correct."))
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
    PC8_over_time <- historymatrix
    fcalls <- fcalls + 2
  }

  if (fcalls == 1) {
    mod_partb <- 3
    mod_parte <- 4
  } else if (fcalls == 2) {
    mod_partb <- 3
    mod_parte <- 5
  } else {
    mod_partb <- 1
    mod_parte <- 2
  }

  # get all files from data folder
  filenames <- list.files(paste0(system.file("extdata", package = "harmonizer"), "/HS6"), pattern="*.csv", full.names=TRUE)
  # read in possible HS6 breaks
  breaks <- HS6breaks
  # check which breaks are needed for the specific time period
  idx_breaks <- b < breaks & e > breaks
  if (any(idx_breaks)) {
    # reduce used files
    index <- NULL
    for (i in 1:length(breaks[idx_breaks])){
      index <- c(index,grep(breaks[idx_breaks][i],filenames))
    }
    index <- unique(index)
    filenames <- filenames[index]
    correspondence_lists <- lapply(filenames, read.csv2)

    # split up data and rename cols
    for(i in 1:length(correspondence_lists)) {
      # unlist as dataframe
      assign(paste0("correspondence_", breaks[idx_breaks][i]), as.data.frame(correspondence_lists[i]))
    #   # split up in two cols
    #   assign(paste0("correspondence_", breaks[idx_breaks][i]),
    #          data.frame(do.call("rbind", strsplit(as.character(eval(parse(text = paste0("correspondence_", breaks[idx_breaks][i], "[[1]]")))),
    #                                               ";", fixed = TRUE))), )
      # rename cols
      assign(paste0("correspondence_", breaks[idx_breaks][i]),
             setNames(eval(parse(text = paste0("correspondence_", breaks[idx_breaks][i]))), c("new", "old")))
    }
  }

  PC8_to_BEC <- pc8_to_bec(b = b, e = e, historymatrix = PC8_over_time, progress = FALSE)

  years <- as.character(b:e)
  nbr_years <- length(years)

  change_code <- rep("clear_code", nrow(PC8_over_time))
  new_code <- rep(0, nrow(PC8_over_time))

  # when did a new code pop up, give the last year of NA
  flag_names <- c("flag", "flagyear")
  for (j in 1:(ncol(PC8_over_time) - length(flag_names))) {
    for (i in 1:nrow(PC8_over_time)) {
      if (is.na(PC8_over_time[i, j])) {
        new_code[i] <- substr(colnames(PC8_over_time)[j], start = 5, stop = 10)
        next
      }
    }
  }

  # product codes changes in time, yes = 1
  for (j in 2:(ncol(PC8_over_time) - length(flag_names))) {
    for (i in 1:nrow(PC8_over_time)) {
      if ((!is.na(PC8_over_time[i, j])) & (PC8_over_time[i, j] != PC8_over_time[i, j - 1]) & (!is.na(PC8_over_time[i, j - 1]))) {
        change_code[i] <- 1
      }
    }
  }
  PC8_over_time$new_code <- new_code
  PC8_over_time$change_code <- change_code

  # various changes in the PC8 codes or merge/split
  temp3 <- PC8_over_time[PC8_over_time$change_code == 1 | PC8_over_time$new_code != 0 | PC8_over_time$flag==1, !(names(PC8_over_time) %in% flag_names)]
  temp3 <- temp3[, !(names(temp3) %in% c("new_code","change_code"))]

  tied_codes <- matrix("no_ties",nrow(temp3),1)
  if (progress) {
    print(paste0("Work in progress... Part ", mod_partb, "/" , mod_parte,": 0%"))
  }
  for (i in 1:nrow(temp3)){
    if (i==nrow(temp3)) break
    for (j in (i + 1):nrow(temp3)) {
      # check if there is a connection between row i and row j
      if (sum(na.omit(t(temp3[i, ])) %in% na.omit(t(temp3[j, ]))) > 0) {
        # check if row i has a family yet; if so use it
        if (tied_codes[i] != "no_ties" & tied_codes[j] == "no_ties") {
          temp_fam <- tied_codes[i]
          tied_codes[j] <- temp_fam
        }
        if (tied_codes[i] == "no_ties" & tied_codes[j] != "no_ties") {
          temp_fam <- tied_codes[j]
          tied_codes[i] <- temp_fam
        }
        if (tied_codes[i] != "no_ties" & tied_codes[j] != "no_ties") {
          if (tied_codes[i] != tied_codes[j]){
            temp_fam <- tied_codes[i]
            tied_codes[tied_codes==tied_codes[j]] <- temp_fam
          }
        }
        if (tied_codes[i] == "no_ties" & tied_codes[j] == "no_ties") {
          tied_codes[j] <- paste0("f", i)
          tied_codes[i] <- paste0("f", i)
        }
      }
    }
    if (progress) {
      if (i %% 100 == 0) {
        print(paste0("Work in progress... Part ", mod_partb,  "/" , mod_parte, ": ", round(i / nrow(temp3), 3) * 100, "%"))
      }
    }
  }
  if (progress) {
    print(paste0("Work in progress... Part ", mod_partb, "/" , mod_parte, ": 100%"))
  }

  # Harmonization to year harm_year
  PC8_over_time$PC8plus <- PC8_over_time[,paste0("PC8_", harm_year)]
  PC8_over_time$PC8plus[PC8_over_time$change_code == 1 | PC8_over_time$new_code != 0 | PC8_over_time$flag == 1] <- tied_codes
  PC8_over_time$PC8plus[PC8_over_time$PC8plus == "no_ties"] <- PC8_over_time[PC8_over_time$PC8plus == "no_ties",paste0("PC8_", harm_year)]

  PC8_over_time <- PC8_over_time[,!(colnames(PC8_over_time) %in% c("change_code"))]

  # merge PC8_over_time and PC8_to_BEC
  PC8_over_time <- merge(PC8_over_time,PC8_to_BEC,by.x = "PC8plus", by.y = "PC8",all.x = TRUE)

  ##### Are there changes of PC8 but within HS6?
  # Define variable family
  PC8_over_time$family <- rep(0,dim(PC8_over_time)[1])
  PC8_over_time$family[grep("f",PC8_over_time$PC8plus)] <- 1

  # HS6 harmonize
  PC8_over_time$HS6 <- as.character(PC8_over_time$HS6)
  PC8_over_time$HS6plus <- as.character(PC8_over_time$HS6)
  HS6_temp <- PC8_over_time[PC8_over_time$family == 1, ]

  # Find HS6 to PC8 and replace PC8 codes with HS6
  for (i in 1:nbr_years){
    # replace all NAs in PC8 codes with a "useless" string
    # otherwise the apply function does not work properly
    HS6_temp[, paste0("PC8_",years[i])][is.na(HS6_temp[, paste0("PC8_",years[i])])] <- "removed"
    # find the corresponding HS6 codes
    indexhs6 <- unlist(apply(as.matrix(paste0("\\b", HS6_temp[, paste0("PC8_",years[i])], "\\b")), 1, grep, x = PC8_to_BEC$PC8))
    # find which PC8 codes have a replacement
    indexpc8 <- HS6_temp[, paste0("PC8_", years[i])] %in% PC8_to_BEC$PC8
    # replace PC8 with HS6 if possible
    HS6_temp[indexpc8, paste0("PC8_", years[i])] <- PC8_to_BEC$HS6[indexhs6]
  }

  fams <- unique(HS6_temp$PC8plus)

  if (progress) {
    print(paste0("Work in progress... Part ", mod_partb + 1,  "/" , mod_parte, ": 0%"))
  }

  # check if breaks are needed
  if (any(idx_breaks)) {
    for (i in 1:length(fams)) {
      ### check consistency of each block
      consistent <- vector(mode = "logical")
      # nbr of blocks = breaks + 1
      for (s in 1:(length(breaks[idx_breaks]) + 1)) {
        if (length(breaks[idx_breaks]) == 1) {
          # special case for only one break
          early_years <- as.integer(substr(colnames(HS6_temp)[grep("PC8_", colnames(HS6_temp))], start = 5, stop = 8)) < breaks[idx_breaks]
          early_years_names <- colnames(HS6_temp)[grep("PC8_", colnames(HS6_temp))][early_years]
          late_years_names <- colnames(HS6_temp)[grep("PC8_", colnames(HS6_temp))][!early_years]
          block_1 <- as.data.frame(HS6_temp[HS6_temp$PC8plus == fams[i], early_years_names])
          block_2 <- as.data.frame(HS6_temp[HS6_temp$PC8plus == fams[i], late_years_names])
          if (length(unique(unlist(block_1))) == 1) {
            consistent <- c(consistent, TRUE)
          } else {
            consistent <- c(consistent, FALSE)
          }
          if (length(unique(unlist(block_2))) == 1) {
            consistent <- c(consistent, TRUE)
          } else {
            consistent <- c(consistent, FALSE)
          }
          break
        } else {
          # derive last block (> last break)
          if (s == (length(breaks[idx_breaks]) + 1)) {
            temp_years <- as.integer(substr(colnames(HS6_temp)[grep("PC8_", colnames(HS6_temp))], start = 5, stop = 8)) >= breaks[idx_breaks][length(breaks[idx_breaks])] #breaks[length(breaks)]
            temp_years_names <- colnames(HS6_temp)[grep("PC8_", colnames(HS6_temp))][temp_years]
            # get all codes in last "time block"
            assign(paste0("block_", s), as.data.frame(HS6_temp[HS6_temp$PC8plus == fams[i], temp_years_names]))

          } else if (s == 1) { # First block
            temp_years <- as.integer(substr(colnames(HS6_temp)[grep("PC8_", colnames(HS6_temp))], start = 5, stop = 8)) < breaks[idx_breaks][s]
            temp_years_names <- colnames(HS6_temp)[grep("PC8_", colnames(HS6_temp))][temp_years]
            # get all codes in one "time block"
            assign(paste0("block_", s), as.data.frame(HS6_temp[HS6_temp$PC8plus == fams[i], temp_years_names]))

          } else { # Blocks in between
            # for all other blocks but the last the following works
            temp_years1 <- as.integer(substr(colnames(HS6_temp)[grep("PC8_", colnames(HS6_temp))], start = 5, stop = 8)) < breaks[idx_breaks][s]
            temp_years2 <- as.integer(substr(colnames(HS6_temp)[grep("PC8_", colnames(HS6_temp))], start = 5, stop = 8)) >= breaks[idx_breaks][s - 1]# temp_years[temp_years >= breaks[idx_breaks][s - 1]]
            temp_years <- temp_years1 & temp_years2
            temp_years_names <- colnames(HS6_temp)[grep("PC8_", colnames(HS6_temp))][temp_years]
            # get all codes in one "time block"
            assign(paste0("block_", s), as.data.frame(HS6_temp[HS6_temp$PC8plus == fams[i], temp_years_names]))
          }

          # check if all codes in this block are the same
          if (length(unique(unlist(eval(parse(text = paste0("block_", s)))))) == 1) {
            consistent <- c(consistent, TRUE)
          } else {
            consistent <- c(consistent, FALSE)
          }
        }
      }
      ### check if all codes across all blocks remain the same
      temp_blockvalue <- vector(mode = "character")
      for (k in 1:(length(breaks[idx_breaks]) + 1)) {
        temp_blockvalue <- c(temp_blockvalue, eval(parse(text = rep(paste0("block_", k, "[[1]][1]")))))
      }
      if (length(unique(temp_blockvalue)) == 1) {
        equal <- TRUE
      } else {
        equal <- FALSE
      }

      ### check if the blocks are consistent and equal
      if (all(consistent) & equal) {
        HS6_temp$HS6plus[HS6_temp$PC8plus == fams[i]] <- rep(unique(unlist(eval(parse(text = "block_1")))),
                                                             times = nrow(eval(parse(text = "block_1"))))
      } else if (all(consistent)) {
        # check if the codes across the blocks differ
        # but the changes are consistent with the change list
        # loop over all correspondece lists
        for (j in 1:sum(idx_breaks)) {
          # n ... rownbr of related correspondence list where the code of block_j appears
          # m ... rownbr of related correspondence list where the code of block_(j+1) appears
          n <- which(eval(parse(text = paste0("block_", j, "[[1]][1]"))) == eval(parse(text = paste0("correspondence_", breaks[idx_breaks][j], "$old"))))
          m <- which(eval(parse(text = paste0("block_", j + 1, "[[1]][1]"))) == eval(parse(text = paste0("correspondence_", breaks[idx_breaks][j], "$new"))))
          # the code has to appear only once, otherwise no clear assignment can be done
          if (length(n) == 1 & length(m) == 1) {
            if ( m == n) {
              HS6_temp$HS6plus[HS6_temp$PC8plus == fams[i]] <- rep(unique(unlist(eval(parse(text = paste0("block_",
                                                                                                          length(breaks[idx_breaks]) + 1))))),
                                                                   times = nrow(eval(parse(text = paste0("block_",
                                                                                                         length(breaks[idx_breaks]) + 1)))))
            }
          } else {
            # if the changes are not consistent with the change list keep the old family
            HS6_temp$HS6plus[HS6_temp$PC8plus == fams[i]] <- rep(fams[i],
                                                                 times = nrow(eval(parse(text = paste0("block_",
                                                                                                       length(breaks[idx_breaks]) + 1)))))
          }
        }
      } else {
        # if the changes are not consistent nor equal keep the old family
        HS6_temp$HS6plus[HS6_temp$PC8plus == fams[i]] <- rep(fams[i],
                                                             times = nrow(eval(parse(text = paste0("block_",
                                                                                                   length(breaks[idx_breaks]) + 1)))))
      }
      if (progress) {
        if (i %% 100 == 0) {
          print(paste0("Work in progress... Part ", mod_partb + 1,  "/" , mod_parte, ": ", round(i / length(fams), 3) * 100, "%"))
        }
      }
    }
  } else {
    # case if no changes of HS6 happened in the observed time period
    year_names <- colnames(HS6_temp)[grep("PC8_", colnames(HS6_temp))]
    for (i in 1:length(fams)) {
      block <- as.data.frame(HS6_temp[HS6_temp$PC8plus == fams[i], year_names])
      # check the code whether it changes across the years?
      if (length(unique(unlist(block))) == 1) {
        # if no change - rewrite HS6plus
        HS6_temp$HS6plus[HS6_temp$PC8plus == fams[i]] <- rep(unique(unlist(block)), times = nrow(block))
      } else {
        # else keep the family
        HS6_temp$HS6plus[HS6_temp$PC8plus == fams[i]] <- rep(fams[i], times = nrow(block))
      }
    }
  }

  if (progress) {
    print(paste0("Work in progress... Part ", mod_partb + 1,  "/" , mod_parte, ": 100%"))
  }

  PC8_over_time$HS6plus[PC8_over_time$family == 1] <- HS6_temp$HS6plus

  # get BEC and BEC_agr for HS6plus
  namescol <- c("BEC","BEC_agr")
  PC8_to_BEC_unique <- PC8_to_BEC[!duplicated(PC8_to_BEC$HS6),2:4]
  PC8_over_time$sort <- seq(1:nrow(PC8_over_time))
  temp <- merge(PC8_over_time[PC8_over_time$family == 1,!colnames(PC8_over_time) %in% namescol],
                PC8_to_BEC_unique, by.x = "HS6plus", by.y = "HS6", all.x=TRUE, sort = TRUE)
  temp <- temp[order(temp$sort),]

  PC8_over_time$BEC[PC8_over_time$family == 1] <- temp$BEC
  PC8_over_time$BEC_agr[PC8_over_time$family == 1] <- temp$BEC_agr
  rm(temp)

  PC8_over_time$family <- NULL
  PC8_over_time$sort <- NULL
  PC8_over_time$HS6 <- NULL
  PC8_over_time$new_code <- NULL



  #############################################

  # define SNA
  if(e < 2012) {
    PC8_over_time$SNA_basic_class <- PC8_over_time$BEC
    PC8_over_time$SNA_basic_class[which(PC8_over_time$SNA_basic_class %in% c("41", "521"))] <- "Capital good"
    PC8_over_time$SNA_basic_class[PC8_over_time$SNA_basic_class %in% c("111", "121", "21",
                                                                       "22", "31", "322",
                                                                       "42", "53")] <- "Intermediate good"
    PC8_over_time$SNA_basic_class[PC8_over_time$SNA_basic_class %in% c("112", "122", "522",
                                                                       "61", "62", "63")] <- "Consumption good"
    PC8_over_time$SNA_basic_class[PC8_over_time$SNA_basic_class %in% c("51", "321", "7")] <- "not_defined"

  } else {
    PC8_over_time$SNA_basic_class <- PC8_over_time$BEC
    PC8_over_time$SNA_basic_class[which(PC8_over_time$SNA_basic_class %in% c("112", "112010", "112020",
                                                                             "212", "212010", "212020",
                                                                             "312", "312010", "312020",
                                                                             "412", "412010", "412020",
                                                                             "512", "512010", "512020",
                                                                             "612", "612010", "612020",
                                                                             "712", "712010", "712020",
                                                                             "812", "812010", "812020"))] <- "Gross Fixed Capital Formation"
    PC8_over_time$SNA_basic_class[PC8_over_time$SNA_basic_class %in% c("111", "1111", "1112", "111210", "111220", "121", "121010", "121020",
                                                                       "211", "2111", "2112", "211210", "211220", "221", "221010", "221020",
                                                                       "311", "3111", "3112", "311210", "311220", "321", "321010", "321020",
                                                                       "411", "4111", "4112", "411210", "411220", "421", "421010", "421020",
                                                                       "511", "5111", "5112", "511210", "511220", "521", "521010", "521020",
                                                                       "611", "6111", "6112", "611210", "611220", "621", "621010", "621020",
                                                                       "711", "7111", "7112", "711210", "711220", "721", "721010", "721020",
                                                                       "811", "8111", "8112", "811210", "811220", "821", "821010", "821020")] <- "Intermediate Consumption"
    PC8_over_time$SNA_basic_class[PC8_over_time$SNA_basic_class %in% c("113", "1131", "113101", "113102", "1132", "113201", "113202", "123",
                                                                       "213", "2131", "213101", "213102", "2132", "213201", "213202", "223",
                                                                       "313", "3131", "313101", "313102", "3132", "313201", "313202", "323",
                                                                       "413", "4131", "413101", "413102", "4132", "413201", "413202", "423",
                                                                       "513", "5131", "513101", "513102", "5132", "513201", "513202", "523",
                                                                       "613", "6131", "613101", "613102", "6132", "613201", "613202", "623",
                                                                       "713", "7131", "713101", "713102", "7132", "713201", "713202", "723",
                                                                       "813", "8131", "813101", "813102", "8132", "813201", "813202", "823")] <- "Final Consumption"
  }

  # remove PC8plus, HS6plus, BEC and SNA if not harmonizeable, i.e. NA in year harm_year

  idx_na_e <- which(is.na(eval(parse(text = paste0("PC8_over_time$PC8_", harm_year)))))
  PC8_over_time$PC8plus[idx_na_e] <- NA
  PC8_over_time$HS6plus[idx_na_e] <- NA
  PC8_over_time$BEC[idx_na_e] <- NA
  PC8_over_time$BEC_agr[idx_na_e] <- NA
  PC8_over_time$SNA_basic_class[idx_na_e] <- NA

  PC8_over_time <- PC8_over_time[!duplicated(PC8_over_time),]

  return(PC8_over_time)
}

