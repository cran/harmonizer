###############################################################################
### Get CN8+ code
### 12-11-2021
### Christoph Baumgartner & Janette Walde
###############################################################################

#' @importFrom stats na.omit setNames
#' @importFrom utils read.csv
#' @export harmonize_cn8

harmonize_cn8 <- function(b, e, historymatrix = NULL,
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
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/CN8/CN8_", b, ".rds"))) {
    stop("There is no data avilable for the first year of interest (b). Consider changing the time periode, or alter data (-> get.data.directory()).")
  }
  if (!file.exists(paste0(system.file("extdata", package = "harmonizer"), "/CN8/CN8_", e, ".rds"))) {
    stop("There is no data avilable for the first year of interest (e). Consider changing the time periode, or alter data (-> get.data.directory()).")
  }

  #########################
  ### history matrix check
  #########################
  fcalls <- sys.nframe()

  if (is.null(historymatrix)) {
    CN8_over_time <- history_matrix_cn8(b = b, e = e, progress = progress)

  } else {
    if (!is.data.frame(historymatrix)) {
      stop("The entered history matrix is not a dataframe. Please correct.")
    }
    if (!any(grep("CN8_", colnames(historymatrix)))) {
      stop("Column names of the history matrix are not correct. Please correct.")
    }
    if (ncol(historymatrix) != (e - b + 3)) {
      stop(paste0("The entered history matrix has the wrong number of colums. It has ", ncol(historymatrix), ", while ", (e - b + 3), " are needed for the entered time period. Please correct."))
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
    CN8_over_time <- historymatrix
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
    correspondence_lists <- lapply(filenames, read.csv)

    # split up data and rename cols
    for(i in 1:length(correspondence_lists)) {
      # unlist as dataframe
      assign(paste0("correspondence_", breaks[idx_breaks][i]), as.data.frame(correspondence_lists[i], responseName = c("HS6", "BEC")))
      # split up in two cols
      assign(paste0("correspondence_", breaks[idx_breaks][i]),
             data.frame(do.call("rbind", strsplit(as.character(eval(parse(text = paste0("correspondence_", breaks[idx_breaks][i], "[[1]]")))),
                                                  ";", fixed = TRUE))), )
      # rename cols
      assign(paste0("correspondence_", breaks[idx_breaks][i]),
             setNames(eval(parse(text = paste0("correspondence_", breaks[idx_breaks][i]))), c("new", "old")))
    }
  }

  CN8_to_BEC <- cn8_to_bec(b = b, e = e, historymatrix = CN8_over_time, progress = FALSE)

  change_code <- rep("clear_code",nrow(CN8_over_time))
  new_code <- rep(0,nrow(CN8_over_time))

  # when did a new code pop up
  flag_names <- c("flag", "flagyear")
  for (j in 1:(ncol(CN8_over_time) - length(flag_names))) {
    for (i in 1:nrow(CN8_over_time)) {
      if (is.na(CN8_over_time[i, j])) {
        new_code[i] <- substr(colnames(CN8_over_time)[j], start = 5, stop = 10)
        next
      }
    }
  }

  # product codes changes in time, yes = 1
  for (j in 2:(ncol(CN8_over_time) - length(flag_names))) {
    for (i in 1:nrow(CN8_over_time)) {
      if ((!is.na(CN8_over_time[i, j])) & (CN8_over_time[i, j] != CN8_over_time[i, j - 1]) & (!is.na(CN8_over_time[i, j - 1]))) {
        change_code[i] <- 1
      }
    }
  }
  CN8_over_time$new_code <- new_code
  CN8_over_time$change_code <- change_code

  # various changes in the CN8 codes or merge/split
  temp3 <- CN8_over_time[CN8_over_time$change_code == 1 | CN8_over_time$new_code != 0 | CN8_over_time$flag==1, !(names(CN8_over_time) %in% flag_names)]
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

  # Harmonization to year e
  CN8_over_time$CN8plus <- CN8_over_time[,paste0("CN8_",e)]
  CN8_over_time$CN8plus[CN8_over_time$change_code == 1 | CN8_over_time$new_code != 0 | CN8_over_time$flag == 1] <- tied_codes
  CN8_over_time$CN8plus[CN8_over_time$CN8plus == "no_ties"] <- CN8_over_time[CN8_over_time$CN8plus == "no_ties",paste0("CN8_",e)]

  CN8_over_time <- CN8_over_time[,!(colnames(CN8_over_time) %in% c("change_code"))]

  # merge CN8_over_time and CN8_to_BEC
  CN8_over_time <- merge(CN8_over_time,CN8_to_BEC,by.x = "CN8plus", by.y = "CN8",all.x = TRUE)

  ##### Are there changes of CN8 but within HS6?
  # Define variable family
  CN8_over_time$family <- rep(0,dim(CN8_over_time)[1])
  CN8_over_time$family[grep("f",CN8_over_time$CN8plus)] <- 1

  # HS6 harmonize
  CN8_over_time$HS6 <- as.character(CN8_over_time$HS6)
  CN8_over_time$HS6plus <- as.character(CN8_over_time$HS6)
  HS6_temp <- CN8_over_time[CN8_over_time$family == 1, ]
  HS6_temp[, grep("CN8_", colnames(HS6_temp))] <- apply(HS6_temp[, grep("CN8_", colnames(HS6_temp))], 2,
                                                        substr, start = 1, stop = 6)
  fams <- unique(HS6_temp$CN8plus)

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
          early_years <- as.integer(substr(colnames(HS6_temp)[grep("CN8_", colnames(HS6_temp))], start = 5, stop = 8)) < breaks[idx_breaks]
          early_years_names <- colnames(HS6_temp)[grep("CN8_", colnames(HS6_temp))][early_years]
          late_years_names <- colnames(HS6_temp)[grep("CN8_", colnames(HS6_temp))][!early_years]
          block_1 <- as.data.frame(HS6_temp[HS6_temp$CN8plus == fams[i], early_years_names])
          block_2 <- as.data.frame(HS6_temp[HS6_temp$CN8plus == fams[i], late_years_names])
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
            temp_years <- as.integer(substr(colnames(HS6_temp)[grep("CN8_", colnames(HS6_temp))], start = 5, stop = 8)) >= breaks[length(breaks)]
            temp_years_names <- colnames(HS6_temp)[grep("CN8_", colnames(HS6_temp))][temp_years]
            # get all codes in last "time block"
            assign(paste0("block_", s), as.data.frame(HS6_temp[HS6_temp$CN8plus == fams[i], temp_years_names]))

          } else if (s == 1) { # First block
            temp_years <- as.integer(substr(colnames(HS6_temp)[grep("CN8_", colnames(HS6_temp))], start = 5, stop = 8)) < breaks[idx_breaks][s]
            temp_years_names <- colnames(HS6_temp)[grep("CN8_", colnames(HS6_temp))][temp_years]
            # get all codes in one "time block"
            assign(paste0("block_", s), as.data.frame(HS6_temp[HS6_temp$CN8plus == fams[i], temp_years_names]))

          } else { # Blocks in between
            # for all other blocks but the last the following works
            temp_years1 <- as.integer(substr(colnames(HS6_temp)[grep("CN8_", colnames(HS6_temp))], start = 5, stop = 8)) < breaks[idx_breaks][s]
            temp_years2 <- as.integer(substr(colnames(HS6_temp)[grep("CN8_", colnames(HS6_temp))], start = 5, stop = 8)) >= breaks[idx_breaks][s - 1]
            temp_years <- temp_years1 & temp_years2
            temp_years_names <- colnames(HS6_temp)[grep("CN8_", colnames(HS6_temp))][temp_years]
            # get all codes in one "time block"
            assign(paste0("block_", s), as.data.frame(HS6_temp[HS6_temp$CN8plus == fams[i], temp_years_names]))
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
        HS6_temp$HS6plus[HS6_temp$CN8plus == fams[i]] <- rep(unique(unlist(eval(parse(text = "block_1")))),
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
            if (m == n) {
              HS6_temp$HS6plus[HS6_temp$CN8plus == fams[i]] <- rep(unique(unlist(eval(parse(text = paste0("block_",
                                                                                                          length(breaks[idx_breaks]) + 1))))),
                                                                   times = nrow(eval(parse(text = paste0("block_",
                                                                                                         length(breaks[idx_breaks]) + 1)))))
            }
          } else {
            # if the changes are not consistent with the change list keep the old family
            HS6_temp$HS6plus[HS6_temp$CN8plus == fams[i]] <- rep(fams[i],
                                                                 times = nrow(eval(parse(text = paste0("block_",
                                                                                                       length(breaks[idx_breaks]) + 1)))))
          }
        }
      } else {
        # if the changes are not consistent nor equal keep the old family
        HS6_temp$HS6plus[HS6_temp$CN8plus == fams[i]] <- rep(fams[i],
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
    for (i in 1:length(fams)) {
      year_names <- colnames(HS6_temp)[grep("CN8_", colnames(HS6_temp))]
      block <- as.data.frame(HS6_temp[HS6_temp$CN8plus == fams[i], year_names])
      # check the code did not change across the years
      if (length(unique(unlist(block))) == 1) {
        # if yes - rewrite HS6plus
        HS6_temp$HS6plus[HS6_temp$CN8plus == fams[i]] <- rep(unique(unlist(eval(parse(text = "block")))),
                                                             times = nrow(eval(parse(text = "block"))))
      } else {
        # else keep the family
        HS6_temp$HS6plus[HS6_temp$CN8plus == fams[i]] <- rep(fams[i], times = nrow(eval(parse(text = "block"))))
      }
    }
  }

  if (progress) {
    print(paste0("Work in progress... Part ", mod_partb + 1,  "/" , mod_parte, ": 100%"))
  }

  CN8_over_time$HS6plus[CN8_over_time$family == 1] <- HS6_temp$HS6plus

  # get BEC and BEC_agr for HS6plus
  namescol <- c("BEC","BEC_agr")
  CN8_to_BEC_unique <- CN8_to_BEC[!duplicated(CN8_to_BEC$HS6),2:4]
  CN8_over_time$sort <- seq(1:nrow(CN8_over_time))
  temp <- merge(CN8_over_time[CN8_over_time$family == 1,!colnames(CN8_over_time) %in% namescol],
                CN8_to_BEC_unique, by.x = "HS6plus", by.y = "HS6", all.x=TRUE, sort = TRUE)
  temp <- temp[order(temp$sort),]

  CN8_over_time$BEC[CN8_over_time$family == 1] <- temp$BEC
  CN8_over_time$BEC_agr[CN8_over_time$family == 1] <- temp$BEC_agr
  rm(temp)

  CN8_over_time$family <- NULL
  CN8_over_time$sort <- NULL
  CN8_over_time$HS6 <- NULL
  CN8_over_time$new_code <- NULL

  # define BEC
  CN8_over_time$BEC_basic_class <- CN8_over_time$BEC
  CN8_over_time$BEC_basic_class[which(CN8_over_time$BEC_basic_class %in% c("41", "521"))] <- "Capital good"
  CN8_over_time$BEC_basic_class[CN8_over_time$BEC_basic_class %in% c("111", "121", "21",
                                                                     "22", "31", "322",
                                                                     "42", "53")] <- "Intermediate good"
  CN8_over_time$BEC_basic_class[CN8_over_time$BEC_basic_class %in% c("112", "122", "522",
                                                                     "61", "62", "63")] <- "Consumption good"
  CN8_over_time$BEC_basic_class[CN8_over_time$BEC_basic_class %in% c("51", "321")] <- "Consumption good / Intermediate good"
  CN8_over_time$BEC_basic_class[CN8_over_time$BEC_basic_class %in% c("7")] <- "not_defined"

  CN8_over_time <- CN8_over_time[!duplicated(CN8_over_time),]

  return(CN8_over_time)
}

