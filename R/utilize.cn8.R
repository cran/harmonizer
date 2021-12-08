############################################################################
### Apply Harmonization & Working Paper Graphs/Analyses & New/Dropped Product Data
### 16-11-2021
### Christoph Baumgartner & Janette Walde
############################################################################

#' @importFrom stats na.omit
#' @export utilize.cn8

utilize.cn8 <- function(b, e, firm.data, harmonized.data = NULL,
                        progress = TRUE, output = "product.changes") {

  #########################
  ### check variable "output"
  #########################

  if(!output %in% c("product.changes", "merged.firm.data", "all")) {
    stop(paste0("'", output, "' is not a valid input for 'output'."))
  }

  #########################
  ### firm data check
  #########################
  if (!any(grep("firmID", colnames(firm.data)))) {
    stop("The entered firm data does not contain a column named 'firmID'. Please correct.")
  }
  if (!any(grep("year", colnames(firm.data)))) {
    stop("The entered firm data does not contain a column named 'year'. Please correct.")
  }
  if (!any(grep("CN8", colnames(firm.data)))) {
    stop("The entered firm data does not contain a column named 'CN8'. Please correct.")
  }
  if (class(firm.data$CN8) != "character") {
    stop("The column 'CN8' has a wrong class. Please convert to 'character'.")
  }

  firm_data <- firm.data
  firm_data$year <- as.integer(firm_data$year)
  firm_data$CN8 <- gsub(".", "", firm_data$CN8, fixed = TRUE)
  firm_data$CN8 <- gsub(" ", "", firm_data$CN8, fixed = TRUE)

  #########################
  ### define basic variables
  #########################

  years <- min(firm_data$year, na.rm = TRUE):max(firm_data$year, na.rm = TRUE)
  nbr_years <- length(years)

  #########################
  ### check if firm data fits input years
  #########################

  if (min(years) != b | max(years) != e) {
    stop("The entered years of interest do not fit the firm data. Please correct.")
  }

  #########################
  ### harmonized data check
  #########################
  if (is.null(harmonized.data)) {
    CN8_harm <- harmonize.cn8(b = b, e = e, progress = progress)
    mod_part <- 5

  } else {
    if (!is.data.frame(harmonized.data)) {
      stop("The entered harmonized data is not a dataframe. Please correct.")
    }
    if (!any(grep("CN8_", colnames(harmonized.data)))) {
      stop("Column names of the harmonized data are not correct. Please correct.")
    }
    if (ncol(harmonized.data) != (e - b + 8)) {
      stop(paste0("The entered harmonized data has the wrong number of columns. It has ", ncol(harmonized.data), ", while ", (e - b + 8), " are needed for the entered time period. Please correct."))
    }
    if (length(grep("CN8_", colnames(harmonized.data))) != (e - b + 1)) {
      stop("The entered harmonized data does not have enough columns which contain CN8 codes. It has ", length(grep("CN8_", colnames(harmonized.data))), " while ", (e - b + 1), " are needed for the entered time period. Please correct.")
    }
    if (suppressWarnings(!all(seq(from = b, to = e) == as.integer(substr(colnames(harmonized.data[grep("CN8_", colnames(harmonized.data))]), start = 5, stop = 8))))) {
      stop("The entered harmonized data does not match the given time period. Please correct.")
    }
    if (suppressWarnings(!all(years == as.integer(substr(colnames(harmonized.data[grep("CN8_", colnames(harmonized.data))]), start = 5, stop = 8))))) {
      stop("The entered harmonized data does not match the given firm data time period. Please correct.")
    }
    CN8_harm <- harmonized.data
    mod_part <- 1
  }

  #########################
  ### harmonize data
  #########################

  current_df <- as.matrix(CN8_harm[, paste0("CN8_", years)])

  temp1 <- subset(CN8_harm, select = c("CN8plus", "flag","flagyear",
                                       "HS6plus", "BEC",
                                       "BEC_agr", "BEC_basic_class"))
  # merge firm data with harmonized data
  get_harm <- function(x) {
    ifelse(length(which(current_df == x)) > 0,
           {# col and row in which the code appears first
           idx <- which(current_df == x, arr.ind=T)
           temp <- temp1[idx[1,1], ]},
           {temp <- rep(NA, ncol(temp1))})
    return(temp)
  }
  firm_data_list <- lapply(firm_data$CN8, get_harm)

  unlistindex <- diag(ncol(temp1))
  unlistindex <- ifelse(unlistindex == 1, TRUE, FALSE)
  firm_data$CN8plus <- unlist(firm_data_list)[unlistindex[1,]]
  firm_data$flag <- unlist(firm_data_list)[unlistindex[2,]]
  firm_data$flagyear <- unlist(firm_data_list)[unlistindex[3,]]
  firm_data$HS6plus <- unlist(firm_data_list)[unlistindex[4,]]
  firm_data$BEC <- unlist(firm_data_list)[unlistindex[5,]]
  firm_data$BEC_agr <- unlist(firm_data_list)[unlistindex[6,]]
  firm_data$BEC_basic_class <- unlist(firm_data_list)[unlistindex[7,]]

  ################################
  ### get all family codes (= syn)
  ################################

  syn_data <- CN8_harm[grep("f", CN8_harm$CN8plus), ]
  syn_codes <- syn_data[, grep("CN8_", colnames(syn_data))]

  syn_codes <- as.vector(t(syn_codes))
  syn_codes <- unique(syn_codes)

  firms <- firm_data[order(firm_data$firmID, firm_data$year),]

  #########################
  ### define variables
  #########################

  dataNP <- data.frame(matrix(NA, nrow(firms), 9))
  colnames(dataNP) <- c("firmID", "year", "period", "gap", "same_products", "new_products",
                        "dropped_products","number_of_products_t", "number_of_products_tplusone")

  yearly_nbr_firms <- rep(0, times = nbr_years - 1)
  yearly_add_prod <- rep(0, times = nbr_years - 1)
  yearly_drop_prod <- rep(0, times = nbr_years - 1)
  yearly_all_prod <- rep(0, times = nbr_years - 1)

  ###########################################################
  ### main function
  ###########################################################
  if (progress) {
    print(paste0("Work in progress... Part ", mod_part, "/", mod_part,": 0%"))
  }

  temp <- 0
  year_matrix <- cbind(c(1:(nbr_years - 1)), years[-1])
  firm_lvls <- levels(as.factor(firms$firmID))
  nbr_firms <- length(firm_lvls)
  # Loop over all firms to get new/changed number of products
  for (i in 1:nbr_firms){
    firm <- firm_lvls[i]
    datafirm <- firms[firms$firmID == firm, ]

    year_lvls <- levels(as.factor(datafirm$year))

    # firms which only exist in one year
    if (length(year_lvls) <= 1) {next}

    # Get all years sequentially to identify the changes in the product codes
    for (j in 1:(length(year_lvls) - 1)){
      temp <- temp + 1
      dataNP$firmID[temp] <- datafirm$firmID[1]
      year <- year_lvls[j]
      nextyear <- year_lvls[j + 1]

      # check if a gap (= year difference > 1) exists
      if (as.integer(nextyear) - as.integer(year) != 1) {dataNP$gap[temp] <- 1}
      else {dataNP$gap[temp] <- 0}

      idx_year <- year_matrix[, 2] == nextyear
      dataNP$year[temp] <- nextyear
      dataNP$period[temp] <- paste0(year, "-", nextyear)

      # idyear = codes in year t; idnextyear = codes in year t + 1
      idyear <- unique(na.omit(datafirm$CN8plus[datafirm$year == year]))
      idnextyear <- unique(na.omit(datafirm$CN8plus[datafirm$year == nextyear]))

      # In two consecutive years exist product codes for a specific firm
      if (length(idyear) != 0 & length(idnextyear) != 0) {
        dataNP$same_products[temp] <- sum(idyear %in% idnextyear)
        # typos/not recognized codes in the firm data are not considered as no CN8plus exists
        dataNP$dropped_products[temp] <- length(idyear) - dataNP$same_products[temp]
        dataNP$new_products[temp] <- length(idnextyear) - dataNP$same_products[temp]
        dataNP$number_of_products_t[temp] <- length(idyear)
        dataNP$number_of_products_tplusone[temp] <- length(idnextyear)
      } else if (length(idyear) != 0 & length(idnextyear) == 0) {
        # In the second year of the two consecutive years no product codes exist for a specific firm
        dataNP$same_products[temp] <- 0
        dataNP$dropped_products[temp] <- length(idyear)
        dataNP$new_products[temp] <- 0
        dataNP$number_of_products_t[temp] <- length(idyear)
        dataNP$number_of_products_tplusone[temp] <- 0
      } else if (length(idyear) == 0 & length(idnextyear) != 0) {
        # In the first year of the two consecutive years no product codes exist for a specific firm
        dataNP$same_products[temp] <- 0
        dataNP$dropped_products[temp] <- 0
        dataNP$new_products[temp] <- length(idnextyear)
        dataNP$number_of_products_t[temp] <- 0
        dataNP$number_of_products_tplusone[temp] <- length(idnextyear)
      }
    }
    if (progress) {
      if (i %% 200 == 0) {
        print(paste0("Work in progress... Part ", mod_part, "/", mod_part,": ", round(i/nbr_firms, 3) * 100, "%"))
      }
    }
  }
  dataNP <- dataNP[1:temp,]

  if (progress) {
    print(paste0("Work in progress... Part ", mod_part, "/", mod_part,": 100%"))
  }

  colnames(dataNP) <- c("firmID", "period_UL", "period", "gap", "same_products", "new_products",
                        "dropped_products","nbr_of_products_period_LL", "nbr_of_products_period_UL")

  ### define output
  if (output == "product.changes") {
    return(dataNP)
  } else if (output == "merged.firm.data") {
    return(firm_data)
  } else {
    return(list(dataNP, firm_data))
  }
}


