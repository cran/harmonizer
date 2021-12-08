## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)

## ---- include=FALSE-----------------------------------------------------------
# get the file path
file_path <- system.file("extdata", package = "harmonizer")

# get some examples
example_cn8 <- readRDS(paste0(file_path, "/CN8/CN8_2000.rds"))
example_pc8 <- readRDS(paste0(file_path, "/PC8/PC8_2010.rds"))
example_pc8_changes <- readRDS(paste0(file_path, "/PC8/PC8_2010_2011.rds"))
example_pc8_cn8_concordance <- readRDS(paste0(file_path, "/PC8/PC8_CN8_2010.rds"))

## ---- echo=FALSE--------------------------------------------------------------
head(example_cn8)

## ----comment='', echo=FALSE---------------------------------------------------
cat(readLines(paste0(file_path, "/CN8/CN8_concordances_1988_2020.csv"), n = 6, encoding = "UTF-8"), sep = '\n')

## ---- echo = FALSE------------------------------------------------------------
	  head(example_pc8)

## ---- echo = FALSE------------------------------------------------------------
head(example_pc8_changes)

## ---- echo = FALSE------------------------------------------------------------
head(example_pc8_cn8_concordance)
example_pc8_cn8_concordance[2400:2405, ]

## ----comment='', echo=FALSE---------------------------------------------------
cat(readLines(paste0(file_path, "/HS6/HS_1996_to_HS_1992.csv"), n = 6, encoding = "UTF-8"), sep = '\n')

## ----comment='', echo=FALSE---------------------------------------------------
cat(readLines(paste0(file_path, "/HS6toBEC/HS2002toBEC.csv"), n = 6), sep = '\n')

