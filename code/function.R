# 依存関係 --------------------------------------------------------------------

library(tidyverse)


# changeValue関数 -------------------------------------------------------------

changeValue <- function(dfNaive){
  
  df <- dfNaive %>% mutate(
    race = case_when(
      !is.na(race) & race != 1 & race != 2 ~ "OTHER",
      race == 1 ~ "HISPANIC",
      race == 2 ~ "BLACK",
    ),
    Isfemale = case_when(
      !is.na(sex) & sex != 1 ~ 1,
      sex == 1 ~ 0,
    )
  )
  
  df <- df %>% select(
    childID,
    motherID,
    race,
    Isfemale,
    firstSurveyYear,
    birthYear,
    birthOrder,
    # CSIBID02_XRND,
    # CSIBID03_XRND,
    # CSIBID04_XRND
  )
  
  return(df)
}

# renameDate関数 ------------------------------------------------------------

renameData <- function(dfNaive){
  df <- dfNaive %>% rename(
    childID = CPUBID_XRND,
    motherID = MPUBID_XRND,
    race = CRACE_XRND,
    sex = CSEX_XRND,
    firstSurveyYear = FSTYRAFT_XRND,
    birthYear = CYRB_XRND,
    birthOrder = BTHORDR_XRND,
  )
  df <- df %>% filter(
    birthYear > 1970,
    firstSurveyYear > 1970
  )
  
  return(df)
}


# makeMotherEducdf関数 ------------------------------------------------------

makeMotherEducdf <- function(df){
  df$HGCREV1994_1994 <- df$HGCREV1993_1993
  years <- c(seq(1979,1993),seq(1994,2016, by = 2))
  colnames <- unlist(map(years, ~Nameyear("HGCREV", .x)))
  df <- df %>% 
    rename_with(~ gsub("HGCREV(\\d+)_(\\d+)", "HGCREV\\1", .), starts_with("HGCREV")) %>%  
    pivot_longer(cols = colnames, 
                 names_to = "year",
                 names_prefix = "HGCREV",
                 values_to = "motherEduc")
  df$year = as.numeric(df$year)
  motherEducdf <- df %>% select(
    childID,
    motherID,
    firstSurveyYear,
    year,
    motherEduc
  ) %>% filter(
    year >= firstSurveyYear
  )
  return(motherEducdf)
}


# makeFamilySizedf関数 ------------------------------------------------------

makeFamilySizedf <- function(df){
  years <- c(seq(1979,1993),seq(1994,2014, by = 2))
  colnames <- unlist(map(years, ~Nameyear("NFAMEM", .x)))
  df <- df %>% 
    rename_with(~ gsub("NFAMEM(\\d+)_(\\d+)", "NFAMEM\\1", .), starts_with("NFAMEM")) %>%  
    pivot_longer(cols = colnames, 
                 names_to = "year",
                 names_prefix = "NFAMEM",
                 values_to = "familySize")
  df$year <- as.numeric(df$year)
  familySizedf <- df %>% select(
    childID,
    motherID,
    firstSurveyYear,
    year,
    familySize
  ) %>% filter(
    year >= firstSurveyYear
  )
  return(familySizedf)
}


# makePIATscoredf関数 --------------------------------------------------------

makePIATscoredf <- function(df){
  # PIAT-Math
  years <- seq(1986,2014,by = 2)
  colnames_math <- unlist(map(years, ~Nameyear("MATHZ", .x)))
  colnames_recog <- unlist(map(years, ~Nameyear("RECOGZ", .x)))
  colnames_vocab <- unlist(map(years, ~Nameyear("PPVTZ", .x)))
  colnames_compreh <- unlist(map(years, ~Nameyear("COMPZ", .x)))
  df_math <- df %>% 
    rename_with(~ gsub("MATHZ(\\d+)_(\\d+)", "MATHZ\\1", .), starts_with("MATHZ")) %>% 
    pivot_longer(cols = colnames_math, 
                 names_to = "PIATmath",
                 names_prefix = "MATHZ",
                 values_to = "PIATmath_v")
  df_math$PIATmath <- as.numeric(df_math$PIATmath)
  df_math <- df_math %>% select(
    childID,
    motherID,
    firstSurveyYear,
    PIATmath,
    PIATmath_v
  ) %>% rename(
    year = PIATmath
  ) %>% filter(
    year >= firstSurveyYear
  )
  
  # PIAT-Recognition
  df_recog <- df %>% 
    rename_with(~ gsub("RECOGZ(\\d+)_(\\d+)", "RECOGZ\\1", .), starts_with("RECOGZ")) %>% 
    pivot_longer(cols = colnames_recog, 
                 names_to = "PIATrecog",
                 names_prefix = "RECOGZ",
                 values_to = "PIATrecog_v")
  df_recog$PIATrecog <- as.numeric(df_recog$PIATrecog)
  df_recog <- df_recog %>% select(
    childID,
    motherID,
    firstSurveyYear,
    PIATrecog,
    PIATrecog_v
  ) %>% rename(
    year = PIATrecog
  ) %>% filter(
    year >= firstSurveyYear
  )
  
  # PIAT-Comprehension
  df_compreh <- df %>% 
    rename_with(~ gsub("COMPZ(\\d+)_(\\d+)", "COMPZ\\1", .), starts_with("COMPZ")) %>% 
    pivot_longer(cols = colnames_compreh, 
                 names_to = "PIATcompreh",
                 names_prefix = "COMPZ",
                 values_to = "PIATcompreh_v")
  df_compreh$PIATcompreh <- as.numeric(df_compreh$PIATcompreh)
  df_compreh <- df_compreh %>% select(
    childID,
    motherID,
    firstSurveyYear,
    PIATcompreh,
    PIATcompreh_v
  ) %>% rename(
    year = PIATcompreh
  ) %>% filter(
    year >= firstSurveyYear
  )
  
  # PIAT-Vocabulary
  df_vocab <- df %>% 
    rename_with(~ gsub("PPVTZ(\\d+)_(\\d+)", "PPVTZ\\1", .), starts_with("PPVTZ")) %>% 
    pivot_longer(cols = colnames_vocab, 
                 names_to = "PIATvocab",
                 names_prefix = "PPVTZ",
                 values_to = "PIATvocab_v")
  df_vocab$PIATvocab <- as.numeric(df_vocab$PIATvocab)
  df_vocab <- df_vocab %>% select(
    childID,
    motherID,
    firstSurveyYear,
    PIATvocab,
    PIATvocab_v
  ) %>% rename(
    year = PIATvocab
  ) %>% filter(
    year >= firstSurveyYear
  )
  
  piatScoredf <- reduce(list(df_math, df_recog, df_compreh, df_vocab), full_join, by = c('childID', 'motherID', 'year','firstSurveyYear')) %>% 
    select(
      childID,
      motherID,
      year,
      firstSurveyYear,
      PIATmath_v,
      PIATrecog_v,
      PIATcompreh_v,
      PIATvocab_v
    )
  
  return(piatScoredf)
}


# makeTroubleScoredf ------------------------------------------------------

makeTroubleScoredf <- function(df){
  years <- seq(2000,2014,by = 2)
  colnames_trouble <- unlist(map(years, ~Nameyear("BPIZ", .x)))
  df_trouble <- df %>%
    rename_with(~ gsub("BPIZ(\\d+)_(\\d+)", "BPIZ\\1", .), starts_with("BPIZ")) %>% 
    pivot_longer(cols = colnames_trouble, 
                 names_to = "year",
                 names_prefix = "BPIZ",
                 values_to = "TroubleScore")
  df_trouble$year <- as.numeric(df_trouble$year)
  df_trouble <- df_trouble %>% select(
    childID,
    motherID,
    firstSurveyYear,
    year,
    TroubleScore
  ) %>% filter(
    year >= firstSurveyYear
  )
}

# makeTransfer関数 ----------------------------------------------------------

makeTransferdf <- function(df){
  years <- seq(2006,2020,by = 2)
  colnames_transfer <- unlist(map(years, ~Nameyear("Q15-74D", .x)))
  df_transfer <- df %>% 
    rename_with(~ gsub("Q15-74D_(\\d+)", "Q15-74D\\1", .), starts_with("Q15-74D")) %>% 
    pivot_longer(cols = colnames_transfer, 
                 names_to = "year",
                 names_prefix = "Q15-74D",
                 values_to = "Transfer")
  df_transfer$year <- as.numeric(df_transfer$year)
  df_transfer <- df_transfer %>% select(
    childID,
    motherID,
    firstSurveyYear,
    year,
    Transfer
  ) %>% filter(
    year >= firstSurveyYear
  )
}


# makeAlcoholcomp関数 -----------------------------------------------------------

makeAlcoholcomp <- function(df){
  years <- seq(2006,2020,by = 2)
  colnames_alcoholComp <- unlist(map(years, ~Nameyear("YASR-5A", .x)))
  df_alcoholComp <- df %>% 
    rename_with(~ gsub("YASR-5A_(\\d+)", "YASR-5A\\1", .), starts_with("YASR-5A")) %>% 
    pivot_longer(cols = colnames_alcoholComp, 
                 names_to = "AlcoholComp",
                 names_prefix = "YASR-5A",
                 values_to = "AlcoholComp_v")
  df_alcoholComp$AlcoholComp <- as.numeric(df_alcoholComp$AlcoholComp)
  df_alcoholComp <- df_alcoholComp %>% select(
    childID,
    motherID,
    firstSurveyYear,
    AlcoholComp,
    AlcoholComp_v
  ) %>% rename(
    year = AlcoholComp
  ) %>% filter(
    year >= firstSurveyYear
  )
  return(df_alcoholComp)
}

# makeVariabledf関数 -----------------------------------------------------------

makeVariabledf <- function(df,VarName,Name,firstyear,lastyear){
  if (1993 < firstyear){
    years <- seq(firstyear,lastyear,by = 2)
  }
  else{
    years <- seq(firstyear,1993) + seq(1994,lastyear,by = 2)
  }
  colnames <- unlist(map(years, ~Name_year(VarName, .x)))
  prefix <- paste0(VarName,"_")
  df_t <- df %>% 
    pivot_longer(cols = colnames, 
                 names_to = "year",
                 names_prefix = prefix,
                 values_to = Name)
  df_t$year <- as.numeric(df_t$year)
  df_t <- df_t %>% select(
    childID,
    motherID,
    firstSurveyYear,
    year,
    !!rlang::sym(Name)
  ) %>% filter(
    year >= firstSurveyYear
  )
  return(df_t)
}


# Nameyear関数 --------------------------------------------------------------


Nameyear <- function(varName,year){
  name <- paste0(varName,as.character(year))
  return(name)
}

# Name_year関数 -------------------------------------------------------------

Name_year <- function(varName,year){
  name <- paste0(varName,"_",as.character(year))
  return(name)
}


# getColum関数 --------------------------------------------------------------

getColum <- function(df,colname){
  col <- which (names(df) == colname)
  return(col)
}


# labelcolums関数 -----------------------------------------------------------

labelcolums <- function(dfNaive){
  source("./data/label.R")
  df <- dfNaive %>% rename(!!!renamelist)
  return(df)
}

