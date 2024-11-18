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
    motherAgeAtBirth
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
    motherAgeAtBirth = MAGEBIR_XRND,
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

# Ttest関数 -----------------------------------------------------------
Ttest <- function(df_T,f_name){
  # 各t検定の結果を取得
  library(broom)
  result_female <- t.test(`女ダミー` ~ `移転over50%ダミー(Y)`, df_T ) %>% tidy()
  result_black <- t.test(`黒人ダミー` ~ `移転over50%ダミー(Y)`, df_T ) %>% tidy()
  result_hispanic <- t.test(`ヒスパニックダミー` ~ `移転over50%ダミー(Y)`, df_T ) %>% tidy()
  result_urban <- t.test(`都市居住ダミー` ~ `移転over50%ダミー(Y)`, df_T ) %>% tidy()
  result_educ <- t.test(`教育年数` ~ `移転over50%ダミー(Y)`, df_T ) %>% tidy()
  result_birth <- t.test(`弟妹の数` ~ `移転over50%ダミー(Y)`, df_T ) %>% tidy()
  result_order <- t.test(`出生順位` ~ `移転over50%ダミー(Y)`, df_T ) %>% tidy()
  # 各結果をリストにまとめる
  results_list <- c(
    female = result_female,
    black = result_black,
    hispanic = result_hispanic,
    urban = result_urban,
    educ = result_educ,
    birth = result_birth,
    order = result_order
  )
  
  all_results_df <- rbind(
    data.frame(Test = "女性ダミー", result_female),
    data.frame(Test = "黒人ダミー", result_black),
    data.frame(Test = "ヒスパニックダミー", result_hispanic),
    data.frame(Test = "都市居住ダミー", result_urban),
    data.frame(Test = "教育年数", result_educ),
    data.frame(Test = "弟妹の数", result_birth),
    data.frame(Test = "出生順位", result_order)
  )
  
  file_path <- "./outputs/"
  file_path <- paste(file_path,as.character(f_name),".csv")
  if (file.exists(file_path)) {
    file.remove(file_path)
  }
  # 
  # エクセルファイルに書き込む
  write.csv(all_results_df, file_path)
  library(readr)
  dfname <- "T_"
  dfname <- paste(dfname,as.character(f_name))
  dfname<- read_csv(file_path)
  View(dfname)
}


# hist_f関数 ----------------------------------------------------------------

hist_f <- function(df, name) {
  # nameを文字列として扱う
  
  # いずれのパターンでも可
  # hist_f(fclean_alldf, "出生順位")      # 文字列として
  # hist_f(fclean_alldf, `出生順位`)      # バッククォートで
  # hist_f(fclean_alldf, 出生順位)        # シンボルとして
  
  # deparse(substitute(name))　を使って、変数名を文字列として使う
  # if() 条件1 else 条件２
  name_str <- if(is.character(name)) name else deparse(substitute(name))
  
  if (name_str == "出生順位") {
    hist(
      df$出生順位,
      breaks = seq(0, 11, 1),
      main = "出生順位の分布",
      xlab = "出生順位",
      ylab = "人数"
    )
  } else if (name_str == "弟妹の数") {
    hist(
      df$弟妹の数,
      breaks = seq(0, 11, 1),
      main = "出生順位の分布-2",
      xlab = "弟妹の数",
      ylab = "人数"
    )
  } else if (name_str == "下との年齢差") {
    hist(
      df$下との年齢差,
      breaks = seq(0, 30, 1),
      main = "年齢差の分布",
      xlab = "年齢差",
      ylab = "人数"
    )
  } else if (name_str == "家族サイズ") {
    hist(
      df$家族サイズ,
      main = "家族サイズの分布",
      xlab = "家族サイズ",
      ylab = "家庭数"
    )
  } else if (name_str == "年齢") {
    hist(
      df$年齢,
      breaks = seq(min(df$年齢), max(df$年齢), 1),  # 年齢に応じた適切な区間
      main = "年齢の分布",
      xlab = "年齢",
      ylab = "人数"
    )
  } else {
    hist(
      df[[name_str]],
      main = paste0(name_str, "の分布"),
      xlab = name_str,
      ylab = "人数"
    )
  }
}



# makeStatic_df関数 ---------------------------------------------------------


makeStatic_df <- function(df){
  f_df <- df %>% rename(
    "子供ID" = childID,
    "母親ID" = motherID,
    "調査開始年度" = firstSurveyYear,
    "調査年度" = year,
    "出生年" = birthYear,
    "年齢" = age,
    "移転(Y)" = Transfer,
    "移転over50%ダミー(Y)" = IsTransfer_over50,
    "女ダミー"= Isfemale,
    "黒人ダミー" = IsBlack,
    "ヒスパニックダミー" = IsHispanic,
    "都市住みダミー" = IsUrban,
    "出生順位" = birthOrder,
    "第1子ダミー" = Is1th,
    "第2子ダミー" = Is2th,
    "第3子ダミー" = Is3th,
    "第4子ダミー" = Is4th,
    "兄弟の数" = N_siblings,
    "弟妹の数" = NYS,
    "未成年の弟妹の数" = U18NYS,
    "下との年齢差" = age_gap,
    "母親の年齢" = motherAge,
    "教育年数(母)" = motherEduc,
    "家族サイズ" = familySize,
    "家族収入" = FamilyIncome,
    "数学スコア" = PIATmath_v,
    "読解力スコア" = PIATrecog_v,
    "読解把握スコア" = PIATcompreh_v,
    "非行スコア" = TroubleScore,
    "語彙力" = PIATvocab_v,
    "物質使用経験(総合)" = substanceExp,
    "飲酒経験" = AlcoholExp,
    "大麻経験" = MarijuanaExp,
    "喫煙経験" = TabacoExp,
    "乱用経験" = IsAbuse,
    "18歳以下ダミー" = IsU18,
    "教育年数" = educ,
    "高校卒業ダミー" = IsGraduate,
    "大学生ダミー" = IsCollegeStudent,
    "リスク_好みダミー" = IsEnjoyRisk,
    "未成年使用ダミー(総合)" = u18_substanceExp,
    "未成年飲酒ダミー" = u18_alcExp,
    "未成年大麻使用ダミー" = u18_mariExp,
    "未成年喫煙ダミー" = u18_tabcExp,
    "未成年乱用ダミー" = u18_Abuse
  ) %>% select(
    "子供ID",
    "母親ID",
    "出生年",
    "年齢",
    # "教育年数",
    "調査開始年度",
    "調査年度",
    "女ダミー",
    "母親の年齢",
    # "移転over50%ダミー(Y)",
    "出生順位",
    "第1子ダミー",
    "第2子ダミー",
    "第3子ダミー",
    "第4子ダミー",
    # "兄弟の数",
    "未成年の弟妹の数",
    "弟妹の数",
    "下との年齢差",
    "未成年使用ダミー(総合)",
    # "未成年飲酒ダミー",
    # "未成年大麻使用ダミー",
    # "未成年喫煙ダミー",
    # "未成年乱用ダミー",
    "物質使用経験(総合)",
    # "家族サイズ",
    "家族収入",
    "18歳以下ダミー",
    "都市住みダミー",
    # "黒人ダミー",
    # "ヒスパニックダミー",
    # "リスク_好みダミー"
  )
  return(f_df)
}


# IsColname関数 -------------------------------------------------------------

IsColname <- function(word,df){
  list <- names(df)
  if(word %in% list){
    index <- grep(word,list)
    return(index)
  }
  else{
    return ("none")
  }
}