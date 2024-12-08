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
    ChildID,
    MotherID,
    race,
    Isfemale,
    FirstSurveyYear,
    BirthYear,
    # BirthOrder,
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
    ChildID = CPUBID_XRND,
    MotherID = MPUBID_XRND,
    race = CRACE_XRND,
    sex = CSEX_XRND,
    FirstSurveyYear = FSTYRAFT_XRND,
    BirthYear = CYRB_XRND,
    # BirthOrder = BTHORDR_XRND,
    motherAgeAtBirth = MAGEBIR_XRND,
  )
  df <- df %>% filter(
    BirthYear > 1970,
    FirstSurveyYear > 1970
  )
  
  return(df)
}


# makeMotherEducdf関数 ------------------------------------------------------

makeMotherEducdf <- function(df){
  df$HGCREV1994_1994 <- df$HGCREV1993_1993
  Years <- c(seq(1979,1993),seq(1994,2016, by = 2))
  colnames <- unlist(map(Years, ~NameYear("HGCREV", .x)))
  df <- df %>% 
    rename_with(~ gsub("HGCREV(\\d+)_(\\d+)", "HGCREV\\1", .), starts_with("HGCREV")) %>%  
    pivot_longer(cols = colnames, 
                 names_to = "Year",
                 names_prefix = "HGCREV",
                 values_to = "motherEduc")
  df$Year = as.numeric(df$Year)
  motherEducdf <- df %>% select(
    ChildID,
    MotherID,
    FirstSurveyYear,
    Year,
    motherEduc
  ) %>% filter(
    Year >= FirstSurveyYear
  )
  return(motherEducdf)
}


# makeFamilySizedf関数 ------------------------------------------------------

makeFamilySizedf <- function(df){
  Years <- c(seq(1979,1993),seq(1994,2014, by = 2))
  colnames <- unlist(map(Years, ~NameYear("NFAMEM", .x)))
  df <- df %>% 
    rename_with(~ gsub("NFAMEM(\\d+)_(\\d+)", "NFAMEM\\1", .), starts_with("NFAMEM")) %>%  
    pivot_longer(cols = colnames, 
                 names_to = "Year",
                 names_prefix = "NFAMEM",
                 values_to = "familySize")
  df$Year <- as.numeric(df$Year)
  familySizedf <- df %>% select(
    ChildID,
    MotherID,
    FirstSurveyYear,
    Year,
    familySize
  ) %>% filter(
    Year >= FirstSurveyYear
  )
  return(familySizedf)
}


# makePIATscoredf関数 --------------------------------------------------------

makePIATscoredf <- function(df){
  # PIAT-Math
  Years <- seq(1986,2014,by = 2)
  colnames_math <- unlist(map(Years, ~NameYear("MATHZ", .x)))
  colnames_recog <- unlist(map(Years, ~NameYear("RECOGZ", .x)))
  colnames_vocab <- unlist(map(Years, ~NameYear("PPVTZ", .x)))
  colnames_compreh <- unlist(map(Years, ~NameYear("COMPZ", .x)))
  df_math <- df %>% 
    rename_with(~ gsub("MATHZ(\\d+)_(\\d+)", "MATHZ\\1", .), starts_with("MATHZ")) %>% 
    pivot_longer(cols = colnames_math, 
                 names_to = "PIATmath",
                 names_prefix = "MATHZ",
                 values_to = "PIATmath_v")
  df_math$PIATmath <- as.numeric(df_math$PIATmath)
  df_math <- df_math %>% select(
    ChildID,
    MotherID,
    FirstSurveyYear,
    PIATmath,
    PIATmath_v
  ) %>% rename(
    Year = PIATmath
  ) %>% filter(
    Year >= FirstSurveyYear
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
    ChildID,
    MotherID,
    FirstSurveyYear,
    PIATrecog,
    PIATrecog_v
  ) %>% rename(
    Year = PIATrecog
  ) %>% filter(
    Year >= FirstSurveyYear
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
    ChildID,
    MotherID,
    FirstSurveyYear,
    PIATcompreh,
    PIATcompreh_v
  ) %>% rename(
    Year = PIATcompreh
  ) %>% filter(
    Year >= FirstSurveyYear
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
    ChildID,
    MotherID,
    FirstSurveyYear,
    PIATvocab,
    PIATvocab_v
  ) %>% rename(
    Year = PIATvocab
  ) %>% filter(
    Year >= FirstSurveyYear
  )
  
  piatScoredf <- reduce(list(df_math, df_recog, df_compreh, df_vocab), full_join, by = c('ChildID', 'MotherID', 'Year','FirstSurveyYear')) %>% 
    select(
      ChildID,
      MotherID,
      Year,
      FirstSurveyYear,
      PIATmath_v,
      PIATrecog_v,
      PIATcompreh_v,
      PIATvocab_v
    )
  
  return(piatScoredf)
}


# makeTroubleScoredf ------------------------------------------------------

makeTroubleScoredf <- function(df){
  Years <- seq(2000,2014,by = 2)
  colnames_trouble <- unlist(map(Years, ~NameYear("BPIZ", .x)))
  df_trouble <- df %>%
    rename_with(~ gsub("BPIZ(\\d+)_(\\d+)", "BPIZ\\1", .), starts_with("BPIZ")) %>% 
    pivot_longer(cols = colnames_trouble, 
                 names_to = "Year",
                 names_prefix = "BPIZ",
                 values_to = "TroubleScore")
  df_trouble$Year <- as.numeric(df_trouble$Year)
  df_trouble <- df_trouble %>% select(
    ChildID,
    MotherID,
    FirstSurveyYear,
    Year,
    TroubleScore
  ) %>% filter(
    Year >= FirstSurveyYear
  )
}

# makeTransfer関数 ----------------------------------------------------------

makeTransferdf <- function(df){
  Years <- seq(2006,2020,by = 2)
  colnames_transfer <- unlist(map(Years, ~NameYear("Q15-74D", .x)))
  df_transfer <- df %>% 
    rename_with(~ gsub("Q15-74D_(\\d+)", "Q15-74D\\1", .), starts_with("Q15-74D")) %>% 
    pivot_longer(cols = colnames_transfer, 
                 names_to = "Year",
                 names_prefix = "Q15-74D",
                 values_to = "Transfer")
  df_transfer$Year <- as.numeric(df_transfer$Year)
  df_transfer <- df_transfer %>% select(
    ChildID,
    MotherID,
    FirstSurveyYear,
    Year,
    Transfer
  ) %>% filter(
    Year >= FirstSurveyYear
  )
}


# makeAlcoholcomp関数 -----------------------------------------------------------

makeAlcoholcomp <- function(df){
  Years <- seq(2006,2020,by = 2)
  colnames_alcoholComp <- unlist(map(Years, ~NameYear("YASR-5A", .x)))
  df_alcoholComp <- df %>% 
    rename_with(~ gsub("YASR-5A_(\\d+)", "YASR-5A\\1", .), starts_with("YASR-5A")) %>% 
    pivot_longer(cols = colnames_alcoholComp, 
                 names_to = "AlcoholComp",
                 names_prefix = "YASR-5A",
                 values_to = "AlcoholComp_v")
  df_alcoholComp$AlcoholComp <- as.numeric(df_alcoholComp$AlcoholComp)
  df_alcoholComp <- df_alcoholComp %>% select(
    ChildID,
    MotherID,
    FirstSurveyYear,
    AlcoholComp,
    AlcoholComp_v
  ) %>% rename(
    Year = AlcoholComp
  ) %>% filter(
    Year >= FirstSurveyYear
  )
  return(df_alcoholComp)
}

# makeVariabledf関数 -----------------------------------------------------------

makeVariabledf <- function(df,VarName,Name,firstYear,lastYear){
  if (1993 < firstYear){
    Years <- seq(firstYear,lastYear,by = 2)
  }
  else{
    Years <- seq(firstYear,1993) + seq(1994,lastYear,by = 2)
  }
  colnames <- unlist(map(Years, ~Name_Year(VarName, .x)))
  prefix <- paste0(VarName,"_")
  df_t <- df %>% 
    pivot_longer(cols = colnames, 
                 names_to = "Year",
                 names_prefix = prefix,
                 values_to = Name)
  df_t$Year <- as.numeric(df_t$Year)
  df_t <- df_t %>% select(
    ChildID,
    MotherID,
    FirstSurveyYear,
    Year,
    !!rlang::sym(Name)
  ) %>% filter(
    Year >= FirstSurveyYear
  )
  return(df_t)
}


# NameYear関数 --------------------------------------------------------------


NameYear <- function(varName,Year){
  name <- paste0(varName,as.character(Year))
  return(name)
}

# Name_Year関数 -------------------------------------------------------------

Name_Year <- function(varName,Year){
  name <- paste0(varName,"_",as.character(Year))
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
  result_female <- t.test(`女ダミー` ~ `譲渡over50%ダミー(Y)`, df_T ) %>% tidy()
  result_black <- t.test(`黒人ダミー` ~ `譲渡over50%ダミー(Y)`, df_T ) %>% tidy()
  result_hispanic <- t.test(`ヒスパニックダミー` ~ `譲渡over50%ダミー(Y)`, df_T ) %>% tidy()
  result_urban <- t.test(`都市居住ダミー` ~ `譲渡over50%ダミー(Y)`, df_T ) %>% tidy()
  result_educ <- t.test(`教育年数` ~ `譲渡over50%ダミー(Y)`, df_T ) %>% tidy()
  result_birth <- t.test(`弟妹の数` ~ `譲渡over50%ダミー(Y)`, df_T ) %>% tidy()
  result_order <- t.test(`出生順位` ~ `譲渡over50%ダミー(Y)`, df_T ) %>% tidy()
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
      df$年下の兄弟との年齢差,
      breaks = seq(0, 30, 1),
      main = "年齢差の分布",
      xlab = "年齢差",
      ylab = "人数"
    )
  } else if (name_str == "兄弟サイズ") {
    hist(
      df$兄弟サイズ,
      main = "兄弟サイズの分布",
      xlab = "兄弟サイズ",
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
    "子供ID" = ChildID,
    "母親ID" = MotherID,
    "調査開始年度" = FirstSurveyYear,
    "調査年度" = Year,
    "出生年" = BirthYear,
    "子供の年齢" = Age,
    "譲渡" = MainTransfer,
    "譲渡over50%ダミー" = IsTransfer_over50,
    "譲渡ダミー" = IsTransfered,
    "同居ダミー" = IsLiveTogether,
    "女ダミー"= Isfemale,
    "黒人ダミー" = isBlack,
    "ヒスパニックダミー" = isHispanic,
    "都市住みダミー" = IsUrban,
    "出生順位" = BirthOrder,
    "第1子ダミー" = Is1th,
    "第2子ダミー" = Is2th,
    "第3子ダミー" = Is3th,
    "第4子ダミー" = Is4th,
    "第5子以上ダミー" = Is5th_OR_more,
    "兄弟サイズ" = N_siblings,
    "弟妹の数" = NYG_ijt,
    "年下の兄弟との年齢差" = AGAP_ijt,
    "教育年数(母)" = motherEduc,
    "母親の年齢" = MotherAge,
    "家族収入(単位は？)" = FamilyIncome,
    "物質使用経験(総合)ダミー" = SubstanceExp,
    "飲酒経験ダミー" = alcoholExp,
    "大麻経験ダミー" = marijuanaExp,
    "喫煙経験ダミー" = tabacoExp,
    "乱用経験ダミー" = isAbuse,
    "未成年ダミー" = isU18,
    "教育年数" = educ,
    # "高校中退ダミー" = IsNoneGraduate,
    "リスク_好みダミー" = IsEnjoyRisk,
    "未成年使用ダミー(総合)ダミー" = U18_SubstanceExp,
    "未成年飲酒ダミー" = U18_alcExp,
    "未成年大麻使用ダミー" = U18_mariExp,
    "未成年喫煙ダミー" = U18_tabcExp,
    "未成年乱用ダミー" = U18_Abuse
  ) %>% select(
    # "子供ID",
    # "母親ID",
    "子供の年齢",
    "未成年ダミー",
    # "母親の年齢",
    # "出生年",
    # "調査年度",
    # "調査開始年度",
    "女ダミー",
    "兄弟サイズ",
    "弟妹の数",
    "年下の兄弟との年齢差",
    "譲渡over50%ダミー",
    # "譲渡ダミー",
    # "同居ダミー",
    "未成年使用ダミー(総合)ダミー",
    "未成年飲酒ダミー",
    "未成年大麻使用ダミー",
    "未成年喫煙ダミー",
    "物質使用経験(総合)ダミー",
    "飲酒経験ダミー",
    "大麻経験ダミー",
    "喫煙経験ダミー",
    "リスク_好みダミー",
    "都市住みダミー",
    "第1子ダミー",
    "第2子ダミー",
    "第3子ダミー",
    "第4子ダミー",
    "第5子以上ダミー"
  )
  return(f_df)
}


# makeSatatic_df_2 ---------------------------------------------------------

makeStatic_df2 <- function(df){
  f_df <- df %>% rename(
    "子供ID" = ChildID,
    "母親ID" = MotherID,
    "調査開始年度" = FirstSurveyYear,
    "調査年度" = Year,
    "出生年" = BirthYear,
    "子供の年齢" = Age,
    "譲渡(Y)" = MainTransfer,
    "譲渡over50%ダミー(Y)" = IsTransfer_over50,
    "譲渡ダミー(Y)" = IsTransfered,
    "同居ダミー(Y)" = IsLiveTogether,
    "女ダミー"= Isfemale,
    "黒人ダミー" = isBlack,
    "ヒスパニックダミー" = isHispanic,
    "都市住みダミー" = IsUrban,
    "出生順位" = BirthOrder,
    "第1子ダミー" = Is1th,
    "第2子ダミー" = Is2th,
    "第3子ダミー" = Is3th,
    "第4子ダミー" = Is4th,
    "第5子以上ダミー" = Is5th_OR_more,
    "兄弟サイズ" = N_siblings,
    "弟妹の数" = NYG_ij,
    "未成年の弟妹の数" = NYG_ijt,
    "年下の兄弟との年齢差" = AGAP_ij,
    "未成年の下の兄弟との年齢差" = AGAP_ijt,
    "教育年数(母)" = motherEduc,
    "母親の年齢" = MotherAge,
    "家族収入(単位は？)" = FamilyIncome,
    "物質使用経験(総合)ダミー" = SubstanceExp,
    "飲酒経験ダミー" = alcoholExp,
    "大麻経験ダミー" = marijuanaExp,
    "喫煙経験ダミー" = tabacoExp,
    "乱用経験ダミー" = isAbuse,
    "未成年ダミー" = isU18,
    "教育年数" = educ,
    "高校中退ダミー" = IsNoneGraduate,
    "大学生ダミー" = isCollegeStudent,
    "リスク_好みダミー" = IsEnjoyRisk,
    "未成年使用ダミー(総合)" = U18_SubstanceExp,
    "未成年飲酒ダミー" = U18_alcExp,
    "未成年大麻使用ダミー" = U18_mariExp,
    "未成年喫煙ダミー" = U18_tabcExp,
    "未成年乱用ダミー" = U18_Abuse,
    "未成年物質使用経験(予測)" = exSubUse,
    "未成年飲酒ダミー(予測)" = exSubUse_alcohol,
    "未成年大麻使用ダミー(予測)" = exSubUse_marijuana,
    "未成年喫煙ダミー(予測)" = exSubUse_tabaco,
  ) %>% select(
    "子供ID",
    "母親ID",
    "子供の年齢",
    "未成年ダミー",
    "母親の年齢",
    "出生年",
    "調査年度",
    "調査開始年度",
    "女ダミー",
    "兄弟サイズ",
    "弟妹の数",
    "年下の兄弟との年齢差",
    "未成年の弟妹の数",
    "未成年の下の兄弟との年齢差",
    "出生順位",
    "第1子ダミー",
    "第2子ダミー",
    "第3子ダミー",
    "第4子ダミー",
    "第5子以上ダミー",
    "譲渡over50%ダミー(Y)",
    "譲渡ダミー(Y)",
    "同居ダミー(Y)",
    "未成年使用ダミー(総合)",
    "未成年飲酒ダミー",
    "未成年大麻使用ダミー",
    "未成年喫煙ダミー",
    "物質使用経験(総合)",
    "家族収入(単位は？)",
    "黒人ダミー",
    "ヒスパニックダミー",
    "リスク_好みダミー",
    "高校中退ダミー",
    "大学生ダミー",
    "未成年物質使用経験(予測)",
    "未成年飲酒ダミー(予測)",
    "未成年大麻使用ダミー(予測)",
    "未成年喫煙ダミー(予測)",
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


# RemoveFiles関数 -----------------------------------------------------------

RemoveFiles <- function(){
  # 不要なファイルを削除（PDF以外）
  files_to_delete <- list.files("./outputs/pdf", pattern = "\\.(aux|log|gz)$", full.names = TRUE)
  file.remove(files_to_delete)
}


# OutputPDF関数 -------------------------------------------------------------

AddHeader <- function(result){
  header <- "\\documentclass{article}\n\\usepackage{booktabs}\n\\usepackage{fontspec}\n\\usepackage{threeparttable}\n\\usepackage{luatexja}\n\\begin{document}\n"
  footer <- "\\end{document}\n"
  output_dir <- "./outputs/"
  
  output_tex <- paste0(output_dir, result,".tex")
  
  tex_content <- readLines(output_tex)
  writeLines(c(header, tex_content, footer), con = output_tex)
}

# OutputPDF関数 -------------------------------------------------------------

AddHeader_wide <- function(result){
  header <- "\\documentclass{article}\n\\usepackage{booktabs}\n\\usepackage{fontspec}\n\\usepackage{threeparttable}\n\\usepackage{luatexja}\n\\usepackage{lscape}\n\n\\begin{document}\n\\begin{landscape}\n"
  footer <- "\\end{landscape}\n\\end{document}\n"
  output_dir <- "./outputs/"
  
  output_tex <- paste0(output_dir, result,".tex")
  
  tex_content <- readLines(output_tex)
  writeLines(c(header, tex_content, footer), con = output_tex)
}

