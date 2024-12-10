
# コーディング規則

# --->分析に使う変数は先頭大文字

# ショートカットキー一覧 -------------------------------------------------------------

# ctrl + shift + h で　作業ファイル開ける
# ctrl + shift + c コメントアウト
# ctrl + shift + r ラベル
# ctrl + shift + s sourceコマンドと同じ
# ctrl + shift + j ctrl + f と同じ
# ctrl + shift + o ラベル先に移動
# ctrl + shift + d 行をコピー


# 依存関係 --------------------------------------------------------------------

library(tidyverse)
library(modelsummary)
library(gtsummary)

# データのインポート ---------------------------------------------------------------

source("C:/Users/watan/2024fall_NakamuroSato/data/tatsumi-data20241116.R")
setwd("C:/Users/watan/2024fall_NakamuroSato/")
source("./code/function.R")

dfNaive <- new_data


# 変数作成用中間テーブルの作成 ----------------------------------------------------------

dfNaive1 <- renameData(dfNaive)
motherEducdf <- makeMotherEducdf(dfNaive1)
familySizedf <- makeFamilySizedf(dfNaive1)
PIATScoredf <- makePIATscoredf(dfNaive1)
TroubleScoredf <- makeTroubleScoredf(dfNaive1)
MainTransferdf <- makeVariabledf(dfNaive1,"Q15-74D","MainTransfer",2006,2020)
IsTransfereddf <- makeVariabledf(dfNaive1,"Q15-74","IsTransfered",2006,2020)
IsLiveTogetherdf <- makeVariabledf(dfNaive1,"Q15-74B","IsLiveTogether",2006,2020)
alcoholExpdf <- makeVariabledf(dfNaive1,"YASR-5A","Alcoholever",2000,2020)
marijuanaExpdf <- makeVariabledf(dfNaive1,"YASR-24A","marijuanaExp",1998,2020)
FirstUseMarijuanadf <- makeVariabledf(dfNaive1,"YASR-25","firstUseMarijuana",1994,2020)
tabacoExpdf <- makeVariabledf(dfNaive1,"YASR-19A","tabacoExp",1998,2020)
Gradedf <- makeVariabledf(dfNaive1,"Q4-2","educ",1994,2020)
IsGraduatedf <- makeVariabledf(dfNaive1,"Q4-28","IsGraduate",1994,2020)
EnjoyRiskdf <- makeVariabledf(dfNaive1,"Q16-5I-D","EnjoyRisk",1994,2020)
Urbandf <- makeVariabledf(dfNaive1,"URBAN-RURAL","Urban",1994,2020)
AbuseCasedf <- makeVariabledf(dfNaive1,"YASR-71A~000025","AbuseCase",2016,2020)
FamilyIncomedf <- makeVariabledf(dfNaive1,"Q15-141-TOP","FamilyIncome",2002,2014)
MachineCheckIsU19 <- makeVariabledf(dfNaive1,"YASR-4BA","MachineCheckIsU19",2002,2014)

# データ整形 -------------------------------------------------------------------

# データの結合
staderd_df <- changeValue(dfNaive1)

masterdf <- left_join(MainTransferdf, staderd_df,
                      by = c('ChildID', 'MotherID', 'FirstSurveyYear'))
covar_df <- reduce(list(
                        motherEducdf,
                        familySizedf,
                        IsTransfereddf,
                        IsLiveTogetherdf,
                        PIATScoredf,
                        TroubleScoredf,
                        alcoholExpdf,
                        marijuanaExpdf,
                        FirstUseMarijuanadf,
                        tabacoExpdf,
                        Gradedf,
                        IsGraduatedf,
                        EnjoyRiskdf,
                        Urbandf,
                        AbuseCasedf,
                        FamilyIncomedf,
                        MachineCheckIsU19
                      ), full_join,
                      by = c('ChildID', 'MotherID', 'Year', 'FirstSurveyYear'))
raw_df <- left_join(masterdf,covar_df,by = c('ChildID', 'MotherID','Year','FirstSurveyYear')) %>% mutate(
    FirstSurveyYear = FirstSurveyYear ,
    Year = Year,
    Age = Year - BirthYear
  )

# 変数の作成 -------------------------------------------------------------------

alldf1 <- raw_df

# 出生順位に関する変数 --------------------------------------------------------------

alldf2 <- alldf1 %>%  
  group_by(MotherID,Year) %>% 
  mutate(
    
    BirthOrder = rank(BirthYear),
    # 兄弟の数->資源希釈
    N_siblings = max(BirthOrder),
    
  ) %>% ungroup %>% 
  group_by(MotherID,Year) %>%
  mutate(
    
    # 下の兄弟の数
    NYG_ijt = N_siblings - BirthOrder,
    NYG = ifelse(NYG_ijt > 2, 3, NYG_ijt),
    
    # 下の兄弟との差
    AGAP_ijt = lag(BirthYear, order_by = NYG_ijt) - BirthYear,
    
    # ※NA処理
    AGAP_ijt = ifelse(is.na(AGAP_ijt),0,AGAP_ijt),
    AGAP_ijt = ifelse(is.infinite(AGAP_ijt),0,AGAP_ijt),
    AGAP = ifelse(AGAP_ijt > 9, 10, AGAP_ijt),
    
    # その他共変量
    FamilyIncome = mean(FamilyIncome, na.rm = TRUE),
    
    familySize = mean(familySize, na.rm = TRUE),
    
    EnjoyRisk = mean(EnjoyRisk, na.rm = TRUE),
    MaxEnjoyRisk = max(EnjoyRisk, na.rm = TRUE),

    # 兄弟の数が欠けていないかVaildate
    Valid_Nsiblings = ifelse(n() == max(BirthOrder),1,0),
    
  ) %>% 
  ungroup() %>% group_by(MotherID,Year,BirthYear) %>%
  mutate(
          # 双子Flag
          isdouble = n()
        ) %>% filter(
                      # 欠けている兄弟を排除
                      Valid_Nsiblings == 1,
                      # 双子を除去
                      isdouble == 1
                     ) %>% ungroup() 

summary <- datasummary(All(alldf2) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max)),
                       data = alldf2,
                       na.rm = TRUE,
                       fmt = 3,
)
summary


# メインの共変量作成 ---------------------------------------------------------------


alldf3 <- alldf2 %>% mutate(
  
  isAbuse = case_when(
                        # これは正しそう
                        is.na(AbuseCase) ~ NA,
                        AbuseCase == 1 ~ 1,
                        AbuseCase == 0 ~ 0,
                    　),
  
  alcoholExp = case_when(
                            # これは正しそう
                            is.na(Alcoholever) ~ NA,
                            Alcoholever == 1 ~ 1,
                            Alcoholever == 95 ~ 0,
                        　)
  
) %>% group_by(MotherID,ChildID) %>% arrange(Year) %>%
  mutate(
    
    # ユーザーの初年度の年齢を取得
    age_min = min(Age),
    
    isAbuse = case_when(
                          isAbuse == 1 ~ 1,
                          cumsum(isAbuse == 1) > 0 ~ 1,
                          isAbuse == 0 ~ 0,
                        ),
          
    alcoholExp = case_when(
                            alcoholExp == 1 ~ 1,
                            cumsum(alcoholExp == 1) > 0 ~ 1,
                            alcoholExp == 0 ~ 0,
                        　),
    
    marijuanaExp = case_when(
                              marijuanaExp == 1 ~ 1,
                              cumsum(marijuanaExp == 1) > 0 ~ 1,
                              marijuanaExp == 0 ~ 0,
                            ),
    
    tabacoExp = case_when(
                            tabacoExp == 1 ~ 1,
                            cumsum(tabacoExp == 1) > 0 ~ 1,
                            tabacoExp == 0 ~ 0,
                          ),
    
    AGAP_ijt = ifelse(is.na(AGAP_ijt), 0, AGAP_ijt),
    AGAP = ifelse(is.na(AGAP), 0, AGAP),
    NYG_ijt = ifelse(is.na(NYG_ijt), 0, NYG_ijt),
    NYG = ifelse(is.na(NYG), 0, NYG),
    
    # IsNoneGraduate = case_when(
    #   max(ifelse(18 < Age, IsGraduate, NA), na.rm = TRUE) == 1 ~ 0,
    #   max(ifelse(18 < Age, IsGraduate, NA), na.rm = TRUE) == 0 ~ 1,
    #   TRUE ~ NA
    # ),
    
    # 母親の年齢
    MotherAge = (Year - FirstSurveyYear) + motherAgeAtBirth,
    
  ) %>% ungroup() %>%
  filter(
          # すくなくともその年に18歳になる -> Age = 18 or 17 つまりGrade12以下なら未成年とする。
          # ただし、推計に使うのは20歳以上にする。なぜなら前年の仕送りを聞いてるから。
          # 少なくともみんな19歳より上の年での仕送りになる
          # 少なくとも18歳より下の年のデータが1つ以上含まれている
          # この操作で、年上の人がいなくなる結果NYG_ijtの最大値が7になっている
          age_min < 19
        )
summary <- datasummary(All(alldf3) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max)),
                       data = alldf3,
                       na.rm = TRUE,
                       fmt = 3,
)
summary


alldf4 <- alldf3 %>% mutate(
  # 未成年ダミー(19歳未満=1)
  isU18 = case_when(
                      # これは正しそう
                      Age > 18 ~ 0,
                      TRUE ~ 1
                    ),
  
  U18_Abuse = case_when(
                          # これは正しそう
                          isAbuse == 1 & isU18 == 1 ~ 1,
                          is.na(isAbuse)  ~ NA,
                          TRUE ~ 0
                        ),
  
  o18_Abuse = case_when(
                          # これは正しそう
                          isAbuse == 1 & isU18 == 0 ~ 1,
                          is.na(isAbuse)  ~ NA,
                          TRUE ~ 0
                        ),
  
  U18_mariExp = case_when(
                            # これは正しそう
                            marijuanaExp == 1 & isU18 == 1 ~ 1,
                            0 < firstUseMarijuana & firstUseMarijuana < 18 ~ 1,
                            0 == firstUseMarijuana ~ NA,
                            is.na(marijuanaExp)  ~ NA,
                            TRUE ~ 0
                          ),
  
  o18_mariExp = case_when(
                            # これは正しそう
                            marijuanaExp == 1 & isU18 == 0 ~ 1,
                            is.na(marijuanaExp)  ~ NA,
                            TRUE ~ 0
                          ),
  
  U18_alcExp = case_when(
                            # これは正しそう
                            alcoholExp == 1 & isU18 == 1 ~ 1,
                            is.na(alcoholExp)  ~ NA,
                            TRUE ~ 0
                          ),
  
  o18_alcExp = case_when(
                            # これは正しそう
                            alcoholExp == 1 & isU18 == 0 ~ 1,
                            is.na(alcoholExp)  ~ NA,
                            TRUE ~ 0
                          ),
  
  U18_tabcExp = case_when(
                            # これは正しそう
                            tabacoExp == 1 & isU18 == 1 ~ 1,
                            is.na(tabacoExp)  ~ NA,
                            TRUE ~ 0
                          ),
  
  o18_tabcExp = case_when(
                            # これは正しそう
                            tabacoExp == 1 & isU18 == 0 ~ 1,
                            is.na(tabacoExp)  ~ NA,
                            TRUE ~ 0
                          ),
  
  SubstanceExp = case_when(
                            # これは正しそう
                            alcoholExp == 1 ~ 1,
                            marijuanaExp == 1 ~ 1,
                            tabacoExp == 1 ~ 1,
                            isAbuse == 1 ~ 1,
                            !is.na(alcoholExp) | !is.na(marijuanaExp) | !is.na(tabacoExp) | !is.na(isAbuse) ~ 0,
                            TRUE ~ NA
                          ),
  U18_SubstanceExp = case_when(
                                # これは正しそう
                                U18_alcExp == 1 ~ 1,
                                U18_mariExp == 1 ~ 1,
                                U18_tabcExp == 1 ~ 1,
                                U18_Abuse == 1 ~ 1,
                                !is.na(U18_alcExp) | !is.na(U18_mariExp) | !is.na(U18_tabcExp) | !is.na(U18_Abuse) ~ 0,
                                TRUE ~ NA
                              ),
  
  isBlack = case_when(
                        race != "BLACK" ~ 0,
                        race == "BLACK" ~ 1,
                      ),
  
  isHispanic = case_when(
                          race != "HISPANIC" ~ 0,
                          race == "HISPANIC" ~ 1,
                        ),
  
  # 第一子ダミー
  Is1th = case_when(
                      BirthOrder != 1 & !is.na(BirthOrder) ~ 0,
                      BirthOrder == 1 ~ 1,
                    ),
  # 第二子ダミー
  Is2th = case_when(
                      BirthOrder != 2 & !is.na(BirthOrder) ~ 0,
                      BirthOrder == 2 ~ 1,
                    ),
  # 第三子ダミー
  Is3th = case_when(
                      BirthOrder != 3 & !is.na(BirthOrder) ~ 0,
                      BirthOrder == 3 ~ 1,
                    ),
  # 第四子ダミー
  Is4th = case_when(
                      BirthOrder != 4 & !is.na(BirthOrder) ~ 0,
                      BirthOrder == 4 ~ 1,
                    ),
  
  # 第五子以上ダミー
  Is5th_OR_more = case_when(
                      BirthOrder < 5 ~ 0,
                      BirthOrder >= 5 ~ 1,
                    ),
  
  # メインのアウトカム
  IsTransfer_over50 = case_when(
                                  MainTransfer <= 2 ~ 0,
                                  MainTransfer > 2 ~ 1,
                                ),
  
  IsEnjoyRisk = case_when(
                            EnjoyRisk <= 2 ~ 0,
                            EnjoyRisk > 2 ~ 1,
                            TRUE ~ NA
                          ),
  
  IsUrban = case_when(
                        Urban == 0 ~ 0,
                        Urban == 1 ~ 1,
                        Urban == 2 ~ NA,
                      )
)

summary <- datasummary(All(alldf4) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max)),
                       data = alldf4,
                       na.rm = TRUE,
                       fmt = 3,
)
summary

alldf <- alldf4 %>% group_by(ChildID,MotherID) %>% arrange(Year) %>% 
  mutate(
    
    U18_alcExp = case_when(
                            max(ifelse(Age < 19, U18_alcExp, NA), na.rm = TRUE) == 1 ~ 1,
                            max(ifelse(Age < 19, U18_alcExp, NA), na.rm = TRUE) == 0 ~ 0,
                            TRUE ~ NA
                          ),
    
    U18_mariExp = case_when(
                              max(ifelse(Age < 19, U18_mariExp, NA), na.rm = TRUE) == 1 ~ 1,
                              max(ifelse(Age < 19, U18_mariExp, NA), na.rm = TRUE) == 0 ~ 0,
                              TRUE ~ NA
                            ),
    
    U18_tabcExp = case_when(
                              max(ifelse(Age < 19, U18_tabcExp, NA), na.rm = TRUE) == 1 ~ 1,
                              max(ifelse(Age < 19, U18_tabcExp, NA), na.rm = TRUE) == 0 ~ 0,
                              TRUE ~ NA
                            ),
    
    # U18_Abuse = case_when(
    #                         max(ifelse(Age < 19, U18_Abuse, NA), na.rm = TRUE) == 1 ~ 1,
    #                         max(ifelse(Age < 19, U18_Abuse, NA), na.rm = TRUE) == 0 ~ 0,
    #                         TRUE ~ NA
    #                       ),
    
    U18_SubstanceExp = case_when(
                                  max(ifelse(Age < 19, U18_SubstanceExp, NA), na.rm = TRUE) == 1 ~ 1,
                                  max(ifelse(Age < 19, U18_SubstanceExp, NA), na.rm = TRUE) == 0 ~ 0,
                                  TRUE ~ NA
                                 ),
    
    SubstanceExp = case_when(
                              max(SubstanceExp, na.rm = TRUE) == 1 ~ 1,
                              max(SubstanceExp, na.rm = TRUE) == 0 ~ 0,
                              TRUE ~ NA
                          　),
    
    marijuanaExp = case_when(
                              max(marijuanaExp, na.rm = TRUE) == 1 ~ 1,
                              max(marijuanaExp, na.rm = TRUE) == 0 ~ 0,
                              TRUE ~ NA
                            ),
    
    alcoholExp = case_when(
                              max(alcoholExp, na.rm = TRUE) == 1 ~ 1,
                              max(alcoholExp, na.rm = TRUE) == 0 ~ 0,
                              TRUE ~ NA
                            ),
    
    tabacoExp = case_when(
                              max(tabacoExp, na.rm = TRUE) == 1 ~ 1,
                              max(tabacoExp, na.rm = TRUE) == 0 ~ 0,
                              TRUE ~ NA
                            ),
    
  ) %>% ungroup()


summary <- datasummary(All(alldf) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max)),
                       data = alldf,
                       na.rm = TRUE,
                       fmt = 3,
)
summary

clean_alldf <- alldf %>% filter(
  # Is1th == 1 | Is2th == 1 | Is3th == 1 | Is4th == 1,
  # 2子以上いる家庭に限定
  N_siblings > 1,
)

fclean_alldf <- makeStatic_df(clean_alldf)


# 基本統計量 -------------------------------------------------------------------

summary <- datasummary(All(fclean_alldf) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max)),
                       data = fclean_alldf,
                       na.rm = TRUE,
                       fmt = 3,
)
summary

