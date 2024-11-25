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
tabacoExpdf <- makeVariabledf(dfNaive1,"YASR-19A","tabacoExp",1998,2020)
Gradedf <- makeVariabledf(dfNaive1,"Q4-2","educ",1994,2020)
IsGraduatedf <- makeVariabledf(dfNaive1,"Q4-28","IsGraduate",1994,2020)
EnjoyRiskdf <- makeVariabledf(dfNaive1,"Q16-5I-D","EnjoyRisk",1994,2020)
Urbandf <- makeVariabledf(dfNaive1,"URBAN-RURAL","Urban",1994,2020)
AbuseCasedf <- makeVariabledf(dfNaive1,"YASR-71A~000025","AbuseCase",2016,2020)
FamilyIncomedf <- makeVariabledf(dfNaive1,"Q15-141-TOP","FamilyIncome",2002,2014)

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
                        tabacoExpdf,
                        Gradedf,
                        IsGraduatedf,
                        EnjoyRiskdf,
                        Urbandf,
                        AbuseCasedf,
                        FamilyIncomedf
                      ), full_join,
                      by = c('ChildID', 'MotherID', 'Year', 'FirstSurveyYear'))
raw_df <- left_join(masterdf,covar_df,by = c('ChildID', 'MotherID','Year','FirstSurveyYear')) %>% mutate(
    Age = Year - BirthYear
  )

# 変数の作成 -------------------------------------------------------------------

alldf1 <- raw_df

# 出生順位に関する変数 --------------------------------------------------------------

alldf2 <- alldf1 %>% group_by(MotherID,Year) %>%
  mutate(
    
    # 兄弟の数
    N_siblings = max(BirthOrder, na.rm = TRUE),
    
    # 下の兄弟の数
    NYG_ij = N_siblings - BirthOrder,
    
    # 下の兄弟との差
    AGAP_ij = lag(BirthYear, order_by = NYG_ij) - BirthYear,
    
    # ※NA処理
    AGAP_ij = ifelse(is.na(AGAP_ij),0,AGAP_ij),
    
    # その年の18未満の最大Age
    maxAgeU18 = max(ifelse(Age < 18, Age, NA), na.rm = TRUE),
    maxAgeU18 = ifelse(is.infinite(maxAgeU18), NA, maxAgeU18),
    
    # (18歳未満の)次の兄弟との年齢差
    age_gap = case_when(
                        is.na(maxAgeU18) ~ NA,
                        TRUE ~ Age - maxAgeU18,
                       ),
    AGAP_ijt = case_when(
                          age_gap <= 0 ~ lag(BirthYear, order_by = Age) - BirthYear,
                          age_gap > 0 ~ age_gap,
                          is.na(age_gap) ~ NA
                         ),
    # ※NA処理
    AGAP_ijt = ifelse(is.infinite(AGAP_ijt), 0, AGAP_ijt),
    
    # その他共変量
    FamilyIncome = mean(FamilyIncome, na.rm = TRUE),
    
    familySize = mean(familySize, na.rm = TRUE),
    
    EnjoyRisk = mean(EnjoyRisk, na.rm = TRUE),
    
    # 兄弟の数が欠けていないかVaildate
    Valid_Nsiblings = ifelse(n() == N_siblings,1,0),
    
  ) %>% arrange(NYG_ij) %>% mutate(
    
    NYG_ijt = cumsum(ifelse(Age < 18,1,0)),
    NYG_ijt = case_when(
                        Age < 18 ~ NYG_ijt - 1,
                        TRUE ~ NYG_ijt
                       )
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
                          )
    
  ) %>% ungroup() %>% filter(
                            # 少なくとも18歳より下の年のデータが1つ以上含まれている
                              age_min < 18
                            )


alldf4 <- alldf3 %>% mutate(
  # 未成年ダミー(18歳未満=1)
  isU18 = case_when(
                      # これは正しそう
                      Age > 17 ~ 0,
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
  
  educ = case_when(
                    educ == 0 | educ == 95 ~ NA,
                    !is.na(educ) ~ educ
                  ),
  
  isCollegeStudent = case_when(
                                educ <= 12 ~ 0,
                                educ > 12 ~ 1,
                              ),
  
  motherEduc = case_when(
                          motherEduc == 0 | educ == 95 ~ NA,
                          !is.na(motherEduc) ~ motherEduc
                        ),
  
  IsUrban = case_when(
                        Urban == 0 ~ 0,
                        Urban == 1 ~ 1,
                        Urban == 2 ~ NA,
                      )
)

alldf <- alldf4 %>% group_by(ChildID,MotherID) %>% arrange(Year) %>% 
  mutate(
    # 母親の年齢
    MotherAge = (Year - FirstSurveyYear) + motherAgeAtBirth,
    
    U18_alcExp = case_when(
                            max(ifelse(Age < 18, U18_alcExp, NA), na.rm = TRUE) == 1 ~ 1,
                            max(ifelse(Age < 18, U18_alcExp, NA), na.rm = TRUE) == 0 ~ 0,
                            TRUE ~ NA
                          ),
    
    U18_mariExp = case_when(
                              max(ifelse(Age < 18, U18_mariExp, NA), na.rm = TRUE) == 1 ~ 1,
                              max(ifelse(Age < 18, U18_mariExp, NA), na.rm = TRUE) == 0 ~ 0,
                              TRUE ~ NA
                            ),
    
    U18_tabcExp = case_when(
                              max(ifelse(Age < 18, U18_tabcExp, NA), na.rm = TRUE) == 1 ~ 1,
                              max(ifelse(Age < 18, U18_tabcExp, NA), na.rm = TRUE) == 0 ~ 0,
                              TRUE ~ NA
                            ),
    
    U18_Abuse = case_when(
                            max(ifelse(Age < 18, U18_Abuse, NA), na.rm = TRUE) == 1 ~ 1,
                            max(ifelse(Age < 18, U18_Abuse, NA), na.rm = TRUE) == 0 ~ 0,
                            TRUE ~ NA
                          ),
    
    # SubstanceExp = case_when(
    #                           cumsum(ifelse(SubstanceExp == 1, 1, 0)) > 0 ~ 1,
    #                           cumsum(ifelse(SubstanceExp == 1, 1, 0)) == 0 ~ 0,
    #                           TRUE ~ NA
    #                       　),
    
    U18_SubstanceExp = case_when(
                                  max(ifelse(Age < 18, U18_SubstanceExp, NA), na.rm = TRUE) == 1 ~ 1,
                                  max(ifelse(Age < 18, U18_SubstanceExp, NA), na.rm = TRUE) == 0 ~ 0,
                                  TRUE ~ NA
                                 ),
    
    AGAP_ijt = ifelse(is.na(AGAP_ijt), 0, AGAP_ijt),
    AGAP_ij = ifelse(is.na(AGAP_ij), 0, AGAP_ij),
    NYG_ijt = ifelse(is.na(NYG_ijt), 0, NYG_ijt),
    NYG_ij = ifelse(is.na(NYG_ij), 0, NYG_ij),
    
    IsNoneGraduate = case_when(
                            max(ifelse(17 < Age & Age < 20, IsGraduate, NA), na.rm = TRUE) == 1 ~ 0,
                            max(ifelse(17 < Age & Age < 20, IsGraduate, NA), na.rm = TRUE) == 0 ~ 1,
                            TRUE ~ NA
                           )
    
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


# 補足

# sapply(alldf[, c("U18_alcExp", "U18_mariExp", "U18_tabcExp", "U18_Abuse")], function(x) sum(is.na(x)))
# sum(is.na(alldf$U18_SubstanceExp))
# sum(!is.na(alldf$U18_SubstanceExp) & !is.na(alldf$U18_alcExp))
# sum(!is.na(alldf$U18_SubstanceExp) & !is.na(alldf$U18_alcExp) & !is.na(alldf$U18_mariExp) & !is.na(alldf$U18_tabcExp)& !is.na(alldf$U18_Abuse))
# 
# 
# 
# ggplot(alldf, aes(x = BirthOrder, y = SubstanceExp)) +
#   geom_smooth(method = "glm", 
#               method.args = list(family = binomial(link = logit)),
#               color = "blue", se = TRUE) +
#   labs(x = "出生順位", y = "未成年使用ダミー") +
#   theme_bw()+
#   scale_x_continuous(limits = c(0, NA)) +  # x軸の最小値を0に設定
#   scale_y_continuous(limits = c(0, NA))    # y軸の最小値を0に設定

