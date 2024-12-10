
# Texについて -----------------------------------------------------------------

# Texの頭部と末尾に追加
# \documentclass{article}
# \usepackage{booktabs}
# \usepackage{fontspec}
# \usepackage{luatexja}
# \begin{document}

# ~hogehoge~

# \end{document}

# 中間ファイルを削除
RemoveFiles()

# 依存関係 --------------------------------------------------------------------
# install.packages("margins")
library(tidyverse)
library(modelsummary)
library(gtsummary)
library(fixest)
library(broom)
library(margins)


 # 予測スコア算出 -----------------------------------------------------------------

#predictを使って確率を計算

# predictSubUse <- predict(B1model_lm, type = "response", newdata = clean_alldf)
# predictMarijuana_Use <- predict(B1model_lm_marijuana, type = "response", newdata = clean_alldf)
# predictAlcohol_Use <- predict(B1model_lm_alcohol, type = "response", newdata = clean_alldf)
# predictTabaco_Use <- predict(B1model_lm_tabaco, type = "response", newdata = clean_alldf)
# 
# #データフレームに傾向スコアを格納
# clean_alldf1 <-clean_alldf %>%
#   mutate(
#     U18_SubstanceExp = predictSubUse,
#     U18_marijuana = predictMarijuana_Use,
#     U18_alcohol = predictAlcohol_Use,
#     U18_tabaco = predictTabaco_Use
#   )


# 基本統計量2 -------------------------------------------------------------------


f_alldf <- makeStatic_df(clean_alldf)

f_alldf <- f_alldf %>% filter(
  17 < `子供の年齢` & `子供の年齢` < 23,
  # `弟妹の数` < 4
)


summary <- datasummary(All(f_alldf) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max)),
                       data = f_alldf,
                       na.rm = TRUE,
                       fmt = 3,
)


summary

hist(
  f_alldf$兄弟サイズ,
  breaks = seq(0, 11, 1),
  main = "兄弟サイズの分布",
  xlab = "兄弟サイズ",
  ylab = "人数"
)

hist(
  f_alldf$弟妹の数,
  # breaks = seq(-1, 3, 1),
  main = "弟妹の数の分布",
  xlab = "弟妹の数",
  ylab = "人数"
)

hist(
  f_alldf$年下の兄弟との年齢差,
  # breaks = seq(-1, 15, 1),
  main = "年下の兄弟との年齢差の分布",
  xlab = "年下の兄弟との年齢差",
  ylab = "人数"
)


str(f_alldf)

clean_alldf1 <- clean_alldf

clean_alldf1$NYG_ijt <- clean_alldf1$NYG
clean_alldf1$AGAP_ijt <- clean_alldf1$AGAP

clean_alldf2 <- clean_alldf1 %>% filter(
  17 < Age & Age < 23,
  # NYG_ijt < 4
)

str(clean_alldf2)
hist(clean_alldf2$AGAP_ijt)
hist(clean_alldf2$NYG_ijt)

# 推計（メイン-線形）(3-1) -----------------------------------------------------------------


model1 <- feols(IsTransfer_over50 ~ U18_mariExp + NYG_ijt + AGAP_ijt 
                + NYG_ijt:U18_mariExp + U18_mariExp:AGAP_ijt
                + Isfemale + IsUrban 
                + IsEnjoyRisk
                | MotherID
                + BirthYear
                # + N_siblings
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2
)
summary(model1)

model2 <- feols(IsTransfer_over50 ~ U18_alcExp + NYG_ijt + AGAP_ijt 
                + NYG_ijt:U18_alcExp + U18_alcExp:AGAP_ijt
                + Isfemale 
                # + N_siblings
                + IsUrban
                + IsEnjoyRisk
                | MotherID
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2
)
summary(model2)

model3 <- feols(IsTransfer_over50 ~ U18_tabcExp + NYG_ijt + AGAP_ijt 
                + NYG_ijt:U18_tabcExp + U18_tabcExp:AGAP_ijt
                + Isfemale 
                # + N_siblings 
                + IsUrban
                + IsEnjoyRisk
                | MotherID
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2
)
summary(model3)

model4 <- feols(IsTransfer_over50 ~ U18_SubstanceExp + NYG_ijt + AGAP_ijt 
                + NYG_ijt:U18_SubstanceExp + U18_SubstanceExp:AGAP_ijt
                + Isfemale
                # + N_siblings 
                + IsUrban + IsEnjoyRisk
                | MotherID
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2
)
summary(model4)

# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result3-1.tex")) {
  file.remove("./outputs/lm_result3-1.tex")
}
etable(model1,model2,model3,model4,
       tex = TRUE, # LaTeXコードとして出力
       headers = list("Sample:" = list("大麻","飲酒","喫煙","総合")),
       dict = c(U18_SubstanceExp = "未成年の違法使用(総合)", U18_mariExp = "未成年大麻経験",
                U18_alcExp = "未成年飲酒経験", U18_tabcExp = "未成年喫煙経験",
                NYG_ijt = "下の兄弟の数", AGAP_ijt = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year",
                IsTransfer_over50 = "譲渡over50%ダミー",
                IsTransfered =　"譲渡ダミー",
                IsLiveTogether = "同居ダミー"
                ),
       file = "./outputs/lm_result3-1.tex", # 出力先のファイル
       keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上23歳以下を対象"
)
AddHeader_wide("lm_result3-1")

# 推計（メイン-非線形）(3-2) -----------------------------------------------------------------

model4 <- feols(IsTransfer_over50 ~ U18_SubstanceExp + NYG_ijt + AGAP_ijt 
                + i(NYG_ijt, U18_SubstanceExp, ref = 0) + U18_SubstanceExp:AGAP_ijt
                + Isfemale + IsUrban + IsEnjoyRisk
                | MotherID 
                # + N_siblings_t
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2
)
summary(model4)

pl <- iplot(model4,zero = TRUE,pt.join = TRUE)

model1 <- feols(IsTransfer_over50 ~ U18_mariExp + NYG_ijt + AGAP_ijt 
                + i(NYG_ijt, U18_mariExp, ref = 0) + U18_mariExp:AGAP_ijt
                # + N_siblings 
                + Isfemale + IsUrban 
                + IsEnjoyRisk
                | MotherID 
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2
)
summary(model1)

pl <- iplot(model1,zero = TRUE,pt.join = TRUE,add = TRUE,col = "green")

model2 <- feols(IsTransfer_over50 ~ U18_alcExp + NYG_ijt + AGAP_ijt 
                + i(NYG_ijt, U18_alcExp, ref = 0) + U18_alcExp:AGAP_ijt
                # + N_siblings 
                + Isfemale + IsUrban + IsEnjoyRisk
                | MotherID 
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2
)
summary(model2)

pl <- iplot(model2,zero = TRUE,pt.join = TRUE,add = TRUE,col = "blue")

model3 <- feols(IsTransfer_over50 ~ U18_tabcExp + NYG_ijt + AGAP_ijt 
                + i(NYG_ijt, U18_tabcExp, ref = 0) + U18_tabcExp:AGAP_ijt
                # + N_siblings 
                + Isfemale + IsUrban + IsEnjoyRisk
                | MotherID 
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2
)
summary(model3)

pl <- iplot(model3,zero = TRUE,pt.join = TRUE,add = TRUE,
            legend= list(                            # 凡例
                 "topright", 
                 lty=c(1,2,3,4),                         # それぞれの線のパターン
                 legend=c("総合", "大麻", "飲酒","喫煙")           # それぞれのデータ名
               ),col = "red")

# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result3-2.tex")) {
  file.remove("./outputs/lm_result3-2.tex")
}
etable(model1, model2, model3,model4,
       tex = TRUE, # LaTeXコードとして出力
       dict = c(U18_SubstanceExp = "未成年の違法使用(総合)", U18_mariExp = "未成年大麻経験",
                U18_alcExp = "未成年飲酒経験", U18_tabcExp = "未成年喫煙経験",
                NYG_ijt = "下の兄弟の数", AGAP_ijt = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result3-2.tex", # 出力先のファイル
       keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上23歳以下を対象"
)
AddHeader_wide("lm_result3-2")


# 男女での異質性(総合)(3-4) -----------------------------------------------------------------

clean_alldf2_man <- clean_alldf2 %>% filter(
  Isfemale == 0
)

model1 <- feols(IsTransfer_over50 ~ U18_SubstanceExp + NYG_ijt + AGAP_ijt 
                + i(NYG_ijt, U18_SubstanceExp, ref = 0) + U18_SubstanceExp:AGAP_ijt
                # + N_siblings 
                + IsUrban 
                + IsEnjoyRisk
                | MotherID 
                + BirthYear
                + Age
                + Year
                , data = clean_alldf2_man
)
summary(model1)

model1 <- feols(IsTransfer_over50 ~ U18_SubstanceExp + NYG_ijt + AGAP_ijt
                + NYG_ijt:U18_SubstanceExp + U18_SubstanceExp:AGAP_ijt
                # + N_siblings
                + IsUrban 
                + IsEnjoyRisk
                | MotherID 
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2_man
)
summary(model1)

clean_alldf2_lady <- clean_alldf2 %>% filter(
  Isfemale == 1
)

model2 <- feols(IsTransfer_over50 ~ U18_SubstanceExp + NYG_ijt + AGAP_ijt 
                + i(NYG_ijt, U18_SubstanceExp, ref = 0) + U18_SubstanceExp:AGAP_ijt
                + IsUrban 
                + IsEnjoyRisk
                | MotherID 
                # + N_siblings 
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2_lady
)
summary(model2)

model2 <- feols(IsTransfer_over50 ~ U18_SubstanceExp + NYG_ijt + AGAP_ijt
                + NYG_ijt:U18_SubstanceExp + U18_SubstanceExp:AGAP_ijt
                # + N_siblings 
                + IsUrban 
                + IsEnjoyRisk
                | MotherID 
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2_lady
)
summary(model2)

model3 <- feols(IsTransfer_over50 ~ U18_SubstanceExp + NYG_ijt + AGAP_ijt
                + i(NYG_ijt, U18_SubstanceExp, ref = 0) + U18_SubstanceExp:AGAP_ijt
                + NYG_ijt:U18_SubstanceExp:Isfemale
                # + N_siblings 
                + IsUrban + Isfemale
                + IsEnjoyRisk
                | MotherID 
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2
)
summary(model3)

pl <- iplot(model3,zero = TRUE,pt.join = TRUE,add = T)

model3 <- feols(IsTransfer_over50 ~ U18_SubstanceExp + NYG_ijt + AGAP_ijt
                + NYG_ijt:U18_SubstanceExp + U18_SubstanceExp:AGAP_ijt
                + NYG_ijt:U18_SubstanceExp:Isfemale
                # + N_siblings 
                + IsUrban + Isfemale
                + IsEnjoyRisk
                | MotherID 
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2
)
summary(model3)

# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result3-4.tex")) {
  file.remove("./outputs/lm_result3-4.tex")
}
etable(model1, model2,model3,
       tex = TRUE, # LaTeXコードとして出力
       headers = list("Sample:" = list("男","女","交差項")),
       dict = c(U18_SubstanceExp = "未成年の違法使用(総合)", U18_mariExp = "未成年大麻経験",
                U18_alcExp = "未成年飲酒経験", U18_tabcExp = "未成年喫煙経験",
                NYG_ijt = "下の兄弟の数", AGAP_ijt = "下の兄弟との年齢差",
                Isfemale = "女性ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year", isCollegeStudent = "大学生ダミー",
                IsTransfer_over50 = "譲渡over50%ダミー"),
       file = "./outputs/lm_result3-4.tex", # 出力先のファイル
       keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上23歳以下を対象"
)
AddHeader_wide("lm_result3-4")

# 男女での異質性(総合)(3-5) -----------------------------------------------------------------

model1 <- feols(IsTransfer_over50 ~ U18_mariExp + NYG_ijt + AGAP_ijt 
                + NYG_ijt:U18_mariExp + U18_mariExp:AGAP_ijt
                + NYG_ijt:U18_mariExp:Isfemale
                + Isfemale 
                # + N_siblings 
                + IsUrban 
                + IsEnjoyRisk
                | MotherID
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2
)
summary(model1)

model2 <- feols(IsTransfer_over50 ~ U18_alcExp + NYG_ijt + AGAP_ijt 
                + NYG_ijt:U18_alcExp + U18_alcExp:AGAP_ijt
                + NYG_ijt:U18_alcExp:Isfemale
                + Isfemale 
                # + N_siblings
                + IsUrban + IsEnjoyRisk
                | MotherID
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2
)
summary(model2)

model3 <- feols(IsTransfer_over50 ~ U18_tabcExp + NYG_ijt + AGAP_ijt 
                + NYG_ijt:U18_tabcExp + U18_tabcExp:AGAP_ijt
                + NYG_ijt:U18_tabcExp:Isfemale
                + Isfemale 
                # + N_siblings 
                + IsUrban + IsEnjoyRisk
                | MotherID
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2
)
summary(model3)

model4 <- feols(IsTransfer_over50 ~ U18_SubstanceExp + NYG_ijt + AGAP_ijt 
                + NYG_ijt:U18_SubstanceExp + U18_SubstanceExp:AGAP_ijt
                + NYG_ijt:U18_SubstanceExp:Isfemale
                + Isfemale + N_siblings + IsUrban + IsEnjoyRisk
                | MotherID
                + BirthYear
                + Age
                + Year
                + MotherAge
                , data = clean_alldf2
)
summary(model4)


# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result3-5.tex")) {
  file.remove("./outputs/lm_result3-5.tex")
}
etable(model1, model2,model3,model4,
       tex = TRUE, # LaTeXコードとして出力
       headers = list("Sample:" = list("大麻","飲酒","喫煙","総合")),
       dict = c(U18_SubstanceExp = "未成年の違法使用", U18_mariExp = "未成年の違法使用",
                U18_alcExp = "未成年の違法使用", U18_tabcExp = "未成年の違法使用",
                NYG_ijt = "下の兄弟の数", AGAP_ijt = "下の兄弟との年齢差",
                Isfemale = "女性ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year", isCollegeStudent = "大学生ダミー",
                IsTransfer_over50 = "譲渡over50%ダミー"),
       file = "./outputs/lm_result3-5.tex", # 出力先のファイル
       keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上23歳以下を対象"
)
AddHeader_wide("lm_result3-5")


# 未成年使用アウトカム＝総合(7-1) ------------------------------------------------------------

clean_alldf3 <- clean_alldf2 %>% filter(
  U18_SubstanceExp == 1
)

model1 <- feols(IsTransfer_over50 ~ NYG_ijt + AGAP_ijt 
                + Isfemale 
                # + N_siblings_t 
                + IsUrban 
                # + IsEnjoyRisk
                | MotherID
                + BirthYear
                + Year
                + Age
                + MotherAge
                , data = clean_alldf3
)
summary(model1)

model2 <- feols(IsTransfer_over50 ~ NYG_ijt + AGAP_ijt
                + NYG_ijt:Isfemale
                + Isfemale 
                # + N_siblings 
                + IsUrban 
                # + IsEnjoyRisk
                | MotherID
                + BirthYear
                + Year
                + Age
                + MotherAge
                , data = clean_alldf3
)
summary(model2)


# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result7-1.tex")) {
  file.remove("./outputs/lm_result7-1.tex")
}
etable(model1, model2,
       tex = TRUE, # LaTeXコードとして出力
       dict = c(U18_SubstanceExp = "未成年の違法使用(総合)", U18_mariExp = "未成年大麻経験",
                U18_alcExp = "未成年飲酒経験", U18_tabcExp = "未成年喫煙経験",
                NYG_ijt = "下の兄弟の数", AGAP_ijt = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result7-1.tex", # 出力先のファイル
       # keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上23歳以下を対象"
)
AddHeader("lm_result7-1")

# 未成年使用アウトカム＝大麻(7-2) ------------------------------------------------------------

clean_alldf4 <- clean_alldf2 %>% filter(
  U18_mariExp == 1
)

model1 <- feols(IsTransfer_over50 ~ NYG_ijt + AGAP_ijt 
                + Isfemale 
                # + N_siblings_t 
                + IsUrban
                | MotherID
                + BirthYear
                + Year
                + Age
                + MotherAge
                , data = clean_alldf4
)
summary(model1)

model2 <- feols(IsTransfer_over50 ~ NYG_ijt + AGAP_ijt
                + NYG_ijt:Isfemale
                + Isfemale 
                # + N_siblings 
                + IsUrban 
                # + IsEnjoyRisk
                | MotherID
                + BirthYear
                + Year
                + Age
                + MotherAge
                , data = clean_alldf4
)
summary(model2)

# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result7-2.tex")) {
  file.remove("./outputs/lm_result7-2.tex")
}
etable(model1, model2,
       tex = TRUE, # LaTeXコードとして出力
       dict = c(U18_SubstanceExp = "未成年の違法使用(総合)", U18_mariExp = "未成年大麻経験",
                U18_alcExp = "未成年飲酒経験", U18_tabcExp = "未成年喫煙経験",
                NYG_ijt = "下の兄弟の数", AGAP_ijt = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result7-2.tex", # 出力先のファイル
       # keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上23歳以下を対象"
)
AddHeader("lm_result7-2")

# 未成年使用アウトカム＝飲酒(7-3) ------------------------------------------------------------

clean_alldf5 <- clean_alldf2 %>% filter(
  U18_alcExp == 1
)

model1 <- feols(IsTransfer_over50 ~ NYG_ijt + AGAP_ijt 
                + Isfemale 
                # + N_siblings_t 
                + IsUrban
                | MotherID
                + Year
                + Age
                + BirthYear
                + MotherAge
                , data = clean_alldf5
)
summary(model1)

model2 <- feols(IsTransfer_over50 ~ NYG_ijt + AGAP_ijt
                + NYG_ijt:Isfemale
                + Isfemale 
                # + N_siblings 
                + IsUrban 
                # + IsEnjoyRisk
                | MotherID
                + BirthYear
                + Year
                + Age
                + MotherAge
                , data = clean_alldf5
)
summary(model2)

# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result7-3.tex")) {
  file.remove("./outputs/lm_result7-3.tex")
}
etable(model1, model2,
       tex = TRUE, # LaTeXコードとして出力
       dict = c(U18_SubstanceExp = "未成年の違法使用(総合)", U18_mariExp = "未成年大麻経験",
                U18_alcExp = "未成年飲酒経験", U18_tabcExp = "未成年喫煙経験",
                NYG_ijt = "下の兄弟の数", AGAP_ijt = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result7-3.tex", # 出力先のファイル
       # keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上23歳以下を対象"
)
AddHeader("lm_result7-3")

# 未成年使用アウトカム＝喫煙(7-4) ------------------------------------------------------------

clean_alldf6 <- clean_alldf2 %>% filter(
  U18_tabcExp == 1
)

model1 <- feols(IsTransfer_over50 ~ NYG_ijt + AGAP_ijt 
                + Isfemale 
                # + N_siblings 
                + IsUrban
                | MotherID
                + BirthYear
                + Year
                + Age
                + MotherAge
                , data = clean_alldf6
)
summary(model1)

model2 <- feols(IsTransfer_over50 ~ NYG_ijt + AGAP_ijt
                + NYG_ijt:Isfemale
                + Isfemale 
                # + N_siblings 
                + IsUrban 
                # + IsEnjoyRisk
                | MotherID
                + BirthYear
                + Year
                + Age
                + MotherAge
                , data = clean_alldf6
)
summary(model2)

# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result7-4.tex")) {
  file.remove("./outputs/lm_result7-4.tex")
}
etable(model1, model2,
       tex = TRUE, # LaTeXコードとして出力
       dict = c(U18_SubstanceExp = "未成年の違法使用(総合)", U18_mariExp = "未成年大麻経験",
                U18_alcExp = "未成年飲酒経験", U18_tabcExp = "未成年喫煙経験",
                NYG_ijt = "下の兄弟の数", AGAP_ijt = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result7-4.tex", # 出力先のファイル
       # keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上23歳以下を対象"
)
AddHeader("lm_result7-4")

