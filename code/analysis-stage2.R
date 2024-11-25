
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

predictSubUse <- predict(B1model_glm, type = "response", newdata = clean_alldf)
predictMarijuana_Use <- predict(B1model_glm_marijuana, type = "response", newdata = clean_alldf)
predictAlcohol_Use <- predict(B1model_glm_alcohol, type = "response", newdata = clean_alldf)
predictTabaco_Use <- predict(B1model_glm_tabaco, type = "response", newdata = clean_alldf)

#データフレームに傾向スコアを格納
clean_alldf1 <-clean_alldf %>%
  mutate(
    exSubUse = predictSubUse,
    exSubUse_marijuana = predictMarijuana_Use,
    exSubUse_alcohol = predictAlcohol_Use,
    exSubUse_tabaco = predictTabaco_Use
  )

hist(clean_alldf1$U18_SubstanceExp,breaks = seq(0, 1, by = 0.01), main = "Distribution")
hist(clean_alldf1$exSubUse,breaks = seq(0, 1, by = 0.05), main = "Distribution")


ggplot(clean_alldf1, aes(x = NYG_ij, y = U18_SubstanceExp)) +
  geom_smooth(method = "lm",
              # method.args = list(family = binomial(link = logit)),
              color = "blue", se = FALSE) +
  labs(x = "弟妹の数", y = "未成年使用ダミー") +
  theme_bw() +
  ylim(0, 1)


ggplot(clean_alldf1, aes(x = NYG_ij, y = exSubUse)) +
  geom_point() +
  geom_smooth(method = "lm",
              # method.args = list(family = binomial(link = logit)),
              color = "blue", se = TRUE) +
  labs(x = "弟妹の数", y = "未成年使用ダミー") +
  theme_bw() +
  ylim(0, 1)


# 基本統計量2 -------------------------------------------------------------------

f_alldf <- makeStatic_df2(clean_alldf1)


f_alldf <- f_alldf %>% filter(
  17 < `子供の年齢` & `子供の年齢` < 30
)
summary <- datasummary(All(f_alldf) ~ ((標本数 = N) + (平均 = Mean) + (標準偏差　= SD) + (最小値 = Min) + (最大値 = Max)),
                       data = f_alldf,
                       na.rm = TRUE,
                       fmt = 3,
)

summary
str(f_alldf)

hist_f(f_alldf,"子供の年齢")
hist_f(f_alldf,"出生順位")
hist_f(f_alldf,"弟妹の数")


clean_alldf2 <- clean_alldf1 %>% filter(
  17 < Age & Age < 25,
)
str(clean_alldf2)

# 推計（メイン） -----------------------------------------------------------------

model3 <- feols(IsLiveTogether ~ exSubUse + NYG_ij + AGAP_ij
               + NYG_ij:exSubUse + exSubUse:AGAP_ij
               + N_siblings + Isfemale
               | MotherID 
               + BirthYear
               , data = clean_alldf2
)
summary(model3)


model2 <- feols(IsTransfered ~ exSubUse + NYG_ij + AGAP_ij
               + NYG_ij:exSubUse + exSubUse:AGAP_ij
               + N_siblings + Isfemale
               | MotherID 
               + BirthYear
               , data = clean_alldf2
)
summary(model2)


model1 <- feols(IsTransfer_over50 ~ exSubUse + NYG_ij + AGAP_ij
               + NYG_ij:exSubUse + exSubUse:AGAP_ij
               + N_siblings + Isfemale
               | MotherID 
               + BirthYear
               , data = clean_alldf2
)
summary(model1)

# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result3-1.tex")) {
  file.remove("./outputs/lm_result3-1.tex")
}
etable(model1, model2, model3,
       tex = TRUE, # LaTeXコードとして出力
       dict = c(exSubUse = "未成年の違法使用(総合)", exSubUse_marijuana = "未成年大麻経験",
                exSubUse_alcohol = "未成年飲酒経験", exSubUse_tabaco = "未成年喫煙経験",
                NYG_ij = "下の兄弟の数", AGAP_ij = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result3-1.tex", # 出力先のファイル
       keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上24歳以下の出生順位が4以下を対象"
)
AddHeader_wide("lm_result3-1")



model1 <- feols(IsTransfer_over50 ~ exSubUse + NYG_ij + AGAP_ij 
                + i(NYG_ij, exSubUse, ref = 0) + exSubUse:AGAP_ij
                + N_siblings + Isfemale
                | MotherID 
                + BirthYear
                , data = clean_alldf2
)
summary(model1)

model2 <- feols(IsTransfered ~ exSubUse + NYG_ij + AGAP_ij 
                + i(NYG_ij, exSubUse, ref = 0) + exSubUse:AGAP_ij
                + N_siblings + Isfemale
                | MotherID 
                + BirthYear
                , data = clean_alldf2
)
summary(model2)

model3 <- feols(IsLiveTogether  ~ exSubUse + NYG_ij + AGAP_ij 
                + i(NYG_ij, exSubUse, ref = 0) + exSubUse:AGAP_ij
                + N_siblings + Isfemale
                | MotherID 
                + BirthYear
                , data = clean_alldf2
)
summary(model3)


# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result3-2.tex")) {
  file.remove("./outputs/lm_result3-2.tex")
}
etable(model1, model2, model3,
       tex = TRUE, # LaTeXコードとして出力
       dict = c(exSubUse = "未成年の違法使用(総合)", exSubUse_marijuana = "未成年大麻経験",
                exSubUse_alcohol = "未成年飲酒経験", exSubUse_tabaco = "未成年喫煙経験",
                NYG_ij = "下の兄弟の数", AGAP_ij = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result3-2.tex", # 出力先のファイル
       keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上24歳以下の出生順位が4以下を対象"
)
AddHeader_wide("lm_result3-2")



# 各種アウトカム(IsTransfer_over50) ----------------------------------------------

model1 <- feols(IsTransfer_over50 ~ exSubUse_marijuana + NYG_ij + AGAP_ij 
               + NYG_ij:exSubUse_marijuana + exSubUse_marijuana:AGAP_ij
               | MotherID
               + ChildID
               + BirthYear
               , data = clean_alldf2
)
summary(model1)

model2 <- feols(IsTransfer_over50 ~ exSubUse_alcohol + NYG_ij + AGAP_ij 
                + NYG_ij:exSubUse_alcohol + exSubUse_alcohol:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)
summary(model2)

model3 <- feols(IsTransfer_over50 ~ exSubUse_tabaco + NYG_ij + AGAP_ij 
               + NYG_ij:exSubUse_tabaco + exSubUse_tabaco:AGAP_ij
               | MotherID
               + ChildID
               + BirthYear
               , data = clean_alldf2
)
summary(model3)

model4 <- feols(IsTransfer_over50 ~ exSubUse + NYG_ij + AGAP_ij 
               + NYG_ij:exSubUse + exSubUse:AGAP_ij
               + Isfemale + N_siblings
               | MotherID
               + BirthYear
               , data = clean_alldf2
)
summary(model4)

# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result4-1.tex")) {
  file.remove("./outputs/lm_result4-1.tex")
}
etable(model1, model2, model3,model4,
       tex = TRUE, # LaTeXコードとして出力
       headers = list("Sample:" = list("大麻","飲酒","喫煙","総合")),
       dict = c(exSubUse = "未成年の違法使用", exSubUse_marijuana = "未成年の違法使用",
                exSubUse_alcohol = "未成年の違法使用", exSubUse_tabaco = "未成年の違法使用",
                NYG_ij = "下の兄弟の数", AGAP_ij = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result4-1.tex", # 出力先のファイル
       keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上24歳以下の出生順位が4以下を対象"
)
AddHeader_wide("lm_result4-1")



# 各種アウトカム
model1 <- feols(IsTransfer_over50 ~ exSubUse_marijuana + NYG_ij + AGAP_ij 
                + i(NYG_ij, exSubUse_marijuana, ref = 0) + exSubUse_marijuana:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)
summary(model1)

model2 <- feols(IsTransfer_over50 ~ exSubUse_alcohol + NYG_ij + AGAP_ij 
                + i(NYG_ij, exSubUse_alcohol, ref = 0) + exSubUse_alcohol:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)

model3 <- feols(IsTransfer_over50 ~ exSubUse_tabaco + NYG_ij + AGAP_ij 
                + i(NYG_ij, exSubUse_tabaco, ref = 0) + exSubUse_tabaco:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)
summary(model3)

model4 <- feols(IsTransfer_over50 ~ exSubUse + NYG_ij + AGAP_ij 
                + i(NYG_ij, exSubUse, ref = 0) + exSubUse:AGAP_ij
                + Isfemale + N_siblings
                | MotherID
                + BirthYear
                , data = clean_alldf2
)
summary(model4)

# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result4-2.tex")) {
  file.remove("./outputs/lm_result4-2.tex")
}
etable(model1, model2, model3,model4,
       tex = TRUE, # LaTeXコードとして出力
       headers = list("Sample:" = list("大麻","飲酒","喫煙","総合")),
       dict = c(exSubUse = "未成年の違法使用", exSubUse_marijuana = "未成年の違法使用",
                exSubUse_alcohol = "未成年の違法使用", exSubUse_tabaco = "未成年の違法使用",
                NYG_ij = "下の兄弟の数", AGAP_ij = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result4-2.tex", # 出力先のファイル
       keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上24歳以下の出生順位が4以下を対象"
)
AddHeader_wide("lm_result4-2")


# 各種アウトカム(IsTransfered) ----------------------------------------------

model1 <- feols(IsTransfered ~ exSubUse_marijuana + NYG_ij + AGAP_ij 
                + NYG_ij:exSubUse_marijuana + exSubUse_marijuana:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)
summary(model1)

model2 <- feols(IsTransfered ~ exSubUse_alcohol + NYG_ij + AGAP_ij 
                + NYG_ij:exSubUse_alcohol + exSubUse_alcohol:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)
summary(model2)

model3 <- feols(IsTransfered ~ exSubUse_tabaco + NYG_ij + AGAP_ij 
                + NYG_ij:exSubUse_tabaco + exSubUse_tabaco:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)
summary(model3)

model4 <- feols(IsTransfered ~ exSubUse + NYG_ij + AGAP_ij 
                + NYG_ij:exSubUse + exSubUse:AGAP_ij
                + Isfemale + N_siblings
                | MotherID
                + BirthYear
                , data = clean_alldf2
)
summary(model4)

# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result5-1.tex")) {
  file.remove("./outputs/lm_result5-1.tex")
}
etable(model1, model2, model3,model4,
       tex = TRUE, # LaTeXコードとして出力
       headers = list("Sample:" = list("大麻","飲酒","喫煙","総合")),
       dict = c(exSubUse = "未成年の違法使用", exSubUse_marijuana = "未成年の違法使用",
                exSubUse_alcohol = "未成年の違法使用", exSubUse_tabaco = "未成年の違法使用",
                NYG_ij = "下の兄弟の数", AGAP_ij = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result5-1.tex", # 出力先のファイル
       keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上24歳以下の出生順位が4以下を対象"
)
AddHeader_wide("lm_result5-1")


# 各種アウトカム
model1 <- feols(IsTransfered ~ exSubUse_marijuana + NYG_ij + AGAP_ij 
                + i(NYG_ij, exSubUse_marijuana, ref = 0) + exSubUse_marijuana:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)
summary(model1)

model2 <- feols(IsTransfered ~ exSubUse_alcohol + NYG_ij + AGAP_ij 
                + i(NYG_ij, exSubUse_alcohol, ref = 0) + exSubUse_alcohol:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)

model3 <- feols(IsTransfered ~ exSubUse_tabaco + NYG_ij + AGAP_ij 
                + i(NYG_ij, exSubUse_tabaco, ref = 0) + exSubUse_tabaco:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)
summary(model3)

model4 <- feols(IsTransfered ~ exSubUse + NYG_ij + AGAP_ij 
                + i(NYG_ij, exSubUse, ref = 0) + exSubUse:AGAP_ij
                + Isfemale + N_siblings
                | MotherID
                + BirthYear
                , data = clean_alldf2
)
summary(model4)

# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result5-2.tex")) {
  file.remove("./outputs/lm_result5-2.tex")
}
etable(model1, model2, model3,model4,
       tex = TRUE, # LaTeXコードとして出力
       headers = list("Sample:" = list("大麻","飲酒","喫煙","総合")),
       dict = c(exSubUse = "未成年の違法使用", exSubUse_marijuana = "未成年の違法使用",
                exSubUse_alcohol = "未成年の違法使用", exSubUse_tabaco = "未成年の違法使用",
                NYG_ij = "下の兄弟の数", AGAP_ij = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result5-2.tex", # 出力先のファイル
       keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上24歳以下の出生順位が4以下を対象"
)
AddHeader_wide("lm_result5-2")



# 各種アウトカム(IsLiveTogether) ----------------------------------------------


model1 <- feols(IsLiveTogether ~ exSubUse_marijuana + NYG_ij + AGAP_ij 
                + NYG_ij:exSubUse_marijuana + exSubUse_marijuana:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)
summary(model1)

model2 <- feols(IsLiveTogether ~ exSubUse_alcohol + NYG_ij + AGAP_ij 
                + NYG_ij:exSubUse_alcohol + exSubUse_alcohol:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)
summary(model2)

model3 <- feols(IsLiveTogether ~ exSubUse_tabaco + NYG_ij + AGAP_ij 
                + NYG_ij:exSubUse_tabaco + exSubUse_tabaco:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)
summary(model3)

model4 <- feols(IsLiveTogether ~ exSubUse + NYG_ij + AGAP_ij 
                + NYG_ij:exSubUse + exSubUse:AGAP_ij
                + Isfemale + N_siblings
                | MotherID
                + BirthYear
                , data = clean_alldf2
)
summary(model4)

# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result6-1.tex")) {
  file.remove("./outputs/lm_result6-1.tex")
}
etable(model1, model2, model3,model4,
       tex = TRUE, # LaTeXコードとして出力
       headers = list("Sample:" = list("大麻","飲酒","喫煙","総合")),
       dict = c(exSubUse = "未成年の違法使用", exSubUse_marijuana = "未成年の違法使用",
                exSubUse_alcohol = "未成年の違法使用", exSubUse_tabaco = "未成年の違法使用",
                NYG_ij = "下の兄弟の数", AGAP_ij = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result6-1.tex", # 出力先のファイル
       keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上24歳以下の出生順位が4以下を対象"
)
AddHeader_wide("lm_result6-1")



# 各種アウトカム
model1 <- feols(IsLiveTogether ~ exSubUse_marijuana + NYG_ij + AGAP_ij 
                + i(NYG_ij, exSubUse_marijuana, ref = 0) + exSubUse_marijuana:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)
summary(model1)

model2 <- feols(IsLiveTogether ~ exSubUse_alcohol + NYG_ij + AGAP_ij 
                + i(NYG_ij, exSubUse_alcohol, ref = 0) + exSubUse_alcohol:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)

model3 <- feols(IsLiveTogether ~ exSubUse_tabaco + NYG_ij + AGAP_ij 
                + i(NYG_ij, exSubUse_tabaco, ref = 0) + exSubUse_tabaco:AGAP_ij
                | MotherID
                + ChildID
                + BirthYear
                , data = clean_alldf2
)
summary(model3)

model4 <- feols(IsLiveTogether ~ exSubUse + NYG_ij + AGAP_ij 
                + i(NYG_ij, exSubUse, ref = 0) + exSubUse:AGAP_ij
                + Isfemale + N_siblings
                | MotherID
                + BirthYear
                , data = clean_alldf2
)
summary(model4)

# 4つの回帰モデルを1つのテーブルにまとめて表示し、LaTeX形式で出力
if (file.exists("./outputs/lm_result6-2.tex")) {
  file.remove("./outputs/lm_result6-2.tex")
}
etable(model1, model2, model3,model4,
       tex = TRUE, # LaTeXコードとして出力
       headers = list("Sample:" = list("大麻","飲酒","喫煙","総合")),
       dict = c(exSubUse = "未成年の違法使用", exSubUse_marijuana = "未成年の違法使用",
                exSubUse_alcohol = "未成年の違法使用", exSubUse_tabaco = "未成年の違法使用",
                NYG_ij = "下の兄弟の数", AGAP_ij = "下の兄弟との年齢差",
                Isfemale = "女ダミー", N_siblings = "兄弟サイズ",
                MotherID = "Mother ID", BirthYear = "Birth Year"),
       file = "./outputs/lm_result6-2.tex", # 出力先のファイル
       keep = c("未成年の","下の兄弟"),
       # title = "",
       tpt = TRUE,
       notes = "18歳以上24歳以下の出生順位が4以下を対象"
)
AddHeader_wide("lm_result6-2")
