# 依存関係 --------------------------------------------------------------------

library(tidyverse)
library(modelsummary)
library(gtsummary)
library(fixest)

fitmodel <- feols(u18_subsatanceExp ~ birthOrder + | childID + motherID + year ,data = alldf)
