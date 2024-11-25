lualatex -synctex=1 -interaction=nonstopmode -output-directory=./pdf/ ./lm_result1.tex
lualatex -synctex=1 -interaction=nonstopmode -output-directory=./pdf/ ./lm_result_factor1.tex
lualatex -synctex=1 -interaction=nonstopmode -output-directory=./pdf/ ./lm_result1-1.tex
lualatex -synctex=1 -interaction=nonstopmode -output-directory=./pdf/ ./lm_result1-2.tex
lualatex -synctex=1 -interaction=nonstopmode -output-directory=./pdf/ ./lm_result2.tex
lualatex -synctex=1 -interaction=nonstopmode -output-directory=./pdf/ ./lm_result2-1.tex
lualatex -synctex=1 -interaction=nonstopmode -output-directory=./pdf/ ./lm_result2-2.tex
lualatex -synctex=1 -interaction=nonstopmode -output-directory=./pdf/ ./lm_result3-1.tex
lualatex -synctex=1 -interaction=nonstopmode -output-directory=./pdf/ ./lm_result3-2.tex
lualatex -synctex=1 -interaction=nonstopmode -output-directory=./pdf/ ./lm_result4-1.tex
lualatex -synctex=1 -interaction=nonstopmode -output-directory=./pdf/ ./lm_result4-2.tex
lualatex -synctex=1 -interaction=nonstopmode -output-directory=./pdf/ ./lm_result5-1.tex
lualatex -synctex=1 -interaction=nonstopmode -output-directory=./pdf/ ./lm_result5-2.tex
lualatex -synctex=1 -interaction=nonstopmode -output-directory=./pdf/ ./lm_result6-1.tex
lualatex -synctex=1 -interaction=nonstopmode -output-directory=./pdf/ ./lm_result6-2.tex

find /mnt/c/Users/watan/2024fall_NakamuroSato/outputs/pdf -type f ! -name "*.pdf" -exec rm -f {} \;