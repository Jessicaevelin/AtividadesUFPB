install.packages("devtools")
devtools::source_gist('284671997992aefe295bed34bb53fde6', filename = 'backstitch.R')

infile <- ('D:/Google Drive/2 - Ufpb/P5/Econometria/Series/Eco/Cap3.Rmd')
output <- backstitch(infile, output_type = 'script', chunk_header = "#+")
cat("```r", output, "```", sep = "\n")

knitr::