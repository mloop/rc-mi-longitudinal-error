all: output/01_simulated_data.rds docs/model.html
.PHONY: all

output/01_simulated_data.rds: scripts/01_simulate_data.R
	cd scripts && Rscript 01_simulate_data.R

docs/model.html: docs/model.Rmd
	cd docs/ && Rscript -e "library(rmarkdown); Sys.setenv(RSTUDIO_PANDOC='/Applications/RStudio.app/Contents/MacOS/pandoc'); render('model.Rmd')"

