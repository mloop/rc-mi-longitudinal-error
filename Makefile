all: output/01_simulated_data.rds output/02_simple_lm.rds figs/03_bias_plot.png
.PHONY: all

figs/03_bias_plot.png: scripts/03_extract_results.R output/02_simple_lm.rds
	cd scripts/ && Rscript 03_extract_results.R

output/02_simple_lm.rds: scripts/02_model.rds output/01_simulated_data.rds scripts/01_simulate_data.R
	cd scripts/ && Rscript 02_model.R
	
output/01_simulated_data.rds: scripts/01_simulate_data.R
	cd scripts/ && Rscript 01_simulate_data.R
	

	
