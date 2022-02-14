all: ../data/01_simulated_data.rds ../output/02_naive.rds ../output/03_complete_case.rds ../output/04_snipe.rds ../output/05_multiple_imputation.rds ../output/07_true_model.rds

../output/07_true_model.rds: 07_true_model.R ../data/01_simulated_data.rds
	Rscript 07_true_model.R

../output/05_multiple_imputation.rds: 05_multiple_imputation.R ../data/01_simulated_data.rds
	Rscript 05_multiple_imputation.R

../output/04_snipe.rds: 04_snipe.R ../data/01_simulated_data.rds
	Rscript 04_snipe.R

../output/03_complete_case.rds: 03_complete_case.R ../data/01_simulated_data.rds
		Rscript 03_complete_case.R

../output/02_naive.rds: 02_naive.R ../data/01_simulated_data.rds
	Rscript 02_naive.R

../data/01_simulated_data.rds: 01_simulate_data.R
	Rscript 01_simulate_data.R
