# Get functions from other scripts.

source("scripts/utils/prep_predictor_data.R")

source("scripts/utils/run_maxent_function.R")

predictor_data = prep_predictor_data()

goldfish_results = run_maxent(species = 'goldfish', predictor_data = predictor_data)