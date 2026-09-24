#!/usr/bin/env bash
set -euo pipefail

if rg -n 'stopApp\(|df59|catch\.wt, weight_at_age|timeseries_dataset_from_array|shiny::bind_task_button|bind_task_button\(|assign\(deparse\(substitute\(range_df\)\)|range_df\[2.*-.*range_df\[1|pred_catch <- netInputs|monitor = "loss"|y - \(\(y_noise_p \+ y_noise_n\) / 2\)|https://miro\.medium\.com|1:(length|nrow|ncol)\(|spline\(|maximum_lookback \+ minimum_training_samples|predicted_features <- as\.matrix|paste\([^\n]*substitute\(code\)|x\[sample_index, , \][[:space:]]*<-|next_features\[, colnames\(predicted_(population|catch)\)\][[:space:]]*<-|aes\(x = Recruitment/1000000, y = Recruitment/1000000|scale_colour_manual\(values = c\("red", "black"\)|geom_line\(aes\(group = iter\), colour = "#D1D5DB"' server.R ui.R maelstrom.Rmd; then
  echo "A forbidden legacy or incompatible pattern was found."
  exit 1
fi

rg -n 'FLCore::setPlusGroup' server.R >/dev/null
rg -n 'stock\.wt, weight_at_age' server.R >/dev/null
rg -n 'baseline_rows <- seq\.int' server.R >/dev/null
rg -n 'buildSequenceSamples' server.R >/dev/null
rg -n 'x_values <- numeric\(n_samples \* lookback \* ncol\(feature_matrix\)\)' server.R >/dev/null
rg -n 'prediction_window <- base::array' server.R >/dev/null
rg -n 'Internal temporal tensor construction returned invalid dimensions' server.R >/dev/null
rg -n 'formatRuntimeError' server.R >/dev/null
rg -n 'withRuntimeStage' server.R >/dev/null
rg -n 'Starting MAELSTROM %s from %s' server.R >/dev/null
rg -n 'temporal-validation Keras fit' server.R >/dev/null
rg -n 'full-period Keras refit' server.R >/dev/null
rg -n 'recursive backtest: initialization %s, year %s' server.R >/dev/null
rg -n 'insertFeatureValues' server.R >/dev/null
rg -n 'target_positions <- match\(source_names, names\(target_values\)\)' server.R >/dev/null
rg -n 'next_values, predicted_catch, "Baranov-predicted catch"' server.R >/dev/null
rg -n 'maelstromPlotTheme' server.R >/dev/null
rg -n 'asMaelstromPlotly' server.R >/dev/null
rg -n 'trace\$legendgrouptitle <- NULL' server.R >/dev/null
rg -n 'y = -0\.24' server.R >/dev/null
if rg -n 'ggplotly\((traintest_plots|pred_plots)' server.R; then
  echo "An SSB plot bypasses the shared Plotly legend layout."
  exit 1
fi
rg -n 'prettyYearBreaks' server.R >/dev/null
rg -n 'annualYearBreaks <- function' server.R >/dev/null
rg -n 'ssbYearAxisTheme <- function' server.R >/dev/null
test "$(rg -c 'breaks = annualYearBreaks' server.R)" -eq 2
test "$(rg -c 'breaks = prettyYearBreaks' server.R)" -eq 1
rg -n 'xaxis = list\(tickangle = -45, automargin = TRUE\)' server.R >/dev/null
rg -n 'SSB backtest —' server.R >/dev/null
rg -n 'SSB forecast —' server.R >/dev/null
rg -n 'Recruitment backtest —' server.R >/dev/null
rg -n 'Recruitment forecast —' server.R >/dev/null
rg -n 'Temporal-validation learning curves' server.R >/dev/null
rg -n 'Error at the selected epoch' server.R >/dev/null
rg -n 'recr_obs = sp_biomass_sub\$recruitment' server.R >/dev/null
rg -n 'connection_data <- proj_biomass\[' server.R >/dev/null
rg -n 'Recruitment forecast plot requires one observed connection year' server.R >/dev/null
rg -n '"Iterations" = "#D55E00"' server.R >/dev/null
rg -n 'Points are individual initializations' server.R >/dev/null
rg -n 'code_label <- as.character\(substitute\(code\)\)' server.R >/dev/null
rg -n 'paste\(parts\[\[1L\]\], parts\[\[length\(parts\)\]\], code_label' server.R >/dev/null
rg -n "Population wide-table columns must use the '_N_' feature code" server.R >/dev/null
rg -n 'expected_catch_names <- sub\("_N_", "_C_"' server.R >/dev/null
rg -n 'coercePredictionRow' server.R >/dev/null
rg -n 'matrix\(' server.R >/dev/null
rg -n 'No population-abundance features were found' server.R >/dev/null
rg -n 'Recursive prediction found no population-abundance columns' server.R >/dev/null
rg -n 'The neural network returned %s finite output values' server.R >/dev/null
rg -n 'normalizationRangeMatrix' server.R >/dev/null
rg -n 'normalization\$values' server.R >/dev/null
rg -n 'predicted_catch <- sweep' server.R >/dev/null
rg -n 'Recursive forecast could not construct' server.R >/dev/null
rg -n 'session\$token' server.R >/dev/null
rg -n 'ensemble_iterations <- 30L' server.R >/dev/null
rg -n 'minimum_preferred_training_samples <- 8L' server.R >/dev/null
rg -n 'absolute_minimum_training_samples <- 6L' server.R >/dev/null
rg -n 'minimum_years_for_temporal_test <- 9L' server.R >/dev/null
rg -n 'preferred_lookback <- n_years -' server.R >/dev/null
rg -n 'short_series = n_training < minimum_preferred_training_samples' server.R >/dev/null
rg -n 'keras3::set_random_seed' server.R >/dev/null
rg -n 'validation_data = list' server.R >/dev/null
rg -n 'monitor = "val_loss"' server.R >/dev/null
rg -n 'bestValidationEpoch' server.R >/dev/null
if rg -n 'warn_complexity|maximum_parameters_per_observation|parameters_per_observation|Short-series mode:|Model-complexity diagnostic:' server.R; then
  echo "A removed parameter-warning path is still present."
  exit 1
fi
rg -n 'historicalRateRow' server.R >/dev/null
rg -n 'appendRecursiveYear' server.R >/dev/null
rg -n 'target_matrix = NULL' server.R >/dev/null
rg -n 'validation_target_df' server.R >/dev/null
rg -n 'final_target_df' server.R >/dev/null
rg -n 'input_range = final_input_normalizer\$range' server.R >/dev/null
rg -n 'output_range = final_output_normalizer\$range' server.R >/dev/null
rg -n 'applyRelativeFishingMortality' server.R >/dev/null
rg -n 'reference_values\[\[source_position\]\] -' server.R >/dev/null
rg -n 'scenario_survivors / reference_survivors' server.R >/dev/null
rg -n 'standardizeTemporalTargets' server.R >/dev/null
rg -n 'prepareTemporalModelData' server.R >/dev/null
rg -n 'validation_reference_fishing' server.R >/dev/null
rg -n 'final_reference_fishing' server.R >/dev/null
rg -n 'transition_fishing_mortality' server.R >/dev/null
rg -n 'reference_fishing_mortality' server.R >/dev/null
rg -n 'catch_fishing_mortality' server.R >/dev/null
rg -n 'source_year <- utils::tail\(iter_physical\$year, 1L\)' server.R >/dev/null
rg -n 'extendScenarioFishingMortality' server.R >/dev/null
rg -n 'normalizeFishingScenario <- function' server.R >/dev/null
rg -n 'forecastFishingSchedule <- function' server.R >/dev/null
rg -n 'selected_rows <- pmin\(seq_len\(depth\), nrow\(validated\)\)' server.R >/dev/null
rg -n 'current_fishing <- fishing_schedule\[i, -1L, drop = FALSE\]' server.R >/dev/null
rg -n 'fishing_schedule\[i - 1L, -1L, drop = FALSE\]' server.R >/dev/null
rg -n 'catch_fishing_mortality = current_fishing' server.R >/dev/null
rg -n 'scenario_schedule = fishing_schedule' server.R >/dev/null
rg -n 'extended\$fmort\[year_rows\] <- replacements' server.R >/dev/null
rg -n 'Annual fishing-mortality schedule failed its extension check' server.R >/dev/null
rg -n 'Annual fishing-mortality schedule failed its truncation check' server.R >/dev/null
rg -n 'A single F row did not remain constant through the forecast' server.R >/dev/null
rg -n 'The annual F scenario changed stock/GSA column names' server.R >/dev/null
rg -n 'Annual fishing-mortality schedule accepted a skipped year' server.R >/dev/null
rg -n 'readFishingScenarioFile <- function' server.R >/dev/null
rg -n 'loadAdjustedFishingMortality <- function' server.R >/dev/null
rg -n 'filetypes = c\("rds", "rda", "rdata", "RData", "csv"\)' server.R >/dev/null
rg -n 'validateCounterfactualProjectionEngine\(\)' server.R >/dev/null
rg -n 'relative-survival formula check' server.R >/dev/null
rg -n 'reference-identity check' server.R >/dev/null
rg -n 'mortality round-trip check' server.R >/dev/null
rg -n 'fishing-mortality response check' server.R >/dev/null
rg -n 'Baranov catch-response check' server.R >/dev/null
rg -n 'future-SSB F scenario check' server.R >/dev/null
if rg -n 'projectAgeStructuredPopulation|validateHybridProjectionEngine|recruitment_output_names|Neural-network recruitment sensitivity|Hybrid age-structured projection' server.R README.md VALIDATION.md; then
  echo "A superseded hybrid or recruitment-only projection path is still present."
  exit 1
fi
if rg -n 'invalidateForecastResults|Fishing mortality changed\. The previous forecast was cleared' server.R; then
  echo "Fishing-mortality changes still clear forecast results."
  exit 1
fi
if rg -n 'Annual F scenario loaded:' server.R; then
  echo "The removed F-import confirmation notification is still present."
  exit 1
fi
rg -n 'compatible_cache_schemas <- c\(' server.R >/dev/null
rg -n 'if \(!loaded_schema %in% compatible_cache_schemas\)' server.R >/dev/null
rg -n 'f_applied <<- fishing_schedule' server.R >/dev/null
rg -n 'f_applied = f_applied' server.R >/dev/null
rg -n 'knitr::kable\(f_applied' maelstrom.Rmd >/dev/null
rg -n 'population_output_names' server.R >/dev/null
rg -n 'Neural-network abundance-vector sensitivity' server.R >/dev/null
rg -n 'Counterfactual full-vector projection' README.md >/dev/null
rg -n 'Changing or loading F keeps existing figures visible' README.md >/dev/null
rg -n 'Manual acceptance test' VALIDATION.md >/dev/null
rg -n 'leaves the previous figures visible' VALIDATION.md >/dev/null
rg -n 'zero F produces zero Baranov catch' VALIDATION.md >/dev/null
rg -n 'Exact cohort survivor mass balance is intentionally not imposed' VALIDATION.md >/dev/null
rg -n 'dataStructureYearBreaks <- function' server.R >/dev/null
rg -n 'dataStructurePlotHeight <- function' server.R >/dev/null
test "$(rg -c 'facet_wrap\(~ tri_gsa, scales = "free_y", ncol = 2L\)' server.R)" -eq 3
rg -n 'class = "data-structure-plot"' server.R >/dev/null
rg -n '\.data-structure-plot' ui.R >/dev/null
rg -n '^2\.3\.2$' VERSION >/dev/null
rg -n 'MAELSTROM v2\.3 requires R >= 4\.3\.0' bootstrap.R >/dev/null
rg -n 'title: "MAELSTROM 2\.3\.2 Report"' maelstrom.Rmd >/dev/null
rg -n 'Load F Vector / Annual Matrix' ui.R >/dev/null
rg -n 'model_pred\[\[iter\]\] <<- fitted\$model' server.R >/dev/null
rg -n 'perturbed_predictions\[positive_row, \] -' server.R >/dev/null
rg -n 'pmin\(' server.R >/dev/null
rg -n 'pmax\(' server.R >/dev/null
rg -n 'setBackgroundImage\(src = "maelstrom_background\.png"\)' ui.R >/dev/null
test -s www/maelstrom_background.png
rg -n '5th–95th' README.md server.R >/dev/null

echo "Static compatibility and scientific checks passed."
