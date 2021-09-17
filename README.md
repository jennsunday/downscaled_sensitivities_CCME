# UW_downscaled_sensitivies
UW_downscaled_sensitivies

Scripts for analysis and figures of sensitivities to downscaled projected changes in temperature, oxygen, CO2, and salinity in the California Current Marine Ecosystem.

Authors
Jennifer Sunday

Scripts and purpose
# 01_process_data
standardize data and get into shape for analysis

# 02_determine_relavent_range_of_variables
get model-driven relevant range for each environmental variable

# 03_subset_to_relevant_window
filter data to only those within the relevant range for each variable (in this temporal and spatial domain)

# 04_mean_deltas_over_shelf
Calculate on-the-shelf mean deltas for each model (2km and 12km), and each zone (bottom, 200, surface), and each response variable,  produce a table

# 05_resample&plot_each_study
Project uncertainty from anova-style data
Do this by resampling from distribution of values with mean and standard error equal to standard deviation of the mean

# 06_plot_models_over_raw_data
Plot linear model fits over raw response data
note: this is an extension of script 05, need to have "lm_anova_resampled" object in environment

# 07_deltas_each_grid_cell
for every grid cell, depth zone, environmental variable, and model domain, get nodel-prejected delta of each response into long format

# 08_caterpillar_depth
calculate a mean sensitivity from each observed response, multiplied by the delta of the relevant environmental variable in each grid cell; only include deltas from relevant occupied depths for each species, make caterpillar plot of responses

# 09_meta_by_response_type
calculate weighted mean within species and response types make caterpillar plot of meta-analyzed responses

# 10_cell_based_caterpillar
for 3 illustrative grid cells in the domain, display weighted mean within species and response types 
for species that occupy that depth in catepillar plot

# 11_viz_gridded_meta_responses_2km
visualize the postitive and negative cumulative effects for each grid cell in the 1.5km model

# 12_viz_gridded_meta_responses_12km
visualize the postitive and negative cumulative effects for each grid cell in the 12km model

# 13_viz_gridded_responses_2km_species
visualize the postitive and negative cumulative effects separated by species, for the 1.5km model

# 14_viz_gridded_responses_12km_species
visualize the postitive and negative cumulative effects separated by species, for the 2km model




