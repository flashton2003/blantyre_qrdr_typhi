# The role of each script

`pefloxacin_typhi_data_cleaning.Rmd` is for taking the various raw data sources and producing two datasets for further analysis/plotting. These are saved in `data` directory

`pefloxacin_typhi_functions.R` has various helper functions to reduce the amount of code in `pefloxacin_typhi_data_cleaning.Rmd` and `pefloxacin_typhi_analysis_and_figures.Rmd`

`pefloxacin_typhi_analysis_and_figures.Rmd` is for running the association models and producing graphs, it reads in the data from `data` dir.
