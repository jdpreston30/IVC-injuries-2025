{
  source("R/Utilities/load_dynamic_config.R")
  config <- load_dynamic_config(computer = "auto", config_path = "all_run/config_dynamic.yaml")
  source("R/Scripts/00a_environment_setup.R")
  source("R/Scripts/00b_setup_and_import.r")
  source("R/Scripts/01_table1.r")
  source("R/Scripts/02_table2.r")
  source("R/Scripts/03_table3.r")
  source("R/Scripts/04_table4.r")
  source("R/Scripts/05_figures.r")
  source("R/Scripts/06_data_not_shown.r")
  source("R/Scripts/07_sup_table1.r")
}