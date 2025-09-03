#* End to End Script Running and Compilation of All Files
#+ Get Full Tree Structure of Repo
  list_tree(getwd())
#+ Create a Compiled File of all Utilities and Scripts
  combine_R_files("R/Utilities", "Compilation/Combined_Utilities_All.R")
  combine_R_files("R/Scripts", "Compilation/Combined_Scripts_All.R")
#+ Run End-to-End Pipeline
  #- Set path to raw data (Laptop)
    raw_path <- "/Users/jdp2019/Library/CloudStorage/OneDrive-Emory/Research/Manuscripts and Projects/Grady/IVC/raw_data/IVC_JDP.xlsx"
  #- Set path to raw data (Desktop)
    raw_path <- "/Users/JoshsMacbook2015/Library/CloudStorage/OneDrive-EmoryUniversity/Research/Manuscripts and Projects/Grady/IVC/raw_data/IVC_JDP.xlsx"
  #- Run pipeline for specified files (or entire repo)
    #! Input the number(s) you want it to run to (00-07 = full)
    run_scripts("00-07", raw_path)