#* Dependencies
  #+ Install all dependencies
    install.packages(c(
      "dplyr", "tidyr", "stringr", "readxl", "labelled", "ggplot2",
      "flextable", "officer", "tibble", "forcats", "stats", "purrr", "broom"
    ))
  #+ Load Packages
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(readxl)
    library(labelled)
    library(ggplot2)
    library(flextable)
    library(officer)
    library(tibble)
    library(forcats)
    library(stats)
    library(purrr)
    library(broom)
    library(TernTablesR)
#* Import Data
  #+ Set raw data path
    raw_path <- "/Users/jdp2019/Library/CloudStorage/OneDrive-Emory/Research/Manuscripts and Projects/Grady/IVC/raw_data/IVC_JDP.xlsx"
  #+ Import raw data
    final <- read_excel(raw_path, sheet = "Final")
  #+ Load old objects
    all_objects <- readRDS("/Users/jdp2019/Library/CloudStorage/OneDrive-Emory/Research/Manuscripts and Projects/Grady/IVC/raw_data/all_objects.rds")
    list2env(all_objects, envir = .GlobalEnv)
#* Figure 1: Flowchart
  #! Did this entirely in excel
#* Figure 2: Heatmap
  #+ Prep Data
    #- Select variables of interest, then break comma multiinjuries into unique rows
      descriptive_heat_i <- final %>%
        select(ID, IVC_loc_heatmap) %>%
        mutate(
          IVC_injury_split = IVC_loc_heatmap %>%
            str_trim() %>% # Remove leading/trailing whitespace
            str_to_title() %>% # Convert to title case
            str_replace_all("\\s+", " ") # Collapse multiple spaces
        ) %>%
        separate_rows(IVC_injury_split, sep = ",\\s*") %>%
        select(ID, IVC_injury_split)
    #- Summarize n's and percentages, export
      fig1_percentages <- descriptive_heat_i %>%
        group_by(IVC_injury_split) %>%
        summarise(
          Count = n(), # Count of injuries for each type
          Percentage = round((Count / nrow(descriptive_heat_i)) * 100), # Calculate percentage based on total population
          .groups = "drop"
        ) %>%
        mutate(
          Count_Percentage = sprintf("%d (%d%%)", Count, Percentage) # Combine count and percentage
        ) %>%
        select(IVC_injury_split, Count_Percentage) %>%
        arrange(IVC_injury_split)
      output_csv(fig1_percentages, "fig1_percentages.csv")
#* Table 1: Demographics
  final_descriptive <- final %>%
    mutate(
      hospital_days = if_else(DC_timing %in% c("died after 72h during readmission", "Alive"), hospital_days, NA_real_),
      ICU_days = if_else(DC_timing %in% c("died after 72h during readmission", "Alive"), ICU_days, NA_real_),
      vent_days = if_else(DC_timing %in% c("died after 72h during readmission", "Alive"), vent_days, NA_real_),
      readmission_wi_30d = if_else(DC_timing %in% c("died after 72h during readmission", "Alive"), readmission_wi_30d, NA_character_)
    ) %>%
    select(age,sex, BMI, injury_type,IVC_repair_type, SBP, DBP, HR, ISS, AIS_abdomen, AIS_thorax, AIS_spine, time_to_ppx, hospital_days, ICU_days, vent_days, readmission_wi_30d) %>%
    mutate(IVC_repair_type = if_else(
      IVC_repair_type == "Other: Temporary Ligation and Allis clamps",
      "Ligation",
      IVC_repair_type
    ))
  descriptive <- ternD(data = final_descriptive, output_docx = "Outputs/table1.docx")
#* Table 2: Survival and mechanism by injury type
  #+ Prep Data
    #- Select variables of interest
      descriptive_i <- final %>%
        select(ID, injury_type, IVC_injury_loc_simpl, IVC_repair_type, DC_timing)
    #- Prepare the dataset with Mortality categories
      injury_counts <- descriptive_i %>%
        filter(!is.na(IVC_injury_loc_simpl)) %>%
        mutate(
          IVC_injury_loc_simpl = str_trim(IVC_injury_loc_simpl) %>%
            str_to_title() %>%
            str_replace_all("\\s+", " "),
          IVC_injury_loc_simpl = case_when(
            IVC_injury_loc_simpl == "Suprarenal, Suprahepatic" ~ "Suprahepatic",
            IVC_injury_loc_simpl == "Suprarenal, Retrohepatic" ~ "Retrohepatic",
            IVC_injury_loc_simpl == "Juxtarenal, Juxtaportal" ~ "Juxtarenal",
            TRUE ~ IVC_injury_loc_simpl
          ),
          IVC_injury_group = case_when(
            IVC_injury_loc_simpl == "Suprahepatic" ~ "Suprahepatic",
            IVC_injury_loc_simpl == "Retrohepatic" ~ "Retrohepatic",
            IVC_injury_loc_simpl %in% c("Suprarenal", "Juxtarenal", "Juxtaportal") ~ "Juxtarenal",
            IVC_injury_loc_simpl == "Infrarenal" ~ "Infrarenal",
            TRUE ~ NA_character_
          ),
          IVC_injury_group = factor(
            IVC_injury_group,
            levels = c("Suprahepatic", "Retrohepatic", "Juxtarenal", "Infrarenal")
          ),
          Mortality = case_when(
            DC_timing == "died in first 24h" ~ "< 24h",
            DC_timing %in% c("died in first 48h", "died in first 72h") ~ "24h - 72h",
            DC_timing == "died after 72h during admission" ~ "> 72h",
            DC_timing %in% c("died after 72h during readmission", "Alive") ~ "Survived",
            TRUE ~ NA_character_
          ),
          Mortality = factor(Mortality, levels = c("< 24h", "24h - 72h", "> 72h", "Survived")),
          Mechanism = factor(injury_type, levels = c("Blunt", "Penetrating"))
        ) %>%
        select(-c(DC_timing)) %>%
        arrange(ID)
      output_csv(injury_counts, "injury_counts.csv")
  #+ Generate Table
    #- Create Table Labels
      label(injury_counts$Mechanism) <- "Mechanism"
      label(injury_counts$Mortality) <- "Mortality"
      label(injury_counts$IVC_injury_group) <- "IVC Injury Group"
    #- Custom render function for categorical variables with integer rounding
      my.render.cat <- function(x, ...) {
        c("", sapply(stats.default(x, ...), function(y) {
          with(y, sprintf("%d (%s%%)", FREQ, ifelse(round(PCT / 0.5) * 0.5 %% 1 == 0,
            as.integer(round(PCT / 0.5) * 0.5),
            round(PCT / 0.5) * 0.5
          ))) # Round percentages to integers
        }))
      }
    #- Generate the table using table1
      table2_output <- table1(
        ~ Mechanism + Mortality | IVC_injury_group,
        data = injury_counts,
        overall = "Total",
        render.categorical = my.render.cat
      )
      table2_df <- as.data.frame(table2_output)
    #- Prep Microsoft Word Table
      group_totals_table2 <- injury_counts %>%
        group_by(IVC_injury_group) %>%
        summarise(total = n())
      header_labels_table2 <- c(
        "", # Row labels column
        paste0(group_totals_table2$IVC_injury_group, "\nn = ", group_totals_table2$total), # Dynamic group labels
        paste0("Total\nn = ", sum(group_totals_table2$total)))
    #- Prep Microsoft Word Table with flextable
      print(
        read_docx() %>%
          body_add_flextable(
            flextable(table2_df[-1, ]) %>% # Remove the first row
              set_table_properties(width = 1, layout = "autofit") %>%
              fontsize(size = 10, part = "all") %>%
              bold(part = "header") %>%
              italic(part = "header") %>%
              color(color = "black", part = "header") %>%
              bg(part = "header", bg = "#d3d3d3") %>%
              bg(j = "Total", bg = "transparent") %>% # Remove shading from the far-right column
              bold(j = "Total", part = "all") %>%
              align(align = "left", j = 1, part = "all") %>%
              align(align = "center", j = 2:ncol(table2_df), part = "all") %>% # Adjust column indices
              set_header_labels(values = header_labels_table2) %>% # Dynamically generated header labels
              colformat_double(j = 2:ncol(table2_df), digits = 0) %>% # Adjust column indices
              height(height = 0.5, part = "header") %>%
              height(height = 0.2, part = "body") %>%
              border_remove() %>%
              hline(border = fp_border(color = "black", width = 1.5), part = "header") %>% # Add border below header row
              hline_top(border = fp_border(color = "black", width = 1.5), part = "header") %>% # Add border above header row
              hline_bottom(border = fp_border(color = "black", width = 1, style = "double"), part = "body") %>% # Double line below bottom row
              bold(i = which(table2_df[-1, 1] %in% c("Mechanism", "Mortality")), part = "body")
          ),
        target = file.path("Outputs", "table2.docx")
      )
#* Figure 3: Survival x Location x Repair
  #+ Take tibble prepped to make table 1 and rework to make stacked bars
    stacked_bar_data <- injury_counts %>%
      mutate(
        IVC_repair_type = case_when(
          IVC_repair_type == "Other: Temporary Ligation and Allis clamps" ~ "Ligation",
          TRUE ~ IVC_repair_type
        )
      ) %>%
      mutate(
        Mortality_bin = ifelse(Mortality == "Survived", "Survived", "Died") # Reclassify mortality
      ) %>%
      group_by(IVC_repair_type, IVC_injury_group, Mortality_bin) %>%
      summarise(Count = n(), .groups = "drop") %>%
      pivot_wider(
        names_from = Mortality_bin,
        values_from = Count,
        values_fill = list(Count = 0) # Fill missing values with 0
      ) %>%
      mutate(Total = Died + Survived) %>%
      group_by(IVC_injury_group) %>%
      mutate(
        Total_Died_in_Group = sum(Died),
        Total_Cases_in_Group = sum(Total),
        Mortality_Percent = (Total_Died_in_Group / Total_Cases_in_Group)*100
      ) %>%
      ungroup() %>%
      select(IVC_repair_type, IVC_injury_group, Survived, Died, Mortality_Percent) %>%
      arrange(IVC_injury_group, IVC_repair_type)
    output_csv(stacked_bar_data, "stacked_bar_data.csv")
  #! At this point, arranged data in excel and graphed in Prism
#* Table 3: VTE Table
  #+ Preprocess all data
    VTE_days <- read_excel(raw_path, sheet = "Final") %>%
      filter(IVC_repair_type != "Ligation", analyze == "Y") %>%
      # filter(DC_timing!="died after 72h during admission") %>%
      # including the 72h patients in this analysis so commented out
      select(any_VTE_index_RA:first_PE_day,first_VTE_day_index:first_PE_day_index,first_VTE_day_RA:first_PE_day_RA,DC_status)
  #+ Any_VTE_index_RA Timing
    #- Import data and select relevant columns
      timing_table <- VTE_days %>%
        select(where(is.numeric)) %>% # Select only numeric columns
        summarise(across(everything(), list(
          mean = ~ round(mean(.x, na.rm = TRUE)),
          sd = ~ round(sd(.x, na.rm = TRUE))
        ))) %>%
        mutate(
          first_VTE = paste(first_VTE_day_mean, " ± ", first_VTE_day_sd),
          first_DVT = paste(first_DVT_day_mean, " ± ", first_DVT_day_sd),
          first_IVCT = paste(first_IVCT_day_mean, " ± ", first_IVCT_day_sd),
          first_PE = paste(first_PE_day_mean, " ± ", first_PE_day_sd)
        ) %>%
        select(first_DVT, first_IVCT, first_PE,first_VTE)
      #! Manually put this into word, see below
    #- Run ANOVA
      long_data_timing <- VTE_days %>%
        select(first_VTE_day, first_DVT_day, first_IVCT_day) %>%
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
        drop_na()
        anova_result_timing <- aov(Value ~ Variable, data = long_data_timing)
        summary(anova_result_timing)
      #! No significant difference in VTE types in time to event, so noted this in footnotes of table
  #+ Preprocess data for counts
    VTE_prev <- VTE_days %>%
        select(any_VTE_index_RA, any_VTE_index, any_VTE_RA, first_DVT_day_index:first_PE_day_index, first_DVT_day_RA:first_PE_day_RA) %>%
        mutate(across(first_DVT_day_index:first_PE_day_RA, ~ ifelse(is.na(.), "N", "Y")))
  #+ Create count summary table
    summary_table_counts <- tibble(
      Variable = c("Incidence", "Index Hospitalization", "Readmission"),
      DVT = c(
        paste0(
          sum(VTE_prev$first_DVT_day_index == "Y" | VTE_prev$first_DVT_day_RA == "Y", na.rm = TRUE), " (",
          round(sum(VTE_prev$first_DVT_day_index == "Y" | VTE_prev$first_DVT_day_RA == "Y", na.rm = TRUE) / nrow(VTE_prev) * 100), "%)"
        ),
        paste0(
          sum(VTE_prev$first_DVT_day_index == "Y", na.rm = TRUE), " (",
          round(sum(VTE_prev$first_DVT_day_index == "Y", na.rm = TRUE) / nrow(VTE_prev) * 100), "%)"
        ),
        paste0(
          sum(VTE_prev$first_DVT_day_RA == "Y", na.rm = TRUE), " (",
          round(sum(VTE_prev$first_DVT_day_RA == "Y", na.rm = TRUE) /
            sum(!is.na(VTE_prev$first_DVT_day_RA)) * 100), "%)"
        )
      ),
      IVCT = c(
        paste0(
          sum(VTE_prev$first_IVCT_day_index == "Y" | VTE_prev$first_IVCT_day_RA == "Y", na.rm = TRUE), " (",
          round(sum(VTE_prev$first_IVCT_day_index == "Y" | VTE_prev$first_IVCT_day_RA == "Y", na.rm = TRUE) / nrow(VTE_prev) * 100), "%)"
        ),
        paste0(
          sum(VTE_prev$first_IVCT_day_index == "Y", na.rm = TRUE), " (",
          round(sum(VTE_prev$first_IVCT_day_index == "Y", na.rm = TRUE) / nrow(VTE_prev) * 100), "%)"
        ),
        paste0(
          sum(VTE_prev$first_IVCT_day_RA == "Y", na.rm = TRUE), " (",
          round(sum(VTE_prev$first_IVCT_day_RA == "Y", na.rm = TRUE) /
            sum(!is.na(VTE_prev$first_IVCT_day_RA)) * 100), "%)"
        )
      ),
      PE = c(
        paste0(
          sum(VTE_prev$first_PE_day_index == "Y" | VTE_prev$first_PE_day_RA == "Y", na.rm = TRUE), " (",
          round(sum(VTE_prev$first_PE_day_index == "Y" | VTE_prev$first_PE_day_RA == "Y", na.rm = TRUE) / nrow(VTE_prev) * 100), "%)"
        ),
        paste0(
          sum(VTE_prev$first_PE_day_index == "Y", na.rm = TRUE), " (",
          round(sum(VTE_prev$first_PE_day_index == "Y", na.rm = TRUE) / nrow(VTE_prev) * 100), "%)"
        ),
        paste0(
          sum(VTE_prev$first_PE_day_RA == "Y", na.rm = TRUE), " (",
          round(sum(VTE_prev$first_PE_day_RA == "Y", na.rm = TRUE) /
            sum(!is.na(VTE_prev$first_PE_day_RA)) * 100), "%)"
        )
      ),
      Any_VTE = c(
        paste0(
          sum(VTE_prev$any_VTE_index_RA == "Y", na.rm = TRUE), " (",
          round(sum(VTE_prev$any_VTE_index_RA == "Y", na.rm = TRUE) / nrow(VTE_prev) * 100), "%)"
        ),
        paste0(
          sum(VTE_prev$any_VTE_index == "Y", na.rm = TRUE), " (",
          round(sum(VTE_prev$any_VTE_index == "Y", na.rm = TRUE) / nrow(VTE_prev) * 100), "%)"
        ),
        paste0(
          sum(VTE_prev$any_VTE_RA == "Y", na.rm = TRUE), " (",
          round(sum(VTE_prev$any_VTE_RA == "Y", na.rm = TRUE) /
            sum(!is.na(VTE_prev$any_VTE_RA)) * 100), "%)"
        )
      ),
      n = c(
        nrow(VTE_prev),
        nrow(VTE_prev),
        sum(!is.na(VTE_prev$any_VTE_RA))
      )
    )
  #+ Bind the two tables and export
    timing_table_bind <- timing_table %>%
      mutate(Variable = "Timing (days)") %>%
      mutate(n = 66) %>%
      rename(DVT = first_DVT,
              IVCT = first_IVCT,
              PE = first_PE,
              Any_VTE = first_VTE)
    table3 <- bind_rows(summary_table_counts, timing_table_bind)
    output_csv(table3, "table3.csv")
  #! Manually copied this into word at this point
#* Suppl Table 1: Clinical characteristics with and without VTE
  #+ Pull Appropriate 
    VTE_clinical_features_i <- read_excel(raw_path, sheet = "Final") %>%
      filter(IVC_repair_type != "Ligation", analyze == "Y") %>%
      # filter(DC_timing!="died after 72h during admission") %>%
      # including the 72h patients in this analysis so commented out
      select(any_VTE_index_RA,any_VTE_index, age, sex, BMI, injury_type, SBP, DBP, HR, ISS,AIS_abdomen,AIS_thorax,AIS_spine, time_to_ppx, hospital_days, ICU_days, vent_days,readmission_wi_30d) %>%
      mutate(any_VTE_index_RA = as.factor(any_VTE_index_RA)) %>%
      mutate(any_VTE_index = as.factor(any_VTE_index)) %>%
      mutate(readmission_wi_30d = as.factor(readmission_wi_30d)) %>%
      arrange(desc(any_VTE_index_RA)) %>%
      mutate(across(
          c(hospital_days, ICU_days, vent_days),
          ~ if_else(is.na(readmission_wi_30d), NA_real_, .)
        ))
  #+ Run TernTablesR to generate table
    ST1 <- ternG(
      data = VTE_clinical_features_i %>% select(-any_VTE_index),
      group_var = "any_VTE_index_RA",
      force_ordinal = c(
        "ISS", "AIS_abdomen", "AIS_thorax", "AIS_spine",
        "hospital_days", "ICU_days", "vent_days"
      ),
      descriptive = TRUE,
      output_docx = "Outputs/ST1.docx",
      OR_col = TRUE,
      OR_method = "dynamic",
      consider_normality = FALSE,
      print_normality = FALSE
    ) 
#* Table 4: Clinical characteristics with and without index VTE
  #+ Run TernTablesR to generate table
    T4 <- ternG(
      data = VTE_clinical_features_i %>% select(-any_VTE_index_RA),
      group_var = "any_VTE_index",
      force_ordinal = c(
        "ISS", "AIS_abdomen", "AIS_thorax", "AIS_spine",
        "hospital_days", "ICU_days", "vent_days"
      ),
      descriptive = TRUE,
      output_docx = "Outputs/Table4.docx",
      OR_col = TRUE,
      OR_method = "dynamic",
      consider_normality = FALSE,
      print_normality = FALSE
    ) 
#* Figure 4: VTE with Antithrombotic
  #+ Index pre-VTE Analysis
    #- Import Data and preprocess
      VTE_therapy <- read_excel(raw_path, sheet = "Final") %>%
      filter(IVC_repair_type != "Ligation", analyze == "Y") %>%
      # filter(DC_timing!="died after 72h during admission") %>%
      # including the 72h patients in this analysis so commented outadmission") %>%
        select(ID, AT_VTE)
    #- Testing
      #_Preprocess
        AT_VTE_therapy <- VTE_therapy %>%
          separate(
            AT_VTE,
            into = c("ASA_Status", "VTE_Status", "PPX_Status", "VTE_Status_2"),
            sep = ",\\s*",
            remove = FALSE
          ) %>%
          select(-c(VTE_Status_2, AT_VTE)) %>%
          mutate(
            AT_Status = case_when(
              ASA_Status == "-ASA" ~ "None",
              ASA_Status == "+ASA" ~ "AP",
              TRUE ~ "Other" # Catch-all for any unexpected combinations
            )
          ) %>%
          mutate(
            PPX_ASA_status = case_when(
              ASA_Status == "-ASA" & PPX_Status == "-PPX" ~ "None",
              ASA_Status == "+ASA" & PPX_Status == "-PPX" ~ "AP Only",
              ASA_Status == "+ASA" & PPX_Status == "+PPX" ~ "AP+PPX",
              ASA_Status == "-ASA" & PPX_Status == "+PPX" ~ "PPX Only"
          )) %>%
          mutate(
            Therapy_Group = case_when(
              PPX_ASA_status %in% c("AP Only", "PPX Only") ~ "Single",
              PPX_ASA_status == "AP+PPX" ~ "Dual",
              TRUE ~ NA_character_  # will exclude "None" or malformed
            )
          )
      #_All
        contingency_table_all <- table(AT_VTE_therapy$PPX_ASA_status, AT_VTE_therapy$VTE_Status)
        fisher_result_all <- fisher.test(contingency_table_all)
      #_Nested (Dual vs Single)
        contingency_table_dvs <- table(AT_VTE_therapy$Therapy_Group, AT_VTE_therapy$VTE_Status)
        fisher_result_dvs <- fisher.test(contingency_table_dvs)
        OR_dual_single <- sprintf(
          "%.2f [%.2f–%.2f]",
          fisher_result_dvs$estimate,
          fisher_result_dvs$conf.int[1],
          fisher_result_dvs$conf.int[2]
        )
    #- Compute percentages for all groups
      row_totals <- rowSums(contingency_table_all)
      row_pct <- prop.table(contingency_table_all, margin = 1) * 100
      n_pct <- matrix(
        paste0(contingency_table_all, " (", round(row_pct, 1), "%)"),
        nrow = nrow(contingency_table_all),
        dimnames = dimnames(contingency_table_all)
      )
      n_pct_df <- as.data.frame.matrix(n_pct)
    #- Compute percentages for all consolidated dual v single v none
      none_row <- contingency_table_all["None", , drop = FALSE]
      contingency_table_extended <- rbind(contingency_table_dvs, None = none_row)
      row_pct_consol <- prop.table(contingency_table_extended, margin = 1) * 100
      n_pct_df <- as.data.frame.matrix(
        matrix(
          paste0(contingency_table_extended, " (", round(row_pct_consol, 1), "%)"),
          nrow = nrow(contingency_table_extended),
          dimnames = dimnames(contingency_table_extended)
        )
      )
    #! Manually copied this into prism at this point