#* Setup
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
  #+ Write export function
    output_dir <- "Outputs/"
    write_out_csv <- function(data, filename) {
      write_csv(data, paste0(output_dir, filename))
    }
  #+ Set raw data path
    raw_path <- "/Users/jdp2019/Library/CloudStorage/OneDrive-Emory/Research/Manuscripts and Projects/Grady/IVC/raw_data/IVC_JDP.xlsx"
#* Import Data
  final <- read_excel(raw_path, sheet = "Final")
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
      write_out_csv(fig1_percentages, "fig1_percentages.csv")
#* Table 1: Survival and mechanism by injury type
  #+ Prep Data
    #- Select variables of interest
      descriptive_i <- final %>%
        select(ID, injury_type, IVC_injury_loc_simpl, IVC_repair_type, DC_timing)
    #- Prepare the dataset with Mortality categories
      injury_counts <- descriptive_i %>%
        filter(!is.na(IVC_injury_loc_simpl)) %>%
        mutate(
          IVC_injury_split = IVC_injury_loc_simpl %>%
            str_trim() %>% # Remove leading/trailing whitespace
            str_to_title() %>% # Convert to title case
            str_replace_all("\\s+", " ") # Collapse multiple spaces
        ) %>%
        separate_rows(IVC_injury_split, sep = ",\\s*") %>%
        rename(Mechanism = injury_type) %>%
        mutate(
          IVC_injury_group = case_when(
            IVC_injury_split == "Suprahepatic" ~ "Suprahepatic", # Keep as is
            IVC_injury_split == "Retrohepatic" ~ "Retrohepatic", # Keep as is
            IVC_injury_split %in% c("Suprarenal", "Juxtarenal", "Juxtaportal") ~ "Juxtarenal", # Combine these
            IVC_injury_split == "Infrarenal" ~ "Infrarenal" # Keep as is
          ),
          IVC_injury_group = factor(
            IVC_injury_group,
            levels = c("Suprahepatic", "Retrohepatic", "Juxtarenal", "Infrarenal")
          ),
          # Create the Mortality variable with specified categories
          Mortality = case_when(
            DC_timing == "died in first 24h" ~ "< 24h",
            DC_timing == "died in first 48h" ~ "24h - 72h",
            DC_timing == "died in first 72h" ~ "24h - 72h",
            DC_timing == "died after 72h during admission" ~ "> 72h",
            DC_timing == "died after 72h during readmission" ~ "> 72h",
            DC_timing == "Alive" ~ "Survived" 
          ),
          Mortality = factor(Mortality, levels = c("< 24h", "24h - 72h", "> 72h", "Survived")),
          Mechanism = factor(Mechanism, levels = c("Blunt", "Penetrating"))
        ) %>%
        select(-c(IVC_injury_split, IVC_injury_loc_simpl, DC_timing)) %>%
        arrange(ID)
      write_out_csv(injury_counts, "injury_counts.csv")
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
      table1_output <- table1(
        ~ Mechanism + Mortality | IVC_injury_group,
        data = injury_counts,
        overall = "Total",
        render.categorical = my.render.cat
      )
      table1_df <- as.data.frame(table1_output)
    #- Prep Microsoft Word Table
      group_totals_table1 <- injury_counts %>%
        group_by(IVC_injury_group) %>%
        summarise(total = n())
      header_labels_table1 <- c(
        "", # Row labels column
        paste0(group_totals_table1$IVC_injury_group, "\nn = ", group_totals_table1$total), # Dynamic group labels
        paste0("Total\nn = ", sum(group_totals_table1$total)))
    #- Prep Microsoft Word Table with flextable
      print(
        read_docx() %>%
          body_add_flextable(
            flextable(table1_df[-1, ]) %>% # Remove the first row
              set_table_properties(width = 1, layout = "autofit") %>%
              fontsize(size = 10, part = "all") %>%
              bold(part = "header") %>%
              italic(part = "header") %>%
              color(color = "black", part = "header") %>%
              bg(part = "header", bg = "#d3d3d3") %>%
              bg(j = "Total", bg = "transparent") %>% # Remove shading from the far-right column
              bold(j = "Total", part = "all") %>%
              align(align = "left", j = 1, part = "all") %>%
              align(align = "center", j = 2:ncol(table1_df), part = "all") %>% # Adjust column indices
              set_header_labels(values = header_labels_table1) %>% # Dynamically generated header labels
              colformat_double(j = 2:ncol(table1_df), digits = 0) %>% # Adjust column indices
              height(height = 0.5, part = "header") %>%
              height(height = 0.2, part = "body") %>%
              border_remove() %>%
              hline(border = fp_border(color = "black", width = 1.5), part = "header") %>% # Add border below header row
              hline_top(border = fp_border(color = "black", width = 1.5), part = "header") %>% # Add border above header row
              hline_bottom(border = fp_border(color = "black", width = 1, style = "double"), part = "body") %>% # Double line below bottom row
              bold(i = which(table1_df[-1, 1] %in% c("Mechanism", "Mortality")), part = "body")
          ),
        target = file.path("Outputs", "table1.docx")
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
    write_out_csv(stacked_bar_data, "stacked_bar_data.csv")
  #! At this point, arranged data in excel and graphed in Prism
#* Table 2: VTE Table
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
    table2 <- bind_rows(summary_table_counts, timing_table_bind)
    write_out_csv(table2, "table2.csv")
  #! Manually copied this into word at this point
#* Table 3: Clinical characteristics with and without VTE
  #+ Pull Numeric Data
    VTE_clinical_features_i <- read_excel(raw_path, sheet = "Final") %>%
      filter(IVC_repair_type != "Ligation", analyze == "Y") %>%
      # filter(DC_timing!="died after 72h during admission") %>%
      # including the 72h patients in this analysis so commented out
      select(any_VTE_index_RA, age, BMI, SBP, DBP, HR, ISS,AIS_abdomen,AIS_thorax,AIS_spine, hospital_days, ICU_days, vent_days,readmission_wi_30d,first_VTE_day) %>%
      mutate(any_VTE_index_RA = as.factor(any_VTE_index_RA)) %>%
      mutate(readmission_wi_30d = as.factor(readmission_wi_30d)) %>%
      arrange(desc(any_VTE_index_RA))
  #+ Manually specify variables for mean ± SD and t-test vs. median [IQR] and Wilcoxon test vs, dichotomous categorical Fisher's
    mean_sd_vars <- c("age", "BMI", "SBP", "DBP", "HR")
    median_iqr_vars <- c(
      "ISS", "AIS_thorax", "AIS_spine",
      "AIS_abdomen", "hospital_days", "ICU_days","vent_days")
    dichotomous_cat <- "readmission_wi_30d"
  #+ Create Summary Table
    #- Process numeric variables separately (variables that include those dying in addmission)
      summary_numeric_full <- VTE_clinical_features_i %>%
        mutate(across(all_of(dichotomous_cat), as.character)) %>%
        select(any_VTE_index_RA, all_of(c(mean_sd_vars, median_iqr_vars))) %>%
        pivot_longer(
          cols = -any_VTE_index_RA,
          names_to = "Variable",
          values_to = "Value"
        ) %>%
        filter(!Variable %in% c("hospital_days", "ICU_days", "vent_days")) %>%
        group_by(Variable) %>%
        reframe(
          VTE = case_when(
            Variable %in% mean_sd_vars ~ paste0(
              round(mean(Value[any_VTE_index_RA == "Y"], na.rm = TRUE), 1), " ± ",
              round(sd(Value[any_VTE_index_RA == "Y"], na.rm = TRUE), 1)
            ),
            Variable %in% median_iqr_vars ~ paste0(
              round(median(Value[any_VTE_index_RA == "Y"], na.rm = TRUE), 1), " [",
              round(quantile(Value[any_VTE_index_RA == "Y"], 0.25, na.rm = TRUE), 1), "-",
              round(quantile(Value[any_VTE_index_RA == "Y"], 0.75, na.rm = TRUE), 1), "]"
            )
          ),
          No_VTE = case_when(
            Variable %in% mean_sd_vars ~ paste0(
              round(mean(Value[any_VTE_index_RA == "N"], na.rm = TRUE), 1), " ± ",
              round(sd(Value[any_VTE_index_RA == "N"], na.rm = TRUE), 1)
            ),
            Variable %in% median_iqr_vars ~ paste0(
              round(median(Value[any_VTE_index_RA == "N"], na.rm = TRUE), 1), " [",
              round(quantile(Value[any_VTE_index_RA == "N"], 0.25, na.rm = TRUE), 1), "-",
              round(quantile(Value[any_VTE_index_RA == "N"], 0.75, na.rm = TRUE), 1), "]"
            )
          ),
          p_value = case_when(
            Variable %in% mean_sd_vars ~ ifelse(
              length(unique(Value[any_VTE_index_RA == "Y"])) > 1 &
                length(unique(Value[any_VTE_index_RA == "N"])) > 1,
              t.test(Value[any_VTE_index_RA == "Y"], Value[any_VTE_index_RA == "N"], var.equal = TRUE)$p.value,
              NA_real_
            ),
            Variable %in% median_iqr_vars ~ ifelse(
              length(unique(Value[any_VTE_index_RA == "Y"])) > 1 &
                length(unique(Value[any_VTE_index_RA == "N"])) > 1,
              wilcox.test(Value[any_VTE_index_RA == "Y"], Value[any_VTE_index_RA == "N"], exact = FALSE)$p.value,
              NA_real_
            )
          )
        ) %>%
        unique()
    #- Process numeric variables separately (variables that exclude patients that died in admission)
      summary_numeric_survivors <- VTE_clinical_features_i %>%
        filter(!is.na(readmission_wi_30d)) %>% # survivors only
        select(any_VTE_index_RA, all_of(c("hospital_days", "ICU_days", "vent_days"))) %>%
        pivot_longer(
          cols = -any_VTE_index_RA,
          names_to = "Variable",
          values_to = "Value"
        ) %>%
        group_by(Variable) %>%
        reframe(
          VTE = paste0(
            round(median(Value[any_VTE_index_RA == "Y"], na.rm = TRUE), 1), " [",
            round(quantile(Value[any_VTE_index_RA == "Y"], 0.25, na.rm = TRUE), 1), "-",
            round(quantile(Value[any_VTE_index_RA == "Y"], 0.75, na.rm = TRUE), 1), "]"
          ),
          No_VTE = paste0(
            round(median(Value[any_VTE_index_RA == "N"], na.rm = TRUE), 1), " [",
            round(quantile(Value[any_VTE_index_RA == "N"], 0.25, na.rm = TRUE), 1), "-",
            round(quantile(Value[any_VTE_index_RA == "N"], 0.75, na.rm = TRUE), 1), "]"
          ),
          p_value = ifelse(
            length(unique(Value[any_VTE_index_RA == "Y"])) > 1 &
              length(unique(Value[any_VTE_index_RA == "N"])) > 1,
            wilcox.test(Value[any_VTE_index_RA == "Y"], Value[any_VTE_index_RA == "N"], exact = FALSE)$p.value,
            NA_real_
          )
        )
    #- Now combine them
      summary_numeric <- bind_rows(summary_numeric_full, summary_numeric_survivors) %>%
        unique()
    #- Process categorical variable separately
      #_Quick check of if we should run Fisher's versus Chi-squared test
        tab <- table(VTE_clinical_features_i$readmission_wi_30d, VTE_clinical_features_i$any_VTE_index_RA)
        expected <- chisq.test(tab)$expected
        if (any(expected < 5)) {
          message("Use Fisher's exact test")
        } else {
          message("Use Chi-square test")
        }
      #_ Run with Fisher's  
        summary_categorical <- VTE_clinical_features_i %>%
          filter(!is.na(readmission_wi_30d)) %>% # Exclude ineligible patients
          group_by(any_VTE_index_RA) %>%
          summarise(
            Y_count = sum(readmission_wi_30d == "Y"),
            N_total = n(),
            .groups = "drop"
          ) %>%
          pivot_wider(names_from = any_VTE_index_RA, values_from = c(Y_count, N_total)) %>%
          mutate(
            Variable = "Readmission within 30 days",
            No_VTE = paste0(Y_count_N, " (", round(Y_count_N / N_total_N * 100, 1), "%)"),
            VTE = paste0(Y_count_Y, " (", round(Y_count_Y / N_total_Y * 100, 1), "%)"),
            p_value = fisher.test(matrix(
              c(Y_count_Y, Y_count_N, N_total_Y - Y_count_Y, N_total_N - Y_count_N),
              nrow = 2
            ))$p.value
          ) %>%
          select(Variable, No_VTE, VTE, p_value)
    #- Combine numeric and categorical results
      summary_table_clin_features_VTE <- bind_rows(summary_numeric, summary_categorical) %>%
          mutate(Variable = factor(Variable, levels = c("age", "BMI","SBP", "DBP", "HR", "ISS", "AIS_abdomen", "AIS_thorax", "AIS_spine", "hospital_days", "ICU_days", "vent_days", "Readmission within 30 days"))) %>%
          select(Variable, No_VTE, VTE, p_value) %>%
          arrange(Variable) %>%
          rename(
            !!paste0("VTE (n = ", sum(VTE_clinical_features_i$any_VTE_index_RA == "Y", na.rm = TRUE), ")") := VTE,
            !!paste0("No VTE (n = ", sum(VTE_clinical_features_i$any_VTE_index_RA == "N", na.rm = TRUE), ")") := No_VTE
          )
    #- Output table to word
      print(
        read_docx() %>%
          body_add_flextable(
            flextable(summary_table_clin_features_VTE) %>%
              set_table_properties(width = 1, layout = "autofit") %>%
              fontsize(size = 10, part = "all") %>%
              bold(part = "header") %>%
              italic(part = "header") %>%
              color(color = "black", part = "header") %>%
              bg(part = "header", bg = "#d3d3d3") %>%
              align(align = "left", j = 1, part = "all") %>%
              align(align = "center", j = 2:ncol(summary_table_clin_features_VTE), part = "all") %>%
              set_header_labels(values = c(Variable = "Variable",`No VTE` = "No VTE", VTE = "VTE", p_value = "P-value")) %>%
              colformat_double(j = 4, digits = 3) %>%
              height(height = 0.5, part = "header") %>%
              height(height = 0.2, part = "body") %>%
              border_remove() %>%
              hline(border = fp_border(color = "black", width = 1.5), part = "header") %>%
              hline_top(border = fp_border(color = "black", width = 1.5), part = "header") %>%
              hline_bottom(border = fp_border(color = "black", width = 1, style = "double"), part = "body")
          ),
        target = file.path("Outputs", "table3.docx")
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
      #_ASA (AP) and PPX Index
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
          )) 
        contingency_table <- table(AT_VTE_therapy$PPX_ASA_status, AT_VTE_therapy$VTE_Status)
        fisher_result <- fisher.test(contingency_table)
    #! Manually copied this into prism at this point