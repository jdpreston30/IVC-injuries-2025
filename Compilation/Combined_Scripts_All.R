# ===== File: 00_setup_and_import.r =====
#* 0: Setup
#+ 0.1: Install all dependencies if missing
  #- 0.1.1: CRAN packages vector
    packages <- c(
      "dplyr", "tidyr", "stringr", "readxl", "labelled", "ggplot2",
      "flextable", "officer", "tibble", "forcats", "stats", "purrr",
      "broom", "table1", "readr", "epitools", "openxlsx", "devtools"
    )
  #- 0.1.2: Install any missing packages (CRAN)
    for (pkg in packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg)
      }
    }
  #- 0.1.3: Install any missing packages (GitHub)
    if (!requireNamespace("TernTablesR", quietly = TRUE)) {
      devtools::install_github("jdpreston30/TernTablesR")
    }
#+ 0.2: Load Packages
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
  library(table1)
  library(readr)
  library(epitools)
  library(openxlsx)
  library(TernTablesR)
#+ 0.3: Call all utility functions
  purrr::walk(
    list.files(
      here::here("R", "Utilities"),
      pattern = "\\.[rR]$",
      full.names = TRUE,
      recursive = TRUE
    ),
    source
  )
#+ 0.4: Import data
  final <- dynamic_import(raw_path)

# ===== File: 01_table1.r =====
#* 1: Table 1 (Demographics)
#+ 1.1: Prep data for descriptive statistics
  final_descriptive <- final %>%
    mutate(
      readmission_wi_30d = if_else(
        DC_timing %in% c("died after 72h during readmission", "Alive"),
        readmission_wi_30d,
        NA_character_
      ),
      hospital_days_72h = if_else(
        DC_timing %in% c("died after 72h during readmission", "Alive", "died after 72h during admission"),
        hospital_days,
        NA_real_
      ),
      ICU_days_72h = if_else(
        DC_timing %in% c("died after 72h during readmission", "Alive", "died after 72h during admission"),
        ICU_days,
        NA_real_
      ),
      vent_days_72h = if_else(
        DC_timing %in% c("died after 72h during readmission", "Alive", "died after 72h during admission"),
        vent_days,
        NA_real_
      )
    ) %>%
    select(
      age, sex, BMI, injury_type, IVC_repair_type, SBP, DBP, HR, ISS,
      AIS_abdomen, AIS_thorax, AIS_spine, time_to_ppx,
      hospital_days,hospital_days_72h, ICU_days,ICU_days_72h, vent_days,
      vent_days_72h,
      readmission_wi_30d
    ) %>%
    mutate(
      IVC_repair_type = if_else(
        IVC_repair_type == "Other: Temporary Ligation and Allis clamps",
        "Ligation",
        IVC_repair_type
      )
    )
#+ 1.2: Run TernTablesR for Descriptive Statistics
  descriptive <- ternD(data = final_descriptive, output_docx = "Outputs/table1.docx", consider_normality = TRUE, print_normality = TRUE)
#+ 1.3: Run normality tests
  VTE_days_norm <- final_descriptive %>%
    summarise(across(
      where(is.numeric),
      ~ list(shapiro.test(.[!is.na(.)])),
      .names = "{.col}_shapiro"
    )) %>%
    pivot_longer(
      everything(),
      names_to = "variable",
      values_to = "shapiro_result"
    ) %>%
    mutate(
      W = map_dbl(shapiro_result, ~ .x$statistic),
      p_value = map_dbl(shapiro_result, ~ .x$p.value)
    ) %>%
    select(variable, W, p_value)

# ===== File: 02_table2.r =====
#* 2: Table 2 (Survival and mechanism by injury type)
#+ 2.1: Prep Data
  #- 2.1.1: Select variables of interest
    descriptive_i <- final %>%
      select(ID, injury_type, IVC_injury_loc_simpl, IVC_repair_type, DC_timing)
  #- 2.1.2: Prepare the dataset with Mortality categories
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
#+ 2.2: Generate Table
  #- 2.2.1: Create Table Labels
    label(injury_counts$Mechanism) <- "Mechanism"
    label(injury_counts$Mortality) <- "Mortality"
    label(injury_counts$IVC_injury_group) <- "IVC Injury Group"
  #- 2.2.2: Generate the table using table1
    table2_output <- table1(
      ~ Mechanism + Mortality | IVC_injury_group,
      data = injury_counts,
      overall = "Total",
      render.categorical = my_render_cat
    )
    table2_df <- as.data.frame(table2_output)
  #- 2.2.3: Prep Microsoft Word Table
    group_totals_table2 <- injury_counts %>%
      group_by(IVC_injury_group) %>%
      summarise(total = n())
    header_labels_table2 <- c(
      "", # Row labels column
      paste0(group_totals_table2$IVC_injury_group, "\nn = ", group_totals_table2$total), # Dynamic group labels
      paste0("Total\nn = ", sum(group_totals_table2$total)))
  #- 2.2.4: Prep Microsoft Word Table with flextable
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

# ===== File: 03_table3.r =====
#* 3: Table 3: VTE Table
#+ 3.1: Preprocess all data
  VTE_days <- final %>%
    filter(IVC_repair_type != "Ligation", analyze == "Y") %>%
    # filter(DC_timing!="died after 72h during admission") %>%
    # including the 72h patients in this analysis so commented out
    select(any_VTE_index_RA:first_PE_day, first_VTE_day_index:first_PE_day_index, first_VTE_day_RA:first_PE_day_RA, DC_status) %>%
    arrange(first_VTE_day)
#+ 3.2: Run normality test on first VTE day
  VTE_days_norm <- VTE_days %>%
    select(first_VTE_day:first_PE_day) %>%
    summarise(across(
      everything(),
      ~ list(shapiro.test(.[!is.na(.)])),
      .names = "{.col}_shapiro"
    )) %>%
    tidyr::pivot_longer(everything(),
      names_to = "variable",
      values_to = "shapiro_result"
    ) %>%
    mutate(
      W = map_dbl(shapiro_result, ~ .x$statistic),
      p_value = map_dbl(shapiro_result, ~ .x$p.value)
    ) %>%
    select(variable, W, p_value)
  #! Two are normal, other two are not. For consistency, using medians
#+ 3.3: Any_VTE_index_RA Timing
  #- 3.3.1: Import data and select relevant columns
    timing_table <- VTE_days %>%
      select(first_VTE_day:first_PE_day) %>%
      summarise(across(
        everything(),
        ~ sprintf(
          "%.0f [%.0f–%.0f]",
          median(.x, na.rm = TRUE),
          quantile(.x, 0.25, na.rm = TRUE),
          quantile(.x, 0.75, na.rm = TRUE)
        )
      )) %>%
      select(first_DVT = first_DVT_day, first_IVCT = first_IVCT_day, first_PE = first_PE_day, first_VTE = first_VTE_day)
    #! Manually put this into word, see below
  #- 3.3.2: Run Kruskal-Wallis
    long_data_timing <- VTE_days %>%
      select(first_VTE_day, first_DVT_day, first_IVCT_day) %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
      drop_na()
    kruskal_result <- kruskal.test(Value ~ Variable, data = long_data_timing)
    #! No significant difference in VTE types in time to event, so noted this in footnotes of table
#+ 3.4: Preprocess data for counts
  VTE_prev <- VTE_days %>%
    select(any_VTE_index_RA, any_VTE_index, any_VTE_RA, first_DVT_day_index:first_PE_day_index, first_DVT_day_RA:first_PE_day_RA) %>%
    mutate(across(first_DVT_day_index:first_PE_day_RA, ~ ifelse(is.na(.), "N", "Y")))
#+ 3.5: Create count summary table
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
#+ 3.6: Bind the two tables and export
  timing_table_bind <- timing_table %>%
    mutate(Variable = "Timing (days)") %>%
    mutate(n = 66) %>%
    rename(
      DVT = first_DVT,
      IVCT = first_IVCT,
      PE = first_PE,
      Any_VTE = first_VTE
    )
  table3 <- bind_rows(summary_table_counts, timing_table_bind)
  output_csv(table3, "table3.csv")
  #! Manually copied this into word at this point

# ===== File: 04_table4.r =====
#* 4: Table 4: Clinical characteristics with and without index VTE
#+ 4.1: Pull Appropriate
  VTE_clinical_features_i <- final %>%
    filter(IVC_repair_type != "Ligation", analyze == "Y") %>%
    # filter(DC_timing!="died after 72h during admission") %>%
    # including the 72h patients in this analysis so commented out
    mutate(
      readmission_wi_30d = if_else(DC_timing %in% c("died after 72h during readmission", "Alive"), readmission_wi_30d, NA_character_)
    ) %>%
    select(any_VTE_index_RA, any_VTE_index, age, sex, BMI, injury_type, SBP, DBP, HR, ISS, AIS_abdomen, AIS_thorax, AIS_spine, time_to_ppx, hospital_days, ICU_days, vent_days, readmission_wi_30d) %>%
    mutate(any_VTE_index_RA = as.factor(any_VTE_index_RA)) %>%
    mutate(any_VTE_index = as.factor(any_VTE_index)) %>%
    mutate(readmission_wi_30d = as.factor(readmission_wi_30d)) %>%
    arrange(desc(any_VTE_index_RA))
#+ 4.2: Run TernTablesR to generate table
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

# ===== File: 05_figures.r =====
#* 5: Figure Creation
#+ 5.1: Figure 1: Flowchart
  #! Did this entirely in excel
#+ 5.2: Figure 2: Heatmap
  #- 5.2.1: Select variables of interest, then break comma multiinjuries into unique rows
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
  #- 5.2.2: Summarize n's and percentages, export
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
#+ 5.4: Figure 4: VTE with Antithrombotic (index pre-VTE)
  #- 5.4.1: Import Data and preprocess
    VTE_therapy <- final %>%
      filter(IVC_repair_type != "Ligation", analyze == "Y") %>%
      # filter(DC_timing!="died after 72h during admission") %>%
      # including the 72h patients in this analysis so commented outadmission") %>%
      select(ID, AT_VTE)
  #- 5.4.2: Testing
    #_ 5.4.2.1: Preprocess
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
          )
        ) %>%
        mutate(
          Therapy_Group = case_when(
            PPX_ASA_status %in% c("AP Only", "PPX Only") ~ "Single",
            PPX_ASA_status == "AP+PPX" ~ "Dual",
            TRUE ~ NA_character_ # will exclude "None" or malformed
          )
        )
    #_ 5.4.2.2: All
      contingency_table_all <- table(AT_VTE_therapy$PPX_ASA_status, AT_VTE_therapy$VTE_Status)
      fisher_result_all <- fisher.test(contingency_table_all)
    #_ 5.4.2.3: Nested (Dual vs Single)
      contingency_table_dvs <- table(AT_VTE_therapy$Therapy_Group, AT_VTE_therapy$VTE_Status)
      fisher_result_dvs <- fisher.test(contingency_table_dvs)
      OR_dual_single <- sprintf(
        "%.2f [%.2f–%.2f]",
        fisher_result_dvs$estimate,
        fisher_result_dvs$conf.int[1],
        fisher_result_dvs$conf.int[2]
      )
  #- 5.4.3: Compute percentages for all groups
    row_totals <- rowSums(contingency_table_all)
    row_pct <- prop.table(contingency_table_all, margin = 1) * 100
    n_pct <- matrix(
      paste0(contingency_table_all, " (", round(row_pct, 1), "%)"),
      nrow = nrow(contingency_table_all),
      dimnames = dimnames(contingency_table_all)
    )
    n_pct_df <- as.data.frame.matrix(n_pct)
  #- 5.4.4: Compute percentages for all consolidated dual v single v none
    none_row <- contingency_table_all["None", , drop = FALSE]
    contingency_table_extended <- rbind(contingency_table_dvs, None = none_row)
    row_pct_consol <- prop.table(contingency_table_extended, margin = 1) * 100
    n_pct_df_consol <- as.data.frame.matrix(
      matrix(
        paste0(contingency_table_extended, " (", round(row_pct_consol, 1), "%)"),
        nrow = nrow(contingency_table_extended),
        dimnames = dimnames(contingency_table_extended)
      )
    )
    fisher_result_dvsvn <- fisher.test(contingency_table_extended)
    #! Manually copied this into prism at this point



# ===== File: 06_data_not_shown.r =====
#* 6: Results for data not shown
#+ 6.1: Compute mortality characteristics within 72h
  #- 6.1.1: Filter to variables of interest
    survival_72 <- final %>%
      filter(DC_timing %in% c("died in first 72h", "died in first 48h", "died in first 24h")) %>%
      select(injury_type, DC_timing, MTP_MBP_registry, MTP_activation) %>%
      arrange(MTP_activation) %>%
      mutate(MTP_activation = if_else(is.na(MTP_activation), 0, MTP_activation)) %>% # assuming NA = no to be conservative
      mutate(MTP_MBP_registry = if_else(is.na(MTP_MBP_registry), 0, MTP_MBP_registry)) %>%
      mutate(
        MTP_activation    = as.integer(MTP_activation %in% c(1, TRUE)),
        MTP_MBP_registry  = as.integer(MTP_MBP_registry %in% c(1, TRUE)),
        any_MTP           = as.integer(MTP_activation == 1 | MTP_MBP_registry == 1)
      )
  #- 6.1.2: Summarize
    injury_type_summary <- survival_72 %>%
      count(injury_type, name = "n") %>%
      mutate(pct = round(100 * n / sum(n), 1))
    MTP_activation_summary <- survival_72 %>%
      count(MTP_activation, name = "n") %>%
      mutate(pct = round(100 * n / sum(n), 1))
    MTP_MBP_registry_summary <- survival_72 %>%
      count(MTP_MBP_registry, name = "n") %>%
      mutate(pct = round(100 * n / sum(n), 1))
    any_MTP_summary <- survival_72 %>%
      count(any_MTP, name = "n") %>%
      mutate(pct = round(100 * n / sum(n), 1))
#+ 6.2: Compute PE rate and PPX strategy in ligation patients
  ligation_PE_rate <- final %>%
    filter(IVC_repair_type == "Ligation", analyze == "Y") %>%
    group_by(AT_VTE) %>%
    summarise(
      n_total = n(),
      n_vte   = sum(any_VTE_index_RA == "Y", na.rm = TRUE),
      pct_vte = round(n_vte / n_total * 100, 1),
      n_pct   = paste0(n_vte, " (", pct_vte, "%)"),
      .groups = "drop"
    )
#+ 6.3: Compute elapsed time discharge to first readmission VTE
  elapsed_VTE <- final %>%
    filter(IVC_repair_type != "Ligation", analyze == "Y") %>%
    filter(DC_timing != "died after 72h during admission") %>%
    select(any_VTE_RA, RA_VTE_elapsed) %>%
    filter(any_VTE_RA == "Y") %>%
    summarise(
      median_IQR = sprintf(
        "%.0f [%.0f–%.0f]",
        median(RA_VTE_elapsed, na.rm = TRUE),
        quantile(RA_VTE_elapsed, 0.25, na.rm = TRUE),
        quantile(RA_VTE_elapsed, 0.75, na.rm = TRUE)
      )
    )
#+ 6.4: Compute percentages of DC AC agents groups
  DC_AC_groupings <- final %>%
    filter(IVC_repair_type != "Ligation", analyze == "Y") %>%
    filter(DC_timing %in% c("died after 72h during readmission", "Alive")) %>%
    select(DC_AC_group) %>%
    count(DC_AC_group) %>%
    mutate(
      pct = round(n / sum(n) * 100, 1),
      n_pct = paste0(n, " (", pct, "%)")
    ) %>%
    select(DC_AC_group, n_pct)
  ANY_PPX_grouping <- final %>%
    filter(IVC_repair_type != "Ligation", analyze == "Y") %>%
    filter(DC_timing %in% c("died after 72h during readmission", "Alive")) %>%
    mutate(ppx_flag = if_else(DC_AC_group != "-AC,-ASA", "ANY_PPX", "NO_PPX")) %>%
    count(ppx_flag) %>%
    mutate(
      pct   = round(n / sum(n) * 100, 1),
      n_pct = paste0(n, " (", pct, "%)")
    ) %>%
    filter(ppx_flag == "ANY_PPX") %>%
    select(DC_AC_group = ppx_flag, n_pct)

# ===== File: 07_sup_table1.r =====
#* 7: ST1: Compute DC AC and readmission VTE stats
#+ 7.1: Start from your filtered two-column table
  base_contingency <- final %>%
    filter(IVC_repair_type != "Ligation", analyze == "Y") %>%
    filter(DC_timing %in% c("died after 72h during readmission", "Alive")) %>%
    select(DC_AC_group, any_VTE_RA) %>%
    count(DC_AC_group, any_VTE_RA, name = "n") %>%
    tidyr::pivot_wider(
      names_from  = any_VTE_RA,
      values_from = n,
      values_fill = 0
    ) %>%
    # Guarantee columns named exactly N and Y exist (even if one level is absent)
    mutate(
      N = dplyr::coalesce(.data[["N"]], 0L),
      Y = dplyr::coalesce(.data[["Y"]], 0L)
    ) %>%
    select(DC_AC_group, N, Y)
#+ 7.2: None / ASA / AC / AC+ASA
  ASA_AC_combo <- base_contingency %>%
    mutate(
      ASA_AC_combo = case_when(
        DC_AC_group == "-AC,-ASA" ~ "None",
        DC_AC_group == "-AC,+ASA" ~ "+AP",
        stringr::str_detect(DC_AC_group, "\\+PAC|\\+TAC") & !stringr::str_detect(DC_AC_group, "\\+ASA") ~ "+AC",
        stringr::str_detect(DC_AC_group, "\\+PAC|\\+TAC") & stringr::str_detect(DC_AC_group, "\\+ASA") ~ "+AC, +AP",
        TRUE ~ "Other"
      )
    ) %>%
    group_by(ASA_AC_combo) %>%
    summarise(across(c(N, Y), sum), .groups = "drop")
#+ 7.3: ANY AC: AC_Y vs AC_N
  ANY_AC <- base_contingency %>%
    mutate(
      AC_flag = if_else(stringr::str_detect(DC_AC_group, "\\+PAC|\\+TAC"), "+AC", "-AC")
    ) %>%
    group_by(AC_flag) %>%
    summarise(across(c(N, Y), sum), .groups = "drop")
#+ 7.4: Single / Dual / None (ASA = one strategy, AC = another)
  Single_Dual_None <- base_contingency %>%
    mutate(
      strategy_count = case_when(
        DC_AC_group == "-AC,-ASA" ~ 0L,
        stringr::str_detect(DC_AC_group, "\\+PAC|\\+TAC") & stringr::str_detect(DC_AC_group, "\\+ASA") ~ 2L,
        TRUE ~ 1L
      ),
      strategy_group = case_when(
        strategy_count == 0L ~ "None",
        strategy_count == 1L ~ "Single",
        strategy_count == 2L ~ "Dual"
      )
    ) %>%
    group_by(strategy_group) %>%
    summarise(across(c(N, Y), sum), .groups = "drop")
#+ 7.5: Run fishers on all
  fisher.test(base_contingency %>% select(Y, N) %>% as.matrix())
  fisher.test(ASA_AC_combo %>% select(Y, N) %>% as.matrix())
  fisher.test(ANY_AC %>% select(Y, N) %>% as.matrix())
  fisher.test(Single_Dual_None %>% select(Y, N) %>% as.matrix())
#+ 7.6: Return all contingency tables
  DC_ppx_RAVTEs <- list(
    base_contingency = base_contingency,
    ASA_AC_combo = ASA_AC_combo,
    ANY_AC = ANY_AC,
    Single_Dual_None = Single_Dual_None
  )
#+ 7.7: Export base contingency as CSV
  output_csv(base_contingency, "ST1.csv")

