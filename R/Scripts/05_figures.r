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
#+ 5.3: Figure 3: VTE with Antithrombotic (index pre-VTE)
#- 5.3.1: Import Data and preprocess
VTE_therapy <- final %>%
  filter(IVC_repair_type != "Ligation", analyze == "Y") %>%
  # filter(DC_timing!="died after 72h during admission") %>%
  # including the 72h patients in this analysis so commented outadmission") %>%
  select(ID, AT_VTE)
#- 5.3.2: Testing
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
#- 5.3.3: Compute percentages for all groups
row_totals <- rowSums(contingency_table_all)
row_pct <- prop.table(contingency_table_all, margin = 1) * 100
n_pct <- matrix(
  paste0(contingency_table_all, " (", round(row_pct, 1), "%)"),
  nrow = nrow(contingency_table_all),
  dimnames = dimnames(contingency_table_all)
)
n_pct_df <- as.data.frame.matrix(n_pct)
#- 5.3.4: Compute percentages for all consolidated dual v single v none
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


