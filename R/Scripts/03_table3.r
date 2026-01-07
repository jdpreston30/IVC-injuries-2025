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