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