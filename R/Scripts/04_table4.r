#* 4: Table 4: Clinical characteristics with and without index VTE
#+ 4.1: Pull Appropriate
VTE_clinical_features_i <- final %>%
  filter(IVC_repair_type != "Ligation", analyze == "Y") %>%
  # filter(DC_timing!="died after 72h during admission") %>%
  # including the 72h patients in this analysis so commented out
  mutate(
    readmission_wi_30d = if_else(DC_timing %in% c("died after 72h during readmission", "Alive"), readmission_wi_30d, NA_character_)
  ) %>%
  select(any_VTE_index_RA, any_VTE_index, age, sex, BMI, injury_type, SBP, DBP, HR, ISS, AIS_abdomen, AIS_thorax, AIS_spine, IVC_repair_type, time_to_ppx, hospital_days, ICU_days, vent_days, readmission_wi_30d) %>%
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
    "hospital_days", "ICU_days", "vent_days", "time_to_ppx"
  ),
  descriptive = TRUE,
  output_docx = "Outputs/Table4.docx",
  OR_col = FALSE,
  OR_method = "dynamic",
  consider_normality = FALSE,
  print_normality = FALSE,
  show_test = FALSE,
  p_digits = 2
)
print(T4, n = Inf)
