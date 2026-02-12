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
#- 4.2.1: Run
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
#- 4.2.2: Run a version just to show you the test being used
T4_tests <- ternG(
  data = VTE_clinical_features_i %>% select(-any_VTE_index_RA),
  group_var = "any_VTE_index",
  force_ordinal = c(
    "ISS", "AIS_abdomen", "AIS_thorax", "AIS_spine",
    "hospital_days", "ICU_days", "vent_days", "time_to_ppx"
  ),
  descriptive = TRUE,
  OR_col = FALSE,
  OR_method = "dynamic",
  consider_normality = FALSE,
  print_normality = FALSE,
  show_test = TRUE,
  p_digits = 2
)
print(T4_tests, n = Inf)

# Fisher variables = Sex, Injury Type, IVC Repair Type, readmission_wi_30d
#+ 4.3: Compute corrected p-values using Fisher's preferred method for 2x2 categorical variables
# Note: R's default fisher.test() uses Fisher-Irwin method. Fisher himself preferred
# doubling the smaller one-tailed p-value (max 1.0) for two-sided tests.
# For regulatory/medical applications, this approach is more appropriate.
#- 4.3.1: Identify categorical 2-level variables in the data
categorical_vars_t4 <- c("sex", "injury_type", "IVC_repair_type", "readmission_wi_30d")
#- 4.3.2: Compute corrected p-values
corrected_pvals_t4 <- compute_fisher_pvalues(
  data = VTE_clinical_features_i %>% select(-any_VTE_index_RA),
  group_var = "any_VTE_index",
  categorical_vars = categorical_vars_t4
)
#- 4.3.3: Print formatted results
print_corrected_pvalues(corrected_pvals_t4, digits = 2)
cat("NOTE: These corrected p-values should replace the values in Table 4 for\n")
cat("the categorical variables listed above. Other p-values remain unchanged.\n\n")
#! These values were manually corrected in the word document output for table 4
