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
#+ 7.5: Run Fisher's preferred method on all tables and display results
# Using Fisher's method (doubling minimum one-tailed p-value) for 2x2 comparisons
# Using Fisher-Freeman-Halton for larger tables
cat("\n=== Supplemental Table 1: P-Values (Fisher's Method) ===\n\n")

cat("Base contingency (all groups):\n")
p_base <- if (nrow(base_contingency) == 2) {
  fisher_method(base_contingency %>% select(Y, N) %>% as.matrix())$p.value
} else {
  fisher.test(base_contingency %>% select(Y, N) %>% as.matrix())$p.value
}
cat(sprintf("  p = %.3f\n\n", p_base))

cat("ASA/AC combinations:\n")
p_combo <- if (nrow(ASA_AC_combo) == 2) {
  fisher_method(ASA_AC_combo %>% select(Y, N) %>% as.matrix())$p.value
} else {
  fisher.test(ASA_AC_combo %>% select(Y, N) %>% as.matrix())$p.value
}
cat(sprintf("  p = %.3f\n\n", p_combo))

cat("Any AC (±AC):\n")
p_anyac <- fisher_method(ANY_AC %>% select(Y, N) %>% as.matrix())$p.value
cat(sprintf("  p = %.3f\n\n", p_anyac))

cat("Single/Dual/None:\n")
p_sdn <- if (nrow(Single_Dual_None) == 2) {
  fisher_method(Single_Dual_None %>% select(Y, N) %>% as.matrix())$p.value
} else {
  fisher.test(Single_Dual_None %>% select(Y, N) %>% as.matrix())$p.value
}
cat(sprintf("  p = %.3f\n\n", p_sdn))
#+ 7.6: Return all contingency tables
DC_ppx_RAVTEs <- list(
  base_contingency = base_contingency,
  ASA_AC_combo = ASA_AC_combo,
  ANY_AC = ANY_AC,
  Single_Dual_None = Single_Dual_None
)
#+ 7.7: Create a total column with n(%) in contingency table
#- 7.7.1: Set label map
  label_map <- c(
    "-AC,-ASA"   = "None",
    "-AC,+ASA"   = "+AP",
    "+PAC,-ASA"  = "+PAC",
    "+TAC,-ASA"  = "+TAC",
    "+PAC,+ASA"  = "+PAC, +AP",
    "+TAC,+ASA"  = "+TAC, +AP"
  )
#- 7.7.2: Desired order
  desired_order <- c("None", "+AP", "+PAC", "+TAC", "+PAC, +AP", "+TAC, +AP")
#- 7.7.3: Create ST1 with total column
  ST1 <- base_contingency %>%
    mutate(
      Label = recode(DC_AC_group, !!!label_map), # rename groups
      Total = N + Y,
      Total_col = sprintf(
        "%d (%d%%)", 
        Total, 
        floor(100 * Total / sum(N + Y) + 0.5)  # round .5 UP
      ),
      Label = factor(Label, levels = desired_order) # enforce order
    ) %>%
      arrange(Label) %>%
      select(Label, N, Y, Total_col)
#+ 7.8: Export base contingency as XLSX
write_xlsx(ST1, "Outputs/ST1.xlsx")
