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