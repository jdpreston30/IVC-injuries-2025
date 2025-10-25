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
        IVC_repair_type = case_when(
          IVC_repair_type == "Other: Temporary Ligation and Allis clamps" ~ "Ligation‡",
          IVC_repair_type == "Ligation" ~ "Ligation‡",
          TRUE ~ IVC_repair_type
        ),
        IVC_repair_type = factor(
          IVC_repair_type,
          levels = c("Primary", "Ligation‡", "Patch", "Irreparable")
        ),
        Mortality = case_when(
          DC_timing == "died in first 24h" ~ "  < 24h",
          DC_timing %in% c("died in first 48h", "died in first 72h") ~ "  24h - 72h",
          DC_timing == "died after 72h during admission" ~ "  > 72h",
          DC_timing %in% c("died after 72h during readmission", "Alive") ~ "  Survived",
          TRUE ~ NA_character_
        ),
        Mortality = factor(Mortality, levels = c("  < 24h", "  24h - 72h", "  > 72h", "  Survived")),
        Mechanism = factor(injury_type, levels = c("Blunt", "Penetrating"))
      ) %>%
      select(-c(DC_timing)) %>%
      arrange(ID)
#+ 2.2: Generate Table
  #- 2.2.1: Create Table Labels
    label(injury_counts$Mechanism) <- "Mechanism"
    label(injury_counts$IVC_repair_type) <- "Repair Type"
    label(injury_counts$Mortality) <- "Mortality"
    label(injury_counts$IVC_injury_group) <- "IVC Injury Group"
  #- 2.2.2: Generate the table using table1
    table2_output <- table1(
      ~ Mechanism + IVC_repair_type + Mortality | IVC_injury_group,
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
            bold(i = which(table2_df[-1, 1] %in% c("Mechanism", "Repair Type", "Mortality")), part = "body")
        ),
      target = file.path("Outputs", "table2.docx")
    )
