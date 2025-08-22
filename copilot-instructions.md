# Copilot Instructions
## Purpose
You are assisting in an R/Tidyverse workflow. Prefer clear, reproducible, production-ready code with minimal side effects.
## General Style
- Use tidyverse verbs (dplyr, tidyr, stringr, purrr, forcats) and piping.
- Keep objects snake_case; avoid overwriting base names.
- Put all constants and parameters near the top of a script section.
- **Do not include library() calls** in R code snippets — packages are preloaded at startup.
## Commenting & Indentation (MANDATORY)
Use this exact hierarchy in R code:
- `#!` Issues/bookmarks/important notes
- `#*` Major sections (numbered: `#* 1:`, `#* 2:` …)
- `#+` Subsections (numbered: `#+ 1.1:`, `#+ 2.1:` …)
- `#-` Sub-subsections (numbered: `#- 1.1.1:`, `#- 2.1.1:` …)
- `#_` Checks/verification steps
Indentation rules:
- Comment headers: `#*` and `#+` at 0 spaces; `#-` and `#_` at 2 spaces from their parent level.
- Code blocks: Always indented 2 spaces under their governing comment header.
- Pattern: `#*` (0) → code (2), `#+` (0) → code (2), `#-` (2) → code (4), `#_` (2) → code (4).
Rules INSIDE functions & long pipelines:
- Within functions or within long `%>%`/`|>` pipelines, use ONLY simple `# comment` lines.
- Do NOT use `#*`, `#+`, `#-`, or `#_` inside functions or pipelines.
- Functions and pipelines belong to a single surrounding section; do not create nested subsections inside them.
NO BLANK LINES POLICY:
- Never insert blank lines anywhere. No empty lines between sections, subsections, statements, or within functions/pipelines.
## Example (no blank lines)
```r
#* 1: Load data
  df <- readr::read_csv("data.csv")
#+ 1.1: Clean names
  df <- janitor::clean_names(df)
  #- 1.1.1: Verify rows
    #_ Expect > 100 rows
    stopifnot(nrow(df) > 100)
#+ 1.2: Function example (plain comments only, no subsections)
  clean_and_summarize <- function(x) {
  # strip empty cols
  x <- x[, colSums(!is.na(x)) > 0, drop = FALSE]
  # summarize
  dplyr::summarise(dplyr::as_tibble(x), rows = dplyr::n())
  }
```
## Special Commands
- **CC** (Comment Clean): If I enter 'CC', clean and update the entire code/document to conform exactly to these rules, including removing all blank lines.
