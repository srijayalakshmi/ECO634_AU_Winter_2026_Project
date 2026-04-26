pkgs <- c("rvest", "tidyverse", "stringr")
new  <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(new)) install.packages(new)

library(rvest); library(tidyverse); library(stringr)

OUT_PATH <- "D:/Srijayalakshmi/AHMEDABAD UNIVERSITY/SEMESTER 2/ECO634/Project/Data"
dir.create(OUT_PATH, recursive = TRUE, showWarnings = FALSE)


# 1. Configuration

ALL_STATES <- c(
  "andhra-pradesh","arunachal-pradesh","assam","bihar","chhattisgarh","goa",
  "gujarat","haryana","himachal-pradesh","jharkhand","karnataka","kerala",
  "madhya-pradesh","maharashtra","manipur","meghalaya","mizoram","nagaland",
  "odisha","punjab","rajasthan","sikkim","tamil-nadu","telangana","tripura",
  "uttar-pradesh","uttarakhand","west-bengal","delhi","jammu-and-kashmir",
  "puducherry"
)

# UG: uncomment and fill your 5 states
# SELECTED_STATES <- c("maharashtra","tamil-nadu","rajasthan","kerala","bihar")
SELECTED_STATES <- ALL_STATES   # PG/PhD

REPORT_YEARS <- c("2018-19","2019-20","2020-21","2021-22",
                  "2022-23","2023-24","2024-25","2025-26","2026-27")

BASE_URL <- "https://prsindia.org/budgets/states"


# 2. Helpers

to_pct <- function(x) suppressWarnings(as.numeric(str_trim(x)))

# Pull first numeric % value from a string
first_pct <- function(s) {
  m <- str_extract(s, "\\d{1,3}(?:\\.\\d+)?(?=\\s*%)")
  to_pct(m)
}

# Pull ALL year-pct pairs from a string using a given year pattern position
# Returns data.frame with columns: year, pct
all_yr_pct <- function(s) {
  # Match patterns like: "2024-25 ... 44.1%" or "44.1% ... 2024-25"
  # We find all fiscal years and all % values in the sentence, then pair them
  yrs  <- str_extract_all(s, "20\\d{2}-\\d{2}")[[1]]
  pcts <- str_extract_all(s, "\\d{1,3}(?:\\.\\d+)?(?=\\s*%)")[[1]]
  if (length(yrs)==0 || length(pcts)==0) return(tibble())
  # Only trust cases where counts match (unambiguous pairing)
  if (length(yrs) != length(pcts)) return(tibble())
  tibble(year=yrs, pct=to_pct(pcts))
}


# 3. Prose extraction

extract_debt_prose <- function(full_text, report_year) {
  
  # Step 1: Split text into sentences (period + space OR newline)
  sentences <- unlist(str_split(full_text, "(?<=[.!?])\\s+|\n"))
  sentences <- str_trim(sentences)
  sentences <- sentences[nchar(sentences) > 15]
  
  # Step 2: Keep only sentences mentioning outstanding liabilities/debt
  # AND containing a % figure. Skip FRBM Act boilerplate definitions.
  debt_sents <- sentences[
    str_detect(sentences, regex("outstanding liabilit|outstanding debt",
                                ignore_case=TRUE)) &
      str_detect(sentences, "\\d{1,3}(?:\\.\\d+)?\\s*%") &
      !str_detect(sentences, regex(
        "provides annual targets|act,?\\s+\\d{4}|frbm act|fiscal responsibility act",
        ignore_case=TRUE))
  ]
  
  if (length(debt_sents)==0) return(tibble())
  
  results <- list()
  
  for (s in debt_sents) {
    
    s_lower <- str_to_lower(s)
    
    # ── Pattern 1 (Format B BE):
    #   "at the end of 2024-25 ... 44.1% of GSDP"
    #   "at the end of 2024-25, outstanding liabilities estimated to be 44.1%"
    m <- str_match(s, regex(
      "at the end of (20\\d{2}-\\d{2})[^%]{0,200}?(\\d{1,3}(?:\\.\\d+)?)\\s*%",
      ignore_case=TRUE))
    if (!is.na(m[1,2])) {
      results[[length(results)+1]] <- tibble(
        fiscal_year=m[1,2], estimate_type="budget estimate", pct=to_pct(m[1,3]))
    }
    
    # ── Pattern 2 (Format B RE in same sentence):
    #   "revised estimate for 2023-24 (43.9% of GSDP)"
    #   "revised estimate for 2023-24 (43.9%)"
    m2 <- str_match_all(s, regex(
      "revised estimate(?:\\s+for)?\\s+(20\\d{2}-\\d{2})\\s*[\\(\\[]?\\s*(\\d{1,3}(?:\\.\\d+)?)\\s*%",
      ignore_case=TRUE))[[1]]
    if (nrow(m2)>0) {
      for (i in seq_len(nrow(m2))) {
        results[[length(results)+1]] <- tibble(
          fiscal_year=m2[i,2], estimate_type="revised estimate", pct=to_pct(m2[i,3]))
      }
    }
    
    # ── Pattern 3 (Format A BE + some Format B):
    #   "In 2018-19, the outstanding liabilities are expected at 16.5%"
    #   "In 2024-25, outstanding liabilities are estimated to be 44.1%"
    #   "outstanding liabilities are estimated to be 44.1% of GSDP" (no explicit year → use report_year)
    m3 <- str_match(s, regex(
      "(?:in\\s+)?(20\\d{2}-\\d{2})[^%]{0,200}?(?:outstanding liabilit|liabilit)[^%]{0,200}?(\\d{1,3}(?:\\.\\d+)?)\\s*%",
      ignore_case=TRUE))
    # Only use if it looks like BE phrasing (estimated/expected/targeted/projected)
    if (!is.na(m3[1,2]) &&
        str_detect(s, regex("estimated|expected|targeted|projected|is estimated",
                            ignore_case=TRUE)) &&
        !str_detect(s, regex("revised estimate|actuals|actual figure|as per actual",
                             ignore_case=TRUE))) {
      results[[length(results)+1]] <- tibble(
        fiscal_year=m3[1,2], estimate_type="budget estimate", pct=to_pct(m3[1,3]))
    }
    
    # ── Pattern 3b: "outstanding liabilities are estimated to be X%" with no year
    #   → the figure must be for the report_year (BE)
    if (!str_detect(s, "20\\d{2}-\\d{2}") &&
        str_detect(s, regex("outstanding liabilit.{0,80}(estimated|expected|targeted)",
                            ignore_case=TRUE))) {
      pct_val <- first_pct(s)
      if (!is.na(pct_val)) {
        results[[length(results)+1]] <- tibble(
          fiscal_year=report_year, estimate_type="budget estimate", pct=pct_val)
      }
    }
    
    # ── Pattern 4 (Actuals explicit):
    #   "In 2022-23, as per actuals, outstanding liabilities were 48.48%"
    #   "as per actual figures for 2022-23, outstanding liabilities were X%"
    #   "In 2022-23, outstanding liabilities were X% (actuals)"
    m4 <- str_match(s, regex(
      "(?:in\\s+)?(20\\d{2}-\\d{2})[^%]{0,150}?(?:as per actual|actuals|actual figure)[^%]{0,150}?(\\d{1,3}(?:\\.\\d+)?)\\s*%",
      ignore_case=TRUE))
    if (is.na(m4[1,2])) {
      # Try reversed: "as per actuals for YYYY-YY, X%"
      m4 <- str_match(s, regex(
        "(?:as per actual|actuals)[^%]{0,100}?(20\\d{2}-\\d{2})[^%]{0,100}?(\\d{1,3}(?:\\.\\d+)?)\\s*%",
        ignore_case=TRUE))
    }
    if (!is.na(m4[1,2])) {
      results[[length(results)+1]] <- tibble(
        fiscal_year=m4[1,2], estimate_type="actuals", pct=to_pct(m4[1,3]))
    }
    
    # ── Pattern 5 (Actuals implicit — past tense):
    #   "outstanding liabilities increased in 2020-21 (19.9% of GSDP)"
    #   "was 16.16% of GSDP in 2016-17"
    #   "stood at X% of GSDP in YYYY-YY"
    #   "had reached X% in YYYY-YY"
    #   Only if sentence does NOT already contain "estimated/expected/projected" 
    #   (those would be BE/RE, not actuals)
    if (!str_detect(s, regex(
      "estimated|expected|targeted|projected|revised estimate",
      ignore_case=TRUE))) {
      m5a <- str_match(s, regex(
        "(\\d{1,3}(?:\\.\\d+)?)\\s*%[^%]{0,80}?(?:in|for|during)\\s+(20\\d{2}-\\d{2})",
        ignore_case=TRUE))
      m5b <- str_match(s, regex(
        "(?:in|for|during)\\s+(20\\d{2}-\\d{2})[^%]{0,80}?(\\d{1,3}(?:\\.\\d+)?)\\s*%",
        ignore_case=TRUE))
      if (!is.na(m5a[1,2])) {
        results[[length(results)+1]] <- tibble(
          fiscal_year=m5a[1,3], estimate_type="actuals", pct=to_pct(m5a[1,2]))
      } else if (!is.na(m5b[1,2])) {
        results[[length(results)+1]] <- tibble(
          fiscal_year=m5b[1,2], estimate_type="actuals", pct=to_pct(m5b[1,3]))
      }
    }
  }
  
  if (length(results)==0) return(tibble())
  
  bind_rows(results) %>%
    filter(!is.na(pct), !is.na(fiscal_year), pct > 0, pct < 200) %>%
    # Remove obviously wrong % values (e.g., GSDP growth %, tax ratios mistakenly caught)
    filter(pct > 1) %>%
    distinct(fiscal_year, estimate_type, .keep_all=TRUE)
}


# 4. Table extraction

extract_debt_tables <- function(tbls, act_yr, re_yr, be_yr) {
  
  results <- list()
  
  for (t in tbls) {
    if (nrow(t) < 2 || ncol(t) < 2) next
    
    labels_col1 <- str_trim(str_remove_all(as.character(t[[1]]), "\\*+"))
    labels_row1 <- str_trim(str_remove_all(as.character(t[1,]), "\\*+"))
    
    # ── Structure A: rows are years, columns include "Debt" ──────────────────
    # Detect: header row contains "debt" or "outstanding" in a column name
    has_debt_col <- any(str_detect(labels_row1,
                                   regex("debt|outstanding", ignore_case=TRUE)))
    has_year_rows <- any(str_detect(labels_col1,
                                    regex("^(re |be |20\\d{2})", ignore_case=TRUE)))
    
    if (has_debt_col && has_year_rows) {
      # Find which column is the debt column
      debt_col_idx <- which(str_detect(labels_row1,
                                       regex("debt|outstanding", ignore_case=TRUE)))
      if (length(debt_col_idx)==0) next
      debt_col_idx <- debt_col_idx[1]
      
      # Iterate non-header rows
      for (ri in 2:nrow(t)) {
        row_label <- str_trim(str_remove_all(as.character(t[ri,1]), "\\*+"))
        cell_val  <- str_trim(as.character(t[ri, debt_col_idx]))
        
        # Parse year and estimate_type from row label
        # Patterns: "2016-17" / "RE 2017-18" / "BE 2018-19" / "2017-18 (RE)"
        yr <- str_extract(row_label, "20\\d{2}-\\d{2}")
        if (is.na(yr)) next
        
        est <- case_when(
          str_detect(str_to_lower(row_label), "^re\\b|\\(re\\)|revised") ~ "revised estimate",
          str_detect(str_to_lower(row_label), "^be\\b|\\(be\\)|budget")  ~ "budget estimate",
          str_detect(str_to_lower(row_label), "actual")                   ~ "actuals",
          TRUE ~ "actuals"   # bare year with no qualifier = actuals
        )
        
        # Extract % from cell (may be "16.5%" or "16.5" or "-16.5%")
        pct <- str_extract(cell_val, "-?\\d{1,3}(?:\\.\\d+)?(?=\\s*%)")
        if (is.na(pct)) {
          # Try as plain number
          pct <- str_extract(cell_val, "-?\\d{1,3}(?:\\.\\d+)?")
        }
        pct_val <- to_pct(pct)
        if (is.na(pct_val) || pct_val <= 0 || pct_val >= 200) next
        
        results[[length(results)+1]] <- tibble(
          fiscal_year=yr, estimate_type=est, pct=pct_val)
      }
      next  # done with this table
    }
    
    # ── Structure B: rows are metrics, columns are years ─────────────────────
    debt_rows <- which(str_detect(labels_col1,
                                  regex("outstanding liabilit|outstanding debt|debt stock",
                                        ignore_case=TRUE)))
    if (length(debt_rows)==0) next
    
    ri <- debt_rows[1]
    
    get_pct_cell <- function(ci) {
      if (ci > ncol(t)) return(NA_real_)
      cell <- str_remove_all(as.character(t[ri, ci]), "[,%]")
      v <- to_pct(str_extract(cell, "-?\\d{1,3}(?:\\.\\d+)?"))
      if (!is.na(v) && v > 0 && v < 200) v else NA_real_
    }
    
    if (!is.na(act_yr)) {
      pct <- get_pct_cell(2)
      if (!is.na(pct)) results[[length(results)+1]] <- tibble(
        fiscal_year=act_yr, estimate_type="actuals", pct=pct)
    }
    if (!is.na(re_yr)) {
      pct <- get_pct_cell(4)
      if (!is.na(pct)) results[[length(results)+1]] <- tibble(
        fiscal_year=re_yr, estimate_type="revised estimate", pct=pct)
    }
    if (!is.na(be_yr)) {
      pct <- get_pct_cell(6)
      if (!is.na(pct)) results[[length(results)+1]] <- tibble(
        fiscal_year=be_yr, estimate_type="budget estimate", pct=pct)
    }
  }
  
  if (length(results)==0) return(tibble())
  bind_rows(results) %>%
    filter(!is.na(pct)) %>%
    distinct(fiscal_year, estimate_type, .keep_all=TRUE)
}


# ── 5. Parse column year labels from any table ─────────────────────────────────

parse_col_years <- function(tbl) {
  if (is.null(tbl) || ncol(tbl)<2) return(list(act=NA_character_,re=NA_character_,be=NA_character_))
  combined <- paste(c(colnames(tbl), as.character(tbl[1,])), collapse=" ")
  yrs <- unique(str_extract_all(combined, "20\\d{2}-\\d{2}")[[1]])
  list(act=if(length(yrs)>=1)yrs[1] else NA_character_,
       re =if(length(yrs)>=2)yrs[2] else NA_character_,
       be =if(length(yrs)>=3)yrs[3] else NA_character_)
}


# 6. Main scraper

scrape_debt_page <- function(state, report_year) {
  
  url <- sprintf("%s/%s-budget-analysis-%s", BASE_URL, state, report_year)
  Sys.sleep(runif(1, 1.0, 2.0))
  
  tryCatch({
    pg   <- read_html(url)
    
    # All tables
    tbls <- pg %>% html_nodes("table") %>% html_table(fill=TRUE, header=FALSE)
    tbls <- lapply(tbls, function(t) {
      t[] <- lapply(t, as.character)
      t[rowSums(!is.na(t) & str_trim(t)!="")>0, , drop=FALSE]
    })
    tbls <- Filter(function(t) ncol(t)>=2, tbls)
    
    # Detect column-year labels from main receipts/expenditure table
    ref_tbl <- Filter(function(t)
      any(str_detect(t[[1]], regex("own tax|fiscal deficit|revenue receipts",
                                   ignore_case=TRUE))), tbls)
    yrs <- if (length(ref_tbl)>0) parse_col_years(ref_tbl[[1]]) else
      list(act=NA_character_, re=NA_character_, be=report_year)
    
    # Full page text
    full_text <- pg %>% html_text2()
    
    # Extract from prose (covers all paragraphs and bullet points)
    from_prose <- extract_debt_prose(full_text, report_year)
    
    # Extract from tables (covers both Format A and B table structures)
    from_table <- extract_debt_tables(tbls, yrs$act, yrs$re, yrs$be)
    
    # Merge: prose first (higher specificity), table fills gaps
    combined <- bind_rows(from_prose, from_table) %>%
      group_by(fiscal_year, estimate_type) %>%
      slice(1) %>%
      ungroup()
    
    n_found <- nrow(combined)
    cat(sprintf("  [%d] %-22s %s\n", n_found, state, report_year))
    
    if (n_found==0) {
      return(tibble(state=state, report_year=report_year,
                    fiscal_year=NA_character_, estimate_type=NA_character_,
                    pct=NA_real_))
    }
    
    combined %>% mutate(state=state, report_year=report_year)
    
  }, error=function(e) {
    cat(sprintf("  FAIL %-22s %s -> %s\n", state, report_year, conditionMessage(e)))
    tibble(state=state, report_year=report_year,
           fiscal_year=NA_character_, estimate_type=NA_character_, pct=NA_real_)
  })
}


# 7. Run

grid <- expand_grid(state=SELECTED_STATES, report_year=REPORT_YEARS)
cat(sprintf("Scraping %d pages. Est. ~%.0f min.\n\n",
            nrow(grid), nrow(grid)*1.5/60))

raw <- grid %>%
  pmap_dfr(function(state, report_year) {
    cat(sprintf("  %-25s %s\n", state, report_year))
    scrape_debt_page(state, report_year)
  })


#  Deduplicate & clean

est_rank <- c("actuals"=1, "revised estimate"=2, "budget estimate"=3)

debt_panel <- raw %>%
  filter(!is.na(fiscal_year), !is.na(pct)) %>%
  mutate(
    gap = abs(as.integer(str_sub(fiscal_year,1,4)) -
                as.integer(str_sub(report_year,1,4))),
    state_name = str_to_title(str_replace_all(state,"-"," "))
  ) %>%
  group_by(state, fiscal_year, estimate_type) %>%
  arrange(gap, .by_group=TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(state, state_name, fiscal_year, report_year,
         estimate_type, total_debt_outstanding_pct_gsdp=pct) %>%
  arrange(state, fiscal_year, estimate_type)


# 9. Quality report

cat("\n── QUALITY REPORT ───────────────────────────────────────────────────\n")
cat("Rows:", nrow(debt_panel),
    "| States:", n_distinct(debt_panel$state),
    "| Fiscal years:", n_distinct(debt_panel$fiscal_year), "\n\n")

cat("By estimate_type:\n")
print(count(debt_panel, estimate_type))

cat("\nCoverage by state (n rows):\n")
state_cov <- count(debt_panel, state) %>% arrange(n)
print(state_cov, n=35)

low <- filter(state_cov, n < 5)
if (nrow(low)>0) {
  cat("\nStates with < 5 observations (verify manually):\n")
  print(low)
}


# 10. Save

out <- debt_panel %>%
  mutate(across(where(is.character), str_to_lower))

# Column headers in "variable (unit)" format
names(out) <- c(
  "state",
  "state_name",
  "fiscal_year",
  "report_year",
  "estimate_type",
  "total_debt_outstanding (% of gsdp)"
)

write_csv(out, file.path(OUT_PATH, "prs_state_debt.csv"))

cat("\n── SAVED ────────────────────────────────────────────────────────────\n")
cat("  prs_state_debt.csv ->", nrow(out), "rows x", ncol(out), "cols\n")
cat("  Location:", OUT_PATH, "\n\n")
cat("NOTES:\n")
cat("  PRS reports debt only as % of GSDP — no absolute crore figure exists.\n")
cat("  To convert: multiply by GSDP (from RBI Handbook) / 100.\n")
cat("  Format A pages (pre-2022) give actuals via FRBM table rows.\n")
cat("  Format B pages (2022+) give BE + RE in prose; actuals where mentioned.\n\n")
