pkgs <- c("pdftools", "tidyverse", "stringr")
new  <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new)

library(pdftools)
library(tidyverse)
library(stringr)


# 1. Paths #

rbi_folder  <- "D:/Srijayalakshmi/AHMEDABAD UNIVERSITY/SEMESTER 2/ECO634/Project/RBI"
data_folder <- "D:/Srijayalakshmi/AHMEDABAD UNIVERSITY/SEMESTER 2/ECO634/Project/Data"


# 2. PDF edition map #

pdf_map <- tribble(
  ~edition,  ~filename,
  "2018-19", "RBI_2018-19.pdf",
  "2019-20", "RBI_2019-20.pdf",
  "2020-21", "RBI_2020-21.pdf",
  "2021-22", "RBI_2021-22.pdf",
  "2022-23", "RBI_2022-23.pdf",
  "2023-24", "RBI_2023-24.pdf",
  "2024-25", "RBI_2024-25.pdf"
)

# Editions that contain the Natural Population Growth Rate table
POP_EDITIONS <- c("2022-23", "2023-24", "2024-25")


# ── 3. State name lookups ─────────────────────────────────────────────────── #

# 3a. GSDP tables: standard state names used across all editions
gsdp_state_map <- c(
  "andaman and nicobar islands" = "andaman and nicobar islands",
  "andaman & nicobar islands"   = "andaman and nicobar islands",
  "andaman & nicobar islan"     = "andaman and nicobar islands",
  "arunachal pradesh"           = "arunachal pradesh",
  "andhra pradesh"              = "andhra pradesh",
  "assam"                       = "assam",
  "bihar"                       = "bihar",
  "chandigarh"                  = "chandigarh",
  "chhattisgarh"                = "chhattisgarh",
  "delhi"                       = "delhi",
  "goa"                         = "goa",
  "gujarat"                     = "gujarat",
  "haryana"                     = "haryana",
  "himachal pradesh"            = "himachal pradesh",
  "jammu and kashmir"           = "jammu and kashmir",
  "jammu & kashmir-u.t."        = "jammu and kashmir",
  "jammu & kashmir*"            = "jammu and kashmir",
  "jammu & kashmir"             = "jammu and kashmir",
  "jharkhand"                   = "jharkhand",
  "karnataka"                   = "karnataka",
  "kerala"                      = "kerala",
  "ladakh"                      = "ladakh",
  "madhya pradesh"              = "madhya pradesh",
  "maharashtra"                 = "maharashtra",
  "manipur"                     = "manipur",
  "meghalaya"                   = "meghalaya",
  "mizoram"                     = "mizoram",
  "nagaland"                    = "nagaland",
  "odisha"                      = "odisha",
  "puducherry"                  = "puducherry",
  "pondicherry"                 = "puducherry",
  "punjab"                      = "punjab",
  "rajasthan"                   = "rajasthan",
  "sikkim"                      = "sikkim",
  "tamil nadu"                  = "tamil nadu",
  "telangana"                   = "telangana",
  "tripura"                     = "tripura",
  "uttar pradesh"               = "uttar pradesh",
  "uttarakhand"                 = "uttarakhand",
  "west bengal"                 = "west bengal"
)

# 3b. Population growth rate table: additional variants + "NCT of Delhi"
pop_state_map <- c(
  gsdp_state_map,
  "nct of delhi"              = "delhi",
  "jammu & kashmir^"          = "jammu and kashmir",
  "dadra & nagar haveli"      = "dadra and nagar haveli",
  "daman & diu"               = "daman and diu",
  "lakshadweep"               = "lakshadweep"
)

# Section header lines to skip in population table
pop_skip_headers <- c(
  "bigger states / union territory",
  "bigger states/ union territory",
  "bigger states",
  "smaller states",
  "union territories",
  "all india"
)

# Sort both maps by key length descending for greedy prefix matching
gsdp_keys <- names(gsdp_state_map)[order(nchar(names(gsdp_state_map)), decreasing = TRUE)]
pop_keys  <- names(pop_state_map) [order(nchar(names(pop_state_map)),  decreasing = TRUE)]


# 4. Helper functions #

# Remove commas (Indian thousands separator), asterisks, whitespace.
# Returns NA for "-", ".", "NA", or empty strings.
clean_num <- function(x) {
  x <- str_trim(x)
  x <- str_remove_all(x, ",")
  x <- str_remove_all(x, "\\*")
  x[x %in% c("-", ".", "NA", "")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

# Estimate-type vintage for GSDP (lag between data year and edition year).
assign_estimate_type <- function(data_year, edition_year) {
  data_end    <- as.integer(str_sub(data_year,    6L, 7L)) + 2000L
  edition_end <- as.integer(str_sub(edition_year, 6L, 7L)) + 2000L
  lag <- edition_end - data_end
  dplyr::case_when(
    lag <= 0L ~ "advance estimate",
    lag == 1L ~ "revised estimate",
    TRUE      ~ "actuals"
  )
}

# Generic prefix-match against a sorted key vector.
# Returns canonical name or NA_character_.
match_name <- function(line_lower, keys, name_map) {
  for (key in keys) {
    if (startsWith(line_lower, key)) {
      rest <- substr(line_lower, nchar(key) + 1L, nchar(key) + 1L)
      if (rest == "" || rest == " ") return(name_map[[key]])
    }
  }
  return(NA_character_)
}


# 5. GSDP extractor (one price type from one PDF) #

extract_gsdp_price_type <- function(raw_pages, edition, price_type) {
  
  kw <- "GROSS STATE DOMESTIC PRODUCT"
  
  is_target <- function(p) {
    str_detect(p, fixed(kw)) &
      str_detect(p, fixed(price_type)) &
      str_detect(p, fixed("Concld"))
  }
  match_idx <- which(vapply(raw_pages, is_target, logical(1L)))
  
  if (length(match_idx) == 0L) {
    is_target_ci <- function(p) {
      str_detect(p, regex(kw, ignore_case = TRUE)) &
        str_detect(p, fixed(price_type)) &
        str_detect(p, fixed("Concld"))
    }
    match_idx <- which(vapply(raw_pages, is_target_ci, logical(1L)))
  }
  
  if (length(match_idx) == 0L) {
    warning("[", edition, "] GSDP ", price_type, " Concld page not found.")
    return(NULL)
  }
  
  lines <- str_split(raw_pages[match_idx[1L]], "\n")[[1L]]
  
  # Year-header: first line with 2+ fiscal "YYYY-YY" patterns
  year_cols  <- character(0)
  header_idx <- NA_integer_
  for (i in seq_along(lines)) {
    yrs <- str_extract_all(lines[i], "20\\d{2}-\\d{2}")[[1L]]
    if (length(yrs) >= 2L) { year_cols <- yrs; header_idx <- i; break }
  }
  if (length(year_cols) == 0L) {
    warning("[", edition, "] GSDP ", price_type, ": year header not found.")
    return(NULL)
  }
  
  n_years <- length(year_cols)
  rows <- list()
  
  for (line in lines[seq(header_idx + 1L, length(lines))]) {
    line_s <- str_trim(line)
    canon  <- match_name(tolower(line_s), gsdp_keys, gsdp_state_map)
    
    if (!is.na(canon)) {
      key_used  <- gsdp_keys[startsWith(tolower(line_s), gsdp_keys)][1L]
      remainder <- substr(line_s, nchar(key_used) + 1L, nchar(line_s))
      tokens    <- str_extract_all(remainder, "[0-9][0-9,]*(?:\\.[0-9]+)?|NA|-")[[1L]]
      vals      <- clean_num(tokens)
      vals      <- vals[!is.na(vals)]
      n_vals    <- min(n_years, length(vals))
      if (n_vals > 0L) {
        rows[[length(rows) + 1L]] <- tibble(
          state_name  = canon,
          year        = year_cols[seq_len(n_vals)],
          value       = vals[seq_len(n_vals)]
        )
      }
    }
  }
  
  if (length(rows) == 0L) return(NULL)
  
  bind_rows(rows) %>%
    mutate(
      price_type    = price_type,
      estimate_type = assign_estimate_type(year, edition),
      edition       = edition,
      source_pdf    = NA_character_
    )
}


# 6. Natural population growth rate extractor (all pages of table) #

extract_pop_growth <- function(raw_pages, edition) {
  
  # Identify all pages belonging to this table
  target_pages <- which(vapply(raw_pages, function(p) {
    str_detect(p, regex("NATURAL POPULATION GROWTH RATE", ignore_case = TRUE))
  }, logical(1L)))
  
  if (length(target_pages) == 0L) return(NULL)
  
  all_rows <- list()
  
  for (pg_idx in target_pages) {
    lines <- str_split(raw_pages[pg_idx], "\n")[[1L]]
    
    # Year-header: first line with 2+ standalone 4-digit calendar years
    # (must NOT match fiscal "YYYY-YY" patterns)
    year_cols  <- character(0)
    header_idx <- NA_integer_
    for (i in seq_along(lines)) {
      if (str_detect(lines[i], "20\\d{2}-\\d{2}")) next   # skip fiscal year lines
      yrs <- str_extract_all(lines[i], "\\b(20\\d{2})\\b")[[1L]]
      if (length(yrs) >= 2L) { year_cols <- yrs; header_idx <- i; break }
    }
    if (length(year_cols) == 0L) next
    
    n_years <- length(year_cols)
    
    for (line in lines[seq(header_idx + 1L, length(lines))]) {
      line_s <- str_trim(line)
      if (nchar(line_s) == 0L) next
      
      line_lower <- tolower(line_s)
      
      # Skip section header rows
      if (any(startsWith(line_lower, pop_skip_headers))) next
      
      # Strip leading row number: "1 Andhra Pradesh" -> "Andhra Pradesh"
      line_stripped <- str_remove(line_s, "^\\d+\\s+")
      canon <- match_name(tolower(line_stripped), pop_keys, pop_state_map)
      
      if (!is.na(canon)) {
        key_used  <- pop_keys[startsWith(tolower(line_stripped), pop_keys)][1L]
        remainder <- substr(line_stripped, nchar(key_used) + 1L, nchar(line_stripped))
        # Each year has 3 sub-columns: Total, Rural, Urban
        tokens <- str_extract_all(remainder, "[\\d.]+\\*?|-|\\.")[[1L]]
        vals   <- clean_num(tokens)
        # Total is at positions 1, 4, 7, ... (every 3rd, 1-indexed)
        for (i in seq_len(n_years)) {
          total_idx <- (i - 1L) * 3L + 1L
          if (total_idx <= length(vals) && !is.na(vals[total_idx])) {
            all_rows[[length(all_rows) + 1L]] <- tibble(
              state_name              = canon,
              year                    = year_cols[i],
              nat_pop_growth_rate_total = vals[total_idx],
              edition                 = edition,
              source_pdf              = NA_character_
            )
          }
        }
      }
    }
  }
  
  if (length(all_rows) == 0L) return(NULL)
  bind_rows(all_rows)
}


# 7. Per-PDF wrapper #

GSDP_EDITIONS <- pdf_map$edition   # all 7 editions have GSDP

extract_all_from_pdf <- function(pdf_path, edition) {
  message("  Reading: ", basename(pdf_path))
  raw_pages <- pdf_text(pdf_path)
  
  # GSDP — both price types
  gsdp_cp <- extract_gsdp_price_type(raw_pages, edition, "Current Prices")
  gsdp_kp <- extract_gsdp_price_type(raw_pages, edition, "Constant Prices")
  
  gsdp_out <- bind_rows(gsdp_cp, gsdp_kp) %>%
    mutate(
      gsdp_crore = value / 100,       # Rs Lakh -> Rs Crore
      gsdp_lakh  = value,
      source_pdf = basename(pdf_path)
    ) %>%
    select(-value)
  
  message("    GSDP Current: ",  if (!is.null(gsdp_cp)) nrow(gsdp_cp) else 0L,
          "  |  Constant: ",      if (!is.null(gsdp_kp)) nrow(gsdp_kp) else 0L)
  
  # Natural population growth rate (only in select editions)
  pop_out <- NULL
  if (edition %in% POP_EDITIONS) {
    pop_out <- extract_pop_growth(raw_pages, edition) %>%
      mutate(source_pdf = basename(pdf_path))
    message("    Nat. pop. growth rate: ", if (!is.null(pop_out)) nrow(pop_out) else 0L)
  }
  
  list(gsdp = gsdp_out, pop = pop_out)
}


# 8. Run across all seven PDFs #

message("\n=== Extracting data from RBI PDFs ===\n")

results_list <- map(
  seq_len(nrow(pdf_map)),
  function(i) {
    pdf_path <- file.path(rbi_folder, pdf_map$filename[i])
    if (!file.exists(pdf_path)) {
      warning("File not found: ", pdf_path); return(NULL)
    }
    extract_all_from_pdf(pdf_path, pdf_map$edition[i])
  }
)

all_gsdp_raw <- map_dfr(results_list, ~ .x$gsdp)
all_pop_raw  <- map_dfr(results_list, ~ .x$pop)

if (nrow(all_gsdp_raw) == 0L)
  stop("No GSDP data extracted. Check rbi_folder path and PDF filenames.")

message("\nRaw obs — GSDP: ", nrow(all_gsdp_raw),
        "  |  Nat. pop. growth: ", nrow(all_pop_raw))


# 9. Deduplicate GSDP #

est_levels <- c("advance estimate", "revised estimate", "actuals")

gsdp_dedup <- all_gsdp_raw %>%
  mutate(
    est_rank      = match(estimate_type, est_levels),
    edition_order = as.integer(str_sub(edition, 6L, 7L))
  ) %>%
  arrange(state_name, year, price_type, desc(est_rank), desc(edition_order)) %>%
  distinct(state_name, year, price_type, .keep_all = TRUE) %>%
  select(state_name, year, price_type, gsdp_crore, gsdp_lakh,
         estimate_type, rbi_edition = edition, source_pdf)

message("GSDP after dedup: ", nrow(gsdp_dedup), " obs")


# 10. Deduplicate natural population growth rate #

pop_dedup <- all_pop_raw %>%
  mutate(edition_order = as.integer(str_sub(edition, 6L, 7L))) %>%
  arrange(state_name, year, desc(edition_order)) %>%
  distinct(state_name, year, .keep_all = TRUE) %>%
  select(state_name, year, nat_pop_growth_rate_total,
         rbi_edition = edition, source_pdf)

message("Nat. pop. growth after dedup: ", nrow(pop_dedup), " obs")


# 11. Coverage summaries #

project_years <- c("2017-18","2018-19","2019-20","2020-21",
                   "2021-22","2022-23","2023-24","2024-25")

message("\nGSDP state coverage (project fiscal years):")
gsdp_dedup %>%
  filter(year %in% project_years) %>%
  count(year, price_type, estimate_type, name = "n_states") %>%
  arrange(year, price_type) %>%
  print(n = 50L)

message("\nNat. pop. growth state coverage (calendar years 2011-2023):")
pop_dedup %>%
  count(year, name = "n_states") %>%
  arrange(year) %>%
  print(n = 20L)


# 12. Pivot GSDP wide (current / constant as column pairs) #

gsdp_wide <- gsdp_dedup %>%
  mutate(pt = if_else(price_type == "Current Prices", "current", "constant")) %>%
  select(-price_type) %>%
  pivot_wider(
    names_from  = pt,
    values_from = c(gsdp_crore, gsdp_lakh, estimate_type, rbi_edition, source_pdf),
    names_glue  = "{.value}_{pt}"
  ) %>%
  rename(
    gsdp_current_crore          = gsdp_crore_current,
    gsdp_current_lakh           = gsdp_lakh_current,
    gsdp_constant_crore         = gsdp_crore_constant,
    gsdp_constant_lakh          = gsdp_lakh_constant,
    estimate_type_gsdp_current  = estimate_type_current,
    estimate_type_gsdp_constant = estimate_type_constant,
    rbi_edition_gsdp_current    = rbi_edition_current,
    rbi_edition_gsdp_constant   = rbi_edition_constant,
    source_pdf_gsdp_current     = source_pdf_current,
    source_pdf_gsdp_constant    = source_pdf_constant
  ) %>%
  rename(fiscal_year = year) %>%
  arrange(state_name, fiscal_year)


# 13. Rename pop columns for clarity, then stack into combined CSV #
# GSDP rows: fiscal_year = "YYYY-YY", calendar_year = NA
# Pop rows:  fiscal_year = NA,        calendar_year = "YYYY"
# A 'variable' column labels the source series for each row.

pop_rows <- pop_dedup %>%
  rename(calendar_year = year) %>%
  mutate(
    fiscal_year  = NA_character_,
    variable     = "nat_pop_growth_rate_total"
  ) %>%
  select(state_name, fiscal_year, calendar_year, variable,
         nat_pop_growth_rate_total,
         rbi_edition_pop = rbi_edition,
         source_pdf_pop  = source_pdf)

gsdp_rows <- gsdp_wide %>%
  mutate(
    calendar_year = NA_character_,
    variable      = "gsdp"
  ) %>%
  relocate(state_name, fiscal_year, calendar_year, variable)

# Full join on shared key columns, keeping all rows from both series
combined <- bind_rows(gsdp_rows, pop_rows) %>%
  arrange(state_name, fiscal_year, calendar_year)

message("\nCombined dataset: ", nrow(combined), " rows x ", ncol(combined), " columns")


# 14. Save #

out_path <- file.path(data_folder, "gsdp_rbi.csv")
write_csv(combined, out_path)
