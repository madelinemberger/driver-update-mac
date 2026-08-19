# Shared functions for the OSDS timeseries pipeline (all islands).
# Extracted from 1a-1d, which had near-identical copies of everything below.
# Sourced by 1_OSDS_TS_allislands.Rmd.

library(tidyverse)
library(janitor)

if (!exists("%!in%")) '%!in%' <- function(x, y) !('%in%'(x, y))

# ---------------------------------------------------------------------------
# TMK standardization
# ---------------------------------------------------------------------------
# Generalized version of clean_hawaii_tmk()/clean_kauai_tmk() (1c/1d), which
# were byte-identical except for the hardcoded island prefix digit ("3" vs "4").
# Produces a 9-digit tmk string starting with `prefix`.
clean_tmk <- function(df, prefix, target_col = "tmk") {
  col_names <- names(df)
  actual_col <- if (target_col %in% col_names) {
    target_col
  } else if ("full_taxid" %in% col_names) {
    "full_taxid"
  } else if ("TMK" %in% col_names) {
    "TMK"
  } else {
    target_col
  }

  df %>%
    mutate(
      raw_val = as.character(.data[[actual_col]]),
      raw_val = if_else(str_detect(raw_val, "e\\+"), sprintf("%.0f", as.numeric(raw_val)), raw_val),
      digits = str_replace_all(raw_val, "[^0-9]", ""),
      digits = if_else(nchar(digits) >= 9, substr(digits, 1, 9), digits),
      digits = case_when(
        nchar(digits) == 8 ~ paste0(prefix, digits),
        nchar(digits) == 9 & str_starts(digits, "0") ~ paste0(prefix, substr(digits, 2, 9)),
        TRUE ~ str_pad(digits, width = 9, side = "left", pad = "0")
      )
    ) %>%
    mutate(tmk = digits) %>%
    select(-any_of(c("raw_val", "digits"))) %>%
    filter(!is.na(tmk) & tmk != "" & tmk != "000000000")
}

# ---------------------------------------------------------------------------
# Small helpers (identical across all 4 original scripts)
# ---------------------------------------------------------------------------

calc_mode <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

primary_disposal <- c("Bed", "Trench", "Infiltrators / Chambers", "ET Bed-Irrigation System", "Presby")
minimal_disposal <- c("Seepage Pit", "Cesspool-New")
unknown_disposal <- c("Other", "Unknown")

assign_osds_class <- function(p_type, d_type) {
  case_when(
    p_type == "Septic Tank" & d_type %in% primary_disposal ~ "1",
    p_type == "Septic Tank" & d_type %in% c(minimal_disposal, unknown_disposal) ~ "2",
    p_type == "Septic Tank" & d_type == "Combination of two types" ~ "1",

    p_type %in% c("Compost Toilet", "Gray Water System", "Compost Toilet/Gray Water System") &
      d_type %in% unknown_disposal ~ "3",
    p_type %in% c("Compost Toilet", "Compost Toilet/Gray Water System") & d_type == "Bed" ~ "1",
    p_type == "Gray Water System" & d_type %in% c("Bed", "Trench") ~ "3",

    p_type == "Aerobic Unit" & d_type %in% primary_disposal ~ "1",
    p_type == "Aerobic Unit" & d_type %in% minimal_disposal ~ "4",
    p_type == "Aerobic Unit" & d_type %in% unknown_disposal ~ "3",
    p_type == "Aerobic Unit" & d_type == "Combination of two types" ~ "1",

    p_type %in% c("Cesspool-New", "Cesspool-Existing") ~ "4",
    p_type %in% c("Presby (passive, NSF 40)", "Holding Tank (Government Only)", "Incinerating Toilet") ~ "1",

    p_type == "Other" & d_type %in% primary_disposal ~ "1",
    TRUE ~ "1" # Catch-all default
  )
}

# pitt_code -> commercial-use classification (identical case_when in 1b/1c/1d)
classify_pitt_code <- function(pitt_code_num) {
  case_when(
    pitt_code_num == 0                            ~ "Timeshare (Maui/Kauai)",
    pitt_code_num == 999                          ~ "Multiple PITT Codes",
    pitt_code_num >= 1000                         ~ "Commercialized Residential",
    pitt_code_num >= 100 & pitt_code_num < 200   ~ "Improved Residential",
    pitt_code_num >= 200 & pitt_code_num < 300   ~ "Apartment",
    pitt_code_num >= 300 & pitt_code_num < 400   ~ "Commercial",
    pitt_code_num >= 400 & pitt_code_num < 500   ~ "Industrial",
    pitt_code_num >= 500 & pitt_code_num < 600   ~ "Agricultural and Rural",
    pitt_code_num >= 600 & pitt_code_num < 700   ~ "Conservation",
    pitt_code_num >= 700 & pitt_code_num < 800   ~ "Hotel and Resort",
    pitt_code_num >= 800 & pitt_code_num < 900   ~ "Unimproved Residential",
    pitt_code_num >= 900 & pitt_code_num < 999   ~ "Homeowner",
    TRUE                                          ~ "Unknown"
  )
}

# ---------------------------------------------------------------------------
# Permit "story" (chain of osds classes over time) + upgrade scenario
# ---------------------------------------------------------------------------
# Collapses a row's values across `story_cols` into a unique, "|"-joined
# string, e.g. "8|1" (cesspool, then upgraded to class 1). Originally only
# computed for "updated" tmks (baseline + new permits); also used now for
# "new" tmks (from Step 2b's Table 3, permit history only - no baseline) and
# "unchanged" tmks (Step 3a, baseline value only - always collapses to a
# single number).
build_permit_story <- function(df, story_cols) {
  df %>%
    rowwise() %>%
    mutate(
      permit_story = {
        vals <- as.character(c_across(all_of(story_cols)))
        vals <- vals[!is.na(vals) & vals != "" & vals != "NA"]
        paste(unique(vals), collapse = "|")
      }
    ) %>%
    ungroup()
}

# Identical case_when block previously duplicated in each island's "updated"
# chunk. A single value (no "|") falls through to "no upgrade detected".
classify_upgrade_scenario <- function(unique_story) {
  case_when(
    unique_story == "9|1|8"  ~ "cesspool and septic - both repaired, no change",
    unique_story == "10|2|9" ~ "cesspool and septic 2 - septic repaired twice, cesspool once",
    unique_story == "7|9|1"  ~ "potential data error: cesspool added, then possibly updated",
    unique_story == "4|2"    ~ "septic and aerobic - replacement of aerobic",
    unique_story == "3|4"    ~ "septic combo to aerobic",
    unique_story == "1|4"    ~ "single septic to aerobic",
    unique_story == "7|4"    ~ "complex septic system to aerobic",
    unique_story == "8|5"    ~ "cesspool to septic / aerobic hybrid",
    unique_story == "5|1"    ~ "septic and aerobic to septic",
    unique_story == "7|3"    ~ "2 septics and aerobic to just 2 septics - aerobic removal",
    unique_story == "7|1|4"  ~ "2 septics and aerobic + one septic repair + one aerobic repair",
    unique_story == "10|9"   ~ "1 septic + cesspool to better septic and cesspool",
    unique_story == "" | !str_detect(unique_story, "\\|") ~ "no upgrade detected",
    str_detect(unique_story, "^8\\|.*[1234]$")           ~ "single cesspool upgraded to septic or aerobic",
    str_detect(unique_story, "^3\\|.*[12]$")             ~ "multiple septic with new permit: possible repair",
    str_detect(unique_story, "^[24]\\|.*1$")             ~ "septic or aerobic system refined to cleanest type (Class 1)",
    str_detect(unique_story, "^(9|1[0-5])\\|.*[1234]$")  ~ "multiple osds with cesspool upgraded",
    str_detect(unique_story, "^(9|1[0-5])\\|.*8$")       ~ "multiple osds with new cesspool application",
    str_detect(unique_story, "^[1-7]\\|.*(8|9|1[0-5])$") ~ "potential data error: backslide to cesspool",
    str_detect(unique_story, "^[123]\\|.*[123]$")        ~ "lateral change (clean to clean)",
    TRUE ~ "other scenario"
  )
}

# Table 3 -> Table 3b bitmask logic (identical in all 4 originals)
calculate_active_permits <- function(apps) {
  active_status <- numeric(length(apps))
  get_highest <- function(x) if (x == 0) 0 else 2^floor(log2(x))

  running_total <- apps[1]
  active_status[1] <- running_total

  for (i in 2:length(apps)) {
    if (apps[i] > 0) {
      highest <- get_highest(running_total)
      running_total <- bitwOr(bitwAnd(running_total, bitwNot(highest)), apps[i])
    }
    active_status[i] <- running_total
  }
  return(active_status)
}

# ---------------------------------------------------------------------------
# Step 2b: master_clean + Tables 1, 2a, 2b, 3 from permit data
# ---------------------------------------------------------------------------
# `permits` must already be filtered to the island and have (at least):
#   tmk, permit_type, disposal_type, final_approval_date,
#   project_street_address, project_street_address2
# `nodwell_tmks` = tmks from the permit list with no matching dwelling record.
build_master_clean <- function(permits, nodwell_tmks) {
  permits %>%
    clean_names() %>%
    mutate(
      approval_date = mdy(final_approval_date),
      year = year(approval_date),
      address_full = if_else(is.na(project_street_address2),
                              project_street_address,
                              paste(project_street_address, "-", project_street_address2)),
      address_lower = str_trim(str_to_lower(address_full)),
      osds_class = assign_osds_class(permit_type, disposal_type),
      dwelling_yn = if_else(tmk %in% nodwell_tmks, "no", "yes")
    ) %>%
    select(-any_of("permit_id")) %>%
    distinct()
}

# `dwell_data` must have columns: tmk, <bed_col>, <tax_year_col>.
# `dwell_tmks` = the (deduplicated) tmk list from the dwelling data, used to
# populate `is_dwelling` in Table 1.
build_permit_tables <- function(master_clean, dwell_data, dwell_tmks,
                                 bed_col = "bedrooms", tax_year_col = "tax_year") {

  table1_tmk_registry <- master_clean %>%
    group_by(tmk) %>%
    summarize(
      n_permits = n(),
      n_years = n_distinct(year, na.rm = TRUE),
      n_addresses = n_distinct(address_lower, na.rm = TRUE),
      min_year = min(year, na.rm = TRUE),
      max_year = max(year, na.rm = TRUE),
      is_dwelling = any(tmk %in% dwell_tmks),
      scenario = case_when(
        n_addresses == 1 & n_permits == 1 ~ "Simple",
        n_addresses > 1 ~ "Multi-Building",
        n_years > 1 ~ "Potential Upgrade",
        TRUE ~ "Multiple Permits Same Year"
      ),
      .groups = "drop"
    )

  table1_tmk_registry_commercial <- table1_tmk_registry %>%
    filter(is_dwelling == FALSE)

  dwell_for_tables <- dwell_data %>%
    filter(tmk %in% master_clean$tmk) %>%
    mutate(.bed = as.numeric(.data[[bed_col]]))

  table2a_bedrooms <- dwell_for_tables %>%
    group_by(tmk, .data[[tax_year_col]]) %>%
    summarize(total_beds = sum(.bed, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = all_of(tax_year_col), values_from = total_beds, names_sort = TRUE)

  table2b_bedrooms <- dwell_for_tables %>%
    group_by(tmk, .data[[tax_year_col]]) %>%
    summarize(mode_beds = calc_mode(.bed, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = all_of(tax_year_col), values_from = mode_beds, names_sort = TRUE)

  table3_classes <- master_clean %>%
    filter(!is.na(year)) %>%
    group_by(tmk, year) %>%
    summarize(
      all_classes_text = paste(sort(unique(osds_class)), collapse = ", "),
      .groups = "drop"
    ) %>%
    mutate(
      encoded_sum =
        (str_detect(all_classes_text, "1") * 1) +
        (str_detect(all_classes_text, "2") * 2) +
        (str_detect(all_classes_text, "3") * 4) +
        (str_detect(all_classes_text, "4") * 8)
    ) %>%
    select(tmk, year, encoded_sum) %>%
    pivot_wider(names_from = year, values_from = encoded_sum, names_sort = TRUE)

  # permit_story/upgrade_scenario from permit history alone (no baseline yet -
  # that gets factored in later, for tmks that also have OTP baseline data,
  # in Step 3's "updated" reconciliation). For a tmk with a single permit
  # this collapses to one class number, as expected.
  story_cols <- setdiff(names(table3_classes), "tmk")
  table3_with_story <- build_permit_story(table3_classes, story_cols) %>%
    mutate(upgrade_scenario = classify_upgrade_scenario(permit_story))

  table1_tmk_registry <- table1_tmk_registry %>%
    left_join(table3_with_story %>% select(tmk, permit_story, upgrade_scenario), by = "tmk")

  list(
    table1 = table1_tmk_registry,
    table1_commercial = table1_tmk_registry_commercial,
    table2a = table2a_bedrooms,
    table2b = table2b_bedrooms,
    table3 = table3_classes
  )
}

# ---------------------------------------------------------------------------
# Step 3: encode 2010 OTP baseline classes
# ---------------------------------------------------------------------------
# Two input shapes exist in the raw data:
#   "wide_quantity" (Maui, Kauai, Hawaii): columns class_i, class_ii, class_iii,
#     class_iv hold quantities per tmk.
#   "text_label" (Oahu): a single `osds_class` text column with values like
#     "Class I".."Class IV", "Multiple".
# Both return: tmk, ttl_osds_2010, encoded_sum (+ any pitt_code/commercial cols
# already present on `otp_df`).
encode_otp_classes <- function(otp_df, encoding_type = c("wide_quantity", "text_label")) {
  encoding_type <- match.arg(encoding_type)

  if (encoding_type == "wide_quantity") {
    otp_df %>%
      pivot_longer(cols = starts_with("class"), names_to = "class", values_to = "quantity") %>%
      filter(!is.na(quantity) & quantity != 0 & quantity != -9999) %>%
      rename(ttl_osds_2010 = any_of(c("osds_qty", "ttl_osds"))) %>%
      mutate(osds_class_otp = case_when(
        class %in% c("class_iv", "class4")  ~ 8, # cesspool
        class %in% c("class_iii", "class3") ~ 4,
        class %in% c("class_ii", "class2")  ~ 2,
        class %in% c("class_i", "class1")   ~ 1,
        TRUE ~ NA_real_
      )) %>%
      pivot_wider(
        id_cols = c("tmk", "ttl_osds_2010", any_of(c("pittcode", "pitt_code", "commercial"))),
        names_from = "class",
        values_from = "osds_class_otp",
        values_fn = max
      ) %>%
      mutate(encoded_sum = rowSums(across(starts_with("class")), na.rm = TRUE))
  } else {
    otp_df %>%
      rename(ttl_osds_2010 = any_of(c("ttl_osds", "osds_qty"))) %>%
      mutate(osds_class_otp = case_when(
        osds_class == "Class IV"  ~ 8, # cesspool
        osds_class == "Class III" ~ 4,
        osds_class == "Class II"  ~ 2,
        osds_class == "Class I"   ~ 1,
        osds_class == "Multiple"  ~ 9, # multiple, at least one is a cesspool
        TRUE ~ NA_real_
      )) %>%
      pivot_wider(
        id_cols = c("tmk", "ttl_osds_2010"),
        names_from = "osds_class",
        values_from = "osds_class_otp",
        values_fn = max
      ) %>%
      rename(Class_mult = any_of("Multiple")) %>%
      mutate(encoded_sum = rowSums(across(starts_with("Class")), na.rm = TRUE))
  }
}

# ---------------------------------------------------------------------------
# Step 4: assemble final Tables 1-4 + Table 3b for one island
# ---------------------------------------------------------------------------
# Reads the intermediate CSVs each island's Step 2/3 chunks already wrote
# (same file naming convention as the original 1a-1d scripts), reconciles
# new/unchanged/updated records, runs integrity checks, and writes final
# Table 1, 2, 3a, 3b, 4 to `out_dir` using the existing {island}_table*.csv
# naming so downstream scripts (2_SpatializeAll_TMKS.Rmd, etc.) are unaffected.
assemble_final_tables <- function(island, prefix, step2_dir, step3_dir, out_dir,
                                   master_tmk_list = NULL) {

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  p <- function(dir, name) file.path(dir, paste0(island, "_", name))

  # ---- Table 1 ----
  table_1_new <- read_csv(p(step2_dir, "table1_tmk_registry_all.csv"), col_types = cols(.default = "c")) %>%
    clean_tmk(prefix, "tmk") %>%
    rename(in_dwelldat = any_of(c("is_dwelling", "in_dwelldat"))) %>%
    mutate(status_since_otp = "New Permit", n_addresses = as.character(n_addresses)) %>%
    distinct(tmk, .keep_all = TRUE)

  table_1_unchanged <- read_csv(p(step3_dir, "table1_tmk_registry_UNCHANGED.csv"), col_types = cols(.default = "c")) %>%
    clean_tmk(prefix, "tmk") %>%
    mutate(scenario = "No new permits") %>%
    distinct(tmk, .keep_all = TRUE)

  bedFlags_forTable1 <- read_csv(p(step3_dir, "bedflags_fortable1.csv"), col_types = cols(.default = "c")) %>%
    clean_tmk(prefix, "tmk")

  classFlags_forTable1 <- read_csv(p(step3_dir, "classflags_fortable1.csv"), col_types = cols(.default = "c")) %>%
    clean_tmk(prefix, "tmk")

  # table_1_new/table_1_unchanged already carry a permit_story/upgrade_scenario
  # computed from permit history alone (Step 2b) or the single baseline value
  # (Step 3a unchanged). classFlags_forTable1 (from Step 3's "updated"
  # reconciliation) has a fuller version for the subset of tmks that have
  # BOTH a baseline AND new permits, since it factors in the pre-2009 value -
  # prefer that one where it exists.
  table_1_complete_flags <- bind_rows(table_1_new, table_1_unchanged) %>%
    distinct(tmk, .keep_all = TRUE) %>%
    left_join(bedFlags_forTable1, by = "tmk") %>%
    left_join(classFlags_forTable1, by = "tmk") %>%
    mutate(
      permit_story = coalesce(permit_story_updated, permit_story),
      upgrade_scenario = coalesce(upgrade_scenario_updated, upgrade_scenario)
    ) %>%
    select(-any_of(c("permit_story_updated", "upgrade_scenario_updated"))) %>%
    distinct(tmk, .keep_all = TRUE)

  # ---- Table 2 (bedrooms) ----
  table_2_unchanged <- read_csv(p(step3_dir, "table2_bedrooms_UNCHANGED.csv"), col_types = cols(.default = "c")) %>%
    clean_tmk(prefix, "tmk") %>%
    mutate(across(-tmk, as.numeric))

  table_2_updated_forbind <- read_csv(p(step3_dir, "table2_updated_gapfilled.csv"), col_types = cols(.default = "c")) %>%
    clean_tmk(prefix, "tmk") %>%
    rename_with(~ str_replace(.x, "^newbeds", "bedrooms_"), starts_with("newbeds")) %>%
    mutate(across(-tmk, as.numeric))

  table_2_new_raw <- read_csv(p(step2_dir, "table2a_bedrooms_sum.csv"), col_types = cols(.default = "c"))

  table_2_newonly <- table_2_new_raw %>%
    clean_tmk(prefix, "tmk") %>%
    filter(tmk %!in% table_2_updated_forbind$tmk) %>%
    rename_with(~ str_replace(.x, "^newbeds", "bedrooms_"), starts_with("newbeds")) %>%
    mutate(across(-tmk, as.numeric))

  table_2_complete <- bind_rows(table_2_unchanged, table_2_updated_forbind, table_2_newonly) %>%
    distinct(tmk, .keep_all = TRUE)

  # ---- Table 3a (permit classes) ----
  table_3_unchanged <- read_csv(p(step3_dir, "table3_classes_UNCHANGED.csv"), col_types = cols(.default = "c")) %>%
    clean_tmk(prefix, "tmk") %>%
    rename_with(~ str_replace(.x, "^osds_cl_", "osds_class_"), starts_with("osds_cl_"))

  table_3_updatedforbind <- read_csv(p(step3_dir, "table3_new_updated.csv"), col_types = cols(.default = "c")) %>%
    clean_tmk(prefix, "tmk") %>%
    rename(osds_class_pre2009 = any_of(c("newpre_2009", "osds_class_pre2009"))) %>%
    rename_with(~ str_replace(.x, "^newclass", "osds_class_"), starts_with("newclass"))

  table_3_new_raw <- read_csv(p(step2_dir, "table3_newclass_all.csv"), col_types = cols(.default = "c"))

  table_3_newonly <- table_3_new_raw %>%
    clean_tmk(prefix, "tmk") %>%
    filter(tmk %!in% table_3_updatedforbind$tmk) %>%
    rename_with(~ str_replace(.x, "^newclass", "osds_class_"), starts_with("newclass"))

  table_3_complete <- bind_rows(table_3_updatedforbind, table_3_unchanged, table_3_newonly) %>%
    distinct(tmk, .keep_all = TRUE)

  # ---- Table 4 (commercial / no-bedroom parcels) ----
  table_4_unchanged <- read_csv(p(step3_dir, "table4_commercial_UNCHANGED_draft.csv"), col_types = cols(.default = "c")) %>%
    clean_tmk(prefix, "tmk") %>%
    select(tmk, any_of(c("commercial", "pitt_code", "pittcode", "x", "y"))) %>%
    rename(pitt_code = any_of(c("pittcode", "pitt_code"))) %>%
    mutate(across(everything(), as.character))

  table4_withUpdates_path <- p(step3_dir, "table4_updated.csv")
  table_4_withUpdates_forbind <- if (file.exists(table4_withUpdates_path)) {
    read_csv(table4_withUpdates_path, col_types = cols(.default = "c")) %>%
      clean_tmk(prefix, "tmk") %>%
      filter(!is.na(commercial) | !is.na(pitt_code)) %>%
      select(tmk, any_of(c("commercial", "pitt_code", "x", "y"))) %>%
      mutate(across(everything(), as.character))
  } else {
    tibble(tmk = character(), commercial = character(), pitt_code = character(), x = character(), y = character())
  }

  table_4_new <- read_csv(p(step2_dir, "table1_tmk_registry_commercial.csv"), col_types = cols(.default = "c")) %>%
    clean_tmk(prefix, "tmk") %>%
    filter(tmk %!in% table_4_withUpdates_forbind$tmk) %>%
    mutate(commercial = "unknown", pitt_code = "-9999") %>%
    select(tmk, any_of(c("commercial", "pitt_code", "x", "y"))) %>%
    mutate(across(everything(), as.character))

  table_4_complete_cl <- bind_rows(table_4_unchanged, table_4_new, table_4_withUpdates_forbind) %>%
    distinct(tmk, .keep_all = TRUE) %>%
    mutate(
      pitt_code = if_else(is.na(pitt_code), "-9999", pitt_code),
      commercial = if_else(is.na(commercial) | commercial == "unknown", "check google places", commercial)
    )

  tmks_with_bedrooms <- table_2_complete %>%
    filter(if_any(starts_with("bedrooms_"), ~ !is.na(.x) & .x > 0)) %>%
    pull(tmk)

  table_4_complete_cl <- table_4_complete_cl %>%
    filter(tmk %!in% tmks_with_bedrooms)

  # ---- Final Table 1 ----
  table_1_final_final <- table_1_complete_flags %>%
    mutate(use_table_4 = tmk %in% table_4_complete_cl$tmk) %>%
    select(
      tmk, starts_with("n_"), min_year, max_year, in_dwelldat, status_since_otp,
      any_of("n_buildings"), any_of("bed_trend"), any_of("year_built_combined"),
      bed_gapfill_flag = any_of("gapfill_flag"), any_of("class_match_flag"),
      any_of("permit_story"), any_of("upgrade_scenario"), use_table_4
    ) %>%
    distinct(tmk, .keep_all = TRUE)

  # ---- Integrity checks ----
  cat("---", toupper(island), "PIPELINE INTEGRITY CHECKS ---\n")
  cat("1. All Table 1 TMKs present in Table 3a:", all(table_1_final_final$tmk %in% table_3_complete$tmk), "\n")
  cat("2. All Table 2 TMKs present in Table 1:", all(table_2_complete$tmk %in% table_1_final_final$tmk), "\n")
  if (!is.null(master_tmk_list)) {
    clean_master_tmks <- tibble(tmk = as.character(master_tmk_list)) %>%
      clean_tmk(prefix, "tmk") %>%
      filter(tmk != "000000000") %>%
      pull(tmk) %>%
      unique()
    missing_master <- setdiff(clean_master_tmks, table_1_final_final$tmk)
    cat("3. All Valid Master TMKs present in Table 1:", length(missing_master) == 0, "\n")
    if (length(missing_master) > 0) {
      cat("   -> Missing TMK count:", length(missing_master), "\n")
    }
  }

  # ---- Table 3b (bitmask "active OSDS" status) ----
  table_3a_long <- table_3_complete %>%
    rename(osds_class_2008 = any_of("osds_class_pre2009")) %>%
    pivot_longer(cols = -tmk, names_to = "Year", values_to = "Applied") %>%
    mutate(Applied = replace_na(as.numeric(Applied), 0))

  table_3b_wide <- table_3a_long %>%
    arrange(tmk, Year) %>%
    group_by(tmk) %>%
    mutate(active_status = calculate_active_permits(Applied)) %>%
    ungroup() %>%
    pivot_wider(id_cols = tmk, names_from = Year, values_from = active_status)

  # ---- Export ----
  write_csv(table_1_final_final, file.path(out_dir, paste0(island, "_table1_tmk_registry_ALL.csv")))
  write_csv(table_2_complete,     file.path(out_dir, paste0(island, "_table2_bedrooms_ALL.csv")))
  write_csv(table_3_complete,     file.path(out_dir, paste0(island, "_table3a_permitsApplied_ALL.csv")))
  write_csv(table_4_complete_cl,  file.path(out_dir, paste0(island, "_table4_commercial_draft.csv")))
  write_csv(table_3b_wide,        file.path(out_dir, paste0(island, "_table3b_activeOSDS.csv")))

  invisible(list(
    table1 = table_1_final_final,
    table2 = table_2_complete,
    table3a = table_3_complete,
    table3b = table_3b_wide,
    table4 = table_4_complete_cl
  ))
}
