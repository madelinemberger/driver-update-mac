## Clean Hawaii Dwell Data - from chat gpt


library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

# --- 0) Read TAB file (don’t expand), keep text, then filter to 2020+ ---
df <- read_delim("raw.txt", delim = "\t",
                 col_names = FALSE, col_types = cols(.default = col_character()),
                 na = c("", "NA"))

# Remove the blank after col1 and insert the blank 'D' column after col3 (aligns structure)
if (ncol(df) >= 2 && all(is.na(df[[2]]) | df[[2]] == "")) df <- df[, -2, drop = FALSE]
df <- bind_cols(df[,1:3], tibble(XD=NA_character_), df[,4:ncol(df)])  # XD = missing “D” col
names(df)[1:6] <- c("TMK","_skip","TAXYR","XD","CARD","USER20")  # optional helpful names

df20 <- df %>% filter(suppressWarnings(as.integer(TAXYR)) >= 2020)

# --- 1) Split the “crammed” columns only when they actually contain two values ---
split_two <- function(x){
  tibble(
    a = str_trim(str_replace(x, "^\\s*(\\S+?)\\s{2,}.*$", "\\1")),
    b = str_trim(str_replace(x, "^.*?\\s{2,}(\\S+)\\s*$", "\\1")),
    matched = str_detect(x %||% "", "^\\s*\\S+\\s{2,}\\S+\\s*$")
  )
}
`%||%` <- function(a,b) ifelse(is.na(a), b, a)
num <- function(x) suppressWarnings(as.integer(str_extract(x, "\\d+")))

# these are the ones that commonly carry two logical values in one tab cell
s12 <- split_two(df20$X12); s22 <- split_two(df20$X22); s23 <- split_two(df20$X23)

df20 <- df20 %>%
  mutate(
    X12a = ifelse(s12$matched, s12$a, NA_character_),
    X12b = ifelse(s12$matched, s12$b, NA_character_),
    X22a = ifelse(s22$matched, s22$a, NA_character_),
    X22b = ifelse(s22$matched, s22$b, NA_character_),
    X23a = ifelse(s23$matched, s23$a, NA_character_),
    X23b = ifelse(s23$matched, s23$b, NA_character_)
  )

# --- 2) Pick candidates for rooms/bedrooms/year built ---
rooms  <- num(df20$X21)                         # in the cleaner file, X21 was rooms
beds_a <- num(df20$X22a); beds_b <- num(df20$X22b)
beds   <- coalesce(beds_a, beds_b, num(df20$X22))  # pick split half if available, else original

# Year built tends to be in the split of X23 for this file
yb <- coalesce(
  ifelse(str_detect(df20$X23a %||% "", "^\\d{4}$"), df20$X23a, NA_character_),
  ifelse(str_detect(df20$X23b %||% "", "^\\d{4}$"), df20$X23b, NA_character_),
  ifelse(str_detect(df20$X23  %||% "", "^\\d{4}$"), df20$X23,  NA_character_)
)

# Full/half baths are earlier; with this file they’re often sitting around X19–X20 region.
# Heuristic: pick two small integers near rooms that maximize (beds <= rooms) and keep baths plausible.
full_bath <- num(df20$X20)              # try X20 first
half_bath <- num(df20$X19)              # try X19 as half
# If these look absurd (e.g., > 10), swap or set NA:
full_bath <- ifelse(!is.na(full_bath) & full_bath <= 10, full_bath, NA_integer_)
half_bath <- ifelse(!is.na(half_bath) & half_bath <= 10, half_bath, NA_integer_)

# --- 3) Final cut + quick QC flags ---
out <- tibble(
  tax_map_key = df20$TMK,          # already the combined ID in this file
  tax_year    = num(df20$TAXYR),
  total_rooms = rooms,
  bedrooms    = beds,
  full_baths  = full_bath,
  half_baths  = half_bath,
  year_built  = num(yb)
) %>%
  mutate(
    qc_bed_le_rooms = ifelse(!is.na(bedrooms) & !is.na(total_rooms), bedrooms <= total_rooms, NA),
    qc_year_reasonable = between(year_built, 1850, as.integer(format(Sys.Date(), "%Y")) + 1)
  )

# peek
out %>% slice(1:10)
summary(select(out, total_rooms, bedrooms, full_baths, half_baths, year_built))
mean(out$qc_bed_le_rooms, na.rm = TRUE)
