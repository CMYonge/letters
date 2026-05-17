# generate_decade_includes_May26.R
# Generates _RANGE_letters.md include files (e.g. _1870-1879_letters.md)
# and appends the {{< include >}} directive to each decade QMD in the website.
#
# Run AFTER STEP3 (which copies decade QMDs to website/letters/decades/).
#
# Changes from Apr26 version:
#   - out_dir fixed: was website/decades/, now website/letters/decades/
#     to match where STEP3 puts the decade QMDs
#   - Link path fixed: was ../letters/N-slug.html
#     now ../N-slug.qmd (correct relative path from letters/decades/)
#   - Uses explicit date ranges (matching page slugs) rather than floor decades.
#     This correctly handles the 1834-1849 page which spans two floor-decades.
#   - Matches decade QMD by decade_label: field in YAML (written by STEP1)
#   - Appends ## Letters heading + {{< include >}} to the matching QMD.
#     Idempotent: skips if the include tag is already present.
#
# NB: 1870s.qmd in website/decades/ is now superseded. The canonical file is
# 3740-letters-1870-1879.qmd in website/letters/decades/. Delete 1870s.qmd.

library(dplyr)
library(readr)
library(stringr)

##### CONFIGURATION #####

website_dir <- "C:/db/cmy_letters_website_Apr"
data_dir    <- "C:/db"

# Where STEP3 puts the decade QMDs — include files must go here too
out_dir <- file.path(website_dir, "letters", "decades")

if (!dir.exists(out_dir)) stop("Decade folder not found: ", out_dir,
                                "\nHave you run STEP3 yet?")

# Date ranges matching the six decade intro page slugs.
# Each entry: label (used for include filename and decade_label YAML field),
#             start year, end year (inclusive).
date_ranges <- list(
  "1834-1849" = c(start = 1834L, end = 1849L),
  "1850-1859" = c(start = 1850L, end = 1859L),
  "1860-1869" = c(start = 1860L, end = 1869L),
  "1870-1879" = c(start = 1870L, end = 1879L),
  "1880-1889" = c(start = 1880L, end = 1889L),
  "1890-1901" = c(start = 1890L, end = 1901L)
)

##### LOAD DATA #####

letterinfo <- read_csv(file.path(data_dir, "wp_letterinfo.csv"),
                       col_types = cols(letter_dbdate = col_character()),
                       show_col_types = FALSE)
posts      <- read_csv(file.path(data_dir, "wp_posts.csv"),
                       show_col_types = FALSE)

##### DATE FORMATTING #####
# Show only as much precision as is known

format_date <- function(d) {
  dplyr::case_when(
    is.na(d)                    ~ "",
    substr(d, 6, 10) == "00-00" ~ substr(d, 1, 4),         # year only
    substr(d, 9, 10) == "00"    ~ substr(d, 1, 7),         # year-month only
    TRUE                        ~ d                         # full date
  )
}

##### BUILD LETTER TABLE #####

letters <- letterinfo |>
  inner_join(
    posts |>
      filter(post_type == "post") |>
      select(post_id, post_name, post_title),
    by = c("post_ID" = "post_id")
  ) |>
  mutate(
    dbdate_clean = if_else(
      is.na(letter_dbdate) | letter_dbdate == "0000-00-00" | letter_dbdate == "",
      NA_character_,
      letter_dbdate
    ),
    year         = as.integer(substr(dbdate_clean, 1, 4)),
    display_date = format_date(dbdate_clean),
    # Correct relative path: decade QMDs are in letters/decades/
    # Letters are in letters/ — one level up from decades/
    link         = sprintf("[%s](../%s-%s.qmd)", post_title, post_ID, post_name)
  ) |>
  arrange(dbdate_clean)

##### HELPER: WRITE ONE INCLUDE FILE #####

write_include <- function(df, filename, date_col = TRUE) {
  if (nrow(df) == 0) {
    message("  No letters found for ", filename, " — file not written")
    return(invisible(NULL))
  }

  if (date_col) {
    lines <- c(
      "| Date | Letter |",
      "|------|--------|",
      sprintf("| %s | %s |", df$display_date, df$link)
    )
  } else {
    lines <- c(
      "| Letter |",
      "|--------|",
      sprintf("| %s |", df$link)
    )
  }

  writeLines(lines, file.path(out_dir, filename))
  message("  Written: ", filename, " (", nrow(df), " letters)")
}

##### HELPER: READ decade_label FROM QMD YAML #####

read_decade_label <- function(qmd_file) {
  lines      <- readLines(qmd_file, warn = FALSE, encoding = "UTF-8")
  yaml_end   <- which(lines == "---")
  if (length(yaml_end) < 2) return(NA_character_)
  yaml_lines <- lines[seq_len(yaml_end[2])]
  label_line <- yaml_lines[str_detect(yaml_lines, "^decade_label:")]
  if (length(label_line) == 0) return(NA_character_)
  str_extract(label_line[1], '(?<=")[^"]+(?=")')
}

##### FIND DECADE QMDs IN WEBSITE #####

decade_qmds <- list.files(out_dir, pattern = "\\.qmd$", full.names = TRUE)

if (length(decade_qmds) == 0) {
  stop("No .qmd files found in ", out_dir, "\nHave you run STEP3?")
}

# Build lookup table: decade_label → filepath
qmd_label_map <- setNames(
  sapply(decade_qmds, read_decade_label),
  decade_qmds
)

message("Found ", length(decade_qmds), " QMD files in ", out_dir)
message("Labels found: ", paste(na.omit(qmd_label_map), collapse = ", "))

if (all(is.na(qmd_label_map))) {
  stop("No decade_label fields found in any QMD.\n",
       "Have you run the May26 version of STEP1 (which writes decade_label: to YAML)?")
}

##### PROCESS EACH DATE RANGE #####

message("\nProcessing date ranges...")

for (label in names(date_ranges)) {

  r                <- date_ranges[[label]]
  include_filename <- paste0("_", label, "_letters.md")
  include_tag      <- paste0("{{< include _", label, "_letters.md >}}")

  message("\n--- Range: ", label, " (", r["start"], "-", r["end"], ") ---")

  # Filter letters to this date range
  range_letters <- letters |>
    filter(!is.na(year) & year >= r["start"] & year <= r["end"])

  # Write the include file
  write_include(range_letters, include_filename)

  # Find the matching decade QMD
  matching_qmds <- names(qmd_label_map)[qmd_label_map == label & !is.na(qmd_label_map)]

  if (length(matching_qmds) == 0) {
    message("  No QMD found with decade_label: \"", label, "\" — skipping include append")
    message("  (Expected a file in ", out_dir, " with decade_label: \"", label, "\" in its YAML)")
    next
  }

  qmd_file   <- matching_qmds[1]
  file_lines <- readLines(qmd_file, warn = FALSE, encoding = "UTF-8")

  if (any(str_detect(file_lines, fixed(include_tag)))) {
    message("  Include already present in ", basename(qmd_file), " — skipping")
  } else {
    append_text <- paste0("\n\n## Letters\n\n", include_tag, "\n")
    cat(append_text, file = qmd_file, append = TRUE)
    message("  Appended letter list (", nrow(range_letters), " letters) to ",
            basename(qmd_file))
  }
}

##### UNDATED LETTERS #####

message("\n--- Undated letters ---")
undated <- filter(letters, is.na(year))
if (nrow(undated) > 0) {
  write_include(undated, "_undated_letters.md", date_col = FALSE)
} else {
  message("  No undated letters found")
}

##### SUMMARY #####

message("\nDone.")
message("Include files and updated QMDs in: ", out_dir)
message("\nReminder: delete the now-superseded file:")
message("  ", file.path(website_dir, "decades", "1870s.qmd"))
message("\nNext: quarto render in ", website_dir)
