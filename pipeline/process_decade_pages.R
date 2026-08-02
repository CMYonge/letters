# process_decade_pages.R
# Combined script: run immediately after STEP3, before quarto render.

# Part A: generate_decade_includes
#   Generates _RANGE_letters.md include files and appends {{< include >}}
#   directives to each decade QMD in the website.

# Part B: fix_decade_pages
#   Fixes _quarto.yml navbar, letters-index.qmd intro links,
#   footnote spacing in decade QMDs, and [[tag]] link paths.

# Run order: STEP3 → process_decade_pages.R → quarto render

library(dplyr)
library(readr)
library(stringr)

##### CONFIGURATION #####

website_dir <- "C:/db/letters"
data_dir    <- "C:/db"
decades_dir <- file.path(website_dir, "decades")

cat("Processing decade pages...\n\n")

# ── PART A: GENERATE DECADE INCLUDES ─────────────────────────────────────────

cat("##### PART A: Generating decade include files #####\n\n")

if (!dir.exists(decades_dir)) stop("Decade folder not found: ", decades_dir,
                                   "\nHave you run STEP3 yet?")

# Date ranges matching the six decade intro page slugs.
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
    # Letters are at root; decades/ is one level deep — ../N-slug.qmd is correct
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
  
  writeLines(lines, file.path(decades_dir, filename))
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

decade_qmds <- list.files(decades_dir, pattern = "\\.qmd$", full.names = TRUE)

if (length(decade_qmds) == 0) {
  stop("No .qmd files found in ", decades_dir, "\nHave you run STEP3?")
}

# Build lookup table: decade_label → filepath
qmd_label_map <- setNames(
  sapply(decade_qmds, read_decade_label),
  decade_qmds
)

message("Found ", length(decade_qmds), " QMD files in ", decades_dir)
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
  
  range_letters <- letters |>
    filter(!is.na(year) & year >= r["start"] & year <= r["end"])
  
  write_include(range_letters, include_filename)
  
  matching_qmds <- names(qmd_label_map)[qmd_label_map == label & !is.na(qmd_label_map)]
  
  if (length(matching_qmds) == 0) {
    message("  No QMD found with decade_label: \"", label, "\" — skipping include append")
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

cat("\n\u2713 Part A complete: include files generated\n\n")

# ── PART B: FIX DECADE PAGES ──────────────────────────────────────────────────

cat("##### PART B: Fixing decade pages and config #####\n\n")

# ── B1. Fix _quarto.yml ───────────────────────────────────────────────────────

cat("Fixing _quarto.yml...\n")

quarto_config <- '
project:
  type: website
  output-dir: docs

website:
  title: "Charlotte Mary Yonge Letters"
  navbar:
    left:
      - text: "Home"
        href: index.qmd
      - text: "Letters"
        href: letters-index.qmd
      - text: "Decades"
        menu:
          - text: "1834\u20131849"
            href: decades/3733-letters-1834-1849.qmd
          - text: "1850\u20131859"
            href: decades/3735-letters-1850-1859.qmd
          - text: "1860\u20131869"
            href: decades/3737-letters-1860-1869.qmd
          - text: "1870\u20131879"
            href: decades/3740-letters-1870-1879.qmd
          - text: "1880\u20131889"
            href: decades/3750-letters-1880-1889.qmd
          - text: "1890\u20131901"
            href: decades/3752-letters-1890-1901.qmd
      - text: "People"
        href: people/index.qmd
      - text: "Organizations"
        href: organizations/index.qmd
      - text: "CMY Bibliography"
        href: cmy_books/index.qmd
      - text: "General Bibliography"
        href: other_books/index.qmd
  search: true

format:
  html:
    theme: cosmo
    css: styles.css
    toc: false
'

writeLines(quarto_config, file.path(website_dir, "_quarto.yml"))
cat("  \u2713 _quarto.yml updated\n\n")

# ── B2. Fix letters-index.qmd: add intro links ────────────────────────────────

cat("Fixing letters-index.qmd...\n")

decade_intro_map <- list(
  "1830s" = list(file = "decades/3733-letters-1834-1849.qmd", label = "1834\u20131849"),
  "1840s" = list(file = "decades/3733-letters-1834-1849.qmd", label = "1834\u20131849"),
  "1850s" = list(file = "decades/3735-letters-1850-1859.qmd", label = "1850\u20131859"),
  "1860s" = list(file = "decades/3737-letters-1860-1869.qmd", label = "1860\u20131869"),
  "1870s" = list(file = "decades/3740-letters-1870-1879.qmd", label = "1870\u20131879"),
  "1880s" = list(file = "decades/3750-letters-1880-1889.qmd", label = "1880\u20131889"),
  "1890s" = list(file = "decades/3752-letters-1890-1901.qmd", label = "1890\u20131901")
)

index_path  <- file.path(website_dir, "letters-index.qmd")
index_lines <- readLines(index_path, warn = FALSE, encoding = "UTF-8")
new_lines   <- c()
intro_link_added <- 0

for (i in seq_along(index_lines)) {
  new_lines <- c(new_lines, index_lines[i])
  
  m <- str_match(index_lines[i], "^## (\\d{4}s)$")
  if (!is.na(m[1, 1])) {
    decade_key <- m[1, 2]
    intro      <- decade_intro_map[[decade_key]]
    if (!is.null(intro)) {
      link_text       <- paste0("*[Introduction to letters ", intro$label, "](", intro$file, ")*")
      already_present <- any(str_detect(index_lines, fixed(intro$file)))
      if (!already_present) {
        new_lines        <- c(new_lines, "", link_text, "")
        intro_link_added <- intro_link_added + 1
      }
    }
  }
}

writeLines(new_lines, index_path, useBytes = TRUE)
cat("  \u2713 Added", intro_link_added, "intro links to letters-index.qmd\n\n")

# ── B3. Fix footnote spacing and tag links in each decade QMD ─────────────────

cat("Fixing footnote spacing and link paths in decade QMDs...\n")

cmy_bib <- read_csv("C:/db/wp_cmybibliography.csv",     show_col_types = FALSE)
gen_bib <- read_csv("C:/db/wp_generalbibliography.csv", show_col_types = FALSE)

get_cmy_title <- function(id) {
  row <- cmy_bib[cmy_bib$cmy_bookID == id, ]
  if (nrow(row) == 0) return(paste("CMY Book", id))
  trimws(str_replace_all(row$display_title[1], "<[^>]+>", ""))
}
get_gen_title <- function(id) {
  row <- gen_bib[gen_bib$general_bookID == id, ]
  if (nrow(row) == 0) return(paste("Book", id))
  trimws(str_replace_all(row$title[1], "<[^>]+>", ""))
}

fix_decade_qmd <- function(filepath) {
  
  lines <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  text  <- paste(lines, collapse = "\n")
  
  # ── Fix [[cmybook:N]display] and [[otherbook:N]display] link paths ──────────
  # Decades are at root/decades/ — one level deep, so ../cmy_books/ is correct
  cmy_display <- str_match_all(text, "\\[\\[cmybook:(\\d+)\\]([^\\]]+)\\]")[[1]]
  for (i in seq_len(nrow(cmy_display))) {
    tag          <- cmy_display[i, 1]
    id           <- as.numeric(cmy_display[i, 2])
    display_text <- trimws(cmy_display[i, 3])
    link         <- paste0("[", display_text, "](../cmy_books/cmybook_", id, ".qmd)")
    text         <- str_replace(text, fixed(tag), link)
  }
  cmy_bare <- str_match_all(text, "\\[\\[cmybook:(\\d+)\\]\\]")[[1]]
  for (i in seq_len(nrow(cmy_bare))) {
    tag   <- cmy_bare[i, 1]
    id    <- as.numeric(cmy_bare[i, 2])
    link  <- paste0("[", get_cmy_title(id), "](../cmy_books/cmybook_", id, ".qmd)")
    text  <- str_replace(text, fixed(tag), link)
  }
  
  book_display <- str_match_all(text, "\\[\\[otherbook:(\\d+)\\]([^\\]]+)\\]")[[1]]
  for (i in seq_len(nrow(book_display))) {
    tag          <- book_display[i, 1]
    id           <- as.numeric(book_display[i, 2])
    display_text <- trimws(book_display[i, 3])
    link         <- paste0("[", display_text, "](../other_books/otherbook_", id, ".qmd)")
    text         <- str_replace(text, fixed(tag), link)
  }
  book_bare <- str_match_all(text, "\\[\\[otherbook:(\\d+)\\]\\]")[[1]]
  for (i in seq_len(nrow(book_bare))) {
    tag   <- book_bare[i, 1]
    id    <- as.numeric(book_bare[i, 2])
    link  <- paste0("[", get_gen_title(id), "](../other_books/otherbook_", id, ".qmd)")
    text  <- str_replace(text, fixed(tag), link)
  }
  
  # ── Ensure blank line before each [N] footnote line ─────────────────────────
  lines     <- strsplit(text, "\n", fixed = TRUE)[[1]]
  new_lines <- c()
  n_fixed   <- 0L
  
  for (i in seq_along(lines)) {
    is_footnote <- str_detect(lines[i], "^\\[\\d+\\]\\s")
    prev_blank  <- i == 1 || lines[i - 1] == ""
    
    if (is_footnote && !prev_blank) {
      new_lines <- c(new_lines, "")
      n_fixed   <- n_fixed + 1L
    }
    new_lines <- c(new_lines, lines[i])
  }
  
  writeLines(new_lines, filepath, useBytes = TRUE)
  message("  \u2713 ", basename(filepath), " (", n_fixed, " spacing fixes; ",
          nrow(cmy_display) + nrow(cmy_bare) + nrow(book_display) + nrow(book_bare),
          " tags resolved)")
}

decade_qmd_files <- list.files(decades_dir, pattern = "\\.qmd$", full.names = TRUE)

if (length(decade_qmd_files) == 0) {
  cat("  No QMD files found in", decades_dir, "\n")
} else {
  for (f in decade_qmd_files) fix_decade_qmd(f)
}

cat("\n\u2705 All done.\n")
cat("Next: quarto render in", website_dir, "\n")