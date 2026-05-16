# fix_decade_pages_May26.R
# Patches the already-built website without rerunning STEP1/STEP3.
# Run this, then quarto render.
#
# Fixes:
#   1. _quarto.yml: toc: false globally + Decades navbar dropdown
#   2. letters/index.qmd: adds intro page links at top of each decade section
#   3. Each decade QMD: adds blank lines between ## Notes items

library(stringr)
library(readr)

website_dir  <- "C:/db/cmy_letters_website_Apr"
decades_dir  <- file.path(website_dir, "letters", "decades")

# ── 1. Fix _quarto.yml ────────────────────────────────────────────────────────

cat("Fixing _quarto.yml...\n")

quarto_config <- '
project:
  type: website
  output-dir: _site

website:
  title: "Charlotte Mary Yonge Letters"
  navbar:
    left:
      - text: "Home"
        href: index.qmd
      - text: "Letters"
        href: letters/index.qmd
      - text: "Decades"
        menu:
          - text: "1834\u20131849"
            href: letters/decades/3733-letters-1834-1849.qmd
          - text: "1850\u20131859"
            href: letters/decades/3735-letters-1850-1859.qmd
          - text: "1860\u20131869"
            href: letters/decades/3737-letters-1860-1869.qmd
          - text: "1870\u20131879"
            href: letters/decades/3740-letters-1870-1879.qmd
          - text: "1880\u20131889"
            href: letters/decades/3750-letters-1880-1889.qmd
          - text: "1890\u20131901"
            href: letters/decades/3752-letters-1890-1901.qmd
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
    title-block-style: plain
'

writeLines(quarto_config, file.path(website_dir, "_quarto.yml"))
cat("  \u2713 _quarto.yml updated\n\n")

# ── 2. Fix letters/index.qmd: add intro links ─────────────────────────────────

cat("Fixing letters/index.qmd...\n")

decade_intro_map <- list(
  "1830s" = list(file = "decades/3733-letters-1834-1849.qmd", label = "1834\u20131849"),
  "1840s" = list(file = "decades/3733-letters-1834-1849.qmd", label = "1834\u20131849"),
  "1850s" = list(file = "decades/3735-letters-1850-1859.qmd", label = "1850\u20131859"),
  "1860s" = list(file = "decades/3737-letters-1860-1869.qmd", label = "1860\u20131869"),
  "1870s" = list(file = "decades/3740-letters-1870-1879.qmd", label = "1870\u20131879"),
  "1880s" = list(file = "decades/3750-letters-1880-1889.qmd", label = "1880\u20131889"),
  "1890s" = list(file = "decades/3752-letters-1890-1901.qmd", label = "1890\u20131901")
)

index_path  <- file.path(website_dir, "letters", "index.qmd")
index_lines <- readLines(index_path, warn = FALSE, encoding = "UTF-8")
new_lines   <- c()
intro_link_added <- 0

for (i in seq_along(index_lines)) {
  new_lines <- c(new_lines, index_lines[i])

  # Check if this line is a decade heading e.g. "## 1870s"
  m <- str_match(index_lines[i], "^## (\\d{4}s)$")
  if (!is.na(m[1, 1])) {
    decade_key <- m[1, 2]
    intro      <- decade_intro_map[[decade_key]]
    if (!is.null(intro)) {
      # Only add if not already present
      link_text <- paste0("*[Introduction to letters ", intro$label, "](", intro$file, ")*")
      already_present <- any(str_detect(index_lines, fixed(intro$file)))
      if (!already_present) {
        new_lines        <- c(new_lines, "", link_text, "")
        intro_link_added <- intro_link_added + 1
      }
    }
  }
}

writeLines(new_lines, index_path, useBytes = TRUE)
cat("  \u2713 Added", intro_link_added, "intro links to letters/index.qmd\n\n")

# ── 3. Fix footnote spacing and tag links in each decade QMD ─────────────────
# Footnotes in decade pages are plain body text with [1], [2], [3] prefixes
# (stored in WP post_content, not in letter_footnote field).
# Two fixes per file:
#   a) Ensure blank line before each [N] footnote line
#   b) Fix [[cmybook:N]...] and [[otherbook:N]...] link paths —
#      decade pages are in letters/decades/ so need ../../ not ../

cat("Fixing footnote spacing and link paths in decade QMDs...\n")

# Load bibliography CSVs for tag resolution
cmy_bib <- read_csv("C:/db/wp_cmybibliography.csv",     show_col_types = FALSE)
gen_bib <- read_csv("C:/db/wp_generalbibliography.csv", show_col_types = FALSE)

get_cmy_title <- function(id) {
  row <- cmy_bib[cmy_bib$cmy_bookID == id, ]
  if (nrow(row) == 0) return(paste("CMY Book", id))
  trimws(str_replace_all(row$title[1], "<[^>]+>", ""))
}
get_gen_title <- function(id) {
  row <- gen_bib[gen_bib$general_bookID == id, ]
  if (nrow(row) == 0) return(paste("Book", id))
  trimws(str_replace_all(row$title[1], "<[^>]+>", ""))
}

fix_decade_qmd <- function(filepath) {

  lines <- readLines(filepath, warn = FALSE, encoding = "UTF-8")
  text  <- paste(lines, collapse = "\n")

  # ── a) Fix [[cmybook:N]display] and [[otherbook:N]display] link paths ──────
  # From letters/decades/, cmy_books is at ../../cmy_books/
  cmy_display <- str_match_all(text, "\\[\\[cmybook:(\\d+)\\]([^\\]]+)\\]")[[1]]
  for (i in seq_len(nrow(cmy_display))) {
    tag          <- cmy_display[i, 1]
    id           <- as.numeric(cmy_display[i, 2])
    display_text <- trimws(cmy_display[i, 3])
    link         <- paste0("[", display_text, "](../../cmy_books/cmybook_", id, ".qmd)")
    text         <- str_replace(text, fixed(tag), link)
  }
  cmy_bare <- str_match_all(text, "\\[\\[cmybook:(\\d+)\\]\\]")[[1]]
  for (i in seq_len(nrow(cmy_bare))) {
    tag   <- cmy_bare[i, 1]
    id    <- as.numeric(cmy_bare[i, 2])
    link  <- paste0("[", get_cmy_title(id), "](../../cmy_books/cmybook_", id, ".qmd)")
    text  <- str_replace(text, fixed(tag), link)
  }

  book_display <- str_match_all(text, "\\[\\[otherbook:(\\d+)\\]([^\\]]+)\\]")[[1]]
  for (i in seq_len(nrow(book_display))) {
    tag          <- book_display[i, 1]
    id           <- as.numeric(book_display[i, 2])
    display_text <- trimws(book_display[i, 3])
    link         <- paste0("[", display_text, "](../../other_books/otherbook_", id, ".qmd)")
    text         <- str_replace(text, fixed(tag), link)
  }
  book_bare <- str_match_all(text, "\\[\\[otherbook:(\\d+)\\]\\]")[[1]]
  for (i in seq_len(nrow(book_bare))) {
    tag   <- book_bare[i, 1]
    id    <- as.numeric(book_bare[i, 2])
    link  <- paste0("[", get_gen_title(id), "](../../other_books/otherbook_", id, ".qmd)")
    text  <- str_replace(text, fixed(tag), link)
  }

  # ── b) Ensure blank line before each [N] footnote line ────────────────────
  # Matches lines starting with [1], [2] etc. that are footnote entries
  lines     <- strsplit(text, "\n", fixed = TRUE)[[1]]
  new_lines <- c()
  n_fixed   <- 0L

  for (i in seq_along(lines)) {
    is_footnote <- str_detect(lines[i], "^\\[\\d+\\]\\s")
    prev_blank  <- i == 1 || lines[i - 1] == ""

    if (is_footnote && !prev_blank) {
      new_lines <- c(new_lines, "")   # insert blank line before footnote
      n_fixed   <- n_fixed + 1L
    }
    new_lines <- c(new_lines, lines[i])
  }

  writeLines(new_lines, filepath, useBytes = TRUE)
  message("  \u2713 ", basename(filepath), " (", n_fixed, " spacing fixes; ",
          nrow(cmy_display) + nrow(cmy_bare) + nrow(book_display) + nrow(book_bare),
          " tags resolved)")
}

decade_qmds <- list.files(decades_dir, pattern = "\\.qmd$", full.names = TRUE)

if (length(decade_qmds) == 0) {
  cat("  No QMD files found in", decades_dir, "\n")
} else {
  for (f in decade_qmds) fix_decade_qmd(f)
}

cat("\n\u2705 All fixes applied.\n")
cat("Next: quarto render in", website_dir, "\n")
