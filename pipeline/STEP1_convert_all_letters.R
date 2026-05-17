# STEP1_convert_all_letters_May26.R
# Converts HTML letters with [[person:123]text] tags to clean [[person:123]] format
#
# Changes from Apr26:
#   - preserve_italics(): <i>/<em> → *...* before HTML stripping in footnotes
#   - content_page_ids: only 6 specific post IDs routed to decades/
#     All other WordPress pages (home, blog, register etc.) are skipped
#   - decade_label extracted as date range "1870-1879" from slug, not floor decade
#   - Decade pages: footnotes as inline ## Notes + <sup>N</sup> markers
#     so they appear BEFORE the letter list include in rendered output
#   - decade_label written to YAML so generate_decades.R can find the QMD

library(dplyr)
library(readr)
library(stringr)

##### CONFIGURATION #####

html_dir   <- "C:/db/all_letters_html_Apr26"
output_dir <- "C:/db/all_letters_qmd_Apr26"

letterinfo <- read_csv("C:/db/wp_letterinfo.csv",
                       show_col_types = FALSE,
                       col_types = cols(letter_date = col_character()))
posts      <- read_csv("C:/db/wp_posts.csv", show_col_types = FALSE)

# Post IDs of the six decade intro pages.
# ALL other WordPress pages (home, blog, bibliography, register, etc.) are skipped.
content_page_ids <- c(3733, 3735, 3737, 3740, 3750, 3752)

dir.create(output_dir,                       showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "decades"), showWarnings = FALSE, recursive = TRUE)

html_files <- list.files(html_dir, pattern = "\\.html$", full.names = TRUE)

cat("Found", length(html_files), "HTML files to process\n\n")

##### FUNCTION: PARSE DATE TO ISO FORMAT #####

parse_iso_date <- function(date_str) {
  if (is.na(date_str) || date_str == "") return("")

  year <- NA
  m <- str_match(date_str, "(\\d{4})")
  if (!is.na(m[1,2])) year <- m[1,2]

  if (is.na(year)) {
    m <- str_match(date_str, "\\[(\\d{4})\\]")
    if (!is.na(m[1,2])) year <- m[1,2]
  }

  if (is.na(year)) return("")

  months <- c(
    jan = "01", january = "01",
    feb = "02", febry = "02", february = "02",
    mar = "03", march = "03",
    apr = "04", april = "04",
    may = "05",
    jun = "06", june = "06",
    jul = "07", july = "07",
    aug = "08", august = "08",
    sep = "09", september = "09",
    oct = "10", october = "10",
    nov = "11", november = "11",
    dec = "12", december = "12"
  )

  month <- "01"
  date_lower <- tolower(date_str)
  for (mon_name in names(months)) {
    if (str_detect(date_lower, mon_name)) {
      month <- months[mon_name]
      break
    }
  }

  day <- "01"
  m <- str_match(date_str, "\\b(\\d{1,2})(?:st|nd|rd|th)?\\b")
  if (!is.na(m[1,2])) {
    d <- as.integer(m[1,2])
    if (d >= 1 && d <= 31) day <- sprintf("%02d", d)
  }

  return(paste0(year, "-", month, "-", day))
}

##### FUNCTION: PRESERVE ITALIC TAGS AS MARKDOWN #####
# Called BEFORE stripping remaining HTML, so <i>/<em> become *...*
# rather than being silently deleted. Used only in footnote processing;
# the letter body handles <em>/<i> separately in the HTML-to-markdown block.

preserve_italics <- function(x) {
  if (is.na(x)) return(x)
  x <- str_replace_all(x, "<i>",       "*")
  x <- str_replace_all(x, "</i>",      "*")
  x <- str_replace_all(x, "<em[^>]*>", "*")
  x <- str_replace_all(x, "</em>",     "*")
  return(x)
}

##### FUNCTION: CONVERT ONE LETTER #####

convert_letter <- function(html_file) {

  body_html <- readLines(html_file, warn = FALSE, encoding = "UTF-8")
  body_html <- paste(body_html, collapse = "\n")

  filename <- basename(html_file)
  post_id  <- as.numeric(str_extract(filename, "^\\d+"))
  is_page  <- str_detect(filename, "page-")

  if (is.na(post_id)) {
    cat("Could not extract post_id from", filename, "\n")
    return(NULL)
  }

  # Skip WordPress utility pages — only process the six decade intro pages
  if (is_page && !(post_id %in% content_page_ids)) {
    cat("  Skipping WP utility page:", filename, "\n")
    return(NULL)
  }

  body_html <- gsub("<body>|</body>", "", body_html)

  ##### FIX FOOTNOTE MARKERS IN BODY #####

  fn_counter <- 0L
  while (str_detect(body_html, "\\[\\[footnote:\\d+\\]\\]")) {
    fn_counter <- fn_counter + 1L
    body_html  <- sub("\\[\\[footnote:\\d+\\]\\]", paste0("[^", fn_counter, "]"),
                      body_html, perl = TRUE)
  }

  ##### CONVERT HTML TO MARKDOWN #####

  body_html <- gsub("<p>",        "\n\n", body_html)
  body_html <- gsub("</p>",       "",     body_html)
  body_html <- gsub("<br\\s*/?>", "  \n", body_html, perl = TRUE)
  body_html <- gsub("<em>",       "*",    body_html)
  body_html <- gsub("</em>",      "*",    body_html)
  body_html <- gsub("<i>",        "*",    body_html)
  body_html <- gsub("</i>",       "*",    body_html)
  body_html <- gsub("<strong>",   "**",   body_html)
  body_html <- gsub("</strong>",  "**",   body_html)
  body_html <- gsub("<b>",        "**",   body_html)
  body_html <- gsub("</b>",       "**",   body_html)

  body_html <- gsub("<u>",  "UNDERLINE_OPEN",  body_html)
  body_html <- gsub("</u>", "UNDERLINE_CLOSE", body_html)
  body_html <- gsub("<[^>]+>", "", body_html)
  body_html <- gsub("UNDERLINE_OPEN",  "<u>",  body_html)
  body_html <- gsub("UNDERLINE_CLOSE", "</u>", body_html)

  body_html <- gsub("\n\n\n+", "\n\n", body_html)
  body_html <- trimws(body_html)
  body_html <- gsub("^[\\s\\n]*'[\\s\\n]*", "", body_html, perl = TRUE)

  ##### GET METADATA #####

  meta     <- letterinfo %>% filter(post_ID == post_id)
  slug_row <- posts      %>% filter(post_id == !!post_id)

  if (nrow(slug_row) > 0 && !is.na(slug_row$post_title[1]) && slug_row$post_title[1] != "") {
    title <- slug_row$post_title[1]
    slug  <- slug_row$post_name[1]
  } else {
    title <- paste("Letter", post_id)
    slug  <- paste0("letter-", post_id)
  }

  if (nrow(meta) == 0) {
    date          <- ""
    location      <- ""
    manuscript    <- ""
    footnote_text <- ""
  } else {
    date <- ifelse(is.na(meta$letter_date[1]) || meta$letter_date[1] == "NA",
                   "", meta$letter_date[1])
    location <- ifelse(is.na(meta$letter_fromAddress[1]) || meta$letter_fromAddress[1] == "NA",
                       "", meta$letter_fromAddress[1])
    manuscript <- ifelse(is.na(meta$manuscript_location[1]) || meta$manuscript_location[1] == "NA",
                         "", meta$manuscript_location[1])
    footnote_text <- ifelse(is.na(meta$letter_footnote[1]) || meta$letter_footnote[1] == "",
                            "", meta$letter_footnote[1])
  }

  date       <- str_replace_all(date,       "\\[\\[footnote:\\d+\\]\\]", "")
  date       <- trimws(date)
  location   <- str_replace_all(location,   "\\[\\[footnote:\\d+\\]\\]", "")
  location   <- trimws(location)
  manuscript <- str_replace_all(manuscript, "\\[\\[footnote:\\d+\\]\\]", "")
  manuscript <- trimws(manuscript)
  iso_date   <- parse_iso_date(date)

  ##### DECADE LABEL (decade intro pages only) #####
  # Extract date range e.g. "1870-1879" from slug "letters-1870-1879".
  # Stored in YAML as decade_label so generate_decades.R can match
  # this QMD to its include file without guessing from filename.

  decade_label <- ""
  if (is_page) {
    m <- str_match(slug, "(\\d{4}-\\d{4})")
    if (!is.na(m[1, 1])) decade_label <- m[1, 1]
  }

  ##### BUILD FOOTNOTE SECTION #####

  footnote_section <- ""
  has_markers      <- str_detect(body_html, "\\[\\^\\d+\\]")

  if (footnote_text != "") {

    parts <- str_split(footnote_text, "(?=\\[\\[footnote:\\d+\\]\\])")[[1]]
    parts <- parts[parts != ""]

    footnotes <- list()
    seq_num   <- 0L
    for (part in parts) {
      num_match <- str_match(part, "^\\[\\[footnote:\\d+\\]\\]([\\s\\S]+)$")
      if (!is.na(num_match[1, 1])) {
        seq_num <- seq_num + 1L
        text    <- trimws(num_match[1, 2])
        text <- str_replace_all(text, "\\[\\[(cmybook|otherbook):\\d+\\]([^\\]]+)\\]", "\\2")
        text <- preserve_italics(text)          # NEW: preserve <i>/<em> before strip
        text <- str_replace_all(text, "<[^>]+>", "")
        text <- gsub("\\'", "'", text, fixed = TRUE)
        text <- trimws(text)
        footnotes[[as.character(seq_num)]] <- text
      }
    }

    if (length(footnotes) > 0) {

      if (is_page) {
        # Decade intro pages: inline Notes section.
        # Quarto/Pandoc moves [^N]: definitions to end of rendered page,
        # after {{< include >}} content. Using <sup>N</sup> + ## Notes
        # keeps them in source order, so notes appear before the letter list.
        for (num in names(footnotes)) {
          body_html <- str_replace_all(body_html,
                                       fixed(paste0("[^", num, "]")),
                                       paste0("<sup>", num, "</sup>"))
        }
        note_lines <- c("", "", "## Notes", "")
        for (num in sort(as.integer(names(footnotes)))) {
          note_lines <- c(note_lines,
                          paste0(num, ". ", footnotes[[as.character(num)]]),
                          "")   # blank line between items for reliable list rendering
        }
        footnote_section <- paste(note_lines, collapse = "\n")

      } else if (has_markers) {
        # Regular letter with markers: Pandoc footnote definitions
        footnote_lines <- c("", "", "---", "")
        for (num in sort(names(footnotes))) {
          footnote_lines <- c(footnote_lines, paste0("[^", num, "]: ", footnotes[[num]]))
        }
        footnote_section <- paste(footnote_lines, collapse = "\n")

      } else {
        # Regular letter without markers: visible Note block
        note_text <- paste(sapply(sort(names(footnotes)), function(n) footnotes[[n]]),
                           collapse = " ")
        footnote_section <- paste0("\n\n---\n\n**Note:** ", note_text)
      }
    }
  }

  # Convert any unmatched [^N] markers to plain superscripts.
  # Not needed for is_page — all [^N] already converted to <sup>N</sup> above.
  if (!is_page && str_detect(body_html, "\\[\\^\\d+\\]")) {
    defined_nums <- str_match_all(footnote_section, "\\[\\^(\\d+)\\]:")[[1]]
    defined_nums <- if (nrow(defined_nums) > 0) defined_nums[, 2] else character(0)
    body_html <- str_replace_all(body_html, "\\[\\^(\\d+)\\]",
                                 function(m) {
                                   n <- str_extract(m, "\\d+")
                                   if (n %in% defined_nums) m else paste0("<sup>", n, "</sup>")
                                 })
  }

  ##### CREATE YAML HEADER #####

  yaml_header <- paste0(
    "---\n",
    "title: \"", str_replace_all(title, '"', "'"), "\"\n",
    ifelse(is_page,                       "listing: false\n",                                ""),
    ifelse(is_page && decade_label != "", paste0("decade_label: \"", decade_label, "\"\n"), ""),
    ifelse(iso_date   != "",              paste0("date: \"",         iso_date,     "\"\n"), ""),
    ifelse(date       != "",              paste0("letter_date: \"",  date,         "\"\n"), ""),
    ifelse(location   != "",              paste0("location: \"",     location,     "\"\n"), ""),
    ifelse(manuscript != "",              paste0("manuscript: \"",   manuscript,   "\"\n"), ""),
    "post_id: ", post_id, "\n",
    "letter_id: \"", slug, "\"\n",
    "---\n\n"
  )

  ##### BUILD METADATA DISPLAY BLOCK #####

  meta_lines <- c()
  if (date       != "") meta_lines <- c(meta_lines, paste0("**Date:** ",       date))
  if (location   != "") meta_lines <- c(meta_lines, paste0("**From:** ",       location))
  if (manuscript != "") meta_lines <- c(meta_lines, paste0("**Manuscript:** ", manuscript))
  
  citation_note <- "  \n*Citation metadata for this letter is available for automatic import into reference managers including Zotero, Mendeley, and EndNote.*"
  
  meta_block <- if (length(meta_lines) > 0) {
    paste0("::: {.letter-metadata}\n",
           paste(meta_lines, collapse = "  \n"),
           "\n:::\n\n")
  } else ""

  ##### COMBINE AND SAVE #####
  # Decade pages: yaml | meta | body | ## Notes
  # generate_decades.R appends: ## Letters\n{{< include _RANGE_letters.md >}}

  refman_note <- if (!is_page) {
    "\n\n*Citation metadata for this letter is available for automatic import into reference managers including Zotero, Mendeley, and EndNote.*"
  } else ""
  
  full_content <- paste0(yaml_header, meta_block, body_html, footnote_section, refman_note)

  if (is_page) {
    output_file <- file.path(output_dir, "decades", paste0(post_id, "-", slug, ".qmd"))
  } else {
    output_file <- file.path(output_dir, paste0(post_id, "-", slug, ".qmd"))
  }

  writeLines(full_content, output_file, useBytes = TRUE)
  return(post_id)
}

##### PROCESS ALL FILES #####

converted_count <- 0
skipped_count   <- 0
error_count     <- 0

for (html_file in html_files) {
  tryCatch({
    res <- convert_letter(html_file)
    if (is.null(res)) {
      skipped_count <<- skipped_count + 1
    } else {
      converted_count <<- converted_count + 1
    }
  }, error = function(e) {
    cat("Error converting", basename(html_file), ":", e$message, "\n")
    error_count <<- error_count + 1
  })
}

cat("\n Conversion complete!\n")
cat("Converted:", converted_count, "files\n")
cat("Skipped (WP utility pages):", skipped_count, "\n")
cat("Errors:   ", error_count, "\n")
cat("Output:   ", output_dir, "\n")
cat("\nDecade intro pages in:", file.path(output_dir, "decades"), "\n")
cat("Run STEP2, STEP3, then generate_decades.R to complete the site.\n")
