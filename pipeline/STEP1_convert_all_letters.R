# STEP1_convert_all_letters.R
# Converts HTML letters with [[person:123]text] tags to clean [[person:123]] format
#
# Aug 2026 changes:
#   - Footnote markers and definitions now use the LITERAL source ID number
#     (e.g. [[footnote:8]] -> [^8]), with no renumbering by position or
#     order of appearance. Gaps in the source stay gaps; a reused ID stays
#     that same ID in both places, correctly pointing at one definition.
#   - Footnote section is split per literal ID into "anchored" (has a
#     marker somewhere - body or metadata - gets a proper [^N]: definition)
#     and "orphan" (no marker anywhere - rendered as a visible **Note:**
#     line instead, since Quarto silently drops unreferenced [^N]:
#     definitions from the rendered page)
#   - Strikethrough tags (<del>, <s>, <strike>) are now preserved through
#     the HTML-to-markdown conversion, same pattern as the existing <u>
#     handling - protected with a placeholder before the generic
#     strip-all-tags line, restored afterward as raw <s> HTML
#
# Earlier changes (Apr26):
#   - preserve_italics(): <i>/<em> -> *...* before HTML stripping in footnotes
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
data_dir <- "C:/db/letters/data"
html_dir   <- "C:/db/letters/data/html"
output_dir <- "C:/db/letters_qmd_build" #disposable build folder kept out of repo

letterinfo <- read_csv(file.path(data_dir, "wp_letterinfo.csv"),
                       show_col_types = FALSE,
                       col_types = cols(letter_date   = col_character(),
                                        letter_dbdate = col_character()))
posts      <- read_csv(file.path(data_dir, "wp_posts.csv"), show_col_types = FALSE)


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

##### FUNCTION: EXTRACT FOOTNOTE IDS FROM A FIELD AND STRIP THE TAGS #####
# Returns the cleaned field text, a marker string of [^N] for each ID
# found (for re-appending to a metadata display line), and the raw list
# of IDs found - all using the LITERAL id from the tag, no renumbering.

extract_and_strip_markers <- function(x) {
  ids <- str_match_all(x, "\\[\\[footnote:(\\d+)\\]\\]")[[1]][, 2]
  clean <- str_replace_all(x, "\\[\\[footnote:\\d+\\]\\]", "")
  clean <- trimws(clean)
  marker_str <- if (length(ids) > 0) paste0("[^", ids, "]", collapse = "") else ""
  list(clean = clean, marker_str = marker_str, ids = ids)
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
    dbdate        <- ""
    location      <- ""
    manuscript    <- ""
    footnote_text <- ""
  } else {
    date <- ifelse(is.na(meta$letter_date[1]) || meta$letter_date[1] == "NA",
                   "", meta$letter_date[1])
    dbdate <- ifelse(is.na(meta$letter_dbdate[1]) || meta$letter_dbdate[1] == "NA" ||
                       meta$letter_dbdate[1] == "0000-00-00" || meta$letter_dbdate[1] == "",
                     "", meta$letter_dbdate[1])
    location <- ifelse(is.na(meta$letter_fromAddress[1]) || meta$letter_fromAddress[1] == "NA",
                       "", meta$letter_fromAddress[1])
    manuscript <- ifelse(is.na(meta$manuscript_location[1]) || meta$manuscript_location[1] == "NA",
                         "", meta$manuscript_location[1])
    footnote_text <- ifelse(is.na(meta$letter_footnote[1]) || meta$letter_footnote[1] == "",
                            "", meta$letter_footnote[1])
  }
  
  ##### FIX FOOTNOTE MARKERS: USE LITERAL SOURCE IDS, NO RENUMBERING #####
  # Every [[footnote:N]] becomes [^N] using its own literal N - not a
  # position-based renumbering. Gaps in the source stay gaps; a reused ID
  # stays that same ID in both places, both correctly pointing at the one
  # definition with that number. numbered_used collects every ID that
  # actually appears as a marker somewhere (date/location/manuscript/body),
  # used below to decide which footnote_text definitions are anchored.
  
  date_r       <- extract_and_strip_markers(date)
  date         <- date_r$clean
  location_r   <- extract_and_strip_markers(location)
  location     <- location_r$clean
  manuscript_r <- extract_and_strip_markers(manuscript)
  manuscript   <- manuscript_r$clean
  
  meta_fn_marker <- list(date = date_r$marker_str,
                         location = location_r$marker_str,
                         manuscript = manuscript_r$marker_str)
  
  body_ids  <- str_match_all(body_html, "\\[\\[footnote:(\\d+)\\]\\]")[[1]][, 2]
  body_html <- str_replace_all(body_html, "\\[\\[footnote:(\\d+)\\]\\]", "[^\\1]")
  
  numbered_used <- unique(c(date_r$ids, location_r$ids, manuscript_r$ids, body_ids))
  
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
  
  # Strikethrough - preserves an author's crossed-out false start in the
  # manuscript (e.g. "prede" struck through, replaced with "successor").
  # Protected the same way as <u> above, so it survives the generic
  # tag-strip below instead of vanishing into plain unstruck text. Source
  # HTML uses a mix of <del>, <s>, and <strike> - all three handled here.
  body_html <- gsub("(?i)<del[^>]*>",    "STRIKE_OPEN",  body_html, perl = TRUE)
  body_html <- gsub("(?i)</del>",        "STRIKE_CLOSE", body_html, perl = TRUE)
  # Lookahead requires the char right after "s" to be whitespace, ">", or
  # "/" (self-closing) - matches <s>, <s class="x">, <s/> but NOT <sup>,
  # <span>, <strong>, <section>, etc.
  body_html <- gsub("(?i)<s(?=[\\s>/])[^>]*>", "STRIKE_OPEN", body_html, perl = TRUE)
  body_html <- gsub("(?i)</s>",          "STRIKE_CLOSE", body_html, perl = TRUE)
  body_html <- gsub("(?i)<strike[^>]*>", "STRIKE_OPEN",  body_html, perl = TRUE)
  body_html <- gsub("(?i)</strike>",     "STRIKE_CLOSE", body_html, perl = TRUE)
  
  body_html <- gsub("<[^>]+>", "", body_html)
  body_html <- gsub("UNDERLINE_OPEN",  "<u>",  body_html)
  body_html <- gsub("UNDERLINE_CLOSE", "</u>", body_html)
  body_html <- gsub("STRIKE_OPEN",     "<s>",  body_html)
  body_html <- gsub("STRIKE_CLOSE",    "</s>", body_html)
  
  body_html <- gsub("\n\n\n+", "\n\n", body_html)
  body_html <- trimws(body_html)
  body_html <- gsub("^[\\s\\n]*'[\\s\\n]*", "", body_html, perl = TRUE)
  
  # Use letter_dbdate as primary ISO date source (clean sortable date);
  # fall back to parse_iso_date(date) when dbdate absent or zero.
  # Normalise dbdate: handles both YYYY-MM-DD and DD/MM/YYYY formats.
  iso_date <- if (dbdate != "") {
    if (str_detect(dbdate, "^\\d{2}/\\d{2}/\\d{4}$")) {
      # DD/MM/YYYY → YYYY-MM-DD
      parts <- str_split(dbdate, "/")[[1]]
      paste0(parts[3], "-", parts[2], "-", parts[1])
    } else {
      dbdate  # already YYYY-MM-DD
    }
  } else {
    parse_iso_date(date)
  }
  
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
  # Footnotes keyed by their LITERAL source ID (not a renumbered seq_num).
  # numbered_used = every ID that got a marker somewhere in this letter.
  # A definition whose ID ISN'T in numbered_used has no marker anywhere -
  # it's rendered as a plain visible Note rather than a [^N]: definition,
  # since Quarto silently drops unreferenced [^N]: definitions from the
  # rendered page.
  
  footnote_section <- ""
  
  if (footnote_text != "") {
    
    parts <- str_split(footnote_text, "(?=\\[\\[footnote:\\d+\\]\\])")[[1]]
    parts <- parts[parts != ""]
    
    footnotes <- list()
    for (part in parts) {
      num_match <- str_match(part, "^\\[\\[footnote:(\\d+)\\]\\]([\\s\\S]+)$")
      if (!is.na(num_match[1, 1])) {
        num  <- num_match[1, 2]
        text <- trimws(num_match[1, 3])
        text <- str_replace_all(text, "\\[\\[(cmybook|otherbook):\\d+\\]([^\\]]+)\\]", "\\2")
        text <- preserve_italics(text)          # preserve <i>/<em> before strip
        text <- str_replace_all(text, "<[^>]+>", "")
        text <- gsub("\\'", "'", text, fixed = TRUE)
        text <- trimws(text)
        footnotes[[num]] <- text
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
        
      } else {
        # Regular letters: split by literal ID into footnotes that have a
        # real marker somewhere (anchored) versus ones with no marker
        # anywhere (orphan). A letter can have both kinds at once.
        anchored_nums <- intersect(names(footnotes), numbered_used)
        orphan_nums   <- setdiff(names(footnotes), numbered_used)
        
        # Numeric sort, not alphabetic - "10" must not sort before "2"
        anchored_nums <- anchored_nums[order(as.integer(anchored_nums))]
        orphan_nums   <- orphan_nums[order(as.integer(orphan_nums))]
        
        section_parts <- c()
        
        if (length(anchored_nums) > 0) {
          footnote_lines <- c("", "", "---", "")
          for (num in anchored_nums) {
            footnote_lines <- c(footnote_lines, paste0("[^", num, "]: ", footnotes[[num]]))
          }
          section_parts <- c(section_parts, paste(footnote_lines, collapse = "\n"))
        }
        
        if (length(orphan_nums) > 0) {
          # No marker anywhere for these - render as visible unlinked text
          # rather than a Pandoc [^N]: definition, which Quarto would
          # silently drop from the rendered page as "unused".
          note_text <- paste(sapply(orphan_nums, function(n) footnotes[[n]]),
                             collapse = " ")
          note_block <- if (length(anchored_nums) > 0) {
            paste0("\n\n**Note:** ", note_text)
          } else {
            paste0("\n\n---\n\n**Note:** ", note_text)
          }
          section_parts <- c(section_parts, note_block)
        }
        
        footnote_section <- paste(section_parts, collapse = "")
      }
    }
  }
  
  # Downgrade any marker with no matching definition anywhere - body OR
  # metadata display line - to a plain superscript, rather than leaving a
  # dangling/broken [^N] reference visible on the page. Must run before
  # BUILD METADATA DISPLAY BLOCK below, since that's where meta_fn_marker
  # is used to build the Date/From/Manuscript lines.
  # Not needed for is_page — all [^N] already converted to <sup>N</sup> above.
  if (!is_page) {
    defined_nums <- str_match_all(footnote_section, "\\[\\^(\\d+)\\]:")[[1]]
    defined_nums <- if (nrow(defined_nums) > 0) defined_nums[, 2] else character(0)
    
    downgrade_unmatched <- function(x) {
      if (!str_detect(x, "\\[\\^\\d+\\]")) return(x)
      str_replace_all(x, "\\[\\^(\\d+)\\]",
                      function(m) {
                        n <- str_extract(m, "\\d+")
                        if (n %in% defined_nums) m else paste0("<sup>", n, "</sup>")
                      })
    }
    
    body_html <- downgrade_unmatched(body_html)
    meta_fn_marker$date       <- downgrade_unmatched(meta_fn_marker$date)
    meta_fn_marker$location   <- downgrade_unmatched(meta_fn_marker$location)
    meta_fn_marker$manuscript <- downgrade_unmatched(meta_fn_marker$manuscript)
  }
  
  ##### CREATE YAML HEADER #####
  
  yaml_header <- paste0(
    "---\n",
    "title: \"", str_replace_all(title, '"', "'"), "\"\n",
    ifelse(is_page,                       "listing: false\n",                                ""),
    ifelse(is_page && decade_label != "", paste0("decade_label: \"", decade_label, "\"\n"), ""),
    ifelse(iso_date   != "",              paste0("letter_iso_date: \"", iso_date,     "\"\n"), ""),
    ifelse(date       != "",              paste0("letter_date: \"",  str_replace_all(date, '"', "'"),         "\"\n"), ""),
    ifelse(location   != "",              paste0("location: \"",     str_replace_all(location, '"', "'"),     "\"\n"), ""),
    ifelse(manuscript != "",              paste0("manuscript: \"",   str_replace_all(manuscript, '"', "'"),   "\"\n"), ""),"post_id: ", post_id, "\n",
    "letter_id: \"", slug, "\"\n",
    "---\n\n"
  )
  
  ##### BUILD METADATA DISPLAY BLOCK #####
  # Footnote markers stripped from the metadata fields above are re-appended
  # here, on the rendered display line, using their literal source number,
  # so they show up exactly where the archived original site showed them
  # (e.g. after the Manuscript line), with the same number the original used.
  
  meta_lines <- c()
  if (date       != "") meta_lines <- c(meta_lines, paste0("**Date:** ",       date,       meta_fn_marker$date))
  if (location   != "") meta_lines <- c(meta_lines, paste0("**From:** ",       location,   meta_fn_marker$location))
  if (manuscript != "") meta_lines <- c(meta_lines, paste0("**Manuscript:** ", manuscript, meta_fn_marker$manuscript))
  
  meta_block <- if (length(meta_lines) > 0) {
    paste0("::: {.letter-metadata}\n",
           paste(meta_lines, collapse = "  \n"),
           "\n:::\n\n")
  } else ""
  
  ##### COMBINE AND SAVE #####
  # Decade pages: yaml | meta | body | ## Notes
  # generate_decades.R appends: ## Letters\n{{< include _RANGE_letters.md >}}
  # Note: "Citation metadata..." text is handled by CSS ::after on .quarto-appendix
  
  full_content <- paste0(yaml_header, meta_block, body_html, footnote_section)
  
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