# IMPORTANT DO NOT RUN UNLESS YOU NEED TO REXTRACT THE LETTERS FROM THE WP DUMP
#EXTRACT_sql_to_html.R
# Stage: SQL dump line files → individual HTML files, one per letter
#
# Prerequisites: 
#   line_650.txt ... line_690.txt in C:/db/
#   (41 files, each containing one enormous INSERT INTO wp_posts line)
#
# Outputs:
#   C:/db/all_letters_html_Apr26/{post_id}-{slug}.html
#   C:/db/wp_posts.csv  (post_id + slug lookup table for use in STEP1)

library(stringr)
library(data.table)

##### CONFIGURATION #####

line_dir   <- "C:/db"
output_dir <- "C:/db/all_letters_html_Apr26"
csv_out    <- "C:/db/wp_posts.csv"

dir.create(output_dir, showWarnings = FALSE)

##### FIND LINE FILES #####

line_files <- list.files(line_dir, pattern = "^line_\\d+\\.txt$", full.names = TRUE)
cat(sprintf("Found %d line files\n\n", length(line_files)))

if (length(line_files) == 0) stop("No line files found in ", line_dir)

##### INITIALISE COUNTERS #####

letter_count     <- 0
page_count       <- 0
revision_records <- 0
other_records    <- 0
post_other       <- 0
post_publish     <- 0
no_content       <- 0

##### INITIALISE RESULTS COLLECTOR #####

posts_list <- list()

##### PROCESS EACH LINE FILE #####

for (line_file in line_files) {
  cat(sprintf("Processing %s...\n", basename(line_file)))
  
  content <- readLines(line_file, warn = FALSE, encoding = "UTF-8")
  
  if (length(content) == 0) {
    cat("  Empty file, skipping\n")
    next
  }
  
  # Each file is one enormous line — split into individual records
  records <- str_split(content, "\\),\\((?=\\d+,)")[[1]]
  
  # Strip leading ( from first record and trailing ); from last
  records[1]               <- str_replace(records[1], "^.*?\\(", "")
  records[length(records)] <- str_replace(records[length(records)], "\\);?$", "")
  
  cat(sprintf("  Split into %d records\n", length(records)))
  
  for (record in records) {
    
    ##### EXTRACT POST ID #####
    
    id_match <- str_match(record, "^(\\d+),")
    if (is.na(id_match[1, 2])) next
    post_id <- id_match[1, 2]
    
    ##### DETERMINE POST TYPE AND STATUS #####
    
    # Anchor to end of record: menu_order,'post_type','mime_type',comment_count
    type_match <- str_match(record, ",\\d+,'([a-z_-]+)','',\\d+\\s*$")
    post_type  <- if (!is.na(type_match[1, 2])) type_match[1, 2] else ""
    
    is_revision <- post_type == "revision"
    is_post     <- post_type == "post"
    is_page     <- post_type == "page"
    is_publish  <- str_detect(record, ",'publish',")
    
    if (is_revision) {
      revision_records <- revision_records + 1
      next
    } else if (!is_post && !is_page) {
      other_records <- other_records + 1
      next
    } else if (!is_publish) {
      post_other <- post_other + 1
      next
    }
    
    post_publish <- post_publish + 1
    
    ##### EXTRACT POST TITLE #####
    
    title_match <- str_match(record, "','([^']+)','','publish'")
    post_title  <- if (!is.na(title_match[1, 2])) title_match[1, 2] else ""
    
    ##### EXTRACT SLUG FROM GUID #####
    
    # Try wordpress/ URL format e.g. http://www.yongeletters.com/wordpress/3045/to-jemima-blackburn-4
    guid_match <- str_match(record, "wordpress/\\d+/([^',]+)'")
    slug <- guid_match[1, 2]
    
    # Try c21ch.newcastle.edu.au format e.g. https://c21ch.newcastle.edu.au/yonge/3406/to-lady-leconfield
    if (is.na(slug)) {
      guid_match2 <- str_match(record, "newcastle\\.edu\\.au/yonge/\\d+/([^',]+)'")
      slug <- guid_match2[1, 2]
    }
    
    # Fall back to generating slug from post title
    if (is.na(slug)) {
      slug <- if (post_title != "") {
        post_title %>%
          tolower() %>%
          str_replace_all("[^a-z0-9]+", "-") %>%
          str_replace_all("-+", "-") %>%
          str_replace("^-|-$", "")
      } else {
        paste0("letter-", post_id)
      }
      cat(sprintf("  ⚠ Generated slug from title for post %s: %s\n", post_id, slug))
    }
    
    ##### EXTRACT POST DATE #####
    
    date_match <- str_match(record, "'(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})'")
    post_date  <- if (!is.na(date_match[1, 2])) date_match[1, 2] else ""
    
    ##### EXTRACT HTML CONTENT #####
    
    # Structure: ...,'date','date_gmt','CONTENT','Title','','publish',...
    # Content lies between the second datetime and the ','Title','','publish' block
    date_locs <- str_locate_all(record, "'\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}'")[[1]]
    
    html_content <- ""
    
    if (nrow(date_locs) >= 2) {
      content_start <- date_locs[2, 2] + 2
      end_loc       <- str_locate(record, "','[^']+','','publish'")
      
      if (!is.na(end_loc[1, 1]) && end_loc[1, 1] > content_start) {
        content_end  <- end_loc[1, 1] - 1
        html_content <- substr(record, content_start, content_end)
        
        # Unescape SQL encoding
        html_content <- str_replace_all(html_content, "''"        , "'")
        html_content <- str_replace_all(html_content, "\\\\r\\\\n", "\n")
        html_content <- str_replace_all(html_content, "\\\\r"     , "\n")
        html_content <- str_replace_all(html_content, "\\\\n"     , "\n")
        html_content <- str_replace_all(html_content, "\\\\t"     , "\t")
        html_content <- str_replace_all(html_content, '\\\\"'     , '"')
      }
    }
    
    if (nchar(trimws(html_content)) == 0) {
      cat(sprintf("  ⚠ No content extracted for post %s (%s)\n", post_id, post_title))
      no_content <- no_content + 1
      next
    }
    
    ##### WRITE HTML FILE #####
    
    # Pages prefixed with "page-" to distinguish from letters
    filename <- if (is_page) {
      paste0(post_id, "-page-", slug, ".html")
    } else {
      paste0(post_id, "-", slug, ".html")
    }
    
    output_file <- file.path(output_dir, filename)
    writeLines(html_content, output_file)
    
    if (is_page) {
      page_count <- page_count + 1
      cat(sprintf("  PAGE %d. %s → %s\n", page_count, post_id, filename))
    } else {
      letter_count <- letter_count + 1
      cat(sprintf("  %d. %s → %s\n", letter_count, post_id, filename))
    }
    
    ##### ACCUMULATE FOR WP_POSTS.CSV #####
    
    posts_list[[length(posts_list) + 1]] <- list(
      post_id    = as.integer(post_id),
      post_type  = post_type,
      post_name  = slug,
      post_title = post_title,
      post_date  = post_date
    )
  }
  
  cat(sprintf("  Running total: %d letters, %d pages extracted\n\n", 
              letter_count, page_count))
}

##### WRITE WP_POSTS.CSV #####

if (length(posts_list) > 0) {
  posts_dt <- rbindlist(posts_list)
  fwrite(posts_dt, csv_out)
  cat(sprintf("Saved wp_posts.csv with %d rows to %s\n\n", nrow(posts_dt), csv_out))
}

##### SUMMARY #####

cat("=== EXTRACTION COMPLETE ===\n")
cat(sprintf("Letters extracted:        %d\n", letter_count))
cat(sprintf("Pages extracted:          %d\n", page_count))
cat(sprintf("Revisions skipped:        %d\n", revision_records))
cat(sprintf("Non-post/page skipped:    %d\n", other_records))
cat(sprintf("Non-publish skipped:      %d\n", post_other))
cat(sprintf("Records with no content:  %d\n", no_content))

cat("\n--- Slug format check ---\n")
with_seq    <- sum(str_detect(posts_dt$post_name, "-\\d+$"))
without_seq <- nrow(posts_dt) - with_seq
cat(sprintf("Slugs WITH sequence number:    %d\n", with_seq))
cat(sprintf("Slugs WITHOUT sequence number: %d\n", without_seq))
cat("Both are expected and correct.\n")

cat("\n--- Sample output files ---\n")
sample_files <- head(list.files(output_dir, pattern = "\\.html$"), 10)
cat(paste(sample_files, collapse = "\n"), "\n")