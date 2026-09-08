  options(shiny.maxRequestSize = 30*1024^2)
  suppressPackageStartupMessages({
    library(shiny)
    library(dplyr)
    library(stringr)
    library(lubridate)
    library(pdftools)
    library(tibble)
    library(purrr)
    library(shinyjs)
    library(tesseract)
    library(DT)
    library(plotly)
    library(tidyr)
    library(gemini.R)
    library(stringdist)
    library(later)
  })
 
  # Gemini API configuration
  setAPI(Sys.getenv("GEMINI_API_KEY"))
  
  reconstruct_sentences <- function(lines) {
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]
    
    out <- character()
    buffer <- ""
    
    for (ln in lines) {
      buffer <- paste(buffer, ln)
      if (grepl("[.!?]$", ln)) {
        out <- c(out, trimws(buffer))
        buffer <- ""
      }
    }
    
    if (nzchar(buffer)) {
      out <- c(out, trimws(buffer))
    }
    
    out
  }
  
  # --------------------------------------------------------------------
  # Shared helpers
  # --------------------------------------------------------------------
  read_pdf_pages <- function(pdf_path, start_page = 1, end_page = 2) {
    pages <- pdftools::pdf_text(pdf_path)
    if (length(pages) == 0) return("")
    start_page <- max(1, start_page)
    end_page   <- min(length(pages), end_page)
    paste(pages[start_page:end_page], collapse = "\n")
  }
  
  clean_input_lines <- function(text_block) {
    if (is.null(text_block) || !nzchar(text_block)) return(character(0))
    
    # Split by newlines
    lines <- unlist(strsplit(text_block, "\\n"))
    lines <- gsub('["]', '', lines)
    lines <- gsub('!', '', lines)     # remove exclamation marks
    lines <- gsub('>', '', lines)
    lines <- gsub('<', '', lines)
    lines <- gsub('<', '', lines)
    lines <- gsub("^\\s*[A-Z]\\s+", "", lines)  # remove leading single uppercase letters like "N   "
    lines <- gsub('', '', lines)
    
    lines <- trimws(lines)
    
    # Group lines: start new item on • or empty line
    items <- character(0)
    cur <- NULL
    for (ln in lines) {
      if (grepl("^(•|\\*)", ln)) {
        if (!is.null(cur)) items <- c(items, cur)
        cur <- sub("^(•|\\*)\\s*", "", ln)
      } else if (nzchar(ln)) {
        cur <- if (is.null(cur)) ln else paste(cur, ln)
      } else {
        if (!is.null(cur)) { items <- c(items, cur); cur <- NULL }
      }
    }
    if (!is.null(cur)) items <- c(items, cur)
    
    items <- items[nchar(items) > 0]
    items
  }
  
  
  pad_to_max_length <- function(vec, max_length) {
    c(vec, rep("", max_length - length(vec)))
  }
  
  
  clean_input_text <- function(text) {
    # Clean and prepare the text
    cleaned_text <- gsub("\r", " ", text)
    cleaned_text <- gsub("[\r\n]+", " ", cleaned_text)
    cleaned_text <- str_squish(cleaned_text)
    return(cleaned_text)
  }
  
  filter_cf_lines <- function(lines) {
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]
    
    # normalize dashes/quotes
    lines <- gsub("[\u2013\u2014]", "-", lines)
    lines <- gsub("[\u2018\u2019]", "'", lines)
    lines <- gsub("[\u201C\u201D]", "\"", lines)
    
    # ---------- 1) REMOVE OUTCOME / INJURY-ONLY ----------
    outcome_only <- "(?i)\\b(fell|fallen|falling|injur(ed|y)|died|dead|killed|fatal|transported|hospital|life support|pronounced)\\b"
    
    # If a sentence mentions outcome BUT has no causal/unsafe pivot, remove it
    unsafe_pivot <- "(?i)\\b(because|due to|as a result|resulting from|after|when|while|failed|failure|unsecured|unstable|without|no\\s+\\w+\\s+protection|improper|incorrect|slipped|collapsed|broke|malfunction|overturned|unprotected|not\\s+secured|did\\s+not\\s+use|parked\\s+at\\s+.*rather\\s+than|climb(ed|ing)|scaled|tucked\\s+.*under\\s+his\\s+shirt)\\b"
    lines <- lines[!(grepl(outcome_only, lines, perl = TRUE) & !grepl(unsafe_pivot, lines, perl = TRUE))]
    
    # ---------- 2) REMOVE PURE BACKGROUND / CHRONOLOGY ----------
    # (these are common "story" phrases with no hazard signal)
    background <- "(?i)\\b(on the day prior|clocked in|routine duties|during the afternoon|during the evening|began arriving|one needed help|tools were located|normally performed|they started|they realized they could not remember|necessary to get the schematics)\\b"
    lines <- lines[!grepl(background, lines, perl = TRUE)]
    
    # ---------- 3) KEEP ONLY “UNSAFE/CAUSE” SENTENCES ----------
    # Stronger keep rule (expand over time as you see false negatives)
    keep <- "(?i)\\b(parked\\b.*rather\\s+than|fixed ladder|scaled|climb(ed|ing)|i-?beam|without\\b|no\\b.*protection|unsecured|unstable|failed|malfunction|improper|incorrect|broke|collapsed|slipped|overturned|tucked\\b.*under\\b.*shirt|carrying\\b.*(schematic|tool|load))\\b"
    lines <- lines[grepl(keep, lines, perl = TRUE)]
    
    unique(lines)
  }
  
  
  
  
  # --------------------------------------------------------------------
  # Incident Date Extraction Only
  # --------------------------------------------------------------------
  extract_report_date <- function(pdf_path = NULL,
                                  pdf_pages = NULL,
                                  summary_text = NULL,
                                  pages_to_scan = 3) {
    if (is.null(pdf_pages)) {
      if (is.null(pdf_path)) stop("Provide either pdf_path or pdf_pages.")
      pdf_pages <- pdftools::pdf_text(pdf_path)
    }
    if (length(pdf_pages) == 0) {
      return(tibble(raw = NA, date = as.Date(NA),
                    year = NA_integer_, month_num = NA_integer_,
                    month_name = NA_character_, day = NA_integer_))
    }
    
    first_n <- paste(pdf_pages[1:pages_to_scan], collapse = "\n")
    candidates <- list()
    push <- function(val) {
      if (!is.na(val) && nzchar(val)) candidates[[length(candidates) + 1]] <<- val
    }
    
    # --- Case 1: Full Month Day, Year ---
    pat_full <- "\\b(?:On|In)?\\s*[A-Z][a-z]+\\s\\d{1,2},\\s\\d{4}\\b"
    push(str_extract(first_n, pat_full))
    if (!is.null(summary_text)) push(str_extract(summary_text, pat_full))
    
    # --- Case 2: Month Year ---
    if (length(candidates) == 0) {
      pat_my <- "(?i)(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{4}"
      push(str_extract(first_n, pat_my))
      if (!is.null(summary_text)) push(str_extract(summary_text, pat_my))
    }
    
    # --- Case 3: Seasons (Spring, Summer, Fall, Winter + year) ---
    if (length(candidates) == 0) {
      pat_season <- "(?i)(Spring|Summer|Fall|Autumn|Winter)\\s+of?\\s*\\d{4}"
      push(str_extract(first_n, pat_season))
      if (!is.null(summary_text)) push(str_extract(summary_text, pat_season))
    }
    
    if (length(candidates) == 0) {
      return(tibble(raw = NA, date = as.Date(NA),
                    year = NA_integer_, month_num = NA_integer_,
                    month_name = NA_character_, day = NA_integer_))
    }
    
    raw <- str_squish(candidates[[1]])
    raw <- str_remove(raw, "^(On|In|the month of|of)\\s+")
    
    # --- Parsing ---
    if (grepl("\\d{1,2},", raw)) {
      # Full date
      parsed <- suppressWarnings(lubridate::mdy(raw))
      day <- lubridate::day(parsed)
      month_num <- lubridate::month(parsed)
      month_name <- as.character(lubridate::month(parsed, label = TRUE, abbr = FALSE))
    } else if (grepl("(?i)(January|February|March|April|May|June|July|August|September|October|November|December)", raw)) {
      # Month + Year
      parsed <- suppressWarnings(lubridate::my(raw))
      day <- NA_integer_
      month_num <- lubridate::month(parsed)
      month_name <- as.character(lubridate::month(parsed, label = TRUE, abbr = FALSE))
    } else if (grepl("(?i)(Spring|Summer|Fall|Autumn|Winter)", raw)) {
      # Seasons → approximate month
      yr <- as.integer(str_extract(raw, "\\d{4}"))
      if (grepl("Spring", raw, ignore.case = TRUE)) month_num <- 3
      if (grepl("Summer", raw, ignore.case = TRUE)) month_num <- 6
      if (grepl("Fall|Autumn", raw, ignore.case = TRUE)) month_num <- 9
      if (grepl("Winter", raw, ignore.case = TRUE)) month_num <- 12
      parsed <- as.Date(sprintf("%04d-%02d-01", yr, month_num))
      day <- NA_integer_
      month_name <- month.name[month_num]
    } else {
      parsed <- as.Date(NA)
      day <- NA_integer_
      month_num <- NA_integer_
      month_name <- NA_character_
    }
    
    tibble(
      raw        = raw,
      date       = parsed,
      year       = lubridate::year(parsed),
      month_num  = month_num,
      month_name = month_name,
      day        = day
    )
  }
  
  create_gemini_prompt <- function(cleaned_text) {
    prompt <- paste(
      "You are a construction safety analyst.",
      "From the following text, extract ONLY the sentences or sentence fragments that describe the *contributing factors* of the incident.",
      "A contributing factor must indicate an unsafe act or decision, improper equipment use, loss of control or mechanical failure, absence of protection, or uncertainty about the mechanism of injury.",
      "DO NOT extract sentences that describe proper or compliant actions, correct setup, routine work steps, neutral sequencing, or equipment specifications unless they explicitly describe failure, instability, or unsafe use.",
      "If a sentence explicitly states that equipment was properly used, correctly installed, or compliant, it must NOT be extracted.",
      "If a sentence contains both a compliant/proper setup clause and an unsafe act/decision clause, extract only the unsafe clause, starting from the first unsafe pivot word (e.g., decided, failed, unsecured, without, improper, slipped, collapsed, due to), and return that clause verbatim; do not include any earlier compliant/proper clause from the same sentence.",
      "If a contributing factor contains a pronoun whose reference is unclear (e.g., “it”, “this”, “they”), expand the sentence minimally by including the immediately preceding noun phrase that clarifies the unsafe act, while preserving the original wording and meaning.",
      "Return each contributing factor as a separate line, exactly as it appears in the text (verbatim).",
      "Do not include headings, bullets, numbering, symbols, or quotation marks.",
      "Do not paraphrase, summarize, interpret, or add commentary.",
      "If no contributing factors are explicitly or implicitly stated, return an empty response.\n\n",
      cleaned_text
    )
    return(prompt)
  }
  
  # Gemini helper: find or summarize contributing factors
  # --------------------------------------------------------------------
  # Gemini Helper Function
  # --------------------------------------------------------------------
  summarize_cf_with_gemini <- function(text_block) {
    if (is.null(text_block) || !nzchar(text_block)) return("")
    # Old Prompt
    # prompt <- paste(
    #   "You are a construction safety analyst. From the following text, extract the sentences or sentence fragments that describe the *contributing factors* of the incident.",
    #   "Return each contributing factor as a separate bullet point, exactly as it appears in the text (verbatim).",
    #   "Do not include any introductory headings, list markers, bullets, asterisks, or quotation marks.",
    #   "Do not paraphrase, summarize, or add commentary.\n\n",
    #   text_block
    # )
    # New prompt
    prompt <- paste(
      "You are a construction safety analyst.",
      "From the following text, extract ONLY the sentences or sentence fragments that describe the *contributing factors* of the incident.",
      "A contributing factor must indicate an unsafe act or decision, improper equipment use, loss of control or mechanical failure, absence of protection, or uncertainty about the mechanism of injury.",
      "DO NOT extract sentences that describe proper or compliant actions, correct setup, routine work steps, neutral sequencing, or equipment specifications unless they explicitly describe failure, instability, or unsafe use.",
      "If a sentence explicitly states that equipment was properly used, correctly installed, or compliant, it must NOT be extracted.",
      "If a sentence contains both a compliant/proper setup clause and an unsafe act/decision clause, extract only the unsafe clause, starting from the first unsafe pivot word (e.g., decided, failed, unsecured, without, improper, slipped, collapsed, due to), and return that clause verbatim; do not include any earlier compliant/proper clause from the same sentence.",
      "If a contributing factor contains a pronoun whose reference is unclear (e.g., “it”, “this”, “they”), expand the sentence minimally by including the immediately preceding noun phrase that clarifies the unsafe act, while preserving the original wording and meaning.",
      "Return each contributing factor as a separate line, exactly as it appears in the text (verbatim).",
      "Do not include headings, bullets, numbering, symbols, or quotation marks.",
      "Do not paraphrase, summarize, interpret, or add commentary.",
      "If no contributing factors are explicitly or implicitly stated, return an empty response.\n\n",
      text_block
    )
    
    
    
    result <- tryCatch({
      cat("Gemini call at:", format(Sys.time(), "%H:%M:%S"), "\n")
      res <- gemini(
        prompt,
        model = "2.5-flash-lite",
        maxOutputTokens = 512
      )
      
      # Handle possible list or nested content
      if (is.list(res)) {
        if (!is.null(res$output)) return(as.character(res$output))
        if (!is.null(res$text)) return(as.character(res$text))
        return(as.character(unlist(res)[1]))
      }
      as.character(res)
    }, error = function(e) {
      warning(paste("Gemini error:", e$message))
      return("")
    })
    
    return(trimws(result))
  }
  
  
  
  extract_age <- function(pdf_path = NULL,
                          pdf_pages = NULL,
                          summary_text = NULL,
                          pages_to_scan = 2) {
    # ---- inline cleaner ----
    .clean_text <- function(x) {
      if (is.null(x) || is.na(x)) return("")
      x <- gsub("\r", "\n", x, fixed = TRUE)
      x <- gsub("-\\s*\\n\\s*", "-", x)                    # de-hyphenate across line breaks
      x <- gsub("(?<!\\n)\\n(?!\\n)", " ", x, perl = TRUE) # single newlines -> space
      str_squish(x)
    }
    
    # ---- get text ----
    if (is.null(pdf_pages)) {
      if (is.null(pdf_path)) stop("Provide either pdf_path or pdf_pages.")
      pdf_pages <- pdftools::pdf_text(pdf_path)
    }
    if (length(pdf_pages) == 0) {
      return(tibble(source = NA_character_, match_text = NA_character_, age = as.numeric(NA)))
    }
    pages_to_scan <- max(1, min(pages_to_scan, length(pdf_pages)))
    
    first_page <- .clean_text(pdf_pages[1])
    first_n    <- .clean_text(paste(pdf_pages[1:pages_to_scan], collapse = "\n"))
    summary_cl <- if (!is.null(summary_text)) .clean_text(summary_text) else NULL
    
    # ---- regex patterns ----
    pats <- c(
      "(?i)\\b(\\d{1,2})\\s*[-]?\\s*year\\s*[-]?\\s*old\\b",       # 45-year-old
      "(?i)\\b(\\d{1,2})\\s+years?\\s+old\\b",                     # 45 years old
      "(?i)\\baged\\s+(\\d{1,2})\\b",                              # aged 45
      "(?i)\\bage\\s*:?\\s*(\\d{1,2})\\b",                         # age 45 / age: 45
      "(?i)\\b(?:in|at)\\s+(?:his|her|their)\\s+(\\d{2})s\\b"      # in his 30s
    )
    
    sources <- list(
      list(name = "first_page", text = first_page),
      list(name = "first_pages", text = first_n),
      list(name = "summary", text = summary_cl)
    )
    
    # ---- search ----
    find_first <- function(txt, src_name) {
      if (is.null(txt) || !nzchar(txt)) return(NULL)
      for (p in pats) {
        m <- str_match(txt, p)
        if (!all(is.na(m))) {
          age_num <- suppressWarnings(as.numeric(m[,2]))
          if (!is.na(age_num)) {
            return(tibble(source = src_name, match_text = m[,1], age = age_num))
          }
        }
      }
      NULL
    }
    
    out <- purrr::reduce(sources, function(acc, s) {
      if (!is.null(acc)) return(acc)
      find_first(s$text, s$name)
    }, .init = NULL)
    
    if (is.null(out)) {
      tibble(source = NA_character_, match_text = NA_character_, age = as.numeric(NA))
    } else {
      out
    }
  }
  
  
  
  # --------------------------------------------------------------------
  # Registry for Summaries (Multi-State)
  # --------------------------------------------------------------------
  .algos <- new.env(parent = emptyenv())
  
  register_algo <- function(code, fun) assign(code, fun, envir = .algos)
  get_algo <- function(code) {
    if (exists(code, envir = .algos, inherits = FALSE)) get(code, envir = .algos) else NULL
  }
  
  # --------------------------------------------------------------------
  # State Summary Extractors
  # --------------------------------------------------------------------
  # --- MO
  register_algo("MO", function(text_in, pdf_path) {
    block <- stringr::str_extract(
      text_in,
      "(?is)SUMMARY[\\s\\S]*?(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSION|RESULTS|\\Z))"
    )
    if (is.na(block)) {
      return(list(summary = NA, method = "MO", conf = 0.1))
    }
    
    # Remove the leading "SUMMARY" header (case-insensitive, with colon/space)
    block <- stringr::str_replace(block, "(?i)^\\s*SUMMARY[:\\s]*", "")
    
    # Cut off before recommendations
    block <- stringr::str_split(block, "(?is)The\\s+MO\\s+FACE\\s+investigator\\s+concluded")[[1]][1]
    
    block <- stringr::str_squish(block)
    
    list(summary = block, method = "MO regex", conf = 0.9)
  })
  
  
  # --- MD Summary
  register_algo("MD", function(text_in, pdf_path) {
    block <- stringr::str_extract(
      text_in,
      "(?is)SUMMARY[\\s\\S]*?(?=(The MD/FACE Field Investigator concluded|Maryland FACE Program concluded|employers should:|INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSION|\\Z))"
    )
    
    if (is.na(block)) return(list(summary=NA, method="MD", conf=0.1))
    
    # Clean out the SUMMARY header
    block <- stringr::str_replace(block, "(?is)^SUMMARY[:\\s]*", "")
    
    list(summary = stringr::str_squish(block), method="MD regex", conf=0.95)
  })
  
  # --- NC
  register_algo("NC", function(text_in, pdf_path) {
    # Extract from SUMMARY to INTRODUCTION (or next section)
    block <- stringr::str_extract(
      text_in,
      "(?is)SUMMARY[\\s\\S]*?(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS?|RESULTS|\\Z))"
    )
    if (is.na(block)) {
      return(list(summary = NA, method = "NC", conf = 0.1))
    }
    
    # Remove the leading "SUMMARY" header
    block <- stringr::str_replace(block, "(?i)^\\s*SUMMARY[:\\s]*", "")
    
    # Cut off if recommendations start ("NIOSH investigators concluded")
    block <- stringr::str_split(block, "(?is)NIOSH\\s+investigators\\s+concluded")[[1]][1]
    
    # Squish whitespace
    block <- stringr::str_squish(block)
    
    list(summary = block, method = "NC regex", conf = 0.9)
  })
  
  
  # --- IN Summary extractor
  register_algo("IN", function(text_in, pdf_path) {
    block <- stringr::str_extract(
      text_in,
      "(?is)SUMMARY[:\\s]*[\\s\\S]*?(?=(The FACE investigator concluded|employers and employees|RECOMMENDATIONS|INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSION|\\Z))"
    )
    
    if (is.na(block)) return(list(summary=NA, method="IN", conf=0.1))
    
    # Remove the SUMMARY header
    block <- stringr::str_replace(block, "(?is)^SUMMARY[:\\s]*", "")
    
    # Clean up whitespace, stray line numbers, etc.
    block <- gsub("\r", " ", block)
    block <- gsub("\n+", " ", block)
    block <- gsub("\\s{2,}", " ", block)
    block <- stringr::str_trim(block)
    
    list(summary = block, method="IN regex", conf=0.95)
  })
  
  
  # --- WV (unified)
  register_algo("WV", function(text_in, pdf_path) {
    block <- stringr::str_extract(
      text_in,
      "(?is)SUMMARY[\\s\\S]*?(?=(RECOMMENDATIONS|BACKGROUND|INTRODUCTION|INVESTIGATION|CONCLUSIONS|\\Z))"
    )
    if (is.na(block)) return(list(summary = NA, method = "WV", conf = 0.1))
    
    # Trim if WV FACE investigator text introduces recos
    block <- stringr::str_replace(
      block,
      "(?is)(The\\s+WV\\s+FACE.*?employers\\s+should:?[\\s\\S]*)$",
      ""
    )
    
    # Cleanup
    block <- stringr::str_remove(block, "(?i)^SUMMARY\\s*")
    block <- stringr::str_squish(block)
    
    list(summary = block, method = "WV regex", conf = 0.9)
  })
  
  
  # --- MA
  register_algo("MA", function(text_in, pdf_path) {
    # Capture only SUMMARY section up until FACE Program conclusions or INTRODUCTION
    block <- stringr::str_extract(
      text_in,
      "(?is)SUMMARY[\\s\\S]*?(?=(The MA FACE Program concluded|Massachusetts FACE Program concluded|employers should:|INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSION|\\Z))"
    )
    
    if (is.na(block)) return(list(summary=NA, method="MA", conf=0.1))
    
    # --- Clean out the SUMMARY header
    block <- stringr::str_replace(block, "(?is)^SUMMARY[:\\s]*", "")
    
    list(summary = stringr::str_squish(block), method="MA regex", conf=0.95)
  })
  
  
  
  # --- CA
  register_algo("CA", function(text_in, pdf_path) {
    block <- stringr::str_extract(
      text_in,
      "(?is)SUMMARY[\\s\\S]*?(?=(The CA/FACE investigator concluded|California FACE investigator concluded|Employers should|INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSION|\\Z))"
    )
    
    if (is.na(block)) return(list(summary=NA, method="CA", conf=0.1))
    
    # --- Clean header ---
    block <- stringr::str_replace(block, "(?is)^SUMMARY[:\\s]*", "")
    
    list(summary = stringr::str_squish(block), method="CA regex", conf=0.95)
  })
  
  
  # --- OR
  register_algo("OR", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "SUMMARY[\\s\\S]*?(?=INTRODUCTION)")
    if (is.na(block)) return(list(summary=NA, method="OR", conf=0.1))
    list(summary = stringr::str_squish(block), method="OR regex", conf=0.9)
  })
  
  # --- OR_v2
  register_algo("OR_v2", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "SUMMARY[\\s\\S]*?(?=INTRODUCTION)")
    if (is.na(block)) return(list(summary=NA, method="OR_v2", conf=0.1))
    list(summary = stringr::str_squish(block), method="OR_v2 regex", conf=0.9)
  })
  
  # --- NJ
  register_algo("NJ", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "(?is)SUMMARY[\\s\\S]*?(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS|\\Z))")
    if (is.na(block)) return(list(summary=NA, method="NJ", conf=0.1))
    list(summary = stringr::str_squish(block), method="NJ regex", conf=0.9)
  })
  
  # --- NY
  register_algo("NY", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "(?is)SUMMARY[\\s\\S]*?(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSION|\\Z))")
    if (is.na(block)) return(list(summary=NA, method="NY", conf=0.1))
    list(summary = stringr::str_squish(block), method="NY regex", conf=0.9)
  })
  
  # --- IA
  register_algo("IA", function(text_in, pdf_path) {
    block <- stringr::str_extract(
      text_in,
      "(?is)SUMMARY[\\s\\S]*?(?=(RECOMMENDATIONS\\b|INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSION|\\Z))"
    )
    
    if (is.na(block)) {
      return(list(summary = NA, method = "IA", conf = 0.1))
    }
    
    # Remove the header "SUMMARY"
    block <- stringr::str_replace(block, "(?is)^SUMMARY[:\\s]*", "")
    
    list(
      summary = stringr::str_squish(block),
      method  = "IA regex",
      conf    = 0.9
    )
  })
  
  
  # --- KY v1–v5
  register_algo("KY_v1", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "SUMMARY[\\s\\S]*?(?=INTRODUCTION)")
    if (is.na(block)) return(list(summary=NA, method="KY_v1", conf=0.1))
    list(summary = stringr::str_squish(block), method="KY_v1 regex", conf=0.9)
  })
  register_algo("KY_v2", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "SUMMARY[\\s\\S]*?(?=INTRODUCTION)")
    if (is.na(block)) return(list(summary=NA, method="KY_v2", conf=0.1))
    list(summary = stringr::str_squish(block), method="KY_v2 regex", conf=0.9)
  })
  register_algo("KY_v3", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "SUMMARY[\\s\\S]*?(?=INTRODUCTION)")
    if (is.na(block)) return(list(summary=NA, method="KY_v3", conf=0.1))
    list(summary = stringr::str_squish(block), method="KY_v3 regex", conf=0.9)
  })
  register_algo("KY_v4", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "SUMMARY[\\s\\S]*?(?=INTRODUCTION)")
    if (is.na(block)) return(list(summary=NA, method="KY_v4", conf=0.1))
    list(summary = stringr::str_squish(block), method="KY_v4 regex", conf=0.9)
  })
  register_algo("KY_v5", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "SUMMARY[\\s\\S]*?(?=INTRODUCTION)")
    if (is.na(block)) return(list(summary=NA, method="KY_v5", conf=0.1))
    list(summary = stringr::str_squish(block), method="KY_v5 regex", conf=0.9)
  })
  
  # --- MI variants
  register_algo("MI", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "(?is)SUMMARY[\\s\\S]*?(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|\\Z))")
    if (is.na(block)) return(list(summary=NA, method="MI", conf=0.1))
    list(summary = stringr::str_squish(block), method="MI regex", conf=0.9)
  })
  register_algo("MI_v1", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "SUMMARY[\\s\\S]*?(?=INTRODUCTION)")
    if (is.na(block)) return(list(summary=NA, method="MI_v1", conf=0.1))
    list(summary = stringr::str_squish(block), method="MI_v1 regex", conf=0.9)
  })
  register_algo("MI_v2", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "SUMMARY[\\s\\S]*?(?=INTRODUCTION)")
    if (is.na(block)) return(list(summary=NA, method="MI_v2", conf=0.1))
    list(summary = stringr::str_squish(block), method="MI_v2 regex", conf=0.9)
  })
  register_algo("MI_v3", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "SUMMARY[\\s\\S]*?(?=INTRODUCTION)")
    if (is.na(block)) return(list(summary=NA, method="MI_v3", conf=0.1))
    list(summary = stringr::str_squish(block), method="MI_v3 regex", conf=0.9)
  })
  register_algo("MI_v4", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "SUMMARY[\\s\\S]*?(?=INTRODUCTION)")
    if (is.na(block)) return(list(summary=NA, method="MI_v4", conf=0.1))
    list(summary = stringr::str_squish(block), method="MI_v4 regex", conf=0.9)
  })
  
  # --- WA
  register_algo("WA", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "(?is)SUMMARY[\\s\\S]*?(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS|\\Z))")
    if (is.na(block)) return(list(summary=NA, method="WA", conf=0.1))
    list(summary = stringr::str_squish(block), method="WA regex", conf=0.9)
  })
  
  # --- WY (Wyoming) Summary
  register_algo("WY", function(text_in, pdf_path) {
    
    # Grab SUMMARY up to INTRODUCTION (or other next section just in case)
    block <- stringr::str_extract(
      text_in,
      "(?is)\\bSUMMARY\\b[:\\s]*[\\s\\S]*?(?=\\n\\s*(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS?)\\b|\\Z)"
    )
    
    if (is.na(block) || nchar(block) < 20) {
      return(list(summary = NA, method = "WY", conf = 0.1))
    }
    
    # Remove the SUMMARY header itself
    block <- stringr::str_replace(block, "(?is)^\\s*SUMMARY\\b[:\\s]*", "")
    
    # Clean up whitespace, keep bullets as plain text
    block <- gsub("\r", " ", block)
    block <- gsub("\n+", " ", block)
    block <- stringr::str_squish(block)
    
    return(list(summary = block, method = "WY regex", conf = 0.9))
  })
  
  
  # --- VA Summary extractor (super robust)
  register_algo("VA", function(pdf_path){
    text <- read_pdf_pages(pdf_path, 1, 5)
    if (is.na(text) || nchar(text) < 20) return(NA_character_)
    
    # Anchor from "On <date>" until just before "NIOSH investigators"
    block <- stringr::str_extract(
      text,
      "(?is)(On\\s+[A-Z][a-z]+\\s+\\d{1,2},\\s+\\d{4}[\\s\\S]*?)(?=(NIOSH investigators|INTRODUCTION|BACKGROUND|INVESTIGATION|\\Z))"
    )
    
    # Fallback: grab everything before INTRODUCTION if regex above fails
    if (is.na(block) || nchar(block) < 50) {
      block <- stringr::str_extract(
        text,
        "(?is)(On[\\s\\S]*?)(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|\\Z))"
      )
    }
    
    if (is.na(block) || nchar(block) < 50) return(NA_character_)
    
    # Cleanup
    block <- gsub("\r", " ", block)
    block <- stringr::str_replace_all(block, "\\s+", " ")
    block <- stringr::str_trim(block)
    
    return(block)
  })
  
  
  
  # --- WI
  register_algo("WI", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "(?is)SUMMARY[\\s\\S]*?(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS|\\Z))")
    if (is.na(block)) return(list(summary=NA, method="WI", conf=0.1))
    list(summary = stringr::str_squish(block), method="WI regex", conf=0.9)
  })
  
  # --- AK
  register_algo("AK", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in,
                                  "(?is)(?<=SUMMARY)([\\s\\S]*?)(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS|\\Z))"
    )
    
    if (is.na(block)) return(list(summary=NA, method="AK", conf=0.1))
    list(summary = stringr::str_squish(block), method="AK regex", conf=0.9)
  })
  
  # --- OK
  register_algo("OK", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "(?is)SUMMARY[\\s\\S]*?(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS|\\Z))")
    if (is.na(block)) return(list(summary=NA, method="OK", conf=0.1))
    list(summary = stringr::str_squish(block), method="OK regex", conf=0.9)
  })
  
  # --- NE
  register_algo("NE", function(text_in, pdf_path) {
    block <- stringr::str_extract(
      text_in,
      "(?is)SUMMARY[\\s\\S]*?(?=(The Nebraska Department of Labor investigator concluded|PROGRAM OBJECTIVE|INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS|\\Z))"
    )
    
    if (is.na(block)) {
      return(list(summary = NA, method = "NE", conf = 0.1))
    }
    
    # Remove the word "SUMMARY" itself
    block <- stringr::str_replace(block, "(?is)^SUMMARY[:\\s]*", "")
    
    list(
      summary = stringr::str_squish(block),
      method  = "NE regex",
      conf    = 0.9
    )
  })
  
  
  # --- TX
  register_algo("TX", function(text_in, pdf_path) {
    block <- stringr::str_extract(text_in, "(?is)SUMMARY[\\s\\S]*?(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS|\\Z))")
    if (is.na(block)) return(list(summary=NA, method="TX", conf=0.1))
    list(summary = stringr::str_squish(block), method="TX regex", conf=0.9)
  })
  
  # --- SC
  register_algo("SC", function(text_in, pdf_path) {
    block <- stringr::str_extract(
      text_in,
      "(?is)SUMMARY[\\s\\S]*?(?=(NIOSH\\s+investigators\\s+concluded|Additionally,\\s+prime\\s+contractors\\s+should|INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS|\\Z))"
    )
    
    if (is.na(block)) {
      return(list(summary = NA, method = "SC", conf = 0.1))
    }
    
    # Remove the header "SUMMARY"
    block <- stringr::str_replace(block, "(?is)^SUMMARY[:\\s]*", "")
    
    list(
      summary = stringr::str_squish(block),
      method  = "SC regex",
      conf    = 0.9
    )
  })
  
  
  
  # --- MN Summary
  register_algo("MN", function(text_in, pdf_path) {
    block <- stringr::str_extract(
      text_in,
      "(?is)SUMMARY[\\s\\S]*?(?=MN\\s+FACE\\s+investigators\\s+concluded|INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS|\\Z)"
    )
    
    if (is.na(block)) {
      return(list(summary = NA, method = "MN", conf = 0.1))
    }
    
    # Remove "SUMMARY" header
    block <- stringr::str_replace(block, "(?is)^SUMMARY[:\\s]*", "")
    
    # Clean up spacing/line breaks
    block <- stringr::str_squish(block)
    
    list(
      summary = block,
      method  = "MN regex",
      conf    = 0.9
    )
  })
  
  # --- CO (Colorado) Summary
  register_algo("CO", function(text_in, pdf_path) {
    block <- stringr::str_extract(
      text_in,
      "(?is)SUMMARY[\\s\\S]*?(?=MN\\s+FACE\\s+investigators\\s+concluded|INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS|\\Z)"
    )
    
    if (is.na(block)) {
      return(list(summary = NA, method = "CO", conf = 0.1))
    }
    
    # Remove "SUMMARY" header
    block <- stringr::str_replace(block, "(?is)^SUMMARY[:\\s]*", "")
    
    # Clean up spacing/line breaks
    block <- stringr::str_squish(block)
    
    list(
      summary = block,
      method  = "CO regex",
      conf    = 0.9
    )
  })
  
  
  # --------------------------------------------------------------------
  # Extract summary dispatcher
  # --------------------------------------------------------------------
  extract_summary <- function(state_code, pdf_path) {
    algo <- get_algo(state_code)
    if (is.null(algo)) {
      warning(sprintf("No summary algo registered for state_code='%s'", state_code))
      return(NA_character_)
    }
    out <- tryCatch(algo(read_pdf_pages(pdf_path, 1, 3), pdf_path),
                    error = function(e) list(summary=NA))
    out$summary
  }
  
  
  # --------------------------------------------------------------------
  # Registry for Recommendations (Multi-State)
  # --------------------------------------------------------------------
  .reco_algos <- new.env(parent = emptyenv())
  
  register_reco_algo <- function(code, fun) assign(code, fun, envir = .reco_algos)
  get_reco_algo <- function(code) {
    if (exists(code, envir = .reco_algos, inherits = FALSE)) get(code, envir = .reco_algos) else NULL
  }
  
  # --------------------------------------------------------------------
  # State Recommendation Extractors
  # --------------------------------------------------------------------
  
  # --- MI
  register_reco_algo("MI", function(pdf_path) {
    text <- read_pdf_pages(pdf_path, 1, 2)
    block <- str_extract(text, "RECOMMENDATIONS[\\s\\S]*?(?=\\n\\s*BACKGROUND|\\n\\s*APPENDIX|\\n\\s*$)")
    if (is.na(block) || nchar(block) < 50)
      block <- str_extract(text, "RECOMMENDATIONS[\\s\\S]*?(?=\\n\\s*INTRODUCTION|\\n\\s*APPENDIX|\\n\\s*$)")
    if (is.na(block)) return(NA_character_)
    
    txt <- str_squish(str_replace_all(block, "\n", " "))
    txt <- str_replace_all(txt, "(?<=[a-z])(?=[A-Z])", " ")
    txt <- str_replace_all(txt, "(?<=\\D)(?=\\d)", " ")
    txt <- str_replace_all(txt, "(?<=\\w)(?=\\p{Punct})", "")
    txt <- str_replace_all(txt, "\\s{2,}", " ")
    
    recs <- str_extract_all(txt, "•\\s*[^•]+")[[1]]
    if (length(recs) == 0) return(str_remove(txt, "^RECOMMENDATIONS\\s*"))
    
    recs <- str_remove(recs, "^•\\s*")
    recs <- str_remove(recs, "…\\s*LEARN[\\s\\S]*$")
    recs <- str_remove(recs, "\\sLEARN[\\s\\S]*$")
    recs <- recs[!grepl("^LEARN MORE>", recs)]
    str_squish(str_trim(recs))
  })
  
  # --- WY Recommendations (bullets inside SUMMARY, before INTRODUCTION)
  register_reco_algo("WY", function(pdf_path) {
    
    text <- read_pdf_pages(pdf_path, 1, 3)
    if (is.na(text) || nchar(text) < 20) return(NA_character_)
    
    # Keep everything from SUMMARY up to INTRODUCTION
    sum_block <- stringr::str_extract(
      text,
      "(?is)\\bSUMMARY\\b[\\s\\S]*?(?=\\n\\s*INTRODUCTION\\b|\\Z)"
    )
    if (is.na(sum_block) || nchar(sum_block) < 50) return(NA_character_)
    
    # Cut to the recommendations lead-in (precautions/should)
    # WY often uses "through the following precautions:"
    reco_block <- stringr::str_extract(
      sum_block,
      "(?is)(?:following\\s+precautions\\s*:|employers\\s+may\\s+be\\s+able[\\s\\S]*?precautions\\s*:)[\\s\\S]*$"
    )
    if (is.na(reco_block) || nchar(reco_block) < 20) return(NA_character_)
    
    # Normalize common bullet glyphs to "•" (just in case)
    reco_block <- gsub("\r", "\n", reco_block)
    reco_block <- stringr::str_replace_all(
      reco_block,
      "[\u2022\u25CF\u25AA\u2219\u2043\u2023\uF0B7\u0095]||·",
      "•"
    )
    
    # Split into lines and rebuild bullet items (handles wrapped lines)
    lines <- unlist(strsplit(reco_block, "\n"))
    lines <- stringr::str_trim(lines)
    lines <- lines[nchar(lines) > 0]
    
    items <- character(0)
    cur <- NULL
    
    for (ln in lines) {
      if (grepl("^•\\s*", ln)) {
        if (!is.null(cur)) items <- c(items, stringr::str_squish(cur))
        cur <- sub("^•\\s*", "", ln)
      } else if (!is.null(cur)) {
        # continuation line for the current bullet
        cur <- paste(cur, ln)
      }
    }
    if (!is.null(cur)) items <- c(items, stringr::str_squish(cur))
    
    items <- stringr::str_squish(items)
    items <- items[nchar(items) > 0]
    
    if (length(items) == 0) NA_character_ else items
  })
  
  # --- CA
  register_reco_algo("CA", function(pdf_path) {
    text <- read_pdf_pages(pdf_path, 1, 3)
    if (is.na(text) || nchar(text) < 20) return(NA_character_)
    
    text <- gsub("\r", "", text)
    text <- gsub("([A-Za-z])-\\s*\\n\\s*([A-Za-z])", "\\1\\2", text)
    
    # --- Grab block starting with "employers should:" inside SUMMARY
    block <- stringr::str_extract(
      text,
      "(?is)SUMMARY[\\s\\S]*?(employers\\s+should:?[\\s\\S]*?)(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSION|\\Z))"
    )
    if (is.na(block) || nchar(block) < 40) return(NA_character_)
    
    # --- Strip lead-in text before actual recommendations
    block <- stringr::str_replace(block, "(?is)^.*?employers\\s+should:?\\s*", "")
    
    # --- Normalize bullet glyphs
    block <- stringr::str_replace_all(
      block,
      "[\u2022\u25CF\u25AA\u2219\u2043\u2023\uF0B7\u0095•·]|[.]", " - "
    )
    
    # --- Break into lines and normalize
    lines <- unlist(strsplit(block, "\n"))
    lines <- stringr::str_trim(lines)
    lines <- lines[nchar(lines) > 0]
    
    # --- Detect bullets / dot-led items
    marked <- stringr::str_replace_all(
      lines,
      "^(\\-|\\*|\\.|)\\s+", "||| "
    )
    
    items <- unlist(strsplit(paste(marked, collapse="\n"), "\\|\\|\\|\\s+"))
    items <- trimws(items)
    items <- stringr::str_squish(items)
    items <- items[nchar(items) > 0]
    
    # --- If nothing parsed, try keyword-based split as fallback
    if (length(items) < 2) {
      items <- unlist(stringr::str_split(
        block,
        "(?i)(?=\\b(Ensure|Consider|Install|Develop|Implement|Provide|Train|Use|Perform|Assure)\\b)"
      ))
      items <- stringr::str_squish(items)
      items <- items[nchar(items) > 0]
    }
    
    if (length(items) == 0) NA_character_ else items
  })
  
  # --- WA
  register_reco_algo("WA", function(pdf_path) {
    text <- read_pdf_pages(pdf_path, start_page = 1, end_page = 4)
    
    end_markers <- c(
      "INTRODUCTION\\b","BACKGROUND\\b","INVESTIGATION\\b","RESULTS?\\b",
      "CONCLUSIONS?\\b","APPENDIX\\b","EMPLOYERS\\s+SHOULD\\b",
      "Washington\\s+State\\s+Fatality\\s+Assessment\\s+and\\s+Control\\s+Evaluation(?:\\s*\\(WA\\s*FACE\\))?",
      "To\\s+prevent\\s+similar\\s+(?:incidents|incidences)\\s+the\\s+Washington\\s+State\\s+Fatality"
    )
    end_alt <- paste(end_markers, collapse = "|")
    
    header_alt <- "(?is)\\b(RECOMMENDATIONS|Recommendations\\s+for\\s+prevention:?|these\\s+guidelines:)\\b"
    pat <- paste0(header_alt, "[\\s\\S]*?(?=\\n\\s*(?:", end_alt, ")\\s*|\\Z)")
    block <- stringr::str_extract(text, pat)
    
    if (is.na(block) || nchar(block) < 50) {
      block <- stringr::str_extract(
        text,
        "(?is)\\b(RECOMMENDATIONS|Recommendations\\s+for\\s+prevention:?|these\\s+guidelines:)\\b[\\s\\S]*?(?=\\n\\s*(INTRODUCTION|APPENDIX)\\b|\\Z)"
      )
    }
    if (is.na(block)) return(NA_character_)
    
    txt <- str_squish(block)
    recs <- extract_numbered_items(txt)
    
    if (length(recs) == 0) {
      bullet_pat <- "(?:•||\\-|–|—|\\*)\\s*[^\n•\\-–—\\*][^•\\-–—\\*]*"
      hits <- stringr::str_extract_all(txt, bullet_pat)[[1]]
      if (length(hits) > 0) recs <- hits
    }
    
    lead_in <- stringr::str_match(
      txt,
      paste0("(?is)^", header_alt, "\\s*(.*?)(?=(?:\\s*(?:•||\\-|–|—|\\*|\\d+\\.)\\s)|\\Z)")
    )[,2]
    lead_in <- if (!is.na(lead_in)) stringr::str_squish(lead_in) else NA_character_
    
    if (length(recs) > 0 && !is.na(lead_in) && nchar(lead_in) > 40) {
      recs <- c(lead_in, recs)
    }
    
    if (length(recs) == 0) {
      out <- stringr::str_remove(txt, paste0("^", header_alt, "\\s*"))
      out <- stringr::str_remove(out, "(?is)\\bLEARN\\s+MORE.*$")
      return(if (nzchar(out)) out else NA_character_)
    }
    
    recs <- stringr::str_remove(recs, "^(?i)\\s*(?:•||\\-|–|—|\\*|\\d+\\.)\\s*")
    recs <- stringr::str_replace_all(recs, "\\s{2,}", " ")
    recs <- recs[nchar(recs) > 0]
    
    if (length(recs) > 0) recs else NA_character_
  })
  
  # --------------------------------------------------------------------
  # (… SNIP — here would continue with NE, NJ, MA, KY_v1, KY_v2, KY_v3,
  # KY_v4, KY_v5, IA, OR, NY, WV, WV_v2, AL, OK, TX, SC, WI … full inline)
  
  # --- NE
  # --- NE Recommendations (handles multi-line bullets)
  register_reco_algo("NE", function(pdf_path) {
    text <- read_pdf_pages(pdf_path, 1, 10)
    if (is.na(text) || nchar(text) < 20) return(NA_character_)
    text <- gsub("\r", "", text)
    text <- gsub("([A-Za-z])-\\s*\\n\\s*([A-Za-z])", "\\1\\2", text)
    
    block <- stringr::str_extract(
      text,
      "(?is)SUMMARY[\\s\\S]*?(?=PROGRAM\\s+OBJECTIVE|INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS|\\Z)"
    )
    if (is.na(block) || nchar(block) < 20) return(NA_character_)
    
    lines <- unlist(strsplit(block, "\\n"))
    lines <- stringr::str_trim(lines)
    lines <- lines[nchar(lines) > 0]
    
    items <- character(0)
    cur <- NULL
    for (ln in lines) {
      if (grepl("^\\*", ln)) {
        # new bullet starts
        if (!is.null(cur)) items <- c(items, cur)
        cur <- gsub("^\\*+\\s*", "", ln)
      } else if (!is.null(cur)) {
        # continuation of previous bullet
        cur <- paste(cur, ln)
      }
    }
    if (!is.null(cur)) items <- c(items, cur)
    
    items <- stringr::str_squish(items)
    if (length(items) == 0) NA_character_ else items
  })
  
  # --- VA Recommendations extractor (robust)
  register_reco_algo("VA", function(pdf_path){
    text <- read_pdf_pages(pdf_path, 1, 6)
    if (is.na(text) || nchar(text) < 20) return(NA_character_)
    
    # Try the clean anchor first
    block <- stringr::str_extract(
      text,
      "(?is)(NIOSH investigators[\\s\\S]*?)(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSION|\\Z))"
    )
    
    # Fallback to "employers should" if first fails
    if (is.na(block) || nchar(block) < 10) {
      block <- stringr::str_extract(
        text,
        "(?is)(employers should[\\s\\S]*?)(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSION|\\Z))"
      )
    }
    
    if (is.na(block) || nchar(block) < 10) return(NA_character_)
    
    # Normalize
    block <- gsub("\r", " ", block)
    lines <- unlist(strsplit(block, "\n"))
    lines <- stringr::str_trim(lines)
    lines <- lines[nchar(lines) > 0]
    
    # Drop lead-ins
    lines <- lines[!grepl("NIOSH investigators", lines, ignore.case = TRUE)]
    lines <- lines[!grepl("employers should", lines, ignore.case = TRUE)]
    
    # Keep recommendation lines
    recos <- lines[
      grepl("^(o\\s*|[-•*]\\s*|ensure|adhere|review|explain)", lines, ignore.case = TRUE)
    ]
    
    if (length(recos) == 0) return(NA_character_)
    return(recos)
  })
  
  
  # --- NJ
  register_reco_algo("NJ", function(pdf_path) {
    text <- read_pdf_pages(pdf_path,1,3)
    if (is.na(text)||nchar(text)<20) return(NA_character_)
    text <- gsub("\r","",text)
    text <- gsub("([A-Za-z])-\\s*\\n\\s*([A-Za-z])","\\1\\2",text)
    
    end_markers <- paste(c("INTRODUCTION\\b","BACKGROUND\\b","INVESTIGATION\\b","RESULTS?\\b","CONCLUSIONS?\\b",
                           "Page\\s*\\d+\\b","New\\s+Jersey","NJ\\s*FACE","APPENDIX\\b"),collapse="|")
    block <- stringr::str_extract(text,
                                  paste0("(?is)(?:NJ\\s*FACE\\s+)?investigators[\\s\\S]{0,200}?employers\\s+should\\s*:\\s*[\\s\\S]*?(?=\\n\\s*(?:",end_markers,")|\\z)"))
    if (is.na(block)) block <- stringr::str_extract(text,paste0("(?is)\\bRECOMMENDATIONS?\\b[\\s\\S]*?(?=\\n\\s*(?:",end_markers,")|\\z)"))
    if (is.na(block)) return(NA_character_)
    
    block <- stringr::str_replace(block,"(?is)^.*?(employers\\s+should|RECOMMENDATIONS?)\\s*:?","")
    block <- gsub("^\\s*·\\s+","• ",block,perl=TRUE)
    
    lines <- unlist(strsplit(block,"\\n"))
    lines <- stringr::str_trim(lines)
    lines <- lines[nchar(lines)>0]
    
    start_pat <- stringr::regex("^([\\x{2022}\\-*]|\\d+\\.|[A-Za-z]\\)|Employers\\s+should\\b|Ensure\\b|Provide\\b|Develop\\b|Train\\b|Implement\\b)",ignore_case=TRUE)
    
    items<-character(0);cur<-NULL
    for(ln in lines){
      if(stringr::str_detect(ln,start_pat)){
        if(!is.null(cur)) items<-c(items,cur)
        ln<-stringr::str_replace(ln,"^(?:[\\x{2022}\\-*]|\\d+\\.|[A-Za-z]\\))\\s*","")
        cur<-ln
      } else cur<-if(is.null(cur)) ln else paste(cur,ln)
    }
    if(!is.null(cur)) items<-c(items,cur)
    
    items<-stringr::str_squish(items)
    items<-items[nchar(items)>0]
    if(length(items)==0) NA_character_ else items
  })
  
  # --- MA
  # --- MA
  register_reco_algo("MA", function(pdf_path){
    text <- read_pdf_pages(pdf_path, 1, 2)
    if (is.na(text) || nchar(text) < 20) return(NA_character_)
    
    # --- Extract block starting at 'should:' up to INTRODUCTION
    block <- stringr::str_extract(
      text,
      "(?is)should:?[\\s\\S]*?(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSION|\\Z))"
    )
    if (is.na(block) || nchar(block) < 10) return(NA_character_)
    
    # --- Normalize line breaks
    block <- gsub("\r", "", block)
    
    # --- Capture different bullet types, including underscore
    recs <- unlist(c(
      stringr::str_extract_all(block, "(?m)^[_•\\-*]\\s*[^\\n]+")[[1]]
    ))
    
    # --- Cleanup bullets
    recs <- stringr::str_remove(recs, "^[ _•\\-*]+")
    recs <- stringr::str_squish(recs)
    recs <- recs[nchar(recs) > 0]
    
    if (length(recs) == 0) return(NA_character_)
    recs
  })
  
  
  # --- KY_v1
  register_reco_algo("KY_v1", function(pdf_path){
    text<-read_pdf_pages(pdf_path,1,3)
    block<-str_extract(text,"Recommendations for prevention:[\\s\\S]*?(?=\\n\\s*EMPLOYER|\\n\\s*APPENDIX|\\n\\s*$)")
    if(is.na(block)) return(NA_character_)
    txt<-str_squish(str_replace_all(block,"\n"," "))
    recs<-str_extract_all(txt,"\\s*[^]+")[[1]]
    recs<-str_remove(recs,"^\\s*")
    recs<-str_squish(str_trim(recs))
    recs[ nchar(recs)>0 ]
  })
  
  # --- KY_v2
  register_reco_algo("KY_v2", function(pdf_path){
    text<-read_pdf_pages(pdf_path,1,6)
    block<-stringr::str_extract(text,"(?is)(?:RECOMMENDATIONS?|In order to prevent[\\s\\S]{0,200}?recommend that:)\\s*[\\s\\S]*?(?=\\n\\s*(INTRODUCTION|APPENDIX|Employer|Key\\s*Words?)|\\z)")
    if(is.na(block)) return(NA_character_)
    block<-stringr::str_replace(block,"(?is)^.*?(RECOMMENDATIONS?|recommend that:)\\s*","")
    lines<-unlist(strsplit(block,"\\n"));lines<-stringr::str_trim(lines)
    start_pat<-stringr::regex("^(?:[\\x{2022}\\-*]|\\d+\\.|[A-Za-z]\\)|Employers\\s+should\\b|Ensure\\b|Provide\\b)",ignore_case=TRUE)
    items<-character(0);cur<-NULL
    for(ln in lines){ if(stringr::str_detect(ln,start_pat)){ if(!is.null(cur)) items<-c(items,cur); cur<-ln } else cur<-if(is.null(cur)) ln else paste(cur,ln)}
    if(!is.null(cur)) items<-c(items,cur)
    items<-stringr::str_squish(items)
    items[ nchar(items)>0 ]
  })
  
  # --- KY_v3
  register_reco_algo("KY_v3", function(pdf_path){
    text<-read_pdf_pages(pdf_path,1,3)
    block<-str_extract(text,"made:[\\s\\S]*?(?=\\n\\s*EMPLOYER|APPENDIX|$)")
    if(is.na(block)) return(NA_character_)
    items<-str_extract_all(block,"Recommendation\\s*No\\.\\s*\\d+:.*?(?=Recommendation|$)")[[1]]
    items<-str_remove(items,"^Recommendation\\s*No\\.\\s*\\d+:")
    items<-str_squish(str_trim(items))
    items
  })
  
  # --- KY_v4
  register_reco_algo("KY_v4", function(pdf_path){
    text<-read_pdf_pages(pdf_path,1,6)
    block<-str_extract(text,"(?is)(To prevent[\\s\\S]{0,200}recommend(s|ed):|RECOMMENDATIONS?:)\\s*[\\s\\S]*?(?=\\n\\s*(INTRODUCTION|APPENDIX|Employer|Key\\s*Words?)|\\z)")
    if(is.na(block)) return(NA_character_)
    block<-str_replace(block,"(?is)^.*?(recommend(s|ed):|RECOMMENDATIONS?:)\\s*","")
    lines<-unlist(strsplit(block,"\\n"));lines<-stringr::str_trim(lines)
    items<-lines[nchar(lines)>0]
    items
  })
  
  # --- KY_v5
  register_reco_algo("KY_v5", function(pdf_path){
    text<-read_pdf_pages(pdf_path,1,4)
    block<-str_extract(text,"(?is)(recommend that:|RECOMMENDATIONS?:)\\s*[\\s\\S]*?(?=\\n\\s*(INTRODUCTION|APPENDIX|BACKGROUND)|\\z)")
    if(is.na(block)) return(NA_character_)
    block<-str_replace(block,"(?is)^.*?(recommend that:|RECOMMENDATIONS?:)\\s*","")
    items<-unlist(strsplit(block,"•"))
    items<-stringr::str_squish(items)
    items[nchar(items)>0]
  })
  
  # --- IA
  register_reco_algo("IA", function(pdf_path){
    text<-read_pdf_pages(pdf_path,1,4)
    block<-str_extract(text,"RECOMMENDATIONS[\\s\\S]*?(?=INTRODUCTION|APPENDIX|$)")
    if(is.na(block)) block<-str_extract(text,"SUMMARY[\\s\\S]*?(?=INTRODUCTION|APPENDIX|$)")
    if(is.na(block)) return(NA_character_)
    recs<-str_extract_all(block,"\\d+\\.\\s[^\\d]+")[[1]]
    recs<-str_squish(str_trim(recs))
    recs
  })
  
  # --- OR
  register_reco_algo("OR", function(pdf_path){
    text<-read_pdf_pages(pdf_path,1,2)
    block<-str_extract(text,"RECOMMENDATIONS[\\s\\S]*?(?=INTRODUCTION|APPENDIX|$)")
    if(is.na(block)) return(NA_character_)
    recs<-c(str_extract_all(block,"\\s*[^]+")[[1]],str_extract_all(block,"•\\s*[^•]+")[[1]])
    recs<-str_remove(recs,"^\\s*");recs<-str_remove(recs,"^•\\s*")
    recs<-stringr::str_squish(str_trim(recs))
    recs[nchar(recs)>0]
  })
  
  # --- NY
  register_reco_algo("NY", function(pdf_path){
    text<-read_pdf_pages(pdf_path,1,3)
    block<-str_extract(text,"KEY RECOMMENDATIONS[\\s\\S]*?(?=INTRODUCTION|APPENDIX|$)")
    if(is.na(block)) block<-str_extract(text,"SUMMARY[\\s\\S]*?(?=INTRODUCTION|APPENDIX|$)")
    if(is.na(block)) return(NA_character_)
    recs<-c(str_extract_all(block,"•\\s*[^•]+")[[1]],str_extract_all(block,"\\s*[^]+")[[1]])
    recs<-str_remove(recs,"^•\\s*");recs<-str_remove(recs,"^\\s*")
    recs<-stringr::str_squish(str_trim(recs))
    recs[nchar(recs)>0]
  })
  
  # --- WV (unified for both WV and WV_v2)
  register_reco_algo("WV", function(pdf_path){
    text <- read_pdf_pages(pdf_path, 1, 10)
    
    # --- Extract the recommendations block ---
    block <- str_extract(
      text,
      "(?is)(?:(?:NIOSH investigators concluded|The WV FACE investigator concluded|The WV FACE Investigator concluded)[\\s\\S]*?)(?=(INTRODUCTION|BACKGROUND|CONCLUSION|\\Z))"
    )
    
    if (is.na(block)) {
      block <- str_extract(
        text,
        "(?is)(RECOMMENDATIONS[:\\s]*[\\s\\S]*?)(?=(INTRODUCTION|BACKGROUND|CONCLUSION|\\Z))"
      )
    }
    if (is.na(block)) return(NA_character_)
    
    # --- Clean header/lead-in text ---
    block <- str_replace(
      block,
      "(?is)^.*?(recommendations should be followed|RECOMMENDATIONS|concluded)[:\\s]*",
      ""
    )
    
    # --- Normalize bullet glyphs before splitting ---
    block <- str_replace_all(
      block,
      "[\u2022\u25CF\u25AA\u2219\u2043\u2023\uF0B7\u0095]|",
      "-"
    )
    
    # --- Split into lines ---
    lines <- unlist(strsplit(block, "\n"))
    lines <- str_trim(lines)
    lines <- lines[nchar(lines) > 0]
    
    # --- Parse bullets into items ---
    items <- character(0)
    cur <- NULL
    for (ln in lines) {
      if (grepl("^[-o•]", ln)) {
        # New bullet
        if (!is.null(cur)) items <- c(items, str_squish(cur))
        cur <- sub("^[-o•\\s]+", "", ln)
      } else if (!is.null(cur)) {
        # Continuation of previous bullet
        cur <- paste(cur, ln)
      }
    }
    if (!is.null(cur)) items <- c(items, str_squish(cur))
    
    # Final cleanup
    items <- str_squish(items)
    items[nchar(items) > 0]
  })
  
  
  # --- AK (fixed to stop at INTRODUCTION or BACKGROUND, expanded verbs)
  register_reco_algo("AK", function(pdf_path){
    text <- read_pdf_pages(pdf_path, 1, 3)
    if (is.na(text) || nchar(text) < 20) return(NA_character_)
    
    # Normalize but keep newlines
    text <- gsub("\r", "", text)
    text <- gsub("([A-Za-z])-\\s*\\n\\s*([A-Za-z])", "\\1\\2", text)
    
    # Cut text so it ends right before INTRODUCTION or BACKGROUND
    text <- str_split(text, "(?i)(INTRODUCTION|BACKGROUND)")[[1]][1]
    
    # Split into lines
    lines <- unlist(strsplit(text, "\n"))
    lines <- trimws(lines)
    
    recos <- character(0)
    cur <- NULL
    
    for (ln in lines) {
      # Expanded verbs for AL FACE reports
      if (grepl("^(Ensure|Maintain|Communicate|Reinforce|Provide|Train|Develop|Employ|Establish)\\b", ln, ignore.case = TRUE)) {
        if (!is.null(cur)) recos <- c(recos, str_squish(cur))
        cur <- ln
      } 
      # Catch lines under "Based on the findings ... employers should"
      else if (grepl("employers should", ln, ignore.case = TRUE)) {
        next  # skip the header line itself
      }
      else if (!is.null(cur) && nzchar(ln)) {
        cur <- paste(cur, ln)
      }
    }
    
    if (!is.null(cur)) recos <- c(recos, str_squish(cur))
    
    # Cleanup
    recos <- gsub("\\s*;?$", ";", recos)
    recos <- recos[nchar(recos) > 0]
    
    if (length(recos) == 0) NA_character_ else recos
  })
  
  
  
  
  # --- OK
  
  register_reco_algo("OK", function(pdf_path){
    text <- read_pdf_pages(pdf_path, 1, 6)
    if (is.na(text) || nchar(text) < 20) return(NA_character_)
    
    # Normalize line breaks
    text <- gsub("\r", "\n", text)
    
    # Step 1: slice between SUMMARY and INTRODUCTION
    block <- str_extract(text, "(?is)SUMMARY[\\s\\S]*?INTRODUCTION")
    if (is.na(block)) return(NA_character_)
    
    # Step 2: keep only part after 'employers should:'
    block <- str_extract(block, "(?is)employers should:?\\s*[\\s\\S]*")
    if (is.na(block)) return(NA_character_)
    
    # Step 3: split on bullet characters
    items <- unlist(strsplit(block, "•"))
    items <- trimws(items)
    items <- items[nchar(items) > 0]
    
    # Step 4: truncate each at first semicolon
    items <- sub("^(.*?;).*", "\\1", items)
    
    # Step 5: keep only lines starting with ensure/implement
    items <- items[grepl("^(ensure|implement)", items, ignore.case = TRUE)]
    
    if (length(items) == 0) NA_character_ else items
  })
  
  # --- TX
  register_reco_algo("TX", function(pdf_path){
    text<-read_pdf_pages(pdf_path,1,5)
    block<-str_extract(text,"SUMMARY[\\s\\S]*?(?=INTRODUCTION)")
    if(is.na(block)) return(NA_character_)
    block<-str_replace(block,"(?is)^.*?employers should:?","")
    recs<-str_extract_all(block,"•\\s*[^•]+")[[1]]
    recs<-str_remove(recs,"^•\\s*")
    recs<-stringr::str_squish(recs)
    recs[nchar(recs)>0]
  })
  
  # --- SC
  register_reco_algo("SC", function(pdf_path){
    text <- read_pdf_pages(pdf_path, 1, 6)
    if (is.na(text) || nchar(text) < 20) return(NA_character_)
    text <- gsub("\r", "", text)
    text <- gsub("([A-Za-z])-\\s*\\n\\s*([A-Za-z])", "\\1\\2", text)
    
    # Capture block starting at NIOSH investigators concluded up to INTRODUCTION
    block <- stringr::str_extract(
      text,
      "(?is)NIOSH investigators concluded[\\s\\S]*?(?=INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS|\\Z)"
    )
    if (is.na(block) || nchar(block) < 20) return(NA_character_)
    
    # Drop the leading "NIOSH investigators concluded..."
    block <- stringr::str_replace(block, "(?is)^.*?should:?\\s*", "")
    
    # Split on "o" bullets (may be indented)
    lines <- unlist(strsplit(block, "\\n"))
    lines <- stringr::str_trim(lines)
    items <- character(0)
    cur <- NULL
    for (ln in lines) {
      if (grepl("^o\\s+", ln)) {
        # start new reco
        if (!is.null(cur)) items <- c(items, cur)
        cur <- sub("^o\\s+", "", ln)
      } else if (!is.null(cur)) {
        # continuation of previous reco
        cur <- paste(cur, ln)
      }
    }
    if (!is.null(cur)) items <- c(items, cur)
    
    # Clean trailing "Additionally..." header if captured
    items <- items[!grepl("Additionally,\\s+prime\\s+contractors", items, ignore.case = TRUE)]
    
    items <- stringr::str_squish(items)
    if (length(items) == 0) NA_character_ else items
  })
  
  
  # --- WI
  register_reco_algo("WI", function(pdf_path){
    text<-read_pdf_pages(pdf_path,1,6)
    block<-str_extract(text,"(?is)(investigator concluded[\\s\\S]*?employers should:?|RECOMMENDATIONS:)[\\s\\S]*?(?=INTRODUCTION|$)")
    if(is.na(block)) return(NA_character_)
    block<-str_replace(block,"(?is)^.*?employers should:?","")
    items<-unlist(strsplit(block,"•"))
    items<-stringr::str_squish(items)
    items[nchar(items)>0]
  })
  
  # --- MN Recommendations
  register_reco_algo("MN", function(pdf_path) {
    text <- read_pdf_pages(pdf_path, 1, 10)
    if (is.na(text) || nchar(text) < 20) return(NA_character_)
    text <- gsub("\r", "", text)
    text <- gsub("([A-Za-z])-\\s*\\n\\s*([A-Za-z])", "\\1\\2", text)
    
    # Capture block between "MN FACE investigators concluded" and INTRODUCTION
    block <- stringr::str_extract(
      text,
      "(?is)MN\\s+FACE\\s+investigators\\s+concluded[\\s\\S]*?(?=INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS|\\Z)"
    )
    if (is.na(block) || nchar(block) < 20) return(NA_character_)
    
    lines <- unlist(strsplit(block, "\\n"))
    lines <- stringr::str_trim(lines)
    lines <- lines[nchar(lines) > 0]
    
    items <- character(0)
    cur <- NULL
    for (ln in lines) {
      if (grepl("^\\$", ln)) {
        # start of new bullet
        if (!is.null(cur)) items <- c(items, cur)
        cur <- gsub("^\\$+\\s*", "", ln)
      } else if (!is.null(cur)) {
        # continuation of previous bullet
        cur <- paste(cur, ln)
      }
    }
    if (!is.null(cur)) items <- c(items, cur)
    
    items <- stringr::str_squish(items)
    if (length(items) == 0) NA_character_ else items
  })
  
  # --- MD Recommendations
  # --- MD Recommendations (final robust)
  # --- MD Recommendations (fixed line wrap)
  register_reco_algo("MD", function(pdf_path){
    text <- read_pdf_pages(pdf_path, 1, 3)
    if (is.na(text) || nchar(text) < 20) return(NA_character_)
    
    # Extract block starting at "employers should:"
    block <- stringr::str_extract(
      text,
      "(?is)(employers\\s*should[:\\s\\-]*[\\s\\S]*?)(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSION|\\Z))"
    )
    if (is.na(block) || nchar(block) < 10) return(NA_character_)
    
    block <- gsub("\r", "", block)
    
    # Split into lines
    lines <- unlist(strsplit(block, "\n"))
    lines <- stringr::str_trim(lines)
    lines <- lines[nchar(lines) > 0]
    
    # Drop the lead-in text ("employers should:")
    lines <- lines[!grepl("employers should", lines, ignore.case = TRUE)]
    
    # Remove stray glyphs at the start of any line
    lines <- stringr::str_remove(lines, "^[\\p{So}\\p{Pd}•♦‚*_]+\\s*")
    
    # Collapse into items: start new item if line begins with an action verb, else append
    items <- character(0)
    cur <- NULL
    for (ln in lines) {
      if (grepl("^(Stress|Instruct|Assure|Ensure|Provide|Develop|Train)", ln, ignore.case = TRUE)) {
        if (!is.null(cur)) items <- c(items, stringr::str_squish(cur))
        cur <- ln
      } else if (!is.null(cur)) {
        cur <- paste(cur, ln)  # continuation line
      }
    }
    if (!is.null(cur)) items <- c(items, stringr::str_squish(cur))
    
    items <- stringr::str_squish(items)
    items <- items[nchar(items) > 0]
    if (length(items) == 0) return(NA_character_)
    items
  })
  
  # --- NC Recommendations
  register_reco_algo("NC", function(pdf_path) {
    text <- read_pdf_pages(pdf_path, 1, 3)
    if (is.na(text) || nchar(text) < 20) return(NA_character_)
    
    text <- gsub("\r", "", text)
    
    # --- Extract from "NIOSH investigators concluded" until INTRODUCTION (or next section)
    block <- stringr::str_extract(
      text,
      "(?is)NIOSH\\s+investigators\\s+concluded[\\s\\S]*?(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS?|\\Z))"
    )
    if (is.na(block)) return(NA_character_)
    
    # --- Remove the lead-in phrase
    block <- stringr::str_replace(
      block,
      "(?is)^.*?(employers\\s+should\\s*:?)",
      ""
    )
    
    # --- Normalize bullets (•, !, -, etc.)
    block <- gsub("(?m)^\\s*[!·•\\-*]+\\s*", "• ", block, perl = TRUE)
    
    # --- Split into lines
    lines <- unlist(strsplit(block, "\\n"))
    lines <- stringr::str_trim(lines)
    lines <- lines[nchar(lines) > 0]
    
    # --- Group lines into recommendations
    items <- character(0); cur <- NULL
    for (ln in lines) {
      if (stringr::str_detect(ln, "^•\\s*")) {
        if (!is.null(cur)) items <- c(items, cur)
        cur <- stringr::str_replace(ln, "^•\\s*", "")
      } else if (grepl("Additionally", ln, ignore.case = TRUE)) {
        # skip the "Additionally," filler
        next
      } else {
        cur <- if (is.null(cur)) ln else paste(cur, ln)
      }
    }
    if (!is.null(cur)) items <- c(items, cur)
    
    items <- stringr::str_squish(items)
    items <- items[nchar(items) > 0]
    
    if (length(items) == 0) NA_character_ else items
  })
  
  
  # --- IN Recommendations extractor
  register_reco_algo("IN", function(pdf_path){
    text <- read_pdf_pages(pdf_path, 1, 5)  # adjust pages if needed
    if (is.na(text) || nchar(text) < 20) return(NA_character_)
    
    # Extract block between FACE investigator → INVESTIGATION (or similar stop)
    block <- stringr::str_extract(
      text,
      "(?is)(FACE investigator concluded[\\s\\S]*?)(?=(INVESTIGATION|BACKGROUND|CONCLUSION|\\Z))"
    )
    if (is.na(block) || nchar(block) < 10) return(NA_character_)
    
    # Normalize
    block <- gsub("\r", " ", block)
    
    # Split into lines
    lines <- unlist(strsplit(block, "\n"))
    lines <- stringr::str_trim(lines)
    lines <- lines[nchar(lines) > 0]
    
    # Drop lead-in
    lines <- lines[!grepl("FACE investigator concluded", lines, ignore.case = TRUE)]
    lines <- lines[!grepl("employers and employees", lines, ignore.case = TRUE)]
    
    # Extract numbered recommendations
    recos <- lines[grepl("^\\s*\\d+\\.", lines)]
    
    # Ensure they’re cleaned up
    recos <- stringr::str_squish(recos)
    
    if (length(recos) == 0) return(NA_character_)
    recos
  })
  
  
  # --- MO Recommendations (fixed to return multiple items)
  register_reco_algo("MO", function(pdf_path) {
    text <- read_pdf_pages(pdf_path, 1, 3)
    if (is.na(text) || nchar(text) < 20) return(NA_character_)
    
    text <- gsub("\r", "", text)
    
    # --- Extract block beginning with "The MO FACE investigator concluded"
    block <- stringr::str_extract(
      text,
      "(?is)The\\s+MO\\s+FACE\\s+investigator\\s+concluded[\\s\\S]*?(?=(INTRODUCTION|BACKGROUND|INVESTIGATION|CONCLUSIONS?|\\Z))"
    )
    if (is.na(block)) return(NA_character_)
    
    # --- Strip the lead-in phrase
    block <- stringr::str_replace(block, "(?is)^.*?(all employers should\\s*:?)", "")
    
    # --- Normalize bullets: convert "!" or "•" or "-" at start of line to "•"
    block <- gsub("(?m)^\\s*[!·•\\-*]+\\s*", "• ", block, perl = TRUE)
    
    # --- Split into lines
    lines <- unlist(strsplit(block, "\\n"))
    lines <- stringr::str_trim(lines)
    lines <- lines[nchar(lines) > 0]
    
    # --- Rebuild items grouped by bullet
    items <- character(0)
    cur <- NULL
    for (ln in lines) {
      if (stringr::str_detect(ln, "^•\\s*")) {
        if (!is.null(cur)) items <- c(items, cur)
        cur <- stringr::str_replace(ln, "^•\\s*", "")
      } else {
        cur <- if (is.null(cur)) ln else paste(cur, ln)
      }
    }
    if (!is.null(cur)) items <- c(items, cur)
    
    items <- stringr::str_squish(items)
    items <- items[nchar(items) > 0]
    
    if (length(items) == 0) NA_character_ else items
  })
  
  # CO Recommendations
  register_reco_algo("CO", function(pdf_path) {
    text <- read_pdf_pages(pdf_path, 1, 10)
    if (is.na(text) || nchar(text) < 20) return(NA_character_)
    text <- gsub("\r", "", text)
    text <- gsub("([A-Za-z])-\\s*\\n\\s*([A-Za-z])", "\\1\\2", text)
    
    # Capture block between "SUMMARY" and "INVESTIGATION"
    block <- stringr::str_extract(
      text,
      "(?is)SUMMARY[\\s\\S]*?(?=INVESTIGATION|CONCLUSIONS|\\Z)"
    )
    if (is.na(block) || nchar(block) < 20) return(NA_character_)
    
    lines <- unlist(strsplit(block, "\\n"))
    lines <- stringr::str_trim(lines)
    lines <- lines[nchar(lines) > 0]
    
    items <- character(0)
    cur <- NULL
    for (ln in lines) {
      if (grepl("^!", ln)) {
        # start of new bullet
        if (!is.null(cur)) items <- c(items, cur)
        cur <- gsub("^!\\s*", "", ln)
      } else if (!is.null(cur)) {
        # continuation of previous bullet
        cur <- paste(cur, ln)
      }
    }
    if (!is.null(cur)) items <- c(items, cur)
    
    items <- stringr::str_squish(items)
    if (length(items) == 0) NA_character_ else items
  })
  
  
  
  # --------------------------------------------------------------------
  # State codes list (all you provided)
  # --------------------------------------------------------------------
  state_codes <- c("WV","MA","CA","OR","OR_v2","NJ","NY","MO","IA","IN",
                   "KY_v1","KY_v2","KY_v3","KY_v4","KY_v5",
                   "MI","MI_v1","MI_v2","MI_v3","MI_v4","MD","MN",
                   "NC","WA","WI","AK","OK","NE","TX","SC","VA","CO","WY")
  state_codes <- sort(state_codes)
  state_choices <- c("Select State Code" = "", state_codes)
  
  # --------------------------------------------------------------------
  # Shiny UI
  # --------------------------------------------------------------------
  # --------------------------------------------------------------------
  # Shiny UI (rewritten for cleaner layout)
  # --------------------------------------------------------------------
  ui <- fluidPage(
    
    tags$style(HTML("
  
    textarea {
      font-family: monospace;
      white-space: pre-wrap;
    }
  
    #summary_input, #cf_input, #recs_input {
      width: 100% !important;
      max-width: 600px !important;
    }
  
    #extracted_text {
      max-width: 700px !important;
      white-space: pre-wrap;
      overflow-x: auto;
    }
  
    /* ================================
       ✅ FIX: ONLY highlight CELL
       ================================ */
  
    table.dataTable tbody tr.selected,
    table.dataTable tbody tr.selected td {
      background-color: transparent !important;
      color: inherit !important;
    }
  
    table.dataTable tbody td.selected {
      background-color: #E6F2FF !important;
      color: #000 !important;
    }
  
    table.dataTable tbody tr:hover,
    table.dataTable tbody tr:hover td {
      background-color: #F5F9FF !important;
      color: #000000 !important;
    }
  
    table.dataTable tbody td input {
      color: #000000 !important;
      background-color: #FFFFFF !important;
    }
  
    /* ======================================
       ⭐ HEADERS: DO NOT WRAP (VERY IMPORTANT)
       ====================================== */
  
    table.dataTable thead th {
      white-space: nowrap !important;
      word-break: normal !important;
      overflow: hidden !important;
      text-overflow: ellipsis !important;
    }
  
    /* ======================================
       ⭐ BODY CELLS: WRAP NICELY
       ====================================== */
  
    table.dataTable tbody td {
      white-space: normal !important;
      word-break: break-word !important;
      vertical-align: top !important;
    }
  "))
    ,
    
    useShinyjs(),
    titlePanel("Multi-State Construction Accident Analysis Tool"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput("pdf_file", "Upload FACE PDF", accept = ".pdf"),
        fileInput("csv_file", "Upload Extracted CSV",
                  accept = c(".csv")),
        # sliderInput("year_range", "Select Year Range:",
        #             min = 1980, max = 2025,
        #             value = c(2000, 2020), step = 1, sep = ""),
        
        
        fileInput("summary_image", "Upload Summary Screenshot",
                  accept = c(".png", ".jpg", ".jpeg")),
        
        selectInput("state_code", "Select State Code", choices = state_choices),
        fileInput("recs_image", "Upload Recommendations Screenshot", 
                  accept = c(".png", ".jpg", ".jpeg")),
        fileInput(
          "investigation_image",
          "Upload Investigation Screenshot",
          accept = c(".png", ".jpg", ".jpeg"),
          multiple = TRUE
        ),
        
        numericInput("start_page", "Start Page", value = 1, min = 1, step = 1),
        numericInput("end_page", "End Page", value = 10, min = 1, step = 1),
        
        actionButton("reiterate_summary", "Reiterate Summary"),
        conditionalPanel(
          condition = "input.reiterate_summary > 0",
          textAreaInput("summary_input", "Paste Summary (one block)",
                        rows = 5,
                        placeholder = "e.g.\nA 30-year-old construction worker fell 30 feet from an unguarded catwalk while installing a billboard sign and died at the scene.")
        ),
        
        
        # --- Contributing Factors Input ---
        textAreaInput(
          "cf_input", "Paste or Copy Investigation Text",
          rows = 5,
          placeholder = "Paste any part of the investigation text here..."
        ),
        
        # --- Gemini Button ---
        actionButton("gemini_cf_btn", "Find Contributing Factors with Gemini"),
        
        br(),
        tags$hr(),
        
        actionButton("reiterate_recs", "Reiterate Recommendations"),
        conditionalPanel(
          condition = "input.reiterate_recs > 0",
          textAreaInput("recs_input", "Paste Recommendations (one per line)",
                        rows = 5,
                        placeholder = "e.g.\nEmployers must ensure fall protection\nInspect equipment regularly")
        ),
        
        actionButton("reiterate_date_age", "Reiterate Date & Age"),
        conditionalPanel(
          condition = "input.reiterate_date_age > 0",
          fluidRow(
            column(
              4,
              selectInput(
                "manual_month",
                "Month",
                choices = month.name,
                selected = "January"
              )
            ),
            
            
            column(4, numericInput("manual_day", "Day", value = NA, min = 1, max = 31)),
            column(4, numericInput("manual_year", "Year", value = NA, min = 1900, max = 2100))
          ),
          numericInput("manual_age", "Age", value = NA, min = 1, max = 120)
        ),
        
        actionButton("analyze", "Extract"),
        downloadButton("download_csv", "Download CSV")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("PDF Preview", uiOutput("pdf_viewer")),
          # tabPanel("Uploaded Data (CSV)",
          #          DT::dataTableOutput("csv_out")
          # ),
          tabPanel("Extracted Text", verbatimTextOutput("extracted_text")),
          tabPanel("Summary", tableOutput("summary_out")),
          tabPanel("Recommendations", tableOutput("recs_out")),
          tabPanel("Contributing Factors Extraction Process Logic",
                   verbatimTextOutput("step_by_step_extraction")
          ),
          
          tabPanel("Contributing Factors",
                   fluidRow(
                     column(12,
                            actionButton("cf_filter_btn", "✅ Filter factors (post-filter)"),
                            actionButton("cf_filter_reset", "↩ Reset (show raw)"),
                            tags$span(style="padding-left:12px;"),
                            actionButton("cf_remove_btn", "🗑 Remove selected"),
                            actionButton("cf_undo_btn", "↩ Undo remove"),
                            br(), br(),
                            DT::dataTableOutput("cf_dt")
                     )
                   )
          ),
          
          
          tabPanel("Validation",
                   sidebarLayout(
                     sidebarPanel(
                       textAreaInput("text_investigation", "Investigation Text", 
                                     value = "", height = "200px"),
                       textAreaInput("text_gemini", "Gemini Contributing Factors", 
                                     value = "", height = "200px"),
                       actionButton("validate_btn", "Validate Factors")
                     ),
                     mainPanel(
                       h4("Validation Results"),
                       DT::dataTableOutput("validation_table"),
                       h4("Highlighted Text"),
                       htmlOutput("highlighted_text")
                     )
                   )
          ),
          
          
          
          tabPanel("Final Dataframe", tableOutput("df_out"))
          
          
          
          
        )
      )
    )
  )
  
  # --------------------------------------------------------------------
  # Shiny Server
  # --------------------------------------------------------------------
  server <- function(input, output, session) {
    results <- reactiveVal(NULL)
    extracted_text <- reactiveVal("")
    # --- helper: update CF column in final dataframe ---
    update_results_cf_column <- function(new_cf) {
      if (is.null(results())) return()
      
      df <- results()
      
      # pad to existing row count
      max_length <- max(nrow(df), length(new_cf), 1)
      df <- df[seq_len(max_length), , drop = FALSE]
      
      df$Contributing_Factors <- c(new_cf, rep("", max_length - length(new_cf)))
      
      results(df)
    }
    
    observeEvent(input$pdf_file, {
      results(NULL)
      extracted_text("")
      gemini_cf("")
      gemini_cf_used(FALSE)
    })
    
    
    read_auto_csv <- function(file) {
      first_line <- readLines(file, n = 1, warn = FALSE)
      
      # Detect delimiter
      if (grepl("\t", first_line)) {
        delim <- "\t"
      } else if (grepl(";", first_line)) {
        delim <- ";"
      } else {
        delim <- ","
      }
      
      # Read everything as character first
      df <- read.csv(file, sep = delim, header = TRUE, stringsAsFactors = FALSE, colClasses = "character")
      
      return(df)
    }
    
    csv_data <- reactiveVal(NULL)
    
    observeEvent(input$csv_file, {
      req(input$csv_file)
      df <- tryCatch(read_auto_csv(input$csv_file$datapath),
                     error = function(e) NULL)
      csv_data(df)
    })
    
    output$csv_out <- DT::renderDataTable({
      req(csv_data())
      df2 <- csv_data()
      
      DT::datatable(
        df2,
        rownames  = FALSE,
        selection = list(mode = "single", target = "cell"),
        options   = list(
          pageLength = 20,
          lengthMenu = c(20, 50, 100),
          scrollX = TRUE,
          autoWidth = FALSE,
          columnDefs = list(
            list(width = "320px", targets = 0),  # Summary
            list(width = "90px",  targets = 1),  # Report_Number
            list(width = "280px", targets = 2),  # Contributing_Factors
            list(width = "360px", targets = 3),  # Recommendations
            list(width = "90px",  targets = 4),  # Month
            list(width = "60px",  targets = 5),  # Day
            list(width = "70px",  targets = 6),  # Year
            list(width = "60px",  targets = 7)   # Age
          )
        )
      )
    })
    
    
    
    
    gemini_cf <- reactiveVal("")
    gemini_prompt <- reactiveVal("")
    gemini_cf_used <- reactiveVal(FALSE)
    cf_view_text <- reactiveVal("")
    cf_is_filtered <- reactiveVal(FALSE)
    cf_items <- reactiveVal(character(0))     # current CF list shown in table
    cf_raw_items <- reactiveVal(character(0)) # raw (unfiltered) baseline for reset
    cf_removed_stack <- reactiveVal(list())   # for undo (stores removed rows)
    
    # ✅ PUT YOUR REMOVE observeEvent RIGHT HERE
    observeEvent(input$cf_remove_btn, {
      req(input$cf_dt_rows_selected)
      
      current <- cf_items()
      sel <- input$cf_dt_rows_selected
      sel <- sel[sel >= 1 & sel <= length(current)]
      if (length(sel) == 0) return()
      
      removed <- current[sel]
      kept <- current[-sel]
      
      st <- cf_removed_stack()
      st[[length(st) + 1]] <- list(removed = removed, idx = sel)
      cf_removed_stack(st)
      
      cf_items(kept)
      cf_view_text(paste(kept, collapse = "\n"))
      
      update_results_cf_column(kept)
      
      showNotification(sprintf("Removed %d factor(s).", length(removed)), type = "message")
    })
    
    observeEvent(input$cf_undo_btn, {
      st <- cf_removed_stack()
      if (length(st) == 0) {
        showNotification("Nothing to undo.", type = "warning")
        return()
      }
      
      last <- st[[length(st)]]
      st <- st[-length(st)]
      cf_removed_stack(st)
      
      current <- cf_items()
      restored <- c(current, last$removed)
      
      cf_items(restored)
      cf_view_text(paste(restored, collapse = "\n"))
      
      update_results_cf_column(restored)
      
      showNotification(sprintf("Restored %d factor(s).", length(last$removed)), type = "message")
    })
    
    observeEvent(input$cf_dt_cell_edit, {
      info <- input$cf_dt_cell_edit
      i <- info$row
      j <- info$col
      v <- info$value
      
      # Only one column exists, but keep it general
      current <- cf_items()
      if (length(current) == 0) return()
      if (i < 1 || i > length(current)) return()
      
      v <- trimws(v)
      
      # Option A: allow blank edits = delete row
      if (!nzchar(v)) {
        removed <- current[i]
        kept <- current[-i]
        
        st <- cf_removed_stack()
        st[[length(st) + 1]] <- list(removed = removed, idx = i)
        cf_removed_stack(st)
        
        cf_items(kept)
        cf_view_text(paste(kept, collapse = "\n"))
        update_results_cf_column(kept)
        
        showNotification("Row cleared → removed (undo available).", type = "message")
        return()
      }
      
      # Normal edit
      current[i] <- v
      cf_items(current)
      cf_view_text(paste(current, collapse = "\n"))
      update_results_cf_column(current)
      
      showNotification("Edited contributing factor saved.", type = "message")
    })
    
    # --- helper: sync cf_view_text() -> cf_items() ---
    sync_cf_items_from_view <- function() {
      txt <- cf_view_text()
      lines <- trimws(unlist(strsplit(txt, "\n", fixed = TRUE)))
      lines <- lines[nzchar(lines)]
      cf_items(lines)
      if (length(cf_raw_items()) == 0) cf_raw_items(lines)  # set baseline once
    }
    
    observeEvent(input$cf_input, {
      gemini_cf_used(FALSE)
      gemini_cf("")
    }, ignoreInit = TRUE)
    
    observeEvent(gemini_cf(), {
      # whenever new Gemini result arrives, default CF tab to raw text
      cf_view_text(gemini_cf())
      cf_is_filtered(FALSE)
    }, ignoreInit = TRUE)
    
    # ---------------------------------------------------
    # ---------------------------------------------------
    # CF post-filter buttons
    # ---------------------------------------------------
    observeEvent(input$cf_filter_btn, {
      txt <- cf_view_text()
      if (!nzchar(txt) && !is.null(results())) txt <- paste(results()$Contributing_Factors, collapse = "\n")
      if (!nzchar(txt)) txt <- input$cf_input
      
      if (!nzchar(txt)) {
        showNotification("Nothing to filter yet. Paste CF text or run Gemini.", type = "warning")
        return()
      }
      
      lines <- trimws(unlist(strsplit(txt, "\n", fixed = TRUE)))
      lines <- lines[nzchar(lines)]
      
      lines <- reconstruct_sentences(lines)
      filtered <- filter_cf_lines(lines)
      
      cf_view_text(paste(filtered, collapse = "\n"))
      cf_is_filtered(TRUE)
      
      showNotification(sprintf("Filtered: %d → %d lines", length(lines), length(filtered)),
                       type = "message")
    })
    
    
    
    # Reactive to hold filtered data from DT
    filtered_data <- reactive({
      req(csv_data())
      DT::datatable(csv_data())  # preview
      csv_data()
    })
    
    
    
    
    
    
    
    observeEvent(csv_data(), {
      df <- csv_data()
      if ("Year" %in% names(df)) {
        min_year <- min(as.numeric(df$Year), na.rm = TRUE)
        max_year <- max(as.numeric(df$Year), na.rm = TRUE)
        updateSliderInput(session, "year_range",
                          min = min_year, max = max_year,
                          value = c(min_year, max_year))
      }
    })
    
    
    output$age_factor_year_plot <- renderPlotly({
      df <- filtered_by_year()
      req(df)
      
      # Clean Age
      df$Age <- suppressWarnings(as.numeric(df$Age))
      df <- df %>% filter(!is.na(Age))
      
      # Categorize contributing factors
      df <- df %>%
        mutate(Categorized = mapply(
          categorize_factor,
          Contributing_Factors,
          Summary
        ))
      
      # Summarize by Year, Age, and Category
      df_summary <- df %>%
        group_by(Year, Age, Categorized) %>%
        summarise(Count = n(), .groups = "drop")
      
      # Plot with animation
      plot_ly(
        df_summary,
        x = ~Age,
        y = ~Count,
        color = ~Categorized,
        type = "bar",
        frame = ~Year
      ) %>%
        layout(
          barmode = "stack",
          title = "Age vs Contributing Factors Over the Years",
          xaxis = list(title = "Age"),
          yaxis = list(title = "Count")
        ) %>%
        animation_opts(
          frame = 1000,      # time per frame in ms
          transition = 500,  # transition duration
          redraw = TRUE
        )
      
    })
    
    
    
    
    
    output$pdf_viewer <- renderUI({
      req(input$pdf_file)
      
      tmp_dir <- file.path(tempdir(), "pdfs")
      dir.create(tmp_dir, showWarnings = FALSE)
      pdf_copy_path <- file.path(tmp_dir, basename(input$pdf_file$name))
      file.copy(input$pdf_file$datapath, pdf_copy_path, overwrite = TRUE)
      
      shiny::addResourcePath("pdfs", tmp_dir)
      pdf_url <- paste0("pdfs/", basename(input$pdf_file$name))
      
      # Add an identifiable div around the PDF
      div(
        id = "pdf_container",
        tags$iframe(src = pdf_url, width = "100%", height = "800px", style = "border:none;")
      )
    })
    
    
    
    
    
    # Rendering the Step-by-Step Process in the UI
    output$step_by_step_extraction <- renderText({
      req(input$cf_input)
      
      cleaned_text <- clean_input_text(input$cf_input)
      prompt <- create_gemini_prompt(cleaned_text)
      
      # store prompt for display (optional)
      gemini_prompt(prompt)
      
      paste0(
        "Step 1: Cleaned Text\n", cleaned_text, "\n\n",
        "Step 2: Formulated Gemini Query\n", prompt, "\n\n",
        "Step 3: Extracted Contributing Factors\n", gemini_cf()
      )
    })
    
    
    
    
    
    
    
    # Screenshot_Image_To_Text_With_gemini
    
    
    
    
    
    
    
    # --------------------------------------------------------------------
    # --------------------------------------------------------------------
    # Gemini Action Button (Extract Contributing Factors Independently)
    # --------------------------------------------------------------------
    busy <- reactiveVal(FALSE)
    
    observeEvent(input$gemini_cf_btn, {
      req(input$cf_input)
      if (busy()) return()
      busy(TRUE)
      on.exit(busy(FALSE), add = TRUE)
      
      showNotification("Analyzing text with Gemini...", type = "message")
      gemini_cf_used(TRUE)
      cleaned_text <- clean_input_text(input$cf_input)
      result <- summarize_cf_with_gemini(cleaned_text)
      
      if (!nzchar(result)) {
        showNotification("Gemini returned no result.", type = "error")
        gemini_cf("")
        return()
      }
      
      # Clean formatting
      result_clean <- gsub("^[\\*•\\-\\s]+", "", result)
      result_clean <- gsub("\\r", "\n", result_clean)
      gemini_cf(result_clean)          # keep RAW Gemini result
      cf_view_text(result_clean)       # show RAW in table by default
      cf_is_filtered(FALSE)
      sync_cf_items_from_view()
      
      
      # ⚠️ Optional: if you still want to overwrite the input box, keep it.
      # But it will cause re-renders. It's safe now because renderText no longer calls Gemini.
      
      
      
      showNotification("✅ Contributing factors extracted.", type = "message")
    })
    
    
    
    
    
    
    observeEvent(input$capture_pdf, {
      req(input$capture_pdf)
      
      # Get the coordinates for the selected area
      x <- as.numeric(input$capture_pdf$x)
      y <- as.numeric(input$capture_pdf$y)
      width <- as.numeric(input$capture_pdf$width)
      height <- as.numeric(input$capture_pdf$height)
      
      # Use html2canvas to capture the selected area (this can be triggered via custom JS)
      session$sendCustomMessage("capture_pdf", list(x = x, y = y, width = width, height = height))
    })
    
    
    
    
    
    
    
    
    
    observeEvent(input$analyze, {
      req(input$pdf_file)
      
      # 🔑 Clear old results immediately
      results(NULL)
      extracted_text("")
      
      pdf_path <- input$pdf_file$datapath
      
      all_pages <- pdftools::pdf_text(pdf_path)
      start <- max(1, input$start_page)
      end   <- min(input$end_page, length(all_pages))
      page_text <- paste(all_pages[start:end], collapse = "\n")
      extracted_text(page_text)
      
      # --- Contributing factors (independent of state) ---
      # --- Contributing factors ---
      contributing_factors <- character(0)
      # Contributing factors source (no reiterate_investigation needed)
      
      # 1) Prefer the current CF view (Gemini result / filtered / edited)
      txt_cf <- cf_view_text()
      
      # 2) Fallback to raw OCR / pasted investigation text box
      if (!nzchar(txt_cf)) {
        txt_cf <- input$cf_input
      }
      
      # Final safety
      if (is.null(txt_cf) || !nzchar(txt_cf)) txt_cf <- ""
      
      
      # Split into contributing factor rows
      contributing_factors <- unlist(strsplit(txt_cf, "\n", fixed = TRUE))
      contributing_factors <- trimws(contributing_factors)
      contributing_factors <- contributing_factors[nzchar(contributing_factors)]
      
      
      # Keep UI view consistent after Extract
      cf_view_text(paste(contributing_factors, collapse = "\n"))
      cf_is_filtered(FALSE)
      
      # --- Initialize defaults for other fields ---
      summary_result <- ""
      recommendations <- ""
      month <- ""
      day <- ""
      year <- ""
      age <- ""
      
      # --- Only if state code is provided ---
      if (nzchar(input$state_code)) {
        state_code <- input$state_code
        
        # --- Summary ---
        if (input$reiterate_summary > 0 && !is.null(input$summary_input) && nzchar(input$summary_input)) {
          summary_result <- input$summary_input
        } else {
          summary_result <- tryCatch(extract_summary(state_code, pdf_path), error = function(e) NA)
        }
        if (is.null(summary_result) || all(is.na(summary_result))) summary_result <- ""
        
        # --- Recommendations ---
        if (input$reiterate_recs > 0 && !is.null(input$recs_input) && nzchar(input$recs_input)) {
          recommendations <- clean_input_lines(input$recs_input)
        } else {
          recommendations <- tryCatch(extract_recommendations(state_code, pdf_path),
                                      error = function(e) NA)
        }
        if (is.null(recommendations) || all(is.na(recommendations))) recommendations <- ""
        
        # --- Date & Age ---
        if (input$reiterate_date_age > 0) {
          month <- if (!is.null(input$manual_month) && nzchar(input$manual_month)) input$manual_month else ""
          day   <- if (!is.null(input$manual_day) && !is.na(input$manual_day)) input$manual_day else ""
          year  <- if (!is.null(input$manual_year) && !is.na(input$manual_year)) input$manual_year else ""
          age   <- if (!is.null(input$manual_age) && !is.na(input$manual_age)) input$manual_age else ""
        } else {
          res <- tryCatch(extract_report_date(pdf_path), error = function(e) NULL)
          day   <- if (!is.null(res$day)   && !is.na(res$day))   res$day   else ""
          month <- if (!is.null(res$month_name) && !is.na(res$month_name)) res$month_name else ""
          year  <- if (!is.null(res$year)  && !is.na(res$year))  res$year  else ""
          age   <- tryCatch({
            a <- extract_age(pdf_path)$age
            if (!is.null(a) && !is.na(a)) a else ""
          }, error = function(e) "")
        }
      }
      
      # --- Report number from file name ---
      report_number <- str_extract(input$pdf_file$name, "\\d+(?=\\.pdf)")
      if (is.na(report_number) | !nzchar(report_number)) report_number <- "Unknown"
      
      # --- Build unified dataframe (always) ---
      max_length <- max(length(summary_result),
                        length(contributing_factors),
                        length(recommendations), 1)
      
      df <- data.frame(
        Summary             = rep(summary_result, length.out = max_length),
        Report_Number       = rep(report_number, length.out = max_length),
        Contributing_Factors= pad_to_max_length(contributing_factors, max_length),
        Recommendations     = pad_to_max_length(recommendations, max_length),
        Month               = rep(month, length.out = max_length),
        Day                 = rep(day, length.out = max_length),
        Year                = rep(year, length.out = max_length),
        Age                 = rep(age, length.out = max_length),
        stringsAsFactors    = FALSE
      )
      
      df$Day <- suppressWarnings(as.integer(df$Day))
      df$Year <- suppressWarnings(as.integer(df$Year))
      df$Age <- suppressWarnings(as.integer(df$Age))
      
      
      
      
      
      # ✅ Always save results — even without state
      results(df)
    })
    
    
    
    observeEvent(input$recs_image, {
      req(input$recs_image)
      
      # Loop through all uploaded files and OCR each
      txts <- lapply(input$recs_image$datapath, function(path) {
        tesseract::ocr(path, engine = tesseract("eng"))
      })
      
      # Collapse into one string with line breaks
      combined <- paste(unlist(txts), collapse = "\n")
      
      updateTextAreaInput(session, "recs_input", value = combined)
      showNotification("OCR complete: Combined recommendations pasted.", type = "message")
    })
    observeEvent(input$summary_image, {
      req(input$summary_image)
      
      # Loop through all uploaded files and OCR each
      txts <- lapply(input$summary_image$datapath, function(path) {
        tesseract::ocr(path, engine = tesseract("eng"))
      })
      
      # Collapse into one string with line breaks
      combined <- paste(unlist(txts), collapse = "\n")
      
      updateTextAreaInput(session, "summary_input", value = combined)
      showNotification("OCR complete: Summary pasted.", type = "message")
    })
    
    # Tabs
    output$extracted_text <- renderText({ extracted_text() })
    
    output$summary_out <- renderTable({
      if (is.null(results())) return(data.frame(Message = "No state selected, so summary not extracted."))
      data.frame(Summary = results()$Summary[1])
    })
    
    observeEvent(input$investigation_image, {
      req(input$investigation_image)
      
      # OCR each uploaded screenshot
      txts <- lapply(input$investigation_image$datapath, function(path) {
        tesseract::ocr(path, engine = tesseract("eng"))
      })
      
      combined <- paste(unlist(txts), collapse = "\n")
      
      # Put OCR text into the Investigation box used for CF extraction
      updateTextAreaInput(session, "cf_input", value = combined)
      
      # Optional: also populate the Validation tab Investigation Text box
      updateTextAreaInput(session, "text_investigation", value = combined)
      
      showNotification("OCR complete: Investigation text pasted.", type = "message")
    })
    
    
    output$recs_out <- renderTable({
      if (is.null(results())) return(data.frame(Message = "No state selected, so recommendations not extracted."))
      data.frame(Recommendation = results()$Recommendations) %>% filter(Recommendation != "")
    })
    
    output$cf_dt <- DT::renderDataTable({
      x <- cf_items()
      if (length(x) == 0) {
        return(DT::datatable(
          data.frame(Contributing_Factor = character(0)),
          selection = "multiple",
          editable = list(target = "cell", disable = list(columns = NULL)),
          rownames = FALSE,
          options = list(pageLength = 15)
        ))
      }
      
      DT::datatable(
        data.frame(Contributing_Factor = x, stringsAsFactors = FALSE),
        selection = "multiple",
        editable = list(target = "cell"),   # ✅ enables inline edits
        rownames = FALSE,
        options = list(pageLength = 15)
      )
    })
    
    
    
    output$df_out <- renderTable({ results() })
    
    output$download_csv <- downloadHandler(
      filename = function() { 
        paste0(tools::file_path_sans_ext(input$pdf_file$name), ".csv") 
      },
      content = function(file) { 
        if (!is.null(results())) {
          write.csv(results(), file, row.names = FALSE)
          
          # After writing, reset inputs
          session$sendCustomMessage("resetInputs", list())
          
          # 🔑 Clear reactive values too
          results(NULL)
          extracted_text("")
        }
      }
      
      
    )
    observeEvent(input$validate_btn, {
      req(input$text_investigation, input$text_gemini)
      
      # --- Normalize function (removes punctuation, case differences) ---
      normalize <- function(x) {
        cleaned <- x |>
          tolower() |>
          stringi::stri_trans_general("Latin-ASCII") |>
          stringr::str_replace_all("’|‘", "'") |>
          stringr::str_replace_all("[“”]", "\"") |>
          stringr::str_replace_all("–|—", "-") |>
          stringr::str_replace_all("[\r\n]+", " ") |>
          stringr::str_replace_all("[[:punct:]]", " ") |>
          stringr::str_replace_all("\\s+", " ") |>
          stringr::str_squish() |>
          stringr::str_replace_all("[^[:print:]]", " ")
        
        cat("Cleaned Text: \n", cleaned, "\n")
        return(cleaned)
      }
      
      # --- Fuzzy matching helper for validation ---
      is_found <- function(factor, text, threshold = 0.6) {  # Lower threshold
        f <- normalize(factor)
        t <- normalize(text)
        sim <- stringdist::stringsim(f, t, method = "jw")
        cat("Similarity for factor:", factor, ": ", sim, "\n")
        return(sim > threshold)
      }
      
      # --- Inputs ---
      investigation_raw <- input$text_investigation
      investigation_clean <- stringr::str_replace_all(investigation_raw, "[\r\n]+", " ")
      investigation_norm <- normalize(investigation_clean)
      
      factors <- strsplit(input$text_gemini, "\\n+")[[1]] |> trimws()
      factors <- factors[factors != ""]
      
      # --- Validate & highlight ---
      results <- lapply(factors, function(f) {
        f_norm <- normalize(f)
        match_loc <- stringr::str_locate(investigation_norm, f_norm)
        
        # Check if match_loc is NA or not
        found <- !is.na(match_loc[1]) && match_loc[1] != 0
        
        list(Factor = f, Status = if (found) "✅ Found" else "❌ Not Found", Position = match_loc[1])
      }) |> dplyr::bind_rows()
      
      highlighted <- investigation_clean
      highlight_color <- "#FFF59D" # yellow
      
      for (f in factors) {
        f_trim <- stringr::str_trim(f)
        if (nzchar(f_trim)) {
          # Escape quotes and punctuation safely
          safe_pattern <- stringr::regex(stringr::str_replace_all(f_trim, "[[:punct:]]", "\\s?"), ignore_case = TRUE)
          highlighted <- tryCatch(
            stringr::str_replace_all(
              highlighted,
              safe_pattern,
              paste0("<mark style='background-color:", highlight_color, "; border-radius:3px; padding:1px 3px;'>", f_trim, "</mark>")
            ),
            error = function(e) highlighted
          )
        }
      }
      
      # --- Render ---
      output$validation_table <- DT::renderDataTable({
        DT::datatable(results, options = list(pageLength = 10))
      })
      
      output$highlighted_text <- renderUI({
        HTML(paste0("<div style='white-space: pre-wrap; line-height:1.6; font-size:15px;'>", highlighted, "</div>"))
      })
    })
    
    
    
  }
  
  
  # --------------------------------------------------------------------
  # Launch
  # --------------------------------------------------------------------
  
  shinyApp(ui = ui, server = server)
  
  
  
  # --------------------------------------------------------------------
  # Public API
  # --------------------------------------------------------------------
  extract_recommendations <- function(state_code, pdf_path) {
    algo <- get_reco_algo(state_code)
    if (is.null(algo)) {
      warning(sprintf("No recommendations algo registered for state_code='%s'", state_code))
      return(NA_character_)
    }
    out <- tryCatch(algo(pdf_path), error = function(e) NA_character_)
    if (is.null(out) || (is.atomic(out) && length(out) == 1 && is.na(out))) NA_character_ else out
  }
  
  extract_recommendations_batch <- function(df) {
    stopifnot(all(c("pdf_path", "state_code") %in% names(df)))
    purrr::pmap_dfr(df, function(pdf_path, state_code, ...) {
      algo <- get_reco_algo(state_code)
      if (is.null(algo)) {
        tibble(pdf_path = pdf_path, state_code = state_code, recommendations = list(NA))
      } else {
        out <- tryCatch(algo(pdf_path), error = function(e) list(NA))
        tibble(pdf_path = pdf_path, state_code = state_code,
               recommendations = list(out))
      }
    })
  }
  # 
  # #📄 Scraping page starting at 880...
  # ⬇️ Downloading https://stacks.cdc.gov/view/cdc/164601/cdc_164601_DS1.pdf ...
  # ✅ Saved: 164601.pdf needs to be downloaded
