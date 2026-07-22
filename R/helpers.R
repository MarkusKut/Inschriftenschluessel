library(dplyr)
library(tidyr)
library(readxl)
library(htmltools)
library(knitr)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x) || x == "") y else x

glyph_img <- function(glyph_path, alt = "", height = 30) {
  # root-relative URL so it works from any page depth
  src <- paste0("/assets/glyphs500px/", gsub("^/+", "", glyph_path))
  
  htmltools::tags$img(
    src   = src,
    alt   = alt,
    class = "glyph",
    style = paste0("height:", height, "px;"),
    loading = "lazy",
    decoding = "async"
  )
}

badge <- function(label) {
  tags$span(class = "badge-slot", label)
}

render_glyphline <- function(df_line) {
  pieces <- lapply(seq_len(nrow(df_line)), function(i) {
    row <- df_line[i, ]
    if (row$kind == "glyph") {
      glyph_img(row$glyph_path)
    } else if (row$kind == "badge") {
      badge(row$badge %||% row$text %||% "")
    } else {
      tags$span(row$text %||% "")
    }
  })
  tags$div(pieces)
}

glyph_block <- function(x) {
  htmltools::tags$div(
    style = "margin-bottom: 0.75rem;",
    x
  )
}

as_raw_html <- function(x) {
  x <- paste0(x, collapse = "\n")
  # prevent accidental indentation turning it into a code block
  x <- gsub("^[ \t]+", "", x)
  paste0("\n\n```{=html}\n", x, "\n```\n\n")
}

read_long_table_base <- function(df_tables, table_id) {
  df_tables %>%
    dplyr::filter(table_id == !!table_id) %>%
    dplyr::transmute(
      row   = as.numeric(row),
      col   = as.character(col),
      value = as.character(value)
    ) %>%
    dplyr::arrange(row) %>%
    tidyr::pivot_wider(
      names_from  = col,
      values_from = value,
      values_fn   = list(value = ~ paste(as.character(.x), collapse = ", ")),
      values_fill = list(value = "")
    )
}

# 
# 
# read_long_table <- function(df_tables, table_id, visited = character()) {
# 
#  
#   
#   if (table_id %in% visited) {
#     stop("Cycle in nested tables: ", paste(c(visited, table_id), collapse = " -> "))
#   }
#   visited <- c(visited, table_id)
# 
#   df_tables %>%
#     filter(table_id == !!table_id) %>%
#     select(row, col, value) %>%
#     arrange(row) %>%
#     pivot_wider(names_from = col, values_from = value) %>%
#     mutate(across(everything(), function(x) {
#       x <- ifelse(is.na(x), "", x)
#       if (is.list(x)) {
#         x <- vapply(x, function(cell) paste(cell, collapse = ", "), character(1))
#       } else {
#         x <- as.character(x)
#       }
# 
# 
#       #x <- paste(x, collapse = ", ")
#       # replace every occurrence of a png path with a markdown image
#       x <- gsub(
#         "([^\\s,]+\\.png)",
#         "![](/assets/glyphs/\\1){.glyph}",
#         x,
#         ignore.case = TRUE,
#         perl = TRUE
#       )
# 
#       # turn comma separators into spacing between glyphs
#       x <- gsub("\\s*,\\s*", "<span class='glyph-row-break'></span>", x, perl = TRUE)
#    
#       
#       x
#     })) %>%
#     select(-row)
# }

render_table_html_by_id <- function(df_tables, table_id) {
  df <- read_long_table(df_tables, table_id)
  kable_html(df)
}


expand_nested_tables_in_html <- function(html, df_tables, visited = character()) {
  out <- html
  pattern <- "\\[\\[table:([^\\]|]+)(\\|([^\\]]+))?\\]\\]"

  repeat {
    m <- regexpr(pattern, out, perl = TRUE)
    if (m[1] == -1) break

    match_text <- regmatches(out, m)[[1]]

    caps <- regmatches(match_text, regexec(pattern, match_text, perl = TRUE))[[1]]
    nested_id <- caps[2]
    summary_text <- if (length(caps) >= 4 && !is.na(caps[4]) && caps[4] != "") {
      caps[4]
    } else {
      #paste0("Öffne ", nested_id)
      "Öffne"
    }

    if (nested_id %in% visited) {
      stop("Cycle in nested tables: ", paste(c(visited, nested_id), collapse = " -> "))
    }

    nested_html <- render_table_html_by_id(df_tables, nested_id)
    nested_html <- expand_nested_tables_in_html(
      nested_html,
      df_tables,
      visited = c(visited, nested_id)
    )

    replacement <- paste0(
      "<details><summary>",
      htmltools::htmlEscape(summary_text),
      "</summary>",
      nested_html,
      "</details>"
    )

    start <- m[1]
    len <- attr(m, "match.length")

    out <- paste0(
      substr(out, 1, start - 1),
      replacement,
      substr(out, start + len, nchar(out))
    )
  }

  out
}


tla_table_badges_html <- function(
    value,
    sep_token = "|||CELLBREAK|||"
) {
  
  if (
    length(value) == 0 ||
    is.na(value)
  ) {
    return("")
  }
  
  value <- trimws(as.character(value))
  
  if (
    !nzchar(value) ||
    value == "NA"
  ) {
    return("")
  }
  
  # Repeated long-table values should also count as separate IDs
  value <- gsub(
    sep_token,
    ";",
    value,
    fixed = TRUE
  )
  
  # Accept:
  # 850653;123456
  # 850653/123456
  ids <- unlist(
    strsplit(
      value,
      "\\s*[;/]\\s*",
      perl = TRUE
    ),
    use.names = FALSE
  )
  
  ids <- trimws(ids)
  ids <- sub("\\.0+$", "", ids)
  
  ids <- ids[
    nzchar(ids) &
      ids != "NA"
  ]
  
  ids <- unique(ids)
  
  if (length(ids) == 0) {
    return("")
  }
  
  links <- vapply(ids, function(id) {
    
    url <- paste0(
      "https://thesaurus-linguae-aegyptiae.de/lemma/",
      utils::URLencode(id, reserved = TRUE)
    )
    
    paste0(
      "<a ",
      "href='", url, "' ",
      "class='tla-badge tla-badge-table' ",
      "title='TLA-Lemma ", id, "' ",
      "aria-label='TLA-Lemma ", id,
      " im Thesaurus Linguae Aegyptiae öffnen'>",
      htmltools::htmlEscape(id),
      "</a>"
    )
    
  }, character(1))
  
  paste0(
    "<span class='tla-table-badges'>",
    paste0(links, collapse = ""),
    "</span>"
  )
}

move_tla_into_table_cell <- function(df, target_col = NULL) {
  
  # Find the TLA column without depending on capitalization
  tla_col <- names(df)[
    toupper(trimws(names(df))) == "TLA"
  ]
  
  # Table has no TLA data
  if (length(tla_col) == 0) {
    return(df)
  }
  
  tla_col <- tla_col[1]
  
  # By default, use the first visible data column
  if (
    is.null(target_col) ||
    length(target_col) == 0 ||
    is.na(target_col) ||
    !nzchar(target_col)
  ) {
    candidates <- setdiff(
      names(df),
      c("row", tla_col)
    )
    
    if (length(candidates) == 0) {
      return(df)
    }
    
    target_col <- candidates[1]
  }
  
  if (!target_col %in% names(df)) {
    stop(
      "TLA target column not found: ",
      target_col
    )
  }
  
  tla_values <- as.character(df[[tla_col]])
  tla_values[
    is.na(tla_values) |
      tla_values == "NA"
  ] <- ""
  
  target_values <- as.character(df[[target_col]])
  target_values[
    is.na(target_values) |
      target_values == "NA"
  ] <- ""
  
  has_tla <- nzchar(trimws(tla_values))
  
  target_values[has_tla] <- paste0(
    target_values[has_tla],
    "<span class='table-inline-tla'>",
    tla_values[has_tla],
    "</span>"
  )
  
  df[[target_col]] <- target_values
  
  # Remove the separate TLA column from the rendered table
  df[[tla_col]] <- NULL
  
  df
}

read_long_table <- function(df_tables, table_id, visited = character()) {
  
  sep_token <- "|||CELLBREAK|||"
  
  if (table_id %in% visited) {
    stop("Cycle in nested tables: ", paste(c(visited, table_id), collapse = " -> "))
  }
  visited <- c(visited, table_id)
  
  df_tables %>%
    filter(table_id == !!table_id) %>%
    select(row, col, value) %>%
    arrange(row) %>%
    pivot_wider(
      names_from = col,
      values_from = value,
      values_fn = list(value = ~ paste(as.character(.x), collapse = sep_token)),
      values_fill = list(value = "")
    ) %>%
    mutate(across(-row, function(x) {
      
      column_name <- dplyr::cur_column()
      
      x <- ifelse(is.na(x), "", as.character(x))
      x[x == "NA"] <- ""
      
      # Special handling for the TLA column
      if (toupper(trimws(column_name)) == "TLA") {
        
        return(
          vapply(
            x,
            function(cell) {
              tla_table_badges_html(
                cell,
                sep_token = sep_token
              )
            },
            character(1)
          )
        )
      }
      
      # Normal text and glyph handling
      vapply(x, function(cell) {
        if (cell == "" || cell == "NA") return("")
        
        parts <- strsplit(cell, sep_token, fixed = TRUE)[[1]]
        parts <- trimws(parts)
        parts[parts == "NA"] <- ""
        parts <- parts[parts != ""]
        
        parts <- vapply(parts, function(part) {
          gsub(
            "([^\\s,]+\\.png)",
            "![](/assets/glyphs500px/\\1){.glyph}",
            part,
            ignore.case = TRUE,
            perl = TRUE
          )
        }, character(1))
        
        paste(parts, collapse = "<span class='glyph-row-break'></span>")
      }, character(1))
    })) %>%
    move_tla_into_table_cell() %>%
    select(-row)
}




# 
# kable_html <- function(df, class = "table") {
#   tbl <- knitr::kable(df, format = "html", escape = FALSE, table.attr = paste0('class="', class, '"'))
#   paste0("<div class='tableFixHead'>", tbl, "</div>")
# }

kable_html <- function(df, class = "table", wrapper_class = "tableFixHead table-wide") {
  tbl <- knitr::kable(
    df,
    format = "html",
    escape = FALSE,
    table.attr = paste0('class="', class, '"')
  )
  paste0("<div class='", wrapper_class, "'>", tbl, "</div>")
}


glyph_md <- function(glyph_path, alt = "", height = 30) {
  # Pandoc Markdown image with an attribute block
  # height works via CSS class; avoid inline HTML
  paste0("![](/assets/glyphs500px/", glyph_path, "){.glyph}")
}

text_md <- function(x) x



badge_class <- function(label) {
  key <- tolower(label)
  key <- gsub("[^a-z0-9]+", "-", key)   # "Spezifische Epitheta" -> "spezifische-epitheta"
  paste0("badge-slot badge-", key)
}


render_glyphline_md <- function(df_line) {
  parts <- vapply(seq_len(nrow(df_line)), function(i) {
    row <- df_line[i, ]
    if (row$kind == "glyph") {
      glyph_md(row$glyph_path)
    } else if (row$kind == "badge") {
      label <- row$text %||% ""
      href  <- row$href %||% ""
      cls   <- badge_class(label)
      
      if (href != "") {
        paste0("[", label, "](", href, "){.", gsub(" ", " .", cls), "}")
      } else {
        paste0("[", label, "]{.", gsub(" ", " .", cls), "}")
      }
    } else if (row$kind == "br") {
      "  \n"   # markdown line break
    } else {
      row$text %||% ""
    }
  }, character(1))
  
  paste(parts, collapse = " ")
}


tla_badge_html <- function(tla_id) {
  
  if (
    length(tla_id) == 0 ||
    all(is.na(tla_id))
  ) {
    return("")
  }
  
  tla_id <- trimws(as.character(tla_id))
  
  if (
    !nzchar(tla_id) ||
    tla_id == "NA"
  ) {
    return("")
  }
  
  # Accept several IDs separated by ";" or "/"
  ids <- unlist(
    strsplit(
      tla_id,
      "\\s*[;/]\\s*",
      perl = TRUE
    ),
    use.names = FALSE
  )
  
  ids <- trimws(ids)
  ids <- sub("\\.0+$", "", ids)
  
  ids <- unique(
    ids[
      nzchar(ids) &
        ids != "NA"
    ]
  )
  
  if (length(ids) == 0) {
    return("")
  }
  
  links <- vapply(ids, function(id) {
    
    url <- paste0(
      "https://thesaurus-linguae-aegyptiae.de/lemma/",
      utils::URLencode(id, reserved = TRUE)
    )
    
    paste0(
      "<a ",
      "href='", url, "' ",
      "class='tla-badge-table' ",
      "title='TLA-Lemma ", id, "' ",
      "aria-label='TLA-Lemma ", id,
      " im Thesaurus Linguae Aegyptiae öffnen'>",
      htmltools::htmlEscape(id),
      "</a>"
    )
    
  }, character(1))
  
  paste0(
    "<span class='glyphvariant-tla'>",
    "<span class='tla-table-badges'>",
    paste0(links, collapse = ""),
    "</span>",
    "</span>"
  )
}


render_glyphvariants_md <- function(df_line) {
  
  df <- df_line[order(df_line$seq), ]
  
  # Split into sections by label rows:
  # each label starts a new section
  df$section_id <- cumsum(df$kind == "label")
  
  sections <- split(df, df$section_id)
  
  out <- vapply(sections, function(sec) {
    
    # Label row for this section
    lbl_rows <- sec[
      sec$kind == "label",
      ,
      drop = FALSE
    ]
    
    lbl <- if (nrow(lbl_rows) == 0) {
      ""
    } else {
      as.character(lbl_rows$text[1])
    }
    
    # Read optional TLA ID from the label row
    tla_id <- if (
      nrow(lbl_rows) > 0 &&
      "tla_id" %in% names(lbl_rows)
    ) {
      lbl_rows$tla_id[1]
    } else {
      NA
    }
    
    tla_badge <- tla_badge_html(tla_id)
    
    tok <- sec[
      sec$kind != "label",
      ,
      drop = FALSE
    ]
    
    if (nrow(tok) == 0) {
      return("")
    }
    
    # Determine bullet grouping
    g <- as.character(tok$group)
    g[is.na(g) | g == ""] <- NA_character_
    
    if (
      !all(is.na(g)) &&
      length(unique(na.omit(g))) > 1
    ) {
      
      tok$bullet_id <- g
      
      # Keep bullet order by first appearance in seq
      ord <- tok |>
        dplyr::group_by(bullet_id) |>
        dplyr::summarise(
          min_seq = min(seq),
          .groups = "drop"
        ) |>
        dplyr::arrange(min_seq) |>
        dplyr::pull(bullet_id)
      
      tok$bullet_id <- factor(
        tok$bullet_id,
        levels = ord
      )
      
    } else {
      
      # If group is empty or constant,
      # start a new bullet at each glyph row
      tok$bullet_id <- cumsum(tok$kind == "glyph")
      tok$bullet_id[tok$bullet_id == 0] <- 1
    }
    
    bullets <- split(tok, tok$bullet_id)
    
    bullet_md <- vapply(bullets, function(b) {
      
      b <- b[
        order(b$seq),
        ,
        drop = FALSE
      ]
      
      paste0(
        "     - ",
        render_glyphline_md(b),
        "\n\n"
      )
      
    }, character(1))
    
    # Label and optional TLA badge
    label_md <- paste0(
      "- <span class='glyphvariant-heading'>",
      "<span class='glyphvariant-term'><em>",
      htmltools::htmlEscape(lbl),
      "</em>:</span>",
      tla_badge,
      "</span>\n\n"
    )
    
    paste0(
      label_md,
      paste0(bullet_md, collapse = "")
    )
    
  }, character(1))
  
  paste0(
    "::: {.glyphvariants-block}\n\n",
    paste0(out, collapse = ""),
    "\n:::\n"
  )
}

# 
# render_glyphvariants_md <- function(df_line) {
#   df <- df_line[order(df_line$seq), ]
#   
#   # Split into sections by label rows: each label starts a new section
#   df$section_id <- cumsum(df$kind == "label")
#   
#   sections <- split(df, df$section_id)
#   
#   out <- vapply(sections, function(sec) {
#     # label text (first label row in this section)
#     lbl_rows <- sec[sec$kind == "label", , drop = FALSE]
#     lbl <- if (nrow(lbl_rows) == 0) "" else as.character(lbl_rows$text[1])
#     
#     tok <- sec[sec$kind != "label", , drop = FALSE]
#     if (nrow(tok) == 0) return("")
#     
#     # Determine bullet grouping
#     # Preferred: use 'group' if it actually splits into multiple bullets
#     g <- as.character(tok$group)
#     g[is.na(g) | g == ""] <- NA_character_
#     
#     if (!all(is.na(g)) && length(unique(na.omit(g))) > 1) {
#       tok$bullet_id <- g
#       # keep bullet order by first appearance in seq
#       ord <- tok |>
#         dplyr::group_by(bullet_id) |>
#         dplyr::summarise(min_seq = min(seq), .groups = "drop") |>
#         dplyr::arrange(min_seq) |>
#         dplyr::pull(bullet_id)
#       tok$bullet_id <- factor(tok$bullet_id, levels = ord)
#       
#     } else {
#       # Fallback: if group is empty/constant, start a new bullet at each glyph row
#       # This matches "glyph + trailing text" pattern in your sheet
#       tok$bullet_id <- cumsum(tok$kind == "glyph")
#       tok$bullet_id[tok$bullet_id == 0] <- 1
#     }
#     
#     bullets <- split(tok, tok$bullet_id)
#     
#     bullet_md <- vapply(bullets, function(b) {
#       b <- b[order(b$seq), , drop = FALSE]
#       paste0("     - ", render_glyphline_md(b), "\n\n")
#     }, character(1))
#     
#     # Label line as its own paragraph, then bullets; exactly one string returned
#     paste0("- ", lbl, ":\n\n", paste0(bullet_md, collapse = ""))
#   }, character(1))
#   
#   #paste0(out, collapse = "")
#   paste0(
#     "::: {.glyphvariants-block}\n\n",
#     paste0(out, collapse = ""),
#     "\n:::\n"
#   )
# }



make_slot_linker <- function(pages_df) {
  # Build map: label -> href
  # Assumption: your slot pages are in pages sheet with type == "slot"
  slots <- pages_df %>%
    dplyr::filter(type == "slot") %>%
    dplyr::transmute(
      label = stringr::str_replace(title, "^Slot:\\s*", ""),  # "Slot: Gottheit" -> "Gottheit"
      href  = paste0(slug, ".qmd")                            # consistent with your project rule
    )
  
  # named vector: slots$href["Gottheit"] gives the href
  setNames(slots$href, slots$label)
}

link_slot_tokens <- function(text, slot_map, from_slug) {
  if (is.na(text) || text == "") return(text)
  
  # compute relative path from current page to target (slug has no .qmd)
  rel_href <- function(target_slug_qmd) {
    # from_slug like "pages/formeln/opferformel"
    # target_slug_qmd like "pages/slots/gottheit.qmd"
    fs::path_rel(target_slug_qmd, start = fs::path_dir(paste0(from_slug, ".qmd")))
  }
  
  out <- text
  
  for (label in names(slot_map)) {
    token <- paste0("\\[", stringr::fixed(label), "\\]")
    href  <- rel_href(slot_map[[label]])
    # cls  <- paste0("badge-slot badge-", tolower(label))
    # cls  <- gsub("[^a-z0-9]+", "-", cls)
    # repl <- paste0("[", label, "](", href, "){.", gsub(" ", " .", cls), "}")
    
    key <- tolower(label)
    key <- gsub("[^a-z0-9]+", "-", key)
    
    cls <- paste0("slot-link slot-", key)
    repl <- paste0("[", label, "](", href, "){.", gsub(" ", " .", cls), "}")
    
    
    # replace only literal [Label]
    out <- gsub(paste0("\\[", label, "\\]"), repl, out, perl = TRUE)
  }
  
  out
}




first_formula_glyphline_md <- function(page_id, content) {
  b <- content$formula_blocks %>%
    dplyr::filter(page_id == !!page_id, block_type == "glyphline") %>%
    dplyr::arrange(dplyr::row_number()) %>%
    dplyr::slice(1)
  
  if (nrow(b) == 0) return("")
  
  line_id <- b$table_id
  line_df <- content$glyphlines %>%
    dplyr::filter(line_id == !!line_id) %>%
    dplyr::arrange(seq)
  
  # render as markdown (glyphs + linked badges, if your render_glyphline_md does that)
  paste0(render_glyphline_md(line_df), "\n")
}



rewrite_slot_links_for_landing <- function(md) {
  # Formula pages use ../slots/..., landing page must use pages/slots/...
  gsub("\\(\\.\\./slots/", "(pages/slots/", md)
}



slot_key <- function(x) {
  x <- gsub("^Slot:\\s*", "", x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "-", x)
  x
}


