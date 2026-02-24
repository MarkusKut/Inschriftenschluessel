library(dplyr)
library(tidyr)
library(readxl)
library(htmltools)
library(knitr)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x) || x == "") y else x

glyph_img <- function(glyph_path, alt = "", height = 30) {
  # root-relative URL so it works from any page depth
  src <- paste0("/assets/glyphs/", gsub("^/+", "", glyph_path))
  
  htmltools::tags$img(
    src   = src,
    alt   = alt,
    class = "glyph",
    style = paste0("height:", height, "px;")
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
    dplyr::select(row, col, value) %>%
    dplyr::arrange(as.numeric(row)) %>%
    tidyr::pivot_wider(names_from = col, values_from = value)
}
# 
# read_long_table <- function(df_tables, table_id) {
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
#       x <- gsub("\\s*,\\s*", " &ensp; ", x, perl = TRUE)
#       
#       x
#     })) %>%
#     select(-row)
# }



read_long_table <- function(df_tables, table_id, visited = character()) {
  
  if (table_id %in% visited) {
    stop("Cycle in nested tables: ", paste(c(visited, table_id), collapse = " -> "))
  }
  visited <- c(visited, table_id)
  
  df_tables %>%
    filter(table_id == !!table_id) %>%
    select(row, col, value) %>%
    arrange(row) %>%
    pivot_wider(names_from = col, values_from = value) %>%
    mutate(across(everything(), function(x) {
      x <- ifelse(is.na(x), "", x)
      if (is.list(x)) {
        x <- vapply(x, function(cell) paste(cell, collapse = ", "), character(1))
      } else {
        x <- as.character(x)
      }
    

      #x <- paste(x, collapse = ", ")
      # replace every occurrence of a png path with a markdown image
      x <- gsub(
        "([^\\s,]+\\.png)",
        "![](/assets/glyphs/\\1){.glyph}",
        x,
        ignore.case = TRUE,
        perl = TRUE
      )

      # turn comma separators into spacing between glyphs
      x <- gsub("\\s*,\\s*", " &ensp; ", x, perl = TRUE)

      x
    })) %>%
    select(-row)
}


kable_html <- function(df, class = "table") {
  tbl <- knitr::kable(df, format = "html", escape = FALSE, table.attr = paste0('class="', class, '"'))
  paste0("<div class='tableFixHead'>", tbl, "</div>")
}



glyph_md <- function(glyph_path, alt = "", height = 30) {
  # Pandoc Markdown image with an attribute block
  # height works via CSS class; avoid inline HTML
  paste0("![](/assets/glyphs/", glyph_path, "){.glyph}")
}

text_md <- function(x) x

render_glyphline_md <- function(df_line) {
  parts <- vapply(seq_len(nrow(df_line)), function(i) {
    row <- df_line[i, ]
    if (row$kind == "glyph") {
      glyph_md(row$glyph_path)
    } else if (row$kind == "badge") {
      label <- row$badge %||% row$text %||% ""
      
      # You must provide the target slug in the glyphlines data (recommended),
      # otherwise we can only guess.
      href <- row$href %||% ""
      
      if (href != "") {
        paste0("[", label, "](", href, "){.badge-slot}")
      } else {
        paste0("[", label, "]{.badge-slot}")
      }
    } else if (row$kind == "br") {
      "  \n"   # markdown line break
    } else {
      row$text %||% ""
    }
  }, character(1))
  
  paste(parts, collapse = " ")
}



render_glyphvariants_md <- function(df_line) {
  df <- df_line[order(df_line$seq), ]
  
  # Split into sections by label rows: each label starts a new section
  df$section_id <- cumsum(df$kind == "label")
  
  sections <- split(df, df$section_id)
  
  out <- vapply(sections, function(sec) {
    # label text (first label row in this section)
    lbl_rows <- sec[sec$kind == "label", , drop = FALSE]
    lbl <- if (nrow(lbl_rows) == 0) "" else as.character(lbl_rows$text[1])
    
    tok <- sec[sec$kind != "label", , drop = FALSE]
    if (nrow(tok) == 0) return("")
    
    # Determine bullet grouping
    # Preferred: use 'group' if it actually splits into multiple bullets
    g <- as.character(tok$group)
    g[is.na(g) | g == ""] <- NA_character_
    
    if (!all(is.na(g)) && length(unique(na.omit(g))) > 1) {
      tok$bullet_id <- g
      # keep bullet order by first appearance in seq
      ord <- tok |>
        dplyr::group_by(bullet_id) |>
        dplyr::summarise(min_seq = min(seq), .groups = "drop") |>
        dplyr::arrange(min_seq) |>
        dplyr::pull(bullet_id)
      tok$bullet_id <- factor(tok$bullet_id, levels = ord)
      
    } else {
      # Fallback: if group is empty/constant, start a new bullet at each glyph row
      # This matches "glyph + trailing text" pattern in your sheet
      tok$bullet_id <- cumsum(tok$kind == "glyph")
      tok$bullet_id[tok$bullet_id == 0] <- 1
    }
    
    bullets <- split(tok, tok$bullet_id)
    
    bullet_md <- vapply(bullets, function(b) {
      b <- b[order(b$seq), , drop = FALSE]
      paste0("- ", render_glyphline_md(b), "\n\n")
    }, character(1))
    
    # Label line as its own paragraph, then bullets; exactly one string returned
    paste0(lbl, ":\n\n", paste0(bullet_md, collapse = ""))
  }, character(1))
  
  paste0(out, collapse = "")
}



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
    repl  <- paste0("[", label, "](", href, "){.badge-slot}")
    
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





