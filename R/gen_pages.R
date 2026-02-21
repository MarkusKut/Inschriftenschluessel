library(dplyr)
library(readxl)
library(whisker)
library(fs)

source("R/helpers.R")

load_content <- function(path = "data/content.xlsx") {
  list(
    pages         = read_excel(path, "pages"),
    landing_cards = read_excel(path, "landing_cards"),
    formula_blocks= read_excel(path, "formula_blocks"),
    slot_content  = read_excel(path, "slot_content"),
    tables        = read_excel(path, "tables") %>%
      dplyr::transmute(
        table_id = as.character(table_id),
        row      = as.character(row),
        col      = as.character(col),
        value    = as.character(value)
      ) %>%
      dplyr::filter(
        !is.na(table_id), table_id != "",
        !is.na(row),      row != "",
        !is.na(col),      col != ""
      ),
    glyphlines    = read_excel(path, "glyphlines")
  )
}

write_qmd <- function(path, text) {
  dir_create(path_dir(path))
  writeLines(text, path, useBytes = TRUE)
}

gen_landing <- function(content) {
  tmpl <- readLines("templates/landing.qmd.tmpl", warn = FALSE) #|> paste(collapse = "\n")
  
  cards <- content$landing_cards %>%
    left_join(content$pages %>% select(page_id, title, slug, type),
              by = c("href_page_id" = "page_id")) %>%
    mutate(
      slug = trimws(slug),
      href = paste0(slug, ".qmd"),
      preview = dplyr::if_else(
        type == "formula",
        vapply(href_page_id, function(pid) {
          rewrite_slot_links_for_landing(first_formula_glyphline_md(pid, content))
        }, character(1)),
        ""
      )
    )
  
  # hard fail if any row contains multiple targets
  bad <- cards %>% filter(grepl(",", slug) | grepl("\\s", slug) & grepl(",", gsub("\\s+", "", slug)))
  if (nrow(bad) > 0) {
    stop("landing_cards joins to pages.slug containing multiple targets (comma). Fix Excel pages.slug to 1 target per row.")
  }
  
  out <- whisker.render(tmpl, list(cards = cards))
  write_qmd("index.qmd", out)
}

gen_formula_pages <- function(content) {
  tmpl <- readLines("templates/formula.qmd.tmpl", warn = FALSE) |> paste(collapse = "\n")
  
  formulas <- content$pages %>% filter(type == "formula")
  
  for (i in seq_len(nrow(formulas))) {
    p <- formulas[i, ]
    blocks <- content$formula_blocks %>% filter(page_id == p$page_id) %>% arrange(match(block_type, c("glyphline","prose","table","glyphvariants")))
    
    slot_map <- make_slot_linker(content$pages)
    
    # pre-render blocks to HTML/markdown fragments
    rendered <- lapply(seq_len(nrow(blocks)), function(j) {
      b <- blocks[j, ]
      if (b$block_type == "glyphline") {
        line_id <- b$table_id
        line_df <- content$glyphlines %>% filter(line_id == !!line_id) %>% arrange(seq)
        paste0(render_glyphline_md(line_df), "\n\n")
        
      } else if (b$block_type == "glyphvariants") {
        line_id <- b$table_id
        line_df <- content$glyphlines %>% filter(line_id == !!line_id) %>% arrange(seq)
        paste0(render_glyphvariants_md(line_df), "\n")
        
      } else if (b$block_type == "table") {
        df <- read_long_table(content$tables, b$table_id)
        kable_html(df)
      } else {
        link_slot_tokens(b$body_md %||% "", slot_map = slot_map, from_slug = p$slug)
      }
    })
    
    block_md <- paste0(
      mapply(function(b, r) {
        h <- b$heading %||% ""
        if (h != "") paste0("## ", h, "\n\n", r, "\n") else paste0(r, "\n")
      }, split(blocks, seq_len(nrow(blocks))), rendered),
      collapse = "\n"
    )
    
    out <- whisker.render(tmpl, list(title = p$title, body = block_md))
    out_path <- paste0(trimws(p$slug), ".qmd")
    write_qmd(out_path, out)
  }
}

gen_slot_pages <- function(content) {
  tmpl <- readLines("templates/slot.qmd.tmpl", warn = FALSE) |> paste(collapse = "\n")
  
  slots <- content$pages %>% filter(type == "slot")
  
  for (i in seq_len(nrow(slots))) {
    p <- slots[i, ]
    sections <- content$slot_content %>% filter(page_id == p$page_id)
    
    rendered_sections <- lapply(seq_len(nrow(sections)), function(j) {
      s <- sections[j, ]
      part <- ""
      if (!is.na(s$body_md) && s$body_md != "") part <- paste0(part, s$body_md, "\n\n")
      
      if (!is.na(s$table_id) && s$table_id != "") {
        df <- read_long_table(content$tables, s$table_id)
        tbl <- kable_html(df)
        
        if (isTRUE(s$collapsible)) {
          tbl <- as.character(details_wrap(s$heading %||% "Tabelle", tbl))
          return(tbl)
        } else {
          part <- paste0(part, tbl, "\n\n")
        }
      }
      
      if (!is.na(s$heading) && s$heading != "" && !isTRUE(s$collapsible)) {
        paste0("## ", s$heading, "\n\n", part)
      } else {
        part
      }
    })
    
    body <- paste(unlist(rendered_sections), collapse = "\n")
    out <- whisker.render(tmpl, list(title = p$title, body = body))
    out_path <- paste0(trimws(p$slug), ".qmd")
    write_qmd(out_path, out)
  }
}

generate_site_pages <- function() {
  content <- load_content()
  
  # ---- VALIDATION GUARDS (PUT IT HERE) ----
  if (any(grepl("\\.qmd$", content$pages$slug))) {
    stop("Excel pages.slug must NOT include '.qmd'. Remove extensions from the slug column.")
  }
  
  if (any(grepl(",", content$pages$slug))) {
    stop("Excel pages.slug must contain exactly ONE path per row. Commas detected.")
  }
  
  if (nrow(content$pages) != dplyr::n_distinct(content$pages$slug)) {
    stop("Duplicate slugs detected in pages sheet. Each page must be unique.")
  }
  # ----------------------------------------
  
  gen_landing(content)
  gen_formula_pages(content)
  gen_slot_pages(content)
}