commits <- system("git log --date=short --pretty=format:'%ad|%h|%s'", intern = TRUE)

categorize <- function(files) {
  cats <- character()
  
  if (any(grepl("^R/|\\.R$", files))) cats <- c(cats, "R code")
  if (any(grepl("^templates/|\\.tmpl$|\\.qmd$", files))) cats <- c(cats, "Templates/Pages")
  if (any(grepl("site\\.css$|\\.css$", files))) cats <- c(cats, "CSS")
  if (any(grepl("^data/|\\.xlsx$", files))) cats <- c(cats, "Data")
  if (any(grepl("sw\\.js$|manifest|pwa-|offline\\.html$", files))) cats <- c(cats, "PWA/Offline")
  
  if (length(cats) == 0) cats <- "Other"
  paste(cats, collapse = ", ")
}

out <- c("# Changelog", "")

for (line in commits) {
  parts <- strsplit(line, "\\|", perl = TRUE)[[1]]
  date <- parts[1]
  hash <- parts[2]
 # msg  <- parts[3]
  msg  <- paste(parts[-c(1, 2)], collapse = "|")
  
  #files <- system(paste("git show --pretty='' --name-only", hash), intern = TRUE)
  files <- system2(
    "git",
    args = c(
      "show",
      "--no-renames",
      "--pretty=",
      "--name-only",
      hash
    ),
    stdout = TRUE
  )
  files <- files[nzchar(files)]
  
  cats <- categorize(files)
  
  out <- c(
    out,
    paste0("## ", date),
    "",
    paste0("- `", hash, "` **[", cats, "]** ", msg),
    ""
  )
}

writeLines(out, "CHANGELOG.md")