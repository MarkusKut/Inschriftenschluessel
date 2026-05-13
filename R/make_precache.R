site_dir <- Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR", unset = "docs")

all_files <- list.files(
  site_dir,
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = FALSE,
  all.files = FALSE
)

keep <- grepl(
  "\\.(html|css|js|json|webmanifest|png|jpg|jpeg|svg|gif|webp|ico|woff|woff2|ttf|eot)$",
  all_files,
  ignore.case = TRUE
)

files <- all_files[keep]

rel <- substring(
  normalizePath(files, winslash = "/"),
  nchar(normalizePath(site_dir, winslash = "/")) + 2
)

rel <- gsub("\\\\", "/", rel)

rel <- rel[!basename(rel) %in% c("sw.js", "precache-manifest.js")]

out <- c(
  "self.__PRECACHE = [",
  paste0("  '", rel, "'", collapse = ",\n"),
  "];"
)

writeLines(out, file.path(site_dir, "precache-manifest.js"))
