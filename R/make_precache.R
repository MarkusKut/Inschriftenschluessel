library(fs)

out_dir <- Sys.getenv(
  "QUARTO_PROJECT_OUTPUT_DIR",
  unset = "docs"
)

# --------------------------------------------------
# Find all generated website files
# --------------------------------------------------

files <- fs::dir_ls(
  out_dir,
  recurse = TRUE,
  type = "file"
)

rel <- fs::path_rel(
  files,
  start = out_dir
)

# Windows-safe paths
rel <- gsub("\\\\", "/", rel)

# Files that must not be included in their own manifests
exclude <- grepl(
  paste0(
    "^sw\\.js$|",
    "^precache-core\\.js$|",
    "^offline-full-manifest\\.txt$"
  ),
  rel
)

rel <- rel[!exclude]

# Relative URLs, therefore compatible with
# /Inschriftenschluessel/ on GitHub Pages
urls <- paste0("./", rel)


# --------------------------------------------------
# Small CORE cache
# --------------------------------------------------

core_pattern <- paste0(
  "^index\\.html$|",
  "^offline\\.html$|",
  "^site\\.css$|",
  "^site\\.webmanifest$|",
  "^favicon\\.ico$|",
  "^search\\.json$|",
  "^site_libs/|",
  "^icons/|",
  "^assets/branding/"
)

core_rel <- rel[
  grepl(core_pattern, rel)
]

core_urls <- paste0("./", core_rel)

# Build identifier changes on every render
build_id <- format(
  Sys.time(),
  "%Y%m%d%H%M%S"
)

quote_js <- function(x) {
  encodeString(
    x,
    quote = '"'
  )
}

core_js <- c(
  paste0(
    'self.__BUILD_ID = "',
    build_id,
    '";'
  ),
  "",
  "self.__CORE_URLS = [",
  paste0(
    "  ",
    vapply(
      core_urls,
      quote_js,
      character(1)
    ),
    collapse = ",\n"
  ),
  "];"
)

writeLines(
  core_js,
  file.path(
    out_dir,
    "precache-core.js"
  )
)


# --------------------------------------------------
# FULL offline manifest
# --------------------------------------------------

writeLines(
  urls,
  file.path(
    out_dir,
    "offline-full-manifest.txt"
  ),
  useBytes = TRUE
)

message(
  "Core cache: ",
  length(core_urls),
  " files"
)

message(
  "Full offline package: ",
  length(urls),
  " files"
)
