urls <- readLines(
  "docs/offline-full-manifest.txt",
  warn = FALSE
)

paths <- sub(
  "^\\./",
  "",
  urls
)

missing <- paths[
  !file.exists(
    file.path(
      "docs",
      paths
    )
  )
]

missing