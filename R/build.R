options(error = NULL)

source("R/gen_pages.R")
generate_site_pages()
quarto::quarto_render(".")

