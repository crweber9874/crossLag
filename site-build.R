devtools::load_all(0)
devtools::document()
pkgdown::build_site()
pkgdown::build_site_github_pages(dest_dir = "docs", install = TRUE)
