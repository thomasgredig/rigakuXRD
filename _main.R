#####################
# run this to generate the
# documentation and HTML
# site with examples and
# results
#####################


# see also:
# https://lbusett.netlify.app/post/building-a-website-with-pkgdown-a-short-guide/

# use_readme_md()
# use_news_md()
# use_vignette("krivAMR")
# pkgdown::clean_site()

pkgdown::build_favicons(pkg = ".", overwrite = FALSE)

devtools::document()

pkgdown::build_reference()

pkgdown::build_site()

