# Build package

# Dependencies
update(devtools::package_deps())

# Check examples and documentation
devtools::check_man()
devtools::run_examples(run = FALSE)

tmp <- combinacoes(
  base.de.dados = read.csv(
    'data/database.csv', sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "",
    encoding = "UTF-8"),
  cruzamento = read.csv(
    'data/cruzas.csv', sep = ",", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8"))
tmp

# Check the package for Linux and Windows
devtools::check(check_version = TRUE, force_suggests = TRUE, args = "--use-valgrind")
devtools::build_win()

# devtools::build()

# upload to CRAN
# devtools::release(check = FALSE)
