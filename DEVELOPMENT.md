## Setting up the development environment

```r
install.packages(c('devtools', 'pkgdown', 'styler', 'lintr')) # install dev dependencies
devtools::install_deps(dependencies = TRUE) # install package dependencies
devtools::document() # generate package meta data and man files
devtools::build() # build package
```

## Validating the package

```r
styler::style_pkg() # format code
lintr::lint_package() # lint code

devtools::test() # test package
devtools::check() # check package for errors
```

## Developing the documentation site

The [documentation site](https://cmu-delphi.github.io/epidatr/) is built off of the `main` branch. The `dev` version of the site is available at https://cmu-delphi.github.io/epidatr/dev.

The documentation site can be previewed locally by running in R

```r
pkgdown::build_site(preview=TRUE)
```

The `main` version is available at `file:///<local path>/epidatr/docs/index.html` and `dev` at `file:///<local path>/epidatr/docs/dev/index.html`.

You can also build the docs manually and launch the site with python. From the terminal, this looks like
```bash
R -e 'devtools::document()'
python -m http.server -d docs
```

For `pkgdown` to correctly generate both public (`main`) and `dev` documentation sites, the package version in `DESCRIPTION` on `dev` must have four components, and be of the format `x.x.x.9000`. The package version on `main` must be in the format `x.x.x`.

The documentation website is updated on push or pull request to the `main` and `dev` branches.

## Release process

### Manual

TBD

### Automated (currently unavailable)

The release consists of multiple steps which can be all done via the GitHub website:

1. Go to [create_release GitHub Action](https://github.com/cmu-delphi/epidatr/actions/workflows/create_release.yml) and click the `Run workflow` button. Enter the next version number or one of the magic keywords (patch, minor, major) and hit the green `Run workflow` button.
2. The action will prepare a new release and will end up with a new [Pull Request](https://github.com/cmu-delphi/epidatr/pulls)
3. Let the code owner review the PR and its changes and let the CI check whether everything builds successfully
4. Once approved and merged, another GitHub action job starts which automatically will
   1. create a git tag
   2. create another [Pull Request](https://github.com/cmu-delphi/epidatr/pulls) to merge the changes back to the `dev` branch
   3. create a [GitHub release](https://github.com/cmu-delphi/epidatr/releases) with automatically derived release notes
5. Release to CRAN
