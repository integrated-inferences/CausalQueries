# NA

## Contributing to CausalQueries

🎉 Welcome to our contribution guidelines and thank you for your
interest in contributing!

### Reporting a bug

Before you report a bug make sure the same bug hasn’t been reported
before. We track bugs as [issues on
GitHub](https://github.com/macartan/CausalQueries/issues). If no related
issue has been opened, create an issue keeping in mind the following
guidelines:

- Use an informative title
- Write a minimal working example that allows us to reproduce the bug
  you found
- If the bug you’re reporting crashes your R session, please mention
  that in the title

### Contributing code

You have had a look at our [issues on
GitHub](https://github.com/macartan/CausalQueries/issues) and would like
to solve one of them? or you would like to develop a feature? That’s
great and we gladly welcome that. We just would like to suggest you
follow these simple guidelines:

- Fork the [CausalQueries
  repository](https://github.com/macartan/CausalQueries)
- Clone your fork locally
- Always be up to date with the `master` branch
- Add your edits
- Run and pass `devtools::check()`
- Reach a 100% coverage `covr::package_coverage()`
- Add yourself as a contributor in the `DESCRIPTION` file
- Open a pull request

Note: members of the `CausalQueries` dev team can skip the first two
bullet points above and branch out instead.

### Updating or writing vignettes

Vignettes that call
[`update_model()`](https://integrated-inferences.github.io/CausalQueries/reference/update_model.md)
are slow, so we pre-knit them locally and ship frozen
`vignettes/<name>.Rmd` files to CRAN. See `vignettes/README.md` for the
folder map.

- **Edit** only `vignettes/<name>.Rmd.orig`
- **Do not edit** the frozen `vignettes/<name>.Rmd` (it is generated)
- Figures land in `vignettes/figures/<name>/`
- Rebuild with `CausalQueries:::build_vignettes(only = "<name>")`

Give code chunks (especially plots) **unique** and **informative** names
so figure files stay stable. The `.orig` suffix keeps sources out of
vignette/pkgdown discovery (plain `.Rmd` under `vignettes/` would be
built twice).

**New vignette:**

- add `vignettes/<name>.Rmd.orig` with normal executable chunks
- set `fig.path = "figures/<name>/"` in the setup chunk
- run `CausalQueries:::build_vignettes(only = "<name>")`
- add `<name>` to the default list in `R/build_vignettes.R`
- ensure `.Rbuildignore` contains `^vignettes/.*\.Rmd\.orig$`

**Update an existing vignette:**

- edit `vignettes/<name>.Rmd.orig`
- run `CausalQueries:::build_vignettes(only = "<name>")` (or omit `only`
  to rebuild all)
- commit the frozen `.Rmd` and any figure changes
