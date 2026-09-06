# Vignettes layout

| Path | Role | Edit? |
|------|------|-------|
| `<name>.Rmd.orig` | Editable sources (run Stan / plots when building) | **Yes** |
| `<name>.Rmd` (this folder) | Frozen HTML-ready vignettes shipped to CRAN | **No** — generated |
| `figures/<vignette-name>/` | PNGs produced by the source knit | regenerated on build |

The `.orig` suffix matters: files under `vignettes/` that end in `.Rmd` are
discovered as vignettes by R and by pkgdown. Sources must not use a plain
`.Rmd` name (a `sources/` subfolder alone is not enough for pkgdown).

## Workflow

1. Edit only `<name>.Rmd.orig`.
2. From the package root (or anywhere the helper can find it):

```r
CausalQueries:::build_vignettes(only = "a-getting-started")
# or rebuild all:
CausalQueries:::build_vignettes()
```

`build_vignettes()` sets `options(mc.cores = …)` for parallel Stan chains (all cores locally; at most 2 under `R CMD check`). You will see a message like `CausalQueries: options(mc.cores = N) …`. Frozen vignettes are what CRAN builds, so this does not affect CRAN check time.

3. Commit the updated frozen `vignettes/<name>.Rmd` and any new/changed files under `figures/<name>/`.

`.Rmd.orig` files are listed in `.Rbuildignore` so CRAN never treats them as buildable vignettes.
