# Vignettes layout

| Path | Role | Edit? |
|------|------|-------|
| `sources/*.Rmd` | Editable sources (run Stan / plots when building) | **Yes** |
| `*.Rmd` (this folder) | Frozen HTML-ready vignettes shipped to CRAN | **No** — generated |
| `figures/<vignette-name>/` | PNGs produced by the source knit | regenerated on build |

## Workflow

1. Edit only `sources/<name>.Rmd`.
2. From the package root (or anywhere the helper can find it):

```r
CausalQueries:::build_vignettes(only = "a-getting-started")
# or rebuild all:
CausalQueries:::build_vignettes()
```

3. Commit the updated frozen `vignettes/<name>.Rmd` and any new/changed files under `figures/<name>/`.

`sources/` is listed in `.Rbuildignore` so CRAN never treats those files as buildable vignettes.
