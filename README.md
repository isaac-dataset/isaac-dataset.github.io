# ISAAC data site

**Live site:** https://isaac-dataset.github.io/

## Replication materials

Each paper gets its own folder, named `<year>_<journal>`, under both `data/` and `R/` (e.g. `data/2026_jids/`, `R/2026_jids/`). Scripts run from the repository root and source their folder's `setup.R`.

## R package management (renv)

Packages are pinned in `renv.lock`. Github restores from it automatically, no need to touch it unless you add a package.

Pre-installed for everyone (no need to install these yourself): `tidyverse`, `ggplot2`, `WDI`, `texreg`, `haven`, `readxl`, `countrycode`, `modelsummary`.

- Restore pinned packages locally:
  ```r
  renv::restore()
  ```
- Add a new package:
  ```r
  install.packages("pkgname")
  renv::snapshot()
  ```
  then commit the updated `renv.lock`.
- R version is pinned separately in `.github/workflows/publish.yml` (`r-version`).

This initially feels complicated, but it makes the site easier to maintain and more stable in the long run.
