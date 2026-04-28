# aeroinputs

> Prepare [AERO](https://doi.org/10.1016/j.aeolia.2021.100769) wind-erosion model inputs from LDC plot data and SOLUS soil-texture rasters.

## Overview

`aeroinputs` provides three exported functions that form a complete
end-to-end pipeline:

| Function | Purpose |
|---|---|
| `fetch_solus()` | Download SOLUS 100 soil-property rasters from cloud storage |
| `fetch_ldc_data()` | Fetch LDC plot-level datasets (tall tables for gap, height, lpi, and indicators) |
| `generate_aero_inputs()` | Build per-plot gap `.txt`, `.ini`, and `input_data.csv` for AERO |

## Installation

`aeroinputs` depends on two GitHub-only packages. Install everything
with [pak](https://pak.r-lib.org/):


```r
# install.packages("pak")
pak::pkg_install("Landscape-Data-Commons/aeroinputs")
```

This will also install [`trex`](https://github.com/Landscape-Data-Commons/trex) and [`terradactyl`](https://github.com/Landscape-Data-Commons/terradactyl) from their respective
GitHub repositories automatically (declared in `DESCRIPTION: Remotes`).

## Quick start

```r
library(aeroinputs)

base_dir <- "~/LDC/AERO_test"
texture_file <- file.path(base_dir, "soil_texture_w_sand_frac.tif")

# 1. Fetch soil texture raster (only needed once)
fetch_solus(
  variables    = c("sandtotal", "claytotal"),
  depth_slices = 0,
  output_type  = "prediction",
  return       = "grid",
  filename     = texture_file,
  overwrite    = FALSE
)

# 2. Set keyring to get API token if you need to get access to non-public data
# Run once per machine to store credentials securely
username <- "your_email@example.com"
trex::setup_keyring(username)
trex::store_password(username)
# Otherwise, set username as NULL for access only public data
username <- NULL

# 3. Fetch LDC plot data (retrieve for all plots for project Jornada_JERHM)
fetch_ldc_data(
  username  = username,
  base_dir  = base_dir,
  project_keys = "Jornada_JERHM",
  write_out = TRUE
)

# 4. Build AERO inputs
result <- generate_aero_inputs(
  data_dir     = base_dir,
  output_dir   = file.path(base_dir, "aero_inputdata"),
  texture_file = texture_file
)
```

## API credentials
Use `trex::setup_keyring()` to securely store your API credentials in your
system keyring. This avoids keeping secrets in plain text files like
`~/.Renviron`.

## Output structure

After running the full pipeline, `aero_inputdata` will contain:

```
aero_inputdata/
  gap/
    <PrimaryKey>.txt     # canopy gap distances (fraction)
  <PrimaryKey>.ini       # AERO configuration file per plot
  input_data.csv         # combined summary table
```

## Vignettes

| Vignette | Description |
|---|---|
| `vignette("fetch-solus",           package = "aeroinputs")` | Retrieve SOLUS rasters |
| `vignette("fetch-ldc-data",        package = "aeroinputs")` | Fetch LDC datasets |
| `vignette("generate-aero-inputs",  package = "aeroinputs")` | Build AERO inputs |

## Development

The original standalone scripts are preserved in [`dev/`](https://github.com/Landscape-Data-Commons/aero_inputs/tree/main/dev) for
reference.


