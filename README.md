# RaschKit

An R package for Rasch modeling, item analysis, calibration, equating, and DIF analysis using ACER ConQuest.

## Installation

### Prerequisites

Install required packages:

```r
lapply(
  c('rlang', 'bookdown', 'ggthemes', 'ggrepel', 'patchwork', 'rmarkdown',
    'gdata', 'janitor', 'data.table', 'knitr', 'RColorBrewer', 'fs',
    'lazyeval', 'writexl', 'ggpubr', 'qpdf', 'tidyverse', 'openxlsx',
    'kableExtra', 'magrittr', 'Rcpp', 'tidyselect', 'grid', 'flextable',
    'magick'),
  function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x)
      library(x, character.only = TRUE)
    }
  }
)
```

### Software Compatibility

| ConQuest Version | conquestr Version | RaschKit Version |
|-----------------|-------------------|------------------|
| v5.47.5 | v1.5.5 | v1.1.3 |
| v5.33.5 | v1.1.0 | v1.0.7 |
| v5.33.5 | v1.0.8 | v1.0.6 |
| v5.27.0 | v1.0.1 | v1.0.1 |
| v5.23.4 | v0.9.995 | v1.0.0 |

## Getting Started

```r
library(RaschKit)
install_packages_ls()
create_folders()
```

Explore `.Rmd` files in the `rCode` folder for examples of:
- Data exploration
- Calibration
- Equating
- DIF analysis

## Key Features

- **Item Analysis**: Comprehensive item statistics with ACER template formatting
- **Calibration**: Support for anchoring, multiple keys, and diagnostic analysis
- **Equating**: Automated DIF analysis with statistics and plots
- **DIF Analysis**: Multiple methods for both dichotomous and polytomous items
- **Visualization**: CCCs, item-person maps, and DIF plots with ACER formatting
- **Output**: Excel files with hyperlinks, formatting, and color-coded flags

## Main Functions

### Calibration
- `calibrateLst()`: Calibrate a list of response dataframes
- `calibrateScale()`: Automated anchor file preparation with three methods
- `calibrateCon()`: Free calibration by regressing on grade

### Equating
- `equateLst()`: DIF analysis on delta dataframes
- `equate2Type()`: Handle horizontal test forms

### DIF Analysis
- `DIFVarTests()`: DIF analysis across multiple variables and tests
- `plot_DIF_group()`: Visualize DIF for polytomous items
- `delta_DIF_dich()`: Dichotomous DIF analysis

### Utilities
- `freq_resps_cat()`: Response frequency analysis
- `poly_recode()`: Recode polytomous items
- `QA2Df()`: Quality assurance for dataframe comparison

## Version History

### v1.1.3 (Latest)

**Major Changes:**
- Reorganized folder structure: changed 'output' to 'calibration' with run-specific subfolders
- Item summary files now stored in `calibration/run` with working hyperlinks
- Compatible with ACER ConQuest v5.47.5
- Replaced `plyr` functions with base R (removed `plyr` dependency)
- Updated `plot_DIF_group()` to use `.data[[DIFVar]]` and `{{ DIFVar }}` instead of `!!sym(DIFVar)`

**Bug Fixes:**
- Fixed installation order issues with symbol evaluation
- Improved file organization for multi-run analyses

### v1.1.2

**Improvements:**
- `CCC_ipMap()`: Better handling of apostrophes in labels, added step numbers
- `equate2Type()`: Support for horizontal test forms without grades
- `delta_DIF_dich()`: Compatible with ConQuest 5.45.2 output
- `item_stats()`: Reads ConQuest 5.45.2 .shw.xls, added 'anchor' parameter
- `calibrate()`: Performs item summary and plotting for anchoring

**Bug Fixes:**
- Fixed file reading issues in `equateLst()`
- Resolved `as_roster` error in `CCC_ipMap()`
- Removed delta shift adjustment for anchoring

### v1.1.1

- Fixed numeric type enforcement in `section_keys()`

### v1.1.0

**Major Updates:**
- Compatible with ConQuest v3.40.0 (backwards compatible)
- Added sigma method to equating functions
- Read deltas from itn.xls files
- Updated `fre_resps_cat()` to preserve frequencies when `proportion = TRUE`

**Bug Fixes:**
- `read2one()`: Removed domains without flagged items, fixed merge issues
- `deltaStepTransform()`: Updated for new ConQuest version

### v1.0.9

- Fixed flag handling in `add_format()` (supports lists and vectors)
- Equating without scaled scores now works
- Variable checks in `DIFDimOne()` occur after data reading
- Corrected 'design_effect' spelling in documentation
- Updated polytomous DIF column names to 'Delta adj ...'

### v1.0.8

- CQS file saving for DIF analyses
- Compatible with ConQuest v5.34.3
- Updated Priority 4 flagging for polytomous items
- Fixed merge issues with different 'Key' variable types
- Improved flag coloring in DIF results

### v1.0.7

**Major Features:**
- Beautified output formats (rounding, fonts, etc.)
- Consolidated item removal and multiple keys in keyDf
- `equateLst()`: Accepts different threshold cutoffs
- Updated item step DIF procedures
- Hyperlinks work across machines (max one level up)
- Added 'useR' argument to `calibrateLst()`

**Enhancements:**
- Test statistics: average facility, correlation, Cronbach's alpha
- Multiple key sorting and checking
- Improved distractor flagging in CCC
- Changed `DIF_adj_cut` to `DIF_std_cut`
- `equate2Type()`: New `linkTypeLst` argument

### v1.0.6

- Fixed iterative DIF item selection in `Equate()`
- `CCC_ipMap()`: Handles items with all correct responses
- Added `.sav` file reading to `DIFVarTests()`

### v1.0.5

- Fixed `poly_recode()` missing 'test' argument
- Resolved pipe operator issue in `CCC_ipMap()`

### v1.0.4

**Major Release:**
- `calibrateCon()`: Merges grades before free calibration
- `calibrateLst()`: Batch calibration with ACER template output
- `calibrateScale()`: Three anchor file preparation methods
- `equateLst()`: Comprehensive DIF analysis with plots
- `DIFVarTests()`: Multi-variable DIF analysis
- ACER template formatting for all outputs
- Hyperlinks and color-coded flags in Excel
- Improved argument naming (e.g., `respDfLst`, `grdIntVec`)
- Enhanced input validation and error messages
- Support for double-key items in CCC
- `QA2Df()`: Dataframe quality assurance
- Trial item diagnostics after core anchoring

### v1.0.3

- Fixed data reading order in `DIF_dim_one()`
- `poly_recode()`: Outputs recoding report
- `freq_resps_cat()`: Handles NA and letter responses
- ConQuest .xls format support
- Added fit indexes to DIF/equating
- Floating TOC in equating reports
- SE reference lines in delta plots
- Removed anchor.xlsx and keys.xlsx requirements

### v1.0.2

- Suppressed unnecessary console output
- `freq_resps_cat()`: Ordered categories, added totals and proportions
- Case weights support via `pweight`
- Covariate support with `n_dims` parameter
- Changed file extensions to .txt
- Improved pipe operator usage
- Enhanced polytomous DIF display

### v1.0.1

- `section_model()`: Correct group model for polytomous DIF
- `DIF_poly_shw()`: Numeric delta extraction
- `plot_DIF_group()`: Polytomous item plotting
- `plot_DIF_poly()`: Up to 30 overlapping labels
- `DIF_dich()`: Reads ConQuest v5.27.0 facility columns

## Folder Structure

After running `create_folders()`:

```
project/
├── data/
│   └── [run]/
│       └── [test]/
├── input/
│   └── [run]/
├── calibration/
│   └── [run]/
├── DIF/
│   └── [DIFVar]/
├── rCode/
└── results/
```

## Contributing

Please report issues and contribute via the package repository.

## License

Copyright (c) 2022 Xiaoliang Zhou, Vernon Mogol

## Citation

If you use RaschKit in your research, please cite:

```
Zhou, X., & Mogol, V. (2022). RaschKit: R Package for Rasch Modeling and Item Analysis. 
R package version 1.1.3.
```

BibTeX entry:

```bibtex
@Manual{raschkit,
  title = {RaschKit: R Package for Rasch Modeling and Item Analysis},
  author = {Xiaoliang Zhou and Vernon Mogol},
  year = {2022},
  note = {R package version 1.1.3},
}
```

## Contact

**Maintainer:** Xiaoliang Zhou

**Email:** xiaoliang.zhou@acer.org

**Organization:** Australian Council for Educational Research (ACER)

For bug reports, feature requests, or questions, please contact the maintainer or open an issue on the package repository.