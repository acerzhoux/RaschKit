This README.md is designed to be the "front door" of your GitHub repository. It clearly communicates what the package does, how to install it, and how the versioning aligns with ACER ConQuest.RaschKit <img src="https://www.r-project.org/logo/Rlogo.png" align="right" height="139" />RaschKit is an R package developed to streamline psychometric workflows, providing automated tools for calibration, equating, and Differential Item Functioning (DIF) analysis. It is specifically designed to integrate with ACER ConQuest and produce industry-standard reports in the ACER template format.Key Features🚀 Full Automation: Consolidated workflows for calibration and reporting via .Rmd templates.📊 ACER-Standard Reporting: Automated generation of Excel and Word reports with embedded plots, colored flags, and formatted tables.🔗 Smart Pathing: Persistent hyperlinking and dynamic folder structures (input, data, calibration, DIF) to keep projects organized.📈 Advanced Visuals: Detailed Item Characteristic Curves (ICC), delta scatterplots with $1.96 \times SE$ bounds, and Category Characteristic Curves (CCC).🛠️ Cross-Compatibility: Bridges different versions of ACER ConQuest (v3.4 through v5.47.5).⚙️ InstallationTo install RaschKit and all required dependencies, run the following block in your R console:R# 1. Install required dependencies
pkgs <- c('rlang','bookdown', 'ggthemes', 'ggrepel', 'patchwork','rmarkdown',  
          'gdata', 'janitor', 'data.table','knitr', 'RColorBrewer', 'fs',  
          'lazyeval', 'writexl', 'ggpubr', 'qpdf', 'tidyverse', 'openxlsx',  
          'kableExtra', 'magrittr', 'Rcpp', 'tidyselect', 'grid', 'flextable', 'magick')

invisible(lapply(pkgs, function(x)){  
  if (!require(x, character.only=TRUE)){  
    install.packages(x, dependencies = TRUE)
  }
}))

# 2. Install RaschKit from local source or GitHub
# devtools::install_github("acerzhoux/RaschKit") 
library(RaschKit)

# 3. Initialize your project directory
create_folders()
📋 Software Compatibility MatrixEnsure you are using the correct version of RaschKit for your ConQuest installation:ConQuest Versionconquestr VersionRaschKit Versionv5.47.5v1.5.5v1.1.3v5.33.5v1.1.0v1.0.7v5.27.0v1.0.1v1.0.1v5.23.4v0.9.995v1.0.0🛠 WorkflowInitialize: Run create_folders() to set up the standard directory.Explore: Use the templates in the rCode folder for initial data exploration.Calibrate: Use calibrateLst() for batch processing of response dataframes.Report: Reports are automatically generated using ACER templates, including:itn_run.xlsx: Comprehensive item analysis and flag summaries.DIF Analysis: Side-by-side item and step DIF summaries.Equating: Scatterplots with regression lines and fit statistics.📝 Recent Updates (v1.1.3)Folder Logic: Introduced run sub-folders to allow multiple iterations without overwriting data.Dependency Management: Cleaned up namespaces (removed plyr) to prevent conflicts with tidyverse.Stability: Updated internal logic to use .data[[DIFVar]] for more robust plot rendering.Reports: Item analysis summary files now include dedicated sheets for flag notes and flagged items.For a full history of changes, see the NEWS.md file.📧 ContactXiaoliang Psychometrics and Methodology Team