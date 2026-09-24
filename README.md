# Maelstrom

Multi-species predictive stock assessment model based on a customizable neural
network and built as an R Shiny application.

Associated publication:
[MAELSTROM, a machine learning-based approach for stock assessment](https://doi.org/10.3389/fmars.2026.1873011).

### Prerequisites

1. Select **Code → Download ZIP** from the
   [GitHub repository](https://github.com/rlabtorvergata/Maelstrom).
2. Extract the complete archive into a new folder.
3. Install a recent version of R (R >= 4.3 is recommended) and RStudio.
4. Open the extracted application folder in RStudio.
5. In a fresh R session, run:

```r
Sys.setenv(MAELSTROM_INSTALL_KERAS = "true")
source("bootstrap.R")
```

The script installs the required R packages and, on the first installation,
the Keras/TensorFlow backend. Restart R after the installation.

To start the application, run:

```r
shiny::runApp()
```

Alternatively, open `server.R` or `ui.R` and click **Run App** in RStudio.

> **Important**
>
> If FLCore or the Keras backend cannot be installed automatically, run:

```r
install.packages(
  "FLCore",
  repos = c("https://flr.r-universe.dev", "https://cloud.r-project.org")
)

install.packages("keras3")
keras3::install_keras(backend = "tensorflow")
```

Further instructions are available inside the application through the
**Instruction** button.

> **Note**
>
> Exporting the final PDF report requires a LaTeX distribution such as MiKTeX
> or TinyTeX. TinyTeX can be installed from R with:

```r
tinytex::install_tinytex()
```

### Data formatting

Input files must contain a valid FLStock object and may use `.rds`, `.RData`
or `.rda` format.

Stock filenames must follow the original convention:

```text
3ACODE_GSA1-GSA2-GSA3.rds
```

For example:

```text
DPS_9-10-11.rds
```

Sample stock objects are available in the `Sample Data` directory.

Optional fishing-mortality schedules must contain forecast years in rows and
stock/cohort F features in columns. If the schedule is shorter than the
forecast, its final row is carried forward; unused additional rows are ignored.