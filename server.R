##### LIBRARIES #####

{library(bslib)
  library(cowplot)
  library(data.table)
  library(dplyr)
  library(FLCore)
  library(ggplot2)
  library(keras3)
  library(Metrics)
  library(patchwork)
  library(plotly)
  library(plotrix)
  library(reshape2)
  library(rmarkdown)
  library(shiny)
  library(shinyBS)
  library(shinycssloaders)
  library(shinyFiles)
  library(shinyWidgets)
  library(tensorflow)
  library(tidyr)
  library(tinytex)
  }

##### OPTIONS #####

options(max.print = 99999)

##### SERVER LOGIC #####

server <- function(input, output, session) {

  ##### VARIABLES #####
  
  app_version <- "1.2.0"
  base_seed <- 123L
  ensemble_iterations <- 30L
  maximum_lookback <- 5L
  validation_fraction <- 0.20
  minimum_preferred_training_samples <- 8L
  absolute_minimum_training_samples <- 6L
  minimum_validation_samples <- 2L
  minimum_years_for_temporal_test <- 9L
  keras3::set_random_seed(base_seed)

  species <- list()
  gsa <- list()
  gsa_tot <- vector()
  rv <- list()

  pops <- list()
  pops_l <- data.frame()
  pops_w <- data.frame()
  catches <- list()
  catches_l <- data.frame()
  catches_w <- data.frame()
  waa <- list()
  waa_l <- data.frame()
  waa_w <- data.frame()
  fmorts <- list()
  fmort_l <- data.frame()
  fmort_w <- data.frame()
  fmort_spawns <- list()
  fmort_spawn_l <- data.frame()
  fmort_spawn_w <- data.frame()
  morts <- list()
  mort_l <- data.frame()
  mort_w <- data.frame()
  mort_spawns <- list()
  mort_spawn_l <- data.frame()
  mort_spawn_w <- data.frame()
  matures <- list()
  mature_l <- data.frame()
  mature_w <- data.frame()

  neuralNetInputs <- data.frame()
  f_w <- data.frame()
  fmort_baseline <- NULL
  f_new <- data.frame()
  f_adj <- data.frame()
  f_tot <- data.frame()
  f_adj_display <- data.frame()

  range_inputs <- data.frame()
  range_outputs <- data.frame()

  depth_test <- NULL
  plotTestCount <- 0
  testfit_results <- data.frame()
  traintest_output_raw <- list()
  traintest_iter_results <- list()
  traintest_metrics <- data.frame()
  traintest_metrics_plot <- NULL
  traintest_nparams <- 0
  traintest_results <- data.frame()
  traintest_plots <- list()
  traintest_recr_plots <- list()
  taylor_diagram <- list()

  depth_pred <- NULL
  plotPredCount <- 0
  forecast_iterations <- ensemble_iterations
  pred_output_raw <- vector("list", length = forecast_iterations)
  model_pred <- list()
  pred_iter_partial <- data.frame()
  pred_results <- list()
  f_applied <- data.frame()
  pred_plots <- list()
  pred_recr_plots <- list()

  sens_results <- data.frame()
  sens_plots <- list()
  save_list <- list()
  vol <- shinyFiles::getVolumes()()

  message(sprintf(
    "Starting MAELSTROM %s from %s (R %s; keras3 %s; tensorflow %s)",
    app_version,
    normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    paste(R.version$major, R.version$minor, sep = "."),
    as.character(utils::packageVersion("keras3")),
    as.character(utils::packageVersion("tensorflow"))
  ))
  
  ##### FUNCTIONS #####

  formatRuntimeError <- function(error, action) {
    error_message <- conditionMessage(error)
    if (startsWith(error_message, "[MAELSTROM ")) return(error_message)

    error_call <- conditionCall(error)
    call_text <- if (is.null(error_call)) {
      "unavailable"
    } else {
      paste(deparse(error_call, width.cutoff = 160L), collapse = " ")
    }
    sprintf(
      "[MAELSTROM %s | %s] %s | originating call: %s",
      app_version, action, error_message, call_text
    )
  }

  withRuntimeStage <- function(stage, expression) {
    tryCatch(
      force(expression),
      error = function(error) {
        stop(formatRuntimeError(error, stage), call. = FALSE)
      }
    )
  }
  
  speciesInfo <- function(triAlphaCode) {
    if (triAlphaCode == "ANE") {
      text <- HTML("<b>Scientific Name</b>: <i>Engraulis encrasicolus</i><br><b>Depth Range</b>: 0-400m")
    } else if (triAlphaCode == "ANK") {
      text <- HTML("<b>Scientific Name</b>: <i>Lophius budegassa</i><br><b>Depth Range</b>: 100-500m")
    } else if (triAlphaCode == "ARA") {
      text <- HTML("<b>Scientific Name</b>: <i>Aristeus antennatus</i><br><b>Depth Range</b>: 350-800m")
    } else if (triAlphaCode == "ARS") {
      text <- HTML("<b>Scientific Name</b>: <i>Aristaeomorpha foliacea</i><br><b>Depth Range</b>: 60-1300m")
    } else if (triAlphaCode == "BSS") {
      text <- HTML("<b>Scientific Name</b>: <i>Dicentrarchus labrax</i><br><b>Depth Range</b>: 10-100m")
    } else if (triAlphaCode == "DGS") {
      text <- HTML("<b>Scientific Name</b>: <i>Squalus acanthias</i><br><b>Depth Range</b>: 0-1460m")
    } else if (triAlphaCode == "DPS") {
      text <- HTML("<b>Scientific Name</b>: <i>Parapenaeus longirostris</i><br><b>Depth Range</b>: 20-700m")
    } else if (triAlphaCode == "GFB") {
      text <- HTML("<b>Scientific Name</b>: <i>Phycis blennoides</i><br><b>Depth Range</b>: 10-1200m")
    } else if (triAlphaCode == "HKE") {
      text <- HTML("<b>Scientific Name</b>: <i>Merluccius merluccius</i><br><b>Depth Range</b>: 70-400m")
    } else if (triAlphaCode == "HMM") {
      text <- HTML("<b>Scientific Name</b>: <i>Trachurus mediterraneus</i><br><b>Depth Range</b>: 0-500m")
    } else if (triAlphaCode == "HOM") {
      text <- HTML("<b>Scientific Name</b>: <i>Trachurus trachurus</i><br><b>Depth Range</b>: 0-1500m")
    } else if (triAlphaCode == "MON") {
      text <- HTML("<b>Scientific Name</b>: <i>Lophius piscatorius</i><br><b>Depth Range</b>: 20-1000m")
    } else if (triAlphaCode == "MTS") {
      text <- HTML("<b>Scientific Name</b>: <i>Squilla mantis</i><br><b>Depth Range</b>: ?-120m")
    } else if (triAlphaCode == "MUR") {
      text <- HTML("<b>Scientific Name</b>: <i>Mullus surmuletus</i><br><b>Depth Range</b>: 5-400m")
    } else if (triAlphaCode == "MUT") {
      text <- HTML("<b>Scientific Name</b>: <i>Mullus barbatus</i><br><b>Depth Range</b>: 10-320m")
    } else if (triAlphaCode == "NEP") {
      text <- HTML("<b>Scientific Name</b>: <i>Nephrops norvegicus</i><br><b>Depth Range</b>: 200-600m")
    } else if (triAlphaCode == "PAC") {
      text <- HTML("<b>Scientific Name</b>: <i>Pagellus erythrinus</i><br><b>Depth Range</b>: 20-200m")
    } else if (triAlphaCode == "PIL") {
      text <- HTML("<b>Scientific Name</b>: <i>Sardina pilchardus</i><br><b>Depth Range</b>: 10-100m")
    } else if (triAlphaCode == "POD") {
      text <- HTML("<b>Scientific Name</b>: <i>Trisopterus minutus</i><br><b>Depth Range</b>: 0-440m")
    } else if (triAlphaCode == "RJC") {
      text <- HTML("<b>Scientific Name</b>: <i>Raja clavata</i><br><b>Depth Range</b>: 10-1020m")
    } else if (triAlphaCode == "RPW") {
      text <- HTML("<b>Scientific Name</b>: <i>Rapana venosa</i><br><b>Depth Range</b>: 10-60m")
    } else if (triAlphaCode == "SBG") {
      text <- HTML("<b>Scientific Name</b>: <i>Sparus aurata</i><br><b>Depth Range</b>: 0-150m")
    } else if (triAlphaCode == "SOL") {
      text <- HTML("<b>Scientific Name</b>: <i>Solea solea</i><br><b>Depth Range</b>: 0-150m")
    } else if (triAlphaCode == "SPC") {
      text <- HTML("<b>Scientific Name</b>: <i>Spicara smaris</i><br><b>Depth Range</b>: 15-320m")
    } else if (triAlphaCode == "SPR") {
      text <- HTML("<b>Scientific Name</b>: <i>Sprattus sprattus</i><br><b>Depth Range</b>: 10-150m")
    } else if (triAlphaCode == "TUR") {
      text <- HTML("<b>Scientific Name</b>: <i>Psetta maxima</i><br><b>Depth Range</b>: 20-70m")
    } else if (triAlphaCode == "WHB") {
      text <- HTML("<b>Scientific Name</b>: <i>Micromesistius poutassou</i><br><b>Depth Range</b>: 150-3000m")
    } else if (triAlphaCode == "WHG") {
      text <- HTML("<b>Scientific Name</b>: <i>Merlangius merlangus</i><br><b>Depth Range</b>: 10-200m")
    } else {
      text <- HTML("Not a Mediterranean species")
    }
    return(text)
  }
  
  gsaInfo <- function(gsa) {
    gsa_list <- vector()
    text <- HTML("")
    if ("1" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 1</b>: Northern Alboran Sea"
      }
    if ("2" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 2</b>: Alboran Island"
      }
    if ("3" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 3</b>: Southern Alboran Sea"
      }
    if ("4" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 4</b>: Algeria"
      }
    if ("5" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 5</b>: Balearic Islands"
      }
    if ("6" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 6</b>: Northern Spain"
      }
    if ("7" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 7</b>: Gulf of Lion"
      }
    if ("8" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 8</b>: Corsica"
      }
    if ("9" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 9</b>: Ligurian Sea and Northern Tyrrhenian Sea"
      }
    if ("10" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 10</b>: Southern and Central Tyrrhenian Sea"
      }
    if ("11" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 11</b>: Sardinia"
      }
    if ("12" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 12</b>: Northern Tunisia"
      }
    if ("13" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 13</b>: Gulf of Hammamet"
      }
    if ("14" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 14</b>: Gulf of Gabes"
      }
    if ("15" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 15</b>: Malta"
      }
    if ("16" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 16</b>: Southern Sicily"
      }
    if ("17" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 17</b>: Northern Adriatic Sea"
      }
    if ("18" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 18</b>: Southern Adriatic Sea"
      }
    if ("19" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 19</b>: Western Ionian Sea"
      }
    if ("20" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 20</b>: Eastern Ionian Sea"
      }
    if ("21" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 21</b>: Southern Ionian Sea"
      }
    if ("22" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 22</b>: Aegean Sea"
      }
    if ("23" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 23</b>: Crete"
      }
    if ("24" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 24</b>: Northern Levant Sea"
      }
    if ("25" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 25</b>: Cyprus"
      }
    if ("26" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 26</b>: Southern Levant Sea"
      }
    if ("27" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 27</b>: Eastern Levant Sea"
      }
    if ("28" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 28</b>: Marmara Sea"
      }
    if ("29" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 29</b>: Black Sea"
      }
    if ("30" %in% gsa) {
      gsa_list[(length(gsa_list) + 1)] <- "<b>GSA 30</b>: Azov Sea"
    }
    if (length(gsa_list) == 0) {
      gsa_list[(length(gsa_list) + 1)] <- "Not a Mediterranean area"
    }
    text <- HTML(paste(gsa_list, collapse = "<br/>"))
    return(text)
  }
  
  loadRData <- function(fileName) {
    load_env <- new.env(parent = emptyenv())
    object_names <- load(fileName, envir = load_env)
    stock_names <- object_names[vapply(object_names, function(object_name) {
      inherits(load_env[[object_name]], "FLStock")
    }, logical(1))]
    if (length(stock_names) != 1L) {
      stop(
        "Each .RData/.rda file must contain exactly one FLStock object.",
        call. = FALSE
      )
    }
    load_env[[stock_names[[1L]]]]
  }

  validateStockObject <- function(stock, file_name = "uploaded file") {
    if (!inherits(stock, "FLStock")) {
      stop(sprintf("%s does not contain an FLStock object.", file_name), call. = FALSE)
    }

    stock_dims <- dim(FLCore::stock.n(stock))
    if (length(stock_dims) != 6L || any(stock_dims[3:6] != 1L)) {
      stop(
        sprintf(
          "%s must contain a single unit, season, area and iteration.",
          file_name
        ),
        call. = FALSE
      )
    }
    if (stock_dims[2L] < 6L) {
      stop(sprintf("%s must contain at least six years.", file_name), call. = FALSE)
    }

    required_slots <- list(
      stock.n = FLCore::stock.n(stock),
      catch.n = FLCore::catch.n(stock),
      stock.wt = FLCore::stock.wt(stock),
      harvest = FLCore::harvest(stock),
      m = FLCore::m(stock),
      mat = FLCore::mat(stock),
      harvest.spwn = FLCore::harvest.spwn(stock),
      m.spwn = FLCore::m.spwn(stock)
    )
    invalid <- names(required_slots)[!vapply(required_slots, function(slot_value) {
      values <- as.numeric(slot_value)
      length(values) > 0L && all(is.finite(values))
    }, logical(1))]
    if (length(invalid)) {
      stop(
        sprintf("%s has missing or non-finite values in: %s.",
                file_name, paste(invalid, collapse = ", ")),
        call. = FALSE
      )
    }

    nonnegative_slots <- required_slots[c("stock.n", "catch.n", "stock.wt", "harvest", "m")]
    negative <- names(nonnegative_slots)[vapply(nonnegative_slots, function(slot_value) {
      any(as.numeric(slot_value) < 0)
    }, logical(1))]
    if (length(negative)) {
      stop(sprintf("%s has negative values in: %s.",
                   file_name, paste(negative, collapse = ", ")), call. = FALSE)
    }
    if (any(as.numeric(required_slots$mat) < 0 | as.numeric(required_slots$mat) > 1) ||
        any(as.numeric(required_slots$harvest.spwn) < 0 | as.numeric(required_slots$harvest.spwn) > 1) ||
        any(as.numeric(required_slots$m.spwn) < 0 | as.numeric(required_slots$m.spwn) > 1)) {
      stop(sprintf("%s has mat or spawning-timing values outside [0, 1].", file_name),
           call. = FALSE)
    }

    asAgeYearMatrix <- function(flq) {
      array_value <- as.array(flq)
      matrix(array_value[, , 1, 1, 1, 1],
             nrow = dim(array_value)[1L], ncol = dim(array_value)[2L])
    }
    manual_ssb <- colSums(
      asAgeYearMatrix(required_slots$stock.n) *
        asAgeYearMatrix(required_slots$stock.wt) *
        asAgeYearMatrix(required_slots$mat) *
        exp(-(
          asAgeYearMatrix(required_slots$harvest) *
            asAgeYearMatrix(required_slots$harvest.spwn) +
            asAgeYearMatrix(required_slots$m) *
            asAgeYearMatrix(required_slots$m.spwn)
        ))
    )
    flcore_ssb <- as.numeric(FLCore::ssb(stock))
    relative_error <- max(abs(manual_ssb - flcore_ssb) / pmax(1, abs(flcore_ssb)))
    if (!is.finite(relative_error) || relative_error > 1e-7) {
      stop(sprintf("%s failed the SSB consistency check against FLCore::ssb().", file_name),
           call. = FALSE)
    }

    invisible(stock)
  }

  loadStockFile <- function(path, original_name) {
    extension <- tolower(tools::file_ext(original_name))
    stock <- switch(
      extension,
      rds = readRDS(path),
      rdata = loadRData(path),
      rda = loadRData(path),
      stop("Supported formats are .rds, .RData and .rda.", call. = FALSE)
    )
    validateStockObject(stock, original_name)
    stock
  }

  numberMultiplier <- function(stock_quant) {
    unit_label <- tolower(trimws(as.character(units(stock_quant))[1L]))
    if (is.na(unit_label) || unit_label %in% c("", "na")) {
      warning("Missing abundance units: retaining the v1 assumption of thousands.",
              call. = FALSE)
      return(1000)
    }
    if (grepl("thousand|1000|10\\^3", unit_label)) return(1000)
    if (unit_label %in% c("1", "number", "numbers", "individual", "individuals")) return(1)
    stop(sprintf("Unsupported abundance unit '%s'.", unit_label), call. = FALSE)
  }

  weightMultiplierToTonnes <- function(weight_quant) {
    unit_label <- tolower(trimws(as.character(units(weight_quant))[1L]))
    if (is.na(unit_label) || unit_label %in% c("", "na")) {
      warning("Missing stock.wt units: retaining the v1 assumption of kilograms.",
              call. = FALSE)
      return(1 / 1000)
    }
    if (grepl("tonne|ton|^t$", unit_label)) return(1)
    if (grepl("kilogram|kg", unit_label)) return(1 / 1000)
    if (grepl("gram|^g$", unit_label)) return(1 / 1e6)
    stop(sprintf("Unsupported stock.wt unit '%s'.", unit_label), call. = FALSE)
  }
  
  asFiniteNumericMatrix <- function(data, context = "Neural-network data") {
    data <- as.data.frame(data, check.names = FALSE)
    if (!nrow(data) || !ncol(data)) {
      stop(paste(context, "must be a non-empty table."), call. = FALSE)
    }

    numeric_columns <- lapply(seq_along(data), function(column_index) {
      column <- data[[column_index]]
      if (is.factor(column)) column <- as.character(column)
      if (is.list(column)) {
        if (any(lengths(column) != 1L)) {
          stop(
            sprintf("%s column '%s' contains non-scalar list values.",
                    context, names(data)[column_index]),
            call. = FALSE
          )
        }
        column <- unlist(column, recursive = FALSE, use.names = FALSE)
      }
      numeric_column <- suppressWarnings(as.numeric(column))
      if (length(numeric_column) != nrow(data) || any(!is.finite(numeric_column))) {
        stop(
          sprintf("%s column '%s' is not entirely finite and numeric.",
                  context, names(data)[column_index]),
          call. = FALSE
        )
      }
      numeric_column
    })

    numeric_matrix <- do.call(cbind, numeric_columns)
    if (!is.matrix(numeric_matrix)) numeric_matrix <- matrix(numeric_matrix, ncol = 1L)
    storage.mode(numeric_matrix) <- "double"
    colnames(numeric_matrix) <- names(data)
    numeric_matrix
  }

  normalizationRangeMatrix <- function(range_df, expected_names) {
    range_matrix <- asFiniteNumericMatrix(range_df, "Normalization range")
    if (nrow(range_matrix) != 2L) {
      stop("Normalization range must contain exactly two rows (minimum and maximum).",
           call. = FALSE)
    }
    if (!identical(colnames(range_matrix), expected_names)) {
      stop("Normalization range columns do not match the supplied neural-network data.",
           call. = FALSE)
    }
    range_matrix
  }

  normalizeInputs <- function(source_df) {
    source_matrix <- asFiniteNumericMatrix(source_df, "Neural-network inputs")
    minima <- apply(source_matrix, 2L, min)
    maxima <- apply(source_matrix, 2L, max)
    spans <- maxima - minima
    safe_spans <- ifelse(spans == 0, 1, spans)
    normalized_matrix <- sweep(sweep(source_matrix, 2L, minima, "-"),
                               2L, safe_spans, "/")
    if (any(spans == 0)) normalized_matrix[, spans == 0] <- 0

    normalized <- as.data.frame(normalized_matrix, check.names = FALSE)
    range <- as.data.frame(rbind(minimum = minima, maximum = maxima),
                           check.names = FALSE)
    list(values = normalized, range = range)
  }

  normalizeUsingRange <- function(source_df, range_df) {
    source_matrix <- asFiniteNumericMatrix(source_df, "Neural-network inputs")
    range_matrix <- normalizationRangeMatrix(range_df, colnames(source_matrix))
    minima <- range_matrix[1L, ]
    maxima <- range_matrix[2L, ]
    spans <- maxima - minima
    safe_spans <- ifelse(spans == 0, 1, spans)
    normalized <- as.data.frame(
      sweep(sweep(source_matrix, 2L, minima, "-"), 2L, safe_spans, "/"),
      check.names = FALSE
    )
    if (any(spans == 0)) normalized[, spans == 0] <- 0
    normalized
  }
  
  denormalizeInputs <- function(source_df, range_df) {
    source_matrix <- asFiniteNumericMatrix(source_df, "Normalized neural-network data")
    range_matrix <- normalizationRangeMatrix(range_df, colnames(source_matrix))
    minima <- range_matrix[1L, ]
    spans <- range_matrix[2L, ] - minima
    as.data.frame(
      sweep(sweep(source_matrix, 2L, spans, "*"), 2L, minima, "+"),
      check.names = FALSE
    )
  }

  buildSequenceSamples <- function(feature_matrix, target_matrix = NULL,
                                   maximum_lookback = 5L) {
    feature_matrix <- asFiniteNumericMatrix(
      feature_matrix, "Temporal neural-network data"
    )
    if (is.null(target_matrix)) target_matrix <- feature_matrix
    target_matrix <- asFiniteNumericMatrix(
      target_matrix, "Temporal neural-network targets"
    )
    if (nrow(feature_matrix) < 3L || ncol(feature_matrix) < 1L ||
        any(!is.finite(feature_matrix))) {
      stop("At least three finite yearly observations are required.", call. = FALSE)
    }
    if (nrow(target_matrix) != nrow(feature_matrix)) {
      stop("Temporal inputs and targets must contain the same years.", call. = FALSE)
    }
    lookback <- min(as.integer(maximum_lookback), nrow(feature_matrix) - 1L)
    n_samples <- nrow(feature_matrix) - lookback

    # Build the tensor through its linear storage. This deliberately avoids a
    # three-subscript replacement such as x[i, , ] <- ..., which can fail when
    # an R/package combination simplifies a one-sample or one-step array to a
    # matrix. The resulting layout is still samples x timesteps x features.
    x_values <- numeric(n_samples * lookback * ncol(feature_matrix))
    sample_rows <- seq_len(n_samples)
    for (feature_index in seq_len(ncol(feature_matrix))) {
      feature_offset <- (feature_index - 1L) * n_samples * lookback
      for (lag_index in seq_len(lookback)) {
        tensor_positions <- feature_offset +
          (lag_index - 1L) * n_samples + sample_rows
        x_values[tensor_positions] <- feature_matrix[
          sample_rows + lag_index - 1L, feature_index
        ]
      }
    }

    x <- base::array(
      x_values,
      dim = c(n_samples, lookback, ncol(feature_matrix))
    )
    y <- target_matrix[
      lookback + sample_rows, seq_len(ncol(target_matrix)), drop = FALSE
    ]
    prediction_x <- base::array(
      as.numeric(utils::tail(feature_matrix, lookback)),
      dim = c(1L, lookback, ncol(feature_matrix))
    )
    if (length(dim(x)) != 3L || length(dim(prediction_x)) != 3L ||
        !identical(dim(x), c(n_samples, lookback, ncol(feature_matrix)))) {
      stop("Internal temporal tensor construction returned invalid dimensions.",
           call. = FALSE)
    }
    list(x = x, y = y, prediction_x = prediction_x, lookback = lookback)
  }

  buildPredictionWindow <- function(feature_matrix, lookback) {
    feature_matrix <- asFiniteNumericMatrix(feature_matrix, "Forecast state")
    lookback <- as.integer(lookback)
    if (!is.finite(lookback) || lookback < 1L || nrow(feature_matrix) < lookback) {
      stop("The forecast state is shorter than the fitted temporal lookback.",
           call. = FALSE)
    }
    prediction_window <- base::array(
      as.numeric(utils::tail(feature_matrix, lookback)),
      dim = c(1L, lookback, ncol(feature_matrix))
    )
    if (length(dim(prediction_window)) != 3L) {
      stop("The forecast window could not be preserved as a three-dimensional tensor.",
           call. = FALSE)
    }
    prediction_window
  }

  coercePredictionRow <- function(prediction, feature_names) {
    prediction_values <- as.numeric(prediction)
    expected_values <- length(feature_names)
    if (length(prediction_values) != expected_values ||
        any(!is.finite(prediction_values))) {
      stop(
        sprintf(
          paste(
            "The neural network returned %s finite output values;",
            "%s were expected."
          ),
          sum(is.finite(prediction_values)), expected_values
        ),
        call. = FALSE
      )
    }
    matrix(
      prediction_values,
      nrow = 1L,
      ncol = expected_values,
      dimnames = list(NULL, feature_names)
    )
  }

  toModelScale <- function(values) {
    values <- as.data.frame(values, check.names = FALSE)
    if (identical(input$activation, "tanh")) {
      values[] <- as.matrix(values) - 0.5
    }
    values
  }

  fromModelScale <- function(values) {
    values <- as.data.frame(values, check.names = FALSE)
    if (identical(input$activation, "tanh")) {
      values[] <- as.matrix(values) + 0.5
    }
    values
  }
  
  procGSA <- function(gsa) {
    paste(as.character(gsa), collapse = "_")
  }
  
  procDfLongQuant <- function(stock, gsa, tri, minAge, baselineAge, baselineYear, fun, var) {
    if (baselineAge < stock@range["max"]) {
      stock <- FLCore::setPlusGroup(stock, plusgroup = as.numeric(baselineAge))
    }
    stk_temp = fun(stock)
    df_temp = as.data.frame(stk_temp)[, c("year", "age", "data")]
    df_temp = df_temp[which(df_temp$year >= baselineYear),]
    df = df_temp[, 1:2]
    df[, 3] = df_temp[, 3] * numberMultiplier(stk_temp)
    df[, 4] = procGSA(paste0(gsa, collapse = "-"))
    df[, 5] = tri
    df[, 6] = paste(tri, paste(paste0(gsa, collapse = "-"), collapse = "_"), sep = "_")
    colnames(df) <- c("year", "age", as.character(substitute(var)), "gsa", "species", "tri_gsa")
    return(df)
  }
  
  procDfLongMult <- function(stock, gsa, tri, minAge, baselineAge, baselineYear, fun, var) {
    if (baselineAge < stock@range["max"]) {
      stock <- FLCore::setPlusGroup(stock, plusgroup = as.numeric(baselineAge))
    }
    stk_temp = fun(stock)
    df_temp = as.data.frame(stk_temp)[, c("year", "age", "data")] #Thousands
    df_temp = df_temp[which(df_temp$year >= baselineYear),]
    df = df_temp[, 1:2]
    multiplier <- if (identical(as.character(substitute(var)), "weight_at_age")) {
      weightMultiplierToTonnes(stk_temp)
    } else {
      1
    }
    df[, 3] = df_temp[, 3] * multiplier
    df[, 4] = procGSA(paste0(gsa, collapse = "-"))
    df[, 5] = tri
    df[, 6] = paste(tri, paste(paste0(gsa, collapse = "-"), collapse = "_"), sep = "_")
    colnames(df) <- c("year", "age", as.character(substitute(var)), "gsa", "species", "tri_gsa")
    return(df)
  }
  
  procDfWide <- function(dflong, var, code) {
    variable_label <- as.character(substitute(var))
    code_label <- as.character(substitute(code))
    df_w <- dcast(
      data = dflong,
      paste0("year + ", variable_label, " ~ tri_gsa + age")
    )
    values <- as.numeric(df_w[[2L]])
    data_columns <- seq.int(3L, ncol(df_w))
    df_w[data_columns] <- lapply(df_w[data_columns], function(column) {
      ifelse(is.na(column), NA_real_, values)
    })
    df_w[[2L]] <- NULL

    original_names <- colnames(df_w)[-1L]
    colnames(df_w)[-1L] <- vapply(original_names, function(original_name) {
      parts <- strsplit(original_name, "_", fixed = TRUE)[[1L]]
      paste(parts[[1L]], parts[[length(parts)]], code_label,
            parts[[2L]], sep = "_")
    }, character(1))

    df_w[is.na(df_w)] <- 0
    df_w <- mutate_all(df_w, function(x) as.numeric(as.character(x)))
    df_w <- aggregate(. ~ year, df_w, FUN = sum)
    return(df_w)
  }

  parseFeatureMetadata <- function(feature_names, variable_code = "N") {
    pattern <- sprintf("^([^_]+)_([^_]+)_%s_(.+)$", variable_code)
    matches <- regexec(pattern, feature_names)
    parts <- regmatches(feature_names, matches)
    if (any(lengths(parts) != 4L)) {
      stop("Unexpected neural-network feature names.", call. = FALSE)
    }
    data.frame(
      species = vapply(parts, `[[`, character(1), 2L),
      age = vapply(parts, `[[`, character(1), 3L),
      gsa = vapply(parts, `[[`, character(1), 4L),
      stringsAsFactors = FALSE
    )
  }

  populationFeatureMetadata <- function(net_inputs) {
    feature_names <- grep("_N_", names(net_inputs), value = TRUE)
    if (!length(feature_names)) {
      stop(
        paste(
          "No population-abundance features were found in the neural-network input.",
          "Expected column names containing '_N_'."
        ),
        call. = FALSE
      )
    }
    metadata <- parseFeatureMetadata(feature_names, "N")
    age_numeric <- suppressWarnings(as.numeric(gsub("\\+", "", metadata$age)))
    if (any(!is.finite(age_numeric))) {
      stop("Population feature ages must be numeric.", call. = FALSE)
    }
    metadata$age_numeric <- age_numeric
    metadata
  }

  totDf <- function(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10) {
    df_names <- substr(colnames(df1)[2], 1, (nchar(df1) - 1))
    if (missing(df2)) {
      tot_df <- df1
      years <- df1$year
    }
    if (!missing(df2)) {
      tot_df <- rbind(df1, df2)
      years <- intersect(df1$year, df2$year)
    }
    if (!missing(df3)) {
      tot_df <- rbind(tot_df, df3)
      years <- intersect(years, df3$year)
    }
    if (!missing(df4)) {
      tot_df <- rbind(tot_df, df4)
      years <- intersect(years, df4$year)
    }
    if (!missing(df5)) {
      tot_df <- rbind(tot_df, df5)
      years <- intersect(years, df5$year)
    }
    if (!missing(df6)) {
      tot_df <- rbind(tot_df, df6)
      years <- intersect(years, df6$year)
    }
    if (!missing(df7)) {
      tot_df <- rbind(tot_df, df7)
      years <- intersect(years, df7$year)
    }
    if (!missing(df8)) {
      tot_df <- rbind(tot_df, df8)
      years <- intersect(years, df8$year)
    }
    if (!missing(df9)) {
      tot_df <- rbind(tot_df, df9)
      years <- intersect(years, df9$year)
    }
    if (!missing(df10)) {
      tot_df <- rbind(tot_df, df10)
      years <- intersect(years, df10$year)
    }
    tot_df <- tot_df[tot_df$year %in% years, ]
    return(tot_df)
  }
  
  catchBaranov <- function(fmortwide, mortwide, popwide) {
    population_matrix <- asFiniteNumericMatrix(popwide, "Forecast population")
    fishing_matrix <- asFiniteNumericMatrix(fmortwide, "Forecast fishing mortality")
    natural_matrix <- asFiniteNumericMatrix(mortwide, "Forecast natural mortality")

    if (nrow(population_matrix) != 1L ||
        nrow(fishing_matrix) != 1L ||
        nrow(natural_matrix) != 1L) {
      stop("Baranov forecast inputs must each contain exactly one row.", call. = FALSE)
    }
    if (ncol(fishing_matrix) != ncol(population_matrix) ||
        ncol(natural_matrix) != ncol(population_matrix)) {
      stop(
        "Population, fishing mortality and natural mortality have different age dimensions.",
        call. = FALSE
      )
    }

    population_metadata <- parseFeatureMetadata(colnames(population_matrix), "N")
    fishing_metadata <- parseFeatureMetadata(colnames(fishing_matrix), "F")
    natural_metadata <- parseFeatureMetadata(colnames(natural_matrix), "M")
    canonicalMetadata <- function(metadata) {
      metadata$gsa <- gsub("\\.", "-", metadata$gsa)
      metadata
    }
    if (!identical(canonicalMetadata(population_metadata),
                   canonicalMetadata(fishing_metadata)) ||
        !identical(canonicalMetadata(population_metadata),
                   canonicalMetadata(natural_metadata))) {
      stop(
        "Population, fishing mortality and natural mortality columns are not aligned by stock and age.",
        call. = FALSE
      )
    }

    fishing_mortality <- as.numeric(fishing_matrix[1L, ])
    natural_mortality <- as.numeric(natural_matrix[1L, ])
    total_mortality <- fishing_mortality + natural_mortality
    exploitation <- ifelse(
      total_mortality > sqrt(.Machine$double.eps),
      fishing_mortality / total_mortality * (-expm1(-total_mortality)),
      0
    )
    predicted_catch <- sweep(population_matrix, 2L, exploitation, "*")
    predicted_catch[] <- pmax(0, predicted_catch)
    colnames(predicted_catch) <- sub(
      "_N_", "_C_", colnames(population_matrix), fixed = TRUE
    )
    predicted_catch
  }

  applyRelativeFishingMortality <- function(neural_population,
                                            previous_population,
                                            scenario_fishing_mortality,
                                            reference_fishing_mortality,
                                            natural_mortality) {
    neural_matrix <- asFiniteNumericMatrix(
      neural_population, "Neural-network abundance vector"
    )
    previous_matrix <- asFiniteNumericMatrix(
      previous_population, "Previous population"
    )
    scenario_matrix <- asFiniteNumericMatrix(
      scenario_fishing_mortality, "Scenario fishing mortality"
    )
    reference_matrix <- asFiniteNumericMatrix(
      reference_fishing_mortality, "Reference fishing mortality"
    )
    natural_matrix <- asFiniteNumericMatrix(
      natural_mortality, "Transition natural mortality"
    )

    matrices <- list(
      neural_matrix, previous_matrix, scenario_matrix,
      reference_matrix, natural_matrix
    )
    if (any(vapply(matrices, nrow, integer(1)) != 1L)) {
      stop("Counterfactual projection inputs must each contain exactly one row.",
           call. = FALSE)
    }
    expected_columns <- ncol(neural_matrix)
    if (any(vapply(matrices, ncol, integer(1)) != expected_columns)) {
      stop("Counterfactual projection inputs have different age dimensions.",
           call. = FALSE)
    }

    neural_metadata <- parseFeatureMetadata(colnames(neural_matrix), "N")
    previous_metadata <- parseFeatureMetadata(colnames(previous_matrix), "N")
    scenario_metadata <- parseFeatureMetadata(colnames(scenario_matrix), "F")
    reference_metadata <- parseFeatureMetadata(colnames(reference_matrix), "F")
    natural_metadata <- parseFeatureMetadata(colnames(natural_matrix), "M")
    canonicalMetadata <- function(metadata) {
      metadata$gsa <- gsub("\\.", "-", metadata$gsa)
      metadata
    }
    population_metadata <- canonicalMetadata(neural_metadata)
    if (!identical(population_metadata, canonicalMetadata(previous_metadata)) ||
        !identical(population_metadata, canonicalMetadata(scenario_metadata)) ||
        !identical(population_metadata, canonicalMetadata(reference_metadata)) ||
        !identical(population_metadata, canonicalMetadata(natural_metadata))) {
      stop(
        paste(
          "Neural abundance, previous population and mortality columns are",
          "not aligned by stock and age."
        ),
        call. = FALSE
      )
    }

    neural_values <- pmax(0, as.numeric(neural_matrix[1L, ]))
    previous_values <- as.numeric(previous_matrix[1L, ])
    scenario_values <- as.numeric(scenario_matrix[1L, ])
    reference_values <- as.numeric(reference_matrix[1L, ])
    natural_values <- as.numeric(natural_matrix[1L, ])
    if (any(previous_values < 0) || any(scenario_values < 0) ||
        any(reference_values < 0) || any(natural_values < 0)) {
      stop("Population and mortality inputs must be non-negative.",
           call. = FALSE)
    }

    age_values <- suppressWarnings(
      as.numeric(gsub("\\+", "", population_metadata$age))
    )
    if (any(!is.finite(age_values))) {
      stop("Counterfactual projection requires numeric age classes.",
           call. = FALSE)
    }
    stock_keys <- paste(
      population_metadata$species, population_metadata$gsa, sep = "\r"
    )
    adjusted_values <- neural_values

    for (stock_key in unique(stock_keys)) {
      stock_positions <- which(stock_keys == stock_key)
      stock_positions <- stock_positions[order(age_values[stock_positions])]
      stock_ages <- age_values[stock_positions]
      if (length(stock_positions) < 2L) {
        stop(
          paste(
            "Counterfactual abundance projection requires at least two age",
            "classes for every stock."
          ),
          call. = FALSE
        )
      }
      if (anyDuplicated(stock_ages) || any(diff(stock_ages) != 1)) {
        stop(
          paste(
            "Counterfactual projection requires unique, consecutive age",
            "classes within every stock."
          ),
          call. = FALSE
        )
      }

      recruitment_position <- stock_positions[[1L]]
      adjusted_values[[recruitment_position]] <-
        neural_values[[recruitment_position]]

      for (age_index in seq.int(2L, length(stock_positions))) {
        source_position <- stock_positions[[age_index - 1L]]
        destination_position <- stock_positions[[age_index]]
        relative_survival <- exp(
          reference_values[[source_position]] -
            scenario_values[[source_position]]
        )
        if (!is.finite(relative_survival)) {
          stop("Fishing-mortality contrast produced a non-finite survival ratio.",
               call. = FALSE)
        }
        adjusted_values[[destination_position]] <-
          neural_values[[destination_position]] * relative_survival
      }

      # The terminal plus-group mixes survivors from the preceding and oldest
      # ages. Its counterfactual factor therefore uses their weighted survival
      # ratio instead of the single-source-age factor used by younger classes.
      plus_position <- utils::tail(stock_positions, 1L)
      preceding_position <- stock_positions[[length(stock_positions) - 1L]]
      scenario_survivors <-
        previous_values[[preceding_position]] * exp(-(
          scenario_values[[preceding_position]] +
            natural_values[[preceding_position]]
        )) +
        previous_values[[plus_position]] * exp(-(
          scenario_values[[plus_position]] + natural_values[[plus_position]]
        ))
      reference_survivors <-
        previous_values[[preceding_position]] * exp(-(
          reference_values[[preceding_position]] +
            natural_values[[preceding_position]]
        )) +
        previous_values[[plus_position]] * exp(-(
          reference_values[[plus_position]] + natural_values[[plus_position]]
        ))
      plus_ratio <- if (reference_survivors <= sqrt(.Machine$double.eps)) {
        1
      } else {
        scenario_survivors / reference_survivors
      }
      if (!is.finite(plus_ratio) || plus_ratio < 0) {
        stop("Plus-group fishing-mortality correction is invalid.",
             call. = FALSE)
      }
      adjusted_values[[plus_position]] <-
        neural_values[[plus_position]] * plus_ratio
    }

    matrix(
      adjusted_values,
      nrow = 1L,
      ncol = expected_columns,
      dimnames = list(NULL, colnames(neural_matrix))
    )
  }

  ensembleSummary <- function(values) {
    values <- as.numeric(values)
    values <- values[is.finite(values)]
    if (!length(values)) return(c(lower = NA_real_, mean = NA_real_, upper = NA_real_))
    bounds <- stats::quantile(values, probs = c(0.05, 0.95),
                              names = FALSE, type = 8)
    c(lower = bounds[1L], mean = mean(values), upper = bounds[2L])
  }

  extendLastBiologicalYear <- function(data, depth) {
    if (!nrow(data) || !"year" %in% names(data)) {
      stop("Cannot extend an empty biological table.", call. = FALSE)
    }
    last_year <- max(data$year)
    last_block <- data[data$year == last_year, , drop = FALSE]
    future <- lapply(seq_len(as.integer(depth)), function(lead) {
      transform(last_block, year = last_year + lead)
    })
    do.call(rbind, c(list(data), future))
  }

  normalizeFishingScenario <- function(scenario, expected_names,
                                       first_forecast_year) {
    if (is.numeric(scenario) && is.null(dim(scenario)) &&
        !is.null(names(scenario))) {
      scenario <- data.frame(as.list(scenario), check.names = FALSE)
    }
    if (!is.data.frame(scenario) && !is.matrix(scenario)) {
      stop("Fishing mortality must be a named vector, matrix or data frame.",
           call. = FALSE)
    }
    scenario <- as.data.frame(
      scenario, optional = TRUE, check.names = FALSE
    )
    if (!nrow(scenario) || !ncol(scenario) ||
        anyNA(names(scenario)) || any(!nzchar(names(scenario))) ||
        anyDuplicated(names(scenario))) {
      stop("The fishing-mortality scenario needs rows and unique column names.",
           call. = FALSE)
    }
    first_forecast_year <- as.integer(first_forecast_year)
    if (length(first_forecast_year) != 1L ||
        !is.finite(first_forecast_year)) {
      stop("The first forecast year is unavailable.", call. = FALSE)
    }
    expected_years <- first_forecast_year + seq_len(nrow(scenario)) - 1L
    year_position <- which(tolower(names(scenario)) == "year")
    if (length(year_position) > 1L) {
      stop("Provide at most one year column in the F scenario.", call. = FALSE)
    }
    if (length(year_position)) {
      supplied_years <- suppressWarnings(as.numeric(
        as.character(scenario[[year_position]])
      ))
      scenario[[year_position]] <- NULL
      if (any(!is.finite(supplied_years)) ||
          !identical(supplied_years, as.numeric(expected_years))) {
        stop(
          sprintf(
            "Fishing-mortality years must be consecutive starting in %s.",
            first_forecast_year
          ), call. = FALSE
        )
      }
    } else {
      row_years <- rownames(scenario)
      if (!identical(row_years, as.character(seq_len(nrow(scenario)))) &&
          all(grepl("^[0-9]{4}$", row_years))) {
        if (!identical(as.integer(row_years), expected_years)) {
          stop(
            sprintf(
              "Fishing-mortality row years must start in %s and be consecutive.",
              first_forecast_year
            ), call. = FALSE
          )
        }
      }
    }
    if (!setequal(names(scenario), expected_names) ||
        length(names(scenario)) != length(expected_names)) {
      stop(
        paste(
          "Fishing-mortality columns must exactly match the loaded stocks",
          "(species, age and GSA)."
        ), call. = FALSE
      )
    }
    scenario <- scenario[, expected_names, drop = FALSE]
    scenario_matrix <- asFiniteNumericMatrix(
      scenario, "Fishing-mortality scenario"
    )
    if (any(scenario_matrix < 0)) {
      stop("Fishing mortality must be non-negative.", call. = FALSE)
    }
    result <- as.data.frame(
      scenario_matrix, optional = TRUE, check.names = FALSE
    )
    rownames(result) <- NULL
    result
  }

  forecastFishingSchedule <- function(scenario, expected_names,
                                      last_observed_year, depth) {
    depth <- as.integer(depth)
    if (length(depth) != 1L || !is.finite(depth) || depth < 1L) {
      stop("Forecast depth must be a positive number of years.", call. = FALSE)
    }
    first_forecast_year <- as.integer(last_observed_year) + 1L
    validated <- normalizeFishingScenario(
      scenario, expected_names, first_forecast_year
    )
    selected_rows <- pmin(seq_len(depth), nrow(validated))
    schedule <- validated[selected_rows, , drop = FALSE]
    rownames(schedule) <- NULL
    data.frame(
      year = first_forecast_year + seq_len(depth) - 1L,
      schedule, check.names = FALSE
    )
  }

  scenarioFishingVector <- function(fishing_row, species_code, gsa_code,
                                    ages) {
    fishing_matrix <- asFiniteNumericMatrix(
      fishing_row, "Scenario fishing mortality"
    )
    if (nrow(fishing_matrix) != 1L) {
      stop("Scenario fishing mortality must contain exactly one row.",
           call. = FALSE)
    }
    metadata <- parseFeatureMetadata(colnames(fishing_matrix), "F")
    metadata$age_numeric <- suppressWarnings(
      as.numeric(gsub("\\+", "", metadata$age))
    )
    metadata$gsa <- gsub("\\.", "-", metadata$gsa)
    gsa_code <- gsub("\\.", "-", as.character(gsa_code))
    selected <- which(
      metadata$species == as.character(species_code) &
        metadata$gsa == gsa_code
    )
    if (!length(selected)) {
      stop("Scenario fishing mortality does not contain the requested stock.",
           call. = FALSE)
    }
    age_match <- match(as.numeric(ages), metadata$age_numeric[selected])
    if (anyNA(age_match) || anyDuplicated(metadata$age_numeric[selected])) {
      stop(
        "Scenario fishing mortality is not aligned with the retained ages.",
        call. = FALSE
      )
    }
    values <- as.numeric(fishing_matrix[1L, selected[age_match]])
    if (any(values < 0)) {
      stop("Scenario fishing mortality must be non-negative.", call. = FALSE)
    }
    stats::setNames(values, as.character(as.numeric(ages)))
  }

  extendScenarioFishingMortality <- function(historical_data, scenario_schedule,
                                             last_observed_year, depth,
                                             species_code, gsa_code) {
    extended <- extendLastBiologicalYear(historical_data, depth)
    schedule <- forecastFishingSchedule(
      scenario_schedule,
      expected_names = names(scenario_schedule)[-1L],
      last_observed_year = last_observed_year,
      depth = depth
    )
    for (step in seq_len(as.integer(depth))) {
      year <- schedule$year[[step]]
      scenario_values <- scenarioFishingVector(
        schedule[step, -1L, drop = FALSE],
        species_code = species_code,
        gsa_code = gsa_code,
        ages = sort(unique(extended$age))
      )
      year_rows <- extended$year == year
      year_ages <- as.character(as.numeric(extended$age[year_rows]))
      replacements <- unname(scenario_values[year_ages])
      if (!any(year_rows) || anyNA(replacements)) {
        stop("Future SSB fishing mortality could not be matched by age and year.",
             call. = FALSE)
      }
      extended$fmort[year_rows] <- replacements
    }
    extended
  }

  validateCounterfactualProjectionEngine <- function() {
    population_names <- paste0("TST_", 0:2, "_N_1")
    fishing_names <- sub("_N_", "_F_", population_names, fixed = TRUE)
    natural_names <- sub("_N_", "_M_", population_names, fixed = TRUE)
    oneRow <- function(values, names) {
      matrix(values, nrow = 1L, dimnames = list(NULL, names))
    }

    previous <- oneRow(c(1000, 500, 200), population_names)
    neural <- oneRow(c(120, 450, 210), population_names)
    reference_fishing <- oneRow(c(0.2, 0.2, 0.2), fishing_names)
    scenario_fishing <- oneRow(c(0.5, 0.4, 0.3), fishing_names)
    natural <- oneRow(c(0.2, 0.2, 0.2), natural_names)
    adjusted <- applyRelativeFishingMortality(
      neural, previous, scenario_fishing, reference_fishing, natural
    )
    plus_ratio <- (
      500 * exp(-0.6) + 200 * exp(-0.5)
    ) / (
      500 * exp(-0.4) + 200 * exp(-0.4)
    )
    expected <- c(
      120,
      450 * exp(-0.3),
      210 * plus_ratio
    )
    if (!isTRUE(all.equal(
      as.numeric(adjusted), expected, tolerance = 1e-12
    ))) {
      stop("Counterfactual projection failed its relative-survival formula check.",
           call. = FALSE)
    }

    identity <- applyRelativeFishingMortality(
      neural, previous, reference_fishing, reference_fishing, natural
    )
    if (!isTRUE(all.equal(as.numeric(identity), as.numeric(neural),
                          tolerance = 1e-12))) {
      stop("Counterfactual projection failed its reference-identity check.",
           call. = FALSE)
    }

    reversed <- applyRelativeFishingMortality(
      adjusted, previous, reference_fishing, scenario_fishing, natural
    )
    if (!isTRUE(all.equal(as.numeric(reversed), as.numeric(neural),
                          tolerance = 1e-12))) {
      stop("Counterfactual projection failed its mortality round-trip check.",
           call. = FALSE)
    }
    if (!all(adjusted[1L, 2:3] < neural[1L, 2:3]) ||
        adjusted[1L, 1L] != neural[1L, 1L]) {
      stop("Counterfactual projection failed its fishing-mortality response check.",
           call. = FALSE)
    }

    zero_fishing <- oneRow(rep(0, 3L), fishing_names)
    unfished_catch <- catchBaranov(zero_fishing, natural, previous)
    fished_catch <- catchBaranov(scenario_fishing, natural, previous)
    if (any(unfished_catch != 0) || any(fished_catch <= 0)) {
      stop("Counterfactual projection failed its Baranov catch-response check.",
           call. = FALSE)
    }

    historical <- data.frame(
      year = rep(2020L, 3L),
      age = 0:2,
      fmort = rep(0.1, 3L),
      gsa = rep("1", 3L),
      species = rep("TST", 3L),
      tri_gsa = rep("TST_1", 3L),
      stringsAsFactors = FALSE
    )
    schedule <- forecastFishingSchedule(
      rbind(scenario_fishing, zero_fishing),
      fishing_names, last_observed_year = 2020L, depth = 4L
    )
    expected_schedule <- rbind(
      scenario_fishing, zero_fishing, zero_fishing, zero_fishing
    )
    if (!identical(schedule$year, 2021:2024) ||
        !isTRUE(all.equal(
          unname(as.matrix(schedule[, -1L, drop = FALSE])),
          unname(expected_schedule), tolerance = 1e-12
        ))) {
      stop("Annual fishing-mortality schedule failed its extension check.",
           call. = FALSE)
    }
    one_row_schedule <- forecastFishingSchedule(
      scenario_fishing, fishing_names, 2020L, 3L
    )
    if (!isTRUE(all.equal(
      unname(as.matrix(one_row_schedule[, -1L, drop = FALSE])),
      unname(rbind(scenario_fishing, scenario_fishing, scenario_fishing)),
      tolerance = 1e-12
    ))) {
      stop("A single F row did not remain constant through the forecast.",
           call. = FALSE)
    }
    hyphen_names <- paste0("TST_", 0:2, "_F_9-10-11")
    hyphen_scenario <- oneRow(c(0.5, 0.4, 0.3), hyphen_names)
    hyphen_schedule <- forecastFishingSchedule(
      hyphen_scenario, hyphen_names, 2020L, 1L
    )
    if (!identical(names(hyphen_schedule)[-1L], hyphen_names)) {
      stop("The annual F scenario changed stock/GSA column names.",
           call. = FALSE)
    }
    longer <- forecastFishingSchedule(
      rbind(scenario_fishing, zero_fishing, reference_fishing),
      fishing_names, last_observed_year = 2020L, depth = 2L
    )
    if (!isTRUE(all.equal(
      as.numeric(as.matrix(longer[2L, -1L, drop = FALSE])),
      c(0, 0, 0), tolerance = 1e-12
    ))) {
      stop("Annual fishing-mortality schedule failed its truncation check.",
           call. = FALSE)
    }
    with_years <- data.frame(
      year = 2021:2022,
      rbind(scenario_fishing, reference_fishing),
      check.names = FALSE
    )
    if (!isTRUE(all.equal(
      as.numeric(as.matrix(forecastFishingSchedule(
        with_years, fishing_names, 2020L, 2L
      )[2L, -1L, drop = FALSE])),
      rep(0.2, 3L), tolerance = 1e-12
    ))) {
      stop("Annual fishing-mortality schedule failed its year-column check.",
           call. = FALSE)
    }
    with_years$year[[2L]] <- 2023L
    if (!inherits(try(
      forecastFishingSchedule(with_years, fishing_names, 2020L, 2L),
      silent = TRUE
    ), "try-error")) {
      stop("Annual fishing-mortality schedule accepted a skipped year.",
           call. = FALSE)
    }
    extended <- extendScenarioFishingMortality(
      historical_data = historical,
      scenario_schedule = schedule,
      last_observed_year = 2020L,
      depth = 4L,
      species_code = "TST",
      gsa_code = "1"
    )
    expected_future <- c(
      0.5, 0.4, 0.3, rep(0, 9L)
    )
    if (!identical(extended$fmort[extended$year == 2020L], rep(0.1, 3L)) ||
        !isTRUE(all.equal(
          extended$fmort[extended$year > 2020L],
          expected_future,
          tolerance = 1e-12
    ))) {
      stop("Counterfactual projection failed its future-SSB F scenario check.",
           call. = FALSE)
    }
    invisible(TRUE)
  }

  validateCounterfactualProjectionEngine()

  dataStructureYearBreaks <- function(years, maximum_breaks = 9L) {
    years <- sort(unique(as.integer(years[is.finite(years)])))
    if (length(years) <= maximum_breaks) return(years)
    positions <- unique(round(seq(1L, length(years), length.out = maximum_breaks)))
    years[positions]
  }

  dataStructurePlotHeight <- function(data) {
    facet_count <- max(1L, length(unique(as.character(data$tri_gsa))))
    facet_rows <- ceiling(facet_count / min(2L, facet_count))
    as.integer(180L + 250L * facet_rows)
  }

  dataStructureTheme <- function() {
    theme_test(base_size = 11) +
      theme(
        axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold",
                                  margin = margin(b = 10)),
        strip.background = element_rect(fill = "#E8F1F8", colour = "#8295A7",
                                        linewidth = 0.4),
        strip.text = element_text(face = "bold", colour = "#263746",
                                  margin = margin(5, 4, 5, 4)),
        panel.spacing.x = grid::unit(1.4, "lines"),
        panel.spacing.y = grid::unit(2.2, "lines"),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.key.width = grid::unit(20, "pt"),
        plot.margin = margin(12, 22, 18, 20)
      )
  }

  plotPop <- function(poplong) {
    poplong$age <- as.factor(poplong$age)
    colnames(poplong)[3] <- "Population"

    ggplot(data = poplong, aes(x = year)) +
      geom_line(aes(y = Population, colour = age), linewidth = 1.1,
                lineend = "round") +
      scale_x_continuous(
        breaks = dataStructureYearBreaks(poplong$year),
        expand = expansion(mult = c(0.015, 0.025))
      ) +
      labs(
        title = "Population by age and year",
        x = "Year", y = "Population (number of individuals)", colour = "Age"
      ) +
      scale_colour_brewer(palette = "Blues", direction = -1) +
      guides(colour = guide_legend(nrow = 1L, byrow = TRUE,
                                   override.aes = list(linewidth = 2))) +
      dataStructureTheme() +
      facet_wrap(~ tri_gsa, scales = "free_y", ncol = 2L)
  }

  plotCatch <- function(catchlong) {
    catchlong$age <- as.factor(catchlong$age)
    colnames(catchlong)[3] <- "Catches"

    ggplot(data = catchlong, aes(x = year)) +
      geom_line(aes(y = Catches, colour = age), linewidth = 1.1,
                lineend = "round") +
      scale_x_continuous(
        breaks = dataStructureYearBreaks(catchlong$year),
        expand = expansion(mult = c(0.015, 0.025))
      ) +
      labs(
        title = "Catch by age and year",
        x = "Year", y = "Catch (number of individuals)", colour = "Age"
      ) +
      scale_colour_brewer(palette = "Blues", direction = -1) +
      guides(colour = guide_legend(nrow = 1L, byrow = TRUE,
                                   override.aes = list(linewidth = 2))) +
      dataStructureTheme() +
      facet_wrap(~ tri_gsa, scales = "free_y", ncol = 2L)
  }

  plotWaa <- function(waalong, poplong) {
    waalong$age <- as.factor(waalong$age)
    biomasslong <- data.frame(
      year = waalong$year,
      age = waalong$age,
      tri_gsa = waalong$tri_gsa,
      TotBiomass = round(waalong$weight_at_age * poplong$pop, 2)
    )

    ggplot(data = biomasslong, aes(x = year)) +
      geom_line(aes(y = TotBiomass, colour = age), linewidth = 1.1,
                lineend = "round") +
      scale_x_continuous(
        breaks = dataStructureYearBreaks(biomasslong$year),
        expand = expansion(mult = c(0.015, 0.025))
      ) +
      labs(
        title = "Total biomass by age and year",
        x = "Year", y = "Total biomass (tonnes)", colour = "Age"
      ) +
      scale_colour_brewer(palette = "Blues", direction = -1) +
      guides(colour = guide_legend(nrow = 1L, byrow = TRUE,
                                   override.aes = list(linewidth = 2))) +
      dataStructureTheme() +
      facet_wrap(~ tri_gsa, scales = "free_y", ncol = 2L)
  }
  
  procInputs <- function(popwide, catchwide) {
    if (!identical(as.integer(popwide$year), as.integer(catchwide$year))) {
      stop("Population and catch tables do not contain the same ordered years.", call. = FALSE)
    }
    population_names <- colnames(popwide)[-1L]
    catch_names <- colnames(catchwide)[-1L]
    if (!length(population_names) || !all(grepl("_N_", population_names, fixed = TRUE))) {
      stop("Population wide-table columns must use the '_N_' feature code.",
           call. = FALSE)
    }
    expected_catch_names <- sub("_N_", "_C_", population_names, fixed = TRUE)
    if (!identical(catch_names, expected_catch_names)) {
      stop(
        "Population and catch wide-table columns are not aligned by stock and age.",
        call. = FALSE
      )
    }
    tot_df <- cbind(popwide, catchwide[,2:ncol(catchwide), drop = FALSE])
    if (any(!is.finite(as.matrix(tot_df)))) {
      stop("The neural-network input table contains NA, NaN or Inf.", call. = FALSE)
    }
    return(tot_df)
  }
  
  plotNet <- function(nLayers, layerTypeTot, neuronsTot, dropoutTot, inputNames) {
    nLayers <- as.integer(nLayers)
    df <- data.frame(label = NULL, x = NULL, y = NULL, type = NULL, dropout = NULL, nLayer = NULL)
    df_iter <- data.frame(label = NULL, x = NULL, y = NULL, type = NULL, dropout = NULL, nLayer = NULL)
    
    for (i in seq_len(nLayers)) {
      
      l <- length(inputNames) * (neuronsTot[i] / length(inputNames))
      
      if (neuronsTot[i] == 0) l <- 1
      
      if (i < as.integer(nLayers)) {
        df_iter <- data.frame(label = NA,
                             x = rep(10 * i, l),
                             y = seq(90, 10, length = l),
                             type = rep(layerTypeTot[i], l),
                             dropout = rep(dropoutTot[i], l),
                             nLayer = rep(as.integer(i), l))
      } else {
        df_iter <- data.frame(label = NA,
                             x = rep(10 * i, l/2),
                             y = seq(90, 10, length = l/2),
                             type = rep(layerTypeTot[i], l/2),
                             dropout = rep(dropoutTot[i], l/2),
                             nLayer = rep(as.integer(i), l/2))
      }
      
      if (neuronsTot[i] == 0) df_iter$y <- 50
      if (i == 1) df_iter$label <- rep(inputNames, each = neuronsTot[i] / length(inputNames))
      if (i == nLayers) {
        df_iter$label <- inputNames[seq_len(as.integer(length(inputNames) / 2L))]
      }
      
      df <- rbind(df, df_iter)
    }
    
    df_links <- data.frame(x0 = NULL, x1 = NULL, y0 = NULL, y1 = NULL)
    df_links_iter <- data.frame(x0 = NULL, x1 = NULL, y0 = NULL, y1 = NULL)
    
    for (link_index in seq_len(max(0L, nLayers - 1L))) {
      set1 <- df[which(df$nLayer == link_index),]
      set2 <- df[which(df$nLayer == (link_index + 1L)),]
      
      df_links_iter = data.frame(x0 = numeric(nrow(set1) * nrow(set2)),
                                 y0 = numeric(nrow(set1) * nrow(set2)),
                                 x1 = numeric(nrow(set1) * nrow(set2)),
                                 y1 = numeric(nrow(set1) * nrow(set2))) 
      k <- 0L
      for (i in seq_len(nrow(set1))) {
        for (j in seq_len(nrow(set2))) {
          k = k + 1
          df_links_iter[k, c("x0", "y0")] = set1[i, c("x", "y")]
          df_links_iter[k, c("x1", "y1")] = set2[j, c("x", "y")]
        }
      }
      
      df_links <- rbind(df_links, df_links_iter)
    }
    
    gElman = ggplot(data = df, aes(x = x, y = y, fill = type, shape = type)) +
      xlim(0, (max(df$x) + 10)) +
      geom_segment(data = df_links,
                   aes(x = x0, y = y0, xend = x1, yend = y1), colour = "grey",
                   inherit.aes = F, linewidth = 1) +
      geom_point(size = 5) +
      scale_shape_manual(values = c("Dense" = 22, "Dropout" = 24, "LSTM" = 21, "SimpleRNN" = 23)) +
      scale_fill_manual(values = c("Dense" = "orange", "Dropout" = "darkred", "LSTM" = "blue", "SimpleRNN" = "darkgreen")) +
      theme_void() +
      geom_text(data = df[which(df$nLayer == 1),], size = 3, fontface = "bold", hjust = 1,
                aes(x = x, y = y, label = label), nudge_x = -1) +
      geom_text(data = df[which(df$nLayer == as.integer(nLayers)),], size = 3, fontface = "bold",
                aes(x = x, y = y, label = label), nudge_x = -1, hjust = 1) +
      theme(legend.position = "bottom")
    
    return(gElman)
  }
  
  buildNet <- function(outputs, nLayer, layerType, neurons, returnSeq, dropout, activation, recdropout, recactivation) {
    
    if (activation == "NULL") activation = NULL
    if (recactivation == "NULL") recactivation = NULL
    
    if (layerType == "Dense") {
      outputs <- outputs %>% layer_dense(units = as.integer(neurons), activation = activation, name = paste0(layerType, "_", nLayer))
    } else if (layerType == "Dropout") {
      outputs <- outputs %>% layer_dropout(rate = as.numeric(dropout), name = paste0(layerType, "_", nLayer))
    } else if (layerType == "LSTM") {
      outputs <- outputs %>% layer_lstm(units = as.integer(neurons), dropout = as.numeric(dropout), recurrent_dropout = as.numeric(recdropout),
                           activation = activation, recurrent_activation = recactivation, return_sequences = as.logical(returnSeq), name = paste0(layerType, "_", nLayer))
    } else if (layerType == "SimpleRNN") {
      outputs <- outputs %>% layer_simple_rnn(units = as.integer(neurons), dropout = as.numeric(dropout), recurrent_dropout = as.numeric(recdropout),
                           activation = activation, return_sequences = as.logical(returnSeq), name = paste0(layerType, "_", nLayer))
    }
    return(outputs)
  }

  validateLayerConfiguration <- function() {
    n_layers <- as.integer(input$nLayers)
    layer_types <- vapply(seq_len(n_layers), function(index) {
      as.character(input[[paste0("layerType", index)]])
    }, character(1))

    for (index in seq_len(n_layers)) {
      if (layer_types[index] != "Dropout") {
        neurons <- as.integer(input[[paste0("neurons", index)]])
        if (!is.finite(neurons) || neurons < 1L) {
          stop(sprintf("Layer %s must contain at least one neuron.", index), call. = FALSE)
        }
      }
      if (layer_types[index] %in% c("LSTM", "SimpleRNN") && index < n_layers) {
        later_recurrent <- any(layer_types[(index + 1L):n_layers] %in% c("LSTM", "SimpleRNN"))
        if (later_recurrent && !isTRUE(as.logical(input[[paste0("returnSeq", index)]]))) {
          stop(
            sprintf("Layer %s must return sequences because it precedes another recurrent layer.",
                    index),
            call. = FALSE
          )
        }
      }
    }
    invisible(TRUE)
  }

  createConfiguredModel <- function(lookback, n_features, model_name) {
    inputs <- layer_input(
      shape = c(as.integer(lookback), as.integer(n_features)),
      name = paste0(model_name, "_input")
    )
    outputs <- inputs
    for (layer_index in seq_len(as.integer(input$nLayers))) {
      outputs <- buildNet(
        outputs = outputs,
        nLayer = layer_index,
        layerType = input[[paste0("layerType", layer_index)]],
        neurons = input[[paste0("neurons", layer_index)]],
        returnSeq = input[[paste0("returnSeq", layer_index)]],
        dropout = input[[paste0("dropout", layer_index)]],
        activation = input$activation,
        recdropout = input[[paste0("recdropout", layer_index)]],
        recactivation = input$recactivation
      )
    }
    outputs <- outputs %>%
      layer_flatten(name = paste0(model_name, "_flatten")) %>%
      layer_dense(
        units = as.integer(n_features),
        activation = "linear",
        name = paste0(model_name, "_output")
      )
    keras_model(inputs, outputs)
  }

  compileConfiguredModel <- function(model) {
    model %>% compile(
      loss = "mse",
      metrics = "mae",
      optimizer = optimizer_rmsprop(
        learning_rate = as.numeric(input$learnParam)
      )
    )
    invisible(model)
  }

  temporalSplitDefinition <- function(n_years) {
    n_years <- as.integer(n_years)
    if (!is.finite(n_years) || n_years < minimum_years_for_temporal_test) {
      stop(
        sprintf(
          paste(
            "Too few years for a defensible temporal test.",
            "At least %s years must remain after the holdout."
          ),
          minimum_years_for_temporal_test
        ),
        call. = FALSE
      )
    }

    preferred_lookback <- n_years -
      minimum_preferred_training_samples -
      minimum_validation_samples
    lookback <- max(1L, min(maximum_lookback, preferred_lookback))
    n_samples <- n_years - lookback
    n_validation <- max(
      minimum_validation_samples,
      as.integer(ceiling(n_samples * validation_fraction))
    )
    n_training <- n_samples - n_validation

    if (n_training < absolute_minimum_training_samples) {
      stop(
        sprintf(
          "Only %s supervised training samples remain; at least %s are required.",
          n_training, absolute_minimum_training_samples
        ),
        call. = FALSE
      )
    }
    list(
      lookback = lookback,
      n_samples = n_samples,
      n_training = n_training,
      n_validation = n_validation,
      scaler_last_row = lookback + n_training,
      short_series = n_training < minimum_preferred_training_samples
    )
  }

  rawHistoryData <- function(history) {
    history_df <- na.omit(as.data.frame(history))
    if (!nrow(history_df)) {
      stop("Keras returned an empty training history.", call. = FALSE)
    }
    history_df
  }

  bestValidationEpoch <- function(history_df) {
    metric <- tolower(as.character(history_df$metric))
    data_type <- rep("", nrow(history_df))
    if ("data" %in% names(history_df)) {
      data_type <- tolower(as.character(history_df$data))
    }
    validation_loss <- (grepl("loss", metric) & grepl("validation", data_type)) |
      grepl("^val[_-]?loss$", metric)
    if (!any(validation_loss)) {
      stop("Temporal validation loss is missing from the Keras history.", call. = FALSE)
    }
    validation_history <- history_df[validation_loss, , drop = FALSE]
    validation_history <- validation_history[
      order(validation_history$epoch), , drop = FALSE
    ]
    as.integer(which.min(validation_history$value))
  }

  formatHistoryData <- function(history_df, iteration) {
    metric <- as.character(history_df$metric)
    if (!"data" %in% names(history_df)) {
      history_df$data <- ifelse(grepl("^val_", metric), "validation", "training")
    }
    metric <- sub("^val_", "", metric)
    metric[metric %in% c("loss", "mean_squared_error", "val_loss")] <- "MSE"
    metric[metric %in% c("mae", "mean_absolute_error", "val_mae")] <- "MAE"
    history_df$metric <- factor(metric)
    history_df$iter <- as.integer(iteration)
    history_df
  }

  fitTemporalModel <- function(source_df, validation_target_df,
                               final_target_df, seed, model_name,
                               verbose = 0L) {
    source_df <- as.data.frame(source_df, check.names = FALSE)
    validation_target_df <- as.data.frame(
      validation_target_df, check.names = FALSE
    )
    final_target_df <- as.data.frame(final_target_df, check.names = FALSE)
    target_tables <- list(validation_target_df, final_target_df)
    if (any(vapply(target_tables, nrow, integer(1)) != nrow(source_df)) ||
        any(vapply(target_tables, ncol, integer(1)) != ncol(source_df)) ||
        any(vapply(
          target_tables,
          function(target) identical(names(target), names(source_df)),
          logical(1)
        ) == FALSE)) {
      stop(
        "Temporal neural-network inputs and targets are not aligned.",
        call. = FALSE
      )
    }
    split <- temporalSplitDefinition(nrow(source_df))

    validation_preparation <- withRuntimeStage(
      "temporal-validation preprocessing",
      {
        validation_input_normalizer <- normalizeInputs(
          source_df[seq_len(split$scaler_last_row), , drop = FALSE]
        )
        validation_input_values <- normalizeUsingRange(
          source_df, validation_input_normalizer$range
        )
        validation_input_values <- toModelScale(validation_input_values)
        validation_training_targets <- seq.int(
          split$lookback + 1L,
          split$lookback + split$n_training
        )
        validation_output_normalizer <- normalizeInputs(
          validation_target_df[
            validation_training_targets, , drop = FALSE
          ]
        )
        validation_target_values <- normalizeUsingRange(
          validation_target_df, validation_output_normalizer$range
        )
        validation_target_values <- toModelScale(validation_target_values)
        validation_sequences <- buildSequenceSamples(
          validation_input_values,
          target_matrix = validation_target_values,
          maximum_lookback = split$lookback
        )
        list(
          input_normalizer = validation_input_normalizer,
          output_normalizer = validation_output_normalizer,
          input_values = validation_input_values,
          target_values = validation_target_values,
          sequences = validation_sequences
        )
      }
    )
    validation_sequences <- validation_preparation$sequences

    training_rows <- seq_len(split$n_training)
    validation_rows <- seq.int(split$n_training + 1L, split$n_samples)

    keras3::set_random_seed(as.integer(seed))
    validation_model <- withRuntimeStage(
      "temporal-validation model construction",
      {
        model <- createConfiguredModel(
          split$lookback, ncol(source_df), paste0(model_name, "_validation")
        )
        compileConfiguredModel(model)
        model
      }
    )

    parameter_count <- as.numeric(validation_model$count_params())

    validation_history <- withRuntimeStage(
      "temporal-validation Keras fit",
      validation_model %>% fit(
        x = validation_sequences$x[training_rows, , , drop = FALSE],
        y = validation_sequences$y[training_rows, , drop = FALSE],
        validation_data = list(
          validation_sequences$x[validation_rows, , , drop = FALSE],
          validation_sequences$y[validation_rows, , drop = FALSE]
        ),
        batch_size = min(8L, split$n_training),
        shuffle = FALSE,
        verbose = as.integer(verbose),
        epochs = as.integer(input$nEpochs),
        callbacks = list(
          callback_early_stopping(
            monitor = "val_loss",
            patience = 15L,
            restore_best_weights = TRUE
          ),
          callback_terminate_on_nan()
        )
      )
    )
    history_selection <- withRuntimeStage(
      "validation-history processing",
      {
        history_df <- rawHistoryData(validation_history)
        list(
          history = history_df,
          best_epoch = max(1L, bestValidationEpoch(history_df))
        )
      }
    )
    history_df <- history_selection$history
    best_epoch <- history_selection$best_epoch

    final_preparation <- withRuntimeStage(
      "full-period refit preprocessing",
      {
        final_input_normalizer <- normalizeInputs(source_df)
        final_input_values <- toModelScale(final_input_normalizer$values)
        final_target_rows <- seq.int(split$lookback + 1L, nrow(source_df))
        final_output_normalizer <- normalizeInputs(
          final_target_df[final_target_rows, , drop = FALSE]
        )
        final_target_values <- normalizeUsingRange(
          final_target_df, final_output_normalizer$range
        )
        final_target_values <- toModelScale(final_target_values)
        final_sequences <- buildSequenceSamples(
          final_input_values,
          target_matrix = final_target_values,
          maximum_lookback = split$lookback
        )
        list(
          input_normalizer = final_input_normalizer,
          output_normalizer = final_output_normalizer,
          input_values = final_input_values,
          target_values = final_target_values,
          sequences = final_sequences
        )
      }
    )
    final_input_normalizer <- final_preparation$input_normalizer
    final_output_normalizer <- final_preparation$output_normalizer
    final_input_values <- final_preparation$input_values
    final_sequences <- final_preparation$sequences

    keras3::set_random_seed(as.integer(seed))
    final_model <- withRuntimeStage(
      "full-period model construction",
      {
        model <- createConfiguredModel(
          split$lookback, ncol(source_df), paste0(model_name, "_final")
        )
        compileConfiguredModel(model)
        model
      }
    )
    withRuntimeStage(
      "full-period Keras refit",
      final_model %>% fit(
        x = final_sequences$x,
        y = final_sequences$y,
        batch_size = min(8L, nrow(final_sequences$y)),
        shuffle = FALSE,
        verbose = 0L,
        epochs = best_epoch,
        callbacks = list(callback_terminate_on_nan())
      )
    )

    list(
      model = final_model,
      history = history_df,
      input_range = final_input_normalizer$range,
      output_range = final_output_normalizer$range,
      model_values = final_input_values,
      lookback = split$lookback,
      best_epoch = best_epoch,
      parameter_count = parameter_count
    )
  }

  historicalRateRow <- function(rate_table, year, label) {
    row <- rate_table[rate_table$year == as.integer(year), -1L, drop = FALSE]
    if (nrow(row) != 1L) {
      stop(sprintf("Expected exactly one %s row for year %s.", label, year),
           call. = FALSE)
    }
    row
  }

  effectiveBaselineYears <- function(n_available_years) {
    requested <- suppressWarnings(as.integer(input$baseline))
    if (!length(requested) || !is.finite(requested) || requested < 1L) {
      requested <- suppressWarnings(as.integer(fmort_baseline))
    }
    if (!length(requested) || !is.finite(requested) || requested < 1L) {
      requested <- 1L
    }
    min(as.integer(requested), as.integer(n_available_years))
  }

  coerceReferenceFishingMortality <- function(reference_row, expected_names,
                                               label) {
    reference_matrix <- asFiniteNumericMatrix(reference_row, label)
    if (nrow(reference_matrix) != 1L ||
        is.null(colnames(reference_matrix)) ||
        anyDuplicated(colnames(reference_matrix)) ||
        !setequal(colnames(reference_matrix), expected_names)) {
      stop(
        sprintf("%s is not aligned with the fishing-mortality features.", label),
        call. = FALSE
      )
    }
    reference_matrix <- reference_matrix[, expected_names, drop = FALSE]
    if (any(reference_matrix < 0)) {
      stop(sprintf("%s must be non-negative.", label), call. = FALSE)
    }
    as.data.frame(reference_matrix, check.names = FALSE)
  }

  referenceFishingMortality <- function(fishing_history, available_years,
                                        baseline_years) {
    fishing_history <- as.data.frame(fishing_history, check.names = FALSE)
    available_years <- sort(unique(as.integer(available_years)))
    selected <- fishing_history[
      fishing_history$year %in% available_years, , drop = FALSE
    ]
    selected <- selected[order(selected$year), , drop = FALSE]
    if (nrow(selected) != length(available_years) ||
        !identical(as.integer(selected$year), available_years)) {
      stop(
        "Reference fishing mortality could not be matched to every model year.",
        call. = FALSE
      )
    }
    baseline_years <- min(
      max(1L, as.integer(baseline_years)), nrow(selected)
    )
    baseline_rows <- seq.int(
      nrow(selected) - baseline_years + 1L, nrow(selected)
    )
    reference_values <- colMeans(
      selected[baseline_rows, -1L, drop = FALSE]
    )
    reference <- data.frame(
      matrix(
        reference_values,
        nrow = 1L,
        dimnames = list(NULL, names(selected)[-1L])
      ),
      check.names = FALSE
    )
    coerceReferenceFishingMortality(
      reference, names(selected)[-1L], "Calculated reference fishing mortality"
    )
  }

  standardizeTemporalTargets <- function(net_inputs,
                                         reference_fishing_mortality) {
    net_inputs <- as.data.frame(net_inputs, check.names = FALSE)
    years <- as.integer(net_inputs$year)
    if (any(!is.finite(years)) || anyDuplicated(years) ||
        is.unsorted(years, strictly = TRUE) || any(diff(years) != 1L)) {
      stop(
        "Counterfactual target standardization requires consecutive model years.",
        call. = FALSE
      )
    }

    feature_names <- names(net_inputs)[-1L]
    population_names <- grep("_N_", feature_names, value = TRUE, fixed = TRUE)
    if (!length(population_names)) {
      stop("Counterfactual target standardization found no abundance features.",
           call. = FALSE)
    }
    expected_catch_names <- sub(
      "_N_", "_C_", population_names, fixed = TRUE
    )
    if (!all(expected_catch_names %in% feature_names)) {
      stop("Counterfactual targets require catches aligned with abundance.",
           call. = FALSE)
    }

    fishing_names <- names(fmort_w)[-1L]
    reference_fishing_mortality <- coerceReferenceFishingMortality(
      reference_fishing_mortality,
      fishing_names,
      "Reference fishing mortality"
    )
    targets <- net_inputs[, -1L, drop = FALSE]

    for (target_index in seq.int(2L, nrow(net_inputs))) {
      source_year <- years[[target_index - 1L]]
      target_year <- years[[target_index]]
      previous_population <- net_inputs[
        target_index - 1L, population_names, drop = FALSE
      ]
      observed_target_population <- net_inputs[
        target_index, population_names, drop = FALSE
      ]
      reference_population <- applyRelativeFishingMortality(
        neural_population = observed_target_population,
        previous_population = previous_population,
        scenario_fishing_mortality = reference_fishing_mortality,
        reference_fishing_mortality = historicalRateRow(
          fmort_w, source_year, "source-year fishing mortality"
        ),
        natural_mortality = historicalRateRow(
          mort_w, source_year, "source-year natural mortality"
        )
      )
      targets[target_index, population_names] <-
        as.numeric(reference_population)

      reference_catch <- catchBaranov(
        reference_fishing_mortality,
        historicalRateRow(
          mort_w, target_year, "target-year natural mortality"
        ),
        reference_population
      )
      catch_positions <- match(colnames(reference_catch), names(targets))
      if (anyNA(catch_positions)) {
        stop("Reference catches could not be matched to neural-network targets.",
             call. = FALSE)
      }
      targets[target_index, catch_positions] <- as.numeric(reference_catch)
    }

    if (any(!is.finite(as.matrix(targets)))) {
      stop("Counterfactual neural-network targets contain non-finite values.",
           call. = FALSE)
    }
    targets
  }

  prepareTemporalModelData <- function(net_inputs,
                                       final_reference_fishing = NULL) {
    net_inputs <- as.data.frame(net_inputs, check.names = FALSE)
    split <- temporalSplitDefinition(nrow(net_inputs))
    baseline_years <- effectiveBaselineYears(nrow(net_inputs))
    validation_years <- net_inputs$year[seq_len(split$scaler_last_row)]
    validation_reference <- referenceFishingMortality(
      fmort_w, validation_years, baseline_years
    )
    if (is.null(final_reference_fishing)) {
      final_reference <- referenceFishingMortality(
        fmort_w, net_inputs$year, baseline_years
      )
    } else {
      final_reference <- coerceReferenceFishingMortality(
        final_reference_fishing,
        names(fmort_w)[-1L],
        "Final reference fishing mortality"
      )
    }

    list(
      source = net_inputs[, -1L, drop = FALSE],
      validation_targets = standardizeTemporalTargets(
        net_inputs, validation_reference
      ),
      final_targets = standardizeTemporalTargets(
        net_inputs, final_reference
      ),
      validation_reference_fishing = validation_reference,
      final_reference_fishing = final_reference
    )
  }

  insertFeatureValues <- function(target_values, source, source_label) {
    source_matrix <- asFiniteNumericMatrix(source, source_label)
    if (nrow(source_matrix) != 1L) {
      stop(sprintf("%s must contain exactly one row.", source_label),
           call. = FALSE)
    }

    source_names <- colnames(source_matrix)
    if (is.null(source_names) || anyNA(source_names) ||
        any(!nzchar(source_names)) || anyDuplicated(source_names)) {
      stop(sprintf("%s must have unique, non-empty feature names.", source_label),
           call. = FALSE)
    }
    target_positions <- match(source_names, names(target_values))
    if (anyNA(target_positions)) {
      stop(
        sprintf(
          "%s contains features absent from the recursive state: %s.",
          source_label,
          paste(source_names[is.na(target_positions)], collapse = ", ")
        ),
        call. = FALSE
      )
    }
    if (any(!is.na(target_values[target_positions]))) {
      stop(sprintf("%s overlaps features already inserted in the recursive state.",
                   source_label), call. = FALSE)
    }

    target_values[target_positions] <- as.numeric(source_matrix)
    target_values
  }

  appendRecursiveYear <- function(physical_state, model_state, fitted,
                                  transition_fishing_mortality,
                                  reference_fishing_mortality,
                                  transition_natural_mortality,
                                  catch_fishing_mortality,
                                  catch_natural_mortality) {
    feature_names <- colnames(physical_state)[-1L]
    population_positions <- grep("_N_", feature_names, fixed = TRUE)
    if (!length(population_positions)) {
      stop(
        paste(
          "Recursive prediction found no population-abundance columns.",
          "Expected feature names containing '_N_'."
        ),
        call. = FALSE
      )
    }
    environmental_positions <- grep("PrP_", feature_names, fixed = TRUE)
    prediction_input <- buildPredictionWindow(
      model_state[, -1L, drop = FALSE], fitted$lookback
    )
    predicted_features <- coercePredictionRow(
      fitted$model %>% predict(prediction_input, verbose = 0L),
      feature_names
    )

    network_population <- predicted_features[, population_positions, drop = FALSE]
    network_population <- fromModelScale(network_population)
    network_population <- denormalizeInputs(
      network_population,
      fitted$output_range[, population_positions, drop = FALSE]
    )
    network_population[] <- pmax(0, as.matrix(network_population))

    previous_population <- physical_state[
      nrow(physical_state), population_positions + 1L, drop = FALSE
    ]
    predicted_population <- applyRelativeFishingMortality(
      neural_population = network_population,
      previous_population = previous_population,
      scenario_fishing_mortality = transition_fishing_mortality,
      reference_fishing_mortality = reference_fishing_mortality,
      natural_mortality = transition_natural_mortality
    )

    predicted_catch <- catchBaranov(
      catch_fishing_mortality,
      catch_natural_mortality,
      predicted_population
    )
    next_values <- stats::setNames(
      rep(NA_real_, length(feature_names)), feature_names
    )
    next_values <- insertFeatureValues(
      next_values, predicted_population, "Predicted population"
    )
    next_values <- insertFeatureValues(
      next_values, predicted_catch, "Baranov-predicted catch"
    )
    if (length(environmental_positions)) {
      next_environment <- physical_state[
        nrow(physical_state), environmental_positions + 1L, drop = FALSE
      ]
      next_values <- insertFeatureValues(
        next_values, next_environment, "Carried-forward environment"
      )
    }
    if (anyNA(next_values)) {
      stop(
        "Recursive forecast could not construct every required feature.",
        call. = FALSE
      )
    }
    next_features <- matrix(
      unname(next_values), nrow = 1L, ncol = length(feature_names),
      dimnames = list(NULL, feature_names)
    )

    next_year <- as.integer(utils::tail(physical_state$year, 1L) + 1L)
    next_physical_row <- data.frame(
      year = next_year, next_features, check.names = FALSE
    )
    physical_state <- rbind(physical_state, next_physical_row)

    next_model_features <- normalizeUsingRange(
      next_features, fitted$input_range
    )
    next_model_features <- toModelScale(next_model_features)
    next_model_row <- data.frame(
      year = next_year, next_model_features, check.names = FALSE
    )
    model_state <- rbind(model_state, next_model_row)

    list(physical = physical_state, model = model_state)
  }
  
  testFitNet <- function(netInputs) {
    validateLayerConfiguration()
    prepared <- prepareTemporalModelData(netInputs)
    fitted <- fitTemporalModel(
      source_df = prepared$source,
      validation_target_df = prepared$validation_targets,
      final_target_df = prepared$final_targets,
      seed = base_seed + 100L,
      model_name = "fit",
      verbose = 1L
    )
    formatHistoryData(fitted$history, 1L)
  }
  
  maelstromPlotTheme <- function(base_size = 12) {
    theme_minimal(base_size = base_size) +
      theme(
        plot.title = element_text(face = "bold", colour = "#111827", size = 14),
        plot.subtitle = element_text(colour = "#4B5563", size = 10.5,
                                     margin = margin(b = 9)),
        plot.caption = element_text(colour = "#6B7280", hjust = 0, size = 9,
                                    margin = margin(t = 8)),
        axis.title = element_text(face = "bold", colour = "#374151"),
        axis.text = element_text(colour = "#4B5563"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.box = "horizontal",
        legend.margin = margin(t = 8),
        legend.box.margin = margin(t = 4),
        legend.spacing.x = grid::unit(8, "pt"),
        plot.margin = margin(12, 16, 16, 12)
      )
  }

  cleanPlotlyLegendName <- function(name) {
    if (is.null(name) || !length(name)) return("")
    name <- as.character(name[[1L]])
    name <- sub("^\\((.*),[[:space:]]*[0-9]+\\)$", "\\1", name)
    name <- sub(",[[:space:]]*[0-9]+$", "", name)
    name
  }

  asMaelstromPlotly <- function(plot, tooltip = c("x", "y")) {
    widget <- ggplotly(plot, tooltip = tooltip)
    shown_names <- character()
    legend_order <- c(
      "Observed SSB" = 1L,
      "Ensemble mean" = 2L,
      "Ensemble forecast" = 2L,
      "90% interval" = 3L
    )

    if (length(widget$x$data)) {
      for (trace_index in seq_along(widget$x$data)) {
        trace <- widget$x$data[[trace_index]]
        clean_name <- cleanPlotlyLegendName(trace$name)
        trace$legendgrouptitle <- NULL

        if (nzchar(clean_name)) {
          trace$name <- clean_name
          trace$legendgroup <- clean_name
          if (is.null(trace$showlegend) || isTRUE(trace$showlegend)) {
            trace$showlegend <- !clean_name %in% shown_names
            if (isTRUE(trace$showlegend)) {
              shown_names <- c(shown_names, clean_name)
            }
          }
          if (clean_name %in% names(legend_order)) {
            trace$legendrank <- unname(legend_order[[clean_name]])
          }
        }
        widget$x$data[[trace_index]] <- trace
      }
    }

    widget %>%
      layout(
        hovermode = "x unified",
        xaxis = list(tickangle = -45, automargin = TRUE),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.24,
          yanchor = "top",
          title = list(text = ""),
          traceorder = "normal"
        ),
        margin = list(l = 82, r = 28, t = 92, b = 118)
      ) %>%
      config(displaylogo = FALSE, responsive = TRUE)
  }

  prettyYearBreaks <- function(years, maximum_breaks = 9L) {
    years <- sort(unique(as.integer(years[is.finite(years)])))
    if (length(years) <= maximum_breaks) return(years)
    break_positions <- unique(round(seq(1, length(years), length.out = maximum_breaks)))
    years[break_positions]
  }

  annualYearBreaks <- function(years) {
    years <- as.integer(years)
    sort(unique(years[is.finite(years)]))
  }

  ssbYearAxisTheme <- function() {
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  }

  stockAreaLabel <- function(data) {
    paste0(unique(as.character(data$species)), " — GSA ",
           unique(as.character(data$gsa)))
  }

  plotFitNet <- function(history_df) {
    history_df <- history_df[
      as.character(history_df$metric) == "MSE" & is.finite(history_df$value),
      , drop = FALSE
    ]
    if (!nrow(history_df)) {
      stop("No finite MSE history is available for the diagnostic plot.", call. = FALSE)
    }

    history_df$series <- ifelse(
      grepl("validation|^val", tolower(as.character(history_df$data))),
      "Validation", "Training"
    )
    history_df$series <- factor(
      history_df$series, levels = c("Training", "Validation")
    )
    n_runs <- dplyr::n_distinct(history_df$iter)
    diagnostic_colours <- c(Training = "#0072B2", Validation = "#D55E00")

    trajectory <- history_df %>%
      group_by(epoch, series) %>%
      summarise(
        mean_mse = mean(value, na.rm = TRUE),
        sd_mse = ifelse(n_distinct(iter) > 1L, sd(value, na.rm = TRUE), 0),
        n_run = n_distinct(iter),
        .groups = "drop"
      ) %>%
      mutate(
        ci = ifelse(n_run > 1L, 1.96 * sd_mse / sqrt(n_run), 0),
        ymin = pmax(0, mean_mse - ci),
        ymax = mean_mse + ci
      )

    validation_best <- history_df %>%
      filter(series == "Validation") %>%
      group_by(iter) %>%
      arrange(value, epoch, .by_group = TRUE) %>%
      slice_head(n = 1L) %>%
      ungroup() %>%
      transmute(iter, best_epoch = epoch, validation_mse = value)
    if (!nrow(validation_best)) {
      stop("Validation MSE is missing from the diagnostic history.", call. = FALSE)
    }

    training_selected <- history_df %>%
      filter(series == "Training") %>%
      inner_join(validation_best, by = "iter") %>%
      filter(epoch == best_epoch) %>%
      transmute(iter, best_epoch, training_mse = value, validation_mse)
    selected_runs <- validation_best %>%
      left_join(training_selected[, c("iter", "training_mse")], by = "iter")
    selected_long <- bind_rows(
      selected_runs %>% transmute(iter, series = "Training", mse = training_mse),
      selected_runs %>% transmute(iter, series = "Validation", mse = validation_mse)
    ) %>%
      filter(is.finite(mse)) %>%
      mutate(series = factor(series, levels = c("Training", "Validation")))

    median_epoch <- as.integer(round(stats::median(validation_best$best_epoch)))
    epoch_range <- range(validation_best$best_epoch)
    run_label <- if (n_runs == 1L) {
      "single initialization"
    } else {
      paste(n_runs, "independent initializations")
    }

    pA <- ggplot(trajectory, aes(x = epoch, y = mean_mse,
                                colour = series, fill = series)) +
      geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.14, colour = NA) +
      geom_vline(xintercept = median_epoch, linetype = "22",
                 colour = "#6B7280", linewidth = 0.55) +
      geom_line(linewidth = 0.95) +
      scale_colour_manual(values = diagnostic_colours, drop = FALSE) +
      scale_fill_manual(values = diagnostic_colours, drop = FALSE) +
      labs(
        title = "A. Temporal-validation learning curves",
        subtitle = paste0(
          run_label, "; line = mean MSE, band = 95% CI; dashed line = median selected epoch (",
          median_epoch, ")"
        ),
        x = "Epoch", y = "Normalized MSE"
      ) +
      maelstromPlotTheme()

    pB <- ggplot(selected_long, aes(x = series, y = mse, colour = series)) +
      geom_boxplot(aes(fill = series), width = 0.48, alpha = 0.18,
                   outlier.shape = NA, colour = "#6B7280") +
      geom_jitter(width = 0.055, size = 2.1, alpha = 0.78) +
      scale_colour_manual(values = diagnostic_colours, drop = FALSE) +
      scale_fill_manual(values = diagnostic_colours, drop = FALSE) +
      labs(
        title = "B. Error at the selected epoch",
        subtitle = paste0(
          "Best validation epoch: median ", median_epoch,
          " (range ", epoch_range[[1L]], "–", epoch_range[[2L]], ")"
        ),
        x = NULL, y = "Normalized MSE",
        caption = "Points are individual initializations; boxes summarize their distributions."
      ) +
      guides(colour = "none", fill = "none") +
      maelstromPlotTheme()

    wrap_plots(pA, pB, widths = c(1.65, 1))
  }
  
  trainTestFitNet <- function(netInputs, depthTest) {

    validateLayerConfiguration()
    
    population_metadata <- populationFeatureMetadata(netInputs)
    species <- population_metadata$species
    age <- as.character(population_metadata$age_numeric)
    gsa <- population_metadata$gsa
    species_levels <- unique(species)
    min_age_vec <- vapply(species_levels, function(species_code) {
      min(population_metadata$age_numeric[population_metadata$species == species_code])
    }, numeric(1))

    niter <- ensemble_iterations
    cutoff_row <- nrow(netInputs) - depthTest
    if (cutoff_row < 1L) {
      stop("The test depth leaves no years for model training.", call. = FALSE)
    }
    training_data <- netInputs[seq_len(cutoff_row), , drop = FALSE]
    prepared <- prepareTemporalModelData(training_data)
    history_df_tot <- data.frame()

    withProgress(message = "Calculating...", value = 0, detail = "0%", {
      
      for (iter in seq_len(niter)) {
        fitted <- fitTemporalModel(
          source_df = prepared$source,
          validation_target_df = prepared$validation_targets,
          final_target_df = prepared$final_targets,
          seed = base_seed + 1000L + iter,
          model_name = paste0("backtest_", iter)
        )
        iter_physical <- training_data
        iter_model <- training_data
        iter_model[, -1L] <- fitted$model_values

        history_df_tot <- rbind(
          history_df_tot,
          formatHistoryData(fitted$history, iter)
        )
        traintest_metrics <<- history_df_tot
        traintest_nparams <<- fitted$parameter_count

        for (i in seq_len(depthTest)) {
          source_year <- utils::tail(iter_physical$year, 1L)
          target_year <- source_year + 1L
          updated <- withRuntimeStage(
            sprintf("recursive backtest: initialization %s, year %s", iter, target_year),
            appendRecursiveYear(
              physical_state = iter_physical,
              model_state = iter_model,
              fitted = fitted,
              transition_fishing_mortality = historicalRateRow(
                fmort_w, source_year, "source-year fishing-mortality"
              ),
              reference_fishing_mortality =
                prepared$final_reference_fishing,
              transition_natural_mortality = historicalRateRow(
                mort_w, source_year, "source-year natural-mortality"
              ),
              catch_fishing_mortality = historicalRateRow(
                fmort_w, target_year, "fishing-mortality"
              ),
              catch_natural_mortality = historicalRateRow(
                mort_w, target_year, "natural-mortality"
              )
            )
          )
          iter_physical <- updated$physical
          iter_model <- updated$model

          completed_steps <- (iter - 1L) * depthTest + i
          total_steps <- depthTest * niter
          incProgress(
            amount = 1 / total_steps,
            detail = paste0(round(100 * completed_steps / total_steps, 2), "%")
          )
        }

      iter_df <- data.frame(year = rep(seq(min(iter_physical$year), max(iter_physical$year), 1),
                                       length(species)),
                            N = as.numeric(data.matrix(iter_physical[, grep("_N", names(iter_physical))])),
                            species = rep(species, each = nrow(iter_physical)),
                            gsa = rep(gsa, each = nrow(iter_physical)),
                            age = as.numeric(rep(age, each = nrow(iter_physical))),
                            type = "Predicted",
                            iter = iter
      )
      
      obs_df <- data.frame(year = rep(seq(min(netInputs$year), max(netInputs$year), 1),
                                       length(species)),
                            N = as.numeric(data.matrix(netInputs[, grep("_N", names(netInputs))])),
                            species = rep(species, each = nrow(netInputs)),
                            gsa = rep(gsa, each = nrow(netInputs)),
                            age = as.numeric(rep(age, each = nrow(netInputs))),
                            type = "Observed",
                            iter = iter
      )
      
      def_df <- rbind(obs_df, iter_df)
      
      if (iter == 1) {
        traintest_df_raw <- def_df
        traintest_df_raw[which(traintest_df_raw$type == "Observed"), "iter"] = 0
      } else {
        traintest_df_raw <- rbind(traintest_df_raw, def_df[which(def_df$type == "Predicted"),])
      }
      
      # Create recruitment dataframe
      
      minimum_age_by_species <- stats::setNames(min_age_vec, species_levels)
      recruitment_rows <- def_df$age == unname(
        minimum_age_by_species[as.character(def_df$species)]
      )
      recr_iter <- def_df[recruitment_rows, , drop = FALSE]
      recr_iter <- recr_iter[order(recr_iter$species), ]
  
      # Create total biomass dataframe
      
      ssb_df_convert <- data.frame()
      ssb_iter <- def_df
      ssb_species <- unique(ssb_iter$species)
      
      for (sp in seq_along(ssb_species)) {
        ssb_iter_sub <- ssb_iter[which(ssb_iter$species == ssb_species[sp]),]
        ssb_year <- sort(unique(ssb_iter_sub$year))
        ssb_age <- sort(unique(ssb_iter_sub$age))
        
        for (y in seq_along(ssb_year)) {
          for (a in seq_along(ssb_age)) {
            ssb_iter_sub[which(ssb_iter_sub$year == ssb_year[y] & ssb_iter_sub$age == ssb_age[a]), "N"] <- ssb_iter_sub[which(ssb_iter_sub$year == ssb_year[y] & ssb_iter_sub$age == ssb_age[a]), "N"] * exp(-(fmort_l[which(fmort_l$year == ssb_year[y] & fmort_l$age == ssb_age[a] & fmort_l$species == unique(ssb_iter_sub$species)), "fmort"] * fmort_spawn_l[which(fmort_spawn_l$year == ssb_year[y] & fmort_spawn_l$age == ssb_age[a] & fmort_spawn_l$species == unique(ssb_iter_sub$species)), "fmort_spawn"] + mort_l[which(mort_l$year == ssb_year[y] & mort_l$age == ssb_age[a] & mort_l$species == unique(ssb_iter_sub$species)), "mort"] * mort_spawn_l[which(mort_spawn_l$year == ssb_year[y] & mort_spawn_l$age == ssb_age[a] & mort_spawn_l$species == unique(ssb_iter_sub$species)), "mort_spawn"])) * waa_l[which(waa_l$year == ssb_year[y] & waa_l$age == ssb_age[a] & waa_l$species == unique(ssb_iter_sub$species)), "weight_at_age"] * mature_l[which(mature_l$year == ssb_year[y] & mature_l$age == ssb_age[a] & mature_l$species == unique(ssb_iter_sub$species)), "mature"]
          }
        }
        if (sp == 1) {
          ssb_df_convert <- ssb_iter_sub
        } else {
          ssb_df_convert <- rbind(ssb_df_convert, ssb_iter_sub)
        }
      }
      colnames(ssb_df_convert)[2] <- "ssb"
      
      ssb_df = aggregate(data = ssb_df_convert, ssb ~ year + species + gsa + type + iter, FUN = "sum")
      ssb_df = ssb_df[order(ssb_df$species),]

      ssb_df$recruitment <- NA
      ssb_df$recruitment <- recr_iter$N
      
      if (iter == 1) {
        ssb_df_tot <- ssb_df
      } else {
        ssb_df_tot <- rbind(ssb_df_tot, ssb_df)
      }
      
      }
    })
    
    traintest_output_raw <<- traintest_df_raw
    traintest_iter_results <<- vector(mode = "list", length = length(unique(species)))
    for (sp in seq_along(unique(species))) {
      traintest_iter_results[[sp]] <<- ssb_df_tot[which(ssb_df_tot$species == unique(species)[sp]),]
    }
    
    proj_biomass_spec <- list()
    
    if (length(unique(species)) == 1) {
      
      sp_biomass_sub <- ssb_df_tot
      rows_per_series <- as.integer(nrow(sp_biomass_sub) / (niter * 2L))
      series_rows <- seq_len(rows_per_series)
      sp_biomass_wide <- data.frame(year = sp_biomass_sub$year[series_rows],
                                    species = sp_biomass_sub$species[series_rows],
                                    gsa = sp_biomass_sub$gsa[series_rows],
                                    ssb_obs = sp_biomass_sub$ssb[which(sp_biomass_sub$type == "Observed" & sp_biomass_sub$iter == 1)],
                                    recr_obs = sp_biomass_sub$recruitment[which(sp_biomass_sub$type == "Observed" & sp_biomass_sub$iter == 1)],
                                    ssb_min = NA, ssb_mean = NA, ssb_max = NA,
                                    recr_min = NA, recr_mean = NA, recr_max = NA,
                                    rmse_min = NA, rmse_mean = NA, rmse_max = NA,
                                    mae_min = NA, mae_mean = NA, mae_max = NA)

      for (i in seq_len(nrow(sp_biomass_wide))) {
        predicted_rows <- which(sp_biomass_sub$year == sp_biomass_wide$year[i] &
                                  sp_biomass_sub$type == "Predicted")
        ssb_summary <- ensembleSummary(sp_biomass_sub[predicted_rows, "ssb"])
        recruitment_summary <- ensembleSummary(sp_biomass_sub[predicted_rows, "recruitment"])
        sp_biomass_wide[i, c("ssb_min", "ssb_mean", "ssb_max")] <- ssb_summary
        sp_biomass_wide[i, c("recr_min", "recr_mean", "recr_max")] <- recruitment_summary
      }
      
      results_incasting <- sp_biomass_wide[(nrow(sp_biomass_wide) - (depthTest - 1)):nrow(sp_biomass_wide), c("ssb_obs", "ssb_min", "ssb_mean", "ssb_max")]
      sp_biomass_wide$rmse_min <- round(rmse(results_incasting$ssb_obs, results_incasting$ssb_min), 2)
      sp_biomass_wide$rmse_mean <- round(rmse(results_incasting$ssb_obs, results_incasting$ssb_mean), 2)
      sp_biomass_wide$rmse_max <- round(rmse(results_incasting$ssb_obs, results_incasting$ssb_max), 2)
      sp_biomass_wide$mae_min <- round(mae(results_incasting$ssb_obs, results_incasting$ssb_min), 2)
      sp_biomass_wide$mae_mean <- round(mae(results_incasting$ssb_obs, results_incasting$ssb_mean), 2)
      sp_biomass_wide$mae_max <- round(mae(results_incasting$ssb_obs, results_incasting$ssb_max), 2)
      
      proj_biomass_spec[[1]] <- sp_biomass_wide
      
    } else {
      
      for (sp in seq_along(unique(species))) {
        
        sp_biomass_sub <- ssb_df_tot[which(ssb_df_tot$species == unique(species)[sp]),]
        rows_per_series <- as.integer(nrow(sp_biomass_sub) / (niter * 2L))
        series_rows <- seq_len(rows_per_series)
        sp_biomass_wide <- data.frame(year = sp_biomass_sub$year[series_rows],
                                      species = sp_biomass_sub$species[series_rows],
                                      gsa = sp_biomass_sub$gsa[series_rows],
                                      ssb_obs = sp_biomass_sub$ssb[which(sp_biomass_sub$type == "Observed" & sp_biomass_sub$iter == 1)],
                                      recr_obs = sp_biomass_sub$recruitment[which(sp_biomass_sub$type == "Observed" & sp_biomass_sub$iter == 1)],
                                      ssb_min = NA, ssb_mean = NA, ssb_max = NA,
                                      recr_min = NA, recr_mean = NA, recr_max = NA,
                                      rmse_min = NA, rmse_mean = NA, rmse_max = NA,
                                      mae_min = NA, mae_mean = NA, mae_max = NA)
        
        for (i in seq_len(nrow(sp_biomass_wide))) {
          predicted_rows <- which(sp_biomass_sub$year == sp_biomass_wide$year[i] &
                                    sp_biomass_sub$type == "Predicted")
          ssb_summary <- ensembleSummary(sp_biomass_sub[predicted_rows, "ssb"])
          recruitment_summary <- ensembleSummary(sp_biomass_sub[predicted_rows, "recruitment"])
          sp_biomass_wide[i, c("ssb_min", "ssb_mean", "ssb_max")] <- ssb_summary
          sp_biomass_wide[i, c("recr_min", "recr_mean", "recr_max")] <- recruitment_summary
        }
        
        results_incasting <- sp_biomass_wide[(nrow(sp_biomass_wide) - (depthTest - 1)):nrow(sp_biomass_wide), c("ssb_obs", "ssb_min", "ssb_mean", "ssb_max")]
        sp_biomass_wide$rmse_min <- round(rmse(results_incasting$ssb_obs, results_incasting$ssb_min), 2)
        sp_biomass_wide$rmse_mean <- round(rmse(results_incasting$ssb_obs, results_incasting$ssb_mean), 2)
        sp_biomass_wide$rmse_max <- round(rmse(results_incasting$ssb_obs, results_incasting$ssb_max), 2)
        sp_biomass_wide$mae_min <- round(mae(results_incasting$ssb_obs, results_incasting$ssb_min), 2)
        sp_biomass_wide$mae_mean <- round(mae(results_incasting$ssb_obs, results_incasting$ssb_mean), 2)
        sp_biomass_wide$mae_max <- round(mae(results_incasting$ssb_obs, results_incasting$ssb_max), 2)
        
        proj_biomass_spec[[sp]] <- sp_biomass_wide
        
      }
      
    }
    
    return(proj_biomass_spec)
    
  }
  
  plotTrainTestFitNet <- function(proj_biomass, plotTrainTestFitCount, depthTest) {
    depthTest <- as.integer(depthTest)
    first_test_year <- max(proj_biomass$year) - depthTest + 1L
    connection_year <- first_test_year - 1L
    projected_window <- proj_biomass[
      proj_biomass$year >= connection_year, , drop = FALSE
    ]
    test_window <- proj_biomass[
      proj_biomass$year >= first_test_year, , drop = FALSE
    ]
    metric_value <- function(column) {
      values <- unique(as.numeric(proj_biomass[[column]]))
      values <- values[is.finite(values)]
      if (length(values)) values[[1L]] else NA_real_
    }
    rmse_value <- metric_value("rmse_mean")
    mae_value <- metric_value("mae_mean")
    metric_text <- if (is.finite(rmse_value) && is.finite(mae_value)) {
      paste0("Holdout RMSE = ", format(rmse_value, big.mark = ",", trim = TRUE),
             "; MAE = ", format(mae_value, big.mark = ",", trim = TRUE), ". ")
    } else {
      ""
    }

    ggplot() +
      annotate(
        "rect", xmin = first_test_year - 0.5, xmax = Inf,
        ymin = -Inf, ymax = Inf, fill = "#F59E0B", alpha = 0.055
      ) +
      geom_vline(
        xintercept = first_test_year - 0.5,
        linetype = "22", colour = "#9CA3AF", linewidth = 0.65
      ) +
      geom_ribbon(
        data = projected_window,
        aes(x = year, ymin = ssb_min, ymax = ssb_max, fill = "90% interval"),
        alpha = 0.22, colour = NA
      ) +
      geom_line(
        data = projected_window,
        aes(x = year, y = ssb_mean, colour = "Ensemble mean"),
        linewidth = 1.15
      ) +
      geom_point(
        data = test_window,
        aes(x = year, y = ssb_mean, colour = "Ensemble mean"),
        size = 2.35
      ) +
      geom_line(
        data = proj_biomass,
        aes(x = year, y = ssb_obs, colour = "Observed SSB"),
        linewidth = 1.05
      ) +
      geom_point(
        data = proj_biomass,
        aes(x = year, y = ssb_obs, colour = "Observed SSB"),
        size = 1.9
      ) +
      scale_colour_manual(
        values = c("Observed SSB" = "#1F2937", "Ensemble mean" = "#D55E00"),
        breaks = c("Observed SSB", "Ensemble mean")
      ) +
      scale_fill_manual(values = c("90% interval" = "#E69F00")) +
      scale_x_continuous(
        breaks = annualYearBreaks(proj_biomass$year),
        expand = expansion(mult = c(0.015, 0.035))
      ) +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      labs(
        title = paste("SSB backtest —", stockAreaLabel(proj_biomass)),
        subtitle = paste0(
          "Model fitted through ", connection_year, "; holdout period ",
          first_test_year, "–", max(proj_biomass$year), "."
        ),
        x = "Year", y = "Spawning stock biomass (tonnes)",
        caption = paste0(
          metric_text,
          paste(
            "Counterfactual backtest: the full neural abundance vector is",
            "corrected from reference F to historical F; "
          ),
          "ribbon: empirical 5th–95th percentiles across initializations."
        )
      ) +
      guides(
        colour = guide_legend(order = 1, override.aes = list(linewidth = 1.1)),
        fill = guide_legend(order = 2)
      ) +
      maelstromPlotTheme() +
      ssbYearAxisTheme()
  }
  
  plotRecruitment <- function(proj_biomass, plotPredCount, depth) {
    depth <- as.integer(depth)
    is_backtest <- "ssb_obs" %in% names(proj_biomass)

    if (is_backtest) {
      first_prediction_year <- max(proj_biomass$year) - depth + 1L
      connection_year <- first_prediction_year - 1L
      observed_data <- proj_biomass
      observed_data$recruitment_observed <- if ("recr_obs" %in% names(observed_data)) {
        observed_data$recr_obs
      } else {
        # Compatibility with results saved before recruitment observations
        # became an explicit output column.
        ifelse(observed_data$year < first_prediction_year,
               observed_data$recr_mean, NA_real_)
      }
      prediction_data <- proj_biomass[
        proj_biomass$year >= connection_year, , drop = FALSE
      ]
      prediction_points <- prediction_data[
        prediction_data$year >= first_prediction_year, , drop = FALSE
      ]
      plot_title <- paste("Recruitment backtest —", stockAreaLabel(proj_biomass))
      plot_subtitle <- paste0(
        "Observed recruitment versus recursive predictions for ",
        first_prediction_year, "–", max(proj_biomass$year), "."
      )
    } else {
      observed_data <- proj_biomass[
        as.character(proj_biomass$type) == "Observed", , drop = FALSE
      ]
      forecast_rows <- as.character(proj_biomass$type) == "Forecast"
      if (!nrow(observed_data) || !any(forecast_rows)) {
        stop("Recruitment forecast plot requires observed and forecast years.",
             call. = FALSE)
      }
      first_prediction_year <- min(proj_biomass$year[forecast_rows])
      connection_year <- first_prediction_year - 1L
      observed_data$recruitment_observed <- observed_data$recr_mean
      # Select the connection row from the unmodified source table. Selecting
      # it from observed_data would include the display-only
      # recruitment_observed column and make rbind() incompatible with the
      # forecast rows.
      connection_data <- proj_biomass[
        as.character(proj_biomass$type) == "Observed" &
          proj_biomass$year == connection_year, , drop = FALSE
      ]
      prediction_points <- proj_biomass[
        forecast_rows, , drop = FALSE
      ]
      if (nrow(connection_data) != 1L) {
        stop("Recruitment forecast plot requires one observed connection year.",
             call. = FALSE)
      }
      prediction_data <- rbind(connection_data, prediction_points)
      plot_title <- paste("Recruitment forecast —", stockAreaLabel(proj_biomass))
      plot_subtitle <- paste0(
        "Observed through ", connection_year, "; forecast ",
        first_prediction_year, "–", max(proj_biomass$year), "."
      )
    }

    ggplot() +
      annotate(
        "rect", xmin = first_prediction_year - 0.5, xmax = Inf,
        ymin = -Inf, ymax = Inf, fill = "#F59E0B", alpha = 0.055
      ) +
      geom_vline(
        xintercept = first_prediction_year - 0.5,
        linetype = "22", colour = "#9CA3AF", linewidth = 0.65
      ) +
      geom_ribbon(
        data = prediction_data,
        aes(x = year, ymin = recr_min, ymax = recr_max, fill = "90% interval"),
        alpha = 0.22, colour = NA
      ) +
      geom_line(
        data = prediction_data,
        aes(x = year, y = recr_mean, colour = "Ensemble mean"),
        linewidth = 1.15
      ) +
      geom_point(
        data = prediction_points,
        aes(x = year, y = recr_mean, colour = "Ensemble mean"),
        size = 2.35
      ) +
      geom_line(
        data = observed_data,
        aes(x = year, y = recruitment_observed, colour = "Observed recruitment"),
        linewidth = 1.05
      ) +
      geom_point(
        data = observed_data,
        aes(x = year, y = recruitment_observed, colour = "Observed recruitment"),
        size = 1.9
      ) +
      scale_colour_manual(
        values = c(
          "Observed recruitment" = "#1F2937",
          "Ensemble mean" = "#D55E00"
        ),
        breaks = c("Observed recruitment", "Ensemble mean")
      ) +
      scale_fill_manual(values = c("90% interval" = "#E69F00")) +
      scale_x_continuous(
        breaks = prettyYearBreaks(c(observed_data$year, prediction_data$year)),
        expand = expansion(mult = c(0.015, 0.035))
      ) +
      scale_y_continuous(
        labels = scales::label_number(scale = 1 / 1e6, accuracy = 0.1,
                                      big.mark = ",")
      ) +
      labs(
        title = plot_title,
        subtitle = plot_subtitle,
        x = "Year", y = "Recruitment (million individuals)",
        caption = "Ribbon: empirical 5th–95th percentiles across initializations."
      ) +
      guides(
        colour = guide_legend(order = 1, override.aes = list(linewidth = 1.1)),
        fill = guide_legend(order = 2)
      ) +
      maelstromPlotTheme()
  }
  
  plotTaylorDiagram <- function(proj_biomass) {
    
    prepData <- function(df, ssb_col = "ssb", type_col = "type", iter_col = "iter",
                         year_col = "year", observed_label = "Observed",
                         predicted_label = "Predicted", sd.method = "sample",
                         grad.corr.lines = c(0.2, 0.4, 0.6, 0.8, 0.9)) {
      
      SD <- function(x, subn) {
        x <- as.numeric(x)
        meanx <- mean(x, na.rm = TRUE)
        devx <- x - meanx
        sqrt(sum(devx * devx, na.rm = TRUE) / (sum(!is.na(x)) - subn))
      }
      
      subn <- sd.method != "sample"
      
      df_wide <- df %>%
        dplyr::filter(.data[[type_col]] %in% c(observed_label, predicted_label)) %>%
        dplyr::select(iter = dplyr::all_of(iter_col), year = dplyr::all_of(year_col),
                      type = dplyr::all_of(type_col), ssb  = dplyr::all_of(ssb_col)) %>%
        dplyr::mutate(ssb = as.numeric(ssb)) %>%
        dplyr::group_by(iter, year, type) %>%
        dplyr::summarise(ssb = mean(ssb, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = type, values_from = ssb)
      
      obs <- df_wide %>%
        dplyr::group_by(year) %>%
        dplyr::summarise(obs = dplyr::first(stats::na.omit(.data[[observed_label]])), .groups = "drop") %>%
        dplyr::arrange(year) %>%
        dplyr::pull(obs)
      
      sd.ref <- SD(obs, subn)
      
      stats_iter <- df_wide %>%
        dplyr::group_by(iter) %>%
        dplyr::arrange(year, .by_group = TRUE) %>%
        dplyr::summarise(sd_pred = SD(.data[[predicted_label]], subn), 
                         corr = abs(cor(.data[[observed_label]], .data[[predicted_label]], use = "pairwise.complete.obs")), .groups = "drop") %>%
        dplyr::slice_head(n = 10) %>%
        dplyr::mutate(x = sd_pred * corr, y = sd_pred * sin(acos(corr)), group = "model")
      
      model_points <- stats_iter %>%
        dplyr::transmute(id = iter, x = x, y = y, group = group)
      
      pred_mean <- df_wide %>%
        dplyr::group_by(year) %>%
        dplyr::summarise(obs = dplyr::first(stats::na.omit(.data[[observed_label]])),
                         pred_avg = mean(.data[[predicted_label]], na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(year)
      
      sd.media <- SD(pred_mean$pred_avg, subn)
      
      corr.media <- abs(cor(pred_mean$obs, pred_mean$pred_avg, use = "pairwise.complete.obs"))
      
      mean_point <- data.frame(id = NA, x = sd.media * corr.media, y = sd.media * sin(acos(corr.media)), group = "mean prediction")
      
      ref_point <- data.frame(x = sd.ref, y = 0)
      
      maxsd <- 1.5 * max(stats_iter$sd_pred, sd.ref, sd.media, na.rm = TRUE)
      
      axis <- data.frame(x = c(0, 0), y = c(0, 0), xend = c(0, maxsd), yend = c(maxsd, 0))
      
      axis.ticks <- pretty(c(0, maxsd))
      axis.ticks <- axis.ticks[axis.ticks <= maxsd]
      
      corr_lines <- do.call(rbind, lapply(grad.corr.lines, function(gcl) {
        data.frame(group = gcl, x = c(0, maxsd * gcl),  y = c(0, maxsd * sqrt(1 - gcl^2)))
      }))
      
      arc_angle <- seq(0, pi/2, by = 0.01)
      
      ext_arc <- data.frame(x = cos(arc_angle) * maxsd, y = sin(arc_angle) * maxsd)
      
      bigtickangles <- acos(seq(0.1, 0.9, by = 0.1))
      medtickangles <- acos(seq(0.05, 0.95, by = 0.1))
      smltickangles <- acos(seq(0.91, 0.99, by = 0.01))
      
      big_ticks <- data.frame(x = cos(bigtickangles) * maxsd, y = sin(bigtickangles) * maxsd,
                              xend = cos(bigtickangles) * 0.97 * maxsd, yend = sin(bigtickangles) * 0.97 * maxsd)
      
      medticks <- data.frame(x = cos(medtickangles) * maxsd, y = sin(medtickangles) * maxsd,
                             xend = cos(medtickangles) * 0.98 * maxsd, yend = sin(medtickangles) * 0.98 * maxsd)
      
      smlticks <- data.frame(x = cos(smltickangles) * maxsd, y = sin(smltickangles) * maxsd,
                             xend = cos(smltickangles) * 0.99 * maxsd, yend = sin(smltickangles) * 0.99 * maxsd)
      
      ang_labels <- c(bigtickangles, acos(c(0.95, 0.99)))
      val_labels <- c(seq(0.1, 0.9, by = 0.1), 0.95, 0.99)
      
      corr_labels <- data.frame(x = cos(ang_labels) * 1.05 * maxsd, y = sin(ang_labels) * 1.05 * maxsd, label = val_labels)
      
      list(
        model_points = model_points,
        mean_point = mean_point,
        ref_point = ref_point,
        axis = axis,
        axis.ticks = axis.ticks,
        corr_lines = corr_lines,
        ext_arc = ext_arc,
        big_ticks = big_ticks,
        medticks = medticks,
        smlticks = smlticks,
        corr_labels = corr_labels,
        maxsd = maxsd,
        sd.ref = sd.ref
      )
    }
    
    taylorDiagram <- function(dati, col = "#D55E00", pch = 19,
                              xlab = "Standard deviation", ylab = "",
                              main = "",
                              show.gamma = TRUE, gamma.col = 8,
                              pcex = 2, cex.axis = 1,
                              lwd.axes = 2, lwd.curve = 2) {
      
      if (nchar(ylab) == 0) ylab <- "Standard deviation"
      
      maxsd <- dati$maxsd
      lim <- maxsd * 1.1
      
      p <- ggplot() +
        # Correlation radial lines
        geom_line(data = dati$corr_lines, aes(x = x, y = y, group = group), linetype = "dotted", linewidth = lwd.axes / 2.5) +
        # Cartesian axes
        geom_segment(data = dati$axis, aes(x = x, y = y, xend = xend, yend = yend), linewidth = lwd.axes / 2.5)
      
      # Centered RMS
      if (show.gamma[1] && !is.null(dati$curve_gamma)) {
        p <- p +
          geom_path(data = dati$curve_gamma, aes(x = x, y = y, group = group), colour = "grey50", linewidth = lwd.curve / 2.5) +
          geom_label(data = dati$etichette_gamma, aes(x = x, y = y, label = label), size = 3 * cex.axis,
                     label.size = 0, fill = "white", label.padding = unit(0.08, "lines"))
      }
      
      # SD arcs
      if (!is.null(dati$archi_sd)) {
        p <- p + geom_path(data = dati$archi_sd, aes(x = x, y = y, group = group), colour = "blue", linetype = "dotted", linewidth = lwd.curve / 2.5)
      }
      
      p <- p +
        # External arc
        geom_path(data = dati$ext_arc, aes(x = x, y = y), linewidth = lwd.axes / 2.5) +
        # Correlation ticks (arc)
        geom_segment(data = dati$big_ticks, aes(x = x, y = y, xend = xend, yend = yend), linewidth = lwd.axes / 2.5) +
        geom_segment(data = dati$medticks, aes(x = x, y = y, xend = xend, yend = yend), linewidth = lwd.axes / 5) +
        geom_segment(data = dati$smlticks, aes(x = x, y = y, xend = xend, yend = yend), linewidth = lwd.axes / 5) +
        # Correlation labels
        geom_text(data = dati$corr_labels, aes(x = x, y = y, label = label), size = 3.2 * cex.axis) +
        annotate("text", x = maxsd * 0.8, y = maxsd * 0.8, label = "Correlation", angle = -40, size = 3.5 * cex.axis) +
        # Reference point
        geom_point(data = dati$ref_point, aes(x = x, y = y),
                   shape = 1, size = pcex * 2, stroke = 1.2) +
        # Model points
        geom_point(data = dati$model_points, aes(x = x, y = y, colour = "Iterations"), shape = pch, size = pcex * 2) +
        # Mean point
        geom_point(data = dati$mean_point, aes(x = x, y = y, colour = "Mean"), fill = "darkblue", shape = 24, size = pcex * 2) +
        scale_colour_manual(
          values = c("Iterations" = "#D55E00", "Mean" = "black"),
          breaks = c("Iterations", "Mean")
        ) +
        scale_x_continuous(name = NULL, limits = c(0, lim), expand = c(0, 0), breaks = dati$axis.ticks) +
        scale_y_continuous(name = ylab, limits = c(0, lim), expand = c(0, 0), breaks = dati$axis.ticks) +
        coord_fixed(ratio = 1, clip = "off") +
        labs(title = main, x = xlab) +
        theme_bw(base_size = 11 * cex.axis) +
        theme(
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.title.x = element_text(margin = margin(t = 8)),
          plot.title = element_text(hjust = 0.5)
        )
      p
    }
    
    taylor_data <- prepData(proj_biomass)
    g <- taylorDiagram(taylor_data, main = "Taylor diagram - SSB observed vs predicted")
    
    return(g)
  }
  
  predNet <- function(netInputs, f_reference, f_adj, depth) {

    validateLayerConfiguration()
    last_observed_year <- max(as.integer(netInputs$year))
    fishing_schedule <- forecastFishingSchedule(
      f_adj, names(fmort_w)[-1L], last_observed_year, depth
    )

    population_metadata <- populationFeatureMetadata(netInputs)
    species <- population_metadata$species
    age <- as.character(population_metadata$age_numeric)
    gsa <- population_metadata$gsa
    species_levels <- unique(species)
    min_age_vec <- vapply(species_levels, function(species_code) {
      min(population_metadata$age_numeric[population_metadata$species == species_code])
    }, numeric(1))

    model_pred <<- vector("list", forecast_iterations)
    pred_output_raw <<- vector("list", forecast_iterations)
    prepared <- prepareTemporalModelData(
      netInputs, final_reference_fishing = f_reference
    )

    withProgress(message = "Calculating...", value = 0, detail = "0%", {
      for (iter in seq_len(forecast_iterations)) {
        fitted <- fitTemporalModel(
          source_df = prepared$source,
          validation_target_df = prepared$validation_targets,
          final_target_df = prepared$final_targets,
          seed = base_seed + 2000L + iter,
          model_name = paste0("forecast_", iter)
        )
        proj_physical <- netInputs
        proj_model <- netInputs
        proj_model[, -1L] <- fitted$model_values

        last_historical_fishing <- historicalRateRow(
          fmort_w, last_observed_year, "last-observed fishing-mortality"
        )
        future_natural_mortality <- historicalRateRow(
          mort_w, last_observed_year, "last-observed natural-mortality"
        )

        for (i in seq_len(depth)) {
          current_fishing <- fishing_schedule[i, -1L, drop = FALSE]
          transition_fishing <- if (i == 1L) {
            last_historical_fishing
          } else {
            fishing_schedule[i - 1L, -1L, drop = FALSE]
          }
          updated <- appendRecursiveYear(
            physical_state = proj_physical,
            model_state = proj_model,
            fitted = fitted,
            transition_fishing_mortality = transition_fishing,
            reference_fishing_mortality =
              prepared$final_reference_fishing,
            transition_natural_mortality = future_natural_mortality,
            catch_fishing_mortality = current_fishing,
            catch_natural_mortality = future_natural_mortality
          )
          proj_physical <- updated$physical
          proj_model <- updated$model

          completed_steps <- (iter - 1L) * depth + i
          total_steps <- forecast_iterations * depth
          incProgress(
            amount = 1 / total_steps,
            detail = paste0(round(100 * completed_steps / total_steps, 2), "%")
          )
        }

        proj <- proj_physical
        pred_output_raw[[iter]] <<- proj
        model_pred[[iter]] <<- fitted$model
        
        # Wide to long
        proj_df <- data.frame(year = rep(seq(min(netInputs$year),
                                             (max(netInputs$year) + depth), 1),
                                         length(species)),
                              N = as.numeric(data.matrix(proj[, grep("_N", names(proj))])),
                              species = rep(species, each = nrow(proj)),
                              gsa = rep(gsa, each = nrow(proj)),
                              age = as.numeric(rep(age, each = nrow(proj))),
                              iter = iter
        )
        
        # Make sure that last observed year are not overwritten
        proj_df$N[which(proj_df$year == (max(unique(netInputs$year)) - 1))] = as.numeric(netInputs[, grep("_N", names(netInputs))][(nrow(netInputs) - 1),])
        proj_df$N[which(proj_df$year == max(unique(netInputs$year)))] = as.numeric(netInputs[, grep("_N", names(netInputs))][nrow(netInputs),])
        
        # Label observed and predicted years
        type = character(nrow(proj_df))
        type[which(proj_df$year %in% netInputs$year)] = "Observed"
        type[which(type != "Observed")] = "Forecast"
        proj_df$type = factor(type, levels = c("Observed", "Forecast"))
        proj_df[which(proj_df$type == "Observed"), "N"] = as.numeric(data.matrix(netInputs[, grep("_N", names(netInputs))]))

        # Create recruitment dataframe
        minimum_age_by_species <- stats::setNames(min_age_vec, species_levels)
        recruitment_rows <- proj_df$age == unname(
          minimum_age_by_species[as.character(proj_df$species)]
        )
        recr_iter <- proj_df[recruitment_rows, , drop = FALSE]
        recr_iter <- recr_iter[order(recr_iter$species), ]
        
        # Create total biomass dataframe
        proj_df_convert <- data.frame()
        ssb_iter <- proj_df
        
        ssb_species <- unique(ssb_iter$species)
        
        for (sp in seq_along(ssb_species)) {
          ssb_iter_sub <- ssb_iter[which(ssb_iter$species == ssb_species[sp]),]
          ssb_year <- sort(unique(ssb_iter_sub$year))
          ssb_age <- sort(unique(ssb_iter_sub$age))

          stock_code <- ssb_species[sp]
          stock_gsa <- unique(as.character(ssb_iter_sub$gsa))
          if (length(stock_gsa) != 1L) {
            stop("Forecast SSB requires one GSA per stock.", call. = FALSE)
          }
          fmort_ssb <- extendScenarioFishingMortality(
            historical_data = fmort_l[
              fmort_l$species == stock_code, , drop = FALSE
            ],
            scenario_schedule = fishing_schedule,
            last_observed_year = last_observed_year,
            depth = depth,
            species_code = stock_code,
            gsa_code = stock_gsa
          )
          fmort_spawn_ssb <- extendLastBiologicalYear(
            fmort_spawn_l[fmort_spawn_l$species == stock_code, , drop = FALSE], depth
          )
          mort_ssb <- extendLastBiologicalYear(
            mort_l[mort_l$species == stock_code, , drop = FALSE], depth
          )
          mort_spawn_ssb <- extendLastBiologicalYear(
            mort_spawn_l[mort_spawn_l$species == stock_code, , drop = FALSE], depth
          )
          waa_ssb <- extendLastBiologicalYear(
            waa_l[waa_l$species == stock_code, , drop = FALSE], depth
          )
          mature_ssb <- extendLastBiologicalYear(
            mature_l[mature_l$species == stock_code, , drop = FALSE], depth
          )
          
          for (y in seq_along(ssb_year)) {
            for (a in seq_along(ssb_age)) {
              ssb_iter_sub[which(ssb_iter_sub$year == ssb_year[y] & ssb_iter_sub$age == ssb_age[a]), "N"] <- ssb_iter_sub[which(ssb_iter_sub$year == ssb_year[y] & ssb_iter_sub$age == ssb_age[a]), "N"] * exp(-(fmort_ssb[which(fmort_ssb$year == ssb_year[y] & fmort_ssb$age == ssb_age[a]), "fmort"] * fmort_spawn_ssb[which(fmort_spawn_ssb$year == ssb_year[y] & fmort_spawn_ssb$age == ssb_age[a]), "fmort_spawn"] + mort_ssb[which(mort_ssb$year == ssb_year[y] & mort_ssb$age == ssb_age[a]), "mort"] * mort_spawn_ssb[which(mort_spawn_ssb$year == ssb_year[y] & mort_spawn_ssb$age == ssb_age[a]), "mort_spawn"])) * waa_ssb[which(waa_ssb$year == ssb_year[y] & waa_ssb$age == ssb_age[a]), "weight_at_age"] * mature_ssb[which(mature_ssb$year == ssb_year[y] & mature_ssb$age == ssb_age[a]), "mature"]
            }
          }
          if (sp == 1) {
            ssb_df_convert <- ssb_iter_sub
          } else {
            ssb_df_convert <- rbind(ssb_df_convert, ssb_iter_sub)
          }
          }
        
          colnames(ssb_df_convert)[2] <- "ssb"
          
          ssb_df = aggregate(data = ssb_df_convert, ssb ~ year + species + gsa + type + iter, FUN = "sum")
          ssb_df = ssb_df[order(ssb_df$species),]
          
          ssb_df$recruitment <- NA
          ssb_df$recruitment <- recr_iter$N
          
          if (iter == 1) {
            ssb_df_tot <- ssb_df
          } else {
            ssb_df_tot <- rbind(ssb_df_tot, ssb_df)
          }
          
        }
      })
    
    pred_iter_partial <<- ssb_df_tot
    
    proj_biomass_spec <- list()
    
    if (length(unique(species)) == 1) {
      
      sp_biomass_sub <- ssb_df_tot
      rows_per_iteration <- nrow(sp_biomass_sub) / forecast_iterations
      sp_biomass_wide <- data.frame(year = sp_biomass_sub$year[seq_len(rows_per_iteration)],
                                    species = sp_biomass_sub$species[seq_len(rows_per_iteration)],
                                    gsa = sp_biomass_sub$gsa[seq_len(rows_per_iteration)],
                                    type = sp_biomass_sub$type[seq_len(rows_per_iteration)],
                                    ssb_min = NA, ssb_mean = NA, ssb_max = NA,
                                    recr_min = NA, recr_mean = NA, recr_max = NA)
      
      for (i in seq_len(nrow(sp_biomass_wide))) {
        year_rows <- which(sp_biomass_sub$year == sp_biomass_wide$year[i])
        ssb_summary <- ensembleSummary(sp_biomass_sub[year_rows, "ssb"])
        recruitment_summary <- ensembleSummary(sp_biomass_sub[year_rows, "recruitment"])
        sp_biomass_wide[i, c("ssb_min", "ssb_mean", "ssb_max")] <- ssb_summary
        sp_biomass_wide[i, c("recr_min", "recr_mean", "recr_max")] <- recruitment_summary
      }
      
      proj_biomass_spec[[1]] <- sp_biomass_wide
      
    } else {
      
      for (j in seq_along(unique(species))) {
        
        sp_biomass_sub <- ssb_df_tot[which(ssb_df_tot$species == unique(species)[j]), ]
        rows_per_iteration <- nrow(sp_biomass_sub) / forecast_iterations
        sp_biomass_wide <- data.frame(year = sp_biomass_sub$year[seq_len(rows_per_iteration)],
                                      species = sp_biomass_sub$species[seq_len(rows_per_iteration)],
                                      gsa = sp_biomass_sub$gsa[seq_len(rows_per_iteration)],
                                      type = sp_biomass_sub$type[seq_len(rows_per_iteration)],
                                      ssb_min = NA, ssb_mean = NA, ssb_max = NA,
                                      recr_min = NA, recr_mean = NA, recr_max = NA)
        
        for (i in seq_len(nrow(sp_biomass_wide))) {
          year_rows <- which(sp_biomass_sub$year == sp_biomass_wide$year[i])
          ssb_summary <- ensembleSummary(sp_biomass_sub[year_rows, "ssb"])
          recruitment_summary <- ensembleSummary(sp_biomass_sub[year_rows, "recruitment"])
          sp_biomass_wide[i, c("ssb_min", "ssb_mean", "ssb_max")] <- ssb_summary
          sp_biomass_wide[i, c("recr_min", "recr_mean", "recr_max")] <- recruitment_summary
        }
        
        proj_biomass_spec[[j]] <- sp_biomass_wide
        
      }
      
    }
    
    f_applied <<- fishing_schedule
    return(proj_biomass_spec)
  }
  
  plotPred <- function(proj_biomass, netInputs, plotPredCount) {
    start_row <- max(1L, nrow(netInputs) - 10L)
    first_display_year <- netInputs$year[start_row]
    proj_biomass_def <- proj_biomass[
      proj_biomass$year >= first_display_year, , drop = FALSE
    ]
    observed_data <- proj_biomass_def[
      as.character(proj_biomass_def$type) == "Observed", , drop = FALSE
    ]
    forecast_data <- proj_biomass_def[
      as.character(proj_biomass_def$type) == "Forecast", , drop = FALSE
    ]
    if (!nrow(observed_data) || !nrow(forecast_data)) {
      stop("SSB forecast plot requires both observed and forecast years.", call. = FALSE)
    }
    last_observed_year <- max(observed_data$year)
    first_forecast_year <- min(forecast_data$year)
    connection_data <- observed_data[
      observed_data$year == last_observed_year, , drop = FALSE
    ]
    forecast_window <- rbind(connection_data, forecast_data)

    ggplot() +
      annotate(
        "rect", xmin = first_forecast_year - 0.5, xmax = Inf,
        ymin = -Inf, ymax = Inf, fill = "#F59E0B", alpha = 0.055
      ) +
      geom_vline(
        xintercept = first_forecast_year - 0.5,
        linetype = "22", colour = "#9CA3AF", linewidth = 0.65
      ) +
      geom_ribbon(
        data = forecast_window,
        aes(x = year, ymin = ssb_min, ymax = ssb_max, fill = "90% interval"),
        alpha = 0.22, colour = NA
      ) +
      geom_line(
        data = forecast_window,
        aes(x = year, y = ssb_mean, colour = "Ensemble forecast"),
        linewidth = 1.15
      ) +
      geom_point(
        data = forecast_data,
        aes(x = year, y = ssb_mean, colour = "Ensemble forecast"),
        size = 2.35
      ) +
      geom_line(
        data = observed_data,
        aes(x = year, y = ssb_mean, colour = "Observed SSB"),
        linewidth = 1.05
      ) +
      geom_point(
        data = observed_data,
        aes(x = year, y = ssb_mean, colour = "Observed SSB"),
        size = 1.9
      ) +
      scale_colour_manual(
        values = c("Observed SSB" = "#1F2937", "Ensemble forecast" = "#D55E00"),
        breaks = c("Observed SSB", "Ensemble forecast")
      ) +
      scale_fill_manual(values = c("90% interval" = "#E69F00")) +
      scale_x_continuous(
        breaks = annualYearBreaks(proj_biomass_def$year),
        expand = expansion(mult = c(0.015, 0.035))
      ) +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      labs(
        title = paste("SSB forecast —", stockAreaLabel(proj_biomass_def)),
        subtitle = paste0(
          "Observed through ", last_observed_year, "; ensemble forecast ",
          first_forecast_year, "–", max(forecast_data$year), "."
        ),
        x = "Year", y = "Spawning stock biomass (tonnes)",
        caption = paste(
          "Full neural abundance vector with relative-survival correction from",
          "reference F to scenario F; catches and spawning-time SSB use the same",
          "scenario. Ribbon: empirical 5th–95th",
          "percentiles across initializations."
        )
      ) +
      guides(
        colour = guide_legend(order = 1, override.aes = list(linewidth = 1.1)),
        fill = guide_legend(order = 2)
      ) +
      maelstromPlotTheme() +
      ssbYearAxisTheme()
  }

  sensAnalysis <- function(netInputs, pred_results, model_pred) {
    n_models <- length(model_pred)
    if (n_models < 1L) {
      stop("No fitted forecast model is available for sensitivity analysis.", call. = FALSE)
    }

    normalization <- normalizeInputs(netInputs[, -1L, drop = FALSE])
    feature_matrix <- as.matrix(toModelScale(normalization$values))
    lookback <- min(maximum_lookback, nrow(feature_matrix) - 1L)
    reference_input <- buildPredictionWindow(feature_matrix, lookback)
    input_vars <- colnames(feature_matrix)
    population_output_names <- grep(
      "_N_", colnames(feature_matrix), value = TRUE, fixed = TRUE
    )
    output_positions <- match(
      population_output_names, colnames(feature_matrix)
    )
    if (anyNA(output_positions)) {
      stop("Abundance outputs could not be matched for sensitivity analysis.",
           call. = FALSE)
    }
    output_vars <- population_output_names
    perturbations <- seq(0.1, 0.5, by = 0.1)
    lower_bound <- if (identical(input$activation, "tanh")) -0.5 else 0
    upper_bound <- if (identical(input$activation, "tanh")) 0.5 else 1
    sensitivity_rows <- vector("list", n_models * length(input_vars))
    row_index <- 0L

    classifyFeature <- function(feature_name) {
      if (grepl("_N_", feature_name, fixed = TRUE)) return("N")
      if (grepl("_C_", feature_name, fixed = TRUE)) return("C")
      if (startsWith(feature_name, "PrP_")) return("Environment")
      "Feature"
    }
    featureSpecies <- function(feature_name) {
      if (startsWith(feature_name, "PrP_")) return("Environment")
      sub("_.*$", "", feature_name)
    }

    withProgress(message = "Calculating...", value = 0, detail = "0%", {
      for (m in seq_len(n_models)) {
        model <- model_pred[[m]]

        for (i in seq_along(input_vars)) {
          n_conditions <- 2L * length(perturbations)
          perturbation_batch <- reference_input[
            rep(1L, n_conditions), , , drop = FALSE
          ]
          effective_span <- numeric(length(perturbations))

          for (j in seq_along(perturbations)) {
            positive_row <- 2L * j - 1L
            negative_row <- 2L * j
            positive_values <- pmin(
              upper_bound,
              reference_input[1L, , i] + perturbations[j]
            )
            negative_values <- pmax(
              lower_bound,
              reference_input[1L, , i] - perturbations[j]
            )
            perturbation_batch[positive_row, , i] <- positive_values
            perturbation_batch[negative_row, , i] <- negative_values
            effective_span[j] <- mean(positive_values - negative_values)
          }

          perturbed_predictions <- as.matrix(
            predict(model, perturbation_batch, verbose = 0L)
          )
          perturbed_predictions <- perturbed_predictions[
            , output_positions, drop = FALSE
          ]
          response <- vapply(seq_along(perturbations), function(j) {
            positive_row <- 2L * j - 1L
            negative_row <- 2L * j
            100 * abs(
              perturbed_predictions[positive_row, ] -
                perturbed_predictions[negative_row, ]
            ) / max(effective_span[j], sqrt(.Machine$double.eps))
          }, numeric(length(output_vars)))

          row_index <- row_index + 1L
          sensitivity_rows[[row_index]] <- data.frame(
            Perturbance = rep(perturbations, each = length(output_vars)),
            Delta = as.numeric(response),
            Input_Species = featureSpecies(input_vars[i]),
            Input_Var = classifyFeature(input_vars[i]),
            Output_Species = rep(
              vapply(output_vars, featureSpecies, character(1)),
              times = length(perturbations)
            ),
            Output_Var = rep(
              "Abundance vector", length(perturbations) * length(output_vars)
            ),
            stringsAsFactors = FALSE
          )

          completed_steps <- (m - 1L) * length(input_vars) + i
          total_steps <- length(input_vars) * n_models
          incProgress(
            amount = 1 / total_steps,
            detail = paste0(round(100 * completed_steps / total_steps, 2), "%")
          )
        }
      }
    })

    sensitivity_data <- do.call(rbind, sensitivity_rows)
    aggregate(
      Delta ~ Input_Species + Output_Species + Perturbance,
      data = sensitivity_data,
      FUN = mean
    )
  }
  
  plotSensAnalysis <- function(sens_results) {
    p <- ggplot(data = sens_results, aes(x = Input_Species, y = Delta, fill = Perturbance)) +
      geom_col(position = "stack") +
      ggtitle("Neural-network abundance-vector sensitivity") +
      theme_test() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "bottom") +
      facet_wrap(~ Output_Species)
    
    return(p)
  }
  
  loadInput <- function(l) {
    getSaved <- function(name, default) {
      value <- l[[name]]
      if (is.null(value)) default else value
    }
    loaded_schema <- as.character(getSaved("schema_version", "legacy"))
    species <<- l[["species"]]
    gsa <<- l[["gsa"]]
    gsa_tot <<- l[["gsa_tot"]]
    pops <<- l[["pops"]]
    pops_l <<- l[["pops_l"]]
    pops_w <<- l[["pops_w"]]
    output$plotPop <- renderPlot({
      plotPop(pops_l)
      })
    catches <<- l[["catches"]]
    catches_l <<- l[["catches_l"]]
    catches_w <<- l[["catches_w"]]
    output$plotCatch <- renderPlot({
      plotCatch(catches_l)
      })
    waa <<- l[["waa"]]
    waa_l <<- l[["waa_l"]]
    waa_w <<- l[["waa_w"]]
    output$plotWaa <- renderPlot({
      plotWaa(waa_l, pops_l)
      })
    fmorts <<- l[["fmorts"]]
    fmort_l <<- l[["fmort_l"]]
    fmort_w <<- l[["fmort_w"]]
    fmort_spawns <<- getSaved("fmort_spawns", list())
    fmort_spawn_l <<- getSaved("fmort_spawn_l", data.frame())
    fmort_spawn_w <<- getSaved("fmort_spawn_w", data.frame())
    morts <<- l[["morts"]]
    mort_l <<- l[["mort_l"]]
    mort_w <<- l[["mort_w"]]
    mort_spawns <<- getSaved("mort_spawns", list())
    mort_spawn_l <<- getSaved("mort_spawn_l", data.frame())
    mort_spawn_w <<- getSaved("mort_spawn_w", data.frame())
    matures <<- getSaved("matures", list())
    mature_l <<- getSaved("mature_l", data.frame())
    mature_w <<- getSaved("mature_w", data.frame())
    neuralNetInputs <<- l[["neuralNetInputs"]]
    range_inputs <<- getSaved("range_inputs", data.frame())
    range_outputs <<- getSaved("range_outputs", data.frame())
    f_w <<- l[["f_w"]]
    fmort_baseline <<- l[["fmort_baseline"]]
    f_new <<- l[["f_new"]]
    f_adj <<- l[["f_adj"]]
    f_tot <<- l[["f_tot"]]
    f_adj_display <<- l[["f_adj_display"]]
    f_applied <<- getSaved("f_applied", data.frame())
    if (length(f_new)) {
      output$statquoFmort <- renderTable(
        f_new, bordered = TRUE, width = "100%", rownames = FALSE
      )
    }
    if (length(f_adj_display)) {
      output$adjustedFmort <- renderTable(
        f_adj_display, bordered = TRUE, width = "100%", rownames = FALSE
      )
    }
    depth_test <<- l[["depth_test"]]
    plotTestCount <<- 1
    testfit_results <<- l[["testfit_results"]]
    traintest_output_raw <<- l[["traintest_output_raw"]]
    traintest_iter_results <<- getSaved("traintest_iter_results", list())
    traintest_metrics <<- l[["traintest_metrics"]]
    traintest_metrics_plot <<- l[["traintest_metrics_plot"]]
    traintest_results <<- l[["traintest_results"]]
    traintest_plots <<- l[["traintest_plots"]]
    traintest_recr_plots <<-l[["traintest_recr_plots"]]
    output$plotRecruitmentTraintest <- renderPlot({
      traintest_recr_plots[[1]]
    })
    taylor_diagram <<- l[["taylor_diagram"]]
    depth_pred <<- l[["depth_pred"]]
    plotPredCount <<- 1
    model_pred <<- l[["model_pred"]]
    pred_iter_partial <<- l[["pred_iter_partial"]]
    pred_results <<- l[["pred_results"]]
    pred_plots <<- l[["pred_plots"]]
    pred_recr_plots <<- l[["pred_recr_plots"]]
    output$plotPred <- renderPlot({
      pred_plots[[1]]
      })
    output$plotRecruitmentForecast <- renderPlot({
      pred_recr_plots[[1]]
      })
    sens_results <<- l[["sens_results"]]
    sens_plots <<- l[["sens_plots"]]
    output$plotSens <- renderPlot({
      sens_plots
      })
    compatible_cache_schemas <- c(
      "2.3.1-original-structure", "2.3.2-original-structure"
    )
    if (!loaded_schema %in% compatible_cache_schemas) {
      depth_test <<- NULL
      plotTestCount <<- 0
      testfit_results <<- data.frame()
      traintest_output_raw <<- list()
      traintest_iter_results <<- list()
      traintest_metrics <<- data.frame()
      traintest_metrics_plot <<- NULL
      traintest_results <<- data.frame()
      traintest_plots <<- list()
      traintest_recr_plots <<- list()
      taylor_diagram <<- list()
      depth_pred <<- NULL
      plotPredCount <<- 0
      model_pred <<- list()
      pred_output_raw <<- vector("list", length = forecast_iterations)
      pred_iter_partial <<- data.frame()
      pred_results <<- list()
      f_applied <<- data.frame()
      pred_plots <<- list()
      pred_recr_plots <<- list()
      sens_results <<- data.frame()
      sens_plots <<- list()
      output$plotFit <- renderPlot({NULL})
      output$plotTrainTest <- renderPlotly({NULL})
      output$plotMetricsTest <- renderPlot({NULL})
      output$plotRecruitmentTraintest <- renderPlot({NULL})
      output$taylorDiagram <- renderPlot({NULL})
      output$plotPred <- renderPlotly({NULL})
      output$plotRecruitmentForecast <- renderPlot({NULL})
      output$plotSens <- renderPlot({NULL})
      showNotification(
        paste(
          "An earlier workspace version was loaded, but cached test and",
          "forecast results were cleared because F scenario handling changed.",
          "Run Train/Test and Forecast again."
        ),
        type = "warning",
        duration = 12
      )
    }
    }
  
  ##### HELP MODALS #####
  
  output$stk_choose <- reactive({
    if(is.null(input$sobj1)) {
      "Upload a file."
      } else {
        "File uploaded."
        }
    })
  
  generalHelp <- tags$div(id = "modalHelp",
                           modalDialog(
                             HTML(
                             "INSTRUCTIONS:<br><br>
                             DATA LOADING PHASE:<br>
                             - Select and load any number of stock objects<br>
                             - Select the maximum cohort to consider for each species<br>
                             (cohorts over the selected one will be aggregated to it;<br>
                             in the counterfactual projection it is the terminal plus-group)<br>
                             and the first year of the time series from which to begin<br>
                             the analysis<br>
                             - Press 'Load' Button to process data<br>
                             NET BUILDING PHASE:<br>
                             - Set number of layers and other hyperparameters<br>
                             - Set layer type and other parameters for each layer<br>
                             - The network will be used both during test and<br>
                             forecast phases<br><br>
                             TEST PHASE (not mandatory):<br>
                             - Press 'Fit' Button to run a single analysis over<br>
                             the whole dataset to tune the neural network<br>
                             - Set number of years (depth) for the prediction<br>
                             - Press 'Test' Button to train the network over the first<br>
                             chunk of time series and predict selected observed years<br><br>
                             FORECAST PHASE:<br>
                             - Inside 'Fishing Mortality' panel set the baseline<br>
                             and adjust values for every class/age combination<br>
                             - Set number of years (depth) for the prediction<br>
                             - Press 'Predict' Button to start the prediction<br>
                             - Switch results for each species with the arrow buttons<br>
                             - Press 'Sens. Analysis' to run a Sensitivity Analysis<br>
                             (a previous prediction is needed)<br><br>
                             EXPORT AND LOAD PHASE:<br>
                             - You can save your session by selecting a folder<br>
                             then pressing 'Save RData' button<br>
                             - Saved sessions can be chosen and loaded<br>
                             by pressing 'Load RData' button<br>
                             - The 'Report' Button will print a .pdf report of the<br>
                             analyses ran using Maelstrom<br>
                             "),
                             footer = NULL,
                             easyClose = TRUE))
  showModal(generalHelp)
  
  observeEvent(input$generalHelp, {
    showModal(generalHelp)
  })
  
  netHelp <- tags$div(id = "modalHelp",
                          modalDialog(
                            HTML(
                            "INSTRUCTIONS:<br><br>
                            GENERAL PARAMETERS:<br>
                            - N° of Layers: number of layers of the neural network<br>
                            - N° of Epochs: max number of epochs for each iteration<br>
                            - Learning Rate: how fast the model adapts to the data<br><br>
                            LAYER PARAMETERS:<br>
                            - Layer Type: type of layer (Dense, Dropout, LSTM or RNN)<br>
                            - N° of Neurons: number of neurons for the layer<br>
                            - Return Sequence: number of hidden states that the layer<br>
                            will output: if TRUE, all the sequence of hidden states <br>
                            (select if there are other recurrent layers after the current);<br>
                            if FALSE, only the hidden state of the last time step<br>
                            (select if there is a dropout or dense layer after the current)<br>
                            - Dropout: proportion of nodes to be dropped from the layer<br>
                            - Recurrent Dropout: dropout for the recurrent state, keeps<br>
                            informations about the previous states<br>
                            - Activation: function for the activation of input/forget/output gate<br>
                            - Recurrent Activation: function for the activation<br>
                            of cell and hidden states<br>
                             "),
                            footer = NULL,
                            easyClose = TRUE))
  
  observeEvent(input$netHelp, {
    showModal(netHelp)
  })
  
  drop_text <- "Use it if the model overfits. Start from a lower value, then raise it if overfitting persists."
  
  recdrop_text <- "Use it if the model overfits. Preferrable than regular dropout."
  
  act_text <- HTML("ReLU: linear, 0 to infinite<br>Sigmoid: 0 to 1, centered in 0.5<br>Softmax: 0 to 1, skewed towards 1<br>Tanh: -1 to 1, suggested")
  
  recact_text <- "The standard function is Sigmoid."
  
  mortalityHelp <- tags$div(id = "mortalityHelp",
                          modalDialog(
                            HTML(
                            "INSTRUCTIONS:<br><br>
                            - Through the baseline drop-down menù select a number of years<br>
                            of the time series (starting from the end and going backwards)<br>
                            to consider as a baseline for the average; press 'Calc' to<br>
                            perform the moving average.<br><br>
                            - The 'Status Quo' table shows the baseline values of<br>
                            Fishing Mortality after the calculation of the moving average.<br><br>
                            - The 'New Exploitation Pattern' table shows the values of<br>
                            Fishing Mortality after the tuning. To tune the baseline Fishing<br>
                            Mortality, the user must first select a species or cohort using<br>
                            the drop-down menù on the bottom left and then adjust it through<br>
                            the slider on the right. The adjusted value will then appear<br>
                            on the relative column in the table.<br><br>
                            - Download the adjusted F vector as .rds, or import a named<br>
                            vector or annual F matrix from .rds, .rda, .RData or .csv.<br>
                            Columns must exactly match the Status Quo F columns. One<br>
                            row applies to every future year; multiple rows start in the<br>
                            first forecast year. An optional 'year' column must contain<br>
                            consecutive forecast years. Extra rows are ignored; when<br>
                            rows run out, the last F row is reused. Moving the slider<br>
                            after importing an annual matrix returns to one-row mode.<br>"),
                            footer = NULL,
                            easyClose = TRUE))
  
  observeEvent(input$mortalityHelp, {
    showModal(mortalityHelp)
  })
  
  ##### REACTIVE VALUES #####
  
  # Stock 1
  
  rv1 <- reactiveValues(
    stk = NULL, tri = NULL, gsa = NULL,
    minYear = 0, maxYear = 0, minAge = 0, maxAge = 0,
    baselineYear = 0, baselineAge = 0,
    spinfo = "No Species Selected", gsainfo = "No GSA Selected"
  )
  
  output$triHelp1 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv1$spinfo
      )
    )
  })
  
  output$gsaHelp1 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv1$gsainfo
      )
    )
  })
  
  output$sobj1 <- renderUI({
    input$reset1
    fileInput(inputId = "sobj1",
              label = NULL,
              placeholder = "Stock Object #1",
              accept = c(".rds", ".RData", ".rda")
    )
  })
  
  observe({
    req(input$sobj1)
    stk1 <- loadStockFile(input$sobj1$datapath[[1L]], input$sobj1$name[[1L]])
    rv1$stk <- stk1
    rv1$minYear <- as.integer(stk1@range[4])
    rv1$maxYear <- as.integer(stk1@range[5])
    rv1$minAge <- as.integer(stk1@range[1])
    rv1$maxAge <- as.integer(stk1@range[2])
    updatePickerInput(
      session,
      "baselineYear1",
      choices = rv1$minYear:rv1$maxYear,
      selected = rv1$minYear
    )
    updatePickerInput(
      session,
      "baselineAge1",
      choices = rv1$minAge:rv1$maxAge,
      selected = rv1$maxAge
    )
    rv1$tri <- sub("_.*", "", input$sobj1$name[[1L]])
    rv1$spinfo <- speciesInfo(rv1$tri)
    rv1$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj1$name[[1L]]), "-")[[1]]))
    rv1$gsainfo <- gsaInfo(rv1$gsa)
  })
  
  observeEvent(input$baselineYear1, {
    rv1$baselineYear <- as.integer(input$baselineYear1)
  })
  
  observeEvent(input$baselineAge1, {
    rv1$baselineAge <- as.integer(input$baselineAge1)
  })
  
  observeEvent(input$reset1, {
    rv1$stk <- NULL
    rv1$obj <- NULL
    rv1$tri <- NULL
    rv1$gsa <- NULL
    rv1$minYear <- 0
    rv1$maxYear <- 0
    rv1$minAge <- 0
    rv1$maxAge <- 0
    rv1$baselineYear <- 0
    rv1$baselineAge <- 0
    rv1$spinfo <- "No Species Selected"
    rv1$gsainfo <- "No GSA Selected"
  })
  
  # Stock 2
  
  rv2 <- reactiveValues(
    stk = NULL, tri = NULL, gsa = NULL,
    minYear = 0, maxYear = 0, minAge = 0, maxAge = 0,
    baselineYear = 0, baselineAge = 0,
    spinfo = "No Species Selected", gsainfo = "No GSA Selected"
  )
  
  output$triHelp2 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv2$spinfo
      )
    )
  })
  
  output$gsaHelp2 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv2$gsainfo
      )
    )
  })
  
  output$sobj2 <- renderUI({
    input$reset2
    conditionalPanel(
      condition = "input.nstocks >= 2",
    fileInput(inputId = "sobj2",
              label = NULL,
              placeholder = "Stock Object #2",
              accept = c(".rds", ".RData", ".rda")
                )
    )
  })
  
  observe({
    req(input$sobj2)
    stk2 <- loadStockFile(input$sobj2$datapath[[1L]], input$sobj2$name[[1L]])
    rv2$stk <- stk2
    rv2$minYear <- as.integer(stk2@range[4])
    rv2$maxYear <- as.integer(stk2@range[5])
    rv2$minAge <- as.integer(stk2@range[1])
    rv2$maxAge <- as.integer(stk2@range[2])
    updatePickerInput(
      session,
      "baselineYear2",
      choices = rv2$minYear:rv2$maxYear,
      selected = rv2$minYear
    )
    updatePickerInput(
      session,
      "baselineAge2",
      choices = rv2$minAge:rv2$maxAge,
      selected = rv2$maxAge
    )
    rv2$tri <- sub("_.*", "", input$sobj2$name[[1L]])
    rv2$spinfo <- speciesInfo(rv2$tri)
    rv2$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj2$name[[1L]]), "-")[[1]]))
    rv2$gsainfo <- gsaInfo(rv2$gsa)
  })
  
  observeEvent(input$baselineYear2, {
    rv2$baselineYear <- as.integer(input$baselineYear2)
  })
  
  observeEvent(input$baselineAge2, {
    rv2$baselineAge <- as.integer(input$baselineAge2)
  })
  
  observeEvent(input$reset2, {
    rv2$stk <- NULL
    rv2$obj <- NULL
    rv2$tri <- NULL
    rv2$gsa <- NULL
    rv2$minYear <- 0
    rv2$maxYear <- 0
    rv2$minAge <- 0
    rv2$maxAge <- 0
    rv2$baselineYear <- 0
    rv2$baselineAge <- 0
    rv2$spinfo <- "No Species Selected"
    rv2$gsainfo <- "No GSA Selected"
  })
  
  # Stock 3
  
  rv3 <- reactiveValues(
    stk = NULL, tri = NULL, gsa = NULL,
    minYear = 0, maxYear = 0, minAge = 0, maxAge = 0,
    baselineYear = 0, baselineAge = 0,
    spinfo = "No Species Selected", gsainfo = "No GSA Selected"
  )
  
  output$triHelp3 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv3$spinfo
      )
    )
  })
  
  output$gsaHelp3 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv3$gsainfo
      )
    )
  })
  
  output$sobj3 <- renderUI({
    input$reset3
    conditionalPanel(
      condition = "input.nstocks >= 3",
    fileInput(inputId = "sobj3",
              label = NULL,
              placeholder = "Stock Object #3",
              accept = c(".rds", ".RData", ".rda")
                )
      )
  })
  
  observe({
    req(input$sobj3)
    stk3 <- loadStockFile(input$sobj3$datapath[[1L]], input$sobj3$name[[1L]])
    rv3$stk <- stk3
    rv3$minYear <- as.integer(stk3@range[4])
    rv3$maxYear <- as.integer(stk3@range[5])
    rv3$minAge <- as.integer(stk3@range[1])
    rv3$maxAge <- as.integer(stk3@range[2])
    updatePickerInput(
      session,
      "baselineYear3",
      choices = rv3$minYear:rv3$maxYear,
      selected = rv3$minYear
    )
    updatePickerInput(
      session,
      "baselineAge3",
      choices = rv3$minAge:rv3$maxAge,
      selected = rv3$maxAge
    )
    rv3$tri <- sub("_.*", "", input$sobj3$name[[1L]])
    rv3$spinfo <- speciesInfo(rv3$tri)
    rv3$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj3$name[[1L]]), "-")[[1]]))
    rv3$gsainfo <- gsaInfo(rv3$gsa)
  })
  
  observeEvent(input$baselineYear3, {
    rv3$baselineYear <- as.integer(input$baselineYear3)
  })
  
  observeEvent(input$baselineAge3, {
    rv3$baselineAge <- as.integer(input$baselineAge3)
  })
  
  observeEvent(input$reset3, {
    rv3$stk <- NULL
    rv3$obj <- NULL
    rv3$tri <- NULL
    rv3$gsa <- NULL
    rv3$minYear <- 0
    rv3$maxYear <- 0
    rv3$minAge <- 0
    rv3$maxAge <- 0
    rv3$baselineYear <- 0
    rv3$baselineAge <- 0
    rv3$spinfo <- "No Species Selected"
    rv3$gsainfo <- "No GSA Selected"
  })
  
  # Stock 4
  
  rv4 <- reactiveValues(
    stk = NULL, tri = NULL, gsa = NULL,
    minYear = 0, maxYear = 0, minAge = 0, maxAge = 0,
    baselineYear = 0, baselineAge = 0,
    spinfo = "No Species Selected", gsainfo = "No GSA Selected"
  )
  
  output$triHelp4 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv4$spinfo
      )
    )
  })
  
  output$gsaHelp4 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv4$gsainfo
      )
    )
  })
  
  output$sobj4 <- renderUI({
    input$reset4
    conditionalPanel(
      condition = "input.nstocks >= 4",
    fileInput(inputId = "sobj4",
              label = NULL,
              placeholder = "Stock Object #4",
              accept = c(".rds", ".RData", ".rda")
      )
    )
  })
  
  observe({
    req(input$sobj4)
    stk4 <- loadStockFile(input$sobj4$datapath[[1L]], input$sobj4$name[[1L]])
    rv4$stk <- stk4
    rv4$minYear <- as.integer(stk4@range[4])
    rv4$maxYear <- as.integer(stk4@range[5])
    rv4$minAge <- as.integer(stk4@range[1])
    rv4$maxAge <- as.integer(stk4@range[2])
    updatePickerInput(
      session,
      "baselineYear4",
      choices = rv4$minYear:rv4$maxYear,
      selected = rv4$minYear
    )
    updatePickerInput(
      session,
      "baselineAge4",
      choices = rv4$minAge:rv4$maxAge,
      selected = rv4$maxAge
    )
    rv4$tri <- sub("_.*", "", input$sobj4$name[[1L]])
    rv4$spinfo <- speciesInfo(rv4$tri)
    rv4$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj4$name[[1L]]), "-")[[1]]))
    rv4$gsainfo <- gsaInfo(rv4$gsa)
  })
  
  observeEvent(input$baselineYear4, {
    rv4$baselineYear <- as.integer(input$baselineYear4)
  })
  
  observeEvent(input$baselineAge4, {
    rv4$baselineAge <- as.integer(input$baselineAge4)
  })
  
  observeEvent(input$reset4, {
    rv4$stk <- NULL
    rv4$obj <- NULL
    rv4$tri <- NULL
    rv4$gsa <- NULL
    rv4$minYear <- 0
    rv4$maxYear <- 0
    rv4$minAge <- 0
    rv4$maxAge <- 0
    rv4$baselineYear <- 0
    rv4$baselineAge <- 0
    rv4$spinfo <- "No Species Selected"
    rv4$gsainfo <- "No GSA Selected"
  })
  
  # Stock 5
  
  rv5 <- reactiveValues(
    stk = NULL, tri = NULL, gsa = NULL,
    minYear = 0, maxYear = 0, minAge = 0, maxAge = 0,
    baselineYear = 0, baselineAge = 0,
    spinfo = "No Species Selected", gsainfo = "No GSA Selected"
  )
  
  output$triHelp5 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv5$spinfo
      )
    )
  })
  
  output$gsaHelp5 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv5$gsainfo
      )
    )
  })
  
  output$sobj5 <- renderUI({
    input$reset5
    conditionalPanel(
      condition = "input.nstocks >= 5",
    fileInput(inputId = "sobj5",
              label = NULL,
              placeholder = "Stock Object #5",
              accept = c(".rds", ".RData", ".rda")
      )
    )
  })
  
  observe({
    req(input$sobj5)
    stk5 <- loadStockFile(input$sobj5$datapath[[1L]], input$sobj5$name[[1L]])
    rv5$stk <- stk5
    rv5$minYear <- as.integer(stk5@range[4])
    rv5$maxYear <- as.integer(stk5@range[5])
    rv5$minAge <- as.integer(stk5@range[1])
    rv5$maxAge <- as.integer(stk5@range[2])
    updatePickerInput(
      session,
      "baselineYear5",
      choices = rv5$minYear:rv5$maxYear,
      selected = rv5$minYear
    )
    updatePickerInput(
      session,
      "baselineAge5",
      choices = rv5$minAge:rv5$maxAge,
      selected = rv5$maxAge
    )
    rv5$tri <- sub("_.*", "", input$sobj5$name[[1L]])
    rv5$spinfo <- speciesInfo(rv5$tri)
    rv5$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj5$name[[1L]]), "-")[[1]]))
    rv5$gsainfo <- gsaInfo(rv5$gsa)
  })
  
  observeEvent(input$baselineYear5, {
    rv5$baselineYear <- as.integer(input$baselineYear5)
  })
  
  observeEvent(input$baselineAge5, {
    rv5$baselineAge <- as.integer(input$baselineAge5)
  })
  
  observeEvent(input$reset5, {
    rv5$stk <- NULL
    rv5$obj <- NULL
    rv5$tri <- NULL
    rv5$gsa <- NULL
    rv5$minYear <- 0
    rv5$maxYear <- 0
    rv5$minAge <- 0
    rv5$maxAge <- 0
    rv5$baselineYear <- 0
    rv5$baselineAge <- 0
    rv5$spinfo <- "No Species Selected"
    rv5$gsainfo <- "No GSA Selected"
  })
  
  # Stock 6
  
  rv6 <- reactiveValues(
    stk = NULL, tri = NULL, gsa = NULL,
    minYear = 0, maxYear = 0, minAge = 0, maxAge = 0,
    baselineYear = 0, baselineAge = 0,
    spinfo = "No Species Selected", gsainfo = "No GSA Selected"
  )
  
  output$triHelp6 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv6$spinfo
      )
    )
  })
  
  output$gsaHelp6 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv6$gsainfo
      )
    )
  })
  
  output$sobj6 <- renderUI({
    input$reset6
    conditionalPanel(
      condition = "input.nstocks >= 6",
    fileInput(inputId = "sobj6",
              label = NULL,
              placeholder = "Stock Object #6",
              accept = c(".rds", ".RData", ".rda")
      )
    )
  })
  
  observe({
    req(input$sobj6)
    stk6 <- loadStockFile(input$sobj6$datapath[[1L]], input$sobj6$name[[1L]])
    rv6$stk <- stk6
    rv6$minYear <- as.integer(stk6@range[4])
    rv6$maxYear <- as.integer(stk6@range[5])
    rv6$minAge <- as.integer(stk6@range[1])
    rv6$maxAge <- as.integer(stk6@range[2])
    updatePickerInput(
      session,
      "baselineYear6",
      choices = rv6$minYear:rv6$maxYear,
      selected = rv6$minYear
    )
    updatePickerInput(
      session,
      "baselineAge6",
      choices = rv6$minAge:rv6$maxAge,
      selected = rv6$maxAge
    )
    rv6$tri <- sub("_.*", "", input$sobj6$name[[1L]])
    rv6$spinfo <- speciesInfo(rv6$tri)
    rv6$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj6$name[[1L]]), "-")[[1]]))
    rv6$gsainfo <- gsaInfo(rv6$gsa)
  })
  
  observeEvent(input$baselineYear6, {
    rv6$baselineYear <- as.integer(input$baselineYear6)
  })
  
  observeEvent(input$baselineAge6, {
    rv6$baselineAge <- as.integer(input$baselineAge6)
  })
  
  observeEvent(input$reset6, {
    rv6$stk <- NULL
    rv6$obj <- NULL
    rv6$tri <- NULL
    rv6$gsa <- NULL
    rv6$minYear <- 0
    rv6$maxYear <- 0
    rv6$minAge <- 0
    rv6$maxAge <- 0
    rv6$baselineYear <- 0
    rv6$baselineAge <- 0
    rv6$spinfo <- "No Species Selected"
    rv6$gsainfo <- "No GSA Selected"
  })
  
  # Stock 7
  
  rv7 <- reactiveValues(
    stk = NULL, tri = NULL, gsa = NULL,
    minYear = 0, maxYear = 0, minAge = 0, maxAge = 0,
    baselineYear = 0, baselineAge = 0,
    spinfo = "No Species Selected", gsainfo = "No GSA Selected"
  )
  
  output$triHelp7 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv7$spinfo
      )
    )
  })
  
  output$gsaHelp7 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv7$gsainfo
      )
    )
  })
  
  output$sobj7 <- renderUI({
    input$reset7
    conditionalPanel(
      condition = "input.nstocks >= 7",
    fileInput(inputId = "sobj7",
              label = NULL,
              placeholder = "Stock Object #7",
              accept = c(".rds", ".RData", ".rda")
      )
    )
  })
  
  observe({
    req(input$sobj7)
    stk7 <- loadStockFile(input$sobj7$datapath[[1L]], input$sobj7$name[[1L]])
    rv7$stk <- stk7
    rv7$minYear <- as.integer(stk7@range[4])
    rv7$maxYear <- as.integer(stk7@range[5])
    rv7$minAge <- as.integer(stk7@range[1])
    rv7$maxAge <- as.integer(stk7@range[2])
    updatePickerInput(
      session,
      "baselineYear7",
      choices = rv7$minYear:rv7$maxYear,
      selected = rv7$minYear
    )
    updatePickerInput(
      session,
      "baselineAge7",
      choices = rv7$minAge:rv7$maxAge,
      selected = rv7$maxAge
    )
    rv7$tri <- sub("_.*", "", input$sobj7$name[[1L]])
    rv7$spinfo <- speciesInfo(rv7$tri)
    rv7$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj7$name[[1L]]), "-")[[1]]))
    rv7$gsainfo <- gsaInfo(rv7$gsa)
  })
  
  observeEvent(input$baselineYear7, {
    rv7$baselineYear <- as.integer(input$baselineYear7)
  })
  
  observeEvent(input$baselineAge7, {
    rv7$baselineAge <- as.integer(input$baselineAge7)
  })
  
  observeEvent(input$reset7, {
    rv7$stk <- NULL
    rv7$obj <- NULL
    rv7$tri <- NULL
    rv7$gsa <- NULL
    rv7$minYear <- 0
    rv7$maxYear <- 0
    rv7$minAge <- 0
    rv7$maxAge <- 0
    rv7$baselineYear <- 0
    rv7$baselineAge <- 0
    rv7$spinfo <- "No Species Selected"
    rv7$gsainfo <- "No GSA Selected"
  })
  
  # Stock 8
  
  rv8 <- reactiveValues(
    stk = NULL, tri = NULL, gsa = NULL,
    minYear = 0, maxYear = 0, minAge = 0, maxAge = 0,
    baselineYear = 0, baselineAge = 0,
    spinfo = "No Species Selected", gsainfo = "No GSA Selected"
  )
  
  output$triHelp8 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv8$spinfo
      )
    )
  })
  
  output$gsaHelp8 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv8$gsainfo
      )
    )
  })
  
  output$sobj8 <- renderUI({
    input$reset8
    conditionalPanel(
      condition = "input.nstocks >= 8",
    fileInput(inputId = "sobj8",
              label = NULL,
              placeholder = "Stock Object #8",
              accept = c(".rds", ".RData", ".rda")
      )
    )
  })
  
  observe({
    req(input$sobj8)
    stk8 <- loadStockFile(input$sobj8$datapath[[1L]], input$sobj8$name[[1L]])
    rv8$stk <- stk8
    rv8$minYear <- as.integer(stk8@range[4])
    rv8$maxYear <- as.integer(stk8@range[5])
    rv8$minAge <- as.integer(stk8@range[1])
    rv8$maxAge <- as.integer(stk8@range[2])
    updatePickerInput(
      session,
      "baselineYear8",
      choices = rv8$minYear:rv8$maxYear,
      selected = rv8$minYear
    )
    updatePickerInput(
      session,
      "baselineAge8",
      choices = rv8$minAge:rv8$maxAge,
      selected = rv8$maxAge
    )
    rv8$tri <- sub("_.*", "", input$sobj8$name[[1L]])
    rv8$spinfo <- speciesInfo(rv8$tri)
    rv8$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj8$name[[1L]]), "-")[[1]]))
    rv8$gsainfo <- gsaInfo(rv8$gsa)
  })
  
  observeEvent(input$baselineYear8, {
    rv8$baselineYear <- as.integer(input$baselineYear8)
  })
  
  observeEvent(input$baselineAge8, {
    rv8$baselineAge <- as.integer(input$baselineAge8)
  })
  
  observeEvent(input$reset8, {
    rv8$stk <- NULL
    rv8$obj <- NULL
    rv8$tri <- NULL
    rv8$gsa <- NULL
    rv8$minYear <- 0
    rv8$maxYear <- 0
    rv8$minAge <- 0
    rv8$maxAge <- 0
    rv8$baselineYear <- 0
    rv8$baselineAge <- 0
    rv8$spinfo <- "No Species Selected"
    rv8$gsainfo <- "No GSA Selected"
  })
  
  # Stock 9
  
  rv9 <- reactiveValues(
    stk = NULL, tri = NULL, gsa = NULL,
    minYear = 0, maxYear = 0, minAge = 0, maxAge = 0,
    baselineYear = 0, baselineAge = 0,
    spinfo = "No Species Selected", gsainfo = "No GSA Selected"
  )
  
  output$triHelp9 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv9$spinfo
      )
    )
  })
  
  output$gsaHelp9 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv9$gsainfo
      )
    )
  })
  
  output$sobj9 <- renderUI({
    input$reset9
    conditionalPanel(
      condition = "input.nstocks >= 9",
    fileInput(inputId = "sobj9",
              label = NULL,
              placeholder = "Stock Object #9",
              accept = c(".rds", ".RData", ".rda")
      )
    )
  })
  
  observe({
    req(input$sobj9)
    stk9 <- loadStockFile(input$sobj9$datapath[[1L]], input$sobj9$name[[1L]])
    rv9$stk <- stk9
    rv9$minYear <- as.integer(stk9@range[4])
    rv9$maxYear <- as.integer(stk9@range[5])
    rv9$minAge <- as.integer(stk9@range[1])
    rv9$maxAge <- as.integer(stk9@range[2])
    updatePickerInput(
      session,
      "baselineYear9",
      choices = rv9$minYear:rv9$maxYear,
      selected = rv9$minYear
    )
    updatePickerInput(
      session,
      "baselineAge9",
      choices = rv9$minAge:rv9$maxAge,
      selected = rv9$maxAge
    )
    rv9$tri <- sub("_.*", "", input$sobj9$name[[1L]])
    rv9$spinfo <- speciesInfo(rv9$tri)
    rv9$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj9$name[[1L]]), "-")[[1]]))
    rv9$gsainfo <- gsaInfo(rv9$gsa)
  })
  
  observeEvent(input$baselineYear9, {
    rv9$baselineYear <- as.integer(input$baselineYear9)
  })
  
  observeEvent(input$baselineAge9, {
    rv9$baselineAge <- as.integer(input$baselineAge9)
  })
  
  observeEvent(input$reset9, {
    rv9$stk <- NULL
    rv9$obj <- NULL
    rv9$tri <- NULL
    rv9$gsa <- NULL
    rv9$minYear <- 0
    rv9$maxYear <- 0
    rv9$minAge <- 0
    rv9$maxAge <- 0
    rv9$baselineYear <- 0
    rv9$baselineAge <- 0
    rv9$spinfo <- "No Species Selected"
    rv9$gsainfo <- "No GSA Selected"
  })
  
  # Stock 10
  
  rv10 <- reactiveValues(
    stk = NULL, tri = NULL, gsa = NULL,
    minYear = 0, maxYear = 0, minAge = 0, maxAge = 0,
    baselineYear = 0, baselineAge = 0,
    spinfo = "No Species Selected", gsainfo = "No GSA Selected"
  )
  
  output$triHelp10 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv10$spinfo
      )
    )
  })
  
  output$gsaHelp10 <- renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = rv10$gsainfo
      )
    )
  })
  
  output$sobj10 <- renderUI({
    input$reset10
    conditionalPanel(
      condition = "input.nstocks >= 10",
    fileInput(inputId = "sobj10",
              label = NULL,
              placeholder = "Stock Object #10",
              accept = c(".rds", ".RData", ".rda")
      )
    )
  })
  
  observe({
    req(input$sobj10)
    stk10 <- loadStockFile(input$sobj10$datapath[[1L]], input$sobj10$name[[1L]])
    rv10$stk <- stk10
    rv10$minYear <- as.integer(stk10@range[4])
    rv10$maxYear <- as.integer(stk10@range[5])
    rv10$minAge <- as.integer(stk10@range[1])
    rv10$maxAge <- as.integer(stk10@range[2])
    updatePickerInput(
      session,
      "baselineYear10",
      choices = rv10$minYear:rv10$maxYear,
      selected = rv10$minYear
    )
    updatePickerInput(
      session,
      "baselineAge10",
      choices = rv10$minAge:rv10$maxAge,
      selected = rv10$maxAge
    )
    rv10$tri <- sub("_.*", "", input$sobj10$name[[1L]])
    rv10$spinfo <- speciesInfo(rv10$tri)
    rv10$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj10$name[[1L]]), "-")[[1]]))
    rv10$gsainfo <- gsaInfo(rv10$gsa)
  })
  
  observeEvent(input$baselineYear10, {
    rv10$baselineYear <- as.integer(input$baselineYear10)
  })
  
  observeEvent(input$baselineAge10, {
    rv10$baselineAge <- as.integer(input$baselineAge10)
  })
  
  observeEvent(input$reset10, {
    rv10$stk <- NULL
    rv10$obj <- NULL
    rv10$tri <- NULL
    rv10$gsa <- NULL
    rv10$minYear <- 0
    rv10$maxYear <- 0
    rv10$minAge <- 0
    rv10$maxAge <- 0
    rv10$baselineYear <- 0
    rv10$baselineAge <- 0
    rv10$spinfo <- "No Species Selected"
    rv10$gsainfo <- "No GSA Selected"
  })
  
  # Load GSAs and 3A codes
  
  observeEvent(input$loadButton, {

    # A second click starts a clean analysis in the current session instead of
    # appending to objects produced by the previous set of uploaded stocks.
    species <<- list()
    gsa <<- list()
    gsa_tot <<- vector()
    rv <<- list()
    pops <<- list()
    catches <<- list()
    waa <<- list()
    fmorts <<- list()
    fmort_spawns <<- list()
    morts <<- list()
    mort_spawns <<- list()
    matures <<- list()
    neuralNetInputs <<- data.frame()
    f_w <<- data.frame()
    f_new <<- data.frame()
    f_adj <<- data.frame()
    f_tot <<- data.frame()
    f_adj_display <<- data.frame()
    f_applied <<- data.frame()
    testfit_results <<- data.frame()
    traintest_results <<- data.frame()
    pred_results <<- list()
    sens_results <<- data.frame()
    
    # Compact the populated upload slots so that leaving a gap (for example,
    # using slots 1 and 3) cannot create NULL entries in downstream loops.
    upload_slots <- list(rv1, rv2, rv3, rv4, rv5, rv6, rv7, rv8, rv9, rv10)
    loaded_slots <- Filter(function(x) !is.null(x$stk), upload_slots)

    if (!length(loaded_slots)) {
      showNotification("Load at least one FLStock object.", type = "error")
      return(invisible(NULL))
    }
    if (!is.null(input$nstocks) && length(loaded_slots) != as.integer(input$nstocks)) {
      showNotification(
        sprintf("Expected %s stock objects but %s valid objects are loaded.",
                input$nstocks, length(loaded_slots)),
        type = "error",
        duration = 8
      )
      return(invisible(NULL))
    }

    rv <<- loaded_slots
    species <<- lapply(rv, function(x) toupper(as.character(x$tri)))
    gsa <<- lapply(rv, function(x) as.integer(x$gsa))

    species_codes <- unlist(species, use.names = FALSE)
    gsa_codes <- unlist(gsa, use.names = FALSE)
    if (anyNA(species_codes) || any(!grepl("^[A-Z0-9]{3}$", species_codes))) {
      showNotification(
        "Invalid species code in a filename. Use names such as DPS_9-10-11.rds.",
        type = "error",
        duration = 8
      )
      return(invisible(NULL))
    }
    if (anyNA(gsa_codes) || any(gsa_codes < 1L | gsa_codes > 30L)) {
      showNotification(
        "Invalid GSA code in a filename. Expected integer GSA values from 1 to 30.",
        type = "error",
        duration = 8
      )
      return(invisible(NULL))
    }
    if (anyDuplicated(species_codes)) {
      showNotification(
        paste(
          "Each uploaded FLStock must have a unique species code.",
          "Combine multiple GSAs for one species in a single FLStock/file."
        ),
        type = "error",
        duration = 10
      )
      return(invisible(NULL))
    }

    gsa_tot <<- sort(unique(gsa_codes))
    
  })
  
  # Neural network help icons
  
  for (layer_index in seq_len(5L)) {
    local({
      index <- layer_index
      output[[paste0("dropHelp", index)]] <- renderUI({
        tags$span(tipify(icon("fas fa-info-circle"), title = drop_text))
      })
      output[[paste0("recdropHelp", index)]] <- renderUI({
        tags$span(tipify(icon("fas fa-info-circle"), title = recdrop_text))
      })
    })
  }
  
  output$actHelp = renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = act_text
      )
    )
  })
  
  output$recactHelp = renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = recact_text
      )
    )
  })
  
  ##### DATAFRAME LOADING #####
  
  observeEvent(input$loadButton, {

    if (!length(rv) || (!is.null(input$nstocks) && length(rv) != as.integer(input$nstocks))) {
      return(invisible(NULL))
    }
    
    # Population

    for (i in seq_along(rv)) {
      pops[[i]] <<- procDfLongQuant(rv[[i]]$stk, rv[[i]]$gsa, rv[[i]]$tri, rv[[i]]$minAge, rv[[i]]$baselineAge, rv[[i]]$baselineYear, stock.n, pop)
    }
    
    if (length(pops) > 0) {
      pops_l <<- do.call(totDf, pops)
      pops_w <<- procDfWide(pops_l, pop, N)
      output$uiPop <- renderUI({
        div(
          class = "data-structure-plot",
          withSpinner(
            plotlyOutput(
              "plotPop", height = paste0(dataStructurePlotHeight(pops_l), "px")
            ),
            type = 3, color.background = "transparent"
          )
        )
      })
      output$plotPop <- renderPlotly({
        plotPopObj <- layout(ggplotly(plotPop(pops_l), tooltip = "y"), hovermode = "x unified")
        return(plotPopObj)
      })
    }
    
    # Catches
    
    for (i in seq_along(rv)) {
      catches[[i]] <<- procDfLongQuant(rv[[i]]$stk, rv[[i]]$gsa, rv[[i]]$tri, rv[[i]]$minAge, rv[[i]]$baselineAge, rv[[i]]$baselineYear, catch.n, catch)
    }
    
    if (length(catches) > 0) {
      catches_l <<- do.call(totDf, catches)
      catches_w <<- procDfWide(catches_l, catch, C)
      output$uiCatch <- renderUI({
        div(
          class = "data-structure-plot",
          withSpinner(
            plotlyOutput(
              "plotCatch", height = paste0(dataStructurePlotHeight(catches_l), "px")
            ),
            type = 3, color.background = "transparent"
          )
        )
      })
      output$plotCatch <- renderPlotly({
        plotCatchObj <- layout(ggplotly(plotCatch(catches_l), tooltip = "y"), hovermode = "x unified")
        return(plotCatchObj)
      })
    }
    
    # Weight at age
    
    for (i in seq_along(rv)) {
      waa[[i]] <<- procDfLongMult(rv[[i]]$stk, rv[[i]]$gsa, rv[[i]]$tri, rv[[i]]$minAge, rv[[i]]$baselineAge, rv[[i]]$baselineYear, stock.wt, weight_at_age)
    }
    
    if (length(waa) > 0) {
      waa_l <<- do.call(totDf, waa)
      waa_w <<- procDfWide(waa_l, weight_at_age, W)
      output$uiWaa <- renderUI({
        div(
          class = "data-structure-plot",
          withSpinner(
            plotlyOutput(
              "plotWaa", height = paste0(dataStructurePlotHeight(waa_l), "px")
            ),
            type = 3, color.background = "transparent"
          )
        )
      })
      output$plotWaa <- renderPlotly({
        plotWaaObj <- layout(ggplotly(plotWaa(waa_l, pops_l), tooltip = "y"), hovermode = "x unified")
        return(plotWaaObj)
      })
    }
    
    # Fishing mortality
    
    for (i in seq_along(rv)) {
      fmorts[[i]] <<- procDfLongMult(rv[[i]]$stk, rv[[i]]$gsa, rv[[i]]$tri, rv[[i]]$minAge, rv[[i]]$baselineAge, rv[[i]]$baselineYear, harvest, fmort)
    }
    
    if (length(fmorts) > 0) {
      fmort_l <<- do.call(totDf, fmorts)
      fmort_w <<- procDfWide(fmort_l, fmort, F)
    }
    
    # Fishing mortality of spawners
    
    for (i in seq_along(rv)) {
      fmort_spawns[[i]] <<- procDfLongMult(rv[[i]]$stk, rv[[i]]$gsa, rv[[i]]$tri, rv[[i]]$minAge, rv[[i]]$baselineAge, rv[[i]]$baselineYear, harvest.spwn, fmort_spawn)
    }
    
    if (length(fmort_spawns) > 0) {
      fmort_spawn_l <<- do.call(totDf, fmort_spawns)
      fmort_spawn_w <<- procDfWide(fmort_spawn_l, fmort_spawn, J)
    }
    
    # Natural mortality
    
    for (i in seq_along(rv)) {
      morts[[i]] <<- procDfLongMult(rv[[i]]$stk, rv[[i]]$gsa, rv[[i]]$tri, rv[[i]]$minAge, rv[[i]]$baselineAge, rv[[i]]$baselineYear, m, mort)
    }
    
    if (length(morts) > 0) {
      mort_l <<- do.call(totDf, morts)
      mort_w <<- procDfWide(mort_l, mort, M)
    }
    
    # Natural mortality of spawners
    
    for (i in seq_along(rv)) {
      mort_spawns[[i]] <<- procDfLongMult(rv[[i]]$stk, rv[[i]]$gsa, rv[[i]]$tri, rv[[i]]$minAge, rv[[i]]$baselineAge, rv[[i]]$baselineYear, m.spwn, mort_spawn)
    }
    
    if (length(mort_spawns) > 0) {
      mort_spawn_l <<- do.call(totDf, mort_spawns)
      mort_spawn_w <<- procDfWide(mort_spawn_l, mort_spawn, K)
    }
    
    # Mature ratio
    
    for (i in seq_along(rv)) {
      matures[[i]] <<- procDfLongMult(rv[[i]]$stk, rv[[i]]$gsa, rv[[i]]$tri, rv[[i]]$minAge, rv[[i]]$baselineAge, rv[[i]]$baselineYear, mat, mature)
    }
    
    if (length(matures) > 0) {
      mature_l <<- do.call(totDf, matures)
      mature_w <<- procDfWide(mature_l, mature, L)
    }
    
    # Create input dataframe for neural network
    
    if (length(pops_l) > 0) {neuralNetInputs <<- procInputs(pops_w, catches_w)}
    
  })
  
  # Zoom and download plots
  
  observeEvent(input$zoomPopButton, {
    if (length(pops_l) > 0) {
      output$zoomPop <- renderPlot({
        plotPopObj <- plotPop(pops_l)
        return(plotPopObj)
        })
      } else {
        showModal(tags$div(id = "modalWarning",
                           modalDialog("Warning: load one or more stock objects first!",
                                       footer = NULL,
                                       easyClose = TRUE)))
        }
    })
  
  output$downloadPop <- downloadHandler(
    filename = "Population Structure Plot.png",
    content = function(file) {
      ggsave(file,
             device = png,
             width = 16,
             height = 9,
             units = "in",
             dpi = 300)
    }
  )
  
  observeEvent(input$zoomCatchButton, {
    if (length(catches_l) > 0) {
      output$zoomCatch <- renderPlot({
        plotCatchObj <- plotCatch(catches_l)
        return(plotCatchObj)
      })
    } else {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: load one or more stock objects first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    }
  })
  
  output$downloadCatch <- downloadHandler(
    filename = "Catches Structure Plot.png",
    content = function(file) {
      ggsave(file,
             device = png,
             width = 16,
             height = 9,
             units = "in",
             dpi = 300)
    }
  )
  
  observeEvent(input$zoomWaaButton, {
    if (length(waa_l) > 0) {
      output$zoomWaa <- renderPlot({
        plotWaaObj <- plotWaa(waa_l, pops_l)
        return(plotWaaObj)
      })
    } else {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: load one or more stock objects first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    }
  })
  
  output$downloadWaa <- downloadHandler(
    filename = "Weight-at-age Structure Plot.png",
    content = function(file) {
      ggsave(file,
             device = png,
             width = 16,
             height = 9,
             units = "in",
             dpi = 300)
    }
  )
    
  ##### FISHING MORTALITY #####
  
  observeEvent(input$loadButton, {
    if (length(pops_l) > 0)
      {f_w <<- fmort_w}
    })
  
  observeEvent(input$calcFmortButton, {
    if (input$baseline == "") {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: choose a baseline first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
      } else if (as.integer(input$baseline) > nrow(f_w)) {
        showModal(tags$div(id = "modalWarning",
                           modalDialog("Warning: baseline can't be higher than total years number!",
                                       footer = NULL,
                                       easyClose = TRUE)))
      } else {
        fmort_baseline <<- as.integer(input$baseline)
        baseline_rows <- seq.int(
          from = nrow(f_w) - as.integer(input$baseline) + 1L,
          to = nrow(f_w)
        )
        f_curr <- colMeans(f_w[baseline_rows, -1, drop = FALSE])
        f_vec <- rep(1, length(f_curr))
        f_new <- f_curr * f_vec
        f_new <<- data.frame(as.list(f_new), check.names = FALSE)
        f_adj <<- data.frame(as.list(f_new), check.names = FALSE)
        f_tot <<- t(rbind(f_new, f_adj, round(f_adj/f_new, 2)))
        colnames(f_tot) <<- c("Calculated", "Adjusted", "Multiplier")
        rownames(f_tot) <<- gsub("_F", "_GSA", rownames(f_tot))
        f_adj_display <<- f_adj
        f_adj_display[1,] <<- paste0(round(f_adj[1,], 2), " (x1)")
        output$statquoFmort <- renderTable(data.frame(as.list(f_new)),
                                           bordered = TRUE,
                                           width = "100%",
                                           align = "r")
        output$adjustedFmort <- renderTable(data.frame(as.list(f_adj_display)),
                                            bordered = TRUE,
                                            width = "100%",
                                            align = "r")
        output$pickFmort <- renderUI({
          pickerInput(inputId = "pickFmort",
                      label = NULL,
                      choices = c(species, colnames(data.frame(as.list(f_new)))),
                      options = list(style = "btn-primary"))
          })
        }
    })
  
  observeEvent(input$adjustFmort, {
    if (length(f_adj) != 0) {
      pick <- if (is.null(input$pickFmort) ||
                  !length(input$pickFmort)) {
        integer(0)
      } else {
        grep(input$pickFmort, colnames(f_adj), fixed = TRUE)
      }
      if (!length(pick)) return(invisible(NULL))
      if (nrow(f_adj) > 1L) {
        f_adj <<- f_new
        f_adj_display <<- f_adj
        showNotification(
          "Slider adjustment restored one-row F mode.",
          type = "message", duration = 6
        )
      }
      f_adj[1, pick] <<- f_new[pick] * input$adjustFmort
      f_tot <<- t(rbind(f_new, f_adj, round(f_adj/f_new, 2)))
      colnames(f_tot) <<- c("Calculated", "Adjusted", "Multiplier")
      rownames(f_tot) <<- gsub("_F", "_GSA", rownames(f_tot))
      f_adj_display[1, pick] <<- paste0(round(f_adj[1, pick], 2), " (x", input$adjustFmort, ")")
      output$adjustedFmort <- renderTable(f_adj_display,
                                          bordered = TRUE,
                                          width = "100%",
                                          align = "r")
      } else {
      return(NULL)
      }
    })
  
  observeEvent(input$calcMultAdjustFmort, {
    if (length(f_adj) != 0) {
      if (nrow(f_adj) > 1L) {
        f_adj <<- f_new
        showNotification(
          "Slider adjustment restored one-row F mode.",
          type = "message", duration = 6
        )
      }
      f_adj[1,] <<- f_new * as.double(input$adjustFmort)
      f_tot <<- t(rbind(f_new, f_adj, round(f_adj/f_new, 2)))
      colnames(f_tot) <<- c("Calculated", "Adjusted", "Multiplier")
      rownames(f_tot) <<- gsub("_F", "_GSA", rownames(f_tot))
      f_adj_display <<- f_adj
      f_adj_display[1,] <<- paste0(round(f_adj[1,], 2), " (x", input$adjustFmort, ")")
      output$adjustedFmort <- renderTable(f_adj_display,
                                          bordered = TRUE,
                                          width = "100%",
                                          align = "r")
      } else {
      return(NULL)
      }
    })
  
  output$downloadAdjFmort <- downloadHandler(
    filename = "Adjusted_Fmort.rds",
    content = function(filename) {
      exported <- if (nrow(f_adj) > 1L) {
        data.frame(
          year = max(as.integer(neuralNetInputs$year)) + seq_len(nrow(f_adj)),
          f_adj, check.names = FALSE
        )
      } else {
        f_adj
      }
      saveRDS(exported, filename)
      }
    )
  
  shinyFileChoose(input, "fileAdjFmort",
                  roots = vol,
                  filetypes = c("rds", "rda", "rdata", "RData", "csv"))
  
  fadjFilename <- reactive({
    parseFilePaths(vol, input$fileAdjFmort)
  })
  
  readFishingScenarioFile <- function(path) {
    extension <- tolower(tools::file_ext(path))
    if (extension == "rds") return(readRDS(path))
    if (extension == "csv") {
      return(utils::read.csv(
        path, check.names = FALSE, stringsAsFactors = FALSE
      ))
    }
    if (extension %in% c("rda", "rdata")) {
      isolated <- new.env(parent = emptyenv())
      objects <- load(path, envir = isolated)
      if (length(objects) != 1L) {
        stop("The RData file must contain exactly one F table.", call. = FALSE)
      }
      return(isolated[[objects[[1L]]]])
    }
    stop("Use an .rds, .rda, .RData or .csv fishing-mortality file.",
         call. = FALSE)
  }

  loadAdjustedFishingMortality <- function() {
    selected <- fadjFilename()
    if (nrow(selected) != 1L ||
        !file.exists(selected$datapath[[1L]])) {
      stop("Select an existing fishing-mortality file first.", call. = FALSE)
    }
    if (!length(f_new) || !nrow(neuralNetInputs)) {
      stop("Load the stocks and calculate baseline F before importing a scenario.",
           call. = FALSE)
    }
    loaded <- normalizeFishingScenario(
      readFishingScenarioFile(selected$datapath[[1L]]),
      names(f_new), max(as.integer(neuralNetInputs$year)) + 1L
    )
    first_row <- loaded[1L, , drop = FALSE]
    new_total <- t(rbind(f_new, first_row, round(first_row / f_new, 2)))
    colnames(new_total) <- c("Calculated", "Adjusted", "Multiplier")
    rownames(new_total) <- gsub("_F", "_GSA", rownames(new_total))
    new_display <- if (nrow(loaded) > 1L) {
      data.frame(
        year = max(as.integer(neuralNetInputs$year)) + seq_len(nrow(loaded)),
        loaded, check.names = FALSE
      )
    } else {
      loaded
    }
    f_adj <<- loaded
    f_tot <<- new_total
    f_adj_display <<- new_display
    output$adjustedFmort <- renderTable(
      f_adj_display, bordered = TRUE, width = "100%", rownames = FALSE
    )
    invisible(nrow(f_adj))
  }
  
  observeEvent(input$loadAdjFmort, {
    loaded <- tryCatch(
      loadAdjustedFishingMortality(), error = function(error) error
    )
    if (inherits(loaded, "error")) {
      showModal(tags$div(
        id = "modalWarning",
        modalDialog(
          paste("F import stopped:", conditionMessage(loaded)),
          footer = NULL, easyClose = TRUE
        )
      ))
    }
  })
  
  ##### NEURAL NETWORK #####
  
  observeEvent(input$loadButton, {
    
    n <- as.integer(ncol(neuralNetInputs) - 1)
    
    updatePickerInput(session = session,
                      inputId = "neurons1",
                      choices = c(n, 2 * n, 4 * n),
                      selected = n)
    
    updatePickerInput(session = session,
                      inputId = "neurons2",
                      choices = c(0, n, 2 * n, 4 * n),
                      selected = n)
    
    updatePickerInput(session = session,
                      inputId = "neurons3",
                      choices = c(0, n, 2 * n, 4 * n),
                      selected = 0)
    
    updatePickerInput(session = session,
                      inputId = "neurons4",
                      choices = c(0, n, 2 * n, 4 * n),
                      selected = n)
    
    updatePickerInput(session = session,
                      inputId = "neurons5",
                      choices = c(0, n, 2 * n, 4 * n),
                      selected = 0)
    
  })
  
  observeEvent(input$plotNetButton, {
    
    if (length(species) == 0) {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: upload one or more stock objects first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    } else {
      layerTypeTot <- vector()
      neuronsTot <- vector()
      dropoutTot <- vector()
      inputNames <- colnames(neuralNetInputs[,-1])
      
      for (i in seq_len(as.integer(input$nLayers))) {
        layerTypeTot[i] <- input[[paste0("layerType", i)]]
        neuronsTot[i] <- as.integer(input[[paste0("neurons", i)]])
        dropoutTot[i] <- as.numeric(input[[paste0("dropout", i)]])
      }
      
      output$plotNet <- renderPlot({
        plotNet(as.integer(input$nLayers), layerTypeTot, neuronsTot, dropoutTot, inputNames)
      }, height = 700)
    }
  })
  
  ##### TEST #####
  
  observeEvent(input$testFitButton, {
    
    if (length(species) == 0) {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: upload one or more stock objects first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    } else {
      showModal(tags$div(id = "modalBackground", modalDialog("", footer = NULL)))
      fit_result <- tryCatch(
        testFitNet(neuralNetInputs),
        error = function(error) error
      )
      removeModal()
      if (inherits(fit_result, "error")) {
        showModal(tags$div(
          id = "modalWarning",
          modalDialog(
            paste("Model fit stopped:", formatRuntimeError(fit_result, "whole-series fit")),
            footer = NULL,
            easyClose = TRUE
          )
        ))
      } else {
        testfit_results <<- fit_result
      }
    }
  })
  
  observeEvent(input$plotFitButton, {
    
    if (length(testfit_results) > 0) {
      testfit_plots <- plotFitNet(testfit_results)
      
      output$plotFit <- renderPlot({
        testfit_plots
        })
    } else {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: fit one or more stock objects first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    }
  })
  
  observeEvent(input$testTrainTestButton, {
    
    depth_test <<- input$depthTest
    
    if (length(species) == 0) {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: upload one or more stock objects first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    } else if (depth_test == "") {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: select the depth first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    } else if (
      nrow(neuralNetInputs) - as.integer(depth_test) <
        minimum_years_for_temporal_test
    ) {
      showModal(tags$div(id = "modalWarning",
                         modalDialog(
                           paste(
                             "Warning: test depth must leave at least",
                             minimum_years_for_temporal_test,
                             "years for temporal training and validation."
                           ),
                                     footer = NULL,
                                     easyClose = TRUE)))
    } else {
      showModal(tags$div(id = "modalBackground", modalDialog("", footer = NULL)))
      train_test_result <- tryCatch(
        trainTestFitNet(neuralNetInputs, as.integer(depth_test)),
        error = function(error) error
      )
      removeModal()
      if (inherits(train_test_result, "error")) {
        showModal(tags$div(
          id = "modalWarning",
          modalDialog(
            paste("Train/test stopped:", formatRuntimeError(train_test_result, "train/test")),
            footer = NULL,
            easyClose = TRUE
          )
        ))
      } else {
        traintest_results <<- train_test_result
      }
    }
  })
  
  observeEvent(input$plotTrainTestButton, {
    
    if (length(traintest_results) > 0) {
      plotTestCount <<- 1
      
      for (i in seq_along(traintest_results)) {
        traintest_plots[[i]] <<- plotTrainTestFitNet(traintest_results[[i]], i, as.integer(input$depthTest))
        traintest_recr_plots[[i]] <<- plotRecruitment(traintest_results[[i]], i, as.integer(input$depthTest))
        taylor_diagram[[i]] <<- plotTaylorDiagram(traintest_iter_results[[i]])
      }
      traintest_metrics_plot <<- plotFitNet(traintest_metrics)
      output$showSpeciesTest <- renderText({
        species[[plotTestCount]]
      })
      output$plotTrainTest <- renderPlotly({
        asMaelstromPlotly(traintest_plots[[plotTestCount]])
      })
      output$plotMetricsTest <- renderPlot({
        traintest_metrics_plot
      })
      output$nParamsTest <- renderText({
        paste0("Number of parameters: ", traintest_nparams)
      })
      output$plotRecruitmentTraintest <- renderPlot({
        traintest_recr_plots[[plotTestCount]]
      })
      output$taylorDiagram <- renderPlot({
        taylor_diagram[[plotTestCount]]
      })
      
    } else {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: test one or more stock objects first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    }
    
  })
  
  observeEvent(input$plotLogTrainTest, {
    if (length(traintest_results) > 0) {
      if (input$plotLogTrainTest == T) {
        output$plotTrainTest <- renderPlotly({
          asMaelstromPlotly(
            traintest_plots[[plotTestCount]] + scale_y_continuous(trans = "log10")
          )
          })
        }
        else {output$plotTrainTest <- renderPlotly({
          asMaelstromPlotly(traintest_plots[[plotTestCount]])
          })
        }
    }
  })
  
  observeEvent(input$plotTestBack, {
    if (plotTestCount > 0) {
      if (length(traintest_results) == 1) {
        return(NULL)
      } else if (plotTestCount == 1) {
        plotTestCount <<- as.numeric(length(traintest_results))
        output$plotTrainTest <- renderPlotly({
          asMaelstromPlotly(traintest_plots[[plotTestCount]])
        })
        output$plotRecruitmentTraintest <- renderPlot({
          traintest_recr_plots[[plotTestCount]]
        })
        output$taylorDiagram <- renderPlot({
          taylor_diagram[[plotTestCount]]
        })
        output$showSpeciesTest <- renderText({
          species[[plotTestCount]]
        })
      } else {
        plotTestCount <<- plotTestCount - 1
        output$plotTrainTest <- renderPlotly({
          asMaelstromPlotly(traintest_plots[[plotTestCount]])
        })
        output$plotRecruitmentTraintest <- renderPlot({
          traintest_recr_plots[[plotTestCount]]
        })
        output$taylorDiagram <- renderPlot({
          taylor_diagram[[plotTestCount]]
        })
        output$showSpeciesTest <- renderText({
          species[[plotTestCount]]
        })
      }
    } else {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: plot test result first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    }
  })
  
  observeEvent(input$plotTestForward, {
    if (plotTestCount > 0) {
      if (length(traintest_results) == 1) {
        return(NULL)
      } else if (plotTestCount == as.numeric(length(traintest_results))) {
        plotTestCount <<- 1
        output$plotTrainTest <- renderPlotly({
          asMaelstromPlotly(traintest_plots[[plotTestCount]])
        })
        output$plotRecruitmentTraintest <- renderPlot({
          traintest_recr_plots[[plotTestCount]]
        })
        output$taylorDiagram <- renderPlot({
          taylor_diagram[[plotTestCount]]
        })
        output$showSpeciesTest <- renderText({
          species[[plotTestCount]]
        })
      } else {
        plotTestCount <<- plotTestCount + 1
        output$plotTrainTest <- renderPlotly({
          asMaelstromPlotly(traintest_plots[[plotTestCount]])
        })
        output$plotRecruitmentTraintest <- renderPlot({
          traintest_recr_plots[[plotTestCount]]
        })
        output$taylorDiagram <- renderPlot({
          taylor_diagram[[plotTestCount]]
        })
        output$showSpeciesTest <- renderText({
          species[[plotTestCount]]
        })
      }
    } else {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: plot test result first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    }
  })
  
  observeEvent(input$zoomPlotFitButton, {
    if (length(traintest_results) > 0) {
      output$zoomPlotFit <- renderPlot({
        traintest_plots[[plotTestCount]]
      })
    } else {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: test one or more stock objects first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    }
  })
  
  ##### PREDICT #####
  
  plotPredCount <- 0
  
  observeEvent(input$calcPredButton, {
    selected_depth <- input$depthPred
    
    if (length(species) == 0) {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: upload one or more stock objects first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    } else if (input$baseline == "" || !length(f_new)) {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: calculate fishing mortality first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    } else if (is.null(selected_depth) ||
               !length(selected_depth) || selected_depth == "") {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: select the depth first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    } else {
      showModal(tags$div(id = "modalBackground", modalDialog("", footer = NULL)))
      forecast_result <- tryCatch(
        predNet(neuralNetInputs, f_new, f_adj, as.integer(selected_depth)),
        error = function(error) error
      )
      removeModal()
      if (inherits(forecast_result, "error")) {
        showModal(tags$div(
          id = "modalWarning",
          modalDialog(
            paste("Forecast stopped:", formatRuntimeError(forecast_result, "forecast")),
            footer = NULL,
            easyClose = TRUE
          )
        ))
      } else {
        depth_pred <<- selected_depth
        pred_results <<- forecast_result
      }
    }
  })
  
  observeEvent(input$plotPredButton, {
    if (length(pred_results) > 0) {
      plotPredCount <<- 1
      
      for (i in seq_along(pred_results)) {
        pred_plots[[i]] <<- plotPred(pred_results[[i]], neuralNetInputs, i)
        pred_recr_plots[[i]] <<- plotRecruitment(pred_results[[i]], i, as.integer(depth_pred))
      }
      
      output$plotPred <- renderPlotly({
        asMaelstromPlotly(pred_plots[[plotPredCount]])
      })
      
      output$plotRecruitmentForecast <- renderPlot({
        pred_recr_plots[[plotPredCount]]
      })
      
      output$showSpeciesPred <- renderText({
        species[[plotPredCount]]
      })
    
    } else {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: predict one or more stock objects first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    }
  })
  
  observeEvent(input$plotLogPred, {
    if (length(pred_results) > 0) {
      if (input$plotLogPred == T) {
        output$plotPred <- renderPlotly({
          asMaelstromPlotly(
            pred_plots[[plotPredCount]] + scale_y_continuous(trans = "log10")
          )
          })
        }
      else {output$plotPred <- renderPlotly({
        asMaelstromPlotly(pred_plots[[plotPredCount]])
        })
      }
    }
  })
  
  observeEvent(input$plotPredBack, {
    if (plotPredCount > 0) {
      if (length(pred_results) == 1) {
        return(NULL)
        } else if (plotPredCount == 1) {
          plotPredCount <<- as.numeric(length(pred_results))
          output$plotPred <- renderPlotly({
            asMaelstromPlotly(pred_plots[[plotPredCount]])
          })
          output$plotRecruitmentForecast <- renderPlot({
            pred_recr_plots[[plotPredCount]]
          })
          output$showSpeciesPred <- renderText({
            species[[plotPredCount]]
          })
        } else {
          plotPredCount <<- plotPredCount - 1
          output$plotPred <- renderPlotly({
            asMaelstromPlotly(pred_plots[[plotPredCount]])
          })
          output$plotRecruitmentForecast <- renderPlot({
            pred_recr_plots[[plotPredCount]]
          })
          output$showSpeciesPred <- renderText({
            species[[plotPredCount]]
          })
        }
    } else {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: plot predict result first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    }
  })
  
  observeEvent(input$plotPredForward, {
    if (plotPredCount > 0) {
      if (length(pred_results) == 1) {
        return(NULL)
        } else if (plotPredCount == as.numeric(length(pred_results))) {
          plotPredCount <<- 1
          output$plotPred <- renderPlotly({
            asMaelstromPlotly(pred_plots[[plotPredCount]])
          })
          output$plotRecruitmentForecast <- renderPlot({
            pred_recr_plots[[plotPredCount]]
          })
          output$showSpeciesPred <- renderText({
            species[[plotPredCount]]
          })
        } else {
          plotPredCount <<- plotPredCount + 1
          output$plotPred <- renderPlotly({
            asMaelstromPlotly(pred_plots[[plotPredCount]])
          })
          output$plotRecruitmentForecast <- renderPlot({
            pred_recr_plots[[plotPredCount]]
          })
          output$showSpeciesPred <- renderText({
            species[[plotPredCount]]
            })
        }
      
    } else {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: plot predict result first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    }
  })
  
  observeEvent(input$zoomPredButton, {
    if (length(pred_results) > 0) {
      output$zoomPred <- renderPlot({
        pred_plots[[plotPredCount]]
      })
    } else {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: predict one or more stock objects first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    }
  })
  
  output$downloadPred <- downloadHandler(
    filename = "Predict Plot.png",
    content = function(file) {
      ggsave(file,
             device = png,
             width = 16,
             height = 9,
             units = "in",
             dpi = 300)
    })
  
  output$downloadRatio <- downloadHandler(
    filename = "Ratio Plot.png",
    content = function(file) {
      ggsave(file,
             device = png,
             width = 16,
             height = 9,
             units = "in",
             dpi = 300)
    })
  
  ##### SENSITIVITY ANALYSIS #####
  
  observeEvent(input$calcSensButton, {
    if (length(pred_results) > 0) {
      showModal(tags$div(id = "modalBackground", modalDialog("", footer = NULL)))
      sens_results <<- sensAnalysis(neuralNetInputs, pred_iter_partial, model_pred)
      removeModal()
    } else {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: predict one or more stock objects first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    }
  })
  
  observeEvent(input$plotSensButton, {
    if (length(sens_results) > 0) {
      sens_plots <<- plotSensAnalysis(sens_results)
      output$plotSens <- renderPlot({
        sens_plots
      }, height = 700)
    } else {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: run sensitivity analysis first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    }
  })
  
  observeEvent(input$zoomSensButton, {
    if (length(sens_results) > 0) {
      z <- plotSensAnalysis(sens_results)
      output$zoomSens <- renderPlot({
        z
      })
    } else {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: run sensitivity analysis first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    }
  })
  
  output$downloadSens <- downloadHandler(
    filename = "Sensitivity Plot.png",
    content = function(file) {
      ggsave(file,
             device = png,
             width = 16,
             height = 9,
             units = "in",
             dpi = 300)
    })
  
  ##### EXPORT AND LOAD WORKSPACE #####
  
  shinyDirChoose(input, "dir",
                 roots = vol,
                 filetypes = c("", "txt"))
  
  dirname <- reactive({
    parseDirPath(vol, input$dir)
    })
  
  observe({
    output$dir <- renderText(dirname())
  })
  
  observeEvent(input$saveWS, {
    if (length(dirname()) == 0) {
      return(NULL)
      } else {
        save_list <<- list(
          schema_version = app_version,
          species = species,
          gsa = gsa,
          gsa_tot = gsa_tot,
          pops = pops,
          pops_l = pops_l,
          pops_w = pops_w,
          catches = catches,
          catches_l = catches_l,
          catches_w = catches_w,
          waa = waa,
          waa_l = waa_l,
          waa_w = waa_w,
          fmorts = fmorts,
          fmort_l = fmort_l,
          fmort_w = fmort_w,
          fmort_spawns = fmort_spawns,
          fmort_spawn_l = fmort_spawn_l,
          fmort_spawn_w = fmort_spawn_w,
          morts = morts,
          mort_l = mort_l,
          mort_w = mort_w,
          mort_spawns = mort_spawns,
          mort_spawn_l = mort_spawn_l,
          mort_spawn_w = mort_spawn_w,
          matures = matures,
          mature_l = mature_l,
          mature_w = mature_w,
          neuralNetInputs = neuralNetInputs,
          range_inputs = range_inputs,
          range_outputs = range_outputs,
          f_w = f_w,
          fmort_baseline = fmort_baseline,
          f_new = f_new,
          f_adj = f_adj,
          f_tot = f_tot,
          f_adj_display = f_adj_display,
          f_applied = f_applied,
          depth_test = depth_test,
          testfit_results = testfit_results,
          traintest_output_raw = traintest_output_raw,
          traintest_iter_results = traintest_iter_results,
          traintest_metrics = traintest_metrics,
          traintest_metrics_plot = traintest_metrics_plot,
          traintest_results = traintest_results,
          traintest_plots = traintest_plots,
          traintest_recr_plots = traintest_recr_plots,
          taylor_diagram = taylor_diagram,
          depth_pred = depth_pred,
          model_pred = model_pred,
          pred_output_raw = pred_output_raw,
          pred_iter_partial = pred_iter_partial,
          pred_results = pred_results,
          pred_plots = pred_plots,
          pred_recr_plots = pred_recr_plots,
          sens_results = sens_results,
          sens_plots = sens_plots
          )
        saveRDS(save_list, file.path(dirname(), paste0(Sys.Date(), "_session.rds")))
        }
    })
  
  shinyFileChoose(input, "file",
                  roots = vol,
                  filetypes = c("rds", "rdata", "RData"))
  
  filename <- reactive({
    parseFilePaths(vol, input$file)
    })
  
  observe({
    selected <- filename()
    output$file <- renderText(if (nrow(selected)) selected$name[[1L]] else "")
  })
  
  loadDFS <- reactive({
    selected <- filename()
    req(nrow(selected) == 1L)
    req(file.exists(selected$datapath[[1L]]))
    l <- readRDS(selected$datapath[[1L]])
    if (!is.list(l) || is.null(l$species) || is.null(l$neuralNetInputs)) {
      stop("The selected file is not a valid MAELSTROM session.", call. = FALSE)
    }
    loadInput(l)
    })
  
  observeEvent(input$loadWS, {
    loadDFS()
  })
  
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      report_dir <- file.path(tempdir(), paste0("maelstrom-report-", session$token))
      dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
      report_output <- rmarkdown::render(
        input = "maelstrom.Rmd",
        output_format = "pdf_document",
        output_file = "report.pdf",
        output_dir = report_dir,
        envir = environment(),
        quiet = TRUE
      )
      if (!file.copy(report_output, file, overwrite = TRUE)) {
        stop("Unable to copy the generated report.", call. = FALSE)
      }
    })
  
}

return(server)
