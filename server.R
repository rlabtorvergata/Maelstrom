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

##### VARIABLES #####

options(max.print = 99999)
set_random_seed(123)

species <- list() #species loaded
gsa <- list() #list of gsa per stock loaded
gsa_tot <- vector() #unique gsa loaded
rv <- list() #list of stocks loaded

pops <- list() #population processing
pops_l <- data.frame() #population processing
pops_w <- data.frame() #population processing
catches <- list() #catches processing
catches_l <- data.frame() #catches processing
catches_w <- data.frame() #catches processing
waa <- list() #weight-at-age processing
waa_l <- data.frame() #weight-at-age processing
waa_w <- data.frame() #weight-at-age processing
fmorts <- list() #fishing mortality processing
fmort_l <- data.frame() #fishing mortality processing
fmort_w <- data.frame() #fishing mortality processing
fmort_spawns <- list() #fishing mortality before spawning processing
fmort_spawn_l <- data.frame() #fishing mortality before spawning processing
fmort_spawn_w <- data.frame() #fishing mortality before spawning processing
morts <- list() #natural mortality processing
mort_l <- data.frame() #natural mortality processing
mort_w <- data.frame() #natural mortality processing
mort_spawns <- list() #natural mortality before spawning processing
mort_spawn_l <- data.frame() #natural mortality before spawning processing
mort_spawn_w <- data.frame() #natural mortality before spawning processing
matures <- list() #mature ratio processing
mature_l <- data.frame() #mature ratio processing
mature_w <- data.frame() #mature ratio processing

neuralNetInputs <- data.frame() #neural network input dataframe
f_w <- data.frame() #fishing mortality dataframe
fmort_baseline <- NULL #baseline for fishing mortality standard
f_new <- data.frame() #status quo fishing mortality vector
f_adj <- data.frame() #adjusted fishing mortality vector
f_tot <- data.frame() #status quo + adjusted fishing mortality table (for markdown)
f_adj_display <- data.frame() #adjusted fishing mortality vector (for modal display)

range_inputs <- data.frame() #range for inputs denormalization
range_outputs <- data.frame() #range for outputs denormalization

depth_test <- NULL #number of years to forecast
plotTestCount <- 0 #traintest species counter
testfit_results <- data.frame() #testfit results
traintest_output_raw <- list() #partial results of prediction
traintest_iter_results <- list() #traintest results per iteration
traintest_metrics <- data.frame() #traintest metrics per species
traintest_metrics_plot <- NULL #traintest metrics plot
traintest_nparams <- 0 #traintest number of parameters
traintest_results <- data.frame() #traintest results
traintest_plots <- list() #traintest plots
traintest_recr_plots <- list() #recruitment distribution plots
taylor_diagram <- list() #Taylor diagrams

depth_pred <- NULL #number of years to forecast
plotPredCount <- 0 #forecast species counter
pred_output_raw <- vector("list", length = 3) # Partial results of prediction
model_pred <- list() #list of models in prediction
pred_iter_partial <- data.frame() #forecast results
pred_results <- list() #forecast results
pred_plots <- list() #forecast plots
pred_recr_plots <- list() #recruitment distribution plots

sens_results <- data.frame() #sensitivity analysis results
sens_plots <- list() #sensitivity analysis plot

save_list <- list() #saved files list

##### SERVER LOGIC #####

server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  ##### FUNCTIONS #####
  
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
    load(fileName)
    get(ls()[ls() != "fileName"])
  }
  
  normalizeInputs <- function(source_df, range_df) {
    norm_df <- data.frame()
    range <- data.frame()
    for (i in 1:ncol(source_df)) {
      range[1, i] <- min(source_df[, i])
      range[2, i] <- max(source_df[, i])
      for (j in 1:nrow(source_df)) {
        norm_df[j, i] = (source_df[j, i] - min(source_df[, i]))/(max(source_df[, i]) - min(source_df[, i]))
      }
    }
    colnames(norm_df) <- colnames(source_df)
    colnames(range) <- colnames(source_df)
    assign(deparse(substitute(range_df)), range, pos = parent.frame())
    return(norm_df)
  }
  
  denormalizeInputs <- function(source_df, range_df) {
    denorm_df <- data.frame()
    for (i in 1:ncol(source_df)) {
      for (j in 1:nrow(source_df)) {
        denorm_df[j, i] = source_df[j, i] * (range_df[2, i] - range_df[1, i]) + (range_df[1, i])
      }
    }
    colnames(denorm_df) <- colnames(source_df)
    return(denorm_df)
  }
  
  procGSA <- function(gsa) {
    for (i in 1:length(gsa)) {
      if (i == 1) {
        gsa1 = NULL
        gsa1 = paste0(gsa1, gsa[i], sep = "_")
      } else {
        gsa1 = paste0(gsa1, gsa[i], sep = "_")
      }
      if (i == length(gsa)) {
        gsa1 = substr(gsa1, 1, nchar(gsa1)-1)
      }
    }
    return(gsa1)
  }
  
  procDfLongQuant <- function(stock, gsa, tri, minAge, baselineAge, baselineYear, fun, var) {
    if (minAge == 0) {baselineAge = baselineAge + 1}
    stk_temp = fun(stock)
    if (nrow(fun(stock)) > baselineAge) {
      stk_temp = stk_temp[1:baselineAge,]
      stk_temp[baselineAge,] = colSums(fun(stock)[baselineAge:nrow(fun(stock)), ])
    }
    df_temp = as.data.frame(stk_temp)[, c("year", "age", "data")]
    df_temp = df_temp[which(df_temp$year >= baselineYear),]
    df = df_temp[, 1:2]
    df[, 3] = df_temp[, 3] * 1000
    df[, 4] = procGSA(paste0(gsa, collapse = "-"))
    df[, 5] = tri
    df[, 6] = paste(tri, paste(paste0(gsa, collapse = "-"), collapse = "_"), sep = "_")
    colnames(df) <- c("year", "age", as.character(substitute(var)), "gsa", "species", "tri_gsa")
    return(df)
  }
  
  procDfLongMult <- function(stock, gsa, tri, minAge, baselineAge, baselineYear, fun, var) {
    if (minAge == 0) {baselineAge = baselineAge + 1}
    stk_temp = fun(stock)
    if (nrow(fun(stock)) > baselineAge) {
      stk_temp = stk_temp[1:baselineAge,]
      stk_temp[baselineAge,] = colMeans(fun(stock)[baselineAge:nrow(fun(stock)), ])
    }
    df_temp = as.data.frame(stk_temp)[, c("year", "age", "data")] #Thousands
    df_temp = df_temp[which(df_temp$year >= baselineYear),]
    df = df_temp[, 1:2]
    df[, 3] = df_temp[, 3]
    df[, 4] = procGSA(paste0(gsa, collapse = "-"))
    df[, 5] = tri
    df[, 6] = paste(tri, paste(paste0(gsa, collapse = "-"), collapse = "_"), sep = "_")
    colnames(df) <- c("year", "age", as.character(substitute(var)), "gsa", "species", "tri_gsa")
    return(df)
  }
  
  procDfWide <- function(dflong, var, code) {
    df_w <- dcast(data = dflong, paste0("year + ", as.character(substitute(var)), " ~ tri_gsa + age"))
    for (i in 1:nrow(df_w)) {
      for (j in 3:ncol(df_w)) {
        if (!is.na(df_w[i, j])) {
          df_w[i, j] = df_w[i, 2]
        }
      }
    }
    df_w[, 2] <- NULL
    for (i in 2:ncol(df_w)) {
      colnames(df_w)[i] <- paste0(strsplit(colnames(df_w)[i], "_")[[1]][1], "_",
                                  strsplit(colnames(df_w)[i], "_")[[1]][length(strsplit(colnames(df_w)[i], "_")[[1]])], "_", as.character(substitute(code)), "_",
                                  strsplit(colnames(df_w)[i], "_")[[1]][2])
    }
    df_w[is.na(df_w)] = 0
    df_w <- mutate_all(df_w, function(x) as.numeric(as.character(x)))
    df_w <- aggregate(. ~ year, df_w, FUN = sum)
    return(df_w)
  }

  adjDfWide <- function(dflong, dfwide) {
    for (i in 2:ncol(dfwide)) {
      out = boxplot.stats(dfwide[, i])$out
      if (length(out) != 0) {
        out_pos = match(out, dfwide[, i])
        for (j in 1:length(out_pos)) {
          if (dfwide[out_pos[j], i] > 2 * mean(dfwide[-out_pos, i]) | dfwide[out_pos[j], i] < mean(dfwide[-out_pos, i])/2) {
            dfwide[out_pos[j], i] = stats::filter(as.double(dfwide[,i]), c(0, 1/2, 1/2), circular = TRUE, sides = 1)[out_pos[j]]
            dflong[match(round(out[j], 5), round(dflong[, 3], 5)), 3] <<- dfwide[out_pos[j], i]
          }
        }
      }
    }
    return(dfwide)
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
      tot_df <- rbind(tot_df, df59)
      years <- intersect(years, df9$year)
    }
    if (!missing(df10)) {
      tot_df <- rbind(tot_df, df10)
      years <- intersect(years, df10$year)
    }
    tot_df <- tot_df[tot_df$year %in% years, ]
    return(tot_df)
  }
  
  totDfBiomass <- function(df1, df2, df3, df4, df5) {
    df_names <- substr(colnames(df1)[2], 1, (nchar(df1) - 1))
    if (missing(df2)) {
      tot_df <- df1
    }
    if (!missing(df2)) {
      tot_df <- rbind(df1, df2)
    }
    if (!missing(df3)) {
      tot_df <- rbind(tot_df, df3)
    }
    if (!missing(df4)) {
      tot_df <- rbind(tot_df, df4)
    }
    if (!missing(df5)) {
      tot_df <- rbind(tot_df, df5)
    }
    return(tot_df)
  }
  
  catchBaranov <- function(fmortwide, mortwide, popwide) {
    ((fmortwide/(fmortwide + mortwide)) * (1 - exp(-(fmortwide + mortwide))) * popwide)
  }
  
  plotPop <- function(poplong) {
    
    poplong$age <- as.factor(poplong$age)
    
    colnames(poplong)[3] <- "Population"
    
    ggplot(data = poplong, aes(x = year)) +
      geom_line(aes(y = Population, colour = age), linewidth = 2) +
      scale_x_continuous(breaks = seq(min(poplong$year), max(poplong$year), 1)) +
      ggtitle("Population by age/year") +
      xlab("Year") +
      ylab("Population (n° of individuals)") +
      scale_colour_brewer(name = "Age", palette = "Blues", direction = -1) +
      theme_test() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "bottom") +
      facet_wrap(~ tri_gsa, scales = "free")
  }
  
  plotCatch <- function(catchlong) {
    
    catchlong$age <- as.factor(catchlong$age)
    
    colnames(catchlong)[3] <- "Catches"
    
    ggplot(data = catchlong, aes(x = year)) +
      geom_line(aes(y = Catches, colour = age), linewidth = 2) +
      scale_x_continuous(breaks = seq(min(catchlong$year), max(catchlong$year), 1)) +
      ggtitle("Catch by age/year") +
      xlab("Year") +
      ylab("Catches (n° of individuals)") +
      scale_colour_brewer(name = "Age", palette = "Blues", direction = -1) +
      theme_test() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "bottom") +
      facet_wrap(~ tri_gsa, scales = "free")
  }
  
  plotWaa <- function(waalong, poplong) {
    
    waalong$age <- as.factor(waalong$age)
    
    biomasslong <- data.frame(year = waalong$year, age = waalong$age, tri_gsa = waalong$tri_gsa,
                              TotBiomass = round((waalong$weight_at_age * poplong$pop)/1000, 2))
    
    ggplot(data = biomasslong, aes(x = year)) +
      geom_line(aes(y = TotBiomass, colour = age), linewidth = 2) +
      scale_x_continuous(breaks = seq(min(biomasslong$year), max(biomasslong$year), 1)) +
      ggtitle("Total Biomass by age/year") +
      xlab("Year") +
      ylab("Total Biomass (tons)") +
      scale_colour_brewer(name = "Age", palette = "Blues", direction = -1) +
      theme_test() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "bottom") +
      facet_wrap(~ tri_gsa, scales = "free")
  }
  
  procInputs <- function(popwide, catchwide) {
    tot_df <- cbind(popwide, catchwide[,2:ncol(catchwide)])
    return(tot_df)
  }
  
  randomEnvVarSum <- function(depth, mult, sd = 1) {
    D = depth
    M = mult
    vec <- rnorm(D, M/D, sd)
    if (abs(sum(vec)) < 0.01) vec <- vec + 1
    vec <- round(vec / sum(vec) * M, 2)
    deviation <- M - sum(vec)
    for (. in seq_len(abs(deviation))) {
      vec[i] <- vec[i <- sample(D, 1)] + sign(deviation)
    }
    return(vec)
  }
  
  plotNet <- function(nLayers, layerTypeTot, neuronsTot, dropoutTot, inputNames) {
    
    df <- data.frame(label = NULL, x = NULL, y = NULL, type = NULL, dropout = NULL, nLayer = NULL)
    df_iter <- data.frame(label = NULL, x = NULL, y = NULL, type = NULL, dropout = NULL, nLayer = NULL)
    
    for (i in 1:as.integer(nLayers)) {
      
      l = length(inputNames) * (neuronsTot[i] / length(inputNames))
      
      if (neuronsTot[i] == 0) {l = 1}
      
      if (i < as.integer(nLayers)) {
        df_iter = data.frame(label = NA,
                             x = rep(10 * i, l),
                             y = seq(90, 10, length = l),
                             type = rep(layerTypeTot[i], l),
                             dropout = rep(dropoutTot[i], l),
                             nLayer = rep(as.integer(i), l))
      } else {
        df_iter = data.frame(label = NA,
                             x = rep(10 * i, l/2),
                             y = seq(90, 10, length = l/2),
                             type = rep(layerTypeTot[i], l/2),
                             dropout = rep(dropoutTot[i], l/2),
                             nLayer = rep(as.integer(i), l/2))
      }
      
      if (neuronsTot[i] == 0) {df_iter$y = 50}
      if (i == 1) {df_iter$label = rep(inputNames, each = neuronsTot[i] / length(inputNames))}
      if (i == as.integer(nLayers)) {df_iter$label = inputNames[1:(length(inputNames)/2)]}
      
      df <- rbind(df, df_iter)
    }
    
    df_links <- data.frame(x0 = NULL, x1 = NULL, y0 = NULL, y1 = NULL)
    df_links_iter <- data.frame(x0 = NULL, x1 = NULL, y0 = NULL, y1 = NULL)
    
    for (i in 1:(nLayers - 1)) {
      set1 <- df[which(df$nLayer == i),]
      set2 <- df[which(df$nLayer == (i + 1)),]
      
      df_links_iter = data.frame(x0 = numeric(nrow(set1) * nrow(set2)),
                                 y0 = numeric(nrow(set1) * nrow(set2)),
                                 x1 = numeric(nrow(set1) * nrow(set2)),
                                 y1 = numeric(nrow(set1) * nrow(set2))) 
      k = 0
      for(i in 1:nrow(set1)) {
        for(j in 1:nrow(set2)) {
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
  
  testFitNet <- function(netInputs) {
    
    norm_inputs = netInputs
    norm_inputs[,2:ncol(norm_inputs)] = normalizeInputs(norm_inputs[,2:ncol(norm_inputs)], range_inputs)
    
    species = substr(colnames(netInputs)[grep("_N", names(netInputs))], 1, 3)
    age = substr(colnames(netInputs)[grep("_N", names(netInputs))], 5, 5)
    s_a = data.frame(species, age)
    s_a_min = s_a %>% group_by(species) %>% slice_min(age)
    s_a = paste(s_a$species, s_a$age)
    s_a_min = paste(s_a_min$species, s_a_min$age)
    
    min_age_pos = vector()
    for (i in 1:length(s_a_min)) {
      min_age_pos[i] = grep(s_a_min[i], s_a)
    }
    
    min_age_vec = vector()
    for (i in 1:length(unique(species))) {
      min_age_vec[i] = min(substr(colnames(netInputs[which(substr(colnames(netInputs), 1, 3) == unique(species)[i])]), 5, 5))
    }
    
    env_var <- length(grep("PrP_", names(netInputs)))
    
    gsa <- c()
    
    for (i in 2:(ncol(netInputs)/2 + 1)) {
      gsa <- append(gsa, strsplit(names(netInputs), "N_")[[i]][2])
    }
    
    proj <- norm_inputs
    
    if (env_var > 0) {
      envMult <- randomEnvVarSum(as.integer(input$depthPred), as.integer(input$envParamMult))
    }
    
    train_df <- as.matrix(proj[, 2:ncol(proj)])
    val_df <- as.matrix(proj[, 2:ncol(proj)])
    
    inputs <- layer_input(shape = c(nrow(train_df), (ncol(proj) - 1)))
    inputs_df <- train_df
    outputs <- inputs
    outputs_df <- train_df
    
    if (as.integer(input$nLayers) >= 1) {outputs <- buildNet(outputs, 1, input$layerType1, input$neurons1, input$returnSeq1,
                                                             input$dropout1, input$activation, input$recdropout1, input$recactivation)}
    if (as.integer(input$nLayers) >= 2) {outputs <- buildNet(outputs, 2, input$layerType2, input$neurons2, input$returnSeq2,
                                                             input$dropout2, input$activation, input$recdropout2, input$recactivation)}
    if (as.integer(input$nLayers) >= 3) {outputs <- buildNet(outputs, 3, input$layerType3, input$neurons3, input$returnSeq3,
                                                             input$dropout3, input$activation, input$recdropout3, input$recactivation)}
    if (as.integer(input$nLayers) >= 4) {outputs <- buildNet(outputs, 4, input$layerType4, input$neurons4, input$returnSeq4,
                                                             input$dropout4, input$activation, input$recdropout4, input$recactivation)}
    if (as.integer(input$nLayers) >= 5) {outputs <- buildNet(outputs, 5, input$layerType5, input$neurons5, input$returnSeq5,
                                                             input$dropout5, input$activation, input$recdropout5, input$recactivation)}
    
    dummy <- timeseries_dataset_from_array(
      inputs_df, outputs_df,
      sequence_length = as.numeric(nrow(train_df)),
      batch_size = 2
    )
    
    dummy_val <- timeseries_dataset_from_array(
      val_df, val_df,
      sequence_length = 4,
      batch_size = 2
    )
    
    model <- keras_model(inputs, outputs)
    
    callbacks <- list(
      callback_early_stopping(
        monitor = "loss", patience = 15),
      callback_model_checkpoint(
        "prova.keras", save_best_only = TRUE))
    
    model %>% compile(
      loss = "mse",
      metrics = "mae",
      optimizer_rmsprop(learning_rate = as.numeric(input$learnParam))
    )
    
    history <- model %>% fit(
      dummy,
      verbose = 1,
      epochs = as.integer(input$nEpochs),
      callbacks = callbacks
    )
    
    history_df <- na.omit(as.data.frame(history))
    levels(history_df$metric)[match("loss", levels(history_df$metric))] <- "MSE"
    levels(history_df$metric)[match("mae", levels(history_df$metric))] <- "MAE"
    history_df$iter <- 1
    
    return(history_df)
    
  }
  
  plotFitNet <- function(history_df) {
    
    n_runs <- 10
    n_last <- 10
    use_ci <- TRUE # TRUE = 95% CI; FALSE = SD
    
    traj <- history_df %>%
      group_by(epoch, data) %>%
      summarise(
        mean_mse = mean(value, na.rm = TRUE),
        sd_mse = sd(value, na.rm = TRUE),
        se_mse = sd_mse / sqrt(n_distinct(iter)),
        n_run = n_distinct(iter),
        .groups = "drop") %>%
      mutate(
        ribbon = ifelse(use_ci, se_mse, sd_mse),
        ymin = pmax(0, mean_mse - ribbon),
        ymax = mean_mse + ribbon)
    
    pA <- ggplot(traj, aes(x = epoch, y = mean_mse, colour = data, fill = data)) +
      geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.18, colour = NA) +
      geom_line(linewidth = 0.8) +
      labs(x = "Epoch", y = "Training and Validation", colour = "Metric", fill = "Metric",
           title = "A. Training and Validation across ten independent random initializations") +
      theme_bw() +
      theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "grey95"),
        panel.grid.minor = element_blank()
      )
    
    plateau_run <- history_df %>%
      group_by(iter, data) %>%
      mutate(max_epoch = max(epoch, na.rm = TRUE)) %>%
      filter(epoch > max_epoch - n_last) %>%
      summarise(plateau_mse = mean(value, na.rm = TRUE), .groups = "drop")
    
    plateau_summary <- plateau_run %>%
      group_by(data) %>% 
      summarise(
        mean_plateau_mse = mean(plateau_mse, na.rm = TRUE),
        sd_plateau_mse = sd(plateau_mse, na.rm = TRUE),
        cv_percent = 100 * sd_plateau_mse / mean_plateau_mse,
        n_runs = 10,
        label = paste0(round(cv_percent, 2), "%"),
        y_label = max(plateau_mse, na.rm = TRUE) + 0.08 * diff(range(plateau_mse, na.rm = TRUE)),
        .groups = "drop"
      )
    
    pB <- ggplot(plateau_run, aes(x = data, y = plateau_mse, fill = data)) +
      geom_boxplot(alpha = 0.65, outlier.shape = NA, width = 0.62) +
      geom_jitter(width = 0.08, size = 1.8, alpha = 0.75) +
      labs(y = paste0("Plateau training and validation\n(mean last ", n_last, " epochs)"),
           title = "B. Run-to-run variability of the validation-loss plateau") +
      theme_bw() +
      theme(
        legend.position = "none",
        strip.background = element_rect(fill = "grey95"),
        panel.grid.minor = element_blank()
      )
    
    fig_stability <- wrap_plots(A = pA, B = pB, design = "AABB")
    
    return(fig_stability)
  }
  
  trainTestFitNet <- function(netInputs, depthTest) {
    
    species = substr(colnames(netInputs)[grep("_N", names(netInputs))], 1, 3)
    age = substr(colnames(netInputs)[grep("_N", names(netInputs))], 5, 5)
    s_a = data.frame(species, age)
    s_a_min = s_a %>% group_by(species) %>% slice_min(age)
    s_a = paste(s_a$species, s_a$age)
    s_a_min = paste(s_a_min$species, s_a_min$age)
    
    min_age_pos = vector()
    for (i in 1:length(s_a_min)) {
      min_age_pos[i] = grep(s_a_min[i], s_a)
    }
    
    min_age_vec = vector()
    for (i in 1:length(unique(species))) {
      min_age_vec[i] = min(substr(colnames(netInputs[which(substr(colnames(netInputs), 1, 3) == unique(species)[i])]), 5, 5))
    }
    
    env_var <- length(grep("PrP_", names(netInputs)))
    
    gsa <- c()
    
    for (i in 2:(ncol(netInputs)/2 + 1)) {
      gsa <- append(gsa, strsplit(names(netInputs), "N_")[[i]][2])
    }
    
    if (env_var > 0) {
      envMult <- randomEnvVarSum(as.integer(input$depthPred), as.integer(input$envParamMult))
    }
    
    niter <- 10
    
    withProgress(message = "Calculating...", value = 0, detail = "0%", {
      
      for (iter in 1:niter) {
        
        norm_inputs = netInputs
        norm_inputs[,2:ncol(norm_inputs)] = normalizeInputs(norm_inputs[,2:ncol(norm_inputs)], range_inputs)
        if (input$activation == "tanh") {
          norm_inputs[,2:ncol(norm_inputs)] <- norm_inputs[,2:ncol(norm_inputs)] - 0.5
        }
        val_df <- as.matrix(norm_inputs[, 2:ncol(norm_inputs)])
        iter_df <- norm_inputs[1:(nrow(val_df) - depthTest),]
    
        for (i in 1:depthTest) {
          
          inputs <- layer_input(shape = c((nrow(iter_df)), (ncol(iter_df) - 1)))
          outputs <- inputs
          inputs_df <- as.matrix(iter_df[, 2:ncol(iter_df)])
          outputs_df <- as.matrix(iter_df[, 2:ncol(iter_df)])
          
          if (as.integer(input$nLayers) >= 1) {outputs <- buildNet(outputs, 1, input$layerType1, input$neurons1, input$returnSeq1,
                                                                   input$dropout1, input$activation, input$recdropout1, input$recactivation)}
          if (as.integer(input$nLayers) >= 2) {outputs <- buildNet(outputs, 2, input$layerType2, input$neurons2, input$returnSeq2,
                                                                   input$dropout2, input$activation, input$recdropout2, input$recactivation)}
          if (as.integer(input$nLayers) >= 3) {outputs <- buildNet(outputs, 3, input$layerType3, input$neurons3, input$returnSeq3,
                                                                   input$dropout3, input$activation, input$recdropout3, input$recactivation)}
          if (as.integer(input$nLayers) >= 4) {outputs <- buildNet(outputs, 4, input$layerType4, input$neurons4, input$returnSeq4,
                                                                   input$dropout4, input$activation, input$recdropout4, input$recactivation)}
          if (as.integer(input$nLayers) >= 5) {outputs <- buildNet(outputs, 5, input$layerType5, input$neurons5, input$returnSeq5,
                                                                   input$dropout5, input$activation, input$recdropout5, input$recactivation)}
          
          dummy <- timeseries_dataset_from_array(
            inputs_df, outputs_df,
            sequence_length = as.numeric(nrow(iter_df)),
            batch_size = 2
          )
          
          dummy_val <- timeseries_dataset_from_array(
            val_df, val_df,
            sequence_length = 4,
            batch_size = 2
          )
          
          model <- keras_model(inputs, outputs)
          
          callbacks <- list(
            callback_early_stopping(
              monitor = "loss", patience = 15),
            callback_model_checkpoint(
              "callback.keras", save_best_only = TRUE))
          
          model %>% compile(
            loss = loss_mean_squared_error(),
            metrics = loss_mean_absolute_error(),
            optimizer_rmsprop(learning_rate = as.numeric(input$learnParam))
          )
          
          history <- model %>% fit(
            dummy,
            validation_data = dummy_val,
            verbose = 0,
            epochs = as.integer(input$nEpochs),
            callbacks = callbacks
          )
          
          if (i == 1) {
            history_df <- na.omit(as.data.frame(history))
            levels(history_df$metric)[match("loss", levels(history_df$metric))] <- "MSE"
            levels(history_df$metric)[levels(history_df$metric) == "mean_absolute_error"] <- "MAE"
            history_df$iter <- iter
            if (iter == 1) {
              history_df_tot <- history_df
              save_model(model, "my_model.keras")
            } else {
              history_df_tot <- rbind(history_df_tot, history_df)
            }
          }
          
          traintest_metrics <<- history_df_tot
          traintest_nparams <<- model$count_params()
          
          if (input$activation == "tanh") {
            iter_df[,2:ncol(iter_df)] <- iter_df[,2:ncol(iter_df)] + 0.5
          }
          iter_df[,2:ncol(iter_df)] <- denormalizeInputs(iter_df[,2:ncol(iter_df)], range_inputs)
          
          pred_vec <- model %>% predict(dummy)
          pred_vec <- t(as.matrix(pred_vec[1:as.integer(length(pred_vec)/2)]))
          colnames(pred_vec) = colnames(iter_df)[2:(ncol(iter_df)/2 + 1)]
          if (input$activation == "tanh") {
            pred_vec <- pred_vec + 0.5
          }
          pred_vec <- denormalizeInputs(pred_vec, range_inputs[1:length(pred_vec)])
          pred_catch <- netInputs[(nrow(iter_df) + 1), grep("_C", colnames(netInputs))]
          pred_vec <- cbind(pred_vec, pred_catch)
          
          if (env_var == 1) {
            pred_vec = cbind(pred_vec, mean(iter_df[(nrow(iter_df)):nrow(iter_df), grep("PrP_", names(iter_df))]))
          } else if (env_var > 1) {
            pred_vec = cbind(pred_vec, t(colMeans(iter_df[(nrow(iter_df)):nrow(iter_df), grep("PrP_", names(iter_df))])))
          }
          
          colnames(pred_vec) = colnames(iter_df)[2:ncol(iter_df)]
          
          iter_df[(nrow(iter_df) + 1),] <- cbind(as.integer(iter_df$year[nrow(iter_df)] + 1), pred_vec)
          
          if (i != depthTest) {
            iter_df[,2:ncol(iter_df)] <- normalizeInputs(iter_df[,2:ncol(iter_df)], range_inputs)
            if (input$activation == "tanh") {
              iter_df[,2:ncol(iter_df)] <- iter_df[,2:ncol(iter_df)] - 0.5
            }
          }
          
          incProgress(amount = 1/(depthTest * niter), detail = paste0(as.character(round((i + (iter - 1) * depthTest)/(depthTest * niter) * 100, 2)), "%"))
          
        }
      
      iter_df <- data.frame(year = rep(seq(min(iter_df$year), max(iter_df$year), 1),
                                       ncol(pred_vec)/2),
                            N = as.numeric(data.matrix(iter_df[, grep("_N", names(iter_df))])),
                            species = rep(species, each = nrow(iter_df)),
                            gsa = rep(gsa, each = nrow(iter_df)),
                            age = as.numeric(rep(age, each = nrow(iter_df))),
                            type = "Predicted",
                            iter = iter
      )
      
      obs_df <- data.frame(year = rep(seq(min(netInputs$year), max(netInputs$year), 1),
                                       ncol(pred_vec)/2),
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
      
      for (i in 1:length(unique(species))) {
        if (i == 1) {
          recr_iter = def_df[which((def_df$species == unique(species)[i]) & (def_df$age == min_age_vec[i])),]
        } else {
          recr_iter_sp = def_df[which((def_df$species == unique(species)[i]) & (def_df$age == min_age_vec[i])),]
          recr_iter = rbind (recr_iter, recr_iter_sp)
        }
      }
      recr_iter = recr_iter[order(recr_iter$species),]
  
      # Create total biomass dataframe
      
      ssb_df_convert <- data.frame()
      ssb_iter <- def_df[which(def_df$age > 0),]
      ssb_species <- unique(ssb_iter$species)
      
      for (sp in 1:length(ssb_species)) {
        ssb_iter_sub <- ssb_iter[which(ssb_iter$species == ssb_species[sp]),]
        ssb_year <- sort(unique(ssb_iter_sub$year))
        ssb_age <- sort(unique(ssb_iter_sub$age))
        
        for (y in 1:nrow(ssb_iter_sub)) {
          for(a in 1:length(ssb_age)) {
            ssb_iter_sub[which(ssb_iter_sub$year == ssb_year[y] & ssb_iter_sub$age == ssb_age[a]), "N"] <- (ssb_iter_sub[which(ssb_iter_sub$year == ssb_year[y] & ssb_iter_sub$age == ssb_age[a]), "N"] * exp(-(fmort_l[which(fmort_l$year == ssb_year[y] & fmort_l$age == ssb_age[a] & fmort_l$species == unique(ssb_iter_sub$species)), "fmort"] * fmort_spawn_l[which(fmort_spawn_l$year == ssb_year[y] & fmort_spawn_l$age == ssb_age[a] & fmort_spawn_l$species == unique(ssb_iter_sub$species)), "fmort_spawn"] + mort_l[which(mort_l$year == ssb_year[y] & mort_l$age == ssb_age[a] & mort_l$species == unique(ssb_iter_sub$species)), "mort"] * mort_spawn_l[which(mort_spawn_l$year == ssb_year[y] & mort_spawn_l$age == ssb_age[a] & mort_spawn_l$species == unique(ssb_iter_sub$species)), "mort_spawn"])) * waa_l[which(waa_l$year == ssb_year[y] & waa_l$age == ssb_age[a] & waa_l$species == unique(ssb_iter_sub$species)), "weight_at_age"] * mature_l[which(mature_l$year == ssb_year[y] & mature_l$age == ssb_age[a] & mature_l$species == unique(ssb_iter_sub$species)), "mature"])/1000
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
    for (sp in 1:length(unique(species))) {
      traintest_iter_results[[sp]] <<- ssb_df_tot[which(ssb_df_tot$species == unique(species)[sp]),]
    }
    
    proj_biomass_spec <- list()
    
    if (length(unique(species)) == 1) {
      
      sp_biomass_sub <- ssb_df_tot
      sp_biomass_wide <- data.frame(year = sp_biomass_sub$year[1:(nrow(sp_biomass_sub)/(niter * 2))],
                                    species = sp_biomass_sub$species[1:(nrow(sp_biomass_sub)/(niter * 2))],
                                    gsa = sp_biomass_sub$gsa[1:(nrow(sp_biomass_sub)/(niter * 2))],
                                    ssb_obs = sp_biomass_sub$ssb[which(sp_biomass_sub$type == "Observed" & sp_biomass_sub$iter == 1)],
                                    ssb_min = NA, ssb_mean = NA, ssb_max = NA,
                                    recr_min = NA, recr_mean = NA, recr_max = NA,
                                    rmse_min = NA, rmse_mean = NA, rmse_max = NA,
                                    mae_min = NA, mae_mean = NA, mae_max = NA)

      for (i in 1:nrow(sp_biomass_wide)) {
        sp_biomass_wide[i, "ssb_min"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i] & sp_biomass_sub$type == "Predicted"), "ssb"])[3]
        sp_biomass_wide[i, "ssb_mean"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i] & sp_biomass_sub$type == "Predicted"), "ssb"])[6]
        sp_biomass_wide[i, "ssb_max"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i] & sp_biomass_sub$type == "Predicted"), "ssb"])[8]
        sp_biomass_wide[i, "recr_min"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i] & sp_biomass_sub$type == "Predicted"), "recruitment"])[3]
        sp_biomass_wide[i, "recr_mean"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i] & sp_biomass_sub$type == "Predicted"), "recruitment"])[6]
        sp_biomass_wide[i, "recr_max"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i] & sp_biomass_sub$type == "Predicted"), "recruitment"])[8]
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
      
      for (sp in 1:length(unique(species))) {
        
        sp_biomass_sub <- ssb_df_tot[which(ssb_df_tot$species == unique(species)[sp]),]
        sp_biomass_wide <- data.frame(year = sp_biomass_sub$year[1:(nrow(sp_biomass_sub)/(niter * 2))],
                                      species = sp_biomass_sub$species[1:(nrow(sp_biomass_sub)/(niter * 2))],
                                      gsa = sp_biomass_sub$gsa[1:(nrow(sp_biomass_sub)/(niter * 2))],
                                      ssb_obs = sp_biomass_sub$ssb[which(sp_biomass_sub$type == "Observed" & sp_biomass_sub$iter == 1)],
                                      ssb_min = NA, ssb_mean = NA, ssb_max = NA,
                                      recr_min = NA, recr_mean = NA, recr_max = NA,
                                      rmse_min = NA, rmse_mean = NA, rmse_max = NA,
                                      mae_min = NA, mae_mean = NA, mae_max = NA)
        
        for (i in 1:nrow(sp_biomass_wide)) {
          sp_biomass_wide[i, "ssb_min"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i] & sp_biomass_sub$type == "Predicted"), "ssb"])[3]
          sp_biomass_wide[i, "ssb_mean"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i] & sp_biomass_sub$type == "Predicted"), "ssb"])[6]
          sp_biomass_wide[i, "ssb_max"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i] & sp_biomass_sub$type == "Predicted"), "ssb"])[8]
          sp_biomass_wide[i, "recr_min"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i] & sp_biomass_sub$type == "Predicted"), "recruitment"])[3]
          sp_biomass_wide[i, "recr_mean"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i] & sp_biomass_sub$type == "Predicted"), "recruitment"])[6]
          sp_biomass_wide[i, "recr_max"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i] & sp_biomass_sub$type == "Predicted"), "recruitment"])[8]
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
    
    year = as.integer(max(unique(proj_biomass$year))) - (depthTest + 1)
    
    line_obs <- as.data.frame(spline(x = proj_biomass$year,
                                     y = proj_biomass$ssb_obs,
                                     xout = seq(min(proj_biomass$year), max(proj_biomass$year), by = 0.25)))

    line_pred_min <- as.data.frame(spline(x = proj_biomass[which(proj_biomass$year > year), "year"],
                                          y = proj_biomass[which(proj_biomass$year > year), "ssb_min"],
                                          xout = seq((year + 1), max(proj_biomass$year), by = 0.25)))

    line_pred_mean <- as.data.frame(spline(x = proj_biomass[which(proj_biomass$year > year), "year"],
                                           y = proj_biomass[which(proj_biomass$year > year), "ssb_mean"],
                                           xout = seq((year + 1), max(proj_biomass$year), by = 0.25)))

    line_pred_max <- as.data.frame(spline(x = proj_biomass[which(proj_biomass$year > year), "year"],
                                          y = proj_biomass[which(proj_biomass$year > year), "ssb_max"],
                                          xout = seq((year + 1), max(proj_biomass$year), by = 0.25)))
    
    g = ggplot(data = proj_biomass, aes(x = year)) +
      geom_ribbon(data = line_pred_min, aes(x = x, ymin = y, ymax = line_pred_max$y,
                                            ),
                  fill = "firebrick", alpha = 0.5) +
      geom_line(data = line_pred_mean, aes(x = x, y = y), color = "red", linewidth = 2) +
      geom_line(data = line_obs, aes(x = x, y = y), color = "black", linewidth = 2) +
      scale_x_continuous(breaks = sort(unique(proj_biomass$year))) +
      ggtitle(paste0(unique(proj_biomass$species), " - ", unique(proj_biomass$gsa))) +
      xlab("Year") +
      ylab("SSB (tonnes)") +
      theme_test() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "bottom")
    
    return(g)
  }
  
  plotRecruitment <- function(proj_biomass, plotPredCount, depth) {
    
    df_recruitment <- proj_biomass[, c(1:3, 8:10)]
    df_recruitment_l <- df_recruitment[(1:(nrow(df_recruitment) - depth)), 1:4]
    colnames(df_recruitment_l) <- c("year", "species", "gsa", "recruitment")
    df_recruitment_l$variable <- "recr_ref"
    
    df_recruitment_pred <- df_recruitment[-(1:(nrow(df_recruitment) - depth)),]
    df_recruitment_pred <- melt(df_recruitment_pred, id.vars = c("year", "species", "gsa"), value.name = "recruitment") %>% relocate(recruitment, .after = gsa)
    
    df_plot <- rbind(df_recruitment_l, df_recruitment_pred) %>%
      mutate(variable = gsub("recr_ref", "Reference", variable)) %>%
      mutate(variable = gsub("recr_min", "Min Recruitment", variable)) %>%
      mutate(variable = gsub("recr_mean", "Mean Recruitment", variable)) %>%
      mutate(variable = gsub("recr_max", "Max Recruitment", variable))
    
    colnames(df_plot) <- c("Year", "Species", "GSA", "Recruitment", "Type")
    
    recr_plot <- ggplot(df_plot, aes(x = Recruitment/1000000, y = Recruitment/1000000, fill = Type, shape = Type)) +
      geom_jitter(size = 4, alpha = 0.75, width = 0.8, height = 0.8) +
      scale_shape_manual(values = c(23, 23, 23, 21)) +
      scale_fill_manual(values = c("firebrick1", "firebrick3", "firebrick4", "blue")) +
      ggtitle(paste0(unique(df_plot$Species), " - ", unique(df_plot$GSA))) +
      xlab("Recruitment (millions)") +
      ylab("Recruitment (millions)") +
      theme_test() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "bottom")
    
    return(recr_plot)
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
    
    taylorDiagram <- function(dati, col = "red", pch = 19,
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
        scale_colour_manual(values = c("red", "black"), labels = c("Iterations", "Mean")) +
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
  
  predNet <- function(netInputs, f_adj, depth) {
    
    norm_inputs = netInputs
    norm_inputs[,2:ncol(norm_inputs)] = normalizeInputs(norm_inputs[, 2:ncol(norm_inputs)], range_inputs)
    if (input$activation == "tanh") {
      norm_inputs[,2:ncol(norm_inputs)] <- norm_inputs[,2:ncol(norm_inputs)] - 0.5
    }
    
    species = substr(colnames(netInputs)[grep("_N", names(netInputs))], 1, 3)
    age = substr(colnames(netInputs)[grep("_N", names(netInputs))], 5, 5)
    
    if (min(age) == 1) {
      age = as.character(as.integer(age) - 1)
    }
    
    s_a = data.frame(species, age)
    s_a_min = s_a %>% group_by(species) %>% slice_min(age)
    s_a = paste(s_a$species, s_a$age)
    s_a_min = paste(s_a_min$species, s_a_min$age)
    
    min_age_pos = vector()
    for (i in 1:length(s_a_min)) {
      min_age_pos[i] = grep(s_a_min[i], s_a)
    }
    
    min_age_vec = vector()
    for (i in 1:length(unique(species))) {
      min_age_vec[i] = min(as.integer(age))
    }
    
    env_var <- length(grep("PrP_", names(netInputs)))
    
    gsa <- c()
    
    for (i in 2:(ncol(netInputs)/2 + 1)) {
      gsa <- append(gsa, strsplit(names(netInputs), "N_")[[i]][2])
    }
    
    proj_df <- data.frame()
    tot_df <- data.frame()
    
    if (env_var > 0) {
      envMult <- randomEnvVarSum(as.integer(input$depthPred), as.integer(input$envParamMult))
    }
    
    withProgress(message = "Calculating...", value = 0, detail = "0%", {
    
      for (iter in 1:3) {
        
        proj <- norm_inputs
      
        for (i in 1:input$depthPred) {
          train_df <- as.matrix(proj[1:(7 + i), 2:ncol(proj)])
          val_df <- as.matrix(proj[(8 + i):nrow(proj), 2:ncol(proj)])
          
          inputs <- layer_input(shape = c((nrow(train_df) - 1), (ncol(proj) - 1)))
          inputs_df <- train_df[1:(nrow(train_df) - 1),]
          outputs <- inputs
          outputs_df <- train_df[2:(nrow(train_df)),]
  
          n_layers <- as.integer(input$nLayers)
  
          dummy <- timeseries_dataset_from_array(
            inputs_df, outputs_df,
            sequence_length = 6 + i,
            batch_size = 2
          )
  
          dummy_val <- timeseries_dataset_from_array(
            val_df, val_df,
            sequence_length = 4,
            batch_size = 2
          )
  
          model <- keras_model_sequential()
          
          if (as.integer(input$nLayers) >= 1) {model <- buildNet(model, 1, input$layerType1, input$neurons1, input$returnSeq1,
                                                                   input$dropout1, input$activation, input$recdropout1, input$recactivation)}
          if (as.integer(input$nLayers) >= 2) {model <- buildNet(model, 2, input$layerType2, input$neurons2, input$returnSeq2,
                                                                   input$dropout2, input$activation, input$recdropout2, input$recactivation)}
          if (as.integer(input$nLayers) >= 3) {model <- buildNet(model, 3, input$layerType3, input$neurons3, input$returnSeq3,
                                                                   input$dropout3, input$activation, input$recdropout3, input$recactivation)}
          if (as.integer(input$nLayers) >= 4) {model <- buildNet(model, 4, input$layerType4, input$neurons4, input$returnSeq4,
                                                                   input$dropout4, input$activation, input$recdropout4, input$recactivation)}
          if (as.integer(input$nLayers) >= 5) {model <- buildNet(model, 5, input$layerType5, input$neurons5, input$returnSeq5,
                                                                   input$dropout5, input$activation, input$recdropout5, input$recactivation)}
    
          callbacks <- list(
            callback_early_stopping(
              monitor = "loss", patience = 10),
            callback_model_checkpoint(
              "prova.keras", save_best_only = TRUE))
    
          model %>% compile(
            loss = "mse",
            metrics = "mae",
            optimizer_rmsprop(learning_rate = as.numeric(input$learnParam))
          )
          
          history <- model %>% fit(
            dummy,
            validation_data = dummy_val,
            verbose = 0,
            epochs = as.integer(input$nEpochs),
            callbacks = callbacks
          )
          
          history_df <- as.data.frame(history)
          
          if (input$activation == "tanh") {
            proj[,2:ncol(proj)] <- proj[,2:ncol(proj)] + 0.5
          }
          proj[,2:ncol(proj)] <- denormalizeInputs(proj[,2:ncol(proj)], range_inputs)
          
          pred_vec <- model %>% predict(dummy)
          pred_vec <- t(as.matrix(pred_vec[1:as.integer(length(pred_vec)/2)]))
          colnames(pred_vec) = colnames(proj)[2:(ncol(proj)/2 + 1)]
          if (input$activation == "tanh") {
            pred_vec <- pred_vec + 0.5
          }
          pred_vec <- denormalizeInputs(pred_vec, range_inputs[1:length(pred_vec)])
          pred_catch <- catchBaranov(f_adj, mort_w[nrow(mort_w), 2:ncol(mort_w)], pred_vec)
          pred_vec <- cbind(pred_vec, pred_catch)
          
          if (env_var == 1) {
            pred_vec = cbind(pred_vec, mean(proj[(nrow(proj)):nrow(proj), grep("PrP_", names(proj))]))
          } else if (env_var > 1) {
            pred_vec = cbind(pred_vec, t(colMeans(proj[(nrow(proj)):nrow(proj), grep("PrP_", names(proj))])))
          }
          
          colnames(pred_vec) = colnames(proj)[2:ncol(proj)]
          
          proj[(nrow(proj) + 1),] <- cbind(as.integer(proj$year[nrow(proj)] + 1), pred_vec)
          proj[,2:ncol(proj)] <- normalizeInputs(proj[,2:ncol(proj)], range_inputs)
          if (input$activation == "tanh") {
            proj[,2:ncol(proj)] <- proj[,2:ncol(proj)] - 0.5
          }
          
          incProgress(amount = 1/(3 * as.integer(input$depthPred)),
                      detail = paste0(as.character(round(((as.integer(input$depthPred) * (iter - 1) + i)/(3 * as.integer(input$depthPred))) * 100, 2)), "%"))
          
        }
        
        # Denormalize outputs
        if (input$activation == "tanh") {
          proj[,2:ncol(proj)] <- proj[,2:ncol(proj)] + 0.5
        }
        proj[,2:ncol(proj)] <- denormalizeInputs(proj[,2:ncol(proj)], range_inputs)
        pred_output_raw[[iter]] <<- proj
        
        # Wide to long
        proj_df <- data.frame(year = rep(seq(min(netInputs$year),
                                             (max(netInputs$year) + depth), 1),
                                         ncol(pred_vec)/2),
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
        for (i in 1:length(unique(species))) {
          if (i == 1) {
            recr_iter = proj_df[which((proj_df$species == unique(species)[i]) & (proj_df$age == min_age_vec[i])),]
          } else {
            recr_iter_sp = proj_df[which((proj_df$species == unique(species)[i]) & (proj_df$age == min_age_vec[i])),]
            recr_iter = rbind (recr_iter, recr_iter_sp)
          }
        }
        recr_iter = recr_iter[order(recr_iter$species),]
        
        # Create total biomass dataframe
        proj_df_convert <- data.frame()
        ssb_iter <- proj_df[which(proj_df$age > min_age_vec[i]),]
        
        ssb_species <- unique(ssb_iter$species)
        
        for (sp in 1:length(ssb_species)) {
          ssb_iter_sub <- ssb_iter[which(ssb_iter$species == ssb_species[sp]),]
          ssb_year <- sort(unique(ssb_iter_sub$year))
          ssb_age <- sort(unique(ssb_iter_sub$age))
          
          fmort_ssb <- fmort_l[which(fmort_l$species == unique(species)[sp]),]
          fmort_ssb <- rbind(fmort_ssb, transform(fmort_ssb[rep((nrow(fmort_ssb) - ssb_age[length(ssb_age)]):nrow(fmort_ssb), depth),], year = year + rep(seq((1:depth)[1], (1:depth)[depth]), each = (length(ssb_age) + 1))))
          fmort_spawn_ssb <- fmort_spawn_l[which(fmort_spawn_l$species == unique(species)[sp]),]
          fmort_spawn_ssb <- rbind(fmort_spawn_ssb, transform(fmort_spawn_ssb[rep((nrow(fmort_spawn_ssb) - ssb_age[length(ssb_age)]):nrow(fmort_spawn_ssb), depth),], year = year + rep(seq((1:depth)[1], (1:depth)[depth]), each = (length(ssb_age) + 1))))
          mort_ssb <- mort_l[which(mort_l$species == unique(species)[sp]),]
          mort_ssb <- rbind(mort_ssb, transform(mort_ssb[rep((nrow(mort_ssb) - ssb_age[length(ssb_age)]):nrow(mort_ssb), depth),], year = year + rep(seq((1:depth)[1], (1:depth)[depth]), each = (length(ssb_age) + 1))))
          mort_spawn_ssb <- mort_spawn_l[which(mort_spawn_l$species == unique(species)[sp]),]
          mort_spawn_ssb <- rbind(mort_spawn_ssb, transform(mort_spawn_ssb[rep((nrow(mort_spawn_ssb) - ssb_age[length(ssb_age)]):nrow(mort_spawn_ssb), depth),], year = year + rep(seq((1:depth)[1], (1:depth)[depth]), each = (length(ssb_age) + 1))))
          waa_ssb <- waa_l[which(waa_l$species == unique(species)[sp]),]
          waa_ssb <- rbind(waa_ssb, transform(waa_ssb[rep((nrow(waa_ssb) - ssb_age[length(ssb_age)]):nrow(waa_ssb), depth),], year = year + rep(seq((1:depth)[1], (1:depth)[depth]), each = (length(ssb_age) + 1))))
          mature_ssb <- mature_l[which(mature_l$species == unique(species)[sp]),]
          mature_ssb <- rbind(mature_ssb, transform(mature_ssb[rep((nrow(mature_ssb) - ssb_age[length(ssb_age)]):nrow(mature_ssb), depth),], year = year + rep(seq((1:depth)[1], (1:depth)[depth]), each = (length(ssb_age) + 1))))
          
          for (y in 1:nrow(ssb_iter_sub)) {
            for(a in 1:length(ssb_age)) {
              ssb_iter_sub[which(ssb_iter_sub$year == ssb_year[y] & ssb_iter_sub$age == ssb_age[a]), "N"] <- (ssb_iter_sub[which(ssb_iter_sub$year == ssb_year[y] & ssb_iter_sub$age == ssb_age[a]), "N"] * exp(-(fmort_ssb[which(fmort_ssb$year == ssb_year[y] & fmort_ssb$age == ssb_age[a]), "fmort"] * fmort_spawn_ssb[which(fmort_spawn_ssb$year == ssb_year[y] & fmort_spawn_ssb$age == ssb_age[a]), "fmort_spawn"] + mort_ssb[which(mort_ssb$year == ssb_year[y] & mort_ssb$age == ssb_age[a]), "mort"] * mort_spawn_ssb[which(mort_spawn_ssb$year == ssb_year[y] & mort_spawn_ssb$age == ssb_age[a]), "mort_spawn"])) * waa_ssb[which(waa_ssb$year == ssb_year[y] & waa_ssb$age == ssb_age[a]), "weight_at_age"] * mature_ssb[which(mature_ssb$year == ssb_year[y] & mature_ssb$age == ssb_age[a]), "mature"])/1000
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
          
          model_pred[[iter]] <<- model
        
        }
      })
    
    pred_iter_partial <<- ssb_df_tot
    
    proj_biomass_spec <- list()
    
    if (length(unique(species)) == 1) {
      
      sp_biomass_sub <- ssb_df_tot
      sp_biomass_wide <- data.frame(year = sp_biomass_sub$year[1:(nrow(sp_biomass_sub)/3)],
                                    species = sp_biomass_sub$species[1:(nrow(sp_biomass_sub)/3)],
                                    gsa = sp_biomass_sub$gsa[1:(nrow(sp_biomass_sub)/3)],
                                    type = sp_biomass_sub$type[1:(nrow(sp_biomass_sub)/3)],
                                    ssb_min = NA, ssb_mean = NA, ssb_max = NA,
                                    recr_min = NA, recr_mean = NA, recr_max = NA)
      
      for (i in 1:nrow(sp_biomass_wide)) {
        sp_biomass_wide[i, "ssb_min"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i]), "ssb"])[1]
        sp_biomass_wide[i, "ssb_mean"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i]), "ssb"])[2]
        sp_biomass_wide[i, "ssb_max"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i]), "ssb"])[3]
        sp_biomass_wide[i, "recr_min"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i]), "recruitment"])[1]
        sp_biomass_wide[i, "recr_mean"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i]), "recruitment"])[2]
        sp_biomass_wide[i, "recr_max"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i]), "recruitment"])[3]
      }
      
      proj_biomass_spec[[1]] <- sp_biomass_wide
      
    } else {
      
      for (j in 1:length(unique(species))) {
        
        sp_biomass_sub <- ssb_df_tot[which(ssb_df_tot$species == unique(species)[j]), ]
        sp_biomass_wide <- data.frame(year = sp_biomass_sub$year[1:(nrow(sp_biomass_sub)/3)],
                                      species = sp_biomass_sub$species[1:(nrow(sp_biomass_sub)/3)],
                                      gsa = sp_biomass_sub$gsa[1:(nrow(sp_biomass_sub)/3)],
                                      type = sp_biomass_sub$type[1:(nrow(sp_biomass_sub)/3)],
                                      ssb_min = NA, ssb_mean = NA, ssb_max = NA,
                                      recr_min = NA, recr_mean = NA, recr_max = NA)
        
        for (i in 1:nrow(sp_biomass_wide)) {
          sp_biomass_wide[i, "ssb_min"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i]), "ssb"])[1]
          sp_biomass_wide[i, "ssb_mean"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i]), "ssb"])[2]
          sp_biomass_wide[i, "ssb_max"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i]), "ssb"])[3]
          sp_biomass_wide[i, "recr_min"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i]), "recruitment"])[1]
          sp_biomass_wide[i, "recr_mean"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i]), "recruitment"])[2]
          sp_biomass_wide[i, "recr_max"] <- sort(sp_biomass_sub[which(sp_biomass_sub$year == sp_biomass_wide$year[i]), "recruitment"])[3]
        }
        
        proj_biomass_spec[[j]] <- sp_biomass_wide
        
      }
      
    }
    
    return(proj_biomass_spec)
  }
  
  plotPred <- function(proj_biomass, netInputs, plotPredCount) {
    
    proj_biomass_def <- proj_biomass[which(proj_biomass$year > netInputs$year[(nrow(netInputs) - 10)]),]
    proj_biomass_def[, 5:10] <- proj_biomass_def[, 5:10]
    
    spline_mean <- as.data.frame(spline(x = proj_biomass_def$year, y = proj_biomass_def$ssb_mean))
    spline_mean$type <- NA
    spline_mean[which(as.numeric(spline_mean$x) < proj_biomass_def[which(proj_biomass_def$type == "Forecast")[1], "year"]), "type"] <- "Observed"
    spline_mean[which(is.na(spline_mean$type)), "type"] <- "Predicted"
    
    spline_min <- as.data.frame(spline(x = proj_biomass_def$year, y = proj_biomass_def$ssb_min))
    spline_min$which <- "min"
    spline_min$type <- NA
    spline_min[which(as.numeric(spline_min$x) < proj_biomass_def[which(proj_biomass_def$type == "Forecast")[1], "year"]), "type"] <- "Observed"
    spline_min[which(is.na(spline_min$type)), "type"] <- "Predicted"
    
    spline_max <- as.data.frame(spline(x = proj_biomass_def$year, y = proj_biomass_def$ssb_max))
    spline_max$which <- "max"
    spline_max$type <- NA
    spline_max[which(as.numeric(spline_max$x) < proj_biomass_def[which(proj_biomass_def$type == "Forecast")[1], "year"]), "type"] <- "Observed"
    spline_max[which(is.na(spline_max$type)), "type"] <- "Predicted"
    
    g <- ggplot(data = proj_biomass_def) +
      geom_ribbon(data = spline_min, aes(x = x, ymin = y, ymax = spline_max$y,
                                         ),
                  fill = "firebrick", alpha = 0.5) +
      geom_line(data = spline_mean, aes(x = x, y = y, color = type,
                                        ),
                linewidth = 2) +
      scale_x_continuous(breaks = sort(unique(proj_biomass_def$year))) +
      scale_color_manual(name = "Forecast", values = c("black", "red")) +
      ggtitle(paste0(unique(proj_biomass_def$species), " - ", unique(proj_biomass_def$gsa))) +
      xlab("Years") +
      ylab("SSB (tonnes)") +
      theme_test() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "bottom")
    
    return(g)
  }
  
  sensAnalysis <- function(netInputs, pred_results, model_pred) {
    
    m_num <- c(1, 2, 3)
    
    norm_inputs = netInputs
    norm_inputs[,2:ncol(norm_inputs)] = normalizeInputs(norm_inputs[,2:ncol(norm_inputs)], range_inputs)
    
    pert = seq(0.1, 0.5, by = 0.1)
    inputs = as.matrix(norm_inputs[1:(nrow(norm_inputs) - 1), 2:ncol(norm_inputs)])
    outputs = as.matrix(norm_inputs[2:nrow(norm_inputs), 2:ncol(norm_inputs)])
    input_vars = colnames(inputs)
    output_vars = colnames(outputs)
    
    dummy <- timeseries_dataset_from_array(
      inputs, outputs,
      sequence_length = 6,
      batch_size = 2
    )
    
    stab = expand.grid(input_vars, m_num, pert, output_vars)
    stab$delta = 0
    
    withProgress(message = "Calculating...", value = 0, detail = "0%", {
      
      for (m in 1:length(m_num)) {
        model = model_pred[[m]]
        
        for(i in 1:(ncol(inputs))) {
          
          for(j in 1:length(pert)) {
            y = predict(model, dummy)
            
            inputs_noise_pos <- inputs
            inputs_noise_pos[,i] <- inputs[,i] + pert[j]
            dummy_pos <- timeseries_dataset_from_array(
              inputs_noise_pos, outputs,
              sequence_length = 6,
              batch_size = 2
            )
            y_noise_p = predict(model, dummy_pos)
            
            inputs_noise_neg <- inputs
            inputs_noise_neg[,i] = inputs[,i] - pert[j]
            dummy_neg <- timeseries_dataset_from_array(
              inputs_noise_neg, outputs,
              sequence_length = 6,
              batch_size = 2
            )
            y_noise_n = predict(model, dummy_neg)
            
            ref = outputs
            
            mse = sqrt((y[1,] - ref[nrow(ref),])^2)
            mse_p = sqrt((y_noise_p[1,] - ref[nrow(ref),])^2)
            mse_n = sqrt((y_noise_n[1,] - ref[nrow(ref),])^2)
            
            stab$delta[which((pert[j] == stab$Var3) & (m_num[m] == stab$Var2) & (colnames(inputs)[i] == stab$Var1))] = 100*abs(mse - ((mse_p + mse_n)/2))/mse
          }
          incProgress(amount = 1/(ncol(inputs) * 3),
                      detail = paste0(as.character(round(((ncol(inputs) * (m - 1) + i)/(ncol(inputs) * 3)) * 100, 2)), "%"))
        }
      }
    })
    
    colnames(stab) <- c("Variable", "Model", "Perturbance", "Output", "Delta")
    stab$Variable = as.character(stab$Variable)
    
    stab$Input_Species = substr(stab$Variable, 1, 3)
    stab$Input_Var = substr(stab$Variable, 5, 6)
    
    stab$Output_Species = substr(stab$Output, 1, 3)
    stab$Output_Var = substr(stab$Output, 5, 6)
    
    
    stab = stab[,c("Perturbance", "Delta", "Input_Species", 
                   "Input_Var", "Output_Species", "Output_Var")]
    
    stab$Delta = as.numeric(stab$Delta)
    stab_def = aggregate(data = stab,
                         Delta ~ Input_Species + Output_Species + Perturbance,
                         FUN = "mean")
    
    return(stab_def)
  }
  
  plotSensAnalysis <- function(sens_results) {
    p = ggplot(data = sens_results, aes(x = Input_Species, y = Delta, fill = Perturbance)) +
      geom_histogram(position = "stack", stat = "identity") +
      ggtitle("Sensitivity Analysis") +
      theme_test() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "bottom") +
      facet_wrap(~ Output_Species)
    
    return(p)
  }
  
  loadInput <- function(l) {
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
    morts <<- l[["morts"]]
    mort_l <<- l[["mort_l"]]
    mort_w <<- l[["mort_w"]]
    neuralNetInputs <<- l[["neuralNetInputs"]]
    f_w <<- l[["f_w"]]
    fmort_baseline <<- l[["fmort_baseline"]]
    f_new <<- l[["f_new"]]
    f_adj <<- l[["f_adj"]]
    f_tot <<- l[["f_tot"]]
    f_adj_display <<- l[["f_adj_display"]]
    depth_test <<- l[["depth_test"]]
    plotTestCount <<- 1
    testfit_results <<- l[["testfit_results"]]
    traintest_output_raw <<- l[["traintest_output_raw"]]
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
                             (cohorts over the selected one will be aggregated to it)<br>
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
                            - The user can download the adjusted Fishing Mortality vector<br>
                            in .rdata format in the last row by clicking the left button,<br>
                            while the button on the right can be used to load a previously<br>
                            exported Fishing Mortality vector."),
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
              accept = "RData"
    )
  })
  
  observe({
    req(input$sobj1)
    stk1 <- if (sub(".*\\.", "\\1", input$sobj1[4]) == "rds") {
      readRDS(as.character(input$sobj1[4]))
      } else {
        loadRData(as.character(input$sobj1[4]))
      }
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
    rv1$tri <- sub("_.*", "", input$sobj1[1])
    rv1$spinfo <- speciesInfo(rv1$tri)
    rv1$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj1[1]), "-")[[1]]))
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
    info = "No Species Selected"
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
                accept = "RData"
                )
    )
  })
  
  observe({
    req(input$sobj2)
    stk2 <- if (sub(".*\\.", "\\1", input$sobj2[4]) == "rds") {
      readRDS(as.character(input$sobj2[4]))
    } else {
      loadRData(as.character(input$sobj2[4]))
    }
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
    rv2$tri <- sub("_.*", "", input$sobj2[1])
    rv2$spinfo <- speciesInfo(rv2$tri)
    rv2$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj2[1]), "-")[[1]]))
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
    info = "No Species Selected"
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
                accept = "RData"
                )
      )
  })
  
  observe({
    req(input$sobj3)
    stk3 <- if (sub(".*\\.", "\\1", input$sobj3[4]) == "rds") {
      readRDS(as.character(input$sobj3[4]))
    } else {
      loadRData(as.character(input$sobj3[4]))
    }
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
    rv3$tri <- sub("_.*", "", input$sobj3[1])
    rv3$spinfo <- speciesInfo(rv3$tri)
    rv3$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj3[1]), "-")[[1]]))
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
    info = "No Species Selected"
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
                accept = "RData"
      )
    )
  })
  
  observe({
    req(input$sobj4)
    stk4 <- if (sub(".*\\.", "\\1", input$sobj4[4]) == "rds") {
      readRDS(as.character(input$sobj4[4]))
    } else {
      loadRData(as.character(input$sobj4[4]))
    }
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
    rv4$tri <- sub("_.*", "", input$sobj4[1])
    rv4$spinfo <- speciesInfo(rv4$tri)
    rv4$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj4[1]), "-")[[1]]))
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
    info = "No Species Selected"
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
                accept = "RData"
      )
    )
  })
  
  observe({
    req(input$sobj5)
    stk5 <- if (sub(".*\\.", "\\1", input$sobj5[4]) == "rds") {
      readRDS(as.character(input$sobj5[4]))
    } else {
      loadRData(as.character(input$sobj5[4]))
    }
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
    rv5$tri <- sub("_.*", "", input$sobj5[1])
    rv5$spinfo <- speciesInfo(rv5$tri)
    rv5$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj5[1]), "-")[[1]]))
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
    info = "No Species Selected"
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
                accept = "RData"
      )
    )
  })
  
  observe({
    req(input$sobj6)
    stk6 <- if (sub(".*\\.", "\\1", input$sobj6[4]) == "rds") {
      readRDS(as.character(input$sobj6[4]))
    } else {
      loadRData(as.character(input$sobj6[4]))
    }
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
    rv6$tri <- sub("_.*", "", input$sobj6[1])
    rv6$spinfo <- speciesInfo(rv6$tri)
    rv6$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj6[1]), "-")[[1]]))
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
    info = "No Species Selected"
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
                accept = "RData"
      )
    )
  })
  
  observe({
    req(input$sobj7)
    stk7 <- if (sub(".*\\.", "\\1", input$sobj7[4]) == "rds") {
      readRDS(as.character(input$sobj7[4]))
    } else {
      loadRData(as.character(input$sobj7[4]))
    }
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
    rv7$tri <- sub("_.*", "", input$sobj7[1])
    rv7$spinfo <- speciesInfo(rv7$tri)
    rv7$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj7[1]), "-")[[1]]))
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
    info = "No Species Selected"
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
                accept = "RData"
      )
    )
  })
  
  observe({
    req(input$sobj8)
    stk8 <- if (sub(".*\\.", "\\1", input$sobj8[4]) == "rds") {
      readRDS(as.character(input$sobj8[4]))
    } else {
      loadRData(as.character(input$sobj8[4]))
    }
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
    rv8$tri <- sub("_.*", "", input$sobj8[1])
    rv8$spinfo <- speciesInfo(rv8$tri)
    rv8$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj8[1]), "-")[[1]]))
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
    info = "No Species Selected"
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
                accept = "RData"
      )
    )
  })
  
  observe({
    req(input$sobj9)
    stk9 <- if (sub(".*\\.", "\\1", input$sobj9[4]) == "rds") {
      readRDS(as.character(input$sobj9[4]))
    } else {
      loadRData(as.character(input$sobj9[4]))
    }
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
    rv9$tri <- sub("_.*", "", input$sobj9[1])
    rv9$spinfo <- speciesInfo(rv9$tri)
    rv9$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj9[1]), "-")[[1]]))
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
    info = "No Species Selected"
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
                accept = "RData"
      )
    )
  })
  
  observe({
    req(input$sobj10)
    stk10 <- if (sub(".*\\.", "\\1", input$sobj10[4]) == "rds") {
      readRDS(as.character(input$sobj10[4]))
    } else {
      loadRData(as.character(input$sobj10[4]))
    }
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
    rv10$tri <- sub("_.*", "", input$sobj10[1])
    rv10$spinfo <- speciesInfo(rv10$tri)
    rv10$gsa <- as.vector(as.integer(strsplit(sub(".*_(.*)\\..*", "\\1", input$sobj10[1]), "-")[[1]]))
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
    info = "No Species Selected"
  })
  
  # Load GSAs and 3A codes
  
  observeEvent(input$loadButton, {
    
    if (!is.null(rv1$gsa)) {gsa[[1]] <<- as.integer(rv1$gsa)}
    if (!is.null(rv2$gsa)) {gsa[[2]] <<- as.integer(rv2$gsa)}
    if (!is.null(rv3$gsa)) {gsa[[3]] <<- as.integer(rv3$gsa)}
    if (!is.null(rv4$gsa)) {gsa[[4]] <<- as.integer(rv4$gsa)}
    if (!is.null(rv5$gsa)) {gsa[[5]] <<- as.integer(rv5$gsa)}
    if (!is.null(rv6$gsa)) {gsa[[6]] <<- as.integer(rv6$gsa)}
    if (!is.null(rv7$gsa)) {gsa[[7]] <<- as.integer(rv7$gsa)}
    if (!is.null(rv8$gsa)) {gsa[[8]] <<- as.integer(rv8$gsa)}
    if (!is.null(rv9$gsa)) {gsa[[9]] <<- as.integer(rv9$gsa)}
    if (!is.null(rv10$gsa)) {gsa[[10]] <<- as.integer(rv10$gsa)}
    
    for (i in 1:length(gsa)) {
      gsa_tot <<- c(gsa_tot, gsa[[i]])
    }
    
    gsa_tot <<- sort(unique(gsa_tot))
    
    if (!is.null(rv1$tri)) {species[[1]] <<- rv1$tri}
    if (!is.null(rv2$tri)) {species[[2]] <<- rv2$tri}
    if (!is.null(rv3$tri)) {species[[3]] <<- rv3$tri}
    if (!is.null(rv4$tri)) {species[[4]] <<- rv4$tri}
    if (!is.null(rv5$tri)) {species[[5]] <<- rv5$tri}
    if (!is.null(rv6$tri)) {species[[6]] <<- rv6$tri}
    if (!is.null(rv7$tri)) {species[[7]] <<- rv7$tri}
    if (!is.null(rv8$tri)) {species[[8]] <<- rv8$tri}
    if (!is.null(rv9$tri)) {species[[9]] <<- rv9$tri}
    if (!is.null(rv10$tri)) {species[[10]] <<- rv10$tri}
    
    rv <<- list()
    if (!is.null(rv1$stk)) {rv[[1]] <<- rv1}
    if (!is.null(rv2$stk)) {rv[[2]] <<- rv2}
    if (!is.null(rv3$stk)) {rv[[3]] <<- rv3}
    if (!is.null(rv4$stk)) {rv[[4]] <<- rv4}
    if (!is.null(rv5$stk)) {rv[[5]] <<- rv5}
    if (!is.null(rv6$stk)) {rv[[6]] <<- rv6}
    if (!is.null(rv7$stk)) {rv[[7]] <<- rv7}
    if (!is.null(rv8$stk)) {rv[[8]] <<- rv8}
    if (!is.null(rv9$stk)) {rv[[9]] <<- rv9}
    if (!is.null(rv10$stk)) {rv[[10]] <<- rv10}
    
  })
  
  # Neural network help icons
  
  output$dropHelp = renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = drop_text
      )
    )
  })
  
  output$recdropHelp = renderUI({
    tags$span(
      tipify(
        icon("fas fa-info-circle"),
        title = recdrop_text
      )
    )
  })
  
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
    
    # Population

    for (i in 1:length(rv)) {
      pops[[i]] <<- procDfLongQuant(rv[[i]]$stk, rv[[i]]$gsa, rv[[i]]$tri, rv[[i]]$minAge, rv[[i]]$baselineAge, rv[[i]]$baselineYear, stock.n, pop)
    }
    
    if (length(pops) > 0) {
      pops_l <<- do.call(totDf, pops)
      pops_w <<- procDfWide(pops_l, pop, N)
      output$uiPop <- renderUI({
        withSpinner(plotlyOutput("plotPop"), type = 3, color.background = "transparent")
      })
      output$plotPop <- renderPlotly({
        plotPopObj <- layout(ggplotly(plotPop(pops_l), tooltip = "y"), hovermode = "x unified")
        return(plotPopObj)
      })
    }
    
    # Catches
    
    for (i in 1:length(rv)) {
      catches[[i]] <<- procDfLongQuant(rv[[i]]$stk, rv[[i]]$gsa, rv[[i]]$tri, rv[[i]]$minAge, rv[[i]]$baselineAge, rv[[i]]$baselineYear, catch.n, catch)
    }
    
    if (length(catches) > 0) {
      catches_l <<- do.call(totDf, catches)
      catches_w <<- procDfWide(catches_l, catch, C)
      output$uiCatch <- renderUI({
        withSpinner(plotlyOutput("plotCatch"), type = 3, color.background = "transparent")
      })
      output$plotCatch <- renderPlotly({
        plotCatchObj <- layout(ggplotly(plotCatch(catches_l), tooltip = "y"), hovermode = "x unified")
        return(plotCatchObj)
      })
    }
    
    # Weight at age
    
    for (i in 1:length(rv)) {
      waa[[i]] <<- procDfLongMult(rv[[i]]$stk, rv[[i]]$gsa, rv[[i]]$tri, rv[[i]]$minAge, rv[[i]]$baselineAge, rv[[i]]$baselineYear, catch.wt, weight_at_age)
    }
    
    if (length(waa) > 0) {
      waa_l <<- do.call(totDf, waa)
      waa_w <<- procDfWide(waa_l, weight_at_age, W)
      output$uiWaa <- renderUI({
        withSpinner(plotlyOutput("plotWaa"), type = 3, color.background = "transparent")
      })
      output$plotWaa <- renderPlotly({
        plotWaaObj <- layout(ggplotly(plotWaa(waa_l, pops_l), tooltip = "y"), hovermode = "x unified")
        return(plotWaaObj)
      })
    }
    
    # Fishing mortality
    
    for (i in 1:length(rv)) {
      fmorts[[i]] <<- procDfLongMult(rv[[i]]$stk, rv[[i]]$gsa, rv[[i]]$tri, rv[[i]]$minAge, rv[[i]]$baselineAge, rv[[i]]$baselineYear, harvest, fmort)
    }
    
    if (length(fmorts) > 0) {
      fmort_l <<- do.call(totDf, fmorts)
      fmort_w <<- procDfWide(fmort_l, fmort, F)
    }
    
    # Fishing mortality of spawners
    
    for (i in 1:length(rv)) {
      fmort_spawns[[i]] <<- procDfLongMult(rv[[i]]$stk, rv[[i]]$gsa, rv[[i]]$tri, rv[[i]]$minAge, rv[[i]]$baselineAge, rv[[i]]$baselineYear, harvest.spwn, fmort_spawn)
    }
    
    if (length(fmort_spawns) > 0) {
      fmort_spawn_l <<- do.call(totDf, fmort_spawns)
      fmort_spawn_w <<- procDfWide(fmort_spawn_l, fmort_spawn, J)
    }
    
    # Natural mortality
    
    for (i in 1:length(rv)) {
      morts[[i]] <<- procDfLongMult(rv[[i]]$stk, rv[[i]]$gsa, rv[[i]]$tri, rv[[i]]$minAge, rv[[i]]$baselineAge, rv[[i]]$baselineYear, m, mort)
    }
    
    if (length(morts) > 0) {
      mort_l <<- do.call(totDf, morts)
      mort_w <<- procDfWide(mort_l, mort, M)
    }
    
    # Natural mortality of spawners
    
    for (i in 1:length(rv)) {
      mort_spawns[[i]] <<- procDfLongMult(rv[[i]]$stk, rv[[i]]$gsa, rv[[i]]$tri, rv[[i]]$minAge, rv[[i]]$baselineAge, rv[[i]]$baselineYear, m.spwn, mort_spawn)
    }
    
    if (length(mort_spawns) > 0) {
      mort_spawn_l <<- do.call(totDf, mort_spawns)
      mort_spawn_w <<- procDfWide(mort_spawn_l, mort_spawn, K)
    }
    
    # Mature ratio
    
    for (i in 1:length(rv)) {
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
        f_curr <- colMeans(f_w[((nrow(f_w) - as.integer(input$baseline)):nrow(f_w)), -1])
        f_vec <- rep(1, length(f_curr))
        f_new <- f_curr * f_vec
        f_new <<- data.frame(as.list(f_new))
        f_adj <<- data.frame(as.list(f_new))
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
      pick = grep(input$pickFmort, colnames(f_adj))
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
    filename = "Adjusted_Fmort.rdata",
    content = function(filename) {
      saveRDS(f_adj, filename)
      }
    )
  
  shinyFileChoose(input, "fileAdjFmort",
                  roots = vol,
                  filetypes = c("rdata", "RData", "rData"))
  
  fadjFilename <- reactive({
    parseFilePaths(vol, input$fileAdjFmort)
  })
  
  loadAdjFmort <- reactive({
    f_adj <<- readRDS(as.character(fadjFilename()[4]))
  })
  
  observeEvent(input$loadAdjFmort, {
    if (length(fadjFilename()) == 5) {
      return(NULL)
    } else {
      loadAdjFmort()
      output$adjustedFmort <- renderTable(f_adj, bordered = TRUE, width = "100%")
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
      
      for (i in 1:as.integer(input$nLayers)) {
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
      testfit_results <<- testFitNet(neuralNetInputs)
      removeModal()
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
    } else {
      showModal(tags$div(id = "modalBackground", modalDialog("", footer = NULL)))
      traintest_results <<- trainTestFitNet(neuralNetInputs, as.integer(depth_test))
      removeModal()
    }
  })
  
  observeEvent(input$plotTrainTestButton, {
    
    if (length(traintest_results) > 0) {
      plotTestCount <<- 1
      
      for (i in 1:length(traintest_results)) {
        traintest_plots[[i]] <<- plotTrainTestFitNet(traintest_results[[i]], i, as.integer(input$depthTest))
        traintest_recr_plots[[i]] <<- plotRecruitment(traintest_results[[i]], i, as.integer(input$depthTest))
        taylor_diagram[[i]] <<- plotTaylorDiagram(traintest_iter_results[[i]])
      }
      traintest_metrics_plot <<- plotFitNet(traintest_metrics)
      output$showSpeciesTest <- renderText({
        species[[plotTestCount]]
      })
      output$plotTrainTest <- renderPlotly({
        ggplotly(traintest_plots[[plotTestCount]])
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
          ggplotly(traintest_plots[[plotTestCount]] + scale_y_continuous(trans = "log10"))
          })
        }
        else {output$plotTrainTest <- renderPlotly({
          ggplotly(traintest_plots[[plotTestCount]])
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
          ggplotly(traintest_plots[[plotTestCount]])
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
          ggplotly(traintest_plots[[plotTestCount]])
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
          ggplotly(traintest_plots[[plotTestCount]])
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
          ggplotly(traintest_plots[[plotTestCount]])
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
    
    depth_pred <<- input$depthPred
    
    if (length(species) == 0) {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: upload one or more stock objects first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    } else if (input$baseline == "") {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: calculate fishing mortality first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    } else if (depth_pred == "") {
      showModal(tags$div(id = "modalWarning",
                         modalDialog("Warning: select the depth first!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    } else {
      showModal(tags$div(id = "modalBackground", modalDialog("", footer = NULL)))
      pred_results <<- predNet(neuralNetInputs, f_adj, as.integer(depth_pred))
      removeModal()
    }
  })
  
  observeEvent(input$plotPredButton, {
    if (length(pred_results) > 0) {
      plotPredCount <<- 1
      
      for (i in 1:length(pred_results)) {
        pred_plots[[i]] <<- plotPred(pred_results[[i]], neuralNetInputs, i)
        pred_recr_plots[[i]] <<- plotRecruitment(pred_results[[i]], i, as.integer(depth_pred))
      }
      
      output$plotPred <- renderPlotly({
        ggplotly(pred_plots[[plotPredCount]])
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
          ggplotly(pred_plots[[plotPredCount]] + scale_y_continuous(trans = "log10"))
          })
        }
      else {output$plotPred <- renderPlotly({
        ggplotly(pred_plots[[plotPredCount]])
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
            ggplotly(pred_plots[[plotPredCount]])
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
            ggplotly(pred_plots[[plotPredCount]])
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
            ggplotly(pred_plots[[plotPredCount]])
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
            ggplotly(pred_plots[[plotPredCount]])
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
  
  vol = getVolumes()()
  
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
          morts = morts,
          mort_l = mort_l,
          mort_w = mort_w,
          neuralNetInputs = neuralNetInputs,
          f_w = f_w,
          fmort_baseline = fmort_baseline,
          f_new = f_new,
          f_adj = f_adj,
          f_tot = f_tot,
          f_adj_display = f_adj_display,
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
        saveRDS(save_list, paste0(dirname(), "/", Sys.Date(), "_", "session.RData"))
        }
    })
  
  shinyFileChoose(input, "file",
                  roots = vol,
                  filetypes = c("rdata", "RData"))
  
  filename <- reactive({
    parseFilePaths(vol, input$file)
    })
  
  observe({
    output$file <- renderText(as.character(filename()[4]))
  })
  
  loadDFS <- reactive({
    l <- readRDS(as.character(filename()[4]))
    loadInput(l)
    })
  
  observeEvent(input$loadWS, {
    if (length(filename()) == 5) {
      return(NULL)
    } else {
      loadDFS()
    }
  })
  
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      output <- rmarkdown::render(input = "maelstrom.Rmd",
                                  output_format = "pdf_document")
      file.rename(output, file)
    })
  
}

return(server)