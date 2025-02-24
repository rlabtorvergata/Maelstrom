##### LIBRARIES #####

{library(bslib)
  library(cowplot)
  library(data.table)
  library(dplyr)
  library(FLCore)
  library(ggplot2)
  library(ggrepel)
  library(gridExtra)
  library(keras)
  library(Metrics)
  library(plotly)
  library(plotrix)
  library(progress)
  library(RColorBrewer)
  library(reshape2)
  library(rmarkdown)
  library(RSNNS)
  library(rsq)
  library(shiny)
  library(shinyBS)
  library(shinycssloaders)
  library(shinyFiles)
  library(shinyWidgets)
  library(tensorflow)
  library(tidyr)
  library(tinytex)
  library(tools)
  library(TSdist)
  }

##### VARIABLES #####

options(max.print = 99999)

species <- list() #species loaded
gsa <- list() #list of gsa per stock loaded
gsa_tot <- vector() #unique gsa loaded
stk1 <- NULL #first stock object
stk2 <- NULL #second stock object
stk3 <- NULL #third stock object
stk4 <- NULL #fourth stock object
stk5 <- NULL #fifth stock object
stk6 <- NULL #sixth stock object
stk7 <- NULL #seventh stock object
stk8 <- NULL #eighth stock object
stk9 <- NULL #ninth stock object
stk10 <- NULL #tenth stock object

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
biomassms <- list() #biomass multiplier processing
biomassm_l <- data.frame() #biomass multiplier processing
biomassm_w <- data.frame() #biomass multiplier processing

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
traintest_iter_results <- data.frame() #traintest results per iteration
traintest_results <- data.frame() #traintest results
traintest_plots <- list() #traintest plots
taylor_diagram <- list() #Taylor diagrams

depth_pred <- NULL #number of years to forecast
plotPredCount <- 0 #forecast species counter
model_pred <- list() #list of models in prediction
pred_iter_partial <- data.frame() #forecast results
pred_results <- list() #forecast results
pred_plots <- list() #forecast plots
ratio_plots <- list() #biomass/recruitment ratio plots

sens_results <- data.frame() #sensitivity analysis results
sens_plots <- list() #sensitivity analysis plot

save_list <- list() #saved files list

##### AMBIENTAL #####

pp <- data.frame(
  year = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
  GSA1 = c(10244.25, 9729.033, 10298.79, 11589.80, 11712.67, 10329.92, 10475.56, 11389.00, 11812.35,  9495.63, 10942.59, 10970.49, 10602.43, 8981.911, 9867.827, 9709.191, 11750.82, 10546.57, 10716.51, 13013.30),
  GSA2 = c(589.6691, 598.0702, 627.7929, 672.2489, 655.5533, 625.0090, 659.8044, 695.9435, 701.2333, 607.9361, 677.6224, 646.7877, 641.1380, 574.1598, 667.7024, 587.1129, 721.7747, 646.6526, 675.8074, 817.3495),
  GSA3 = c(6329.225, 6976.454, 6674.087, 7183.146, 7095.723, 6959.898, 6736.979, 7491.467, 7532.008, 6271.641, 7376.077, 6872.068, 6688.171, 6462.249, 7136.752, 6233.229, 7350.424, 7151.143, 6432.927, 8750.791),
  GSA4 = c(25130.17, 26800.27, 28866.68, 27905.09, 29321.96, 30439.74, 27479.31, 31728.23, 30415.31, 27208.77, 26955.74, 30808.19, 31636.53, 27543.98, 28512.67, 26672.25, 31856.13, 28424.46, 27373.53, 38474.08),
  GSA5 = c(16310.98, 18798.76, 20277.91, 20229.43, 22124.90, 22288.06, 20238.66, 20996.98, 21498.29, 19105.07, 18396.04, 21818.95, 21238.50, 19630.89, 20163.70, 19549.58, 22159.05, 20491.80, 20666.57, 25727.51),
  GSA6 = c(18534.28, 21837.39, 22600.65, 23184.71, 24763.44, 24086.73, 22129.00, 21662.85, 24737.61, 21636.42, 20490.54, 24287.70, 23227.50, 20773.27, 23093.50, 20980.17, 24011.68, 23101.20, 24086.76, 27446.95),
  GSA7 = c(14482.52, 14881.93, 15550.66, 17939.60, 17713.35, 17516.26, 17812.42, 15231.93, 18145.77, 15440.16, 16059.60, 17198.84, 17220.14, 14502.03, 17532.77, 14551.11, 16262.17, 16329.68, 16525.20, 17727.17),
  GSA8 = c(4131.792, 4822.893, 5380.070, 6564.359, 5647.303, 5621.297, 6055.189, 5084.251, 6149.678, 5003.304, 5481.033, 5366.824, 5338.590, 5085.508, 5224.433, 4869.408, 5364.349, 5492.859, 5220.715, 6163.947),
  GSA9 = c(9271.031, 10714.10, 12333.45, 13045.27, 12403.98, 11555.75, 12331.62, 11334.32, 13445.45, 11294.33, 11261.56, 12504.75, 11622.00, 11245.48, 11894.07, 10713.59, 11362.97, 11732.54, 11892.59, 13582.60),
  GSA10 = c(16989.00, 20782.56, 22759.54, 23309.67, 22368.19, 21485.69, 21363.61, 22075.04, 24493.62, 21627.18, 20289.45, 23167.04, 21892.20, 22890.42, 21249.55, 21020.20, 23170.84, 22546.61, 21316.23, 25536.27),
  GSA11 = c(18430.48, 20583.18, 21802.50, 24743.52, 23414.19, 23293.12, 22725.42, 22847.39, 24598.90, 21785.40, 20997.34, 24903.23, 23066.78, 23070.31, 22122.23, 21092.16, 24233.62, 23018.78, 22000.67, 27210.10),
  GSA12 = c(5993.409, 7076.711, 7362.986, 7600.086, 7394.874, 7671.994, 7175.817, 7995.528, 7866.468, 7161.144, 7008.909, 8368.976, 7991.283, 7500.908, 7032.760, 7261.937, 7839.672, 7718.865, 7115.316, 8604.147),
  GSA13 = c(7094.965, 8554.749, 8125.751, 7964.883, 7548.377, 7738.310, 7642.026, 8184.146, 8081.285, 7881.899, 7907.322, 8378.254, 8235.281, 8106.112, 7881.768, 7837.581, 7888.435, 8250.610, 7735.791, 8271.480),
  GSA14 = c(15710.12, 16924.58, 16267.19, 15733.12, 15922.04, 15482.18, 15577.73, 14893.01, 16020.20, 15566.07, 15115.40, 14809.62, 15202.81, 15121.63, 15409.36, 15288.81, 15482.77, 15118.42, 15216.41, 14315.94),
  GSA15 = c(3394.706, 3983.982, 4146.702, 4168.459, 3780.261, 3907.022, 3722.232, 4042.219, 4233.087, 4207.445, 3927.861, 4123.659, 4143.011, 4154.593, 4119.490, 3749.420, 3951.069, 4031.194, 3981.010, 4698.133),
  GSA16 = c(5741.087, 6520.132, 6892.717, 7105.883, 6568.862, 6794.010, 6624.109, 7133.681, 7300.990, 6630.483, 6562.937, 7385.813, 7022.618, 6764.651, 6668.021, 6646.200, 7165.248, 7111.742, 6655.675, 7700.996),
  GSA17 = c(25831.25, 21594.01, 27338.06, 25202.09, 23435.16, 24416.95, 24484.44, 28609.15, 30847.17, 28404.35, 24724.78, 32945.35, 31529.85, 27001.47, 27780.25, 25616.02, 28471.78, 27132.26, 30557.90, 25022.43),
  GSA18 = c(11017.58, 10548.96, 12120.75, 11565.67, 12047.47, 11255.83, 10887.70, 12509.51, 13057.54, 11539.80, 10787.46, 12245.27, 12034.90, 11612.29, 12081.72, 10837.73, 11659.06, 11350.89, 13089.08, 13125.27),
  GSA19 = c(22094.09, 24495.46, 27089.39, 26033.99, 25011.97, 24325.72, 24039.31, 25711.82, 28181.63, 25397.15, 23731.61, 25767.58, 24190.04, 25454.68, 25801.96, 24186.13, 25026.27, 26334.44, 28011.80, 31563.82),
  GSA20 = c(14087.32, 16840.76, 17572.05, 17355.84, 17252.85, 17157.95, 16721.06, 17367.88, 17381.92, 16486.81, 16261.95, 16804.43, 15489.01, 16717.21, 17502.45, 17139.70, 17568.30, 18029.46, 18805.84, 21466.27),
  GSA21 = c(38975.58, 46056.71, 49115.09, 46611.57, 45611.09, 47271.79, 45037.82, 45834.07, 46082.52, 46219.87, 45552.46, 45709.80, 44220.07, 47104.25, 47266.46, 45787.47, 45032.76, 48113.48, 48186.54, 56986.85),
  GSA22 = c(31419.72, 39526.72, 38801.94, 38092.96, 41333.42, 38078.20, 37818.77, 36524.33, 40078.02, 39063.07, 35953.59, 36999.62, 35038.00, 41799.87, 37387.32, 38635.90, 38652.00, 40448.61, 39295.71, 44328.21),
  GSA23 = c(6803.593, 8761.482, 8554.959, 8081.821, 8240.977, 8476.934, 8332.452, 8330.986, 8270.548, 8062.048, 7980.835, 8015.699, 7607.560, 8307.684, 8148.720, 8339.683, 8098.652, 9054.360, 9277.245, 10834.51),
  GSA24 = c(10630.97, 14311.57, 13873.57, 13578.11, 14796.26, 14355.36, 13424.28, 13882.43, 13541.59, 13385.04, 13511.89, 13347.00, 12387.84, 13504.62, 12862.57, 13865.51, 13253.16, 14728.95, 15290.27, 17743.92),
  GSA25 = c(4259.723, 5602.161, 5560.685, 5495.746, 5732.694, 5942.124, 5548.213, 5447.151, 5290.512, 5453.932, 5414.167, 5247.524, 5211.055, 5421.592, 5249.132, 5674.287, 5390.549, 5773.391, 6258.438, 7514.001),
  GSA26 = c(32543.92, 41675.76, 42345.13, 41081.15, 41047.90, 42377.43, 40841.84, 40679.61, 40302.49, 40629.51, 38740.39, 40381.66, 38826.99, 40388.80, 41126.80, 40355.47, 39321.45, 42132.36, 45615.16, 55088.15),
  GSA27 = c(3703.390, 5326.922, 4879.239, 4838.633, 4806.908, 4882.936, 4577.611, 4607.241, 4689.100, 4848.776, 4781.825, 4914.756, 4403.847, 4783.745, 4667.031, 4661.406, 4556.354, 5056.754, 5705.640, 6738.782),
  GSA28 = c(5378.125, 12054.63, 17418.60, 12361.15, 12435.85, 8260.557, 15213.99, 10674.12, 14370.93, 17470.32, 10867.15, 14507.06, 12754.81, 16640.05, 10790.34, 9446.886, 13243.32, 12322.46, 9950.202, 9696.405)
)

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
  
  procDfLongQuant <- function(stock, gsa, tri, minAge, baseline, fun, var) {
    if (minAge == 0) {baseline = baseline + 1}
    stk_temp = fun(stock)
    if (nrow(fun(stock)) > baseline) {
      stk_temp = stk_temp[1:baseline,]
      stk_temp[baseline,] = colSums(fun(stock)[baseline:nrow(fun(stock)), ])
    }
    df_temp = as.data.frame(stk_temp)[, c("year", "age", "data")]
    df = df_temp[, 1:2]
    df[, 3] = df_temp[, 3] * 1000
    df[, 4] = procGSA(paste0(gsa, collapse = "-"))
    df[, 5] = tri
    df[, 6] = paste(tri, paste(paste0(gsa, collapse = "-"), collapse = "_"), sep = "_")
    colnames(df) <- c("year", "age", as.character(substitute(var)), "gsa", "species", "tri_gsa")
    return(df)
  }
  
  procDfLongMult <- function(stock, gsa, tri, minAge, baseline, fun, var) {
    if (minAge == 0) {baseline = baseline + 1}
    stk_temp = fun(stock)
    if (nrow(fun(stock)) > baseline) {
      stk_temp = stk_temp[1:baseline,]
      stk_temp[baseline,] = colMeans(fun(stock)[baseline:nrow(fun(stock)), ])
    }
    df_temp = as.data.frame(stk_temp)[, c("year", "age", "data")] #Thousands
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
  
  procBiomassMultLong <- function(stock, gsa, tri, minAge, baseline) {
    if (minAge == 0) {baseline = baseline + 1}
    stk_temp_n = stock.n(stock)
    if (nrow(stock.n(stock)) > baseline) {
      stk_temp_n = stk_temp_n[1:baseline,]
      stk_temp_n[baseline,] = colSums(stock.n(stock)[baseline:nrow(stock.n(stock)), ])
    }
    stk_temp_wt = stock.wt(stock)
    if (nrow(stock.wt(stock)) > baseline) {
      stk_temp_wt = stk_temp_wt[1:baseline,]
      stk_temp_wt[baseline,] = colSums(stock.wt(stock)[baseline:nrow(stock.wt(stock)), ])
    }
    stock_n = as.data.frame(stk_temp_n)[, c("year", "age", "data")] #Thousands
    stock_wt = as.data.frame(stk_temp_wt)[, c("year", "age", "data")] #Thousands
    stock_biomass = data.frame(year = stock_n$year,
                           age = stock_n$age,
                           biomass = (stock_n[,3] * stock_wt[,3])*1000)
    stock_biomass = aggregate(data = stock_biomass, biomass ~ year, FUN = "sum")
    obj_biomass = as.data.frame(biomass(stock))
    stock_biomass[, 2] = (obj_biomass[, 7] * 1000) / stock_biomass[, 2]
    stock_biomass[, 3] = procGSA(paste0(gsa, collapse = "-"))
    stock_biomass[, 4] = tri
    colnames(stock_biomass) <- c("year", "biomassMult", "gsa", "species")
    stock_biomass <- aggregate(data = stock_biomass, biomassMult ~ gsa + species, FUN = "mean")
    stock_biomass <- rbind(stock_biomass, stock_biomass[rep(1, (length(unique(stock_n$age))) - 1),])
    stock_biomass[, 4] = rep(unique(stock_n$age))
    colnames(stock_biomass) <- c("gsa", "species", "biomassMult", "age")
    return(stock_biomass)
  }
  
  procBiomassMultWide <- function(biomassmultlong) {
    biomassm_w <- dcast(biomassmultlong, biomassMult ~ species + gsa + age)
    for (i in 1:nrow(biomassm_w)) {
      for (j in 2:ncol(biomassm_w)) {
        if (!is.na(biomassm_w[i, j])) {
          biomassm_w[i, j] = biomassm_w$biomassMult[i]
        }
      }
    }
    biomassm_w$biomassMult <- NULL
    for (i in 1:ncol(biomassm_w)) {
      colnames(biomassm_w)[i] <- paste0(strsplit(colnames(biomassm_w)[i], "_")[[1]][1], "_",
                                    strsplit(colnames(biomassm_w)[i], "_")[[1]][length(strsplit(colnames(biomassm_w)[i], "_")[[1]])], "_S_",
                                    strsplit(colnames(biomassm_w)[i], "_")[[1]][2])
    }
    biomassm_w[is.na(biomassm_w)] = 0
    biomassm_w <- mutate_all(biomassm_w, function(x) as.numeric(as.character(x)))
    biomassm_w <- as.data.frame(lapply(biomassm_w, sum))
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
    # tot_df[,2:ncol(tot_df)] <- log10(tot_df[,2:ncol(tot_df)])
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
    
    # inputNames <- c("DPS_0_N", "DPS_1_N", "DPS_2_N", "DPS_3_N", "DPS_0_C", "DPS_1_C", "DPS_2_C", "DPS_3_C")
    # layerTypeTot <- c("LSTM", "LSTM", "Dropout", "Dense")
    # neuronsTot <- c(8, 8, 0, 4)
    # dropoutTot <- c(0.5, 0.5, 0.5, 0)
    # nLayers <- 4
    
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
                                                             input$dropout1, input$activation1, input$recdropout1, input$recactivation1)}
    if (as.integer(input$nLayers) >= 2) {outputs <- buildNet(outputs, 2, input$layerType2, input$neurons2, input$returnSeq2,
                                                             input$dropout2, input$activation2, input$recdropout2, input$recactivation2)}
    if (as.integer(input$nLayers) >= 3) {outputs <- buildNet(outputs, 3, input$layerType3, input$neurons3, input$returnSeq3,
                                                             input$dropout3, input$activation3, input$recdropout3, input$recactivation3)}
    if (as.integer(input$nLayers) >= 4) {outputs <- buildNet(outputs, 4, input$layerType4, input$neurons4, input$returnSeq4,
                                                             input$dropout4, input$activation4, input$recdropout4, input$recactivation4)}
    if (as.integer(input$nLayers) >= 5) {outputs <- buildNet(outputs, 5, input$layerType5, input$neurons5, input$returnSeq5,
                                                             input$dropout5, input$activation5, input$recdropout5, input$recactivation5)}
    
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
      dummy, dummy,
      # validation_data = dummy_val,
      verbose = 1,
      epochs = as.integer(input$nEpochs),
      callbacks = callbacks
    )
    
    history_df <- na.omit(as.data.frame(history))
    levels(history_df$metric)[match("loss", levels(history_df$metric))] <- "MSE"
    levels(history_df$metric)[match("mae", levels(history_df$metric))] <- "MAE"
    
    return(history_df)
    
  }
  
  plotFitNet <- function(testfit_results) {
    g = ggplot(data = testfit_results, aes(x = epoch)) +
      geom_line(aes(y = value, color = metric)) +
      ggtitle(paste0("Mean Squared/Absolute Error")) +
      scale_color_manual(name = "Metrics", values = c("darkblue", "darkcyan")) +
      xlab("Epoch") +
      ylab("Value") +
      theme_test() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "bottom") +
      facet_wrap(~ metric, scales = "free")
    
    return(g)
  }
  
  trainTestFitNet <- function(netInputs, depthTest) {
    
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
    
    if (env_var > 0) {
      envMult <- randomEnvVarSum(as.integer(input$depthPred), as.integer(input$envParamMult))
    }
    
    niter <- 10
    
    withProgress(message = "Calculating...", value = 0, detail = "0%", {
      
      for (iter in 1:niter) {
        
        norm_inputs = netInputs
        norm_inputs[,2:ncol(norm_inputs)] = normalizeInputs(norm_inputs[,2:ncol(norm_inputs)], range_inputs)
        train_df <- as.matrix(norm_inputs[, 2:ncol(norm_inputs)])
        val_df <- as.matrix(norm_inputs[, 2:ncol(norm_inputs)])
        iter_df <- norm_inputs[1:(nrow(train_df) - depthTest),]
    
        for (i in 1:depthTest) {
          
          inputs <- layer_input(shape = c((nrow(iter_df)), (ncol(iter_df) - 1)))
          outputs <- inputs
          inputs_df <- as.matrix(iter_df[, 2:ncol(iter_df)])
          outputs_df <- as.matrix(iter_df[, 2:ncol(iter_df)])
          
          if (as.integer(input$nLayers) >= 1) {outputs <- buildNet(outputs, 1, input$layerType1, input$neurons1, input$returnSeq1,
                                                                   input$dropout1, input$activation1, input$recdropout1, input$recactivation1)}
          if (as.integer(input$nLayers) >= 2) {outputs <- buildNet(outputs, 2, input$layerType2, input$neurons2, input$returnSeq2,
                                                                   input$dropout2, input$activation2, input$recdropout2, input$recactivation2)}
          if (as.integer(input$nLayers) >= 3) {outputs <- buildNet(outputs, 3, input$layerType3, input$neurons3, input$returnSeq3,
                                                                   input$dropout3, input$activation3, input$recdropout3, input$recactivation3)}
          if (as.integer(input$nLayers) >= 4) {outputs <- buildNet(outputs, 4, input$layerType4, input$neurons4, input$returnSeq4,
                                                                   input$dropout4, input$activation4, input$recdropout4, input$recactivation4)}
          if (as.integer(input$nLayers) >= 5) {outputs <- buildNet(outputs, 5, input$layerType5, input$neurons5, input$returnSeq5,
                                                                   input$dropout5, input$activation5, input$recdropout5, input$recactivation5)}
          
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
              "prova.keras", save_best_only = TRUE))
          
          model %>% compile(
            loss = "mse",
            metrics = "mae",
            optimizer_rmsprop(learning_rate = as.numeric(input$learnParam))
          )
          
          history <- model %>% fit(
            dummy, dummy,
            validation_data = dummy_val,
            verbose = 0,
            epochs = as.integer(input$nEpochs),
            callbacks = callbacks
          )
          
          iter_df[,2:ncol(iter_df)] <- denormalizeInputs(iter_df[,2:ncol(iter_df)], range_inputs)
          
          pred_vec <- model %>% predict(dummy)
          pred_vec <- t(as.matrix(pred_vec[1:as.integer(length(pred_vec)/2)]))
          colnames(pred_vec) = colnames(iter_df)[2:(ncol(iter_df)/2 + 1)]
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
    
    traintest_iter_results <<- ssb_df_tot
    
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
      
      results_incasting <- sp_biomass_wide[(nrow(sp_biomass_wide) - 4):nrow(sp_biomass_wide), c("ssb_obs", "ssb_min", "ssb_mean", "ssb_max")]
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
        
        results_incasting <- sp_biomass_wide[(nrow(sp_biomass_wide) - 4):nrow(sp_biomass_wide), c("ssb_obs", "ssb_min", "ssb_mean", "ssb_max")]
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
                                            # text = paste0("SSB min: ", y, "\nSSB max: ", spline_max$y)
                                            ),
                  fill = "firebrick", alpha = 0.5) +
      geom_line(data = line_pred_mean, aes(x = x, y = y), color = "red", linewidth = 2) +
      geom_line(data = line_obs, aes(x = x, y = y), color = "black", linewidth = 2) +
      # geom_ribbon(aes(ymin = ssb_min, ymax = ssb_max,
      #                                       # text = paste0("SSB min: ", y, "\nSSB max: ", spline_max$y)
      #                                       ),
      #             fill = "firebrick", alpha = 0.5) +
      # geom_line(aes(y = ssb_mean), color = "red", linewidth = 2) +
      # geom_line(aes(y = ssb_obs), color = "black", linewidth = 2) +
      scale_x_continuous(breaks = sort(unique(proj_biomass$year))) +
      # scale_y_continuous(trans = "log10") +
      ggtitle(paste0(unique(proj_biomass$species), " - ", unique(proj_biomass$gsa))) +
      xlab("Year") +
      ylab("SSB (tonnes)") +
      theme_test() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "bottom")
    
    return(g)
  }
  
  plotTaylorDiagram <- function(proj_biomass, plotTrainTestFitCount) {
    
    year = as.integer(max(unique(proj_biomass$year))) - 6
    
    g = taylor.diagram(ref = proj_biomass[which(proj_biomass$year > year), "ssb_obs"],
                       model = proj_biomass[which(proj_biomass$year > year), "ssb_mean"],
                       col = "red", pcex = 2.5)
    
    return(g)
  }
  
  predNet <- function(netInputs, f_adj, depth, biomassMult) {
    
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
    
    pred_biomassMult <- vector()
    for (i in 1:length(unique(species))) {
      pred_biomassMult[i] = rowMeans(biomassMult[, grep(unique(species)[i], names(biomassMult))])
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
  
          # if (as.integer(input$nLayers) >= 1) {outputs <- buildNet(outputs, 1, input$layerType1, input$neurons1, input$returnSeq1,
          #                                                          input$dropout1, input$activation1, input$recdropout1, input$recactivation1)}
          # if (as.integer(input$nLayers) >= 2) {outputs <- buildNet(outputs, 2, input$layerType2, input$neurons2, input$returnSeq2,
          #                                                          input$dropout2, input$activation2, input$recdropout2, input$recactivation2)}
          # if (as.integer(input$nLayers) >= 3) {outputs <- buildNet(outputs, 3, input$layerType3, input$neurons3, input$returnSeq3,
          #                                                          input$dropout3, input$activation3, input$recdropout3, input$recactivation3)}
          # if (as.integer(input$nLayers) >= 4) {outputs <- buildNet(outputs, 4, input$layerType4, input$neurons4, input$returnSeq4,
          #                                                          input$dropout4, input$activation4, input$recdropout4, input$recactivation4)}
          # if (as.integer(input$nLayers) >= 5) {outputs <- buildNet(outputs, 5, input$layerType5, input$neurons5, input$returnSeq5,
          #                                                          input$dropout5, input$activation5, input$recdropout5, input$recactivation5)}
  
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
  
          # model <- keras_model(inputs, outputs)
          
          model <- keras_model_sequential()
          
          if (as.integer(input$nLayers) >= 1) {model <- buildNet(model, 1, input$layerType1, input$neurons1, input$returnSeq1,
                                                                   input$dropout1, input$activation1, input$recdropout1, input$recactivation1)}
          if (as.integer(input$nLayers) >= 2) {model <- buildNet(model, 2, input$layerType2, input$neurons2, input$returnSeq2,
                                                                   input$dropout2, input$activation2, input$recdropout2, input$recactivation2)}
          if (as.integer(input$nLayers) >= 3) {model <- buildNet(model, 3, input$layerType3, input$neurons3, input$returnSeq3,
                                                                   input$dropout3, input$activation3, input$recdropout3, input$recactivation3)}
          if (as.integer(input$nLayers) >= 4) {model <- buildNet(model, 4, input$layerType4, input$neurons4, input$returnSeq4,
                                                                   input$dropout4, input$activation4, input$recdropout4, input$recactivation4)}
          if (as.integer(input$nLayers) >= 5) {model <- buildNet(model, 5, input$layerType5, input$neurons5, input$returnSeq5,
                                                                   input$dropout5, input$activation5, input$recdropout5, input$recactivation5)}
    
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
            dummy, dummy,
            validation_data = dummy_val,
            verbose = 0,
            epochs = as.integer(input$nEpochs),
            callbacks = callbacks
          )
          
          history_df <- as.data.frame(history)
          
          proj[,2:ncol(proj)] <- denormalizeInputs(proj[,2:ncol(proj)], range_inputs)
          
          pred_vec <- model %>% predict(dummy)
          pred_vec <- t(as.matrix(pred_vec[1:as.integer(length(pred_vec)/2)]))
          colnames(pred_vec) = colnames(proj)[2:(ncol(proj)/2 + 1)]
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
          
          incProgress(amount = 1/(3 * as.integer(input$depthPred)),
                      detail = paste0(as.character(round(((as.integer(input$depthPred) * (iter - 1) + i)/(3 * as.integer(input$depthPred))) * 100, 2)), "%"))
          
        }
        
        # Denormalize outputs
        proj[,2:ncol(proj)] <- denormalizeInputs(proj[,2:ncol(proj)], range_inputs)
        
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
        ssb_iter <- proj_df[which(proj_df$age > 0),]
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
                                         # text = paste0("SSB min: ", y, "\nSSB max: ", spline_max$y)
                                         ),
                  fill = "firebrick", alpha = 0.5) +
      geom_line(data = spline_mean, aes(x = x, y = y, color = type,
                                        # text = paste0("SSB mean: ", y)
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
  
  SRRicker <- function(Theta, biomass) {
    log.alpha = Theta$log.alpha
    beta = Theta$beta
    logValue = log.alpha + log(biomass) - beta * biomass + log(1000)
    value = exp(logValue)
    return(value)
  }
  
  plotRatio <- function(proj_biomass, plotPredCount) {
    
    proj_biomass$ssb = proj_biomass$ssb_mean/10^3
    proj_biomass$recruitment = proj_biomass$recr_mean/10^3
    biomass_vec = seq(0, ceiling(max(proj_biomass$ssb)), by = 1)
    z = proj_biomass[,c("recruitment", "ssb")]
    y = log(z$recruitment) - log(1000) - log(z$ssb)
    fit = lm(y ~ proj_biomass$ssb)
    log.alpha = as.numeric(coefficients(fit)[1])
    beta = as.numeric(coefficients(fit)[2])
    
    Theta_fit = list(log.alpha = log.alpha, beta = beta)
    pred_R_fit = SRRicker(Theta_fit, biomass_vec)
    
    df_plot = data.frame(ssb = biomass_vec, R = pred_R_fit)
    
    g = ggplot(data = df_plot, aes(x = ssb, y = R)) + 
      geom_point(size = 0.5) + 
      geom_point(data = proj_biomass, aes(x = ssb, y = recruitment, fill = type), pch = 21, size = 6) + 
      scale_y_continuous(trans = "log10") +
      scale_fill_manual(name = "Ratio", values = c("deepskyblue", "darkblue")) +
      ggtitle(paste0(unique(proj_biomass$species), " - ", unique(proj_biomass$gsa))) +
      xlab("SSB") +
      ylab("Recruitment") +
      theme_test() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
            axis.text.y = element_text(size = 12),
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
    biomassms <<- l[["biomassms"]]
    biomassm_l <<- l[["biomassm_l"]]
    biomassm_w <<- l[["biomassm_w"]]
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
    traintest_results <<- l[["traintest_results"]]
    traintest_plots <<- l[["traintest_plots"]]
    taylor_diagram <<- l[["taylor_diagram"]]
    depth_pred <<- l[["depth_pred"]]
    plotPredCount <<- 1
    model_pred <<- l[["model_pred"]]
    pred_iter_partial <<- l[["pred_iter_partial"]]
    pred_results <<- l[["pred_results"]]
    pred_plots <<- l[["pred_plots"]]
    ratio_plots <<- l[["ratio_plots"]]
    output$plotPred <- renderPlot({
      pred_plots[[1]]
      })
    output$plotRatio <- renderPlot({
      ratio_plots[[1]]
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
                             - Press 'Load' Button to process datas<br>
                             - Press 'Adjust' if you find any outlying value<br><br>
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
                            - Recurrent Dropout: dropout for the recurrent state<br>
                            - Activation: function for the activation of input/forget/output gate<br>
                            - Recurrent Activation: function for the activation<br>
                            of cell and hidden states<br>
                             "),
                            footer = NULL,
                            easyClose = TRUE))
  
  observeEvent(input$netHelp, {
    showModal(netHelp)
  })
  
  ##### REACTIVE VALUES #####
  
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
    stk1 <<- if (strsplit(as.character(input$sobj1[4]), "[.]")[[1]][2] == "rds") {
      readRDS(as.character(input$sobj1[4]))
      } else {
        loadRData(as.character(input$sobj1[4]))
      }
    rv1$min <- as.integer(stk1@range[1])
    rv1$max <- as.integer(stk1@range[2])
    updatePickerInput(
      session,
      "baseline1",
      choices = rv1$min:rv1$max,
      selected = rv1$max
    )
    rv1$tri <- as.character(strsplit(as.character(input$sobj1[1]), "_")[[1]][1])
    rv1$spinfo <- speciesInfo(rv1$tri)
    rv1$gsa <- as.vector(as.integer(strsplit(strsplit(strsplit(as.character(input$sobj1[1]), "_")[[1]][2], "\\.")[[1]][1], "-")[[1]]))
    rv1$gsainfo <- gsaInfo(rv1$gsa)
  })
  
  rv1 <- reactiveValues(
    tri = NULL,
    gsa = NULL,
    min = 0,
    max = 0,
    spinfo = "No Species Selected",
    gsainfo = "No GSA Selected"
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
  
  observeEvent(input$reset1, {
    rv1$obj <- NULL
    rv1$tri <- NULL
    rv1$gsa <- NULL
    info = "No Species Selected"
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
    stk2 <<- if (strsplit(as.character(input$sobj2[4]), "[.]")[[1]][2] == "rds") {
      readRDS(as.character(input$sobj2[4]))
    } else {
      loadRData(as.character(input$sobj2[4]))
    }
    rv2$min <- as.integer(stk2@range[1])
    rv2$max <- as.integer(stk2@range[2])
    updatePickerInput(
      session,
      "baseline2",
      choices = rv2$min:rv2$max,
      selected = rv2$max
    )
    rv2$tri <- as.character(strsplit(as.character(input$sobj2[1]), "_")[[1]][1])
    rv2$spinfo <- speciesInfo(rv2$tri)
    rv2$gsa <- as.vector(as.integer(strsplit(strsplit(strsplit(as.character(input$sobj2[1]), "_")[[1]][2], "\\.")[[1]][1], "-")[[1]]))
    rv2$gsainfo <- gsaInfo(rv2$gsa)
  })
  
  rv2 <- reactiveValues(
    tri = NULL,
    gsa = NULL,
    min = 0,
    max = 0,
    spinfo = "No Species Selected",
    gsainfo = "No GSA Selected"
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
  
  observeEvent(input$reset2, {
    rv2$obj <- NULL
    rv2$tri <- NULL
    rv2$gsa <- NULL
    info = "No Species Selected"
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
    stk3 <<- if (strsplit(as.character(input$sobj3[4]), "[.]")[[1]][2] == "rds") {
      readRDS(as.character(input$sobj3[4]))
    } else {
      loadRData(as.character(input$sobj3[4]))
    }
    rv3$min <- as.integer(stk3@range[1])
    rv3$max <- as.integer(stk3@range[2])
    updatePickerInput(
      session,
      "baseline3",
      choices = rv3$min:rv3$max,
      selected = rv3$max
    )
    rv3$tri <- as.character(strsplit(as.character(input$sobj3[1]), "_")[[1]][1])
    rv3$spinfo <- speciesInfo(rv3$tri)
    rv3$gsa <- as.vector(as.integer(strsplit(strsplit(strsplit(as.character(input$sobj3[1]), "_")[[1]][2], "\\.")[[1]][1], "-")[[1]]))
    rv3$gsainfo <- gsaInfo(rv3$gsa)
  })
  
  rv3 <- reactiveValues(
    tri = NULL,
    gsa = NULL,
    min = 0,
    max = 0,
    spinfo = "No Species Selected",
    gsainfo = "No GSA Selected"
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
  
  observeEvent(input$reset3, {
    rv3$obj <- NULL
    rv3$tri <- NULL
    rv3$gsa <- NULL
    info = "No Species Selected"
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
    stk4 <<- if (strsplit(as.character(input$sobj4[4]), "[.]")[[1]][2] == "rds") {
      readRDS(as.character(input$sobj4[4]))
    } else {
      loadRData(as.character(input$sobj4[4]))
    }
    rv4$min <- as.integer(stk4@range[1])
    rv4$max <- as.integer(stk4@range[2])
    updatePickerInput(
      session,
      "baseline4",
      choices = rv4$min:rv4$max,
      selected = rv4$max
    )
    rv4$tri <- as.character(strsplit(as.character(input$sobj4[1]), "_")[[1]][1])
    rv4$spinfo <- speciesInfo(rv4$tri)
    rv4$gsa <- as.vector(as.integer(strsplit(strsplit(strsplit(as.character(input$sobj4[1]), "_")[[1]][2], "\\.")[[1]][1], "-")[[1]]))
    rv4$gsainfo <- gsaInfo(rv4$gsa)
  })
  
  rv4 <- reactiveValues(
    tri = NULL,
    gsa = NULL,
    min = 0,
    max = 0,
    spinfo = "No Species Selected",
    gsainfo = "No GSA Selected"
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
  
  observeEvent(input$reset4, {
    rv4$obj <- NULL
    rv4$tri <- NULL
    rv4$gsa <- NULL
    info = "No Species Selected"
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
    stk5 <<- if (strsplit(as.character(input$sobj5[4]), "[.]")[[1]][2] == "rds") {
      readRDS(as.character(input$sobj5[4]))
    } else {
      loadRData(as.character(input$sobj5[4]))
    }
    rv5$min <- as.integer(stk5@range[1])
    rv5$max <- as.integer(stk5@range[2])
    updatePickerInput(
      session,
      "baseline5",
      choices = rv5$min:rv5$max,
      selected = rv5$max
    )
    rv5$tri <- as.character(strsplit(as.character(input$sobj5[1]), "_")[[1]][1])
    rv5$spinfo <- speciesInfo(rv5$tri)
    rv5$gsa <- as.vector(as.integer(strsplit(strsplit(strsplit(as.character(input$sobj5[1]), "_")[[1]][2], "\\.")[[1]][1], "-")[[1]]))
    rv5$gsainfo <- gsaInfo(rv5$gsa)
  })
  
  rv5 <- reactiveValues(
    tri = NULL,
    gsa = NULL,
    min = 0,
    max = 0,
    spinfo = "No Species Selected",
    gsainfo = "No GSA Selected"
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
  
  observeEvent(input$reset5, {
    rv5$obj <- NULL
    rv5$tri <- NULL
    rv5$gsa <- NULL
    info = "No Species Selected"
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
    stk6 <<- if (strsplit(as.character(input$sobj6[4]), "[.]")[[1]][2] == "rds") {
      readRDS(as.character(input$sobj6[4]))
    } else {
      loadRData(as.character(input$sobj6[4]))
    }
    rv6$min <- as.integer(stk6@range[1])
    rv6$max <- as.integer(stk6@range[2])
    updatePickerInput(
      session,
      "baseline6",
      choices = rv6$min:rv6$max,
      selected = rv6$max
    )
    rv6$tri <- as.character(strsplit(as.character(input$sobj6[1]), "_")[[1]][1])
    rv6$spinfo <- speciesInfo(rv6$tri)
    rv6$gsa <- as.vector(as.integer(strsplit(strsplit(strsplit(as.character(input$sobj6[1]), "_")[[1]][2], "\\.")[[1]][1], "-")[[1]]))
    rv6$gsainfo <- gsaInfo(rv6$gsa)
  })
  
  rv6 <- reactiveValues(
    tri = NULL,
    gsa = NULL,
    min = 0,
    max = 0,
    spinfo = "No Species Selected",
    gsainfo = "No GSA Selected"
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
  
  observeEvent(input$reset6, {
    rv6$obj <- NULL
    rv6$tri <- NULL
    rv6$gsa <- NULL
    info = "No Species Selected"
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
    stk7 <<- if (strsplit(as.character(input$sobj7[4]), "[.]")[[1]][2] == "rds") {
      readRDS(as.character(input$sobj7[4]))
    } else {
      loadRData(as.character(input$sobj7[4]))
    }
    rv7$min <- as.integer(stk7@range[1])
    rv7$max <- as.integer(stk7@range[2])
    updatePickerInput(
      session,
      "baseline7",
      choices = rv7$min:rv7$max,
      selected = rv7$max
    )
    rv7$tri <- as.character(strsplit(as.character(input$sobj7[1]), "_")[[1]][1])
    rv7$spinfo <- speciesInfo(rv7$tri)
    rv7$gsa <- as.vector(as.integer(strsplit(strsplit(strsplit(as.character(input$sobj7[1]), "_")[[1]][2], "\\.")[[1]][1], "-")[[1]]))
    rv7$gsainfo <- gsaInfo(rv7$gsa)
  })
  
  rv7 <- reactiveValues(
    tri = NULL,
    gsa = NULL,
    min = 0,
    max = 0,
    spinfo = "No Species Selected",
    gsainfo = "No GSA Selected"
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
  
  observeEvent(input$reset7, {
    rv7$obj <- NULL
    rv7$tri <- NULL
    rv7$gsa <- NULL
    info = "No Species Selected"
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
    stk8 <<- if (strsplit(as.character(input$sobj8[4]), "[.]")[[1]][2] == "rds") {
      readRDS(as.character(input$sobj8[4]))
    } else {
      loadRData(as.character(input$sobj8[4]))
    }
    rv8$min <- as.integer(stk8@range[1])
    rv8$max <- as.integer(stk8@range[2])
    updatePickerInput(
      session,
      "baseline8",
      choices = rv8$min:rv8$max,
      selected = rv8$max
    )
    rv8$tri <- as.character(strsplit(as.character(input$sobj8[1]), "_")[[1]][1])
    rv8$spinfo <- speciesInfo(rv8$tri)
    rv8$gsa <- as.vector(as.integer(strsplit(strsplit(strsplit(as.character(input$sobj8[1]), "_")[[1]][2], "\\.")[[1]][1], "-")[[1]]))
    rv8$gsainfo <- gsaInfo(rv8$gsa)
  })
  
  rv8 <- reactiveValues(
    tri = NULL,
    gsa = NULL,
    min = 0,
    max = 0,
    spinfo = "No Species Selected",
    gsainfo = "No GSA Selected"
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
  
  observeEvent(input$reset8, {
    rv8$obj <- NULL
    rv8$tri <- NULL
    rv8$gsa <- NULL
    info = "No Species Selected"
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
    stk9 <<- if (strsplit(as.character(input$sobj9[4]), "[.]")[[1]][2] == "rds") {
      readRDS(as.character(input$sobj9[4]))
    } else {
      loadRData(as.character(input$sobj9[4]))
    }
    rv9$min <- as.integer(stk9@range[1])
    rv9$max <- as.integer(stk9@range[2])
    updatePickerInput(
      session,
      "baseline9",
      choices = rv9$min:rv9$max,
      selected = rv9$max
    )
    rv9$tri <- as.character(strsplit(as.character(input$sobj9[1]), "_")[[1]][1])
    rv9$spinfo <- speciesInfo(rv9$tri)
    rv9$gsa <- as.vector(as.integer(strsplit(strsplit(strsplit(as.character(input$sobj9[1]), "_")[[1]][2], "\\.")[[1]][1], "-")[[1]]))
    rv9$gsainfo <- gsaInfo(rv9$gsa)
  })
  
  rv9 <- reactiveValues(
    tri = NULL,
    gsa = NULL,
    min = 0,
    max = 0,
    spinfo = "No Species Selected",
    gsainfo = "No GSA Selected"
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
  
  observeEvent(input$reset9, {
    rv9$obj <- NULL
    rv9$tri <- NULL
    rv9$gsa <- NULL
    info = "No Species Selected"
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
    stk10 <<- if (strsplit(as.character(input$sobj10[4]), "[.]")[[1]][2] == "rds") {
      readRDS(as.character(input$sobj10[4]))
    } else {
      loadRData(as.character(input$sobj10[4]))
    }
    rv10$min <- as.integer(stk10@range[1])
    rv10$max <- as.integer(stk10@range[2])
    updatePickerInput(
      session,
      "baseline10",
      choices = rv10$min:rv10$max,
      selected = rv10$max
    )
    rv10$tri <- as.character(strsplit(as.character(input$sobj10[1]), "_")[[1]][1])
    rv10$spinfo <- speciesInfo(rv10$tri)
    rv10$gsa <- as.vector(as.integer(strsplit(strsplit(strsplit(as.character(input$sobj10[1]), "_")[[1]][2], "\\.")[[1]][1], "-")[[1]]))
    rv10$gsainfo <- gsaInfo(rv10$gsa)
  })
  
  rv10 <- reactiveValues(
    tri = NULL,
    gsa = NULL,
    min = 0,
    max = 0,
    spinfo = "No Species Selected",
    gsainfo = "No GSA Selected"
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
  
  observeEvent(input$reset10, {
    rv10$obj <- NULL
    rv10$tri <- NULL
    rv10$gsa <- NULL
    info = "No Species Selected"
  })
    
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

  })
  
  ##### POP #####
  
  pop1 <- reactive({
    if (!is.null(stk1) & !is.null(rv1$tri) & !is.null(rv1$gsa) & !is.null(rv1$min)) {
      procDfLongQuant(stk1, rv1$gsa, rv1$tri, rv1$min, as.integer(input$baseline1), stock.n, pop)
    } else if (is.null(stk1) & is.null(rv1$tri) & is.null(rv1$gsa)) {
      return(NULL)
    } else {
      showModal(tags$div(id = "modalWarning", modalDialog("Warning: incomplete data loading!",
                            footer = NULL,
                            easyClose = TRUE)))
      }
  })

  pop2 <- reactive({
    if (!is.null(stk2) & !is.null(rv2$tri) & !is.null(rv2$gsa) & !is.null(rv2$min)) {
      procDfLongQuant(stk2, rv2$gsa, rv2$tri, rv2$min, as.integer(input$baseline2), stock.n, pop)
    } else if (is.null(rv2$obj) & is.null(rv2$tri) & is.null(rv2$gsa)) {
      return(NULL)
    } else {
      showModal(tags$div(id = "modalWarning", modalDialog("Warning: incomplete data loading!",
                                                          footer = NULL,
                                                          easyClose = TRUE)))
      }
  })

  pop3 <- reactive({
    if (!is.null(stk3) & !is.null(rv3$tri) & !is.null(rv3$gsa) & !is.null(rv3$min))  {
      procDfLongQuant(stk3, rv3$gsa, rv3$tri, rv3$min, as.integer(input$baseline3), stock.n, pop)
    } else if (is.null(rv3$obj) & is.null(rv3$tri) & is.null(rv3$gsa)) {
      return(NULL)
    } else {
      showModal(tags$div(id = "modalWarning", modalDialog("Warning: incomplete data loading!",
                                                          footer = NULL,
                                                          easyClose = TRUE)))
    }
  })

  pop4 <- reactive({
    if (!is.null(stk4) & !is.null(rv4$tri) & !is.null(rv4$gsa) & !is.null(rv4$min))  {
      procDfLongQuant(stk4, rv4$gsa, rv4$tri, rv4$min, as.integer(input$baseline4), stock.n, pop)
    } else if (is.null(rv4$obj) & is.null(rv4$tri) & is.null(rv4$gsa)) {
      return(NULL)
    } else {
      showModal(tags$div(id = "modalWarning", modalDialog("Warning: incomplete data loading!",
                                                          footer = NULL,
                                                          easyClose = TRUE)))
    }
  })

  pop5 <- reactive({
    if (!is.null(stk5) & !is.null(rv5$tri) & !is.null(rv5$gsa) & !is.null(rv5$min))  {
      procDfLongQuant(stk5, rv5$gsa, rv5$tri, rv5$min, as.integer(input$baseline5), stock.n, pop)
    } else if (is.null(rv5$obj) & is.null(rv5$tri) & is.null(rv5$gsa)) {
      return(NULL)
    } else {
      showModal(tags$div(id = "modalWarning", modalDialog("Warning: incomplete data loading!",
                                                          footer = NULL,
                                                          easyClose = TRUE)))
    }
  })

  pop6 <- reactive({
    if (!is.null(stk6) & !is.null(rv6$tri) & !is.null(rv6$gsa) & !is.null(rv6$min))  {
      procDfLongQuant(stk6, rv6$gsa, rv6$tri, rv6$min, as.integer(input$baseline6), stock.n, pop)
    } else if (is.null(rv6$obj) & is.null(rv6$tri) & is.null(rv6$gsa)) {
      return(NULL)
    } else {
      showModal(tags$div(id = "modalWarning", modalDialog("Warning: incomplete data loading!",
                                                          footer = NULL,
                                                          easyClose = TRUE)))
    }
  })

  pop7 <- reactive({
    if (!is.null(stk7) & !is.null(rv7$tri) & !is.null(rv7$gsa) & !is.null(rv7$min))  {
      procDfLongQuant(stk7, rv7$gsa, rv7$tri, rv7$min, as.integer(input$baseline7), stock.n, pop)
    } else if (is.null(rv7$obj) & is.null(rv7$tri) & is.null(rv7$gsa)) {
      return(NULL)
    } else {
      showModal(tags$div(id = "modalWarning", modalDialog("Warning: incomplete data loading!",
                                                          footer = NULL,
                                                          easyClose = TRUE)))
    }
  })

  pop8 <- reactive({
    if (!is.null(stk8) & !is.null(rv8$tri) & !is.null(rv8$gsa) & !is.null(rv8$min))  {
      procDfLongQuant(stk8, rv8$gsa, rv8$tri, rv8$min, as.integer(input$baseline8), stock.n, pop)
    } else if (is.null(rv8$obj) & is.null(rv8$tri) & is.null(rv8$gsa)) {
      return(NULL)
    } else {
      showModal(tags$div(id = "modalWarning", modalDialog("Warning: incomplete data loading!",
                                                          footer = NULL,
                                                          easyClose = TRUE)))
    }
  })

  pop9 <- reactive({
    if (!is.null(stk9) & !is.null(rv9$tri) & !is.null(rv9$gsa) & !is.null(rv9$min))  {
      procDfLongQuant(stk9, rv9$gsa, rv9$tri, rv9$min, as.integer(input$baseline9), stock.n, pop)
    } else if (is.null(rv9$obj) & is.null(rv9$tri) & is.null(rv9$gsa)) {
      return(NULL)
    } else {
      showModal(tags$div(id = "modalWarning", modalDialog("Warning: incomplete data loading!",
                                                          footer = NULL,
                                                          easyClose = TRUE)))
    }
  })

  pop10 <- reactive({
    if (!is.null(stk10) & !is.null(rv10$tri) & !is.null(rv10$gsa) & !is.null(rv10$min))  {
      procDfLongQuant(stk10, rv10$gsa, rv10$tri, rv10$min, as.integer(input$baseline10), stock.n, pop)
    } else if (is.null(rv10$obj) & is.null(rv10$tri) & is.null(rv10$gsa)) {
      return(NULL)
    } else {
      showModal(tags$div(id = "modalWarning", modalDialog("Warning: incomplete data loading!",
                                                          footer = NULL,
                                                          easyClose = TRUE)))
    }
  })
  
  observeEvent(input$loadButton, {
    if (is.null(pop1()) &
        is.null(pop2()) &
        is.null(pop3()) &
        is.null(pop4()) &
        is.null(pop5()) &
        is.null(pop6()) &
        is.null(pop7()) &
        is.null(pop8()) &
        is.null(pop9()) &
        is.null(pop10()))
      return(NULL)
    if (!is.null(pop1())) {pops[[1]] <- pop1()}
    if (!is.null(pop2())) {pops[[2]] <- pop2()}
    if (!is.null(pop3())) {pops[[3]] <- pop3()}
    if (!is.null(pop4())) {pops[[4]] <- pop4()}
    if (!is.null(pop5())) {pops[[5]] <- pop5()}
    if (!is.null(pop6())) {pops[[6]] <- pop6()}
    if (!is.null(pop7())) {pops[[7]] <- pop7()}
    if (!is.null(pop8())) {pops[[8]] <- pop8()}
    if (!is.null(pop9())) {pops[[9]] <- pop9()}
    if (!is.null(pop10())) {pops[[10]] <- pop10()}
    
    pops <<- pops
    
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
    })
  
  observeEvent(input$adjustButton, {
    if (length(pops_w) == 0) {return(NULL)} else {
      pops_w <<- adjDfWide(pops_l, pops_w)
      output$plotPop <- renderPlotly({
        plotPopObj <- layout(ggplotly(plotPop(pops_l), tooltip = "y"), hovermode = "x unified")
        return(plotPopObj)
      })
      showModal(tags$div(id = "modalWarning",
                         modalDialog("DataFrame Adjusted!",
                                     footer = NULL,
                                     easyClose = TRUE)))
    }
  })
  
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
  
  ##### CATCHES #####
  
  catch1 <- reactive({
    if (!is.null(stk1) & !is.null(rv1$tri) & !is.null(rv1$gsa) & !is.null(rv1$min))
      procDfLongQuant(stk1, rv1$gsa, rv1$tri, rv1$min, as.integer(input$baseline1), catch.n, catch)
  })
  
  catch2 <- reactive({
    if (!is.null(stk2) & !is.null(rv2$tri) & !is.null(rv2$gsa) & !is.null(rv2$min))
      procDfLongQuant(stk2, rv2$gsa, rv2$tri, rv2$min, as.integer(input$baseline2), catch.n, catch)
  })
  
  catch3 <- reactive({
    if (!is.null(stk3) & !is.null(rv3$tri) & !is.null(rv3$gsa) & !is.null(rv3$min))
      procDfLongQuant(stk3, rv3$gsa, rv3$tri, rv3$min, as.integer(input$baseline3), catch.n, catch)
  })
  
  catch4 <- reactive({
    if (!is.null(stk4) & !is.null(rv4$tri) & !is.null(rv4$gsa) & !is.null(rv4$min))
      procDfLongQuant(stk4, rv4$gsa, rv4$tri, rv4$min, as.integer(input$baseline4), catch.n, catch)
  })
  
  catch5 <- reactive({
    if (!is.null(stk5) & !is.null(rv5$tri) & !is.null(rv5$gsa) & !is.null(rv5$min))
      procDfLongQuant(stk5, rv5$gsa, rv5$tri, rv5$min, as.integer(input$baseline5), catch.n, catch)
  })
  
  catch6 <- reactive({
    if (!is.null(stk6) & !is.null(rv6$tri) & !is.null(rv6$gsa) & !is.null(rv6$min))
      procDfLongQuant(stk6, rv6$gsa, rv6$tri, rv6$min, as.integer(input$baseline6), catch.n, catch)
  })
  
  catch7 <- reactive({
    if (!is.null(stk7) & !is.null(rv7$tri) & !is.null(rv7$gsa) & !is.null(rv7$min))
      procDfLongQuant(stk7, rv7$gsa, rv7$tri, rv7$min, as.integer(input$baseline7), catch.n, catch)
  })
  
  catch8 <- reactive({
    if (!is.null(stk8) & !is.null(rv8$tri) & !is.null(rv8$gsa) & !is.null(rv8$min))
      procDfLongQuant(stk8, rv8$gsa, rv8$tri, rv8$min, as.integer(input$baseline8), catch.n, catch)
  })
  
  catch9 <- reactive({
    if (!is.null(stk9) & !is.null(rv9$tri) & !is.null(rv9$gsa) & !is.null(rv9$min))
      procDfLongQuant(stk9, rv9$gsa, rv9$tri, rv9$min, as.integer(input$baseline9), catch.n, catch)
  })
  
  catch10 <- reactive({
    if (!is.null(stk10) & !is.null(rv10$tri) & !is.null(rv10$gsa) & !is.null(rv10$min))
      procDfLongQuant(stk10, rv10$gsa, rv10$tri, rv10$min, as.integer(input$baseline10), catch.n, catch)
  })
  
  observeEvent(input$loadButton, {
    if (is.null(catch1()) &
        is.null(catch2()) &
        is.null(catch3()) &
        is.null(catch4()) &
        is.null(catch5()) &
        is.null(catch6()) &
        is.null(catch7()) &
        is.null(catch8()) &
        is.null(catch9()) &
        is.null(catch10()))
      return(NULL)
    if (!is.null(catch1())) {catches[[1]] <- catch1()}
    if (!is.null(catch2())) {catches[[2]] <- catch2()}
    if (!is.null(catch3())) {catches[[3]] <- catch3()}
    if (!is.null(catch4())) {catches[[4]] <- catch4()}
    if (!is.null(catch5())) {catches[[5]] <- catch5()}
    if (!is.null(catch6())) {catches[[6]] <- catch6()}
    if (!is.null(catch7())) {catches[[7]] <- catch7()}
    if (!is.null(catch8())) {catches[[8]] <- catch8()}
    if (!is.null(catch9())) {catches[[9]] <- catch9()}
    if (!is.null(catch10())) {catches[[10]] <- catch10()}
    
    catches <<- catches
    
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
  })
  
  observeEvent(input$adjustButton, {
    if (length(catches_w) == 0) {return(NULL)} else {
      catches_w <<- adjDfWide(catches_l, catches_w)
      output$plotCatch <- renderPlotly({
        plotCatchObj <- layout(ggplotly(plotCatch(catches_l), tooltip = "y"), hovermode = "x unified")
        return(plotCatchObj)
      })
    }
  })
  
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
  
  ##### WEIGHT AT AGE #####
  
  waa1 <- reactive({
    if (!is.null(stk1) & !is.null(rv1$tri) & !is.null(rv1$gsa) & !is.null(rv1$min))
      procDfLongMult(stk1, rv1$gsa, rv1$tri, rv1$min, as.integer(input$baseline1), catch.wt, weight_at_age)
  })
  
  waa2 <- reactive({
    if (!is.null(stk2) & !is.null(rv2$tri) & !is.null(rv2$gsa) & !is.null(rv2$min))
      procDfLongMult(stk2, rv2$gsa, rv2$tri, rv2$min, as.integer(input$baseline2), catch.wt, weight_at_age)
  })
  
  waa3 <- reactive({
    if (!is.null(stk3) & !is.null(rv3$tri) & !is.null(rv3$gsa) & !is.null(rv3$min))
      procDfLongMult(stk3, rv3$gsa, rv3$tri, rv3$min, as.integer(input$baseline3), catch.wt, weight_at_age)
  })
  
  waa4 <- reactive({
    if (!is.null(stk4) & !is.null(rv4$tri) & !is.null(rv4$gsa) & !is.null(rv4$min))
      procDfLongMult(stk4, rv4$gsa, rv4$tri, rv4$min, as.integer(input$baseline4), catch.wt, weight_at_age)
  })
  
  waa5 <- reactive({
    if (!is.null(stk5) & !is.null(rv5$tri) & !is.null(rv5$gsa) & !is.null(rv5$min))
      procDfLongMult(stk5, rv5$gsa, rv5$tri, rv5$min, as.integer(input$baseline5), catch.wt, weight_at_age)
  })
  
  waa6 <- reactive({
    if (!is.null(stk6) & !is.null(rv6$tri) & !is.null(rv6$gsa) & !is.null(rv6$min))
      procDfLongMult(stk6, rv6$gsa, rv6$tri, rv6$min, as.integer(input$baseline6), catch.wt, weight_at_age)
  })
  
  waa7 <- reactive({
    if (!is.null(stk7) & !is.null(rv7$tri) & !is.null(rv7$gsa) & !is.null(rv7$min))
      procDfLongMult(stk7, rv7$gsa, rv7$tri, rv7$min, as.integer(input$baseline7), catch.wt, weight_at_age)
  })
  
  waa8 <- reactive({
    if (!is.null(stk8) & !is.null(rv8$tri) & !is.null(rv8$gsa) & !is.null(rv8$min))
      procDfLongMult(stk8, rv8$gsa, rv8$tri, rv8$min, as.integer(input$baseline8), catch.wt, weight_at_age)
  })
  
  waa9 <- reactive({
    if (!is.null(stk9) & !is.null(rv9$tri) & !is.null(rv9$gsa) & !is.null(rv9$min))
      procDfLongMult(stk9, rv9$gsa, rv9$tri, rv9$min, as.integer(input$baseline9), catch.wt, weight_at_age)
  })
  
  waa10 <- reactive({
    if (!is.null(stk10) & !is.null(rv10$tri) & !is.null(rv10$gsa) & !is.null(rv10$min))
      procDfLongMult(stk10, rv10$gsa, rv10$tri, rv10$min, as.integer(input$baseline10), catch.wt, weight_at_age)
  })
  
  observeEvent(input$loadButton, {
    if (is.null(waa1()) &
        is.null(waa2()) &
        is.null(waa3()) &
        is.null(waa4()) &
        is.null(waa5()) &
        is.null(waa6()) &
        is.null(waa7()) &
        is.null(waa8()) &
        is.null(waa9()) &
        is.null(waa10()))
      return(NULL)
    if (!is.null(waa1())) {waa[[1]] <- waa1()}
    if (!is.null(waa2())) {waa[[2]] <- waa2()}
    if (!is.null(waa3())) {waa[[3]] <- waa3()}
    if (!is.null(waa4())) {waa[[4]] <- waa4()}
    if (!is.null(waa5())) {waa[[5]] <- waa5()}
    if (!is.null(waa6())) {waa[[6]] <- waa6()}
    if (!is.null(waa7())) {waa[[7]] <- waa7()}
    if (!is.null(waa8())) {waa[[8]] <- waa8()}
    if (!is.null(waa9())) {waa[[9]] <- waa9()}
    if (!is.null(waa10())) {waa[[10]] <- waa10()}
    
    waa <<- waa
    
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
  })
  
  observeEvent(input$adjustButton, {
    if (length(waa_w) == 0) {return(NULL)} else {
      waa_w <<- adjDfWide(waa_l, waa_w)
      output$plotWaa <- renderPlotly({
        plotWaaObj <- layout(ggplotly(plotWaa(waa_l, pops_l), tooltip = "y"), hovermode = "x unified")
        return(plotWaaObj)
      })
    }
  })
  
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
  
  ##### FMORT #####
  
  fmort1 <- reactive({
    if (!is.null(stk1) & !is.null(rv1$tri) & !is.null(rv1$gsa) & !is.null(rv1$min))
      procDfLongMult(stk1, rv1$gsa, rv1$tri, rv1$min, as.integer(input$baseline1), harvest, fmort)
  })
  
  fmort2 <- reactive({
    if (!is.null(stk2) & !is.null(rv2$tri) & !is.null(rv2$gsa) & !is.null(rv2$min))
      procDfLongMult(stk2, rv2$gsa, rv2$tri, rv2$min, as.integer(input$baseline2), harvest, fmort)
  })
  
  fmort3 <- reactive({
    if (!is.null(stk3) & !is.null(rv3$tri) & !is.null(rv3$gsa) & !is.null(rv3$min))
      procDfLongMult(stk3, rv3$gsa, rv3$tri, rv3$min, as.integer(input$baseline3), harvest, fmort)
  })
  
  fmort4 <- reactive({
    if (!is.null(stk4) & !is.null(rv4$tri) & !is.null(rv4$gsa) & !is.null(rv4$min))
      procDfLongMult(stk4, rv4$gsa, rv4$tri, rv4$min, as.integer(input$baseline4), harvest, fmort)
  })
  
  fmort5 <- reactive({
    if (!is.null(stk5) & !is.null(rv5$tri) & !is.null(rv5$gsa) & !is.null(rv5$min))
      procDfLongMult(stk5, rv5$gsa, rv5$tri, rv5$min, as.integer(input$baseline5), harvest, fmort)
  })
  
  fmort6 <- reactive({
    if (!is.null(stk6) & !is.null(rv6$tri) & !is.null(rv6$gsa) & !is.null(rv6$min))
      procDfLongMult(stk6, rv6$gsa, rv6$tri, rv6$min, as.integer(input$baseline6), harvest, fmort)
  })
  
  fmort7 <- reactive({
    if (!is.null(stk7) & !is.null(rv7$tri) & !is.null(rv7$gsa) & !is.null(rv7$min))
      procDfLongMult(stk7, rv7$gsa, rv7$tri, rv7$min, as.integer(input$baseline7), harvest, fmort)
  })
  
  fmort8 <- reactive({
    if (!is.null(stk8) & !is.null(rv8$tri) & !is.null(rv8$gsa) & !is.null(rv8$min))
      procDfLongMult(stk8, rv8$gsa, rv8$tri, rv8$min, as.integer(input$baseline8), harvest, fmort)
  })
  
  fmort9 <- reactive({
    if (!is.null(stk9) & !is.null(rv9$tri) & !is.null(rv9$gsa) & !is.null(rv9$min))
      procDfLongMult(stk9, rv9$gsa, rv9$tri, rv9$min, as.integer(input$baseline9), harvest, fmort)
  })
  
  fmort10 <- reactive({
    if (!is.null(stk10) & !is.null(rv10$tri) & !is.null(rv10$gsa) & !is.null(rv10$min))
      procDfLongMult(stk10, rv10$gsa, rv10$tri, rv10$min, as.integer(input$baseline10), harvest, fmort)
  })
  
  observeEvent(input$loadButton, {
    if (is.null(fmort1()) &
        is.null(fmort2()) &
        is.null(fmort3()) &
        is.null(fmort4()) &
        is.null(fmort5()) &
        is.null(fmort6()) &
        is.null(fmort7()) &
        is.null(fmort8()) &
        is.null(fmort9()) &
        is.null(fmort10()))
      return(NULL)
    if (!is.null(fmort1())) {fmorts[[1]] <- fmort1()}
    if (!is.null(fmort2())) {fmorts[[2]] <- fmort2()}
    if (!is.null(fmort3())) {fmorts[[3]] <- fmort3()}
    if (!is.null(fmort4())) {fmorts[[4]] <- fmort4()}
    if (!is.null(fmort5())) {fmorts[[5]] <- fmort5()}
    if (!is.null(fmort6())) {fmorts[[6]] <- fmort6()}
    if (!is.null(fmort7())) {fmorts[[7]] <- fmort7()}
    if (!is.null(fmort8())) {fmorts[[8]] <- fmort8()}
    if (!is.null(fmort9())) {fmorts[[9]] <- fmort9()}
    if (!is.null(fmort10())) {fmorts[[10]] <- fmort10()}
    
    fmorts <<- fmorts
    
    if (length(fmorts) > 0) {
      fmort_l <<- do.call(totDf, fmorts)
      fmort_w <<- procDfWide(fmort_l, fmort, F)
    }
  })
  
  ##### FMORT SPAWN #####
  
  fmort_spawn1 <- reactive({
    if (!is.null(stk1) & !is.null(rv1$tri) & !is.null(rv1$gsa) & !is.null(rv1$min))
      procDfLongMult(stk1, rv1$gsa, rv1$tri, rv1$min, as.integer(input$baseline1), harvest.spwn, fmort_spawn)
  })
  
  fmort_spawn2 <- reactive({
    if (!is.null(stk2) & !is.null(rv2$tri) & !is.null(rv2$gsa) & !is.null(rv2$min))
      procDfLongMult(stk2, rv2$gsa, rv2$tri, rv2$min, as.integer(input$baseline2), harvest.spwn, fmort_spawn)
  })
  
  fmort_spawn3 <- reactive({
    if (!is.null(stk3) & !is.null(rv3$tri) & !is.null(rv3$gsa) & !is.null(rv3$min))
      procDfLongMult(stk3, rv3$gsa, rv3$tri, rv3$min, as.integer(input$baseline3), harvest.spwn, fmort_spawn)
  })
  
  fmort_spawn4 <- reactive({
    if (!is.null(stk4) & !is.null(rv4$tri) & !is.null(rv4$gsa) & !is.null(rv4$min))
      procDfLongMult(stk4, rv4$gsa, rv4$tri, rv4$min, as.integer(input$baseline4), harvest.spwn, fmort_spawn)
  })
  
  fmort_spawn5 <- reactive({
    if (!is.null(stk5) & !is.null(rv5$tri) & !is.null(rv5$gsa) & !is.null(rv5$min))
      procDfLongMult(stk5, rv5$gsa, rv5$tri, rv5$min, as.integer(input$baseline5), harvest.spwn, fmort_spawn)
  })
  
  fmort_spawn6 <- reactive({
    if (!is.null(stk6) & !is.null(rv6$tri) & !is.null(rv6$gsa) & !is.null(rv6$min))
      procDfLongMult(stk6, rv6$gsa, rv6$tri, rv6$min, as.integer(input$baseline6), harvest.spwn, fmort_spawn)
  })
  
  fmort_spawn7 <- reactive({
    if (!is.null(stk7) & !is.null(rv7$tri) & !is.null(rv7$gsa) & !is.null(rv7$min))
      procDfLongMult(stk7, rv7$gsa, rv7$tri, rv7$min, as.integer(input$baseline7), harvest.spwn, fmort_spawn)
  })
  
  fmort_spawn8 <- reactive({
    if (!is.null(stk8) & !is.null(rv8$tri) & !is.null(rv8$gsa) & !is.null(rv8$min))
      procDfLongMult(stk8, rv8$gsa, rv8$tri, rv8$min, as.integer(input$baseline8), harvest.spwn, fmort_spawn)
  })
  
  fmort_spawn9 <- reactive({
    if (!is.null(stk9) & !is.null(rv9$tri) & !is.null(rv9$gsa) & !is.null(rv9$min))
      procDfLongMult(stk9, rv9$gsa, rv9$tri, rv9$min, as.integer(input$baseline9), harvest.spwn, fmort_spawn)
  })
  
  fmort_spawn10 <- reactive({
    if (!is.null(stk10) & !is.null(rv10$tri) & !is.null(rv10$gsa) & !is.null(rv10$min))
      procDfLongMult(stk10, rv10$gsa, rv10$tri, rv10$min, as.integer(input$baseline10), harvest.spwn, fmort_spawn)
  })
  
  observeEvent(input$loadButton, {
    if (is.null(fmort_spawn1()) &
        is.null(fmort_spawn2()) &
        is.null(fmort_spawn3()) &
        is.null(fmort_spawn4()) &
        is.null(fmort_spawn5()) &
        is.null(fmort_spawn6()) &
        is.null(fmort_spawn7()) &
        is.null(fmort_spawn8()) &
        is.null(fmort_spawn9()) &
        is.null(fmort_spawn10()))
      return(NULL)
    if (!is.null(fmort_spawn1())) {fmort_spawns[[1]] <- fmort_spawn1()}
    if (!is.null(fmort_spawn2())) {fmort_spawns[[2]] <- fmort_spawn2()}
    if (!is.null(fmort_spawn3())) {fmort_spawns[[3]] <- fmort_spawn3()}
    if (!is.null(fmort_spawn4())) {fmort_spawns[[4]] <- fmort_spawn4()}
    if (!is.null(fmort_spawn5())) {fmort_spawns[[5]] <- fmort_spawn5()}
    if (!is.null(fmort_spawn6())) {fmort_spawns[[6]] <- fmort_spawn6()}
    if (!is.null(fmort_spawn7())) {fmort_spawns[[7]] <- fmort_spawn7()}
    if (!is.null(fmort_spawn8())) {fmort_spawns[[8]] <- fmort_spawn8()}
    if (!is.null(fmort_spawn9())) {fmort_spawns[[9]] <- fmort_spawn9()}
    if (!is.null(fmort_spawn10())) {fmort_spawns[[10]] <- fmort_spawn10()}
    
    fmort_spawns <<- fmort_spawns
    
    if (length(fmort_spawns) > 0) {
      fmort_spawn_l <<- do.call(totDf, fmort_spawns)
      fmort_spawn_w <<- procDfWide(fmort_spawn_l, fmort_spawn, J)
    }
  })
  
  ##### MORT #####
  
  mort1 <- reactive({
    if (!is.null(stk1) & !is.null(rv1$tri) & !is.null(rv1$gsa) & !is.null(rv1$min))
      procDfLongMult(stk1, rv1$gsa, rv1$tri, rv1$min, as.integer(input$baseline1), m, mort)
  })
  
  mort2 <- reactive({
    if (!is.null(stk2) & !is.null(rv2$tri) & !is.null(rv2$gsa) & !is.null(rv2$min))
      procDfLongMult(stk2, rv2$gsa, rv2$tri, rv2$min, as.integer(input$baseline2), m, mort)
  })
  
  mort3 <- reactive({
    if (!is.null(stk3) & !is.null(rv3$tri) & !is.null(rv3$gsa) & !is.null(rv3$min))
      procDfLongMult(stk3, rv3$gsa, rv3$tri, rv3$min, as.integer(input$baseline3), m, mort)
  })
  
  mort4 <- reactive({
    if (!is.null(stk4) & !is.null(rv4$tri) & !is.null(rv4$gsa) & !is.null(rv4$min))
      procDfLongMult(stk4, rv4$gsa, rv4$tri, rv4$min, as.integer(input$baseline4), m, mort)
  })
  
  mort5 <- reactive({
    if (!is.null(stk5) & !is.null(rv5$tri) & !is.null(rv5$gsa) & !is.null(rv5$min))
      procDfLongMult(stk5, rv5$gsa, rv5$tri, rv5$min, as.integer(input$baseline5), m, mort)
  })
  
  mort6 <- reactive({
    if (!is.null(stk6) & !is.null(rv6$tri) & !is.null(rv6$gsa) & !is.null(rv6$min))
      procDfLongMult(stk6, rv6$gsa, rv6$tri, rv6$min, as.integer(input$baseline6), m, mort)
  })
  
  mort7 <- reactive({
    if (!is.null(stk7) & !is.null(rv7$tri) & !is.null(rv7$gsa) & !is.null(rv7$min))
      procDfLongMult(stk7, rv7$gsa, rv7$tri, rv7$min, as.integer(input$baseline7), m, mort)
  })
  
  mort8 <- reactive({
    if (!is.null(stk8) & !is.null(rv8$tri) & !is.null(rv8$gsa) & !is.null(rv8$min))
      procDfLongMult(stk8, rv8$gsa, rv8$tri, rv8$min, as.integer(input$baseline8), m, mort)
  })
  
  mort9 <- reactive({
    if (!is.null(stk9) & !is.null(rv9$tri) & !is.null(rv9$gsa) & !is.null(rv9$min))
      procDfLongMult(stk9, rv9$gsa, rv9$tri, rv9$min, as.integer(input$baseline9), m, mort)
  })
  
  mort10 <- reactive({
    if (!is.null(stk10) & !is.null(rv10$tri) & !is.null(rv10$gsa) & !is.null(rv10$min))
      procDfLongMult(stk10, rv10$gsa, rv10$tri, rv10$min, as.integer(input$baseline10), m, mort)
  })
  
  observeEvent(input$loadButton, {
    if (is.null(mort1()) &
        is.null(mort2()) &
        is.null(mort3()) &
        is.null(mort4()) &
        is.null(mort5()) &
        is.null(mort6()) &
        is.null(mort7()) &
        is.null(mort8()) &
        is.null(mort9()) &
        is.null(mort10()))
      return(NULL)
    if (!is.null(mort1())) {morts[[1]] <- mort1()}
    if (!is.null(mort2())) {morts[[2]] <- mort2()}
    if (!is.null(mort3())) {morts[[3]] <- mort3()}
    if (!is.null(mort4())) {morts[[4]] <- mort4()}
    if (!is.null(mort5())) {morts[[5]] <- mort5()}
    if (!is.null(mort6())) {morts[[6]] <- mort6()}
    if (!is.null(mort7())) {morts[[7]] <- mort7()}
    if (!is.null(mort8())) {morts[[8]] <- mort8()}
    if (!is.null(mort9())) {morts[[9]] <- mort9()}
    if (!is.null(mort10())) {morts[[10]] <- mort10()}
    
    morts <<- morts
    
    if (length(morts) > 0) {
      mort_l <<- do.call(totDf, morts)
      mort_w <<- procDfWide(mort_l, mort, M)
    }
  })
  
  ##### MORT SPAWN #####
  
  mort_spawn1 <- reactive({
    if (!is.null(stk1) & !is.null(rv1$tri) & !is.null(rv1$gsa) & !is.null(rv1$min))
      procDfLongMult(stk1, rv1$gsa, rv1$tri, rv1$min, as.integer(input$baseline1), m.spwn, mort_spawn)
  })
  
  mort_spawn2 <- reactive({
    if (!is.null(stk2) & !is.null(rv2$tri) & !is.null(rv2$gsa) & !is.null(rv2$min))
      procDfLongMult(stk2, rv2$gsa, rv2$tri, rv2$min, as.integer(input$baseline2), m.spwn, mort_spawn)
  })
  
  mort_spawn3 <- reactive({
    if (!is.null(stk3) & !is.null(rv3$tri) & !is.null(rv3$gsa) & !is.null(rv3$min))
      procDfLongMult(stk3, rv3$gsa, rv3$tri, rv3$min, as.integer(input$baseline3), m.spwn, mort_spawn)
  })
  
  mort_spawn4 <- reactive({
    if (!is.null(stk4) & !is.null(rv4$tri) & !is.null(rv4$gsa) & !is.null(rv4$min))
      procDfLongMult(stk4, rv4$gsa, rv4$tri, rv4$min, as.integer(input$baseline4), m.spwn, mort_spawn)
  })
  
  mort_spawn5 <- reactive({
    if (!is.null(stk5) & !is.null(rv5$tri) & !is.null(rv5$gsa) & !is.null(rv5$min))
      procDfLongMult(stk5, rv5$gsa, rv5$tri, rv5$min, as.integer(input$baseline5), m.spwn, mort_spawn)
  })
  
  mort_spawn6 <- reactive({
    if (!is.null(stk6) & !is.null(rv6$tri) & !is.null(rv6$gsa) & !is.null(rv6$min))
      procDfLongMult(stk6, rv6$gsa, rv6$tri, rv6$min, as.integer(input$baseline6), m.spwn, mort_spawn)
  })
  
  mort_spawn7 <- reactive({
    if (!is.null(stk7) & !is.null(rv7$tri) & !is.null(rv7$gsa) & !is.null(rv7$min))
      procDfLongMult(stk7, rv7$gsa, rv7$tri, rv7$min, as.integer(input$baseline7), m.spwn, mort_spawn)
  })
  
  mort_spawn8 <- reactive({
    if (!is.null(stk8) & !is.null(rv8$tri) & !is.null(rv8$gsa) & !is.null(rv8$min))
      procDfLongMult(stk8, rv8$gsa, rv8$tri, rv8$min, as.integer(input$baseline8), m.spwn, mort_spawn)
  })
  
  mort_spawn9 <- reactive({
    if (!is.null(stk9) & !is.null(rv9$tri) & !is.null(rv9$gsa) & !is.null(rv9$min))
      procDfLongMult(stk9, rv9$gsa, rv9$tri, rv9$min, as.integer(input$baseline9), m.spwn, mort_spawn)
  })
  
  mort_spawn10 <- reactive({
    if (!is.null(stk10) & !is.null(rv10$tri) & !is.null(rv10$gsa) & !is.null(rv10$min))
      procDfLongMult(stk10, rv10$gsa, rv10$tri, rv10$min, as.integer(input$baseline10), m.spwn, mort_spawn)
  })
  
  observeEvent(input$loadButton, {
    if (is.null(mort_spawn1()) &
        is.null(mort_spawn2()) &
        is.null(mort_spawn3()) &
        is.null(mort_spawn4()) &
        is.null(mort_spawn5()) &
        is.null(mort_spawn6()) &
        is.null(mort_spawn7()) &
        is.null(mort_spawn8()) &
        is.null(mort_spawn9()) &
        is.null(mort_spawn10()))
      return(NULL)
    if (!is.null(mort_spawn1())) {mort_spawns[[1]] <- mort_spawn1()}
    if (!is.null(mort_spawn2())) {mort_spawns[[2]] <- mort_spawn2()}
    if (!is.null(mort_spawn3())) {mort_spawns[[3]] <- mort_spawn3()}
    if (!is.null(mort_spawn4())) {mort_spawns[[4]] <- mort_spawn4()}
    if (!is.null(mort_spawn5())) {mort_spawns[[5]] <- mort_spawn5()}
    if (!is.null(mort_spawn6())) {mort_spawns[[6]] <- mort_spawn6()}
    if (!is.null(mort_spawn7())) {mort_spawns[[7]] <- mort_spawn7()}
    if (!is.null(mort_spawn8())) {mort_spawns[[8]] <- mort_spawn8()}
    if (!is.null(mort_spawn9())) {mort_spawns[[9]] <- mort_spawn9()}
    if (!is.null(mort_spawn10())) {mort_spawns[[10]] <- mort_spawn10()}
    
    mort_spawns <<- mort_spawns
    
    if (length(mort_spawns) > 0) {
      mort_spawn_l <<- do.call(totDf, mort_spawns)
      mort_spawn_w <<- procDfWide(mort_spawn_l, mort_spawn, K)
    }
  })
  
  ##### MATURE RATIO #####
  
  mature1 <- reactive({
    if (!is.null(stk1) & !is.null(rv1$tri) & !is.null(rv1$gsa) & !is.null(rv1$min))
      procDfLongMult(stk1, rv1$gsa, rv1$tri, rv1$min, as.integer(input$baseline1), mat, mature)
  })
  
  mature2 <- reactive({
    if (!is.null(stk2) & !is.null(rv2$tri) & !is.null(rv2$gsa) & !is.null(rv2$min))
      procDfLongMult(stk2, rv2$gsa, rv2$tri, rv2$min, as.integer(input$baseline2), mat, mature)
  })
  
  mature3 <- reactive({
    if (!is.null(stk3) & !is.null(rv3$tri) & !is.null(rv3$gsa) & !is.null(rv3$min))
      procDfLongMult(stk3, rv3$gsa, rv3$tri, rv3$min, as.integer(input$baseline3), mat, mature)
  })
  
  mature4 <- reactive({
    if (!is.null(stk4) & !is.null(rv4$tri) & !is.null(rv4$gsa) & !is.null(rv4$min))
      procDfLongMult(stk4, rv4$gsa, rv4$tri, rv4$min, as.integer(input$baseline4), mat, mature)
  })
  
  mature5 <- reactive({
    if (!is.null(stk5) & !is.null(rv5$tri) & !is.null(rv5$gsa) & !is.null(rv5$min))
      procDfLongMult(stk5, rv5$gsa, rv5$tri, rv5$min, as.integer(input$baseline5), mat, mature)
  })
  
  mature6 <- reactive({
    if (!is.null(stk6) & !is.null(rv6$tri) & !is.null(rv6$gsa) & !is.null(rv6$min))
      procDfLongMult(stk6, rv6$gsa, rv6$tri, rv6$min, as.integer(input$baseline6), mat, mature)
  })
  
  mature7 <- reactive({
    if (!is.null(stk7) & !is.null(rv7$tri) & !is.null(rv7$gsa) & !is.null(rv7$min))
      procDfLongMult(stk7, rv7$gsa, rv7$tri, rv7$min, as.integer(input$baseline7), mat, mature)
  })
  
  mature8 <- reactive({
    if (!is.null(stk8) & !is.null(rv8$tri) & !is.null(rv8$gsa) & !is.null(rv8$min))
      procDfLongMult(stk8, rv8$gsa, rv8$tri, rv8$min, as.integer(input$baseline8), mat, mature)
  })
  
  mature9 <- reactive({
    if (!is.null(stk9) & !is.null(rv9$tri) & !is.null(rv9$gsa) & !is.null(rv9$min))
      procDfLongMult(stk9, rv9$gsa, rv9$tri, rv9$min, as.integer(input$baseline9), mat, mature)
  })
  
  mature10 <- reactive({
    if (!is.null(stk10) & !is.null(rv10$tri) & !is.null(rv10$gsa) & !is.null(rv10$min))
      procDfLongMult(stk10, rv10$gsa, rv10$tri, rv10$min, as.integer(input$baseline10), mat, mature)
  })
  
  observeEvent(input$loadButton, {
    if (is.null(mature1()) &
        is.null(mature2()) &
        is.null(mature3()) &
        is.null(mature4()) &
        is.null(mature5()) &
        is.null(mature6()) &
        is.null(mature7()) &
        is.null(mature8()) &
        is.null(mature9()) &
        is.null(mature10()))
      return(NULL)
    if (!is.null(mature1())) {matures[[1]] <- mature1()}
    if (!is.null(mature2())) {matures[[2]] <- mature2()}
    if (!is.null(mature3())) {matures[[3]] <- mature3()}
    if (!is.null(mature4())) {matures[[4]] <- mature4()}
    if (!is.null(mature5())) {matures[[5]] <- mature5()}
    if (!is.null(mature6())) {matures[[6]] <- mature6()}
    if (!is.null(mature7())) {matures[[7]] <- mature7()}
    if (!is.null(mature8())) {matures[[8]] <- mature8()}
    if (!is.null(mature9())) {matures[[9]] <- mature9()}
    if (!is.null(mature10())) {matures[[10]] <- mature10()}
    
    matures <<- matures
    
    if (length(matures) > 0) {
      mature_l <<- do.call(totDf, matures)
      mature_w <<- procDfWide(mature_l, mature, L)
    }
    
    if (length(pops_l) > 0) {neuralNetInputs <<- procInputs(pops_w, catches_w)}
  })
  
  ##### BIOMASS MULTIPLIER #####
  
  biomassm1 <- reactive({
    if (!is.null(stk1) & !is.null(rv1$tri) & !is.null(rv1$gsa) & !is.null(rv1$min))
      procBiomassMultLong(stk1, rv1$gsa, rv1$tri, rv1$min, as.integer(input$baseline1))
  })
  
  biomassm2 <- reactive({
    if (!is.null(stk2) & !is.null(rv2$tri) & !is.null(rv2$gsa) & !is.null(rv2$min))
      procBiomassMultLong(stk2, rv2$gsa, rv2$tri, rv2$min, as.integer(input$baseline2))
  })
  
  biomassm3 <- reactive({
    if (!is.null(stk3) & !is.null(rv3$tri) & !is.null(rv3$gsa) & !is.null(rv3$min))
      procBiomassMultLong(stk3, rv3$gsa, rv3$tri, rv3$min, as.integer(input$baseline3))
  })
  
  biomassm4 <- reactive({
    if (!is.null(stk4) & !is.null(rv4$tri) & !is.null(rv4$gsa) & !is.null(rv4$min))
      procBiomassMultLong(stk4, rv4$gsa, rv4$tri, rv4$min, as.integer(input$baseline4))
  })
  
  biomassm5 <- reactive({
    if (!is.null(stk5) & !is.null(rv5$tri) & !is.null(rv5$gsa) & !is.null(rv5$min))
      procBiomassMultLong(stk5, rv5$gsa, rv5$tri, rv5$min, as.integer(input$baseline5))
  })
  
  biomassm6 <- reactive({
    if (!is.null(stk6) & !is.null(rv6$tri) & !is.null(rv6$gsa) & !is.null(rv6$min))
      procBiomassMultLong(stk6, rv6$gsa, rv6$tri, rv6$min, as.integer(input$baseline6))
  })
  
  biomassm7 <- reactive({
    if (!is.null(stk7) & !is.null(rv7$tri) & !is.null(rv7$gsa) & !is.null(rv7$min))
      procBiomassMultLong(stk7, rv7$gsa, rv7$tri, rv7$min, as.integer(input$baseline7))
  })
  
  biomassm8 <- reactive({
    if (!is.null(stk8) & !is.null(rv8$tri) & !is.null(rv8$gsa) & !is.null(rv8$min))
      procBiomassMultLong(stk8, rv8$gsa, rv8$tri, rv8$min, as.integer(input$baseline8))
  })
  
  biomassm9 <- reactive({
    if (!is.null(stk9) & !is.null(rv9$tri) & !is.null(rv9$gsa) & !is.null(rv9$min))
      procBiomassMultLong(stk9, rv9$gsa, rv9$tri, rv9$min, as.integer(input$baseline9))
  })
  
  biomassm10 <- reactive({
    if (!is.null(stk10) & !is.null(rv10$tri) & !is.null(rv10$gsa) & !is.null(rv10$min))
      procBiomassMultLong(stk10, rv10$gsa, rv10$tri, rv10$min, as.integer(input$baseline10))
  })
  
  observeEvent(input$loadButton, {
    if (is.null(biomassm1()) &
        is.null(biomassm2()) &
        is.null(biomassm3()) &
        is.null(biomassm4()) &
        is.null(biomassm5()) &
        is.null(biomassm6()) &
        is.null(biomassm7()) &
        is.null(biomassm8()) &
        is.null(biomassm9()) &
        is.null(biomassm10()))
      return(NULL)
    if (!is.null(biomassm1())) {biomassms[[1]] <- biomassm1()}
    if (!is.null(biomassm2())) {biomassms[[2]] <- biomassm2()}
    if (!is.null(biomassm3())) {biomassms[[3]] <- biomassm3()}
    if (!is.null(biomassm4())) {biomassms[[4]] <- biomassm4()}
    if (!is.null(biomassm5())) {biomassms[[5]] <- biomassm5()}
    if (!is.null(biomassm6())) {biomassms[[6]] <- biomassm6()}
    if (!is.null(biomassm7())) {biomassms[[7]] <- biomassm7()}
    if (!is.null(biomassm8())) {biomassms[[8]] <- biomassm8()}
    if (!is.null(biomassm9())) {biomassms[[9]] <- biomassm9()}
    if (!is.null(biomassm10())) {biomassms[[10]] <- biomassm10()}
    
    biomassms <<- biomassms
    
    if (length(biomassms) > 0) {
      biomassm_l <<- do.call(totDfBiomass, biomassms)
      biomassm_w <<- procBiomassMultWide(biomassm_l)
    }
  })
  
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
                      selected = 2 * n)
    
    updatePickerInput(session = session,
                      inputId = "neurons2",
                      choices = c(0, n, 2 * n, 4 * n),
                      selected = 0)
    
    updatePickerInput(session = session,
                      inputId = "neurons3",
                      choices = c(0, n, 2 * n, 4 * n),
                      selected = n)
    
    updatePickerInput(session = session,
                      inputId = "neurons4",
                      choices = c(0, n, 2 * n, 4 * n),
                      selected = 0)
    
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
      
      output$plotFit <- renderPlotly({
        ggplotly(testfit_plots)
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
        taylor_diagram[[i]] <<- plotTaylorDiagram(traintest_results[[i]], i)
      }
      
      output$showSpeciesTest <- renderText({
        species[[plotTestCount]]
      })
      
      output$plotTrainTest <- renderPlotly({
        ggplotly(traintest_plots[[plotTestCount]])
      })
      
      output$taylorDiagram <- renderPlot({
        plotTaylorDiagram(traintest_results[[plotTestCount]], plotTestCount)
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
        output$taylorDiagram <- renderPlot({
          plotTaylorDiagram(traintest_results[[plotTestCount]], plotTestCount)
        })
        output$showSpeciesTest <- renderText({
          species[[plotTestCount]]
        })
      } else {
        plotTestCount <<- plotTestCount - 1
        output$plotTrainTest <- renderPlotly({
          ggplotly(traintest_plots[[plotTestCount]])
        })
        output$taylorDiagram <- renderPlot({
          plotTaylorDiagram(traintest_results[[plotTestCount]], plotTestCount)
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
        output$taylorDiagram <- renderPlot({
          plotTaylorDiagram(traintest_results[[plotTestCount]], plotTestCount)
        })
        output$showSpeciesTest <- renderText({
          species[[plotTestCount]]
        })
      } else {
        plotTestCount <<- plotTestCount + 1
        output$plotTrainTest <- renderPlotly({
          ggplotly(traintest_plots[[plotTestCount]])
        })
        output$taylorDiagram <- renderPlot({
          plotTaylorDiagram(traintest_results[[plotTestCount]], plotTestCount)
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
      pred_results <<- predNet(neuralNetInputs, f_adj, as.integer(depth_pred), biomassm_w)
      removeModal()
    }
  })
  
  observeEvent(input$plotPredButton, {
    if (length(pred_results) > 0) {
      plotPredCount <<- 1
      
      for (i in 1:length(pred_results)) {
        pred_plots[[i]] <<- plotPred(pred_results[[i]], neuralNetInputs, i)
        ratio_plots[[i]] <<- plotRatio(pred_results[[i]], i)
      }
      
      output$plotPred <- renderPlotly({
        ggplotly(pred_plots[[plotPredCount]]
                 )
      })
      
      output$plotRatio <- renderPlot({
        ratio_plots[[plotPredCount]]
      }, height = 700)
      
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
          output$plotRatio <- renderPlot({
            ratio_plots[[plotPredCount]]
          }, height = 700)
          output$showSpeciesPred <- renderText({
            species[[plotPredCount]]
          })
        } else {
          plotPredCount <<- plotPredCount - 1
          output$plotPred <- renderPlotly({
            ggplotly(pred_plots[[plotPredCount]])
          })
          output$plotRatio <- renderPlot({
            ratio_plots[[plotPredCount]]
          }, height = 700)
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
          output$plotRatio <- renderPlot({
            ratio_plots[[plotPredCount]]
          }, height = 700)
          output$showSpeciesPred <- renderText({
            species[[plotPredCount]]
          })
        } else {
          plotPredCount <<- plotPredCount + 1
          output$plotPred <- renderPlotly({
            ggplotly(pred_plots[[plotPredCount]])
          })
          output$plotRatio <- renderPlot({
            ratio_plots[[plotPredCount]]
          }, height = 700)
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
          biomassms = biomassms,
          biomassm_l = biomassm_l,
          biomassm_w = biomassm_w,
          neuralNetInputs = neuralNetInputs,
          f_w = f_w,
          fmort_baseline = fmort_baseline,
          f_new = f_new,
          f_adj = f_adj,
          f_tot = f_tot,
          f_adj_display = f_adj_display,
          depth_test = depth_test,
          testfit_results = testfit_results,
          traintest_iter_results = traintest_iter_results,
          traintest_results = traintest_results,
          traintest_plots = traintest_plots,
          taylor_diagram = taylor_diagram,
          depth_pred = depth_pred,
          model_pred = model_pred,
          pred_iter_partial = pred_iter_partial,
          pred_results = pred_results,
          pred_plots = pred_plots,
          ratio_plots = ratio_plots,
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