# Maelstrom

Multi-species predictive stock assessment model based on a customizable neural network and built as a ShinyApp.

### Prerequisites

To run the ShinyApp you have to download all the files in this repository, open **server.R** and run all the libraries in the first lines of the script, nested within the **LIBRARIES** section.

> [!IMPORTANT]
> Some packages, listed below, require a slightly more complex installation.

To install the *FLR* package you can run the following line of code:

```         
install.packages(repos = c(FLR = "https://flr.r-universe.dev", CRAN = "https://cloud.r-project.org"))
```

as specified in the [FLR package site](https://flr-project.org/), and choose **FLR** from the libraries menù.

To install *Keras* and *Tensorflow* packages, you have to install first Python on your PC. You can download it on its [website](https://www.python.org/downloads/); it is advised to download the latest stable version, which usually is **two** versions before the current.

After installing Python, you can install the libraries **keras** and **tensorflow** via the RStudio installer, then you have to run the following functions in a fresh R session:

```         
install_tensorflow()
install_keras()
```

After that, both FLR and Keras/Tensorflow should be totally functioning.

Further instructions on how to use the ShinyApp can be found inside the application by clicking the **Instruction** button on the first screen.

> [!NOTE]
> To print the final report, you must have installed the latest version of [MikTeX](https://miktex.org/download) on your PC. During the installation, check the "all users" button, even if the compiler states that it's a bad idea. After the installation, open the MikTeX console and install all the updates, then open Rstudio.

### Data Formatting

You can find three sample input datasets in the repository folder **Sample Data**. These datasets are stock objects used during STECF EWG 23-09, and can be found in the relative [Annex](https://stecf.jrc.ec.europa.eu/documents/d/stecf/stecf-23-09-annex_i).

> [!WARNING]
> Due to code necessities, input stock objects must be formatted as following: 3ACODE_GSA1-GSA2-GSA3.rds or .rdata (e.g. for *Parapenaeus longirostris* in GSAs 9, 10 and 11, the file must be named DPS_9-10-11.rds).

### Authors

Matteo STEFANI (University of Rome Tor Vergata), Simone LIBRALATO (OGS), Cecilia PINTO (University of Genova), Tommaso RUSSO (University of Rome Tor Vergata, CoNiSMa, CNR-IRBIM)



