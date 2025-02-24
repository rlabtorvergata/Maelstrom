# Maelstrom

Multi-species stock assessment model based on a customizable neural network and built as a ShinyApp.

## ShinyApp

To run the ShinyApp you have to download all the files in this repository, open "server.R" and run all the libraries in the first lines of server.R. To install the FLR package you can run the following line of code:

```         
install.packages(repos = c(FLR = "https://flr.r-universe.dev", CRAN = "https://cloud.r-project.org"))
```

as specified in the [FLR package site](https://flr-project.org/).

Further instructions on how to use the ShinyApp can be found inside the application by clicking the "Instruction" button on the first screen.

> [!WARNING]
> To print the final report, you must have installed the latest version of [MikTeX](https://miktex.org/download) on your PC. During the installation, check the "all users" button, even if the compiler says that it's a bad idea. After the installation, open the MikTeX console and install all the updates, then open Rstudio.
