# Eyetracking Processing

This repo houses scripts for preprocessing and analysis of eyetracking data from Junghee Lee's lab at UAB

## Required Software

Download the following pieces of software first, each link goes directly to the download instructions.

- [R](https://mirror.las.iastate.edu/CRAN/) version: R >= 4.2.0 (latest preferred)
- [RStudio](https://posit.co/download/rstudio-desktop/) version: 2022.07.2-576 (or latest release)
- [Quarto](https://quarto.org/docs/get-started/)
- [Git](https://git-scm.com/downloads)

To use Git in Rstudio, you may need to set the path to the Git executable, especially if you are working on a Windows machine where Git is not automatically installed. To do so, perform the following steps:

1. Select Tools -> Global Options...
2. Select Git/SVN in the left side menu
3. If the Git executable path is empty, you will need to find and enter the executable path yourself. For a default Windows installation, the path will look something like `C:/Program Files/Git/bin/git.exe`. If you installed Git into a different directory, you will need to find the executable where you installed it. See [these instructions](https://happygitwithr.com/rstudio-see-git.html) for more details.

## Repository Installation

To access the code, it's suggested to clone the repository using RStudio's Project functionality. To do so, perform the following steps:

1. Click File -> New Project
2. Select Version Control -> Git
3. Enter the URL for the repository `https://github.com/mdefende/lee-eyetracking.git`
4. Enter the name of the folder you want the repository downloaded to
5. Select the parent directory for the repo directory

4 and 5 can be whatever you would like them to be. 4 is just what you want the name of the top-level directory to be, and 5 is the path where you want 4 saved to.

After entering these things, the repository should be automatically downloaded and set up for you.

### Package Environment

`renv` was used to save the package environment. You will need to download that package first using either the `install.packages` command or the `Install` button in RStudio's package window. After successful installation, run the following command:

`renv::restore('renv.lock')`

This will download and install all of the necessary packages needed to run the repository scripts.

## Usage

The entire repo can be run via the code/main.R script. Eyetracking and stimulus inputs should be saved in CSV format and their paths should be specified at the beginning of the script. Generated reports will be saved as HTML in the `reports` directory by default. Each generated HTML file will come with a folder of the same name containing the figures and other parts of the report, this folder should not be deleted and should be kept with the report for proper viewing.