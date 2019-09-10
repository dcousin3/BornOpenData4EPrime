#' @title readBornOpenData.eprime2
#'
#' @description
#' This function reads data from E-Prime stored on GitHub
#' by the E-Prime package BornOpenData4Eprime. Note that the function
#' requires 10-30 seconds to retrive the information from GitHub. Also,
#' an internet connection must be effective.
#'
#' @usage
#' readBornOpenData.eprime2(owner, repository)
#'
#' @param owner is the GitHub member who created the repository
#' @param repository is the name of the repository on GitHub
#'
#' @return            the data of all participants x session in a data.frame
#'
#' @details
#'
#' @author Denis Cousineau, \email{denis.cousineau@@uottawa.ca}
#' @keywords Born Open Data, E-Prime
#'
#' @examples
#' # Assume a user on GitHub called VIC-Laboratory-ExperimentalData
#' # and a repository created by that user called Test.
#' # You can check online at https://github.com/VIC-Laboratory-ExperimentalData/Test
#' # that this user and this repository exists.
#'
#' # This will read all the data files (stored in the rawdata folder):
#' readBornOpenData.eprime2("VIC-Laboratory-ExperimentalData","Test")
#'
#' @import rvest
#' @import rprime
#' @import plyr
#'

# the main function
readBornOpenData.eprime2 <- function(
    owner,
    repository,
    verbose = FALSE
) {
    ## 0- required library
    require(rvest)
    require(rprime)
    require(plyr)

    ## 1- gettting subjectslog.txt for exp name, subjects, sessions 
    verboseprint(paste("Repository is https://github.com",owner, repository, sep="/"), verbose)
    repo <- paste("https://github.com/", owner, repository, "raw/master", sep = "/")
    
    verboseprint("Fetching subjectsLog.txt...", verbose)
    file0 <- "subjectsLog.txt"
    myurl0  <- paste(repo, file0, sep="/")
     
    synopsis <- html_text(read_html(myurl0))
    synopsis <- as.data.frame(matrix(strsplit(synopsis, "\n|\t")[[1]], ncol=6, byrow=T))
    allfiles <-paste(synopsis$V1,synopsis$V2,synopsis$V3, sep = "-")
    verboseprint(paste("Found Experiments:", paste(unique(synopsis$V1),collapse=", ") , sep = " "), verbose)
    verboseprint(paste("  with subject numbers:", paste(unique(synopsis$V2),collapse=", "), sep = " "), verbose)
    verboseprint(paste("  and session numbers: ", paste(unique(synopsis$V3),collapse=", "), sep = " "), verbose)

    ## 2- preparing a list of data.frame, one per participant x session
    alldata = list()
    i= 1

    ## 3- reading all the files one-by-one
    for (file in allfiles) {
        ## 3.1- reading the txt file into ecnt
        verboseprint(paste("Importing ", file, "...", sep = ""), verbose)
        dtafle = paste(repo, "rawdata", paste(file,"txt", sep = "."), sep="/") 
        ecnt <- html_text(read_html(dtafle))

        ## 3.2- cutting ecnt at every \r\n into a vector of strings
        elog     <- strsplit(ecnt, "\r\n")[[1]]
        clk_line <- pmatch("Clock.Information: ", elog)
        elog     <- elog[!(1:length(elog)==clk_line)]
        # these two lines needed?
        attr(elog, "basename") <- file
        class(elog) <- c("EprimeLines", "character")

        ## 3.3- breaking down the text files into "layers" of informations with rprime functions
        elist  <- FrameList(elog)
        level1 <- keep_levels(elist, 1)
        edf1   <- to_data_frame(level1)
        level2 <- keep_levels(elist, 2)
        edf2   <- to_data_frame(level2)

        ## 3.4- removing from level 1 some unneeded information
        edf1b <- apply(edf1, 2, min, na.rm = TRUE)
        edf1c <- subset(data.frame(t(edf1b)), 
            select = -c(Eprime.Level, Eprime.LevelName, Eprime.Basename, Eprime.FrameNumber,
            Procedure, Running, VersionPersist, LevelName)
        )

        ## 3.5- removing from level 2 some unneeded information
        edf2c <- subset(edf2, 
            select = -c(Eprime.Level, Eprime.Basename, Eprime.FrameNumber)
        )

        ## 3.6- getting the number of trials in level2 information
        ntrials <- dim(edf2c)[1]
        ncolumns <- length(edf1c)

        ## 3.7- replicating edf1c ntrials times
        edf1c[2:ntrials, 1:ncolumns] <- edf1c

        ## 3.8- concatenating level-1 and level-2 information
        alldata[[i]] <- cbind(edf1c, edf2c)  
        i <- i+1
    }

    ## 4- merging the individual-subjects data.frame into a master data.frame
    plyr::rbind.fill(alldata)
}
#' @export


# a small function to facilitate the display of feedback
verboseprint <- function(msg, verbose) {
  if (verbose == TRUE) 
    cat(msg)
    cat("\n")
}
#' @noexport
