#' @title readBornOpenData.eprime2
#'
#' @description
#' This function reads data from E-Prime stored on GitHub
#' by the E-Prime package BornOpenData4Eprime. Note that the function
#' requires 10-30 seconds to retrive the information from GitHub. Also,
#' an internet connection must be effective.
#'
#' @usage
#' readBornOpenData.eprime2(owner, repository, verbose = FALSE)
#'
#' @param owner is the GitHub member who created the repository
#' @param repository is the name of the repository on GitHub
#' @param verbose = TRUE displays complementary information.
#'
#' @return the data of all participants, all sessions in a data.frame
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
#' dta <- readBornOpenData.eprime2("VIC-Laboratory-ExperimentalData","Test")
#'
#' @import rvest
#' @import rprime
#' @import plyr
#'
#' @export

# the main function
readBornOpenData.eprime2 <- function(
    owner,
    repository,
    branch = "master",
    verbose = FALSE
) {
    ## 0- required library
    require(rvest)      # for read_html, html_text
    require(rprime)     # for FrameList, keep_levels, to_data_frame
    require(data.table) # for rbindlist, setnames

    ## 1- gettting subjectslog.txt for exp name, subjects, sessions 
    verboseprint(paste("Repository is https://github.com",owner, repository, sep="/"), verbose)
    repo <- paste("https://github.com/", owner, repository, "raw", branch, sep = "/")

    verboseprint("Fetching subjectsLog.txt...", verbose)
    file0 <- "subjectsLog.txt"
    myurl0  <- paste(repo, file0, sep="/")

    synopsis <- html_text(read_html(myurl0))
    synopsis <- read.table(text = synopsis)
    allfiles <-paste(synopsis$V1,synopsis$V2,synopsis$V3, sep = "-")

    verboseprint(paste("Found Experiments:", paste(unique(synopsis$V1),collapse=", ") , sep = " "), verbose)
    verboseprint(paste("  with subject numbers:", paste(synopsis$V2,collapse=", "), sep = " "), verbose)
    verboseprint(paste("  and session numbers: ", paste(synopsis$V3,collapse=", "), sep = " "), verbose)

    ## 2- preparing a list of data.frame, one per participant x session
    alldata <- list()
    index   <- 1

    ## 3- reading all the files one-by-one
    for (file in allfiles) {
        ## 3.1- reading the txt file into ecnt
        verboseprint(paste("Importing ", file, "...", sep = ""), verbose)
        dtafle = paste(repo, "rawdata", paste(file,"txt", sep = "."), sep="/") 
        ecnt <- html_text(read_html(dtafle))

        ## 3.2- cutting ecnt at every \r\n into a vector of strings
        # (instead of writing to a temp file then read with eprime_read which can't read over the web)
        elog     <- strsplit(ecnt, "\r\n")[[1]]
        clk_line <- pmatch("Clock.Information: ", elog)
        elog     <- elog[!(1:length(elog)==clk_line)]
        # these two lines needed
        attr(elog, "basename") <- file
        class(elog) <- c("EprimeLines", "character")

        ## 3.3- breaking down the text files into "layers" of informations with rprime functions
        elist  <- FrameList(elog)
        level1 <- keep_levels(elist, 1)
        level2 <- keep_levels(elist, 2) # there are   6 lines
        level3 <- keep_levels(elist, 3) # there are 648 lines

        ## 3.4- removing from level 1 some unneeded information
        # note that level 1 is always two lines (pre-initialiation and post-experiment)
        level1    <- modifyList( level1[[2]], level1[[1]] ) # merge the two lines
        level1_df <- clean_data_frame(to_data_frame(level1),"Experiment")


        if (length(keep_levels(elist, 2)) == 0) {
            # this experiment has only 1 layer?? nothing to do
            res = level1_df

        } else if (length(keep_levels(elist, 3)) == 0) {
            # this experiment has only 2 layers
            level1_df[2:length(level2), ] <- level1_df

            level2_df <- clean_data_frame(to_data_frame(level2),"Block")
            level12_df <- cbind(level1_df, level2_df)

            res <-  level12_df

        } else if (length(keep_levels(elist, 4)) == 0) {
            # this experiment has 3 layers
            level1_df[2:length(level3), ] <- level1_df

            level2_df <- clean_data_frame(to_data_frame(level2),"Block")
            level3_df <- clean_data_frame(to_data_frame(level3),"Trial")

            # get the number of trials in each level-2 blocks
            nt <- as.integer(level2[[1]]$Eprime.FrameNumber)-2
            for (i in 2:6) (nt[i] <- as.integer(level2[[i]]$Eprime.FrameNumber) - 
                as.integer(level2[[i-1]]$Eprime.FrameNumber)-1)

            # get the level 2 expanded by nt
            level2_ex <- level2_df[0,]
            for (i in 1:length(nt) ) {
              level2_ex[ (nrow(level2_ex )+1):(nrow(level2_ex )+nt[i]), ] = level2_df[i,]
            }

            level123_df <- cbind(level1_df, level2_ex, level3_df)
            res <-  level123_df

        } else {
            # this experiment has 4 layers or more
            verboseprint("   watchout! 4 levels or more are present; for exporting level 4 and beyond for this version contact the author", verbose)
        }

        ## 3.8- keep the file
        alldata[[index]] <-  res
        index <- index + 1
    }

    ## 4- merging the individual-subjects data.frame into a master data.frame
    res <- data.table::rbindlist(alldata, fill = TRUE, use.names = TRUE)
    res
}


# a small function to facilitate the display of feedback
verboseprint <- function(msg, verbose) {
  if (verbose == TRUE) {
    cat(msg)
    cat("\n")
} }


clean_data_frame <- function(df, post) {
    # change names to duplicate names
    res <- setnames(df, 
        old = c('Procedure','Running'),
        new = c(paste('Procedure',post,sep="."),paste('Running',post,sep="."))
    )
    # remove unneeded columns
    res <- subset(data.frame(res), 
        select = -c(Eprime.Level, Eprime.LevelName, Eprime.Basename, Eprime.FrameNumber)
    )
    res
}






