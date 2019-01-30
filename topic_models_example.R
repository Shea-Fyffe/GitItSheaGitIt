#' @title Topic Model example using ONET data
#' @author Shea Fyffe email: shea.fyffe@@gmail.com
#' @description An example script that uses text from ONET (see \link{https://www.onetonline.org/})
#' to develop topic models.
#' @seealso \code{vignette("topicmodels", package = "topicmodels")}
#' 

#Add some help functions and options, then load packages
#' @author Danielle Smith
#' @details \link{https://gist.github.com/smithdanielle/9913897}
source('E:/Jen\'s Stuff/R Utils/POS_script.R')
check.packages <- function(pkg){
  options(repos=structure(c(CRAN="http://cloud.r-project.org/")))
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) { 
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE, quietly = TRUE)
}
options(stringsAsFactors = FALSE)
#check packages
PKG <- c("tm", "topicmodels")
check.packages(PKG)

#####
#####
#TODO(shea.fyffe)
#Define Parameters#
topics <- 5

#' @section Data Import
#get text files you want to work with
FILES <- list.files()[-7]

#define columns that contain value text
COLUMNS <- list(DES = 3, DWA = 2, IWA = 4, TR = 1, WA = 2,  WC = 2, WS = 2)

#return list of data.frames each representing a file
DAT <- Map(import_text_data, path = FILES, columns = COLUMNS)

#' @section Clean Data
DAT <- lapply(DAT, function(X) clean_TT_corpora(X[,1]))

#' @section Convert to corpus
DAT <- lapply(DAT, function(X) tm::Corpus(tm::VectorSource(X)))

#' @section Convert to Document term matrix
DTM <- lapply(DAT, function(X) tm::DocumentTermMatrix(X), control = list(stemming = TRUE, stopwords = TRUE,
                                                          minWordLength = 2))
CDTM <- do.call(tm:::c.DocumentTermMatrix, DTM)
CDTM$v <- as.integer(CDTM$v)
CDTM[CDTM==0] <- 0.001
#' @section Run topic model
model <- topicmodels::LDA(CDTM, k = 5, method = "Gibbs", control = list(iter=500, seed = 0622))
