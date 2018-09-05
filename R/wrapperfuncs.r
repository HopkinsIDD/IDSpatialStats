##' Cross type K function using homotypic and heterotypic case types
##'
##' A wrapper function of the \link[spatstat]{Kcross} function from the \pkg{spatstat} package that takes epidemiological data used by \pkg{IDSpatialStats} functions and calculates the cross type K-function based on user defined case type homology
##'
##' @param epi.data a three-column numerical matrix that contains coordinates (\code{x} and \code{y}) for each case and information on case type (e.g. genotype or serotype). First two columns must be \code{x} and \code{y}
##' @param type an integer giving the column that contains information on case type. Must be an integer or a character
##' @param hom a scalar or vector giving the homotypic case type(s). Equivalent to the 'j' point type used in the cross K function. Must be an integer or character
##' @param het a scalar or vector giving the heterotypic case type(s). Equivalent to the 'i' point type used in the cross K function. The default is \code{NULL}, which uses any case type not defined in the \code{hom} argument as heterotypic. Must be an integer or a character
##' @param r a numeric vector giving the spatial distances
##' @param correction type of edge correction to be applied (default set to simple 'border' edge correction). See the \link[spatstat]{Kcross} function in the \pkg{spatstat} package for more details
##' 
##' @return a data frame with a minimum of two columns giving the radius \code{r} and value of the K function, where the titel of the column the type of edge correction used 
##'
##' @author Justin Lessler, Henrik Salje, and John Giles
##' 
##' @references
##' Baddeley A, Rubak E, and Turner R. (2016). "Spatial Point Patterns: Methodology and Applications with R". CRC Press.
##'
##' @example R/examples/get_cross_k.R
##'

get.cross.K <- function(epi.data, # matrix containing xy coordinates and case type
                        type, # column indicating case type
                        hom, # value of type that defines a homotypic case
                        het=NULL, # value of type that comprises all heterotypic cases
                        r=NULL, # vector of distance to evaluate the cross K function
                        correction='border' # type of edge correction to be applied
){
     
     if(is.matrix(epi.data) == FALSE && is.numeric(epi.data) == FALSE) stop('epi.data must be a numeric matrix')

     xy <- epi.data[, 1:2, drop=FALSE]
     case.types <- epi.data[, type]
     unique.case.types <- unique(case.types)
     
     if(is.null(het))  het <- unique.case.types[!(unique.case.types %in% hom)]
     
     hom <- as.numeric(hom)
     het <- as.numeric(het)
     
     message("Calculating cross type K function for:")
     message(paste(c("Homotypic case type(s):", hom, paste("(n=", length(which(case.types %in% hom)), ")", sep="")), 
                   collapse=" "))
     message(paste(c("Heterotypic case type(s):", het, paste("(n=", length(which(case.types %in% het)), ")", sep="")), 
                   collapse=" "))
     
     excl <- unique.case.types[which(!(unique.case.types %in% c(hom, het)))]
     message(paste(c("Excluded case type(s):", excl, paste("(n=", length(which(case.types %in% excl)), ")", sep="")), 
                   collapse=" "))

     if(all(hom %in% unique.case.types) == FALSE | all(het %in% unique.case.types) == FALSE) 
          stop('all homotypic and heterotypic case types are not found in case type data') 
     
     case.marks <- rep(NA, length(case.types))
     case.marks[which(case.types %in% het)] <- 0
     case.marks[which(case.types %in% hom)] <- 1
     
     ppp.dat <- cbind(xy, case.types, case.marks)[complete.cases(case.marks),]
     
     epi.data.ppp <- spatstat::as.ppp(ppp.dat, spatstat::bounding.box.xy(xy), check=FALSE)
     marks(epi.data.ppp) <- as.factor(ppp.dat[,ncol(ppp.dat)])

     return(as.data.frame(spatstat::Kcross(X=epi.data.ppp, i=0, j=1, r=r, correction=correction))[,-2])
}

##' Cross type Pair Correlation Function using homotypic and heterotypic case types
##'
##' A wrapper function of the \link[spatstat]{pcf} and \link[spatstat]{Kcross} functions from the \pkg{spatstat} package that takes epidemiological data used by \pkg{IDSpatialStats} functions and calculates the cross type Pair Correlation Function based on user defined case type homology
##'
##' @param epi.data a three-column numerical matrix that contains coordinates (\code{x} and \code{y}) for each case and information on case type (e.g. genotype or serotype). First two columns must be \code{x} and \code{y}
##' @param type an integer giving the column that contains information on case type. Must be an integer or a character
##' @param hom a scalar or vector giving the homotypic case type(s). Equivalent to the 'j' point type used in the cross K function. Must be an integer or character
##' @param het a scalar or vector giving the heterotypic case type(s). Equivalent to the 'i' point type used in the cross K function. The default is \code{NULL}, which uses any case type not defined in the \code{hom} argument as heterotypic. Must be an integer or a character
##' @param r a numeric vector giving the spatial distances
##' @param correction type of edge correction to be applied (default set to simple 'border' edge correction). See the \link[spatstat]{Kcross} function in the \pkg{spatstat} package for more details
##' 
##' @return a data frame with two columns giving the radius \code{r} and value of the Pair Correlation Function \code{pcf}
##'
##' @author Justin Lessler, Henrik Salje, and John Giles
##' 
##' @references
##' Baddeley A, Rubak E, and Turner R. (2016). "Spatial Point Patterns: Methodology and Applications with R". CRC Press.
##'
##' @example R/examples/get_cross_pcf.R
##' 

get.cross.PCF <- function(epi.data, # matrix containing xy coordinates and case type
                          type, # column indicating case type
                          hom, # value of type that defines a homotypic case
                          het=NULL, # value of type that comprises all heterotypic cases
                          r=NULL, # vector of distance to evaluate the cross K function
                          correction='border' # type of edge correction to be applied
){
     
     if(is.matrix(epi.data) == FALSE && is.numeric(epi.data) == FALSE) stop('epi.data must be a numeric matrix')
     
     xy <- epi.data[, 1:2, drop=FALSE]
     case.types <- epi.data[, type]
     unique.case.types <- unique(case.types)
     
     if(is.null(het))  het <- unique.case.types[!(unique.case.types %in% hom)]
     
     hom <- as.numeric(hom)
     het <- as.numeric(het)
     
     message("Calculating cross type Pair Correlation Function for:")
     message(paste(c("Homotypic case type(s):", hom, paste("(n=", length(which(case.types %in% hom)), ")", sep="")), 
                   collapse=" "))
     message(paste(c("Heterotypic case type(s):", het, paste("(n=", length(which(case.types %in% het)), ")", sep="")), 
                   collapse=" "))
     
     excl <- unique.case.types[which(!(unique.case.types %in% c(hom, het)))]
     message(paste(c("Excluded case type(s):", excl, paste("(n=", length(which(case.types %in% excl)), ")", sep="")), 
                   collapse=" "))
     
     if(all(hom %in% unique.case.types) == FALSE | all(het %in% unique.case.types) == FALSE) 
          stop('all homotypic and heterotypic case types are not found in case type data') 
     
     case.marks <- rep(NA, length(case.types))
     case.marks[which(case.types %in% het)] <- 0
     case.marks[which(case.types %in% hom)] <- 1
     
     ppp.dat <- cbind(xy, case.types, case.marks)[complete.cases(case.marks),]
     
     epi.data.ppp <- spatstat::as.ppp(ppp.dat, spatstat::bounding.box.xy(xy), check=FALSE)
     marks(epi.data.ppp) <- as.factor(ppp.dat[,ncol(ppp.dat)])
     
     return(as.data.frame(spatstat::pcf(spatstat::Kcross(X=epi.data.ppp, i=0, j=1, r=r, correction=correction), spar=1, method='b'))[,-2])
}