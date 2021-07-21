##' Cross type K function using homotypic and heterotypic case types
##'
##' A wrapper function of the \link[spatstat.core]{Kcross} function from the \pkg{spatstat.core} package (Baddeley et al. 2016) that takes epidemiological data used by \pkg{IDSpatialStats} functions and calculates the cross type K-function based on user defined case type homology
##'
##' @param epi.data a three-column numerical matrix that contains coordinates (\code{x} and \code{y}) for each case and information on case type (e.g. genotype or serotype). First two columns must be \code{x} and \code{y}
##' @param type an integer giving the column that contains information on case type. Must be an integer or a character
##' @param hom a scalar or vector giving the homotypic case type(s). Equivalent to the 'j' point type used in the cross K function. Must be an integer or character
##' @param het a scalar or vector giving the heterotypic case type(s). Equivalent to the 'i' point type used in the cross K function. The default is \code{NULL}, which uses any case type not defined in the \code{hom} argument as heterotypic. Must be an integer or a character
##' @param r a numeric vector giving the spatial distances
##' @param correction type of edge correction to be applied (default set to 'none'). See the \link[spatstat.core]{Kcross} function in the \pkg{spatstat.core} package for more details
##' 
##' @return a data frame with a minimum of three columns giving the radius (\code{r}), the theoretical value of the K function for a Poisson process (\code{theo}), and value of the K function evaluated at radius \code{r}. The column name gives the type of edge correction used 
##'
##' @author John Giles
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
                        correction='none' # type of edge correction to be applied
){
     
     if(is.matrix(epi.data) == FALSE | is.numeric(epi.data) == FALSE) stop('epi.data must be a numeric matrix')
     tmp <- type > ncol(epi.data); if(tmp == TRUE) stop('type column defined is out of bounds')

     xy <- epi.data[, 1:2, drop=FALSE]
     case.types <- epi.data[, type]
     unique.case.types <- unique(case.types)
     
     if(is.null(het))  het <- unique.case.types[!(unique.case.types %in% hom)]
     if(all(hom %in% unique.case.types) == FALSE | all(het %in% unique.case.types) == FALSE) 
          stop('all homotypic and heterotypic case types are not found in case type data') 
     
     message("Calculating cross type K function for:")
     message(paste(c("Homotypic case type(s):", hom, paste("(n=", length(which(case.types %in% hom)), ")", sep="")), 
                   collapse=" "))
     message(paste(c("Heterotypic case type(s):", het, paste("(n=", length(which(case.types %in% het)), ")", sep="")), 
                   collapse=" "))
     
     excl <- unique.case.types[which(!(unique.case.types %in% c(hom, het)))]
     message(paste(c("Excluded case type(s):", excl, paste("(n=", length(which(case.types %in% excl)), ")", sep="")), 
                   collapse=" "))

     case.marks <- rep(NA, length(case.types))
     case.marks[which(case.types %in% het)] <- 0
     case.marks[which(case.types %in% hom)] <- 1
     
     ppp.dat <- cbind(xy, case.types, case.marks)[complete.cases(case.marks),]
     
     epi.data.ppp <- spatstat.geom::ppp(xy[,1], xy[,2], 
                                        spatstat.geom::bounding.box.xy(xy), 
                                        marks=as.factor(ppp.dat[,ncol(ppp.dat)]),
                                        check=FALSE)

     return(as.data.frame(spatstat.core::Kcross(X=epi.data.ppp, i=0, j=1, r=r, correction=correction)))
}

##' Cross type Pair Correlation Function using homotypic and heterotypic case types
##'
##' A wrapper function of the \link[spatstat.core]{pcf} function from the \pkg{spatstat.core} package (Baddeley et al. 2016) that takes epidemiological data used by \pkg{IDSpatialStats} functions and calculates the cross type Pair Correlation Function based on user defined case type homology
##'
##' @param epi.data a three-column numerical matrix that contains coordinates (\code{x} and \code{y}) for each case and information on case type (e.g. genotype or serotype). First two columns must be \code{x} and \code{y}
##' @param type an integer giving the column that contains information on case type. Must be an integer or a character
##' @param hom a scalar or vector giving the homotypic case type(s). Equivalent to the 'j' point type used in the cross K function. Must be an integer or character
##' @param het a scalar or vector giving the heterotypic case type(s). Equivalent to the 'i' point type used in the cross K function. The default is \code{NULL}, which uses any case type not defined in the \code{hom} argument as heterotypic. Must be an integer or a character
##' @param r a numeric vector giving the spatial distances
##' @param correction type of edge correction to be applied (default set to 'none'). See the \link[spatstat.core]{pcf} function in the \pkg{spatstat.core} package for more details
##' 
##' @return a data frame with two columns giving the radius \code{r}, the theoretical value of the Pair Correlation Function for a Poisson process (\code{theo}), and value of the Pair Correlation Function \code{pcf}
##'
##' @author John Giles
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
                          correction='none' # type of edge correction to be applied
){
     
     if(is.matrix(epi.data) == FALSE | is.numeric(epi.data) == FALSE) stop('epi.data must be a numeric matrix')
     tmp <- type > ncol(epi.data); if(tmp == TRUE) stop('type column defined is out of bounds')
     
     xy <- epi.data[, 1:2, drop=FALSE]
     case.types <- epi.data[, type]
     unique.case.types <- unique(case.types)
     
     if(is.null(het))  het <- unique.case.types[!(unique.case.types %in% hom)]
     if(all(hom %in% unique.case.types) == FALSE | all(het %in% unique.case.types) == FALSE) 
          stop('all homotypic and heterotypic case types are not found in case type data') 
     
     message("Calculating cross type PCF function for:")
     message(paste(c("Homotypic case type(s):", hom, paste("(n=", length(which(case.types %in% hom)), ")", sep="")), 
                   collapse=" "))
     message(paste(c("Heterotypic case type(s):", het, paste("(n=", length(which(case.types %in% het)), ")", sep="")), 
                   collapse=" "))
     
     excl <- unique.case.types[which(!(unique.case.types %in% c(hom, het)))]
     message(paste(c("Excluded case type(s):", excl, paste("(n=", length(which(case.types %in% excl)), ")", sep="")), 
                   collapse=" "))
     
     case.marks <- rep(NA, length(case.types))
     case.marks[which(case.types %in% het)] <- 0
     case.marks[which(case.types %in% hom)] <- 1
     
     ppp.dat <- cbind(xy, case.types, case.marks)[complete.cases(case.marks),]
     
     epi.data.ppp <- spatstat.geom::ppp(xy[,1], xy[,2], 
                                        spatstat.geom::bounding.box.xy(xy), 
                                        marks=as.factor(ppp.dat[,ncol(ppp.dat)]),
                                        check=FALSE)
     
     return(as.data.frame(spatstat.core::pcf(spatstat.core::pcf(X=epi.data.ppp, i=0, j=1, r=r, correction=correction), spar=0.8, method='c')))
}

