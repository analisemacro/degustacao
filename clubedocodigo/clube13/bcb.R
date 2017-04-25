

library(SSOAP)
library(XML)
library(RCurl)

wsdl <- getURL("https://www3.bcb.gov.br/sgspub/JSP/sgsgeral/FachadaWSSGS.wsdl", 
               ssl.verifypeer = FALSE)
doc  <- xmlInternalTreeParse(wsdl)

def <- processWSDL(doc)
ff  <- genSOAPClientInterface(def = def)

inicio <- '01/01/1999'

fim <- '04/09/2015'

getSeries <- function(codigos, data.ini = inicio, data.fim = fim, remove.old = TRUE) {
  xmlstr <- ff@functions$getValoresSeriesXML(codigos, data.ini, data.fim, 
                                             .opts = list(ssl.verifypeer = FALSE))
  doc <- xmlInternalTreeParse(xmlstr)
  
  cleanup <- xpathApply(doc,"//SERIE", function(s) {
    id <- xmlGetAttr(s, "ID")
    s1 <- xmlSApply(s, function(x) xmlSApply(x, xmlValue))
    s1 <- t(s1)
    dimnames(s1) <- list(NULL, dimnames(s1)[[2]])
    df <- as.data.frame(s1, stringsAsFactors=FALSE)
    df$SERIE <- id
    df
  })
  df <- Reduce(rbind, cleanup)
  
  df$data  <- as.Date(sapply(strsplit(df$DATA,  "/"),
                             function(x) paste(c(x[2:1], 1), collapse="-")), "%Y-%m-%d")
  df$valor <- as.numeric(df$VALOR)
  df$serie <- factor(df$SERIE)
  
  if(remove.old){
    df$BLOQUEADO <- NULL
    df$SERIE <- NULL
    df$DATA <- NULL
    df$VALOR <- NULL
  }
  df
}

