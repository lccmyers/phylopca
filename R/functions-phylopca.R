
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' @title A function that runs a PCA on a user-defined taxonomic family
#' @description This function allows you to compare numerical data within a  taxonomic family
#' @param data dataframe including numerical data as well as information on taxonomic family and genus
#' @param family.name family name as it appears in the dataframe
#' @keywords PCA, taxonomic
#' @export
#' @examples
#' pca.family(data = d, family.name = "Cercopithecidae")

pca.family<-function(data, family.name){
  data<-na.omit(data)
  dfam<-filter(data, Family==family.name)
  pcafam<-prcomp(Filter(is.numeric, dfam), center=T, scale=T)
  return(summary(pcafam))
}


#' @title A function that runs a PCA on a user-defined taxonomic superfamily
#' @description This function allows you to compare numerical data within a  taxonomic superfamily
#' @param data dataframe including numerical data as well as information on taxonomic superfamily and family
#' @param superfamily.name superfamily name as it appears in the dataframe
#' @keywords PCA, taxonomic
#' @export
#' @examples
#' pca.superfamily(data = d, superfamily.name = "Cebioidea")

pca.superfamily<-function(data, superfamily.name){
  data<-na.omit(data)
  dsup<-filter(data, Superfamily==superfamily.name)
  pcasup<-prcomp(Filter(is.numeric, dsup), center=T, scale=T)
  return(summary(pcasup))
}

#' @title A function that graphs a PCA run on a user-defined taxonomic family
#' @description This is a sister function to pca.family() allowing you to visualize the analysis
#' @param data dataframe including numerical data as well as information on taxonomic family and genus
#' @param family.name family name as it appears in the dataframe
#' @keywords PCA, taxonomic, graph
#' @export
#' @examples
#' graph.family(data = d, family.name = "Cercopithecidae")

graph.family<-function(data, family.name){
  data<-na.omit(data)
  dfam<-filter(data, Family==family.name)
  pcafam<-prcomp(Filter(is.numeric, dfam), center=T, scale=T)
  fviz_pca_ind(pcafam, geom.ind = "point", pointshape = 21,
  pointsize = 2, fill.ind = dfam$Genus, invisible = "quali", palette = "pal8",
  addEllipses = T, legend.title="Genus") + labs(title = "PCA", x = "PC1", y = "PC2")
}

#' @title A function that graphs a PCA run on a user-defined taxonomic superfamily
#' @description This is a sister function to pca.superfamily() allowing you to visualize the analysis
#' @param data dataframe including numerical data as well as information on taxonomic superfamily and family
#' @param superfamily.name superfamily name as it appears in the dataframe
#' @keywords PCA, taxonomic, graph
#' @export
#' @examples
#' graph.superfamily(data = d, superfamily.name = "Ceboidea")

graph.superfamily<-function(data, superfamily.name){
  data<-na.omit(data)
  dsup<-filter(data, Superfamily==superfamily.name)
  pcasup<-prcomp(Filter(is.numeric, dsup), center=T, scale=T)
  fviz_pca_ind(pcasup, geom.ind = "point", pointshape = 21,
  pointsize = 2, fill.ind = dsup$Family, invisible = "quali", palette = "pal8",
  addEllipses = T, legend.title="Family") + labs(title = "PCA", x = "PC1", y = "PC2")
}
