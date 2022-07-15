#' @title Plot intersecting set
#' @description
#' Plot intersecting set through an array
#' 
#' @importFrom dplyr %>%
#' @importFrom networkD3 sankeyNetwork
#' @importFrom htmlwidgets JS onRender saveWidget 
#' @importFrom webshot webshot
#' 
#' @name intersectSankey
#' @param x an array for constructing intersecting set 
#' @param out.fig print the figure into \code{"html"} or \code{"pdf"} file. Default is \code{NULL} with R graphics device
#' @param colors a vector of colors corresponding to individual tasks
#' @param step.names names of the three dimensions of the array \code{x}, i.e. names of multiple levels, intermediate variables and tasks.
#' Default is \code{c("Levels","Variables","Tasks")}. If \code{step.names=NULL}, it will not show the names
#' @param fontSize a value or vector of three values. If it is one value, it is the font size for all labels. 
#' But a vector of three values specifies the font size of the labels in the left, middle and right, respectively.  Default is \code{c(20,10,20)}
#' @param nodePadding parameter from \code{sankeyNetwork()}. Default is \code{11}
#' @param nodeWidth parameter from \code{sankeyNetwork()}. Default is \code{5}
#' @param ... graphics parameters to be passed to \code{sankeyNetwork()} from R package \code{networkD3}
#' 
#' @examples
#' # Data set 'cancers_genes_drugs' is an array with association scores between 55 genes (1st dimension), 
#' # 2 cancer types (2nd dimension) and 2 cancer types (3rd dimension)
#' data(cancers_genes_drugs, package = "EnrichIntersect")
#' 
#' intersectSankey(cancers_genes_drugs, step.names=c("Cancers","Genes","Drugs"))
#' 
#' @export
intersectSankey <- function(x, out.fig=NULL, colors=NULL, step.names=c("Levels","Variables","Tasks"), fontSize=c(20,10,20), nodePadding = 11, nodeWidth=5,...){
  
  # intermediate variables
  inter_var <- dimnames(x)[[1]]
  # multiple sample groups
  multilevel <- dimnames(x)[[2]]
  # multiple response variables
  multitask <- dimnames(x)[[3]]
  
  # extract nonzero x
  x0 <- array(as.numeric(x!=0),dim=dim(x)) # inter_var, multilevel, multitask
  source <- target <- NULL
  for(i in 1:dim(x0)[2]){
    source <- c( source, rep(multilevel[i], sum(x0[,i,])) )
    target <- c( target, rep(inter_var[rowSums(x0[,i,])!=0], times=rowSums(x0[,i,])[rowSums(x0[,i,])!=0]) )
  }
  for(i in 1:dim(x0)[1]){
    source <- c( source, rep(inter_var[i],sum(colSums(x0[i,,])!=0)) )
    target <- c( target, multitask[colSums(x0[i,,])!=0] )
  }
  # ## add "+/-" effect signs
  # for(i in 1:dim(beta.ij.sel)[1]){
  #   sign.beta <- t(beta.ij.sel[i,,])
  #   sign.beta <- sign.beta[sign.beta!=0]
  #   sign.beta <- ifelse(sign.beta>0,"+","\u2212")
  #   if(length(sign.beta)==1){
  #     target[target==inter_var[i]] <- paste(inter_var[i], "(",sign.beta,")", sep="")
  #     source[source==inter_var[i]] <- paste(inter_var[i], "(",sign.beta,")", sep="")
  #   }else{
  #     target[target==inter_var[i]] <- paste(inter_var[i], "(",paste(sign.beta,collapse ="/"),")", sep="")
  #     source[source==inter_var[i]] <- paste(inter_var[i], "(",paste(sign.beta,collapse ="/"),")", sep="")
  #   }
  # }
  links <- data.frame(source=source, target=target)
  links$value <- rep(.1, nrow(links))
  
  # construct edge colors
  links$group <- c(rep("white", nrow(links)))
  for(i in 1:length(multitask)){
    links$group[links$target==multitask[i]] <- multitask[i]
  }
  group_tmp <- NULL
  for(i in 1:dim(x0)[2]){
    group_tmp <- c( group_tmp, unlist(apply(x0[rowSums(x0[,i,])>0,i,], 1, function(xx){multitask[xx==1]})) )
  }
  links$group[1:length(group_tmp)] <- group_tmp
  links$group <- as.factor(links$group)
  
  nodes <- data.frame( name=c(as.character(links$source), as.character(links$target)) %>% unique() )
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  nodes$group <- as.factor(c("my_unique_group"))
  nodes$targetL <- c(rep(TRUE,dim(x)[2]), rep(FALSE,length(nodes$name)-dim(x)[2]))
  nodes$targetR <- c(rep(FALSE,length(nodes$name)-dim(x)[3]), rep(TRUE,dim(x)[3]))
  nodes$targetM <- c(rep(FALSE,dim(x)[2]), rep(TRUE,dim(x)[1]), rep(FALSE,dim(x)[3]))
  
  # put colors for different tasks in a data.frame
  if(is.null(colors)){
    mycolor <- c(hcl.colors(dim(x)[3],palette="Dynamic"), "white") #c("blue", "green", "white")
  }else{
    if(length(colors) != dim(x)[3])
      stop("Please specify the argument 'colors' correctly!")
  }
  color_scale <- data.frame(
    range = mycolor,
    domain = c(paste("Task",1:dim(x)[3],sep=""), "my_unique_group"),
    stringsAsFactors = FALSE
  )
  #my_color <- 'd3.scaleOrdinal() .domain(["RG-108", "JQ-1", "my_unique_group"]) .range(["blue", "green", "white"])'
  
  # if(length(fontSize) == 3){
  #   fontsize1 <- paste(fontSize[1], "px", sep="")
  #   fontsize2 <- fontSize[2] * 1.6
  #   fontsize3 <- paste(fontSize[3], "px", sep="")
  # }
  if( ! (length(fontSize) ==1 | length(fontSize) ==3) ){
    stop("Please specify the argument 'fontSize' correctly!")
  }
  if(length(fontSize) == 1){
    #fontsize2 <- fontSize * 1.6
    #fontsize1 <- fontsize3 <- paste(fontSize, "px", sep="")
    fontSize <- rep(fontSize, 3)
  }
  
  # plot a Sankey diagram
  g <- sankeyNetwork(Links=links, Nodes=nodes, Source="IDsource", Target="IDtarget", Value="value", NodeID = "name",
                     sinksRight=FALSE, fontSize=fontSize[1], nodePadding=nodePadding, nodeWidth=nodeWidth, 
                     colourScale = JS(
                       sprintf('d3.scaleOrdinal() .domain(%s) .range(%s)',
                               jsonlite::toJSON(color_scale$domain),
                               jsonlite::toJSON(color_scale$range)) ), 
                     #colourScale = my_color,
                     LinkGroup="group", NodeGroup = "group",...)#, iterations = 10)
  # push left labels to the left of the nodes
  g$x$nodes$targetL <- nodes$targetL
  g$x$nodes$targetM <- nodes$targetM
  g$x$nodes$targetR <- nodes$targetR
  
  # change font sizes of the node labels on the left and right via widget's built-in JavaScript
  df <- data.frame(fontsize = fontSize, #paste(fontSize, "px", sep=""),
    stringsAsFactors = FALSE
  )
  if(is.null(step.names)){
    df$step <- rep("",length(fontSize))
    df$stepSize <- rep(0,length(fontSize))
  }else{
    df$step <- c(step.names, rep(NA,length(fontSize)-3))
    df$stepSize <- c(max(fontSize), rep(NA,length(fontSize)-1))
  }
  
  g <- onRender(g, '
  function(el, x, data) {
    // var fontsize1 = fontsize1;
    // set label font size
    
    d3.select(el)
      .selectAll(".node text")
      .filter(d => d.targetL)
      .attr("x", -6)
      .attr("text-anchor", "end")
      .style("font-size", data.fontsize[0]);
      
    d3.select(el)
      .selectAll(".node text")
      .filter(d => d.targetM)
      .style("font-size", data.fontsize[1]);
      
    d3.select(el)
      .selectAll(".node text")
      .filter(d => d.targetR)
      .style("font-size", data.fontsize[2]);
      
      // set x-axis labels
    var cols_x = this.sankey.nodes()
      .map(d => d.x).filter((v, i, a) => a.indexOf(v) === i)
      .sort(function(a, b){return a - b});

    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d)
        .attr("y", 12)
        .attr("font-weight", "bold")
        .text(data.step[i])
        .style("font-size", data.stepSize[0]);
    })
  }
  ', data=df)
  
  # output figure
  if(!is.null(out.fig)){
    if(out.fig %in% c("html", "pdf")){
      saveWidget(g, file="sankey.html")
    }
    if(out.fig == "pdf"){
      webshot("sankey.html", "sankey.pdf")
    }
  }else{
    g
  }

}