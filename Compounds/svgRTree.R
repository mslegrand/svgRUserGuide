library(svgR)

#---------------------------------------------------------------
# begin svgTree

#' Creates a layout dataframe the specifies location of tree nodes
#' The rows consist of
#' nodeNum: The number of identifier of the node
#' px: the column position of the node (in units of node width)
#' py: the row position of the node (in units of node height)
#' note: THIS DOES NOT CONTAIN PADDING BETWEEN NODES.
#'       svgRTree is responsible for the padding
svgTreeLayout  <-  function(nodeMapping){
  #find node with no parent
  names(nodeMapping)<-c("parent","child")
  rootId<-setdiff(nodeMapping$parent, nodeMapping$child)
  if(length(rootId)!=1){
    base::stop("tree requires exactyl one root")
  }
  #this is what calculates the layout
  rowNum<-0
  #trPos return the lower righthand coord of nodeRect
  trPos<-function(nodeNum, depth=1){ #nodeNum is the id of the node
    #kidsNum<-nodeMapping[nodeMapping[[1]]==nodeNum,2]
    kidsNum<-subset(nodeMapping, parent==nodeNum)$child
    if(length(kidsNum)==0){ #ENDNODE
      rowNum<<-rowNum+1
      data.frame(nodeNum=nodeNum, px=depth, py=rowNum)
    }else{
      ka<-lapply(kidsNum, function(k)trPos(k,depth+1 ) )
      ka<-do.call(rbind,ka)
      npy<-mean(subset(ka, nodeNum==nodeNum)$py)
      rbind(ka, data.frame(nodeNum=nodeNum, px=depth,py=npy))
    }
  }
  tr<-trPos(rootId) #this  calculates the node layout
  tr<-tr[order(tr$nodeNum),]
  return(tr)
}

#' svgRTree
#' produces the svg for an svgRTree
#' nodeMapping is dataframe of nodeids" from.id,to.d
#' nodeLabels is an vector of node Labels,
#' nodeLabels[id] is the label for node with id=id
#' nodeColors is either NULL (pick random), or a vector of specified colors
#' nodeColor[id] is the color for the node with id=id
#' nodeWH is the pair c(nodeWidth, nodeHeight), specifying the size of each node
#' rxy is the rounding of the node corners
svgRTree  %<c-%  function(nodeMapping, nodeLabels, nodeColors=NULL, nodeWH=c(120,20), rxy=c(5,5)){
  #each unique id requires a name (not necessarly unique)
  if(is.null(nodeMapping)){
    base::stop("null nodeMapping not allowed")
  }
  if(is.null(nodeLabels)){
    base::stop("null nodeLabels not allowed")
  }
  textBox <- function(txt, cxy, boxWH=c(120,20), rxy=c(5,5),  fill='lightblue', stroke='darkblue'){
    g( stroke.width=1, stroke='darkblue',
       rect(cxy=cxy, wh=boxWH, rxy=rxy, fill=fill, stroke=stroke),
       text(txt, cxy=cxy, font.size=ceiling(.7*boxWH[2]))
    )
  }

  if(is.null(nodeColors) || length(nodeColors)!=length(nodeLabels)){
    nodeColors<-rep("lightblue", length(nodeLabels))
  }
  #this makes the arrows between nodes
  cubicArrow<-function(xy1,xy2,color='black'){
    dx<-xy2[1]-xy1[1]
    dy<-xy2[2]-xy1[2]
    path(d=c("M",xy1, "C", xy1+.75*c(dx,0), xy2-.75*c(dx,0),xy2), stroke=color , fill='none',
         marker.end = 'url(#triangle)')
  }

  tr<-svgTreeLayout(nodeMapping)

  #root id is at depth 0
  offSet<-c(70,10) + nodeWH
  offSet<-c(190,30)
  offSet<-1.75*nodeWH
  list(
    defs(
      marker(id="triangle", viewBox=c(0, 0,10, 10),
             markerWidth = 20, markerHeight = 26,
             refx = 2, refy = 3, orient='auto', markerUnits="userSpaceOnUse",
             path(d="M0,0 L0,6 L9,3 z")
      )
    ),
    g( stroke.width=2,
       lapply(1:nrow(tr), function(i){ #render nodes
         n0<-tr[i,1]
         xy0<-c(tr[i,2]-1, tr[i,3]-1)
         fill<-nodeColors[n0]
         xy0<-c(1,1)+ offSet*xy0 + c(.5,.5)*nodeWH
         cxy<-xy0
         textBox(nodeLabels[n0], cxy, nodeWH, rxy, fill=fill)
       }) ,
       lapply(1:nrow(nodeMapping), function(i){ #render rows
         nx<-which(tr$nodeNum==nodeMapping[i,1])
         ny<-which(tr$nodeNum==nodeMapping[i,2])
         xy1=c(tr[nx,2]-1, tr[nx,3]-1)
         xy2=c(tr[ny,2]-1, tr[ny,3]-1)
         xy1<-c(1,1)+offSet*xy1 + c( 1,.5)*nodeWH
         xy2<-c(1,1)+offSet*xy2 + c(0,.5)*nodeWH -c(15,0) #15 is the arrow length
         cubicArrow(xy1,xy2)
       })
    )
  )
}

#--------------------------------------------------------------
# This is used for assigning a color to a tree node based on the element name
eleColors<-function(){
  colMap<-c(
    'orange',  #'animate',
    '#F0F0F0', #grey',
    '#F0B0FF', #'fe',
    '#FFFF88', #AFFFA0', #'gradient',
    "#CCCCFF", #'graphics',
    '#AAFFAA', #'container',
    '#FFAAAA', #text',
    '#D060E0',  #'filter',
    '#E0E0A0'  #mask
  )

  catId<-strsplit("1 1 1 1 1 2 2 2 5 5 5 5 5 5 5 6 6 6 6 6 4 4 7 7 7 7 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 9 8 7 9"," ")[[1]]
  eles<-strsplit("animate animateColor animateMotion animateTransform set desc metadata title circle ellipse line path polygon polyline rect defs g svg symbol use linearGradient radialGradient altGlyph textPath tref tspan feBlend feColorMatrix feComponentTransfer feComposite feConvolveMatrix feDiffuseLighting feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feOffset feSpecularLighting feTile feTurbulence mask filter text marker"," ")[[1]]
  #animate, desc, circle, defs, grad(2), text, fe
  colors<-sapply(catId, function(j){
    i<-as.integer(j)

    colMap[as.numeric(i)]
  })
  names(colors)<-eles
  colors
}

element.color<-eleColors()

autoNodeColor <-function(nodeNames){
  nodeColors<-sapply(nodeNames, function(n){
    if(n %in% names(element.color)){
      element.color[n]
    }else{
      if(grepl("=",n))
        "#FFFFCC"
      else
        "#FF88BB"
    }
  })
}
#------------------------------------------------------------------------


#' this returns the dim of the viewbox
trLayout2ViewBoxWH<-function(trLayout, nodeWH){
  col<-max(trLayout$px)
  row<-max(trLayout$py)
  c(col,row)*1.75*nodeWH - c(.75,.75)*nodeWH +c(1,1)

}

#' Width is the Max Width allow to the SVG
#' returns  the optimal Height for the given viewBox and width

width2SVGHeight<-function(viewBox, width){
  dx<-viewBox[3]-viewBox[1]
  dy<-viewBox[4]-viewBox[2]
  width*(dy/dx)
}

#' Generates a tree given nodeMapping,
#' Width is the Width reservered for drawing region (includes margin padding)
#' xmarg is the left-right margins applied to the drawing region
#' ymarg is the top-bottom margins applied to the drawing region
#' nodeWH is the width-height of the node (before viewboxing)
autoTree %<c-% function(  main, nodeMapping, nodeNames,
                     Width=540, xmarg=c(25,50),
                     ymarg=c(50,50),nodeWH=c(110,20) ){

  #checks
  stopifnot(class(main)=="character")
  stopifnot(inherits(nodeMapping,"data.frame"))
  stopifnot(class(nodeNames)=="character")
  #do the geometry
  trLayout<-svgTreeLayout(nodeMapping)
  dimVB<-trLayout2ViewBoxWH(trLayout,nodeWH)
  viewBox<-c(0,0,dimVB)
  IW<-Width-sum(xmarg) # Interior Width
  IH<-width2SVGHeight(viewBox,IW) #Interior Height
  WH<-c(Width,IH+sum(ymarg)) #svg WH
  #colors
  nodeColors<-autoNodeColor(nodeNames)
  #shapes
  rs<-5*(grepl("=",nodeNames)+1)

  #' svgRTree
  #' produces the svg for an svgRTree
  #' nodeMapping is dataframe of nodeids" from.id,to.d
  #' nodeLabels is an vector of node Labels,
  #' nodeLabels[id] is the label for node with id=id
  #' nodeColors is either NULL (pick random), or a vector of specified colors
  #' nodeColor[id] is the color for the node with id=id
  #' nodeWH is the pair c(nodeWidth, nodeHeight), specifying the size of each node
  #' rxy is the rounding of the node corners
  svgRTreeN <- function(trLayout,
                      nodeLabels,
                      rs,
                      nodeColors=NULL,
                      nodeWH=c(120,20)
  ){
    #each unique id requires a name (not necessarly unique)
    #this makes the arrows between nodes
    cubicArrow<-function(xy1,xy2,color='black'){
      dx<-xy2[1]-xy1[1]
      dy<-xy2[2]-xy1[2]
      path(d=c("M",xy1, "C", xy1+.75*c(dx,0), xy2-.75*c(dx,0),xy2), stroke=color , fill='none',
           marker.end = 'url(#triangle)')
    }
    offSet<-1.75*nodeWH
    list(
      defs(
        marker(id="triangle", viewBox=c(0, 0,10, 10),
               markerWidth = 20, markerHeight = 26,
               refx = 2, refy = 3, orient='auto', markerUnits="userSpaceOnUse",
               path(d="M0,0 L0,6 L9,3 z")
        )
      ),
      g( stroke.width=2,
         lapply(1:nrow(trLayout), function(i){ #render nodes
           n0=trLayout[i,1]
           xy0=c(trLayout[i,2]-1, trLayout[i,3]-1)
           fill=nodeColors[n0]
           xy0<-c(1,1)+ offSet*xy0 + c(.5,.5)*nodeWH
           cxy<-xy0
           textBox(nodeLabels[n0], cxy, nodeWH, rxy=c(1,1)*rs[n0], fill=fill)
         }) ,
         lapply(1:nrow(nodeMapping), function(i){ #render rows
           nx<-which(trLayout$nodeNum==nodeMapping$parent[i])
           ny<-which(trLayout$nodeNum==nodeMapping$child[i])
           xy1=c(trLayout[nx,2]-1, trLayout[nx,3]-1)
           xy2=c(trLayout[ny,2]-1, trLayout[ny,3]-1)
           xy1<-c(1,1)+offSet*xy1 + c( 1,.5)*nodeWH
           xy2<-c(1,1)+offSet*xy2 + c(0,.5)*nodeWH -c(15,0) #15 is the arrow length
           cubicArrow(xy1,xy2)
         })
      )
    )
  }
  svgR( wh=WH,
        rect(c(0,0),wh=WH-c(xmarg[2]/2,0),
             fill=linearGradient(colors=c('lightblue','white','lightblue'))),
        text( main, font='black', xy=c(20,25), font.size=20),
        svg(xy=c(xmarg[1],ymarg[1]),wh=c(IW,IH), viewBox=viewBox,
            svgRTreeN(trLayout, nodeNames, rs, nodeColors=nodeColors, nodeWH=nodeWH)
        )
  )
}


