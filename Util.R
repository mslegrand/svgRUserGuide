stopifnot(require(svgR, quietly=TRUE))
library(svgR)

#---------------------------------------------------------------
#utility to makes id Generators
IdGeneratorFactory<-function(prefix='docId'){
  count<-0
  function(){count<<-count+1;
             paste0(prefix,count)
  }
}

newId<-IdGeneratorFactory()
#---------------------------------------------------------------

#some phrazes in chinese
hello<-"您好"
dataScience<-"数据科学"
statistics<-"统计"
mathematics<-"数学"
linearRegression<-"线性回归"
integral<-"积分"
normalDistribution<-"正态分布"

#---------------------------------------------------------------
textLines<-function(txtLines, xy=c(20,20), font.size=16, stroke='darkblue'){
  lapply(1:length(txtLines),function(i){
    text(txtLines[i], xy=c(xy[1],xy[2]+i*1.1*font.size), 
         font.size=font.size, stroke=stroke)
  })
}

textLines2<-function(txtLines, xy=c(20,20),  font.size=16, stroke='darkblue'){
  y<-xy[2]+1:length(txtLines)*1.1*font.size
  x<-xy[1]
  mapply(function(t,x,y)text(t, xy=c(x,y), font.size=font.size), 
         txtLines,x, y , SIMPLIFY=FALSE)
}


#---------------------------------------------------------------
#this draws the box with centered text
textBox<-function(txt, cxy, boxWH=c(120,20), rxy=c(5,5),  fill='lightblue', stroke='darkblue'){
  g( stroke.width=1, stroke='darkblue',
     rect(cxy=cxy, wh=boxWH, rxy=rxy, fill=fill, stroke=stroke),
     text(txt, cxy=cxy, font.size=ceiling(.7*boxWH[2]))
  )
}

textCircle<-function(txt, cxy, r=120,  fill='lightblue', stroke='darkblue'){
  g( stroke.width=1, stroke='darkblue',
     circle(cxy=cxy,  r=r, fill=fill, stroke=stroke),
     text(txt, cxy=cxy, font.size=ceiling(.4*r))
  )
}

matrix2Arrows<-function( points=matrix(c(0,0,100,100), 2,2), ... ){
  if(length(points)%%4 !=0){ stop("bad point given to matrix2Arrows") }
  if( inherits(points, "matrix") && nrow(points)%%2!=0){
    stop("points must be a  2 by 2n matrix")
  } 
  mid<-newId()
  points<-matrix(points, 4, )
  c(
    defs(
      marker(
        id=mid,  viewBox=c(0, 0, 10, 10), refXY=c(1,5), 
        markerWidth=6, markerHeight=6, orient="auto",
        path( d=c("M", 0, 0, "L", 10, 5, "L", 0, 10, "z") ) 
      )
    ),
    apply(points, 2, function(v){
      line(xy1=v[1:2], xy2=v[3:4], ... ,
           marker.end=paste0( 'url(#',mid,')' )
      )
    })
  )
}

#--------------------------------------------------------------
# pie to represent an angle
# given cxy, r and angles theta1, theta2 (in radians), draw a clockwise arc
pie<-function(cxy=c(0,0), r=100, theta1=0, theta2=pi, fill="none", sf=0, ...){
  theta1<-theta1%%(2*pi)
  theta2<-theta2%%(2*pi)
  startPt<-r*c( cos(theta1), -sin(theta1) )+cxy
  endPt  <-r*c( cos(theta2), -sin(theta2) )+cxy
  laf<-ifelse(abs(theta2-theta1)>pi,1,0)
  d<-c("M", startPt, "A", c(r,r), 0, laf, sf, endPt )
  if(fill!="none"){
    d=c(d, "L", cxy, "Z" )
  }
  path(d=d , stroke='red', fill=fill, ...)
}



#---------------------------------------------------------------
#graphPaper background
graphPaper<-function(wh=c(600,200), dxy=c(10, 10), labels=FALSE ){
  seq(0,wh[1],dxy[1])->xs
  seq(0,wh[2],dxy[2])->ys
  grph<-c(
    lapply(xs, function(x)line(xy1=c(x,0),xy2=c(x,wh[2]))),
    lapply(ys, function(y)line(xy1=c(0,y),xy2=c(wh[1],y)))
  )
  if(labels){
    grph<-c(grph, 
            lapply(xs, function(x)text(xy=c(x+2,10),x)),
            lapply(ys, function(y)text(xy=c(2,y),y))
    )
  }
  g( stroke.width=1,
     font.size=10,
     stroke="lightgrey",
     grph
  )       
}

#' render braces
#'
#' v1 is the vertiex of the brace
#' p2 endpoint of brace leg (abs coord)
#' p3 endpoint of brace leg (abs coord)
brace<-function(v1,p2,p3, stroke='black', stroke.width=1 ){ 
  xa<-v1-p2
  xb<-p3-p2
  n1<-(sum(xa*xb)/sum(xb*xb))*xb +p2 #m=midpt
  n2<-p2+v1-n1
  n3<-p3+v1-n1
  #now the braces
  list(
    path(d=c("M",v1, "C", n1, n2,p2), stroke=stroke , fill='none', stroke.width=stroke.width),
    path(d=c("M",v1, "C", n1, n3,p3), stroke=stroke , fill='none', stroke.width=stroke.width)
  )
}

#---------------------------------------------------------------
# playBar code begins here

nextPlayId<-IdGeneratorFactory('playButton')
playButton.click<-""

playButton<-function( xy=c(20,80), br=10){
  playButtonId<-nextPlayId()
  playButton.click<<-paste0(playButtonId,".click")
  pt=xy-br*c(2,2)
  g(id=playButtonId,
    circle(cxy=pt, r=br+2, stroke="grey", fill='grey'),
    circle(cxy=pt, r=br, stroke="grey", fill='#404040'),
    polygon(points=list( pt+c(br,0),pt+c(-br*.3,br*.4), 
                         pt+c(-br*.3,-br*.4)),
            fill="lime")
  )  
}

playBar<-function(wh, UseGraphPaper=FALSE){
  tmp<-list(
    rect(xy=c(0,wh[2]),  wh=c(wh[1],40), 
         fill= linearGradient(
           xy1=c(0,0), xy2=c(0,1),
           colors=c("lightblue","white","lightblue") 
         )
    ),
    playButton(xy=wh+c(0,40)),
    rect(xy=c(0,0),wh=wh, fill='none', stroke='blue', stroke.width=3)
  )
  if(UseGraphPaper){
    tmp<-c(graphPaper(wh=wh-c(0,40)),tmp)
  }
  tmp<-c( tmp,list(wh=wh+c(0,40) ) )
}


#  toggle bar code begins here
toggleButtons<-IdGeneratorFactory(prefix='toggleButton')
nexttoggleId<-function(){ c(left=toggleButtons(), right=toggleButtons())} 
toggleButton.click<-c(left="",right="")
toggleButton.end<-c(left="",right="")
toggleButton.begin<-c(left="",right="")

toggleButton<-function( xy=c(20,80), br=10){
  toggleButtonId<-nexttoggleId()
  toggleButton.click<<-structure(paste0(toggleButtonId,".click"),names=c('left','right'))
  A='green'
  B='red'
  spacing=8
  bd=2*br #br is button radius
  x=4+cumsum(c(spacing,br,br,spacing,spacing,br,br,spacing))
  w<-x[length(x)]-4
  y<-br +spacing+2
  h<-2*y-8
  wh=c(w,h)
  g(
    rect(cxy=xy-wh/2-c(4,4), wh=wh, rxy=c(5,5), fill='none', stroke='grey'),
    circle(cxy=xy-c(x[2],y), r=br+2, stroke="grey", fill='none'),
    circle(cxy=xy-c(x[6],y), r=br+2, stroke="grey", fill='none'),
    circle(cxy=xy-c(x[6],y), r=br-3, stroke="grey", fill=A,
           id=toggleButtonId['left'],
           set( attributeName="fill", to=B,
                begin=toggleButton.click["left"],
                end=  toggleButton.click["right"]
           )
    ),
    circle(cxy=xy-c(x[2],y), r=br-3, stroke="grey", fill=A,
           id=toggleButtonId['right'], 
           set(attributeName="fill", to=B,
               begin=paste0("0;",toggleButton.click["right"]),#onclick", 
               end=toggleButton.click["left"]
           )       
    )    
  )
}

toggleBar<-function(xy){
  tmp<-list(
    rect(xy=c(0,WH[2]-40),  wh=c(WH[1],40), 
         fill= linearGradient(
           xy1=c(0,0), xy2=c(0,1),
           colors=c("lightblue","white","lightblue") 
         )
    ),
    toggleButton(xy=WH),
    rect(xy=c(0,0),wh=WH, fill='none', stroke='blue', stroke.width=3)
  )
  tmp  
}

#---------------------------------------------------------------
#svgR logo generaor
logo<-function(WH, txt="Users Guide"){
  g(
    text( "svgR", cxy=WH*c(.25,.5), fill="darkblue", font.size=100, 
          font.family="san serif", stroke.width=3),
    text( txt, cxy=WH*c(.7,.25), fill="darkblue", font.size=60, 
          font.family="fantasy", stroke.width=3),
    circle(cxy=WH*c(.25,.5), r=99, stroke.width=10, fill='none'  ),
    stroke='black',
    filter = filter(
      feMerge( 
        feMergeNode(in1="SourceGraphic"),
        feMergeNode(
          in1=feComposite( operator='in',
                           in1=feSpecularLighting( surfaceScale=6,
                                                   specularConstant=1,
                                                   specularExponent=30,
                                                   lighting.color="yellow",
                                                   in1=feGaussianBlur(
                                                     stdDeviation=5,
                                                     in1="SourceAlpha"),
                                                   fePointLight(xyz=c(40,-30,200))
                           ),
                           in2="SourceAlpha"
          )
        )
      )
    )
  )
}



logo2<-function(wh){
  g(
    #text( "svgR", cxy=wh*c(.25,.5), fill="darkblue", font.size=.5*wh[2], 
    #font.family="san serif", stroke.width=3),
    #circle(cxy=wh*c(.25,.5), r=99, stroke.width=10, fill='none'  ),
    text( "svgR", xy=wh*c(.001,.9), fill="darkblue", font.size=.8*wh[2], 
          font.family="san serif", stroke.width=3),
    line(xy1=wh*c(.2,.5), xy2=wh*c(.99,.5), stroke.width=5),
    stroke='black',
    filter = filter(
      feMerge( 
        feMergeNode(in1="SourceGraphic"),
        feMergeNode(
          in1=feComposite( operator='in',
                           in1=feSpecularLighting( surfaceScale=6,
                                                   specularConstant=1,
                                                   specularExponent=30,
                                                   lighting.color="pink",
                                                   in1=feGaussianBlur(
                                                     stdDeviation=5,
                                                     in1="SourceAlpha"),
                                                   fePointLight(xyz=c(40,-30,200))
                           ),
                           in2="SourceAlpha"
          )
        )
      )
    )
  )
}


#svgR logo generaor
logo3<-function(WH=c(800,200), txt="Users Guide"){
  r<-ceiling(.4*WH[2])
  g(
    text( "svgR", cxy=WH*c(.25,.5), fill="darkblue", font.size=r, 
          font.family="san serif", stroke.width=3),
    text( txt, cxy=WH*c(.7,.25), fill="darkblue", font.size=.6*r, 
          font.family="fantasy", stroke.width=3),
    circle(cxy=WH*c(.25,.5), r=r-1, stroke.width=10, fill='none'  ),
    stroke='black',
    filter = filter(
      feMerge( 
        feMergeNode(in1="SourceGraphic"),
        feMergeNode(
          in1=feComposite( operator='in',
                           in1=feSpecularLighting( surfaceScale=6,
                                                   specularConstant=1,
                                                   specularExponent=30,
                                                   lighting.color="yellow",
                                                   in1=feGaussianBlur(
                                                     stdDeviation=5,
                                                     in1="SourceAlpha"),
                                                   fePointLight(xyz=c(40,-30,200))
                           ),
                           in2="SourceAlpha"
          )
        )
      )
    )
  )
}


WH3<-c(800,250)
library(svgR)

svgR( wh=WH3,
logo3(WH=WH3, txt="Whirlwind Tutorial")
)->tmp

as.character(tmp,file="whirlwind.svg")

#---------------------------------------------------------------
#funkyFilter
funkyFilter<-function(id, baseFrequency=.01, numOctaves=3, slope=c(4,4,4), seed=100){
  filter( id=id, 
          feTurbulence(baseFrequency=baseFrequency, numOctaves=numOctaves, seed=100),
          feComponentTransfer(
            feFuncR(type="linear", slope=slope[1], intercept=-1),
            feFuncG(type="linear", slope=slope[2], intercept=-1),
            feFuncB(type="linear", slope=slope[3], intercept=-1),
            feFuncA(type="linear", slope=0, intercept=1)
          ),
          feColorMatrix(type="saturate") 
  )
} 

#---------------------------------------------------------------
#---------------------------------------------------------------
#an arrow from point p0 to p1
polyArrow<-function(p0,p1, w=20, fill='lightblue'){
  normalize<-function(v){ v/sqrt(sum(v^2)) }
  v<-normalize(p1-p0)
  u<- c(-v[2], v[1])
  # thickness
  polygon( fill=fill, stroke='black',
           points=c(
             p1,
             p1 - 2*w*v - 1.5*w*u,
             p1 - 2*w*v - w/2*u,
             p0  - w/2 * u,
             p0  + w/2 * u,
             p1 - 2*w*v + w/2*u,
             p1 - 2*w*v + 1.5*w*u
           )
  )
}


#---------------------------------------------------------------
# begin arrow formed by 2 or more points
polyArrowX<-function(bPoints, w=20){
  pts<-bPoints
  w<-20
  normalize<-function(v){
    s<-sqrt(sum(v^2))
    if(s>0){
      v/s
    } else {
      v
    }
  }
  #Assume pts are string
  N<-length(pts)/2
  pts<-matrix(pts,2,N)
  pathVect<-matrix( pts[,2:N] - pts[,1:(N-1)],2,N-1 )
  pathVect<-apply(pathVect,2,normalize)
  normVect<-apply(pathVect,2,function(v){c(v[2],-v[1])})
  dd<-ncol(normVect)
  #
  NV1<-matrix(normVect[,1:(dd-1)],2,dd-1)
  NV2<-matrix(normVect[,2:dd],2,dd-1)
  NV3<-rbind(NV2[2,],NV2[1,])
  dot<-apply(matrix(NV1*NV2,2,dd-1),2,sum)
  cross<-apply(matrix(NV1*NV3,2,dd-1),2,diff)
  delta<-w/2*sqrt((1-dot)/(1+dot))
  delta<-sign(cross)*delta
  delta2<-rbind(c(delta,0),c(delta,0))
  # multipie delta into pathVect (the tangent)
  dTan<-cbind(matrix(0,2,2),delta2*pathVect )
  # mutliplew/2 into normVect
  dNorm<-0.5*w*cbind(2*normVect[,1], normVect[,1],normVect)
  # put together
  dC<-cbind(-dTan+dNorm)
  dCX<-cbind(dC, -dC[ ,(dim(dC)[2]):1])
  NN<-dim(dCX)[2]-1
  dCX[,1:2]<-dCX[,1:2] + 1.5*w*pathVect[,1]
  dCX[,NN:(NN+1)]<-dCX[,NN:(NN+1)] +1.5*w*pathVect[,1]
  ptsX<-cbind(pts[,1],pts, pts[,N:1],pts[,1] )
  points<-dCX+ptsX
  pt<-matrix(bPoints, 2, 1)
  points<-cbind(pt, points)
  polygon( fill='lightgreen', stroke='black',
           points=points
  )
}

#---------------------------------------------------------------
#polyBand: band formed by two or moe points
polyBand<-function(bPoints, w=20){
  pts<-bPoints
  w<-20
  normalize<-function(v){
    s<-sqrt(sum(v^2))
    if(s>0){
      v/s
    } else {
      v
    }
  }
  #Assume pts are string
  N<-length(pts)/2
  pts<-matrix(pts,2,N)
  pathVect<-matrix( pts[,2:N] - pts[,1:(N-1)],2,N-1 )
  pathVect<-apply(pathVect,2,normalize)
  normVect<-apply(pathVect,2,function(v){c(-v[2],v[1])})
  dd<-ncol(normVect)
  prod<-matrix(normVect[,1:(dd-1)]*normVect[,2:dd],2,dd-1)
  theta<-.5*apply(prod, 2, function(v)acos(sum(v)))
  delta<-sapply(theta,function(theta) w/2*tan(theta))
  delta2<-rbind(c(delta,0),c(delta,0))
  # multipie delta into pathVect (the tangent)
  dTan<-cbind(c(0,0),delta2*pathVect )
  # mutliplew/2 into normVect
  dNorm<-0.5*w*cbind(normVect[,1],normVect)
  # put together
  dC<-cbind(dTan+dNorm)
  dCX<-cbind(dC, -dC[ ,(dim(dC)[2]):1])
  ptsX<-cbind(pts, pts[,N:1] )
  points<-dCX+ptsX
  polygon( fill='lightgreen', stroke='black',
           points=points
  )
}


#---------------------------------------------------------------
# begin svgTree 

#' Creates a layout dataframe the specifies location of tree nodes
#' The rows consist of 
#' nodeNum: The number of identifier of the node
#' px: the column position of the node (in units of node width)
#' py: the row position of the node (in units of node height)
#' note: THIS DOES NOT CONTAIN PADDING BETWEEN NODES. 
#'       svgRTree is responsible for the padding
svgTreeLayout<-function(nodeMapping){
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
  tr
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
svgRTree<-function(nodeMapping, nodeLabels, nodeColors=NULL, nodeWH=c(120,20), rxy=c(5,5)){
  #each unique id requires a name (not necessarly unique)
  if(is.null(nodeMapping)){
    base::stop("null nodeMapping not allowed")
  }
  if(is.null(nodeLabels)){
    base::stop("null nodeLabels not allowed")
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
         n0=tr[i,1]
         xy0=c(tr[i,2]-1, tr[i,3]-1)
         fill=nodeColors[n0]
         xy0<-c(1,1)+ offSet*xy0 + c(.5,.5)*nodeWH
         textBox(nodeLabels[n0], xy0, nodeWH, rxy, fill=fill)
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

autoNodeColor<-function(nodeNames){
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
autoTree<-function(  main, nodeMapping, nodeNames,
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
  svgRTreeN<-function(trLayout, 
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
           textBox(nodeLabels[n0], xy0, nodeWH, rxy=c(1,1)*rs[n0], fill=fill)
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


