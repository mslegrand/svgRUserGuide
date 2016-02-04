
#  toggle bar code begins here
toggleButtons<-IdGeneratorFactory(prefix='toggleButton')
# nexttoggleId<-function(){ c(left=toggleButtons(), right=toggleButtons())}
# toggleButton.click<-c(left="",right="")
# toggleButton.end<-c(left="",right="")
# toggleButton.begin<-c(left="",right="")

nexttoggleId<-function(){
  toggleButtons()
}

toggleButtonId<-function(choice){
  if(!(choice %in% c('left','right'))){
    stop("toggle button choice can be either 'left' or 'right' ")
  }
  paste(toggleButtons(FALSE), choice, sep="_")
}

# toggleLeftClick<-function(){ paste(toggleButtons(F),"left","click",sep="."}
# toggleRightClick<-function(){ paste(toggleButtons(F),"right","click",sep="."}

toggleButton.click<-function(choice){
  return(paste0( toggleButtonId(choice), ".click" ) )
}



toggleButton %<c-% function( xy=c(20,80), br=10){
  nexttoggleId()
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
           id=toggleButtonId('left'),
           set( attributeName="fill", to=B,
                begin=toggleButton.click("left"),
                end=  toggleButton.click("right")
           )
    ),
    circle(cxy=xy-c(x[2],y), r=br-3, stroke="grey", fill=A,
           id=toggleButtonId('right'),
           set(attributeName="fill", to=B,
               begin=paste0("0;", toggleButton.click("right")),
               end=toggleButton.click("left")
           )
    )
  )
}

toggleBar %<c-% function(xy){
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

