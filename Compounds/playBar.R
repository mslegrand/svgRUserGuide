#---------------------------------------------------------------
# playBar code begins here

if(!exists("nextPlayId")){
  nextPlayId<-IdGeneratorFactory('playButton')
}

playButton.click<-function(){
  paste0(nextPlayId(FALSE),".click")
}

playButton %<c-% function( xy=c(20,80), br=10){
  playButtonId<-nextPlayId()
  pt=xy-br*c(2,2)
  g(id=playButtonId,
    circle(cxy=pt, r=br+2, stroke="grey", fill='grey'),
    circle(cxy=pt, r=br, stroke="grey", fill='#404040'),
    polygon(points=list( pt+c(br,0),pt+c(-br*.3,br*.4),
                         pt+c(-br*.3,-br*.4)),
            fill="lime")
  )
}


playBar %<c-% function(wh, UseGraphPaper=FALSE){
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


