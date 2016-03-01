
library(svgR)

#---------------------------------------------------------------
#svgR logo generaor
logo %<c-% function(WH, txt="Users Guide"){
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



logo2 %<c-%function(wh){
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
logo3 %<c-% function(WH=c(800,200), txt="Users Guide"){
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

cat(as.character(tmp),file="whirlwind.svg")
