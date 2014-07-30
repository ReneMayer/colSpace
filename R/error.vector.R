error.vector <-
function(start=c(0,0,0), end=NA,size=NA, mycol='green', theta.phi=c(pi/2,0), y_p, x_p,... ){ # theta = y-rotation, phi = z-rotation 
      size=end
      # ... rotate cylinder horizontally and scale it
      len=sqrt(sum(abs(end)*abs(end)))
      cone.length=len/10
      rad=len/20

      # ... cylinder as basis-vector
      c=cylinder3d(rbind(      c(start), # start
			      c(0,0,1)), # end
			      radius = rad,
			      e1=cbind(0, 0, 1),
			      e2=cbind(1, 0, 0),
			      sides=10
			)

      c=scale3d(c,0.1,0.1,len-cone.length) 

      # ... Duncan Murdoch's code snipe
      rotation <- rgl:::GramSchmidt(end-start+c(1,0,0),end-start+c(0,1,0),end-start, order=c(3,1,2))
      c <- rotate3d(c, matrix=rotation) 

      if (theta.phi[1] != 0){c=rotate3d(c, theta.phi[1] ,0,1,0)} # y rotation xRy} 
      if (theta.phi[2] != 0){c=rotate3d(c, theta.phi[2] ,0,0,1)} # z rotation xRy}
      c=translate3d(c,x_p,y_p,0) 

      shade3d(addNormals(c), col=mycol)

      # ... cone3d from rgl-demos
      q1 <- cone3d(qmesh=T,trans=diag(4))  # height=1,radius=1, base at (0,0,0)
      q1=translate3d(scale3d(q1,1*rad/3,1*rad/3,1*cone.length),0,0,len-cone.length) 
      q1=rotate3d(q1, matrix=rotation) # hinlegen ... negative rotation == zeigerrrichtung
      if (theta.phi[1] != 0){q1=rotate3d(q1, theta.phi[1] ,0,1,0)} # y rotation xRy} 
      if (theta.phi[2] != 0){q1=rotate3d(q1, theta.phi[2] ,0,0,1)} # z rotation xRy}
      q1=translate3d(q1,x_p,y_p,0) 
      shade3d(q1,col=mycol) # verschachtelt aufrufen

}
