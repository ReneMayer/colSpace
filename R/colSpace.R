colSpace <-
function(formula=NA, data=NA,...) {


# formula
 mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf[[1L]] <- as.name("model.frame")
     mf <- eval(mf, parent.frame())
     y <- model.response(mf, "numeric")
      mt <- attr(mf, "terms")
      x <- model.matrix(mt, mf, contrasts)
      X=x[,2:3]
      labels = dimnames(X)[[2]]





# projection: b's
cData=data.frame(scale(cbind(X,y), center = TRUE, scale = FALSE))
X <- cbind(cData[,1],cData[,2])
X
y <- cData[,3] 
b <- solve(t(X) %*% X) %*% t(X) %*% y
b

names(cData) = c('x1','x2','y')

# prediction
y_hat=X %*% b

# for vector scaling
ratio.x2.x1 = sd(cData$x2)/sd(cData$x1)

# second predictor
z.second.predictor=complex(modulus = 1, argument = cor(cData$x1,cData$x2))
vector.second.predictor=c(Im(z.second.predictor),Re(z.second.predictor),0) * ratio.x2.x1

zprediction=complex(modulus = 1, argument = cor(y_hat,cData$x1))
predictionvector=c(Im(zprediction),Re(zprediction),0) * ( sd(y_hat)/sd(cData$x1)) 

observationvector = c( 	predictionvector[1], 
			predictionvector[2],  
			tan( acos(cor(y_hat, y))  ) )
			
			
# why ... tan( acos(cor(y_hat, y))  ) ) 			
#      /|			
#     / |			
#    /  | opposite			
#   /   |
#  /    |
#  ------
#  cos(alpha) = length of adjecent; alpha =  adjecent/opposite = cor(y,y_hat)
#  acos = cos^-1 (length of adjecent) = alpha
#  alpha * lenth of adjecent = opposite
			

# ... draw prediction and error space
open3d()
    # ... x1
    vector3D(end=c(1,0,0),0,mycol='green')
    # ... x2
    vector3D( end=c(vector.second.predictor[1],vector.second.predictor[2],vector.second.predictor[3]),mycol='red'  )
    # ... y_hat=prediction(y | x1, x2)
    vector3D(  end=c(predictionvector[1],predictionvector[2],predictionvector[3]),
	      mycol='yellow' ) 
    # .... y

    vector3D(  end=c(observationvector[1],observationvector[2],observationvector[3]),
	      mycol='blue')
    # ... e 
    error.vector(end   = c(observationvector[3],0,0),x_p=observationvector[1], y_p=observationvector[2], mycol='grey')

    # triangle between the projection of y onto the span of X
    triangles3d(rbind(c(0,0,0),
                      c(observationvector[1],observationvector[2],observationvector[3]),
                      c(predictionvector[1],predictionvector[2],predictionvector[3])),col="blue",alpha=0.5)
    
    # ... draw one plane resulting from the span of X
    p1=matrix(c(0,1,-1,0), nrow=2) %*%
    matrix(c(observationvector[1],observationvector[2]), ncol=1, byrow=TRUE)                  
    p2=p1+matrix(c(observationvector[1],observationvector[2]), ncol=1, byrow=TRUE)*1.33 
    
    p4=p1-2*p1
    p3=p4+matrix(c(observationvector[1],observationvector[2]), ncol=1, byrow=TRUE)*1.33 
    mm=rbind(t(p1),t(p2),t(p3), t(p4))
    vs=cbind(mm, 0)
    names(vs)=c('x','y','z')
    quads3d(vs,col="purple",alpha=0.5)
   
axes3d(c('x','y','z'))
title3d('column space','',labels[1],labels[2],'error')
#box3d()
}
