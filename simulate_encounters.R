## simulate consumer encounters at resource sites
## using scaled resource density and detection distance

require(plotrix)


#=========================================================
#=========================================================
#
# 
#
#=========================================================
#=========================================================

rho=.2                      # consumer intensity

D = 1000 #10000         		#	number of 'days' or trials

kappa.exp=seq(-4,0.5,length=200)
kappa.real = 10^(kappa.exp) #  intensity of resources
kappa = kappa.real/rho      # scaled resource intensity

calculateAvgEncounters <- function(detect.distance){

  l = detect.distance 
  R = 3*l  
  A = pi*R^2    			#	total area of the arena
  
avg_encounters=rep(as.numeric(NA),length(kappa))


for(j in 1:length(kappa))
{
  num_encounters = rep(NA,D)
  
  for(d in 1:D)
    {
    Zsquare = rpois(1,((2*R)^2)*kappa[j]) # number of resources in square 
    
    # pick locations for resources
    ZsquareX = runif(Zsquare,-R,R)
    ZsquareY = runif(Zsquare,-R,R)
    
    # thin to resources within detecting distance of focal consumer
    Zsquare_dist = sqrt(ZsquareX^2+ZsquareY^2) # distance between focal consumer and each resource
    keep_list = which(Zsquare_dist<R)
    
    ZX = ZsquareX[keep_list]
    ZY = ZsquareY[keep_list]
    Zd = Zsquare_dist[keep_list]
    Z = length(Zd)                  # thinned number of resources
    
    if(Z==0 || (min(Zd)>l)){
      num_encounters[d]=0
      }else{
    # find the resource that the focal consumer picks
    closest_index = which(Zd == min(Zd))
    closest_x = ZX[closest_index]
    closest_y = ZY[closest_index]
    closest_d = Zd[closest_index]
    
    # generate consumers within detecting distance (l) of the focal consumer's chosen resource
    Nsquare = rpois(1,(2*l)^2) # number of consumers in square 
    
    # pick locations for relevant consumers
    NsquareX = runif(Nsquare,closest_x-l,closest_x+l)
    NsquareY = runif(Nsquare,closest_y-l,closest_y+l)
    
    # thin to consumers that could detect same resource as the focal consumer
    Nsquare_dist = sqrt((NsquareX-closest_x)^2+(NsquareY-closest_y)^2) # dist between each consumer and the resource chosen by focal consumer
    if((min(Nsquare_dist)>l)|| Nsquare==0){num_encounters[d]=0}else{
    keep_list = which(Nsquare_dist<l) # indices of consumers that could choose same resource
    
    NX = NsquareX[keep_list]
    NY = NsquareY[keep_list]
    Nd = Nsquare_dist[keep_list]
    N = length(Nd)                  # thinned number of consumers 
    
    # determine if other consumers pick the same resource as the focal consumer
    dummy_Z = matrix(1,1,Z)
    same_choice = mat.or.vec(1,N)
    
    for(i in 1:N){
      dist = sqrt((NX[i]*dummy_Z-ZX)^2+(NY[i]*dummy_Z-ZY)^2) # distance between consumer i and all resources
      same_choice[i] = (min(dist)==Nd[i])                    # T/F indicates whether consumer i chooses the same resource as focal consumer
    }   
    num_encounters[d] = sum(same_choice) }
    }
  }
  
  avg_encounters[j]= sum(num_encounters)/D
#  print(avg_encounters[j])
  
  }

return(avg_encounters)

}

Encounters = c()
l_vals=c(4*sqrt(rho),10*sqrt(rho))

for(i in 1:length(l_vals)) 
{ 
  Encounters = cbind(Encounters,calculateAvgEncounters(l_vals[i]))
}

#save.image(file="l_4_10_scaled.RData")

## test plot
# plot(ZsquareX,ZsquareY,xlim=c(-R,R),ylim=c(-R,R)) # resources
# draw.circle(0,0,R) # arena
# points(0,0,pch=15) # focal consumer
# points(NsquareX,NsquareY,pch=0,col='gray') # consumers in square around chosen resource
# points(NX,NY,pch=15,col='gray') # consumers that can detect chosen resource
# draw.circle(closest_x,closest_y,radius=l) # 
# points(NX[which(same_choice==1)],NY[which(same_choice==1)],pch=4) # consumers that choose same


