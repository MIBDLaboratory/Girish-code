AGR<-matrix(c(0.0098,0.0306,0.1814,0.0449,0.0025,0.0083,0.0033),1,7,
            dimnames = list(1,c("10","12","21","23","24","32","34")))

AIR<-matrix(rep(c(0,0.0004,0.0013,0.0008,0.02,0.0406,0.0520,0.0599,0.0738,0.0984,0.1398,0.1720,0.1415,0.0971,
                  0.0510,0.0100,0.0006,0.0004,0.0001),c(4,rep(5,17),6)),nrow=95,ncol=1) #adenoma incidence rates matrix

PR<-matrix(c(0.072,0.064,0.054,0.04,0.03,0.024,0.019,0.018,0.018,0.018)) #population ratio

Q<-10 # of iteration

K<-1000 # of people

SA<-50 # specific age

n<-8 #maximum number of adenomas

AI<-(SA/5)-9 #age index


Result<-matrix(0,9,5,byrow=FALSE,dimnames = list(c("ZERO","ONE","TWO","THREE","FOUR","FIVE","SIX","SEVEN","EIGHT"),
                                                 c("Healthy","Dimunitive","Medium","Large","CRC")))

Result1<-matrix(0,1,10,dimnames = list("CRC-CASES",
                                       c("A50","A55","A60","A65","A70","A75","A80","A85","A90","A95")))

for(q in c(1:Q)){ #iterating
  
  P<-matrix(c(0),K,4) #People
  
  for(s in seq(50,95,5)) { #setting the CRC cases counter in each age as zer
    nam <- paste("A", s, sep = "")
    assign(nam, 0)
  }
  
  for(k in c(1:K)){ #people
    
    S<-matrix(c(0),95,4,byrow=TRUE,dimnames = list(c(1:95),c("Dimunitive","Medium","Large","CRC"))) # reseting the patient's yearly matrix
    
    for(i in c(1:95)){ #aging from 1 to 95
      
      if(sum(S[i,])<n){ #checking the maximum allowed number of adenoma condition
        r0<-runif(1,0,1) #adenoma incidence in each year
        f<-0
        for(j in c((n-sum(S[i,])):1)){#number of adenoma that can be generated
          if(r0<=(AIR[i])^(j) & f==0){ #number of adenoma that the body generates 
            S[i,1]<-S[i,1]+(j) #adding generated adenomas to body
            f<-1
          }
          j<-j-1
        }
      }
      
      D<-S[i,1] #number of dimunitive adenomas in this state
      
      if(D>=1){ #in case the patient have at least one small adenoma
        for(l in c(1:D)){ #counting adenoma
          
          r1<-runif(1,0,1) #
          if(r1<=AGR[1]){ #10
            S[i,1]<-S[i,1]-1 
          }
          
          if(r1>=(1-AGR[2])){ #12
            S[i,1]<-S[i,1]-1
            S[i,2]<-S[i,2]+1
            
          }
          l<-l+1 #next adenoma
        }
      }
      
      M<-S[i,2] #number of medium adenomas in this state
      
      if(M>=1){ #in case the patient have at least one small adenoma
        for(m in c(1:M)){ #counting adenoma
          r2<-runif(1,0,1)
          
          if(r2<=AGR[3]){ #21
            S[i,2]<-S[i,2]-1
            S[i,1]<-S[i,1]+1
          }
          
          if(r2>=(1-AGR[4])){ #23
            S[i,2]<-S[i,2]-1
            S[i,3]<-S[i,3]+1
          }
          
          if(r2>=0.5 & r2<=0.5+AGR[5]){ #24
            S[i,2]<-S[i,2]-1
            S[i,4]<-S[i,4]+1
          }
          m<-m+1 #next adenoma
        }
      }
      
      L<-S[i,3] #number of large adenomas in this state
      if(L>=1){ #in case the patient have at least one small adenoma
        for(o in c(1:L)){ #counting adenoma
          r3<-runif(1,0,1)
          
          if(r3<=AGR[6]){ #32
            S[i,3]<-S[i,3]-1
            S[i,2]<-S[i,2]+1
          }
          
          if(r3>=(1-AGR[7])){ #34
            S[i,3]<-S[i,3]-1
            S[i,4]<-S[i,4]+1 
          }
          
          o<-o+1 #next adenoma
        }
      }
      
      if(i<=94){ #copying matrix row to the next row
        S[i+1,]<-S[i,]
        i<-i+1 #next year
      }  
    }
    
    A50<-A50+length(which(S[50,4]>0))
    A55<-A55+length(which(S[55,4]>0))
    A60<-A60+length(which(S[60,4]>0))
    A65<-A65+length(which(S[65,4]>0))
    A70<-A70+length(which(S[70,4]>0))
    A75<-A75+length(which(S[75,4]>0))
    A80<-A80+length(which(S[80,4]>0))
    A85<-A85+length(which(S[85,4]>0))
    A90<-A90+length(which(S[90,4]>0))
    A95<-A95+length(which(S[95,4]>0))
    
    
    P[k,]<-S[SA,] #copying each person's 50th year condition into one matrix
    
    k<-k+1 #next person
  } #simulating K people
  
  Summary<-c((K),0,0,0,0,0,0,0,0,
             length(which(P[,1]==0)),length(which(P[,1]==1)),length(which(P[,1]==2)),length(which(P[,1]==3)),length(which(P[,1]==4)),length(which(P[,1]==5)),length(which(P[,1]==6)),length(which(P[,1]==7)),length(which(P[,1]==8)),
             length(which(P[,2]==0)),length(which(P[,2]==1)),length(which(P[,2]==2)),length(which(P[,2]==3)),length(which(P[,2]==4)),length(which(P[,2]==5)),length(which(P[,2]==6)),length(which(P[,2]==7)),length(which(P[,2]==8)),
             length(which(P[,3]==0)),length(which(P[,3]==1)),length(which(P[,3]==2)),length(which(P[,3]==3)),length(which(P[,3]==4)),length(which(P[,3]==5)),length(which(P[,3]==6)),length(which(P[,3]==7)),length(which(P[,3]==8)),
             length(which(P[,4]==0)),length(which(P[,4]==1)),length(which(P[,4]==2)),length(which(P[,4]==3)),length(which(P[,4]==4)),length(which(P[,4]==5)),length(which(P[,4]==6)),length(which(P[,4]==7)),length(which(P[,4]==8))
  )
  Summary1<-(c(A50,A55,A60,A65,A70,A75,A80,A85,A90,A95)*PR)
  
  Result<-Result+matrix(Summary,9,5,byrow=FALSE,
                        dimnames = list(c("ZERO","ONE","TWO","THREE","FOUR","FIVE","SIX","SEVEN","EIGHT"),
                                        c("Healthy","Dimunitive","Medium","Large","CRC")))
  
  Result1<-Result1+matrix(Summary1,1,10,dimnames = list("CRC-CASES",
                                                        c("A50","A55","A60","A65","A70","A75","A80","A85","A90","A95")))
  
  q<-q+1 #next iteration
}

FR<-(Result*100000*PR[AI])/(Q*K) #Final result of specified age
AA<-(Result1*100000)/(Q*K) #Final result of all ages

library(xlsx)

write.xlsx(FR,"C:/Users/DMRLAG/Desktop/FR.xlsx",sheetName = "FR")
write.xlsx(AA,"C:/Users/DMRLAG/Desktop/AA.xlsx",sheetName = "AA")

FR
AA 
