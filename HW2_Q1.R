
MYUNIFORM <- function(inputvar) {

  IM1<-2147483563
  IM2<-2147483399
  IA1<-40014
  IA2<-40692
  IQ1<-53668
  IQ2<-52774
  IR1<-12211
  IR2<-3791
  NTAB<-32
  EPS<-1.2e-7
  RNMX<-1.-EPS

  IMM1<-IM1-1
  NDIV<-as.integer(1+IMM1/NTAB)
  AM<-1.0/IM1
  
  idum<-inputvar[1]
  idum2<-123456789
  numran<-inputvar[2]
  ran2<-0
  iy<-0
  iv<-rep(0,NTAB)
  rand_uniform_c<-rep(0,numran)

  icount<-1
  for (icount in 1:numran) {
    if (idum <= 0) {
      idum<-max(-idum,1)
      idum2<-idum
      j<-NTAB+8
      while (j > 0) {
        k=as.integer(idum/IQ1)
        idum<-IA1*(idum-k*IQ1)-k*IR1
        if (idum < 0) {idum=idum+IM1}
        if (j <= NTAB) {iv[j]<-idum}
        j<-j-1
      }
      iy<-iv[1]
    }
    k<-as.integer(idum/IQ1)
    idum<-IA1*(idum-k*IQ1)-k*IR1
    if(idum < 0) {idum=idum+IM1}
    k=as.integer(idum2/IQ2)
    idum2<-IA2*(idum2-k*IQ2)-k*IR2 
    if (idum2 < 0) {idum2<-idum2+IM2}
    j<-as.integer(iy/NDIV)+1
    iy<-iv[j]-idum2
    iv[j]<-idum
    if(iy < 1) {iy<-iy+IMM1}
    ran2<-min(AM*iy,RNMX)
    rand_uniform_c[icount]<-ran2
  }
  return(rand_uniform_c)
}


MYNORM <- function(rand_c) {
  a0<-2.50662823884
  a1<--18.61500062529
  a2<-41.39119773534
  a3<--25.44106049637
  b0<--8.47351093090
  b1<-23.08336743743
  b2<--21.06224101826
  b3<-3.13082909833
  c0<-0.3374754822726147
  c1<-0.9761690190917186
  c2<-0.1607979714918209
  c3<-0.0276438810333863
  c4<-0.0038405729373609
  c5<-0.0003951896511919
  c6<-0.0000321767881768
  c7<-0.0000002888167364
  c8<-0.0000003960315187

  jcount<-1
  numran<-length(rand_c)
  rand_norm_c<-rep(0,numran)
  while(jcount <= numran) {
    u<-rand_c[jcount]
    y<-u-0.5
    if(abs(y) < 0.42) {
      r<-y*y
      x<-y*(((a3*r+a2)*r+a1)*r+a0)/((((b3*r+b2)*r+b1)*r+b0)*r+1)
    } else {
      r<-u
      if(y>0){r<-1-u}
      r<-log(-log(r))
      x<-c0+r*(c1+r*(c2+r*(c3+r*(c4+r*(c5+r*(c6+r*(c7+r*c8)))))))
      if(y<0){x<--x}
    }
    rand_norm_c[jcount]<-x
    jcount=jcount+1
  }
  return(rand_norm_c)
}


seed<--2000
numran<-1000000
inputvar<-c(seed,numran)
rand_uniform_c<-MYUNIFORM(inputvar)
rand_uniform_c[1:10]

rand_norm_c<-MYNORM(rand_uniform_c)
cat("\n")
rand_norm_c[1:10]


Price_Paths <- function(T, intervals, rf, Volatility, numpath) {
  S0<-100
  Strike<-100
  TotalT=1
  T=TotalT/12
  Volatility<-0.25
  ST_One<-matrix(rep(0),numpath)
  ST_One_Rets<-matrix(rep(0),numpath)
  ST_Vals<-matrix(rep(0),nrow=12, ncol=numpath)
  ST_Rets<-rep(0,numpath)
  BS_12_Val1<-matrix(rep(0),numpath)
  BS_12_Val2<-matrix(rep(0),numpath)
  jcount<-1
  lcount<-1
  while(jcount <= numpath) {
    St<-S0
    for (kcount in 1:12) {
      St<-St*(exp((rf-0.5*(Volatility^2))*T+Volatility*sqrt(T)*rand_norm_c[lcount]))
      ST_Vals[kcount,jcount]<-St
      lcount<-lcount+1
    }
    ST_Rets[jcount]<-log(ST_Vals[12,jcount]/S0)
    BS_12_Val1[jcount]<-max(max(ST_Vals[,jcount]-Strike),0)
    BS_12_Val2[jcount]<-max(ST_Vals[12,jcount] - min(ST_Vals[,jcount]),0)
    jcount=jcount+1
  }
  return(list(BS_12_Val1, BS_12_Val2))
  
}

res1000 = Price_Paths(1/12, 12, 0.03, 0.25, 1000)
res10000 = Price_Paths(1/12, 12, 0.03, 0.25, 10000)
mean1 = mean(res1000[[1]][1:1000])*exp(-0.03*1)
mean2 = mean(res1000[[2]][1:1000])*exp(-0.03*1)
sd1 = sd(res1000[[1]][1:1000])*exp(-0.03*1)
sd2 = sd(res1000[[2]][1:1000])*exp(-0.03*1)
lb1 = mean1-1.645*sd1
ub1 = mean1 +1.645*sd1
lb2 = mean2-1.645*sd2
ub2 = mean2 +1.645*sd2



