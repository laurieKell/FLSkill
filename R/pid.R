nextCatch<-function(bstate, fstate, btrnd, ftrnd,
                    aucBstate, aucFstate, aucBtrnd, aucFtrnd,
                    ent,
                    CNow,
                    lambdaBstate=1.0, lambdaFstate=1.0, lambdaBtrnd=1.0, lambdaFtrnd=1.0,
                    Kp       = 0.5,     # proportional gain
                    entLow   = 0.0,     # entropy range for scaling
                    entHigh  = 1.0,
                    gMin     = 0.2,     # minimum gain when entropy is high
                    maxChange= 0.3      # max +/- annual change (30%)
) {
  
  # 1. Build "good = positive" errors
  eB  = bstate         # higher biomass relative to target is good
  eF  =-fstate         # higher F than target is bad, so flip sign
  edB = btrnd          # positive trend in B is good
  edF =-ftrnd          # increasing F is bad, so flip sign
  
  # 2. Convert AUCs to skills above random and normalise to weights
  skill=c(
    max(0, aucBstate - 0.5)*lambdaBstate,
    max(0, aucFstate - 0.5)*lambdaFstate,
    max(0, aucBtrnd  - 0.5)*lambdaBtrnd,
    max(0, aucFtrnd  - 0.5)*lambdaFtrnd)
  
  if (sum(skill) == 0) {
    w=rep(1/4, 4)
  } else {
    w=skill / sum(skill)
  }
  
  # 3. Composite error (weighted by AUC skill)
  E=w[1] * eB + w[2] * eF + w[3] * edB + w[4] * edF
  
  # 4. Entropy-based gain: higher ent -> lower gain
  # Linear mapping entLow..entHigh -> 1..gMin, clipped
  entScaled=(ent - entLow) / (entHigh - entLow)
  entScaled=min(max(entScaled, 0), 1)
  gEnt=1 - (1 - gMin) * entScaled
  
  # 5. Proportional control on log-catch
  delta=gEnt * Kp * E
  
  # 6. Update catch multiplicatively and apply annual change cap
  CTarget=CNow * exp(delta)
  
  # impose max +/- change
  CMin=CNow * (1 - maxChange)
  CMax=CNow * (1 + maxChange)
  
  CNext=max(CMin, min(CMax, CTarget))
  
  return(CNext)}
