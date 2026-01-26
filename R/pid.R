#' Next-year catch from a PID-style HCR
#'
#' @description Calculates the next year's TAC/catch based on four indicators
#'   (biomass state, Fstate, trends) using AUC-weighted composite error and
#'   entropy-based damping.
#'
#' @param bstate     Numeric. Biomass state (log-ratio to target).
#' @param fstate     Numeric. Fishing mortality state (log-ratio to target).
#' @param btrnd      Numeric. Biomass trend (slope).
#' @param ftrnd      Numeric. F trend (slope).
#' @param aucBstate  Numeric. AUC (0.5-1) for bstate indicator quality.
#' @param aucFstate  Numeric. AUC for fstate indicator quality.
#' @param aucBtrnd   Numeric. AUC for btrnd indicator quality.
#' @param aucFtrnd   Numeric. AUC for ftrnd indicator quality.
#' @param ent        Numeric. Entropy (0-1) of biomass time series.
#' @param CNow       Numeric. Current year's catch.
#' @param lambdaBstate Numeric (default 1.0). Multiplier on bstate weight.
#' @param lambdaFstate Numeric (default 1.0). Multiplier on fstate weight.
#' @param lambdaBtrnd  Numeric (default 1.0). Multiplier on btrnd weight.
#' @param lambdaFtrnd  Numeric (default 1.0). Multiplier on ftrnd weight.
#' @param Kp         Numeric (default 0.5). Proportional gain.
#' @param entLow     Numeric (default 0.0). Lower entropy bound.
#' @param entHigh    Numeric (default 1.0). Upper entropy bound.
#' @param gMin       Numeric (default 0.2). Minimum gain at high entropy.
#' @param maxChange  Numeric (default 0.3). Max ± annual TAC change (30%).
#'
#' @return Numeric. Next year's catch, constrained by maxChange.
#' @details
#'   The rule constructs a composite error E from the four indicators,
#'   weighted by lambda_i * max(0, AUC_i - 0.5). Entropy dampens the
#'   proportional response via a linear gain g_ent between 1 (low entropy)
#'   and gMin (high entropy). The update is multiplicative on log-scale:
#'   C_next=C_now * exp( g_ent * Kp * E ), clipped to [C_now*(1-maxChange),
#'   C_now*(1+maxChange)].
#'
#' @export
nextCatch<-function(bstate, fstate, btrnd, ftrnd,
                    aucBstate, aucFstate, aucBtrnd, aucFtrnd,
                    ent,
                    CNow,
                    lambdaBstate = 1.0, lambdaFstate = 1.0,
                    lambdaBtrnd  = 1.0, lambdaFtrnd  = 1.0,
                    Kp           = 0.5,   # P gain
                    Kd           = 0.0,   # D gain (set >0 to use trends)
                    entLow       = 0.0,
                    entHigh      = 1.0,
                    gMin         = 0.2,
                    maxChange    = 0.3) {
  if(FALSE){
    bstate=1.5;fstate=0.9;btrnd=0.1;ftrnd=-0.1;aucBstate=aucFstate=aucBtrnd=aucFtrnd=0.5; ent=0.5
    CNow=1000
    
    lambdaBstate = 1.0; lambdaFstate = 1.0
    lambdaBtrnd  = 1.0; lambdaFtrnd  = 1.0
    Kp           = 0.5   # P gain
    Kd           = 0.0   # D gain (set >0 to use trends)
    entLow       = 0.0
    entHigh      = 1.0
    gMin         = 0.2
    maxChange    = 0.3
  }
  
  ## 1. Build "good = positive" errors
  eB = bstate   # biomass above target -> positive
  eF =-fstate   # F above target is bad, so flip sign
  eDB= btrnd    # biomass increasing -> positive
  eDF=-ftrnd    # F increasing is bad, so flip sign
  
  ## 2. AUC-based skills
  skillP=c(
    max(0, aucBstate - 0.5) * lambdaBstate,
    max(0, aucFstate - 0.5) * lambdaFstate)
  skillD=c(
    max(0, aucBtrnd  - 0.5) * lambdaBtrnd,
    max(0, aucFtrnd  - 0.5) * lambdaFtrnd)
  
  if (sum(skillP) == 0) skillP=rep(1/2, 2) else skillP=skillP / sum(skillP)
  if (sum(skillD) == 0) skillD=rep(1/2, 2) else skillD=skillD / sum(skillD)
  
  ## 3. Separate P and D composite errors
  EP=skillP[1] * eB  + skillP[2] * eF
  ED=skillD[1] * eDB + skillD[2] * eDF
  
  ## 4. Entropy-based gain
  den=entHigh - entLow
  entScaled=if (den == 0) 0 else (ent - entLow) / den
  entScaled=min(max(entScaled, 0), 1)
  gEnt     =1 - (1 - gMin) * entScaled
  
  ## 5. PD control on log-catch
  delta=gEnt * (Kp * EP + Kd * ED)
  
  ## 6. Multiplicative TAC update with cap
  CTarget=CNow * exp(delta)
  
  CMin=CNow * (1 - maxChange)
  CMax=CNow * (1 + maxChange)
  
  CNext=max(CMin, min(CMax, CTarget))
  
  CNext}

nextCatch2<-function(bstate, fstate, btrnd, ftrnd,
                    aucBstate, aucFstate, aucBtrnd, aucFtrnd,
                    ent,
                    CNow,
                    lambdaBstate = 1.0, lambdaFstate = 1.0,
                    lambdaBtrnd  = 1.0, lambdaFtrnd  = 1.0,
                    Kp           = 0.5,   # P gain
                    Kd           = 0.0,   # D gain (set >0 to use trends)
                    entLow       = 0.0,
                    entHigh      = 1.0,
                    gMin         = 0.2,
                    maxChange    = 0.3) {
  if(FALSE){
    bstate=1.5;fstate=0.9;btrnd=0.1;ftrnd=-0.1;aucBstate=aucFstate=aucBtrnd=aucFtrnd=0.5; ent=0.5
    CNow=1000
    
    lambdaBstate = 1.0; lambdaFstate = 1.0
    lambdaBtrnd  = 1.0; lambdaFtrnd  = 1.0
    Kp           = 0.5   # P gain
    Kd           = 0.0   # D gain (set >0 to use trends)
    entLow       = 0.0
    entHigh      = 1.0
    gMin         = 0.2
    maxChange    = 0.3
  }
  
  ## 1. Build "good = positive" errors
  eB = bstate   # biomass above target -> positive
  eF =-fstate   # F above target is bad, so flip sign
  eDB= btrnd    # biomass increasing -> positive
  eDF=-ftrnd    # F increasing is bad, so flip sign
  
  ## 2. AUC-based skills
  skillP=c(
    max(0, aucBstate - 0.5) * lambdaBstate,
    max(0, aucFstate - 0.5) * lambdaFstate)
  skillD=c(
    max(0, aucBtrnd  - 0.5) * lambdaBtrnd,
    max(0, aucFtrnd  - 0.5) * lambdaFtrnd)
  
  if (sum(skillP) == 0) skillP=rep(1/2, 2) else skillP=skillP / sum(skillP)
  if (sum(skillD) == 0) skillD=rep(1/2, 2) else skillD=skillD / sum(skillD)
  
  ## 3. Separate P and D composite errors
  EP=skillP[1] * eB  + skillP[2] * eF
  ED=skillD[1] * eDB + skillD[2] * eDF
  
  ## 4. Entropy-based gain
  den=entHigh - entLow
  entScaled=if (den == 0) 0 else (ent - entLow) / den
  entScaled=min(max(entScaled, 0), 1)
  gEnt     =1 - (1 - gMin) * entScaled
  
  ## 5. PD control on log-catch
  delta=gEnt * (Kp * EP + Kd * ED)
  
  ## 6. Multiplicative TAC update with cap
  CTarget=CNow * exp(delta)
  
  CMin=CNow * (1 - maxChange)
  CMax=CNow * (1 + maxChange)
  
  CNext=max(CMin, min(CMax, CTarget))
  
  CNext}
