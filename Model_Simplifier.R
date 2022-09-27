############################
# Model Simplifier
# This script:
# 1. simulates a random data set, several variables all from a normal distribution
# 2. run a loop to perform model selection:
#    start from the full model with all effects (main, and pairwise interactions)
#    one at a time, remove the effect with the highest P value
#    stop and report the simplest model with only significant effect(s).
############################
# YP 2022.07.21 
############################

# 0.0 change these numbers to play -----------
N_OBS = 30
N_MAIN_EFFECTS = 6

# 0.1 setups ---------
# tools 
require(data.table)
require(magrittr)

# 0.2 all effects: main and interactions -----------
## main effects 
EFFECTS_MAIN = LETTERS[1:N_MAIN_EFFECTS]
## all pairwise combination of effects 
myef2 = transpose(data.table(combn(EFFECTS_MAIN, 2) ))
myef2[, INTERACTIONS := paste0( V1, ":" , V2)]
EFFECTS_INTERACTIONS = myef2$INTERACTIONS

N_EFFECTS = length(EFFECTS_MAIN) + length(EFFECTS_INTERACTIONS)

# 0.3 function ----------
# OUT values : P and sigYN (P <0.05 or pseudo-significance if the main effect presents in an interaction term)
extract_Pval = function ( lm) {
    sm =summary(lm )
    fe = sm $coefficients %>% data.frame
    fe = data.table( pred = row.names(fe),  fe  )
    setnames(fe, c('Predictor', 'Estimate', "SE", "Z",  "P"))
    # remove intercept 
    fe = fe[ Predictor != "(Intercept)"]
   
    fe[, rankP := rank(-P) ] # rankP, high to low 
    fe[, sigYN := ifelse(P< 0.05, 1, 0)] # which effect is significant
    # Note that a main effect is classified as pseudo-significant if it presents in an interaction term
    fe[ Predictor %in% EFFECTS_MAIN , sigYN := ifelse( paste(fe[ Predictor %in% EFFECTS_INTERACTIONS ]$Predictor , collapse = " " ) %like% Predictor, 1, 0 ), by = Predictor]
    
    fe
  }

set.seed(7)
# 1 randomly generate data -------
  ## randomly generate a data.table with N_OBS rows and N_MAIN_EFFECTS +1 columns, all values are from a normal distribution
  mydata = matrix( data = rnorm( (N_OBS *(N_MAIN_EFFECTS + 1)) ), 
                   nrow = N_OBS, ncol = (N_MAIN_EFFECTS + 1) ) %>% data.table
  ## col names of Y, A, B, C...
  colnames (mydata) = c("Y", LETTERS[1:N_MAIN_EFFECTS])
 
# 2. model selection --------------
  ## increment loop, starting from the full model with all effects
  ## remove the effect with the highest P value, one at a time
  ## stop and report the simplest model with only significant effect(s).
  ## Note that the main effect has to stay in the model if its interaction term is significant
  
  # loop starts
  for (i in 1:1000) { 
  
  # write my formula
  # first full model
  if (i == 1) { MODFUN = paste( c("Y~1", EFFECTS_MAIN,  EFFECTS_INTERACTIONS), collapse = "+" ) } 
  # or a reduced model based on the previous model
  if (i > 1 ) { MODFUN = paste( c("Y~1", EFFECTS_LEFT), collapse = "+" ) } 
    
  # run lm function
    mymod = lm(   MODFUN  , mydata)  
 
  # Get the P values for effects, and get sigYN: 1 either P <0.05 or a non-significant main effect was used in a significant interaction term
    myP = extract_Pval(mymod)
    
  # if no non-significant effects or no effect left, stop here and return the model 
    if (  nrow( myP[ sigYN == 0 ]) == 0)  { print ( summary( mymod)); cat( paste("Your simplest best-fit model is:",   MODFUN) ); break()}
  
  # if there are non-significant effects  
  # take out the most non-significant effect, one at a time 
    if ( nrow( myP[ sigYN == 0 ]) >= 1 ) {  
       
      CANDI_TO_RM = myP[ sigYN == 0][ order(-P)]$Predictor[1]
      
      EFFECTS_LEFT = myP [ ! Predictor %in% CANDI_TO_RM]$Predictor 
      
      i = i+1
      }  
}
