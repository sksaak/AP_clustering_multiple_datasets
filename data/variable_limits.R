#################################################################################
#
#
# Defines the theoretical limits for the variables 
# -> to be used for the density overlap
#
#################################################################################

# adjust limits according to included features
limits = list(
            # AUDIOGRAM
              AC_PTA            = c(-10,120),
              BC_PTA            = c(-10,120),
              ASYM              = c(0,120),
              ABG               = c(0,120),
              UCL_PTA           = c(0,140),
              
            # Anamnesis 
              age               = c(0, 120),
            
            # GOESA
              goesa_S0N0_bin    = c(-14,30),     
              goesa_S0N90_bin   = c(-20,30),
              goesa_S0N90_mon   = c(-20,30),
              goesa_ILD         = c(-20,20),
              goesa_BILD        = c(-20,20),
     
            # ACALOS         
              acalos_1000_L15   = c(0,120),
              acalos_1000_L35   = c(0,120),
              acalos_2000_L15   = c(0,120),
              acalos_2000_L35   = c(0,120),
              acalos_4000_L15   = c(0,120),
              acalos_4000_L35   = c(0,120),
              acalos_1000_diff  = c(0,120),
              acalos_2000_diff  = c(0,120),
              acalos_4000_diff  = c(0,120)
              
          
              )