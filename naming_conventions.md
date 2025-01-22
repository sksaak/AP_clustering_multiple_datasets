# Naming conventions

To ease the applicability of the code to various datasets we have decided to use certain names for the features of audiological tests. This ensures that the code can work with more datasets than the datasets tested. These naming conventions are not set in stone but are used consistently throughout the code. It may be reasonable to update these conventions in the future to streamline the names with e.g. openEHR naming proposals. This needs to be done then consistently throughout the code. 

Here are the naming conventions for audiological measures thus far included:

## General data

id = patient ID
age = age in years 


## Audiogram 

Feature names are structured in the following way:

"TestEar_Frequency"

Test: 
AC = Air-conduction
BC = Bone-conduction 
UCL = Uncomfortable level

Ear: 
R = Right ear
L = Left ear 

Frequency:
in Hz = Frequencies 

Examples:
Air-conduction result for the right ear at 2000 Hz
-> AC_R_2000
Bone-conduction result for the left ear at 500 Hz
-> BC_L_500
Uncomfortable level for the right ear at 4000 Hz
-> UCL_R_4000



## Speech tests 

Feature names are structures in the following way: 

"test_condition_presentation"

Test:
goesa = Goettingen Sentence Test

Condition:
S0N0 = SRT where speech & noise from 0°
S0N90 = SRT where speech from 0° and noise from 90°
ILD = intelligibility level difference
BILD = binaural intelli

Presentation: 
bin = binaural
mon = monaural

Examples:
goesa_S0N0_bin
goesa_ILD
goesa_BILD
goesa_S0N0_mon



## Adaptive Categorical Loudness Scaling 
Feature names are structured in the following way:

Test: 
acalos = adaptive categorical loudness scaling

Ear:
R = right ear
L = left ear

Frequency:
in Hz = frequencies 

Condition
Lcut = slope at cutpoint
mlow = slope of lower portion
mhigh = slope of higher portion
L2_5 = loudness at level 2.5
L15 = loudness at level 15
L35 = loudness at level 35
diff = L35-L15
L25 = loudness at level 25
L50 = loudness at level 50 


Examples:
acalos_R_500
acalos_L_1000
acalos_L_4000



## Otoscopy 

Oto_R = Result from the Otoscopy for the right ear 
Oto_L = Result from the Otoscopy for the left ear 

-> factor with levels: not okay, not completely okay, okay


## Valsalva 
Val_R = Result from the Valsalva for the right ear
Val_L = Result from the Valsalva for the left ear

-> factor with levels: not okay, not completely okay, okay


## Tympanogram 
Tymp_R = Result from the Tympanogram for the right ear 
Tymp_L = Result from the Tympanogram for the left ear 

-> factor with levels: 





