# Author: Collin Mitchell
# Purpose: To be a collection of referenceable functions and examples from the class

## ------ t.test  ------- ##
data( sleep )
sleepSlice <- sleep[, 1:2]


t.test( extra ~ group, data = sleepSlice)

t.test( extra ~ group,
        data = sleepSlice,
        alternative = "less",
        conf.level = .8)

require('MASS')
# paired coordinate plot
pairs( datasetHere )

# parallel Coordinate Plot
parcoord( datasetHere )

# compare one group against itself.
t.test( dataset1, dataset2, paired = TRUE)

t.test( dataset1, dataset2,
        paired = TRUE,
        mu = 5, # set the mean
        alternative = "greater", # one-tail test
        conf.level = .99)


## ---- 2F ANOVA ----- ##
data( warpbreaks )

# Analysis of Variance
AoV <- aov( breaks ~ wool + tension + wool:tension, data = warpbreaks )

summary( AoV )
# Df Sum Sq Mean Sq F value   Pr(>F)    
# wool          1    451   450.7   3.765 0.058213 .  
# tension       2   2034  1017.1   8.498 0.000693 ***
#   wool:tension  2   1003   501.4   4.189 0.021044 *  
#   Residuals    48   5745   119.7                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Additional info about model.
model.tables( AoV )
model.tables( AoV, type = "means")
model.tables( AoV, type = "effects ") # default

# Interaction by Interaction Comparison:
TukeyHSD( AoV )


### ---- PCA   ------ ##
data( mtcars )
mtcarsSubset <- mtcars[ , c( 1:4, 6:7, 9:11)]

# Principle Components:
pc <- prcomp( mtcarsSubset, center = TRUE, scale = TRUE )

# can also use formula notation:
pcForumula <- prcomp( ~ mpg + cyl #... + 
                        data = mtcarsSubset,
                        scale = TRUE)

summary( pc )
# Importance of components:
#   PC1    PC2     PC3     PC4     PC5     PC6     PC7     PC8
# Standard deviation     2.3391 1.5299 0.71836 0.46491 0.38903 0.35099 0.31714 0.24070
# Proportion of Variance 0.6079 0.2601 0.05734 0.02402 0.01682 0.01369 0.01118 0.00644
# Cumulative Proportion  0.6079 0.8680 0.92537 0.94939 0.96620 0.97989 0.99107 0.99750
# PC9
# Standard deviation     0.1499
# Proportion of Variance 0.0025
# Cumulative Proportion  1.0000

# see each Principle Component; "scree plot"
plot( pc )


pc
# Standard deviations (1, .., p=9):
#   [1] 2.3391410 1.5299383 0.7183646 0.4649052 0.3890348 0.3509911 0.3171373 0.2406989
# [9] 0.1498962
# 
# Rotation (n x k) = (9 x 9):
#   PC1         PC2         PC3        PC4         PC5          PC6        PC7
# mpg  -0.4023287  0.02205294 -0.17272803 -0.1366169  0.31654561 -0.718609897  0.3633216
# cyl   0.4068870  0.03589482 -0.27747610  0.1410976  0.02066646 -0.214224005  0.2099893
# disp  0.4046964 -0.06479590 -0.17669890 -0.5089434  0.21525777  0.010052074  0.2007152
# hp    0.3699702  0.26518848 -0.01046827 -0.1273173  0.42166543 -0.254229405 -0.6741641
# wt    0.3850686 -0.15955242  0.33740464 -0.4469327 -0.21141143  0.002897706  0.3392809
# qsec -0.2168575 -0.48343885  0.54815205 -0.2545226  0.05466817 -0.226660704 -0.2986852
# am   -0.2594512  0.46039449 -0.19492256 -0.5354196 -0.55331460 -0.087616182 -0.2135605
# gear -0.2195660  0.50608232  0.34579810 -0.1799814  0.50533262  0.393990378  0.2484622
# carb  0.2471604  0.44322600  0.53847588  0.3203064 -0.25696817 -0.398353829  0.1321064
# PC8         PC9
# mpg  -0.1487806  0.13567069
# cyl   0.7951724  0.11635839
# disp -0.1346748 -0.66099594
# hp   -0.1210386  0.25474680
# wt   -0.1598333  0.57211273
# qsec  0.4144075 -0.19671599
# am    0.1897463 -0.02465169
# gear  0.2614819  0.05482771
# carb -0.1054553 -0.31083546

predict( pc )

# biplot
biplot( pc )

# use a factor analysis
factanal( mtcarsSubset, 1 )
factanal( mtcarsSubset, 2 )
factanal( mtcarsSubset, 3 )

