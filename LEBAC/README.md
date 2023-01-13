# Notes coming from LEBAC 

## Some notes I took during my internship at LEBAC - Laboratory of hydrographic basin studies 
## (OBS: I didn't use real lab data in these files)

# Mann-Kendall

Mann-Kendall test: is a non parametrical test to verify if a time series has a statistically significant trend.

Null hypothesis: there is not a trend in the series.

Alternative hypothesis: there is a trend in the series.

# Sen's slope

Sen's slope: is a method for robust linear regression that chooses the median slope among all lines through pairs of sample points. It's a famous non parametricla method for linear trend estimation.

# Trend free pre-whitening

Trend free pre-whitening: Some articles claim that autocorrelation hinders trend analysis (cite). 

First, the trend estimated by the Sen'Slope is removed. Then the lag-1 auto-correlation coefficient of Trend-Free Series (ro) is calculated and removed. 
Finally, the trend is added again to proceed with the Mann-Kendall test, so we have the Trend Free Pre-Whitening Mann-Kendall test.
