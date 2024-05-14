# HopfieldNetwork_withExpectedValue
Hopfield Network with expected value

The original hopfield network only calculates the stochastic model probability that depends on the previous 3 states, and it's nicely a recurrent type of network. 
This takes a step further to include the expected values. 
This is original applied to calculating the expected value of debt returm. 
The algorihtm is now basically a step improved to the binomial tree, where binomial tree only use the last state, while this can look back more than 1 step.

This algorithm also reduced the expoential step of calculations o(2^n) to o(n^2) too by aggregrating the probability based on number of payments.
