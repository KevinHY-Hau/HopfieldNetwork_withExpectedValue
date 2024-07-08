# HopfieldNetwork_withExpectedValue
Hopfield Network with expected value

The original hopfield network only calculates the stochastic model probability that depends on the previous 3 states, and it's nicely a recurrent type of network. 
The algorithm I proposed takes a step further to include the expected values and at the same time, improved to take any number of previous step > 1, and reduced the exponential steps of calculations as time increase. 
This is original applied to calculating the expected value of debt returm. 
The algorihtm is now basically a step improved to the binomial tree, where binomial tree only use the last state, while this can look back more than 1 step.
The result of the algorithm can replace binomial tree, where it only depends on the last state.

This algorithm also reduced the expoential step of calculations o(2^n) to o(n^2) too by aggregrating the probability based on number of payments.
