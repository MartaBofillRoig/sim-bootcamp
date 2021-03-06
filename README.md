MUW Sim-Bootcamp
================

Welcome to the Bootcamp!!

This Bootcamp has been planned to introduce you to the design and
implementation of simulation studies in clinical trials. Ideally, in the
end, you will be able to:

-   program simple trial simulations using different endpoints;

-   use simulation to calculate required sample sizes or calibrate other
    simulation parameters;

-   understand the main idea behind any clinical trial simulation
    software.

We also want to touch more advanced topics such as reporting and
visualisation.

To share your doubts, organise yourselves, or simply stop for a coffee,
here you will find the link to [Gather](https://gather.town/i/mkPDPA2h).

## GitHub Documents

0.1. Organise the scripts in folders within this repository.

0.2. Document all functions and files you create.

## Task 1

### Getting into the basics

#### Data generation and simple tests

1.1. Simulate 100 realizations of a normally distributed variable with
mean 4 and standard deviation 2 (population a). Save the results in the
vector a.

1.2. Simulate 100 realizations of a normally distributed variable with
mean 4.8 and standard deviation 2 (population b). Save the results in
the vector b.

1.3. Use a t-test to test the null hypothesis that the mean of
population a is the same as the mean of population b. What is the
interpretation of the p-value? What happens when you re-sample the
vectors a and b?

1.4. Simulate 500 realizations of a Bernoulli distributed random
variable with success probability p = 0.15 (population c). Save the
results in the vector c. (Hint: Use Binomial Distribution).

1.5. Simulate 500 realizations of a Bernoulli distributed random
variable with success probability p = 0.20 (population d). Save the
results in the vector d.

1.6. Use a *χ*<sup>2</sup>-test to test the null hypothesis that the
success probability of population c is the same as the success
probability of population d. What is the p-value? What happens when you
re-sample the vectors c and d? Which other test could be used to test
this null hypothesis?

#### Repetitions

2.1. The workflows from 1.1-1.3 and 1.4-1.6 assumed that there was
indeed an underlying difference in populations a vs b and c vs d. What
happens if you simulate twice from the same distribution and use a
statistical test to compare the mean/success proportion? If you repeat
this many times and average over the test decisions, which quantity are
you estimating?

2.2. The workflows from 1.1-1.3 and 1.4-1.6 could correspond to a single
clinical trial, e.g. testing whether two patient populations differ with
respect to a treatment outcome, whereby the realizations correspond to
different observed patient outcomes. As a biostatistician, you are
interested in investigating how often on average the statistical test
would wrongly not reject the null hypothesis (type two error). How can
you do this (and then calculate the power)?

2.3. For tasks 1.1-1.3 we have a power of 80% and a type 1 error of 5%,
which is what is usually required for a clinical study. Assume that the
assumptions of a t-test are violated with respect to the normality of
the data, i.e. both populations follow a log-normal distribution with
the above means and standard deviations. What happens with the power and
type 1 error?

2.4. For tasks 1.1-1.3, what happens with the power and type 1 error if
we use a Wilcoxon test instead of a t Test?

## Task 2

### Moving towards advanced concepts

#### Custom Test Functions

3.1. Write a function z\_test() that takes as input parameters 1) two
vectors of observations x and y, 2) an assumed common standard deviation
and 3) the mean difference corresponding to the null hypothesis (delta)
(two sample z-Test formula can be found online
e.g. [here](https://www.cliffsnotes.com/study-guides/statistics/univariate-inferential-tests/two-sample-z-test-for-comparing-two-means)).
The output should be a list including the test statistic and the
p-value.

3.2. Repeat the exercises from 1.1-1.3 now using the z\_test() function
and the true standard deviation. Compare the results with those obtained
previously.

#### Runtime considerations

4.1. What are possible ways in R to measure the runtime of a particular
program or line of code? Choose one option from this
[article](https://www.r-bloggers.com/2017/05/5-ways-to-measure-running-time-of-r-code/)
and measure the runtime of task 3.2, whereby you run multiple
repetitions similar to the tasks in section 2. Try to increase the
number of repetitions, until you achieve a runtime of 5 seconds.

4.2. For tasks 1.1 and 1.2 you know the distribution of the two sample
means ([hint](https://en.wikipedia.org/wiki/Sampling_distribution)).
Instead of simulating the observations and passing them to the z\_test()
function, you can also simulate the means and implement the z-Test
directly (i.e. a numerical operation that returns the p-value). Use the
same number of repetitions as in 4.1. Did the runtime change? Did the
simulated power change? What changes, if you increase the number of
observations from 100 to 10.000? (Hint: Make sure to reduce the number
of repetitions.)

## The ultimate challenge

#### Full Simulation Function

5.1. Based on everything you have programmed so far, write a simulation
function that calculates the rejection probability of a particular set
of assumptions (sample sizes, means, standard deviations). This function
takes the following input parameters: 1) Sample Sizes in both groups
(assumed to be equal), 2) Means and Standard Deviations of both groups,
3) Choice of t-test, z-test or Wilcoxon test, 4) Number of simulation
repetitions. The output should be the rejection probability of the
statistical test.

5.2. Visualize the impact of the sample size, assumed means and test
procedure on the power and type 1 error. On the y-axis, both power and
type 1 error should be plotted. On the x-axis you have the sample size,
in the rows the test procedure and in the columns the assumed means. Fix
the standard deviation to a sensible value. Use ggplot and the
facet\_grid() function.

#### Shiny and R package

6.1. Implement everything from section 5 in Shiny.

6.2. Create an R package with the function created in 5.1.
