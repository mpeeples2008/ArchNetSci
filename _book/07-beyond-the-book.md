# Going Beyond the Book{#BeyondTheBook}

Although the Brughmans and Peeples (2022) book covers quite a bit, there are areas of network research that we did not have space to cover in detail. In this section, we plan to offer additional extended discussions of topics given limited attention in the published book or new topics or methods that come up in the future. If you have an idea for an addition here, don't hesitate to let us know we'll do our best to include it (or better yet, collaborate on creating a new section or tutorial to post here).

In this initial version, we provide here an extended discussion of two topics that are given limited attention in the book: 1) exponential random graph models and 2) spatial interaction models.

## Exponential Random Graph Models (ERGM){#ERGM}

Exponential Random Graph Models (ERGM; typically pronounced "UR-gum") are a class of statistical models designed to help represent, evaluate, and simulate ideas about network generating processes and structural properties (for a good introductions to the method see Lusher et al. 2013; and for archaeological cases see Amati et al. 2020; Brughmans et al. 2014; Wang and Marwick 2021). These models allow us to formally represent our theories about how particular patterns of relationships (such as paths of a given length or triads of a specific configuration) or associations (such as mutuality or connections among nodes that share an attribute) emerge and persist in our networks. Further ERGMs help us evaluate how well such theories account for our observed network data. Specifically, an ERGM can be used to generate large numbers of networks in a random process targeted towards particular configurations and associations that represent our theories of interest. We can then compare those simulated networks to our observed network to generate perspectives on the plausibility of our theory. Essentially, ERGMs help us determine how the local tendencies in network formation generate the global properties and structures of our networks.

In many ways, ERGMs are similar to logistic regression models where we predict the presence or absence of ties between pairs of nodes with edge formation modeled as dependent on network structure and properties (e.g., density, transitivity, centralization, etc.). Such models help us assess the probability that the observed network is a product of specified properties or generative processes that may be more or less likely to occur than we would expect by chance in a random network.

The details of ERGMs and the underlying mathematics are beyond the scope of this document, but we present a brief overview of the highlights based heavily on [a workshop on ERGM by the `statnet` team](http://statnet.org/Workshops/ergm_tutorial.html) (Krivitsky et al. 2021). See that workshop for more details. 

The general model form for an ERGM can be written as:

$$P(Y=y) = \frac{\text{exp}(\theta' g(y))}{k(\theta)}$$

where 

* $P(Y=y)$ is the probability that the network will take a given state $y$ among random possibilities $Y$.
* $g(y)$ is the set of model ERGM terms considered. These are essential the covariates in the model.
* $\theta$ is the set of coefficients for model terms.
* $k(\theta)$ is a normalizing constant defined as numerator summed overall all possible networks constrained on the node set $y$. In other words, all possible network configurations that could exist with the given node set.

The general form for an ERGM expressed in terms of the entire network as we see above can also be expressed in terms of the conditional log odds of an edge existing between any two nodes as follows:

$$\text{logit}(Y_{ij}=y_{ij}) = \theta'\delta(y_{ij})$$

where

* $Y_{ij}$ is the random variable for the state of the edge (present or absent) for a given pair of nodes $i$ and $j$ and $y_{ij}$ is the observed state.
* $\delta (y_{ij})$ is the change statistic representing how $g(y)$ (the state of the graph and associated terms) changes if the edge between $i$ and $j$ is active or not. 
* $\theta$ describes the contribution of a term to the log odds of an individual edge between $i$ and $j$ conditioned on the state of all other edges remaining the same (we explain this in more detail below with examples).

The coefficient estimates in ERGM models are returned in log odds which indicates the change in the likelihood of an edge per unit change in the given predictor (this is where the "change statistic") comes in. For example a coefficient estimate $\theta$ of 1.5 for a given term would indicate that the likelihood of an edge is 1.5 times higher for every change of that term by 1 unit. Conversely, an coefficient estimate for a term of -5.5 would suggest that the likelihood of an edge is 5.5 times *less* likely for every unit change of the term. In general, positive coefficients suggest that a given network feature denoted by the term is more common than we would expect by chance and a negative value suggests it is less common than we would expect by chance (given the constraints placed on network construction). The magnitude of the coefficients further provides an indication of how much more or fewer of a given features we see than we would expect. We explain how this works in more detail in the examples below.

<div class="rmdtip">
<p>The log odds is the logarithm of the odds ratio. The odds ratio
refers to the probability that an event occurs divided by the
probability that an event does not occur (1 minus the probability that
it occurs). This can be written formally as:</p>
<p><span class="math inline">\(\text{log}(A) =
\frac{\text{log}(P(A)}{(1-P(A))}\)</span></p>
<p>where</p>
<ul>
<li><span class="math inline">\(\text{log}(A)\)</span> is the log odds
of event A</li>
<li><span class="math inline">\(\text{log}(P(A)\)</span> is the
probability of event A occurring</li>
<li><span class="math inline">\(\text{log}(1-(P(A))\)</span> is the
probability of event A not occurring</li>
</ul>
<p>Negative log odds values indicate that probability of an event
occurring is &lt; 0.5 and positive log odds values indicate that the
probability of an event occurring is &gt; 0.5. Log odds will be exactly
0 when the probability of an event occurring is 0.5.</p>
</div>


### ERGMs in R {#ERGMsInR}

In general, the analysis of ERGMs in R is conducted in three basic steps:

* First, we asses the general properties of interest in our network using exploratory network statistics described [in the Exploratory Network Analysis section of this document](#Exploratory).
* Next, we define our network terms of interest and fit one or more ERGMs to our observed network and assess the results.
* Finally, we assess the goodness of fit of our models and assess the diagnostic statistics for our model generating processes.

If all goes well in the steps above, we can then evaluate our network theory or property of interest in relation to the ERGM that we created.

<div class="rmdnote">
<p>The <code>statnet</code> suite of packages includes a package called
<code>ergm</code> that facilitates the analysis of ERGMs in R and an
additional package called <code>tergm</code> that provides terms and
methods for analyzing temporal networks using ERGMs. Networks need to be
in the <code>network</code> format to be analysed using the
<code>statnet</code> suite of packages.</p>
</div>

Let's initialize our `statnet` suite to get started:


```r
library(statnet) # initialize statnet library
```

In many ways it is easiest to describe what ERGMs do and how they work by example. In the next sections we provide a couple of archaeological examples that highlight some of the ways ERGMs have or could be used in archaeology. We further provide additional resources for taking these methods further.

### Cranborne Chase Visibility Network Example{#CranborneChase}

We start with an example that was described briefly in the Brughmans and Peeples (2022) book in Chapter 4, but not covered in detail. Specifically, we explore the potential generative processes involved in the development of the intervisibility network among long barrows in the Cranborne Chase area in southern England. As this example is only briefly described in the Brughmans and Peeples (2022) book, you may also want to read and follow along with the original article where that analyses first appeared ([Brughmans and Brandes 2017](https://www.frontiersin.org/articles/10.3389/fdigh.2017.00017/)).

Briefly, the network consists of a set of nodes which represent long barrows and edges among them which represent ground-truthed ties of intervisbility between pairs of barrows. The original data came from work by Chris Tilley (1994). These data were used by Brughmans and Brandes (2017) to formally test the notion put forth by Tilley that highly visible barrows "attracted" others over time. In network terms this could be characterized as a "preferential attachment" process. Brughmans and Brandes created an ERGM model with particular properties drawn from Tilley's theoretical model of network development and found that networks simulated with those properties using ERGMs had substantially similar properties to the observed network. Based on this, they considered Tilley's theoretical model plausible.

![Photograph of a long barrow at Cranborne Chase. [Original image by Jim Champion: CC 3.0](https://commons.wikimedia.org/wiki/File:Gussage_down_long_barrow.jpg)](images/long_barrow.jpg){width=100%}

The original ERGM analysis published by Brughmans and Brandes was conducted in a Java program designed for ERGM analysis called [PNet](http://www.melnet.org.au/pnet). Here we replicate some of their results and a few additional analyses using slightly different methods and assumptions in R by way of demonstration. Our results differ slightly from the published results because of the randomness inherent in fitting ERGMs but all coefficient retain the same sign and magnitude suggesting a good replication of the most important results. 

### Assessments of Network Properties{#NetProperties}

Let's start by bringing in our Cranborne Chase network data (as a `network` object) and looking at the general properties of the network object.


```r
load("data/cranborne.Rdata")
cranborne
#>  Network attributes:
#>   vertices = 32 
#>   directed = FALSE 
#>   hyper = FALSE 
#>   loops = FALSE 
#>   multiple = FALSE 
#>   bipartite = FALSE 
#>   total edges= 46 
#>     missing edges= 0 
#>     non-missing edges= 46 
#> 
#>  Vertex attribute names: 
#>     vertex.names 
#> 
#> No edge attributes
```

This network is an undirected, unweighted network object with 32 nodes and 46 edges. Let's look at a few properties of the network including density, mean degree, degree centralization, and number of isolates.


```r
sna::gden(cranborne) # density
#> [1] 0.09274194
mean(sna::degree(cranborne)) # mean degree
#> [1] 5.75
sna::centralization(cranborne, g = 1, degree) # degree centralization
#> [1] 0.1419355
length(sna::isolates(cranborne)) # numbe of isolates
#> [1] 3
```

This is a fairly sparse network with few isolates and a low degree centralization.

Let's plot it with nodes scaled by degree:


```r
set.seed(4367)
plot(cranborne, vertex.cex = (sna::degree(cranborne) / 4) + 1)
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-6-1.png" width="672" />

This network has 3 components and a few isolates. In general many of the nodes have similar degree centrality values but there are a few nodes which appear to have higher degree. We can look at a histogram of degree centrality to further assess the distribution.


```r
hist(sna::degree(cranborne),
     main = "Degree Distribution",
     xlab = "Degree Centrality")
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-7-1.png" width="672" />

### Fitting Models with `ergm`{#FitModels}

Now that we've explored some of the basic properties of our network, the next step is to begin to fit ERGMs to our observed network. The first thing we are going to do is fit a very simple model with only one term. In the `ergm` package "terms" refer to the specific constraints placed on our randomly generated networks (see `?ergm.terms` for a list of the many built-in terms). The most basic term that is included in many models is `edges` which simply refers to the number of edges in a network. An ERGM with a single `edges` term is conceptually equivalent to a typical GLM regression model where the only predictor is the intercept.

In the chunk of code below we see the form that `ergm` model objects take in R. Inside the `ergm` call we have our network on the left hand size `cranborne` followed by `~` and then followed by `edges` which is a built-in "term" in the `ergm` package. As we will see below, when we use multiple terms we separate them by a `+`. Once we have crated our `ergm` model object we then explore the output using the `summary()` function. 


```r
mod_null <- ergm(cranborne ~ edges)
#> Starting maximum pseudolikelihood estimation (MPLE):
#> Evaluating the predictor and response matrix.
#> Maximizing the pseudolikelihood.
#> Finished MPLE.
#> Stopping at the initial estimate.
#> Evaluating log-likelihood at the estimate.
summary(mod_null)
#> Call:
#> ergm(formula = cranborne ~ edges)
#> 
#> Maximum Likelihood Results:
#> 
#>       Estimate Std. Error MCMC % z value Pr(>|z|)    
#> edges  -2.2806     0.1548      0  -14.73   <1e-04 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>      Null Deviance: 687.6  on 496  degrees of freedom
#>  Residual Deviance: 306.4  on 495  degrees of freedom
#>  
#> AIC: 308.4  BIC: 312.6  (Smaller is better. MC Std. Err. = 0)
```

In the output above there are a number of important features that need explanation. 

The summary output includes the call/model formula we used followed by the Maximum Likelihood Results. The output we will focus on here includes the estimates of each model term, the standard error of the estimates, and the p-value associated with that term:

* First, in the example here, we get an estimate of `edges` as `-2.2806` which is the conditional log odds of two nodes having an edge between them (explained further below) 
* Next, we have the standard error of the coefficient estimate.
* And we also have "Pr(>|z|) which is the p-value associated with a particular term. The p-value here is calculated as a function of the relative size of the coefficient estimate and the standard error.

What the estimate (and associated standard error and p-value) indicates is how much a change in the term by one unit changes the likelihood that a particular edge is present. In this case, a change by one unit in the term `edges` refers to the addition of exactly 1 edge to the network ($\delta(g(y)) = 1$) so the coefficient is an estimate of how much the addition of 1 edge to the network changes the likelihood of any particular edge:

$$\begin{aligned}
\text{logit}(p(y)) & = \theta \times \delta(g(y))\\
& = -2.2806 \times \text{change in the number of edges}\\
& = -2.2806 \times 1\\
& = -2.2806
\end{aligned}$$

So in this example, the likelihood of an edge between two particular nodes is `2.2806` times *less* likely for every additional increase in the number of network by 1 edge in the network as a whole. So for every edge added the probability that a particular edge is present decreases. What this negative coefficient means is that an edge is more likely absent than present (and a positive coefficient would suggest the opposite) and thus, if we add an edge elsewhere in the network it is even less likely that our target edge will be active. We can calculate the probability of that an edge is present by taking the inverse logit of $\theta$:


```r
exp(-2.2806) / (1 + exp(-2.2806))
#> [1] 0.09274246
```

As we would expect, this number is very close to the density of the network which is what the `edges` term uses as a constraint (number of edges is a function of density):


```r
sna::gden(cranborne)
#> [1] 0.09274194
```

What this indicates is that if we are trying to predict a given network state (a given set of present an absent edges) and the only information we know is the network density, the probability that a particular edge is present is roughly equal to the network density. As the coefficient is statistically significant, this means that there is a low probability (p-value) of obtaining a model with no terms at random that provides as good or better predictions of the observed than the model including the `edges` term.

Finally we can see our model fit statistics at the bottom with [AIC (Akaike Information Criterion)](https://en.wikipedia.org/wiki/Akaike_information_criterion) and [BIC (Bayesian Information Criterion)](https://en.wikipedia.org/wiki/Bayesian_information_criterion). These are both model fit statistics that can be used to compare competing models where lower values represent better fit between the model and the data. Further, the Null deviance is a measure of how well the network was predicted by a model with no covariates vs. the residual deviance which is a measure of how well the network is predicted by a model with the covariates. Residual deviance will be lower than Null deviance and bigger gap between the two is better. In general, the absolute values of these model fit terms do not matter but rather they provide a means for comparing multiple models for predicting the same observations as we will see below.

### Building a Model Based on Theory{#ModelTheory}

The simple example above built an ERGM predicated on nothing but network density using the `edges` term. As outlined by Brughmans and Brandes (2017) there are specific features of the Cranborne Chase network development process theorized by Tilley which could be converted into a formal ERGM model using specific `ergm.terms`. Specifically, Tilley suggested that long barrows tended to be clustered into groups and intervisibility was a primary concern for some, but not all long barrows. Further, he suggested that long barrows tended to be clustered in sets and include straight paths where multiple barrows were visible from a single point. Finally, Tilley suggested that barrows that were already highly visible tended to attract new visibility connections through time. To capture this theory of network development in formal terms, Brughmans and Brandes (2017) create a set of terms to match Tilley's expectations. They include the following terms:

* **`edges`** - the number of active edges: this term represents the tendency for long barrows to have visibility connections.
* **`triangle`** - the number of closed triangles: this term represents the clustering that Tilley expects in the network as networks with many closed triangles often have distinct clusters.
* **`threetrail`** - the number of paths or trails of 3 (`threepath` and `threetrail` are used equivalently in the `ergm` here) in the network: this term is meant to capture Tilley's visual pathways where multiple barrows are visible in a specific direction.
* **`altkstar`** - alternating stars: this term is used to represent certain nodes with high degree distribution representing the prominent nodes in the network generated through a process of preferential attachment.
* **`isolates`** - the number of isolates in the network: this term is here to capture the tendency for nodes to not be isolate as Tilley describes.

Here are visual representations of these network configurations from Brughmans and Brandes (2017):

![Network terms included in ERGM](images/BrughmansBrandes.jpg){width=60%}

Brughmans and Brandes present two versions of the model in the article. The first excludes the `isolates` term and the second includes it. Let's replicate their results here. Note that we are using different software and terms may be defined slightly differently so our results may differ a bit from their published results. Further, ERGMs include random simulation to two runs of the same model will not return the same results unless we supply a random seed. To do that in the `ergm` call we use a `control` argument as we see below. 

Let's first go over what it is to be included in the terms. We want to first create a model with the terms `edges`, `triangle`, `threetrail`, and `altkstar`. Most of the terms can be used without further arguments but the `altkstar` term needs an additional weight parameter `lambda` and for us to define that weight parameter as fixed (see [term descriptions here](https://zalmquist.github.io/ERGM_Lab/ergm-terms.html#:~:text=ergm%20functions%20such%20as%20ergm,valued%20mode%20and%20vice%20versa.) for more details).

<div class="rmdwarning">
<p>ERGMs can sometimes take quite a bit of time to run as they involve
generating lots of estimates of random variables using the MCMC process.
In order to control the behavior of the MCMC sampling process, we can
use the <code>control</code> argument within the <code>ergm</code>
function. In the examples here we have opted for a fairly large sample
size per chain and a relatively large interval between samples. As we
will see further below, this will help with our coefficient estimates
and model fit but the trade off is time. If you want to simply run the
models in the examples below quickly, you simply remove these three
arguements within the <code>control.ergm</code> function call:
<code>MCMC.burnin</code>, <code>MCMC.interval</code>, and
<code>MCMC.samplesize</code>.</p>
</div>

Let's fit the model and look at the summary. Note when you run this on your own computer you will see additional verbose output on the console as the sampling process proceeds. We have eliminated that here to avoid visual clutter:


```r
mod1 <- ergm(
  cranborne ~ edges + triangle + threetrail +
    altkstar(lambda = 2, fixed = TRUE),
  control = control.ergm(
    MCMC.burnin = 1000,
    MCMC.interval = 10000,
    MCMC.samplesize = 25000,
    seed = 34526
  )
)
summary(mod1)
#> Call:
#> ergm(formula = cranborne ~ edges + triangle + threetrail + altkstar(lambda = 2, 
#>     fixed = TRUE), control = control.ergm(MCMC.burnin = 1000, 
#>     MCMC.interval = 10000, MCMC.samplesize = 25000, seed = 34526))
#> 
#> Monte Carlo Maximum Likelihood Results:
#> 
#>            Estimate Std. Error MCMC % z value Pr(>|z|)    
#> edges      -3.84281    1.05839      0  -3.631 0.000283 ***
#> triangle    1.79394    0.24931      0   7.195  < 1e-04 ***
#> threetrail -0.06218    0.02695      0  -2.307 0.021045 *  
#> altkstar.2  0.73755    0.51720      0   1.426 0.153853    
#> ---
#> Signif. codes:  
#> 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>      Null Deviance: 687.6  on 496  degrees of freedom
#>  Residual Deviance: 277.0  on 492  degrees of freedom
#>  
#> AIC: 285  BIC: 301.8  (Smaller is better. MC Std. Err. = 0.03905)
```

As our results show, we have three significant predictors: `edges`, `triangle`, and `threetrail` and `altkstar` is not significant just as Brughmans and Brandes (2017) found. Looking at our coefficients, our negative `edges` term suggests that edges are more likely absent than present in our model as we would expect given the density. For `triangle` we have a positive coefficient suggesting that `triangles` are more likely than we would expect by chance. Finally, `threetrails` are slightly less common than we would expect in a random network. The difference is small but statistically significant.

Brughmans and Brandes (2017) generated similar results but their assessments of the goodness of fit of their model (see discussion below) caused them to create a second model with an additional term to capture the tendency for nodes to be connected to other nodes (and thus not be `isolates`). 

Let's run the second model and look at the results:


```r
mod2 <- ergm(
  cranborne ~ edges + triangle + threetrail +
    altkstar(2, fixed = TRUE) + isolates,
  control = control.ergm(
    MCMC.burnin = 1000,
    MCMC.interval = 10000,
    MCMC.samplesize = 25000,
    seed = 1346
  )
)
summary(mod2)
#> Call:
#> ergm(formula = cranborne ~ edges + triangle + threetrail + altkstar(2, 
#>     fixed = TRUE) + isolates, control = control.ergm(MCMC.burnin = 1000, 
#>     MCMC.interval = 10000, MCMC.samplesize = 25000, seed = 1346))
#> 
#> Monte Carlo Maximum Likelihood Results:
#> 
#>            Estimate Std. Error MCMC % z value Pr(>|z|)    
#> edges      -8.62662    3.03274      0  -2.844  0.00445 ** 
#> triangle    1.82713    0.23686      0   7.714  < 1e-04 ***
#> threetrail -0.10134    0.03922      0  -2.584  0.00978 ** 
#> altkstar.2  2.50887    1.20530      0   2.082  0.03738 *  
#> isolates   -2.93421    1.67966      0  -1.747  0.08065 .  
#> ---
#> Signif. codes:  
#> 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>      Null Deviance: 687.6  on 496  degrees of freedom
#>  Residual Deviance: 274.2  on 491  degrees of freedom
#>  
#> AIC: 284.2  BIC: 305.3  (Smaller is better. MC Std. Err. = 0.04273)
```

In this model we again obtain results that mirror those of Brughmans and Brandes (2017). We see with our `edges` term a tendency for edges to be absent as we would expect. For `triangle` we see a strong tendency for closed triangles in our network as Tilley's model predicted. We do not however see a tendency towards visual pathways beyond what we would expect by chance as our `threetrail` term suggests a slight tendency away from these configurations. With the addition of the `isolates` term our `altkstar` term is significant and positive suggesting a tendency for some nodes to have higher degree than most. Finally, `isolates` is negative suggesting a tendency against isolated nodes but the p-value is a bit higher so we should not put too much interpretive weight in this coefficient estimate.

### Assessing Goodness-of-Fit{#GOF}

If we compare model fit statistics we can see that the AIC for model 2 is slightly lower than for model 1. Further, the difference between the Null and residual deviance is slightly greater for model 2. At the same time, the BIC for model 2 is slightly higher than for model 1. Overall this suggests that the two models are quite similar in terms of their improvement over a model with no predictors but we don't have strong statistical argument from these terms alone for picking one over the other (and thus it probably makes sense to evaluate fit statistics as we show here or theoretical arguments for preferring one model or the other).

To take this further we can use the `gof` or goodness-of-fit function in `ergm` to assess the degree to which our models provide reasonable descriptions of our observations. We can start by running the `gof` function for both models. This function provides visualizations and other statistics to help assess the degree to which model statistics, node degree, edge-wise shared partners, and geodesic distance between nodes are preserved in the networks simulated in the ERGM. If you run this function on a directed network, you additionally get assessmess of indegree and outdegree.


```r
mod1_gof <- gof(mod1)
mod2_gof <- gof(mod2)

mod1_gof$summary.model
#>                  obs      min      mean      max MC p-value
#> edges       46.00000  33.0000  45.30000  59.0000       0.94
#> triangle    17.00000   4.0000  16.27000  35.0000       0.96
#> threetrail 446.00000 185.0000 425.82000 858.0000       0.88
#> altkstar.2  91.34375  56.9375  88.94297 130.0781       0.86

mod2_gof$summary.model
#>                  obs    min      mean      max MC p-value
#> edges       46.00000  34.00  45.89000  59.0000       1.00
#> triangle    17.00000   5.00  18.07000  43.0000       0.84
#> threetrail 446.00000 209.00 447.84000 896.0000       0.92
#> altkstar.2  91.34375  58.25  91.27828 134.7656       0.94
#> isolates     3.00000   0.00   3.24000   9.0000       1.00
```

The summary output for each model shows the observed feature value for a given term and then the min, max, and mean value in the simulated networks. In general, we want the mean values to match closely with relatively small ranges around them. The MC p-value (Markov Chain p-value) provides and indication of fit here where higher numbers generally indicate a better fit. This is essentially the proportion of the steps in the chain where a given term met certain criteria. In general the results here suggest that the model terms generally provide a better fit for model 2 than model 1 (as Brughmans and Brandes also suggested using somewhat different goodness-of-fit statistics not directly calculated in `ergm`).

It is also instructive to compare the properties of our randomly generated networks under each model to the observed network for properties that weren't directly included in our model. The `gof` function can be plotted directly to provide this information. Let's look at the four plots provided for both models:


```r
par(mfrow = c(2, 2))
plot(mod1_gof)
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-15-1.png" width="672" />

```r
plot(mod2_gof)
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-15-2.png" width="672" />



In each of these plots the solid black line represents the values for a given property in our observed network and the boxplots represent the distribution of values obtained in our randomly generated networks. As both plots show the median model statistics are quite similar to the observed in both models. In general we want to see the observed values to fall within the densest portion of the values for our randomly generated networks (i.e., near the middle of the boxplots and certainly within the range). In our example here, both the observed degree distribution and edge-wise shared partners (the number of nodes with a specific number of partners) are quite similar to the simulated range of values. Importantly, we did not include terms for degree or edge-wise shared partners in our model but it still generated networks that closely match our observed in terms of these properties. This is evidence of a good fit. For minimum geodesic distance (length of shortest paths) however, we see that both models consistently over-estimated the geodesic distance for nodes for middling values. Overall, this suggests a fairly good (but not perfect) match between our simulated and observed network properties despite these properties not be directly included in our models. Importantly, our interpretation of our network doesn't hinge on geodesic distance so this mismatch is not a huge problem. No model is perfect but these results suggest that that model we tested here at least approximates the features of our observed network most relevant to our theoretical model.

### Assessing Models and MCMC Diagnostics{#Diagnostics}

Another important consideration we have not yet discussed is the need to assess the diagnostics of our model generating process to evalute if it operated as expected. The `ergm` package generates our random networks using a [Markov Chain Monte Carlo (MCMC)](https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo) process. MCMC is a means for efficiently randomly sampling from a high-dimensional probability distribution. We want to ensure that as our MCMC process explores the parameter space fully and that it does not generate problematic data such as temporally correlated estimates or highly skewed distributions of coefficient estimates. Problems like these would be an indication of poor model specification (the inappropriate inclusion or exclusion of relevant terms for predicting our network).

In order to assess our models, we can use the `mcmc.diagnostics` function. Here we run it for model 2 and look at the results. We also call the `latticeExtra` package here as that helps make the visual output look a bit better.



```r
library(latticeExtra)
mcmc.diagnostics(mod2)
#> Sample statistics summary:
#> 
#> Iterations = 3127500:62500000
#> Thinning interval = 2500 
#> Number of chains = 1 
#> Sample size per chain = 23750 
#> 
#> 1. Empirical mean and standard deviation for each variable,
#>    plus standard error of the mean:
#> 
#>                 Mean      SD Naive SE Time-series SE
#> edges      4.961e-01   5.810  0.03770        0.03936
#> triangle   1.505e+00   8.300  0.05386        0.06611
#> threetrail 2.358e+01 149.343  0.96906        1.07973
#> altkstar.2 1.796e+00  17.066  0.11074        0.11878
#> isolates   5.895e-04   1.799  0.01167        0.01167
#> 
#> 2. Quantiles for each variable:
#> 
#>               2.5%     25%    50%    75%  97.5%
#> edges       -11.00  -3.000  1.000   4.00  12.00
#> triangle    -11.00  -4.000  0.000   6.00  22.00
#> threetrail -232.00 -81.000 11.000 114.00 357.00
#> altkstar.2  -31.01  -9.656  1.595  13.08  35.78
#> isolates     -3.00  -1.000  0.000   1.00   4.00
#> 
#> 
#> Are sample statistics significantly different from observed?
#>                   edges      triangle    threetrail
#> diff.      4.961263e-01  1.505432e+00  2.358029e+01
#> test stat. 1.260538e+01  2.277319e+01  2.183906e+01
#> P-val.     1.972186e-36 8.456534e-115 9.875989e-106
#>              altkstar.2     isolates Overall (Chi^2)
#> diff.      1.795793e+00 0.0005894737              NA
#> test stat. 1.511878e+01 0.0505052519    8.728784e+02
#> P-val.     1.217728e-51 0.9597197643   1.031866e-182
#> 
#> Sample statistics cross-correlations:
#>                 edges    triangle threetrail altkstar.2
#> edges       1.0000000  0.66456994  0.8629153  0.9781116
#> triangle    0.6645699  1.00000000  0.8495920  0.7523098
#> threetrail  0.8629153  0.84959205  1.0000000  0.9371681
#> altkstar.2  0.9781116  0.75230982  0.9371681  1.0000000
#> isolates   -0.5432036 -0.05753382 -0.2053427 -0.3803291
#>               isolates
#> edges      -0.54320356
#> triangle   -0.05753382
#> threetrail -0.20534272
#> altkstar.2 -0.38032908
#> isolates    1.00000000
#> 
#> Sample statistics auto-correlation:
#> Chain 1 
#>                   edges     triangle   threetrail
#> Lag 0      1.0000000000 1.000000e+00  1.000000000
#> Lag 2500   0.0429091833 1.923780e-01  0.095528809
#> Lag 5000   0.0097481059 4.664168e-02  0.021285573
#> Lag 7500  -0.0004124377 8.484439e-03 -0.004538084
#> Lag 10000  0.0013139749 3.333200e-03  0.002856167
#> Lag 12500  0.0049697502 1.209685e-05  0.006275546
#>             altkstar.2     isolates
#> Lag 0      1.000000000  1.000000000
#> Lag 2500   0.059569304  0.004099518
#> Lag 5000   0.013925158 -0.006208182
#> Lag 7500  -0.003439839  0.009006027
#> Lag 10000  0.002174916  0.003930273
#> Lag 12500  0.007192035 -0.006963082
#> 
#> Sample statistics burn-in diagnostic (Geweke):
#> Chain 1 
#> 
#> Fraction in 1st window = 0.1
#> Fraction in 2nd window = 0.5 
#> 
#>      edges   triangle threetrail altkstar.2   isolates 
#>    -0.3864     0.6796     0.2605    -0.1335     0.7546 
#> 
#> Individual P-values (lower = worse):
#>      edges   triangle threetrail altkstar.2   isolates 
#>  0.6991902  0.4967317  0.7944651  0.8938198  0.4504707 
#> Joint P-value (lower = worse):  0.7725547 .
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-17-1.png" width="672" /><img src="07-beyond-the-book_files/figure-html/unnamed-chunk-17-2.png" width="672" />

```
#> 
#> MCMC diagnostics shown here are from the last round of simulation, prior to computation of final parameter estimates. Because the final estimates are refinements of those used for this simulation run, these diagnostics may understate model performance. To directly assess the performance of the final model on in-model statistics, please use the GOF command: gof(ergmFitObject, GOF=~model).
```

In this output the particularly relevant parts include:

* **sample statistic auto-correlation** - This is a measure of the correlation between values in the MCMC chain for each term across the number of steps (lags) indicated. Ideally, we would want to see low values for all but the Lag 0 and our example here looks good in that respect.
* **sample statistic burn-in diagnostic (Geweke)** - Burn-in refers to the number of points calculated before the MCMC starts recording points that will be included in our coefficient estimates. A burn-in helps deal with "start up effects" that can sometimes appear when we have a poor initial estimate of a parameter. For the Geweke statistics we actually want to obtain p-values close to 1 which, again this example satisfies.
* **MCMC plots** - The plots presented above show two plots for each term. The plot on the left is called the trace plot and it displays every retained value in the MCMC sampling chain included in the estimate. For this plot, we want to see values with roughly even distributions above and below 0 and with no obvious trends. The second plot shows the density of estimates for each term as a simple density plot. For these we want to see roughly bell-shaped curves centered close to 0, which indicates good convergence of our model. In our example here most of our terms look good though `triangle` is slightly skewed. This is not particularly egregious but if working on this model to make a specific argument about our `triangle` term we might choose to run a much longer MCMC chain to improve our fit. For some very complex models this may take many hours so it is often a good idea to run initial models and then set up longer runs overnight or when you will not be using your computer.

<div class="rmdwarning">
<p>In the example above we noted that all of our terms appeared to look
good in our model diagnostics though the term <code>triangle</code>
produced a slightly skewed distribution with a long-tail of randomly
generated networks that had substantially more closed triangles than the
mean. Why might this be?</p>
<p>Let’s consider that our network has 32 nodes and 46 edges with 17
closed triangles. It turns out that it is actually fairly hard to draw
configurations with far fewer than the observed number of closed
triangles given the size of the network, the density, and the tendency
against isolates. Thus, it is not surprising that our random networks
veered slightly away from the observed value in our example here. This
is not uncommon for ERGM terms that include dyadic or triadic
relationships. In a section below we discuss <a href="#Degeneracy">model
degeneracy</a> (which refers to models that fail to converge) and what
can be done about it, including alternatives to the
<code>triangle</code> model term.</p>
</div>


### Simulating Networks from ERGMs{#SimERGMs}

It is possible to generate and explore network simulated using a particular ERGM using the `simulate` function. Let's generate some random networks from model 2 used above and then look at them along with the original network.

In the code below we simply run a single `simulate` function with the model object, the argument `nsim` representing the number of networks we wish to generate, and `seed` which is the random seed for reproducability. The output is a `list()` object containing multiple `network` format objects.


```r
sim_nets <- simulate(mod2, nsim = 9, seed = 34464)

par(mfrow = c(3, 3)) # set up for multipanel plotting
for (i in 1:9) {
  plot(sim_nets[[i]],
       vertex.cex = (sna::degree(sim_nets[[i]]) / 4) + 1)
}
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-19-1.png" width="672" />

```r

par(mfrow = c(1, 1)) # return to single panel
plot(cranborne,
     vertex.cex = (sna::degree(cranborne) / 4) + 1)
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-19-2.png" width="672" />

These simulations help us better understand the model we have created. There are a obvious similarities between the original network and the simulations but there are also key differences. In particular, most of the random simulations created networks with a single large component whereas the original network has multiple components. This likely explains the mismatch in our goodness-of-fit statistics for geodesic distance. We could perhaps deal with this by including additional terms such as terms defined in relation to geographic location or clustering, but that is an experiment for another day.

### Additional Info on ERGM Terms{#ERGMterms}

In the Cranborne Chase example above, we were working with a published example so the hard part (thinking about how a particular theory can be conceptualized in formal network model terms) was done for us. In practice, choosing terms to use can be quite difficult and confusing. This is particularly true because there are multiple terms that do essentially the same thing in different ways. In this section we first walk through a few of the other common options that were not covered above and then provide some advice on where to go next.

In the first example below we will be using the [Cibola technological similarity networks](#Cibola) used in several other portions of this guide. The data imported below includes a `network` object and a data frame that contains attributes relating to the nodes in that network. We load in the data and then assign attributes to the `Cibola_n` object.

The attributes we assign include:

* **region** - A nominal regional designation for each node.
* **pubarch** - A nominal identification of the type of public architecture present at each settlement. Note that model terms cannot include `NA` data so empty values should include names like "none"
* **d_mat** - An edge attribute which is defined by a distance matrix among all settlements in meters.


```r
load("data/Cibola_n.RData")
# Cibola_n network object
# Cibola_attr - attribute data frame

# add node attribute based on region
cibola_n %v% "region" <- cibola_attr$Region
# add node attribute based on public architecture
cibola_n %v% "pubarch" <- cibola_attr$Great.Kiva

# matrix of distances among settlements
d_mat <- as.matrix(dist(cibola_attr[, 2:3]))
```

In many cases we want to use attributes of nodes or edges as predictors in our ERGMs rather than simply network structures. This can be done a few different ways but in the example below we use the `nodematch` term which calculates a coefficient for nodes that share values for a given attribute. We can also set an additional argument in `nodematch` which specifies coefficient for each unique value in the node attribute (`diff = TRUE`). Finally, we use a matrix of geographic distances of edges as a `edgecov` (edge covariate) term. This term expects a square matrix of `n x n` for where `n` is the number of nodes in the network and helps us assess the degree to which the distance between settlemetns is predictive of the presence or absence of an edge.

Let's take a look at an example using all three of these terms:


```r
mod_cibola <- ergm(cibola_n ~ edges + nodematch("region") +
                     nodematch("pubarch", diff = TRUE) +
                     edgecov(d_mat))
summary(mod_cibola)
#> Call:
#> ergm(formula = cibola_n ~ edges + nodematch("region") + nodematch("pubarch", 
#>     diff = TRUE) + edgecov(d_mat))
#> 
#> Maximum Likelihood Results:
#> 
#>                                            Estimate Std. Error MCMC % z value
#> edges                                     1.196e+00  3.513e-01      0   3.405
#> nodematch.region                          1.299e+00  4.593e-01      0   2.828
#> nodematch.pubarch.Cicular Great Kiva      2.843e-01  4.439e-01      0   0.640
#> nodematch.pubarch.none                   -7.750e-01  2.763e-01      0  -2.805
#> nodematch.pubarch.Rectangular Great Kiva -6.913e-01  5.448e-01      0  -1.269
#> edgecov.d_mat                            -2.323e-05  3.847e-06      0  -6.039
#>                                          Pr(>|z|)    
#> edges                                    0.000662 ***
#> nodematch.region                         0.004688 ** 
#> nodematch.pubarch.Cicular Great Kiva     0.521864    
#> nodematch.pubarch.none                   0.005026 ** 
#> nodematch.pubarch.Rectangular Great Kiva 0.204441    
#> edgecov.d_mat                             < 1e-04 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>      Null Deviance: 644.6  on 465  degrees of freedom
#>  Residual Deviance: 489.2  on 459  degrees of freedom
#>  
#> AIC: 501.2  BIC: 526.1  (Smaller is better. MC Std. Err. = 0)
```

This creates output just like our example above and this gives you a sense of how categorical and covariate ERGM terms work. In this example we have a positive coefficient for `edges` suggesting that there more edges are active than are not. Further, we have a positive coefficient for `nodematch.region` indicating that there are more edges between pairs of sites in the same region than would be expected by chance. If we skip down to `edgecov.d_mat` we can see the impact of distance on edges. We have a negative coefficient (which is very close to zero: `-2.323e-05`)which suggests that there are slight more longer distance connections than shorter ones in this network (because although there is a tendency for connections within regions there are also many connections between regions). Finally, we have the `nodematch.pubarch` variables for each value in `pubarch`. The only term that is statistically significant here is `nodematch.pubarch.none` which is negative suggesting that sites without public architecture have fewer connections than we would expect by chance.

The examples above basically cover all of the common applications of `ergm` terms. There are terms that are specific to directed networks, weighted networks, bipartite networks, and even multilayers networks but the basic procedures of using them are covered in the examples above. Everything else is finding the right model to fit your data (and this really is the hard part). There is no magic bullet here but in general we suggest you carefully read the [ERGM term descriptions](https://zalmquist.github.io/ERGM_Lab/ergm-terms.html#:~:text=ergm%20functions%20such%20as%20ergm,valued%20mode%20and%20vice%20versa.) and consider how these different terms relate to your data and network theories. Your efforts will be better spent when your model is designed in relation to a specific and well-described network theory/hypothesis. We suggest reading the archaeological examples of ERGMs cited in this document and in the broader networks literature to get a sense of what is possible before diving into your own ERGM project. 

#### Avoiding Model Degeneracy{#Degeneracy}

Model degeneracy refers to when a specified ERGM never converges. What this means is there is some term or combination of terms in the model that have created a situation where no networks with the given properties can be obtained (or can only be obtained in very rare combinations of circumstances). What this typically looks like when this happens in R is that you enter your `ergm` call at the command line and things appear to be going okay but then you eventually get hung up with something like "Estimating equations are not within tolerance region. Iteration 2 of at most 60" and nothing happens for a long time. 

As described above in our assessment of MCMC diagnostics, this can sometimes happen because you have specified a term that essentially does not allow for for simulated networks that approximate the observed. A classic example is a network with terms for `edges` and `triangle` for triadic closure and no other terms.

If you were to run a model using our Cranborne data using on the `edges` and `triangle` term, it would never converge despite the fact that the `triangle` term was included in the successful model above. As this suggests, poorly specified models are not just about the presence or absence of a single term but about the combination of terms used.

<div class="rmdwarning">
<p>Do not run the chunk of code below. We promise, it doesn’t go
anywhere and will just waste your time.</p>
</div>


```r
mod_fail <- ergm(cranborne ~ edges + triangle)
```

What then, can we do in the place of including terms that cause model degeneracy? Luckily there are a number of additional terms that have been designed to deal with exactly this issue. These include the "geometrically weighted" terms that are already built right into the `ergm` package. For example, the term `gwesp` or geometrically weighted shared partners is a measure of triadic closure that doesn't rely on the specific count of triangles, but instead on the tendency towards closing individual triads in the network.

Let's try our model again substituting the `gwesp` term in the place of `triangle`.


```r
mod_win <- ergm(cranborne ~ edges + gwesp(0.25, fixed = TRUE),
                control = control.ergm(seed = 2362))
summary(mod_win)
#> Call:
#> ergm(formula = cranborne ~ edges + gwesp(0.25, fixed = TRUE), 
#>     control = control.ergm(seed = 2362))
#> 
#> Monte Carlo Maximum Likelihood Results:
#> 
#>                  Estimate Std. Error MCMC % z value Pr(>|z|)    
#> edges             -3.4233     0.3252      0 -10.526   <1e-04 ***
#> gwesp.fixed.0.25   1.0975     0.2614      0   4.198   <1e-04 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>      Null Deviance: 687.6  on 496  degrees of freedom
#>  Residual Deviance: 279.4  on 494  degrees of freedom
#>  
#> AIC: 283.4  BIC: 291.9  (Smaller is better. MC Std. Err. = 0.3086)
```

So the model converges and we get `gwesp` as a statistically significant predictor with a positive coefficient estimate just as we saw with `triangle` in the complete model. Indeed if we include `gwesp` in the complete model we get results that largely mirror those above suggesting that this term is playing a similar role.


```r
mod_win2 <-
  ergm(
    cranborne ~ edges + gwesp(0.25, fixed = TRUE) + threetrail +
      altkstar(2, fixed = TRUE) + isolates,
    control = control.ergm(seed = 1346)
  )
summary(mod_win2)
#> Call:
#> ergm(formula = cranborne ~ edges + gwesp(0.25, fixed = TRUE) + 
#>     threetrail + altkstar(2, fixed = TRUE) + isolates, control = control.ergm(seed = 1346))
#> 
#> Monte Carlo Maximum Likelihood Results:
#> 
#>                  Estimate Std. Error MCMC % z value Pr(>|z|)    
#> edges            -10.6887     3.5864      0  -2.980  0.00288 ** 
#> gwesp.fixed.0.25   1.4626     0.3691      0   3.963  < 1e-04 ***
#> threetrail        -0.0901     0.0464      0  -1.942  0.05213 .  
#> altkstar.2         2.8456     1.3950      0   2.040  0.04136 *  
#> isolates          -4.2948     1.9158      0  -2.242  0.02497 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>      Null Deviance: 687.6  on 496  degrees of freedom
#>  Residual Deviance: 272.3  on 491  degrees of freedom
#>  
#> AIC: 282.3  BIC: 303.3  (Smaller is better. MC Std. Err. = 0.5114)
```

What about the numbers we're providing to the `gwesp` term argument `(0.25, fixed = T)`. These number specify the so-called decay parameter in the model and whether or not that parameter should be fixed or allowed to vary across steps in the MCMC process. The details of this are well beyond the scope of this tutorial but suffice it to say that the general advice is to select the decay value that produces the best fit model in your given analysis. If you run your model without `fixed = T` the model will attempt to estimate the decay parameter and you will get an additional result in our output the specifies the coefficient for that decay term as well. Keep in mind that this is essentially adding a term to the model so it may then be harder or take longer to fit your models.

Here is an example:


```r
mod_nofix <- ergm(cranborne ~ edges + gwesp,
                  control = control.ergm(seed = 2362))
summary(mod_nofix)
#> Call:
#> ergm(formula = cranborne ~ edges + gwesp, control = control.ergm(seed = 2362))
#> 
#> Monte Carlo Maximum Likelihood Results:
#> 
#>             Estimate Std. Error MCMC % z value Pr(>|z|)    
#> edges        -3.4081     0.3144      0 -10.840  < 1e-04 ***
#> gwesp         1.0186     0.2972      0   3.427  0.00061 ***
#> gwesp.decay   0.3447     0.2914      0   1.183  0.23698    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>      Null Deviance: 687.6  on 496  degrees of freedom
#>  Residual Deviance: 279.6  on 493  degrees of freedom
#>  
#> AIC: 285.6  BIC: 298.2  (Smaller is better. MC Std. Err. = 0.3423)
```


In addition to the `gwesp` term, there are many additional terms [listed here](https://zalmquist.github.io/ERGM_Lab/ergm-terms.html#:~:text=ergm%20functions%20such%20as%20ergm,valued%20mode%20and%20vice%20versa.) which fill similar roles and help you build models that avoid degeneracy. For more information see Hunter and Handcock (2006).

## Spatial Interaction Models{#SpatialInteraction}

In the [Spatial Networks](#SpatialNetworks) section of this document we cover most of the simple network models for generating spatial networks based on absolute distance, configurations of locations, and territories. There is one general class of spatial network model that we described briefly in the Brughmans and Peeples (2022) book but did not cover in detail as the specifics require considerably more discussion. This includes a wide variety of spatial interaction models such as gravity models, truncated power functions, radiation models, and similar custom derivations of these approaches. In general, a spatial interaction model is a formal mathematical model that is used to predict the movement of people (or other sorts of entities) between origins and destinations. These models typically use information on the relative sizes or "attractiveness" of origins and destinations or other additional information on volumes of flows in and out. Such models have long been popular in geography, economics, and other fields for doing things like predicting the amount of trade between cities or nations or predicting or improving the location of services in a given geographic extent. Statistical spatial interaction models have been used in archaeology as well for both empirical and simulation studies (e.g., Bevan and Wilson 2013; Evans et al. 2011; Gauthier 2020; Paliou and Bevan 2016; Rihll and Wilson 1987) though they have not had nearly the impact they have had in other fields. We suggest that there is considerable potential for these models, in particular in contexts where we have other independent information for evaluating network flows across a study area.

In this section, we briefly outline a few common spatial interaction models and provide examples. For additional detailed overview and examples of these models there are several useful publications (see [Evans et al. 2011](https://www.researchgate.net/publication/277221754_Interactions_In_Space_For_Archaeological_Models); [Rivers et al. 2011](https://plato.tp.ph.ic.ac.uk/~time/networks/arch/BevanRewriteFigTableInText110727.pdf); and [Amati et al. 2018](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5797198/)). 

### Simple Gravity Models{#GravityModel}

We'll start here with the very simple gravity model. This model is built on the notion that the "mass" of population at different origins and destinations creates attractive forces between them that is further influenced by the space and travel costs between them. This model takes many forms but the simplest can be written as:

$$x_{ij} = cv_iv_jf(d_{ij})$$

where

* $x_{ij}$ is the number or strength of connection between nodes $i$ and $j$
* $c$ is a proportionality constant (gravitational constant) which balances the units of the formula. For our purposes we can largely ignore this value as it changes all absolute values but not relative values between nodes.
* $v_i$ and $v_j$ are attributes of nodes $i$ and $j$ contributing to their "mass" (attractiveness). This could be population or resource catchment or area or anything other factor that likely influences the attractiveness of the node in terms of the interaction that is the focus of the network.
* $f(d_{ij})$ is the cost or "deterrence" function defining the travel costs between $i$ and $j$. Frequently, this cost function is further specified using inverse power law or exponential decay functions defined respectively by the equations below:

$$f(d_{ij}) = \frac{1}{(1+\beta d{ij})^\gamma} \text{  and  } f(d_{ij}) = \frac{1}{e^{\beta d{ij}}}$$

where $\beta$ is a scaling factor for both models sand $\gamma$ determines the weight of the tail in the power law distribution. 

There are numerous different configurations of the simple gravity model in the literature. Some versions add exponents to $V_i$ or $V_j$ to vary the importance of inflow and outflow independently, other versions use different derivations of deterrence, and many define inflows and outflows using different sources of empirical information with additional terms to scale the units. Calculating models like these is relatively easy but determining how to parameterize such models is typically the hard part as we discuss further below. 

In order to demonstrate simple gravity models, we're going to use a regional data set of Late Intermediate periods Wankarani sites in the Bolivian Altiplano provided online on the [Comparative Archaeology Database at the University of Pittsburgh](http://www.cadb.pitt.edu/mcandrews/index.html) (McAndrews 2005). The details of this database and all of the variables are described at the link provided. This data set has more variables than we will use here but this provides a good example to explore because it includes location and size information for all sites. 

First, let's read in the data and create a quick plot showing site locations with points scaled by site area (as a measure of potential attractiveness and outflow potential). We will then select sites dating to the Late Intermediate period for analysis. We further limit consideration to habitation sites.


```r
wankarani <- read.csv("data/Wankarani_siteinfo.csv")
wankarani <- wankarani[which(wankarani$Period == "Late Intermediate"), ]
wankarani <- wankarani[which(wankarani$Type == "habitation"), ]

library(ggmap)
library(sf)

# Convert attribute location data to sf coordinates and change
# map projection
locations_sf <-
  st_as_sf(wankarani, coords = c("Easting", "Northing"), crs = 32721)
loc_trans <- st_transform(locations_sf, crs = 4326)
coord1 <- do.call(rbind, st_geometry(loc_trans)) %>%
  tibble::as_tibble() %>%
  setNames(c("lon", "lat"))

xy <- as.data.frame(coord1)
colnames(xy) <- c("x", "y")

# Get basemap "terrain-background" data for map in black and white
# the bbox argument is used to specify the corners of the box to be
# used and zoom determines the detail.
base_bolivia <- get_stamenmap(
  bbox = c(-55.97, -17.92, -55.40, -17.70),
  zoom = 10,
  maptype = "terrain-background",
  color = "bw"
)

# Plot original data on map
ggmap(base_bolivia, darken = 0.35) +
  geom_point(
    data = xy,
    aes(x, y, size = wankarani$Area),
    color = "red",
    alpha = 0.8,
    show.legend = FALSE
  ) +
  theme_void()
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-27-1.png" width="672" />

As this plot shows, there is one site that is considerably larger than the others and a few other clusters of large sites in the eastern portion of the study area. 

Now we're going to build a small function called `grav_mod` that includes 3 arguments:

* **`attract`** which is the measure of settlement attractiveness in the network. In our example here we will use area as a measure of attractiveness for both the sending and receiving site, though this can be varied.
* **`B`** which is our $\beta$ parameter for the exponential decay cost function outlined above.
* **`d`** which is a matrix of distances among our nodes. Note that this could be something other than physical distance like travel time as well. It helps to have this in units that don't result in very large numbers to keep the output manageable (though the actual absolute numbers don't matter)


```r
grav_mod <- function(attract, B, d) {
  res <- matrix(0, length(attract), length(attract))
  
  for (i in seq_len(length(attract))) {
    for (j in seq_len(length(attract))) {
      res[i, j] <-
        attract[i] * attract[j] * exp(-B * d[i,j])
    }
  }
  diag(res) <- 0
  return(res)
}
```

Now let's try an example using the Wankarani data. For this first example we will set `B` to `1`. We'll take a look at the results by first, creating a heatmap of the gravity model for every pair of nodes using the `superheat` package. We will then plot the site size against the estimated flow from our model with both axes transformed to base-10 logarithms to see how those variables relate. We use the packages `scales` to provide exponential notation for the axis labels.


```r
# First calculate a distance matrix. We divide by
# 1000 so results are in kilometers.
d_mat <- as.matrix(dist(wankarani[, 5:6])) / 1000

test1 <-
  grav_mod(attract = wankarani$Area,
           B = 1,
           d = d_mat)

library(superheat)
superheat(test1)
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-29-1.png" width="672" />

```r

library(scales)
df <- data.frame(Flow = rowSums(test1), Area = wankarani$Area)

ggplot(data = df) +
  geom_point(aes(x = Area, y = Flow)) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  theme_bw()
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-29-2.png" width="672" />

As these plots illustrate, there are clusters in the estimated flows among nodes, which is not surprising given that the sites themselves form a few clusters. Further, the plot comparing area to flow shows that there is a roughly positive linear relationship between the two but there is also variation with nodes with more or less flow than would be expected based on distance alone.

Next, let's plot a network showing all connections scaled and colored based on their strength in terms of estimated flow and with nodes scaled based on weighted degree (the total volume of flow incident on that node). The blue colored ties are weaker and the yellow colored ties are stronger. For the sake of visual clarity we omit the 25% weakest edges.


```r
library(statnet)
library(igraph)
library(ggraph)

sel_edges <- event2dichot(test1, method = "quantile", thresh = 0.25)
test1_plot <- test1 * sel_edges

net <-
  graph_from_adjacency_matrix(test1_plot, mode = "undirected",
                              weighted = TRUE)

# Extract edgelist from network object
edgelist <- get.edgelist(net)

# Create dataframe of beginning and ending points of edges
edges <- data.frame(xy[edgelist[, 1], ], xy[edgelist[, 2], ])
colnames(edges) <- c("X1", "Y1", "X2", "Y2")

# Calculate weighted degree
dg_grav <- rowSums(test1) / 10000

# Plot data on map
ggmap(base_bolivia, darken = 0.35) +
  geom_segment(
    data = edges,
    aes(
      x = X1,
      y = Y1,
      xend = X2,
      yend = Y2,
      col = log(E(net)$weight),
      alpha = log(E(net)$weight)),
      show.legend = FALSE
    ) +
  scale_alpha_continuous(range = c(0, 0.5)) +
  scale_color_viridis() +
  geom_point(
    data = xy,
    aes(x, y, size = dg_grav),
    alpha = 0.8,
    color = "red",
    show.legend = FALSE
  ) +
  theme_void() 
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-30-1.png" width="672" />

As this plot shows, there are areas characterized by higher and lower flow throughout this study area. The largest site in the study area (shown in the first map above) is characterized by a high weighted degree but there are other smaller sites that also have high weighted degree, especialy in the eastern half of the study area.

Let's now take a look at the same data again, this time we set `B` or $\beta$ to `0.1`.


```r
# First calculate a distance matrix. We divide by
# 1000 so results are in kilometers.
d_mat <- as.matrix(dist(wankarani[, 5:6])) / 1000

test2 <-
  grav_mod(
    attract = wankarani$Area,
    B = 0.1,
    d = d_mat
  )

superheat(test2)
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-31-1.png" width="672" />

```r

df <- data.frame(Flow = rowSums(test2), Area = wankarani$Area)

ggplot(data = df) +
  geom_point(aes(x = Area, y = Flow)) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  theme_bw()
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-31-2.png" width="672" />


```r
sel_edges <- event2dichot(test2, method = "quantile", thresh = 0.25)
test2_plot <- test2 * sel_edges

net2 <-
  graph_from_adjacency_matrix(test2_plot, mode = "undirected",
                              weighted = TRUE)

# Extract edgelist from network object
edgelist <- get.edgelist(net2)

# Create dataframe of beginning and ending points of edges
edges <- data.frame(xy[edgelist[, 1], ], xy[edgelist[, 2], ])
colnames(edges) <- c("X1", "Y1", "X2", "Y2")

# Calculate weighted degree
dg <- rowSums(test2) / 10000

# Plot data on map
ggmap(base_bolivia, darken = 0.35) +
  geom_segment(
    data = edges,
    aes(
      x = X1,
      y = Y1,
      xend = X2,
      yend = Y2,
      col = log(E(net2)$weight),
      alpha = log(E(net2)$weight)),
      show.legend = FALSE
    ) +
  scale_alpha_continuous(range = c(0, 0.5)) +
  scale_color_viridis() +
  geom_point(
    data = xy,
    aes(x, y, size = dg),
    alpha = 0.8,
    color = "red",
    show.legend = FALSE
  ) +
  theme_void() 
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-32-1.png" width="672" />

When we lower the `B` $\beta$ parameter we get a stronger linear relationship between site area and flow. Further, when we look at the network, we see a more even degree distribution (though there are still nodes with higher degree) and more distributed edge weights across the network, though the high values are still. concentrated in the eastern cluster. 

#### Parameterizing the Gravity Model{#ParameterizeGravity}

The simple model above uses just a single parameter $\beta$ and is largely based on empirical information on the distance among settlements and their sizes. The basic assumption here is that larger sizes "attract" more flow and also have more flow to provide to other sites. Distance is also important and the $\beta$ parameter determines the decay rate of distance as the plot below illustrates. 


```r
brk <- seq(0.01, 1, by = 0.01)
out <- as.data.frame(matrix(0, length(brk), 6))
out[, 1] <- brk

for (i in seq_len(length(brk))) {
  out[i, 2] <- exp(-brk[i])
  out[i, 3] <- exp(-brk[i] * 2)
  out[i, 4] <- exp(-brk[i] * 5)
  out[i, 5] <- exp(-brk[i] * 10)
  out[i, 6] <- exp(-brk[i] * 20)
}

colnames(out) <- c("beta", "D=1", "D=2", "D=5", "D=10", "D=20")

library(reshape2)
df <- melt(out[, 2:6])
df$beta <- rep(brk, 5)

ggplot(data = df) +
  geom_line(aes(x = beta, y = value, color = variable)) +
  ylab("Decay") +
  xlab(expression( ~ beta)) +
  theme_bw()
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-33-1.png" width="672" />

As we see in the plot above, the decay rate varies at different values of $\beta$ and also in relation to the distance between points. How then, do we select the appropriate $\beta$ in a model like this? There are a few ways to address this question depending on data availability and the nature of the issue such networks are being used to address. First, do we have some independent measure of network flow among our sites? For example, perhaps we could take information on the number or diversity of trade wares recovered at each site. We might expect sites with greater flow to have higher numbers or more diverse trade ware assemblages. We could evaluate this proposition using regression models and determine which $\beta$ provides the best fit for the data and theory. Often, however, when we are working with simple gravity models we have somewhat limited data and cannot make such direct comparisons. It is possible that we have a theoretical expectation for the shape of the decay curve as shown above (note that distances could also be things like cost-distance here so perhaps we have a notion of maximal travel times or a caloric budget for movement) and we can certainly factor that into our model. As we will see below, however, there are alternatives to the simple gravity model that provide additional avenues for evaluating model fit.

### The Rihll and Wilson "Retail" Model{#RihllWilson}

One of the most popular extensions of the gravity model in archaeology was published by Rihll and Wilson in 1987 for their study of Greek city states in the 9th through 8th centuries B.C. They used a spatial interaction model that is sometimes called the "retail" model. This approach was originally designed for assessing the likely flows of resources into retail shops and how that can lead to shop growth/increased income and the establishment of a small number of super-centers that receive a large share of the overall available flow (often at the expense of smaller shops). When thinking about this model in a settlement context, the "flows" are people and resources and the growth of highly central nodes in the network is used to approximate the development of settlement hierarchy and the growth of large settlement centers. 

One of the big advantages of this model is that it only requires the locations of origins and destinations and some measure of the cost of movement among sites. From that, an iterative approach is used to model the growth or decline of nodes based on their configurations in cost space and a couple of user provided parameters. Versions of this model have been used in a number of archaeological studies (e.g., [Bevan and Wilson 2013](https://www.researchgate.net/publication/257154745_Models_of_settlement_hierarchy_based_on_partial_evidence); [Evans and Rivers 2016](https://arxiv.org/abs/1611.07839); [Filet 2017](https://www.frontiersin.org/articles/10.3389/fdigh.2016.00010/full); [Rihll and Wilson 1987](https://www.persee.fr/doc/hism_0982-1783_1987_num_2_1_1300)). Here we present a version of the model inspired by the recent work by these scholars. The code below was based in part on an [R function](https://rdrr.io/github/CRC1266-A2/moin/src/R/Rhill_Wilson_algebraic.R) created as part of an [ISAAKiel](https://isaakiel.github.io/) Summer School program.

Let's first formally describe the Rihll and Wilson model. The model of interaction $T_{ij}$ among a set of sites $k$ can be represented as:

$$T_{ij} = \frac{O_iW_j^\alpha e^{-\beta c_{ij}}}{\Sigma_k W_k^\alpha e^{-\beta c_{jk}}}$$

where

* $T_{ij}$ is the matrix of flows or interaction between nodes $i$ and $j$ (which may or may not be the same set)
* $O_i$ is the estimated weight of flow or interaction out of the origins $i$
* $W_j$ is the estimated weight of flow or interaction into the destinations $j$. In most archaeological applications, this is used to represent some measurement of settlement size.
* $\alpha$ is a parameter which defines the importance of resource flow into destinations. Numbers greater than 1 essentially model increasing returns to scale for every unit of flow.
* $e^{-\beta c_{ij}}$ is the "deterrence" function where $e$ is an exponential ($exp(-\beta c_{ij})$), $c_ij$ is a measure of the cost of travel between nodes $i$ and $j$ and $\beta$ is the decay parameter that describes the rate of decay in interaction at increasing distance.
* $\Sigma_k W_k^\alpha e^{-\beta c_{jk}}$ is the sum for all nodes $k$ of the expression using the $W_j$, $\alpha$, and deterrence function terms as described above.

In the case here (and in many archaeological examples) we start with a simple naive assumption of flow by setting all values of $O$ and $W$ equal to 1. Our goal, indeed, is to estimate $W$ for all nodes iteratively. In order to do this after each calculation of $T_{ij}$ we calculate two new values $D_j$ and $\Delta W_j$ defined as:

$$\begin{aligned} D_j & = \Sigma_i T_{ij}\\
\\
\Delta W_j & = \epsilon(D_j - KW_j) \end{aligned}$$

where

* $D_j$ is a vector of values defined as the sum weights incident on a given node (column sums of $T_{ij}$)
* $\Delta W_j$ is the change in values of $W$ (or estimated settlement size)
* $\epsilon$ is a control parameter that determines how quickly $W$ can change at each iterative step.
* $K$ is a factor that is used to convert size $W$ to the sum of flows $D_j$

For the purposes of our examples here, we set $K$ to 1 to assume that sum of flows and size are in equal units and we set $\epsilon$ to 0.01 so that the model does not converge too rapidly. 

By way of example, we will use the original Greek city states data used by Rihll and Wilson (1987) and [put online](https://figshare.com/articles/dataset/Locations_of_108_Archaeic_Greek_settlements_used_in_Rihll_and_Wilson_1987_/868961) by Tim Evans based on his own work using these data and related spatial interaction models. 

Let's first map the Greek city states data.


```r
library(sf)
library(ggmap)

dat <- read.csv(file = "data/Rihll_Wilson.csv")
locs <-
  st_as_sf(dat,
           coords = c("Longitude_E", "Latitude_N"),
           crs = 4326)

library(ggmap)
map <- get_stamenmap(
  bbox = c(22.45, 37.45, 24.1, 38.6),
  maptype = "terrain-background",
  zoom = 10
)

ggmap(map) +
  geom_point(data = dat, aes(x = Longitude_E, y = Latitude_N)) +
  theme_void()
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-34-1.png" width="672" />

In the code below, we define a vector for the initial values of $O_i$ and $W_j$ setting the value to 1 for every site. We then define an initial $\alpha = 1.05$ (to suggest that flows in provide slight increasing returns) and $\beta = 0.1$ (which is a low decay rate such that long distance connections will retain some importance). The code then iteratively calculates values of $T_{ij}$ until the sum of weights stops changing by more than some very low threshold for a number of time steps. In the chunk of code below we calculate $T_{ij}$ and then plot histogram of $W_j$ in final state of model.


```r
# Set model parameters and initial variable states
Oi <- rep(1, nrow(dat))
Wj <- rep(1, nrow(dat))
alpha <- 1.05
beta <- 0.1
eps <- 0.01
K <- 1

# Define distance among points in kilometers. Because our
# points are in geographic coordinates we use the distm
# function. See the section on Spatial Networks for more.
library(geosphere)
d <- as.matrix(distm(dat[, c(7, 6)])) / 1000

# Dj is ineitial set as a vector of 1s like Wj
Dj <- Wj

# Create objects for keeping track of the number
# of iterations and the conditions required to end
# the loop.
end_condition <- 1
iter <- 0

# Define the deterrence function as a exponential
det <- exp(-beta * d)

# Create while loop that will continue to iterate Tij
# until 10,000 iterations or until the end_condition
# object is less than the threshold indicated.
while (!(end_condition < 1e-5) & iter < 10000) {
  # Set Wj to Dj
  Wj <- Dj
  # Calculate Tij as indicated above
  Tij <-
    apply(det * Oi %o% Wj ^ alpha, 2, '/',
          (Wj ^ alpha %*% det))
  # Calculate change in W using equation above
  delta_W <- eps * (colSums(Tij) - (K * Wj))
  
  # Calculate new Dj
  Dj <- delta_W + Wj
  
  # Add to iterator and check for end conditions
  iter <- iter  + 1
  end_condition <- sum((Dj - Wj) ^ 2)
}

hist(Wj, breaks = 15)
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-35-1.png" width="672" />

As this shows, the Rihll and Wilson model generates flow weights with a heavy tailed distribution with these parameters. This means that a small number of nodes are receiving lots of flow but most are receiving very little. 

In order to look at this geographically, Rihll and Wilson defined what they call "Terminal Sites". Terminal sites in the network are nodes where the total flow of inputs into the site $Wj$ is bigger than the largest flow out of the site. Let's define our terminal sites for our model run above and then examine them.
 

```r
terminal_sites <- NULL
for (i in 1:109) terminal_sites[i] <- sum(Tij[-i, i]) > max(Tij[i, ])

knitr::kable(dat[terminal_sites,])
```



|    | SiteID| ShortName|Name        |      XPos|      YPos| Latitude_N| Longitude_E|
|:---|------:|---------:|:-----------|---------:|---------:|----------:|-----------:|
|14  |     13|        14|Aulis       | 0.6366255| 0.2547325|   38.40000|    23.60000|
|18  |     17|        18|Onchestos   | 0.3962963| 0.2967078|   38.37327|    23.15027|
|22  |     21|        22|Alalkomenai | 0.3098765| 0.2860082|   38.41000|    22.98600|
|25  |     24|        25|Thebes      | 0.4876543| 0.3197531|   38.32872|    23.32191|
|45  |     44|        45|Megara      | 0.5098765| 0.5197531|   38.00000|    23.33333|
|50  |     49|        50|Menidi      | 0.7255144| 0.4761317|   38.08333|    23.73333|
|56  |     55|        56|Markopoulo  | 0.8415638| 0.5740741|   37.88333|    23.93333|
|70  |     69|        70|Athens      | 0.7164609| 0.5320988|   37.97153|    23.72573|
|78  |     77|        78|Kromna      | 0.3098765| 0.5823045|   37.90479|    22.94820|
|80  |     79|        80|Lekhaion    | 0.2777778| 0.5666667|   37.93133|    22.89293|
|97  |     96|        97|Prosymnia   | 0.2119342| 0.7074074|   37.70555|    22.76298|
|101 |    100|       101|Argos       | 0.1930041| 0.7444444|   37.63092|    22.71955|
|103 |    102|       103|Magoula     | 0.1773663| 0.7740741|   37.59310|    22.70769|
|104 |    103|       104|Tiryns      | 0.2349794| 0.7748971|   37.59952|    22.79959|

Interesting, our terminal sites include many historically important and larger centers (such as Athens, Thebes, and Megara), despite the fact that we included no information about size in our model.

Now let's map them. Points are scaled based on the weight of inflow and terminal sites are colored in blue.


```r
ggmap(map) +
  geom_point(
    data = dat,
    aes(
      x = Longitude_E,
      y = Latitude_N,
      size = Wj,
      color = terminal_sites
    ),
    show.legend = FALSE
  ) +
  theme_void()
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-37-1.png" width="672" />

As this map illustrates, the terminal sites are roughly evenly distributed across the study area rather than clustered together. Now, let's see what happens when we vary our parameters $\alpha$ and $\beta$.

For the next map, we will set $alpha = 1.15$ and leave $\beta$ as is. To make it easier to calculate everything, we're going to call an R script using `source()` that includes the full function and outputs $W_j$, $T_{ij}$, the number of iterations, and a logical vector indicating which nodes are terminals.



```r
source("scripts/rihll_wilson.R")

rw2 <- rihll_wilson(alpha = 1.15, beta = 0.1, dist_mat = d)

ggmap(map) +
  geom_point(
    data = dat,
    aes(
      x = Longitude_E,
      y = Latitude_N,
      size = rw2$Wj,
      color = rw2$terminals
    ),
    show.legend = FALSE
  ) +
  theme_void()
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-38-1.png" width="672" />

```r

knitr::kable(dat[rw2$terminals,])
```



|   | SiteID| ShortName|Name       |      XPos|      YPos| Latitude_N| Longitude_E|
|:--|------:|---------:|:----------|---------:|---------:|----------:|-----------:|
|14 |     13|        14|Aulis      | 0.6366255| 0.2547325|   38.40000|    23.60000|
|18 |     17|        18|Onchestos  | 0.3962963| 0.2967078|   38.37327|    23.15027|
|45 |     44|        45|Megara     | 0.5098765| 0.5197531|   38.00000|    23.33333|
|56 |     55|        56|Markopoulo | 0.8415638| 0.5740741|   37.88333|    23.93333|
|70 |     69|        70|Athens     | 0.7164609| 0.5320988|   37.97153|    23.72573|
|78 |     77|        78|Kromna     | 0.3098765| 0.5823045|   37.90479|    22.94820|
|97 |     96|        97|Prosymnia  | 0.2119342| 0.7074074|   37.70555|    22.76298|

Increasing $\alpha$ increases the importance of inflow so we end up with fewer terminal sites. Notably, we are still retaining many large and historically important cities despite not including information on site size in our model.

Now let's set $\alpha = 1.05$ and increase $\beta = 0.35$:


```r
rw3 <- rihll_wilson(alpha = 1.05, beta = 0.35, dist_mat = d)

ggmap(map) +
  geom_point(
    data = dat,
    aes(
      x = Longitude_E,
      y = Latitude_N,
      size = rw3$Wj,
      color = rw3$terminals
    ),
    show.legend = FALSE
  ) +
  theme_void()
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-39-1.png" width="672" />

```r

knitr::kable(dat[rw3$terminals,])
```



|    | SiteID| ShortName|Name           |      XPos|      YPos| Latitude_N| Longitude_E|
|:---|------:|---------:|:--------------|---------:|---------:|----------:|-----------:|
|4   |      3|         4|Ay.Marina      | 0.4267490| 0.2201646|   38.48208|    23.20793|
|50  |     49|        50|Menidi         | 0.7255144| 0.4761317|   38.08333|    23.73333|
|68  |     67|        68|Phaleron       | 0.6958848| 0.5650206|   37.93731|    23.70625|
|71  |     70|        71|Kallithea      | 0.7074074| 0.5419753|   37.95589|    23.70209|
|78  |     77|        78|Kromna         | 0.3098765| 0.5823045|   37.90479|    22.94820|
|80  |     79|        80|Lekhaion       | 0.2777778| 0.5666667|   37.93133|    22.89293|
|88  |     87|        88|Kleonai        | 0.2168724| 0.6341564|   37.81142|    22.77429|
|90  |     89|        90|Zygouries      | 0.2316872| 0.6489712|   37.80300|    22.78015|
|95  |     94|        95|Mykenai        | 0.2069959| 0.6884774|   37.73079|    22.75638|
|97  |     96|        97|Prosymnia      | 0.2119342| 0.7074074|   37.70555|    22.76298|
|98  |     97|        98|Argive_Heraion | 0.2185185| 0.7172840|   37.69194|    22.77472|
|100 |     99|       100|Pronaia        | 0.2333333| 0.7880658|   37.57682|    22.79954|
|102 |    101|       102|Kephalari      | 0.1609054| 0.7666667|   37.59736|    22.69123|
|103 |    102|       103|Magoula        | 0.1773663| 0.7740741|   37.59310|    22.70769|
|104 |    103|       104|Tiryns         | 0.2349794| 0.7748971|   37.59952|    22.79959|
|105 |    104|       105|Prof.Elias     | 0.2514403| 0.7683128|   37.60818|    22.81920|
|106 |    105|       106|Nauplia        | 0.2292181| 0.7946502|   37.56812|    22.80871|

As this map illustrates, increasing $\beta$ increases the distance decay meaning that local interactions are more important leading to a more even distribution of $W_j$ values and more terminal sites (which are further somewhat spatially clustered). 

#### Parameterizing the Retail Model{#ParameterizingRetail}

How might we select appropriate values for $\alpha$ and $\beta$ in our model? The approach Rihll and Wilson and many subsequent researchers have taken (see Bevan and Wilson 2013; Filet 2017) is to use our knowledge of the archaeological record and regional settlement patterns and to select a model that is most consistent with that knowledge. Our model creates more or fewer highly central nodes depending on how we set our parameters. As we noted above, the terminal nodes we defined consistently include historically important and large sites like Athens suggesting that our model is likely doing something reasonable. One potential approach would be to run comparisons for a plausible range of values for $\alpha$ and $\beta$ and to evaluate relationships with our own archaeological knowledge of settlement hierarchy and which sites/places are defined as terminal sites or highly central places in our model. 

In order to test a broader range of parameter values and their impact on the number of terminal sites, we have created a function that takes the parameters, data, and distance matrix and outputs just the number of terminals. 

Let's run this for a range of plausible parameter values:

<div class="rmdwarning">
<p>If you attempt to run this on your own note that it takes quite a
while to complete.</p>
</div>



```r
source("scripts/terminals_by_par.R")

alpha_ls <- seq(0.90, 1.25, by = 0.01)
beta_ls <- seq(0.05, 0.40, by = 0.01)

res <- matrix(NA, length(alpha_ls), length(beta_ls))
row.names(res) <- alpha_ls
colnames(res) <- beta_ls

for (i in seq_len(length(alpha_ls))) {
  for (j in seq_len(length(beta_ls))) {
    res[i, j] <-
      terminals_by_par(
        alpha = alpha_ls[i],
        beta = beta_ls[j],
        dist_mat = d
      )
  }
}
```

In case you want to see the data but don't want to wait for the above chunk to run, we have created an Rdata object with the output. Let's load those data and plot them as a heatmap/tile plot:


```r
load(file = "data/retail_pars.Rdata")

library(reshape2)
library(ggraph)
#> Registered S3 method overwritten by 'ggforce':
#>   method           from 
#>   scale_type.units units
res_df <- melt(res)

ggplot(data = res_df) +
  geom_tile(aes(x = Var2, y = Var1, fill = value)) +
  scale_fill_viridis(option = "turbo") +
  xlab(expression( ~ beta)) +
  ylab(expression( ~ alpha)) +
  theme_bw()
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-42-1.png" width="672" />

As this plot shows low values for both $\alpha$ and $\beta$ tend to generate networks with lots of terminals but the relationship between these parameters is not linear. Based on this and our knowledge of the archaeological record, we could make an argument for evaluating a particular combination of parameters but there is certainly no single way to make that decision. To see further expansions of such an approach that attempts to deal with incomplete survey data and other kinds of considerations of settlement prominence, see the published example by [Bevan and Wilson (2013)](https://www.researchgate.net/publication/257154745_Models_of_settlement_hierarchy_based_on_partial_evidence).

### Truncated Power Functions{#TruncatedPower}

Another similar spatial interaction model was used in a study by [Menze and Ur (2012)](https://dash.harvard.edu/handle/1/8523994) in their exploration of networks in northern Mesopotamia. Their model is quite similar to the simple gravity model we saw above but with a couple of additional parameters and constraints. We leave the details of the approach to the published article but briefly describe their model here. This truncated power function requires information on settlement location, some measure of size or "attraction," and three parameter values. Edge interaction $E_{ij}$ in this model can be formally defined as:

$$E_{ij}(\alpha,\beta,\gamma) = V_i^\gamma V_j^\gamma d_{ij}^{-\alpha} e^{(-d_{ij}/\beta)}$$

where 

* $V$ is some measure of the attractiveness of node $i$ or $j$, typically defined in terms of settlement size.
* $d_{ij}$ is the distance between nodes $i$ and $j$. Again, this can use measures of distance other than simple Euclidean distances.
* $\alpha$ is a constraint on the distance between nodes.
* $\beta$ is the physical distance across which distance decay should be considered (defined in units of $d$).
* $\gamma$ is used to define the importance of $V$ on interaction where values above 1 suggest incresing returns on scale.

The model output $E_{ij}$ is, according to Menze and Ur, meant to approximate the movement of people among nodes across the landscape. In order to evaluate this function, we replicate the results of the Menze and Ur paper with one small change. We drop the bottom 50% smallest sites from consideration due to the large sample size to keep run times manageable (but this could certainly be changed in the code below). We use the replication data set provided by Menze and Ur online [here](https://dataverse.harvard.edu/dataset.xhtml;jsessionid=adeb8ff43c833f1efe447dc9e8ba?persistentId=hdl%3A1902.1%2F17731&version=&q=&fileTypeGroupFacet=%22Text%22&fileAccess=&fileTag=&fileSortField=type&fileSortOrder=). 

Let's read in the data, omit the rows without site volume estimates, and then remove the lowest 50% of sites in terms of volume values. We then plot the sites with points scaled by site volume.


```r
mesop <- read.csv("data/menze_ur_sites.csv")
mesop <- mesop[-which(is.na(mesop$volume)),]
mesop <- mesop[which(mesop$volume > quantile(mesop$volume, 0.5)),]

ggplot(mesop) +
  geom_point(aes(x = x, y = y, size = volume),
             show.legend = FALSE) +
  scale_size_continuous(range = c(1, 3)) +
  theme_void()
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-43-1.png" width="672" />

And here we implement the truncated power approach rolled into a function called `truncated_power`. We use the values selected as optimal for the Menze and Ur (2012) paper.

<div class="rmdwarning">
<p>Note that the block below takes several minutes to run.</p>
</div>



```r
d <- as.matrix(dist(mesop[, 1:2])) / 1000

truncated_power <- function (V, d, a, y, B) {
  temp <- matrix(0, nrow(d), ncol(d))
  for (i in seq_len(nrow(d))) {
    for (j in seq_len(ncol(d))) {
      temp[i, j] <- V[i] ^ y * V[j] ^ y * d[i, j] ^ -a * exp(-d[i, j] / B)
      if (temp[i, j] == Inf) {
        temp[i, j] <- 0
      }
    }
  }
  return(temp)
}

res_mat <- truncated_power(V = mesop$volume, d = d, a = 1, y = 1, B = 4)
```

We can now plot the sites again, this time with points scaled based on the total volume of flow incident on each node.


```r

edge_flow <- rowSums(res_mat)

ggplot(mesop) +
  geom_point(aes(
    x = x,
    y = y,
    size = edge_flow,
    alpha = edge_flow
  ),
  show.legend = FALSE) +
  scale_size_continuous(range = c(1, 3)) +
  theme_void()
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-46-1.png" width="672" />

If we compare the plot above to figure 8 in Menze and Ur (2012) we see highly central sites in the same locations suggesting that we've reasonably approximated their results even though we are using a slightly different sample. Further, as the next plot shows, if we remove the few sites that are isolated in our sample (due to us removing the bottom 50% of sites) we also see the same strong linear correlation between the log of site volume and the log of our measure of interaction.


```r
rem_low <- which(edge_flow > quantile(edge_flow, 0.01))

library(ggplot2)
library(scales)
df <- data.frame(Volume = mesop$volume[rem_low], Interaction = edge_flow[rem_low])

ggplot(data = df) +
  geom_point(aes(x = Volume, y = Interaction)) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  theme_bw()
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-47-1.png" width="480" />

We could go about parameterizing this truncated power function in much the same way that we saw with the models above (i.e., testing values and evaluating results against the archaeological pattern). Indeed that is what Menze and Ur do but with a slight twist on what we've seen so far. In their case, they are lucky enough to have remotely sensed data on actual trails among sites for some portion of their study area. How they selected model parameters is by testing a range of values for all parameters and selecting the set that produced the closest match between site to site network edges and the orientations of actual observed trails (there are some methodological details I'm glossing over here so refer to the article for more). As this illustrates, there are many potential ways to select model parameters based on empirical information.

### Radiation Models{#RadiationModels}

In 2012 Filippo Simini and colleagues ([Simini et al. 2012](https://dspace.mit.edu/handle/1721.1/77896)) presented a new model, designed specifically to model human geographic mobility called the radiation model. This model was created explicitly as an alternative to various gravity models and, in certain cases, was demonstrated to generate improved empirical predictions of human population movement between origins and destinations. This model shares some basic features with gravity models but importantly, the approach includes no parameters at all. Instead, this model uses simply measures of population at a set of sites and the distances between them. That is all that is required so this model is relatively simple and could likely be applied in many archaeological cases. Let's take a look at how it is formally defined: 

$$T_{ij} = T_i \frac{m_in_j}{(m_i + s_{ij})(m_i + n_j + s_{ij})}$$

where

* $T_i$ is the total number of "commuters" or migrating individuals from node $i$.
* $m_i$ and $n_j$ are the population estimates of nodes $i$ and $j$ respectively.
* $s_{ij}$ is the total population in a circle centered at node $i$ and touching node $j$ excluding the populations of both $i$ and $j$.

We have defined a function for calculating radiation among a set of sites using just two inputs:

* **`pop`** is a vector of population values.
* **`d_mat`** is a distance matrix among all nodes.

A script containing this function can also be downloaded [here](scripts/radiation.R)


```r
radiation <- function(pop, d_mat) {
  ## create square matrix with rows and columns for every site
  out <-
    matrix(0, length(pop), length(pop))
  for (i in seq_len(length(pop))) {
    # start loop on rows
    for (j in seq_len(length(pop))) {
      # start loop on columns
      if (i == j)
        next()
      # skip diagonal of matrix
      m <- pop[i] # set population value for site i
      n <- pop[j] # set population value for site j
      # find radius as distance between sites i and j
      r_ij <-
        d_mat[i, j]
      # find all sites within the distance from i to j centered on i
      sel_circle <-
        which(d_mat[i, ] <= r_ij)
      # remove the site i and j from list
      sel_circle <-
        sel_circle[-which(sel_circle %in% c(i, j))]
      s <- sum(pop[sel_circle]) # sum population within radius
      # calculate T_i and output to matrix
      temp <-
        pop[i] * ((m * n) / ((m + s) * (m + n + s)))
      if (is.na(temp)) temp <- 0
      out[i, j] <- temp
    }
  }
  return(out)
}
```

In order to test this model, we will use the same Wankarani settlement data we used above for the simple gravity model. We will use site area divided by 500 as our proxy for population here. Again we limit our sample to Late Intermediate period sites and habitations.



```r
wankarani <- read.csv("data/Wankarani_siteinfo.csv")
wankarani <- wankarani[which(wankarani$Period == "Late Intermediate"), ]
wankarani <- wankarani[which(wankarani$Type == "habitation"), ]

d <- as.matrix(dist(wankarani[, 5:6]))

rad_test <- radiation(pop = wankarani$Area / 500, d_mat = d)
```

Now let's plot the resulting network with each node scaled by the total incident flow (row sums of the output of the function above). We plot network edges with weights indicated by color (blue indicates low weight and yellow indicates high weight).


```r
library(igraph)
library(ggraph)
library(sf)
library(ggmap)

locations_sf <-
  st_as_sf(wankarani, coords = c("Easting", "Northing"), crs = 32721)
loc_trans <- st_transform(locations_sf, crs = 4326)
coord1 <- do.call(rbind, st_geometry(loc_trans)) %>%
  tibble::as_tibble() %>%
  setNames(c("lon", "lat"))

xy <- as.data.frame(coord1)
colnames(xy) <- c("x", "y")

base_bolivia <- get_stamenmap(
  bbox = c(-55.97, -17.92, -55.40, -17.70),
  zoom = 10,
  maptype = "terrain-background",
  color = "bw"
)

net <-
  graph_from_adjacency_matrix(rad_test, mode = "undirected",
                              weighted = TRUE)

# Extract edgelist from network object
edgelist <- get.edgelist(net)

# Create dataframe of beginning and ending points of edges
edges <-
  data.frame(xy[edgelist[, 1], ],
             xy[edgelist[, 2], ])
colnames(edges) <- c("X1", "Y1", "X2", "Y2")

# Calculate weighted degree
dg <- rowSums(rad_test)

# Plot data on map
ggmap(base_bolivia, darken = 0.35) +
  geom_segment(
    data = edges,
    aes(
      x = X1,
      y = Y1,
      xend = X2,
      yend = Y2,
      col = log(E(net)$weight),
      alpha = log(E(net)$weight)
    ),
    show.legend = FALSE
  ) +
  scale_alpha_continuous(range = c(0, 0.5)) +
  scale_color_viridis() +
  geom_point(
    data = xy,
    aes(x, y, size = dg),
    alpha = 0.8,
    color = "red",
    show.legend = FALSE
  ) +
  theme_void() 
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-50-1.png" width="672" />

This map shows clusters of higher and lower edge weights and again variation in total weighted degree (with higher values in the east). The results are similar, but not identical to the output of the simple gravity model.


```r
dg_grav <-  rowSums(grav_mod(
  attract = wankarani$Area / 1000,
  B = 1,
  d = d_mat
))

dg_rad <- rowSums(rad_test)

library(ggplot2)
library(scales)
df <-
  data.frame(Radiation = dg_rad, Gravity = dg_grav)

ggplot(data = df) +
  geom_point(aes(x = Radiation, y = Gravity)) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  theme_bw()
```

<img src="07-beyond-the-book_files/figure-html/unnamed-chunk-51-1.png" width="672" />

We are aware of few published examples of the use of radiation models for archaeological cases, but there is certainly potential (see Evans 2016).

### Other Spatial Interaction Models{#OtherModels}

There are many other spatial interaction models we haven't covered here. Most are fairly similar in that they take information on site size, perhaps other relevant archaeological information, and a few user selected parameters to model flows across edges and sometimes to iteratively predict sizes of nodes, the weights of flows, or both. Other common models we haven't covered here include the XTENT model (Renfrew and Level 1979; see Ducke and Suchowska 2021 for an example with code for GRASS GIS) and various derivations of MaxEnt (or maximum entropy) models. Another approach that merits mention here is the [ariadne model](https://figshare.com/articles/dataset/ariadne/97746) designed by Tim Evans and used in collaboration with Ray Rivers, Carl Knappett, and others. This model provides a means for predicting site features and estimating optimal networks based on location and very general size information (or other archaeological features). This model has features that make it particularly useful for generating directed spatial networks (see [Evans et al. 2011](https://plato.tp.ph.ic.ac.uk/~time/networks/arch/interactionsArxivSubmissionV2.pdf)). Although there is a basic R implementation for the ariadne model developed by the ISAAKiel team [available here](https://rdrr.io/github/CRC1266-A2/moin/src/R/hamiltonian.R) the computational constraints make this function unfeasible in R for all but very small networks. Instead, if you are interested in applying the ariadne model, we suggest you use the original Java program created by Tim Evans and [available here](https://figshare.com/articles/dataset/ariadne/97746).
