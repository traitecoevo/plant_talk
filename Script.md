
NOTES

Our paper appeared in journal Methods in Ecology and Evolution

It's an "application note", meaning it's a bout a new piece of software - package called `plant` for the R platform

---

One of ways we can study demography is via simulation models, these are attempts to encode rules about how a system works.

We then ask whether these rules combine to produce emergent patterns we see in real systems.

The two big focusses for us --  and why we built plant -- was we were interested in understanding

1) what effect a collection of ecophsyiological traits, & their associated trade-offs, have on demography,

and

2) whether these trade-offs can enable species with different traits to stably coexist?

---

Plant model allows us take trait based trade-offs and look at their influence on emergent behaviours at three different scales

- individual trees
- patches of competing plants
  - in which different plants interact by shading each other
- entire metapopulations of competing plants
- or with multiple species differing in traits, a metacommunity
  - here we have series of patches linked by dispersal and subject to a disturbance regime

-----

It's at this level of metacommunity, evaluate questions about coexistence,  by estimating fitness of different types growing in competition with one another.

----

At this point I'd like to acknowledge deep history behind plant model

- plant package is latests in a long history of models that
- started with back in 1970's, wth what was then called gap models, where essential idea of simulating size- and patch-structured meta-communities appeared
- since then there's' been a whole bunch of advancements, which we've bundled into plant model

Let me comment on just a couple of these features

----

The first is way we distributing the model

- model is implemented as R package
  - now usually you wouldn't build a model like this in R, because it would be HORRENDOUSLY slow
  - solve that by having core code for running the simulation written in C++, which is one of fastest languages

BUT you get to control that ugly C++ code entirely from within R, and that is great
  - 1. because packages are relatively easy to distribute
  - 2. We code write nice code to generate data and then directly access it and plot it
    - so all you need to do simulate an metapopulation is run few lines of code

----

Second core feature is focus on fitness, and by extension, the fact that we simulate plants across entire life-cycle

- this plot shows individuals of two different species, indicated by colours, growing in a patch as it recovers from disturbance
  --  as simulated from previous slide

Wondering what patch size is, answer is there isn't one

- what we're able to do is simulate effectively an infinite number of plants, could be a very large patch or the average across many patches, by using a PDE to approximate a continuous size distribution.

----

That's important because we can then integrate over that distribution to get fitness of new types in competition with residents

- This plot shows a fitness landscape of species with different values of a leaf trait -- LMA -- competing against two species we simulated in previously slides,
-  what it shows that all trait values except for the residents have fitness less than 0, meaning this is an evolutionarily stable meta-community.

Remember - to get fitness at every point on graph, we're  integrating population dynamics across life cycle of an infinite population of plants with those traits.

-----

Final feature worth highlighting is that we've try to provide multiple ways by which you can parametrise the model:

-----

Let me take this opportunity to thank
  - Rob, Allen & others for putting together special issue
  - funding organisations
  - and collaborators, most especially Rich FitzJohn who was written most of the code and implemented some really cool features like wrapping of C++ into R
    - if you're interested in that side of things, checkout our blog post where we elaborate on technologies used
  - examples:
    - whole lot of code examples in Supp Mat of paper
    - also see code at the link provided there which reproduces all figures from the paper

-----

Questions:

1. DGVMS -- don't these already have traits, competition for light?
  - well yes and no
  - competition for light -- nearly all vegetation models have shading down through the canopy, but as far as I know, the current DGVMs, like LPJ, all have populations consist of single average sized individual. So no actual competition among individuals, not growing individuals through their life cycle.
    - an exception is the ED model
  - traits: yes --lots of traits, but as two important things missing
    1. Is your only growing average individual, not able to capture effects of traits at different points in life cycle and successional process
    2. Is trade-offs - often have differences among PFTs, but need to parametrise these in terms of a trade-off, otherwise plant at one end of trait gradient is getting something for nothing.

2. What are the differences or similarities between what's happening in plant and IPMs
  - similarities: both seek to estimate continuous size distribution, model demography across life cycle
  - one big difference I've identified: how they handle growth and variance in growth at a given size. In IPMs assume some variance in growth rates at given size, range of variance comes from observed variance.
  In plant, explictly modelling factors contributing to that variance, that's from different light envs in different patches.


3. You made a big deal about ability to write a script in R, why is this important?

