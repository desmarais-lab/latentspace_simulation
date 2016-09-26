This repository contains the code, data, and results from "Inference on the Effects of Observed Covariates in Latent Space Models for Networks" by [Zachary Jones](http://zmjones.com), [Matthew Denny](http://www.mjdenny.com/), [Bruce Desmarais](http://sites.psu.edu/desmaraisgroup/), and [Hanna Wallach](http://dirichlet.net/).

> Due to the complex interdependence found in relational data, political networks scholars draw upon an increasingly sophisticated toolkit for statistical inference. The latent space model (LSM) for network data combines a generalized linear model with a latent spatial embedding of the network. It is assumed that the latent spatial embedding can control for unmeasured confounding structure that is related to the values of edges in the network. There has been little to no research that considers the LSM's performance in adjusting for unmeasured structure. We investigate the LSM's performance via a simulation study. In the presence of an unmeasured covariate that can be modeled using a latent space, estimation and inferential error remain high under even moderate confounding. However, the prediction error of the LSM when unmeasured network structure is present is substantially lower in most cases. We conclude that the LSM is most appropriate for exploration or prediction.

This work was supported by National Science Foundation grants DGE-1144860, SES-1558661, SES-1619644, and CISE-1320219. Any opinions, findings, and conclusions or recommendations are those of the authors and do not necessarily reflect those of the sponsor.

<<<<<<< HEAD
The core of our analysis is a simulation study which is implemented with [BatchExperiments](https://cran.r-project.org/web/packages/BatchExperiments/index.html) (also see the [associated paper](https://www.jstatsoft.org/article/view/v064i11)). All code is in the `src` folder. Due to the computational expense of studying the behavior of the latent space model, this analysis was run on a Torque cluster. Thus, unless you aim to execute our code on another Torque cluster, it is necessary to modify `.BatchJobs.R`, `simulation.R` and possibly`template.tmpl`. Using `BatchJobs` (the library underlying `BatchExperiments`) it is possible to simply substitute the appropriate cluster function backend here (explanations on how to do this are contained in the documentation linked to as well as the paper mentioned).

The core of the simulation is contained in `simulation.R`. This file contains the parameter specifications of the simulation, the estimation functions, as well as the control parameters for parallelizing the simulation and submitting it to the relevant cluster backend defined in `.BatchJobs.R`. The object `resources` in `simulation.R` must be modified to match the resources of the system on which the code is executed.

`reduce.R` contains the code to take the result of `simulation.R`, which stores results in a database, and produce the performance/evaluation criteria described in the paper (prediction, inference, and estimation error). `reduce.R` is set up to execute in parallel on a cluster as well, though depending on the system on which the code executes, this may be unecessary. In that case (a sequential reduce step) the commented code can be used rather than the lines above (as described in the code).

`analysis.R` takes `results.csv` and produces the graphs in the paper.

`upload.txt` and `download.txt` specify the relevant files to upload/download from the custer system on which the simulation runs.
=======
The core of our analysis is a simulation study which is implemented with [BatchExperiments](https://cran.r-project.org/web/packages/BatchExperiments/index.html) (also see the [associated paper](https://www.jstatsoft.org/article/view/v064i11)). All code is in the `src` folder. Due to the computational expense of studying the behavior of the latent space model, this analysis was run on a Torque cluster. Thus, unless you aim to execute our code on another Torque cluster, it is necessary to modify some of our code. I have modularized it to make this easy and am happy to assist anyone interested in verifying our work (please [email](mailto:zmj@zmjones.com) with any questions).

The core of the simulation, contained in `src/simulation.R` is agnostic to what cluster (or lack thereof) you use to run the simulation. It contains the functions which generate our simulated data, as well as the functions to estimate the versions of the latent space model and generalized linear models we compare. It should be unecessary to modify any code here. This code was reviewed by Matt and implemented by Zach.

`.BatchJobs.R` contains the definition of an object `cluster.functions` which is loaded when `BatchJobs` or `BatchExperiments` is loaded, and defines what cluster backend will be used. Available cluster types are described [here](https://github.com/tudo-r/BatchJobs/wiki/Cluster-Functions), and include interactive (serial) execution, multicore clusters, as well as a variety of other systems. The Torque cluster we used also makes use of a template (`template.tmpl`) into which parameters of the job are inserted prior to submission to the cluster scheduler.

`submit.R` defines the submission process. The `resources` object is specific to our cluster, as is the "chunking" of the jobs (grouping them for batch submission to the scheduler). It is necessary to adapt these values to your cluster.

General tutorials on the use of `BatchJobs` and `BatchExperiments` are available [here](https://github.com/tudo-r/BatchJobs) and [here](https://github.com/tudo-r/BatchExperiments).

`reduce.R` contains the code to take the result of `simulation.R`, which stores results in a database, and produce the performance/evaluation criteria described in the paper (prediction, inference, and estimation error). `reduce.R` is set up to execute serially (due to IO limitations) and should not need to be modified.

`analysis.R` takes `results.csv` and produces the graphs in the paper.

`upload.txt` and `download.txt` specify the relevant files to upload/download from the cluster system on which the simulation runs. The `makefile` is also largely specific to our system.
>>>>>>> ed56a3194150f997c5837cb159d58c8229ba14b0
