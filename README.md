# Simulation-based-Critical-Point-for-Simplex-Lattice-Designs

Mixture Experiments provide a foundation to optimize the predicted response based on blends of different components . Parody and Edwards (2006) gave a method of inference on the expected response of a 2nd-order rotatable design, utilizing a simulation-based critical point to give substantially sharper intervals when compared to the simultaneous confidence intervals provided by Sa and Edwards (1993). Here, we discuss the method of constructing simulation - based confidence intervals of the predicted response from a simplex-lattice design and demonstrate improvement over Scheffe’s simultaneous confidence intervals.

### mixtures.algorithm.R 
provides the main simulation algorithm to generate the critical points

### mixtures.analysis.R 
computes the relative efficiency of the simulation-based critical point with that of the scheffe adaptation under various settings

### pseudographs.R 
generates plots to visualize pseudocomponents of a simplex lattice design

### mixtures.contours.R 
Performs data visualizations and analysis of several datasets using prediction and improvement bands 

### ArtificialSweetener.csv, fruit.csv 
are the datasets on which the analysis is performed