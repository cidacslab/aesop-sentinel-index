Network Science and Sentinel Surveillance

This repository contains codes and data to the evaluation of the sentinel surveillance index.
Repository Contents

LICENSE: File containing the repository's usage license.
Network science algorithms.zip: Package containing Fortran codes for network analysis.
SIRmeta_RGI.ipynb: Notebook for SIR modeling and spatial epidemiological dynamics in Immediate Geographic Regions (IGRs).
Sentinel_index.ipynb: Notebook for calculating sentinel surveillance indices.

Codes implemented in Fortran

These algorithms are used to study the general network structure, leading to the construction of a dendrogram for community identification. Input data consists of a matrix containing the edge strengths of a weighted network, besides parameters indicating the number of nodes, threshold limits to project weighted network into a sequence of unweighted ones. Outputs of an algorithm will, in the sequence, be used as input to the forthcoming algorithm in the sequence. For the current case, the weights of the input network correspond to the number of persons moving from one to another IGR, extracted from the IBGE mobility data. 

Folders and Codes

1. Convert weighted mobility network into unweighted counterpart: 
redecrit1mc13.for – This algorithm uses the dissimilarity concept to determine the best critical values of mobility weight to be used in the evaluation of Sentinel Index, both in the betweenness centrality analysis and in the meta-population SIR model.(Input data to start reproducibility of results is "ms_BA_rodo_34rgi_fs.dat")

2. Generates adjacency matrix for selected critical value of mobility weight: 
simi.f90 – This algorithm generates the adjacency matrix for the selected critical value of mobility weight selected by analyzing the results provided by redecrit1mc13.for .

3. Generates neighborhood matrix for selected critical value of mobility weight: madchar13.for – This algorithm uses the adjacency matrix for the selected critical value of mobility weight to generate the corresponding neighborhood matrix. 

4. Determines community structure and build dendrogram using Newman-Girvan method: 
dendo2uQ.for – The Newman-Girvan algorithm is used to identify the community structure of the un-weighted network obtained at the selected critical value of mobility weight.

5. Computes betweenness centrality for the network:
betweenness.f90 – This algorithm implements the Brandes algorithm to calculate the betweenness centrality of the nodes in the unweighted network. Betweenness centrality measures the importance of each node in terms of the shortest paths passing through it, providing insights into network connectivity and key nodes for information or disease spread. 


Requirements
To run the algorithms, make sure you have a Fortran compiler installed:
Gfortran
Intel Fortran Compiler

Codes Implemented in Python
The first algorithm is used to evaluate the potential number of infection transmission between IRG’s based on the metapopulation model. Input data consists of two tables, one containing the numbers of URTI related PHC encounters in each IGR, and the other one containing the number of persons moving from one to another IGR, extracted from the IBGE mobility data
The second algorithm evaluates the Sentinel Index. Input data consists of three lists, containing the IGR population, the total number of infections exported by each IGR, and the betweeness centrality score of the network nodes evaluated at the selected value of the weight threshold.

Meta-Population SIR Model (SIRmeta_RGI.ipynb) – This notebook implements an SIR model to study disease spread across different Immediate Geographic Regions (IGRs), considering urban mobility data and primary healthcare data. 


Sentinel Surveillance Index (Sentinel_index.ipynb) – This notebook evaluates surveillance indices to identify critical regions for disease monitoring. 

Requirements
To run the notebooks, make sure you have installed:
Python 3.8+
Jupyter Notebook
Libraries: numpy, pandas, matplotlib, scipy, seaborn

Authors
Dérick G. F. Borges and Roberto F. S. Andrade

Email address
derick.gabriel@ufba.br or randrade@ufba.br

