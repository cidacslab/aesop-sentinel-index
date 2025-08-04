# 🦠 Sentinel Surveillance Index Evaluation

This repository contains code and data for evaluating the **Sentinel Surveillance Index**, as described in:

> **"An integrated framework for modeling respiratory disease"**  
> *(Accepted for publication in the Royal Society Open Access journal)*  
> **Authors**: Borges, Dérick G. F.; Coutinho, Eluã R.; Jorge, Daniel; Barreto, Marcos E.; Ramos, Pablo I. P.; Barral-Netto, Manoel; Coutinho, Alvaro L.G.A.; Landau, Luiz; Pinho, Suani; Andrade, Roberto.
>
> Contacts: {derick.gabriel, randrade}@ufba.br

---

## 📁 Repository Contents

| File | Description |
|------|-------------|
| `LICENSE` | Repository usage license |
| `Network science algorithms.zip` | Fortran code package for network analysis |
| `SIRmeta_RGI.ipynb` | SIR modeling notebook for spatial epidemiological dynamics in Immediate Geographic Regions (IGRs) |
| `Sentinel_index.ipynb` | Notebook for calculating Sentinel Surveillance Indices |

---

## ⚙️ Fortran Code Overview

These algorithms analyze the structure of mobility networks and generate dendrograms for community identification.  
**Input**: Weighted network matrix (mobility data from IBGE, with weights of the input network corresponding to the number of persons moving from one IGR to another), node count, and threshold parameters.  
**Output**: Unweighted network representations used in subsequent algorithms.

### 📂 Fortran Algorithms

1. **Convert weighted mobility network into unweighted counterpart**  
   `redecrit1mc13.for`  
   Uses dissimilarity to determine critical mobility weights for Sentinel Index evaluation.  
   *Input file*: `ms_BA_rodo_34rgi_fs.dat`

2. **Generate adjacency matrix**  
   `simi.f90`  
   Produces adjacency matrix for selected critical mobility weight.

3. **Generate neighborhood matrix**  
   `madchar13.for`  
   Uses adjacency matrix to generate corresponding neighborhood matrix.

4. **Community detection and dendrogram construction**  
   `dendo2uQ.for`  
   Implements Newman-Girvan method to identify community structure.

5. **Compute betweenness centrality**  
   `betweenness.f90`  
   Uses Brandes algorithm to evaluate node importance via shortest paths.

---

## 🐍 Python Code Overview

These notebooks evaluate infection transmission and surveillance indices using mobility and healthcare data. The first algorithm is used to evaluate the potential number of infection transmission between IRG’s based on the metapopulation model. Input data consists of two tables, one containing the numbers of URTI-related PHC encounters in each IGR, and the other one containing the number of persons moving from one IGR to another, extracted from the IBGE mobility data. 

The second algorithm evaluates the Sentinel Index. Input data consists of three lists, containing the IGR population, the total number of infections exported by each IGR, and the betweeness centrality score of the network nodes evaluated at the selected value of the weight threshold (as identified using the code above).

### 📘 Notebooks

- **Meta-Population SIR Model**  
  `SIRmeta_RGI.ipynb`  
  Simulates disease spread across IGRs using mobility and PHC data, based on the metapopulation model.

- **Sentinel Surveillance Index Evaluation**  
  `Sentinel_index.ipynb`  
  Assesses surveillance indices to identify critical regions for monitoring.

**Inputs**:
- URTI-related PHC encounters per IGR
- Mobility data between IGRs (from IBGE)
- IGR population
- Exported infections per IGR
- Betweenness centrality scores

---

## 🛠️ Requirements

### 🧮 Fortran

To run the Fortran algorithms, ensure you have a Fortran compiler installed:

- `gfortran`
- Intel Fortran Compiler

### 🐍 Python

To run the Jupyter notebooks, make sure you have the following installed:

- **Python**: version 3.8 or higher  
- **Jupyter Notebook**
- **Python Libraries**:
  - `numpy`
  - `pandas`
  - `matplotlib`
  - `scipy`
  - `seaborn`

Install the required libraries using pip:

```bash
pip install numpy pandas matplotlib scipy seaborn
```
