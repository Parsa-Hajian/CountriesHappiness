# Clustering Countries Based on Happiness Indicators (2015 – 2019)

This project uncovers how 150‑plus countries group together in terms of “happiness” and its key drivers.  
Using a fully reproducible **R** workflow, it harmonises five years of *World Happiness Report* data, applies robust preprocessing, and compares several unsupervised algorithms (k‑means, Ward hierarchical, DBSCAN) to reveal three clear clusters of nations.
> **High‑level takeaway:** about **90 %** of countries remain in the same happiness cluster from 2015 to 2019.

---

## Table of Contents
1. [Project Overview](#project-overview)  
2. [Data Source](#data-source)  
3. [Methodology](#methodology)  
4. [Findings & Policy Insights](#findings--policy-insights)  


---

## Project Overview

* **Goal:** discover natural clusters of countries based on seven happiness determinants (GDP per capita, social support, healthy life expectancy, freedom, generosity, corruption, overall score).  
* **Scope:** 5 × annual datasets (2015‑2019) merged into a single panel after rigorous column harmonisation and missing‑value imputation.
* **Output:** cluster labels, interactive maps, and low‑dimensional embeddings that remain stable across years.

---

## Data Source

World Happiness Report CSVs for **2015–2019** (public domain).  
Key numeric columns retained after uniform renaming:  

| Variable | Description | Range (all years) |
|----------|-------------|-------------------|
| `Score` | Overall happiness score | 2.69 – 7.77 |
| `GDP_per_capita` | Log‑GDP proxy | 0 – 2.10 |
| `Social_support` | Family & friends support | 0 – 1.64 |
| `Healthy_life_expectancy` | Life expectancy index | 0 – 1.14 |
| `Freedom` | Freedom to choose | 0 – 0.72 |
| `Generosity` | Charitable giving index | 0 – 0.82 |
| `Corruption` | Perceived corruption | 0 – 0.51 |


---

## Methodology

1.  **Load & Harmonise** – consistent column names, add Year, drop non‑overlapping features.

2.  **Pre‑EDA** – histograms, box‑plots, skewness; median imputation for the single missing value.

3.  **Robust Scaling** – centre by median, scale by MAD to curb 84 outliers in Generosity & Corruption. 

4.  **Robust Distances** – Minimum Covariance Determinant → robust Mahalanobis matrix.

5.  **Dimensionality Reduction** – Classical MDS, t‑SNE, UMAP for 2‑D visualisation.

6.  **Clustering**

    * **k‑means** (k = 3) chosen via elbow + silhouette.
    * **Ward Hierarchical** (k = 3) for corroboration.
    * **DBSCAN explored**; produced one giant cluster ⇒ discarded for policy use.

7.  **Validation** – average silhouette widths, cluster profiles, adjusted Rand with alternative methods.

8.  **Mapping** – join clusters to Natural Earth polygons for small‑multiple world maps.

---

## Findings & Policy Insights

### **1. Three clusters emerge**

* **High‑Happiness (≈ 11 %)** – high GDP, strong social support, long life expectancy, but higher perceived corruption.

* **Mid‑Happiness (≈ 50 %)** – average across most factors.

* **Low‑Happiness (≈ 39 %)** – low GDP, support, health & freedom; generosity/corruption near global mean. 

### **2. Temporal stability** – ≥ 90 % of countries keep their cluster for five straight years.

### **3. Most influential drivers** – GDP per capita and social support explain ≈ 80 % of the variance in overall score.

### **4. Policy levers**

* **Low cluster**: prioritise economic opportunity and community networks.

* **High cluster**: focus on good governance and anti‑corruption to safeguard current happiness levels.

