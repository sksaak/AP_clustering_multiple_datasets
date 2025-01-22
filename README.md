# Integrating audiological datasets via federated merging of Auditory Profiles 

This folder contains the code for Auditory Profiling for the paper "Integrating audiological datasets via federated merging of Auditory Profiles". 

# Abstract 

Audiological datasets contain valuable knowledge about hearing loss in patients, which can be uncovered using data-driven, federated learning techniques. Our previous approach summarized patient information from one audiological dataset into distinct Auditory Profiles (APs). To obtain a better estimate of the audiological patient population, however, patient patterns must be analyzed across multiple, separated datasets, and finally, be integrated into a combined set of APs.

This study aimed at extending the existing profile generation pipeline with an AP merging step, enabling the combination of APs from different datasets based on their similarity across audiological measures. The 13 previously generated APs (NA=595) were merged with 31 newly generated APs from a second dataset (NB=1272) using a similarity score derived from the overlapping densities of common features across the two datasets. To ensure clinical applicability, random forest models were created for various scenarios, encompassing different combinations of audiological measures.

A new set with 13 combined APs is proposed, providing separable profiles, which still capture detailed patient information from various test outcome combinations. The classification performance across these profiles is satisfactory. The best performance was achieved using a combination of loudness scaling, audiogram and speech test information, while single measures performed worst.

The enhanced profile generation pipeline demonstrates the feasibility of combining APs across datasets, which should generalize to all datasets and could lead to an interpretable global profile set in the future. The classification models maintain clinical applicability.

#### Link to the paper on arXiv: https://arxiv.org/abs/2407.20765

# Folder structure

Start the scripts with AuditoryProfiling.RProj. The scripts can be used for generating auditory profiles with federated learning. 

## Profile_generation
- Code to generate profiles on a dataset
- These analyses can be performed at the data sensitive location and only the anonymized profiles need to be shared

## Profile_merging
- Code to merge profiles generated on different datasets

<img src="https://github.com/sksaak/AP_clustering_multiple_datasets/blob/main/analyses_pipeline.png?raw=true" alt="Pipeline Analysis" width="500"/>
-> Visualization of the One-shot federated learning framework (code stored in "Profile_generation" & "Profile_merging"

## Classification
- Code to build classification models for the profile set 



#### Data availability
According to the Data Usage Agreement of the authors, the datasets analyzed in this study can only be shared upon motivated request. Note that dataset A will soon be published.
