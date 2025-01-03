# Predator-recognition

## Title of project
Recognition of predator cues hinders social communication and group cohesion

## Authors
Melquisedec Gamba-Rios, Gary F. McCracken and Gloriana Chaverri

## General info
We tested the ability of bats that are preyed upon by other bats to discriminate between echolocation calls of predators and non-predators, and the impact of risk reduction strategies on communication and sociality. 

## Data
* [Data for number of response calls emitted per individual and treatment](https://github.com/morceglo/Predator-recognition/blob/main/data.csv)
Key to listed variables: bat = unique identifier of individuals, experiment = type of echolocation calls emitted during the trial in the order in which they were presented to focal bats (C: control, P: predator, F: frugivorous, I: insectivorous), treatment = identifies if the response calls quantified correspond to events in which a predator or non-predator echolocation call was broadcast, bat_type = identifies the type of echolocation calls (control, carnivorous, frugivorous, insectivorous) that were being emitted when responses were quantified, period = determines if data on number of responses were collected before, during, or after the emission of echolocation calls, responses = number of response calls emitted by the focal bat.

* [Results of experiment 1](E1_data.csv)
Key to listed variables: id = unique identifier of individuals, trial = identifies whether the data correspond to the first, second, third, fourth or fifth trial performed, stimulus = shows which stimulus was being presented in a given trial, occ = shows the number of occurrences of a feeeding attempt, dur = shows how long bats fed for, first = determines if this trial represented the first time in which the bat fed, first_graph = in words (yes or no) based on the previous column (first). 

* [Results of experiment 2](E2_data.csv)
Key to listed variables: id = unique identifier of individuals, obs = a unique identifier (based on the video's id) of the overall test perfomed on an individual, trial = identifies whether the data correspond to the first, second, or third trial performed on an individual, stimulus = shows which stimulus was being presented, pre_calls = shows whether the bat had been exposed to food calls during a given instance (row), pre_chew = shows whether the bat had been exposed to chewing sounds during a given instance (row), pre_pn = shows whether the bat had been exposed to pink noise during a given instance (row), playlist = unique identifier of the playlist used, dist_75_cm = represents the time (in seconds) that the bat spent in the 75 cm region, dist_50_cm = represents the number of visits that bats made to the 50 cm region. 

## Analyses

* [Statistical analysis and graphs](https://github.com/morceglo/Predator-recognition/blob/main/Analyses.R)

## Status
Project is: Under consideration at Proceedings of the Royal Society of London

## Contact
Created by [Gloriana_Chaverri](batcr.com/)
