# MLINDIV
Data Processing scripts for MLINDIV. Participant numbered data files are too large for Github, so in order to access them go to the Spatial Neuroscience Lab Google Drive > Projects > MLINDIV. If you do not have access to those files, contact someone within the lab who does.

The scripts are still in the process of being revised and compiled into one clean script, but for now are in separate scripts for ease of use and tweaking.

In order to run the full process:
  1) Make sure all scripts, data tables found in this github page, and Subject EPrime Data folders (001 ... n) are all in the same directory.
  2) For each script, assign the above mentioned directory as 'working_dir' found in the top section of each script (i.e., working_dir <- 'my_directory')
  3) Run Explore_MLINDIV.R to obtain the behavioral full
  4) Run trial_full.R to obtain the trial full
  5) Run path_distances.R to calculate and add the traveled trial distances to trial full
  6) Run participant_full.R to obtain the participant full

  


Explore_MLINDIV.R -> MLINDIV_behavioral_full.csv
- 
This script reads in all the participant behavioral data spit out from eprime (found in the SNL google drive), and compiles a full behavioral data frame that contains 30+ variables on over 80K observations. Each observation is an Eprime recorded response from a participant during their experiment session. See MLINDIV_Output_Data for more details.


trial_full.R -> MLINDIV_trial_full.csv
-
This script reads in the full file created by the Explore_MLINDIV.R script and creates a trial by trial data frame. This means that instead of each row representing a recorderd response by a participant, each row is a summary of each different trial undergone by the participant. This includes average response time, trial completion time, start/goal locations, path vectors, etc. See MLINDIV_Output_Data for more details.


path_distances.R -> MLINDIV_trial_full.csv
-
This contains some necessary functions for calculating distances traveled across trials. 


participant_full.R -> MLINDIV_participant_full.csv
-
This script reads in the trial full file output from trial_full.R and creates a data frame where each row is a single participant, and subsequent columns are all summary data from each individual's experiment run. This includes subject means and standard deviations for accuracy, selections made, nodes count, turns count, response time, etc. This file also contains the visits count for different nodes, object, and landmarks during the explore session.
