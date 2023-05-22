Calculate the Stochastic Cost Frontier of the Portuguese Healthcare System, with particular focus on insularity costs from the regions of Madeira and Açores. 

The project contains the following files:
1. Descriptive.R
2. OLSRegressions.R
3. SFA.R
3. SFA_TESTS.R
4. Simplified_SFA.R
SFA_Insularidade.Rproj

Required Databases:
- all: Costs and Volume variables from Hospitals in the mainland. These were taken from ACSS portal da transparência and Benchmarking Hospitais.

- hospital: Costs and Volume variables from Hospitals in the mainland. These were taken from ACSS portal da transparência. (I noticed some differences in costs between Benchmarking and ACSS datasets)

- hospitals_aco: Costs and Volume variables from Hospitals in the mainland and Açores. Data was provided from Universidade dos açores, gathered from yearly accounting reports.

- hospital_all: a join between hospitals_aco and hospital.

Instructions:

To run files 1. Descriptive.R, 2. OLSRegressions.R and 3. SFA_TESTS.R make sure you have the "all" database loaded.

To run files 3. SFA.R and 4. Simplified_SFA.R make sure you have the "hospital" and "hospitals_all" and "hospital_aco"
