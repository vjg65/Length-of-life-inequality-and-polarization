# An Open Database on Inequality and Polarization in Length of Life (1950-2021)
This database includes a comprehensive set of estimates of length of life inequality and polarization for 258 countries/areas for the period 1950-2021.

## The Database:
The database can be accessed at [https://osf.io/8ru5c/](https://osf.io/8ru5c/). The dataset contains seven files with information on national and regional estimates of inequality and polarization in the length of life for 258 countries/areas for each year from 1950 to 2021.

1. *NOV2023_both_total* includes the estimates for the whole population.
2. *NOV2023_both_15* gathers the data for the population aged over 15.
3. *NOV2023_female_total* stores the estimates for all women.
4. *NOV2023_female_15* includes the estimates for women aged over 15.
5. *NOV2023_male-total* gathers the estimates for the male population.
6. *NOV2023_male_total* provides estimates only for men aged over 15.
7. *quality_data* gathers information on the type of data used to build our estimates for each country and year, retrieved from past editions of the Demographic Year Book National.

## Replication Code
This repository includes code to compute and assemble the database and the tables and figures presented in the paper:

1. *ineq_both.R* can be used to reproduce the estimates on polarization and inequality for both sexes, including the whole population and only those individuals aged over 15.
2. *ineq_female.R* can be used to reproduce the estimates on polarization and inequality for the female population, including the whole population and only those individuals aged over 15.
3. *ineq_male.R* can be used to reproduce the estimates on polarization and inequality for the male population, including the whole population and only those individuals aged over 15.

To run any of the codes above, data from World Population Prospects on abridged life tables available at [https://population.un.org/wpp/Download/Standard/Mortality/](https://population.un.org/wpp/Download/Standard/Mortality/) must be downloaded.

4. *example_dist.R* reproduces Figure 1.
5. *maps.R* generates Figure 2.
6. *scatter.R* generates Figure 3.
7. *table_quality.R* reproduces Table 2.
