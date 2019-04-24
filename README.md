# nrl

This is a predictive model for the 2019 NRL season using features generated from historical data to determine whether the home team will win. The input data required is the home and away team names and the home and away odds. Rolling historical features are used on past games to help influence the decision. Model training is using autoML from h2o and current accuracy in 2019 season is 32/48 (66%). This is so far better than both picking bookmaker favourite (57.7%) and always the home team (54.2%)
