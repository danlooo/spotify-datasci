# Spotify Data Science

Exploring properties of songs and predicting track features like its genre from spatially scaled notes.

## Abstract

Music genres are often composed of particular pitch patterns that can be used for prediction.
The Spotify API provides features for entire tracks, e.g. its loudness or acousticness scores, as well as the sequence of the individual pitches (notes).
Totaling 3600 tracks across techno, rock, jazz and classicsal generes were analyzed and used for both classical Machine Learning and Deep Learning modeling methods.
Validation accuracy of both approaches were similar suggesting that more sophisticated network architectures are needed to increase the model performance.

## Tech stack

- [keras](https://keras.io/) deep learning framework
- [Tensorflow](https://www.tensorflow.org/) deep learning framework
- [tidymodels](https://www.tidymodels.org/) machine learning framework
- [tidyverse](https://www.tidyverse.org/) data wrangling
- [R targets](https://books.ropensci.org/targets/) pipeline system
- [spotifyr](https://www.rcharlie.com/spotifyr/) REST API calls
- [quarto](https://quarto.org/) notebook documentation

Keywords:

- Spatial data analysis
- deep learning
- REST APIs

[This project on GitHub Pages](https://danlooo.github.io/spotify-datasci/)

## Development

Create a file `.env` in the main directory to define the environment variables `SPOTIFY_CLIENT_ID` and `SPOTIFY_CLIENT_SECRET`