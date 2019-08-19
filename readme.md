# EyeR
Package to load, preprocess and work with raw eyetracking data.

## Core ideas

The package works with `EyerObject` of S3 class `eyer`. You can construct the object on your own based on [documentation](https://hejtmy.github.io/eyer/reference/EyerObject.html)

### Loading
As the raw eyetracker data can get very large, the idea is that after the data has been preprocessed and fixations, saccades and events have been extracted, the package resaves the preprocessed files inside the original folder. In this way, when you load the participant's data again, you can immediately load the preprocessed files. In this way, the general function to read data takes a folder rather than a file and tries to look for and load any preprocessed raw files

## Example code

```{r}
dir <- 'path to the directory with the eyetracker data'
EYETRACKER <- 'SR 1000' #eyetracker type
eye <- load_eyetracker_data(dir, EYETRACKER)
```

### Supported eyetrackers
Currently this package supports these eyetrackers. Code for the functions is in parenthesis.

- Eyelink SR 1000 (SR 1000)
