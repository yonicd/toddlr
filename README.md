# toddlr

R package that creates a self-updating interactive dashboard displaying [@dandrenzer](https://twitter.com/dandrezner)'s "Toddler in Chief" Twitter thread.

## Installation

```r
install.packages('remotes')
remotes::install_github('yonicd/toddlr')
```

## Running the app

```r
toddlr::toddlr()
```

## In the App

### Side Panel

   - Filter for the timeline
   - Filter for the subject making the statment
   - Slick carousel with last N statuses in thread
   - Download button to get data source
   
### Main Panel

  - (Left) Barplot of frequency of type of subject making the statment by proximity to the president
  - (Right Top) Trend of cumulative frequency of statuses by proximity to the president
  - (Right Bottom) Trend of frequency of statusesby proximity to the president

## Example

![](https://github.com/yonicd/toddlr/blob/gif/toddlr.gif?raw=true)