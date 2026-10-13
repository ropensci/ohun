# Example data frame of a selection table including all sound events of interests

`lbh_reference` is a data frame containing the start, end, bottom and
top frequency of all songs in 'lbh_1.wav' and 'lbh_2.wav' recordings.

## Usage

``` r
data(lbh_reference)
```

## Format

A data frame with 19 rows and 6 variables:

- sound.files:

  recording names

- selec:

  selection numbers within recording

- start:

  start times of selected sound event

- end:

  end times of selected sound event

- bottom.freq:

  lower limit of frequency range

- top.freq:

  upper limit of frequency range

## Source

Marcelo Araya-Salas, ohun

## Details

A data frame containing the start, end, low and high frequency of
*Phaethornis longirostris* (Long-billed Hermit) songs from the 2 example
sound files included in this package ('lbh_1' and 'lbh_2'). These two
files are clips extracted from the xeno-canto's '154138' and '154129'
recordings respectively.
