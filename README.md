# Sleepr

Sleep analysis with R.

## Installation

```
devtools::install_github("boupetch/rmdf")
```

## Testing

Testing use [testthat](https://github.com/r-lib/testthat). Sample EDFs from various databases are downloaded first. 

## Usage

### Formats

MDF<sup>1</sup> is a simple, efficient and interoperable file format for biological timeseries. The format supports raw signal as long as metadata storage. 
MDF uses binary files for signals and JSON for metadata. Signals values are encoded in binary, 32 bits, little endian.

## References

1. P. Bouchequet, D. Jin, G. Solelhac, M. Chennaoui, D. Leger, [«*Morpheo Data Format (MDF), un nouveau format de données simple, robuste et performant pour stocker et analyser les enregistrements de sommeil*»](https://www.sciencedirect.com/science/article/pii/S1769449318301304), Médecine du Sommeil, vol. 15, n 1, p. 48‑49, march 2018.