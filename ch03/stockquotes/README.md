#### Files

```
Main.hs
Params.hs (process command-line arguments)
QuoteData.hs (read quote data from a CSV file)
StatReport.hs (compute statistics)
Charts.hs (plot SVG charts)
HtmlReport.hs (generate HTML reports)
```

#### Generate module structure
```
graphmod > modules.dot
xdot modules.dot
dot -Tsvg modules.dot > modules.svg
```

#### Run examples
```
cabal run stockquotes -- -h
cabal run stockquotes -- ../../data/quotes.csv
cabal run stockquotes -- ../../data/quotes.csv -n Example -c --html Example.html
xdg-open chart_Example.svg
xdg-open Example.html
```
