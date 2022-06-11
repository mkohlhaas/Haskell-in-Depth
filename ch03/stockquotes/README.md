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

#### Test in REPL
```
cabal repl stockquotes
ghci> quotes <- readQuotes "../../data/quotes.csv
ghci> day $ head quotes
ghci> field2fun High $ last quotes
ghci> field2fun Volume $ last quotes
ghci> plotChart "Sample quotes" quotes "chart.svg"
ghci> si = statInfo quotes
ghci> import Fmt
ghci> pretty $ unlinesF si
ghci> putStr $ textReport $ statInfo quotes
ghci> readQuotes "data/quotes.csv" >>= putStr . textReport . statInfo
ghci> import Text.Blaze.Colonnade
ghci> printCompactHtml (encodeHtmlTable mempty colStats si)
```
