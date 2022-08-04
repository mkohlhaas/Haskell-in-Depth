#### Files

```
app/Main.hs
app/Params.hs (process command-line arguments)
app/QuoteData.hs (read quote data from a CSV file)
app/StatReport.hs (compute statistics)
app/Charts.hs (plot SVG charts)
app/HtmlReport.hs (generate HTML reports)
```

#### Generate module structure
``` shell
graphmod > modules.dot
xdot modules.dot
dot -Tsvg modules.dot > modules.svg
```

#### Run examples
``` shell
cabal run stockquotes -- -h
cabal run stockquotes -- ../../data/quotes.csv
cabal run stockquotes -- ../../data/quotes.csv -n Example -c --html Example.html
xdg-open chart_Example.svg
xdg-open Example.html
```

#### Test in REPL
``` shell
cabal repl stockquotes
ghci> quotes <- readQuotes "../../data/quotes.csv"
ghci> import QuoteData
ghci> day $ head quotes
ghci> field2fun High $ last quotes
ghci> field2fun Volume $ last quotes
ghci> plotChart "Sample quotes" quotes "chart.svg"
ghci> si = statInfo quotes
ghci> import Fmt
ghci> pretty $ unlinesF si
ghci> putStr $ textReport $ statInfo quotes
ghci> readQuotes "data/quotes.csv" >>= putStr . textReport . statInfo
ghci> readQuotes "../../data/quotes.csv" >>= putStr . textReport . statInfo
ghci> readQuotes "../../data/quotes.csv" >>= fmt . jsonListF
ghci> import Text.Blaze.Colonnade
ghci> printCompactHtml (encodeHtmlTable mempty colStats si)
```
