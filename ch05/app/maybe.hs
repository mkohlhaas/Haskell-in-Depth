import Text.Read (readMaybe)

type Name = String

type Phone = String

type Location = String

type PhoneNumbers = [(Name, Phone)]

type Locations = [(Phone, Location)]

doubleStrNumber1 ∷ (Num a, Read a) ⇒ String → Maybe a
doubleStrNumber1 str =
  case readMaybe str of
    Just x → Just (2 * x)
    Nothing → Nothing

doubleStrNumber2 ∷ (Num a, Read a) ⇒ String → Maybe a
doubleStrNumber2 str = (2 *) <$> readMaybe str

plusStrNumbers ∷ (Num a, Read a) ⇒ String → String → Maybe a
plusStrNumbers str1 str2 = (+) <$> readMaybe str1 <*> readMaybe str2

locateByName ∷ PhoneNumbers → Locations → Name → Maybe Location
locateByName pnumbers locs name =
  lookup name pnumbers >>= flip lookup locs

locateByName' ∷ PhoneNumbers → Locations → Name → Maybe Location
locateByName' pnumbers locs name =
  case lookup name pnumbers of
    Just number → lookup number locs
    Nothing → Nothing

phoneNumbers ∷ PhoneNumbers
phoneNumbers =
  [ ("Berlin", "030"),
    ("Bielefeld", "0521"),
    ("Bochum", "0234"),
    ("Bremen", "0421"),
    ("Dortmund", "0231"),
    ("Dresden", "0351"),
    ("Duisburg", "0203"),
    ("Düsseldorf", "0211"),
    ("Essen", "0201"),
    ("Frankfurt am Main", "069"),
    ("Hamburg", "040"),
    ("Hannover", "0511"),
    ("Köln", "0221"),
    ("Leipzig", "0341"),
    ("München", "089"),
    ("Nürnberg", "0911"),
    ("Stuttgart", "0711"),
    ("Wuppertal", "0202")
  ]

locations ∷ Locations
locations =
  [ ("0201", "Nordrhein-Westfalen"),
    ("0202", "Nordrhein-Westfalen"),
    ("0203", "Nordrhein-Westfalen"),
    ("0211", "Nordrhein-Westfalen"),
    ("0221", "Nordrhein-Westfalen"),
    ("0231", "Nordrhein-Westfalen"),
    ("0234", "Nordrhein-Westfalen"),
    ("030", "Berlin"),
    ("0341", "Sachsen"),
    ("0351", "Sachsen"),
    ("040", "Hamburg"),
    ("0421", "Bremen"),
    ("0511", "Niedersachsen"),
    ("0521", "Nordrhein-Westfalen"),
    ("069", "Hessen"),
    ("0711", "Baden-Württemberg"),
    ("089", "Bayern"),
    ("0911", "Bayern")
  ]

main ∷ IO ()
main = do
  print (doubleStrNumber1 "21" ∷ Maybe Int) --------------------- Just 42
  print (doubleStrNumber1 "x" ∷ Maybe Int) ---------------------- Nothing
  print (doubleStrNumber2 "21" ∷ Maybe Int) --------------------- Just 42
  print (plusStrNumbers "20" "22" ∷ Maybe Int) ------------------ Just 42
  print (plusStrNumbers "10" "x" ∷ Maybe Int) ------------------- Nothing
  print $ locateByName phoneNumbers locations "Wuppertal" ------- Just "Nordrhein-Westfalen"
  print $ locateByName phoneNumbers locations "Koblenz" --------- Nothing
  print $ locateByName' phoneNumbers locations "Wuppertal" ------ Just "Nordrhein-Westfalen"
  print $ locateByName' phoneNumbers locations "Koblenz" -------- Nothing
