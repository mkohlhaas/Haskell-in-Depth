import Data.Functor ((<&>))
import Text.Read (readMaybe)

type Name = String

type Phone = String

type Location = String

type PhoneNumbers = [(Name, Phone)]

type Locations = [(Phone, Location)]

doubleStrNumber ∷ (Num a, Read a) ⇒ String → Maybe a
doubleStrNumber str = readMaybe str <&> (2 *)

-- applicative style
plusStrNumbers ∷ (Num a, Read a) ⇒ String → String → Maybe a
plusStrNumbers str1 str2 = (+) <$> readMaybe str1 <*> readMaybe str2

-- monadic style
plusStrNumbers' ∷ (Read a, Num a) ⇒ String → String → Maybe a
plusStrNumbers' str1 str2 = do
  n1 ← readMaybe str1
  n2 ← readMaybe str2
  pure $ n1 + n2

locateByName ∷ PhoneNumbers → Locations → Name → Maybe Location
locateByName phoneNumbers locations name = lookup name phoneNumbers >>= flip lookup locations

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

-- >>> doubleStrNumber "21" ∷ Maybe Int
-- Just 42
--
-- >>> doubleStrNumber "x" ∷ Maybe Int
-- Nothing
--
-- >>> plusStrNumbers "20" "22" ∷ Maybe Int
-- Just 42
--
-- >>> plusStrNumbers "10" "x" ∷ Maybe Int
-- Nothing
--
-- >>> plusStrNumbers' "20" "22" ∷ Maybe Int
-- Just 42
--
-- >>> plusStrNumbers' "10" "x" ∷ Maybe Int
-- Nothing
--
-- >>> locateByName phoneNumbers locations "Wuppertal"
-- Just "Nordrhein-Westfalen"
--
-- >>> locateByName phoneNumbers locations "Koblenz"
-- Nothing

main ∷ IO ()
main = do
  print (doubleStrNumber "21" ∷ Maybe Int) --------------------- Just 42
  print (doubleStrNumber "x" ∷ Maybe Int) ---------------------- Nothing
  print (plusStrNumbers "20" "22" ∷ Maybe Int) ----------------- Just 42
  print (plusStrNumbers "10" "x" ∷ Maybe Int) ------------------ Nothing
  print (plusStrNumbers' "20" "22" ∷ Maybe Int) ---------------- Just 42
  print (plusStrNumbers' "10" "x" ∷ Maybe Int) ----------------- Nothing
  print $ locateByName phoneNumbers locations "Wuppertal" ------ Just "Nordrhein-Westfalen"
  print $ locateByName phoneNumbers locations "Koblenz" -------- Nothing
