module Library where
import PdePreludat

-- 1.a
data Pizza = Pizza {
  ingredientes :: [String],
  tamanio :: Number,
  calorias :: Number
} deriving Show


data P = P {
  a :: String,
  b :: B
} deriving Show
data B = B {
  x :: Number,
  y :: Number
} deriving Show

tt p = p{ b = (b p){x = 2} }
-- 1.b
grandeDeMuzza = Pizza ["salsa", "mozzarella", "orégano"] 8 350
grandeDeJamonPalmitos = Pizza ["salsa", "mozzarella", "palmito", "jamon"] 8 500

-- 2
satisfaccion pizza 
  | elem "palmitos" . ingredientes $ pizza = 0
  | calorias pizza < 500 = valorSatisfaccion pizza
  | otherwise = (/2) . valorSatisfaccion $ pizza

valorSatisfaccion = (80*) . length . ingredientes

-- | (<500).calorias $ pizza = cuenta

-- 3
valorPizza pizza = 
  (*tamanio pizza) . (120*) . length . ingredientes $ pizza

valorPizza' (Pizza ings tam _) = 
  (tam*). (120*) . length $ ings

-- 4
-- 4.a
nuevoIngrediente ingrediente pizza =
  (sumarCalorias (2 * length ingrediente) . agregarIngrediente ingrediente) pizza
        
agregarIngrediente ingrediente pizza = 
  pizza{ ingredientes = ingrediente : ingredientes pizza}
-- pizza{ ingredientes = ingredientes pizza ++ [ingrediente] }

sumarCalorias valor pizza = 
  pizza{calorias = calorias pizza + valor }

-- 4.b
agrandar pizza = pizza{ tamanio = min 10 (tamanio pizza + 2) }

-- 4.c
mezcladita p1 p2 =
  sumarCalorias (calorias p1 / 2) p2{ ingredientes = ingredientes p2 ++ nuevosIngredientes }
  where nuevosIngredientes = filter (not.(`elem` (ingredientes p2))) (ingredientes p1)
-- where: Definición local

mezcladita' p1 p2 =
  (sumarCalorias (calorias p1 / 2). agregarIngredientes nuevosIngredientes) p2
  where nuevosIngredientes = filter (not.(`elem` (ingredientes p2))) (ingredientes p1)

-- elem "palmito" ["muzza", "tomate"]
-- "palmito" `elem` ["muzza", "tomate"]
-- flip elem ["muzza", "tomate"] "palmito"

agregarIngredientes ingredientes pizza =
  foldr agregarIngrediente pizza ingredientes

agregarIngredientes' ingredientes pizza =
  foldl (flip agregarIngrediente) pizza ingredientes

-- foldl' _ s [] = s
-- foldl' f s (x:xs) = foldl' f (f s x) xs

-- foldr' _ s [] = s
-- foldr' f s (x:xs) = f x (foldr' f s xs)

-- Pedidos! Ahora si llegaron las pip-sas
-- 5


-- 6
--6.a
--pizzeriaLosHijosDePato

--6.b
--pizzeriaElResumen

--6.c
--pizzeriaEspecial
--pizzeriaPescadito

--6.d
--pizzeriaGourmet
--pizzeriaLaJauja


--7
-- sonDignasDeCalleCorrientes