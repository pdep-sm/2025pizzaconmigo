module Library where
import PdePreludat
import Data.Foldable (maximumBy)

-- 1.a
data Pizza = Pizza {
  ingredientes :: [String],
  tamanio :: Number,
  calorias :: Number
} deriving Show

-- 1.b
grandeDeMuzza = Pizza ["salsa", "mozzarella", "orégano"] 8 350
grandeDeJamonPalmitos = Pizza ["salsa", "mozzarella", "palmito", "jamón"] 8 500

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
nuevoIngrediente' ingrediente =
  sumarCalorias (2 * length ingrediente) . agregarIngrediente ingrediente
   
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
  where nuevosIngredientes = filter (not.(`elem` ingredientes p2)) (ingredientes p1)
-- where: Definición local

mezcladita' p1 p2 =
  (sumarCalorias (calorias p1 / 2). agregarIngredientes nuevosIngredientes) p2
  where nuevosIngredientes = filter (not.(`elem` ingredientes p2)) (ingredientes p1)

-- elem "palmito" ["muzza", "tomate"]
-- "palmito" `elem` ["muzza", "tomate"]
-- flip elem ["muzza", "tomate"] "palmito"

agregarIngredientes ingredientes pizza =
  foldr agregarIngrediente pizza ingredientes

agregarIngredientes' ingredientes pizza =
  foldl (flip agregarIngrediente) pizza ingredientes

-- 5
-- Pedidos! Ahora si llegaron las pip-sas
type Pedido = [Pizza]

satisfaccionPedido :: Pedido -> Number
satisfaccionPedido = sum . map satisfaccion

-- 6
type Pizzeria = Pedido -> Pedido
pizzeriaLosHijosDePato, pizzeriaElResumen :: Pizzeria
pizzeriaEspecial :: Pizza -> Pizzeria
pizzeriaGourmet :: Number -> Pizzeria

--6.a
pizzeriaLosHijosDePato = map (agregarIngrediente "palmito")

--6.b
pizzeriaElResumen pedido = zipWith mezcladita' pedido (tail pedido)

--6.c
pizzeriaEspecial predilecta = map (mezcladita' predilecta)
anchoasBasica :: Pizza
anchoasBasica = Pizza {
  ingredientes = ["salsa", "anchoas"],
  calorias = 270,
  tamanio = 8
}
pizzeriaPescadito :: Pizzeria
pizzeriaPescadito = pizzeriaEspecial anchoasBasica

--6.d
pizzeriaGourmet nivelExquisitez = map agrandar . filter ((>nivelExquisitez).satisfaccion)
pizzeriaLaJauja :: Pizzeria
pizzeriaLaJauja = pizzeriaGourmet 399

-- foldl' _ s [] = s
-- foldl' f s (x:xs) = foldl' f (f s x) xs

-- foldr' _ s [] = s
-- foldr' f s (x:xs) = f x (foldr' f s xs)

-- foldl1' f (x:xs) = foldl' f x xs
-- foldr1' f lista  = foldr' f (last lista) (init lista)


--7.a
sonDignasDeCalleCorrientes :: Pedido -> [Pizzeria] -> [Pizzeria]
sonDignasDeCalleCorrientes pedido pizzerias = filter (esDignaDeCalleCorrientes pedido) pizzerias

esDignaDeCalleCorrientes :: Pedido -> Pizzeria -> Bool
esDignaDeCalleCorrientes pedido pizzeria =
  satisfaccionPedido pedido < (satisfaccionPedido . pizzeria) pedido

--7.b
mejorPizzeria :: Pedido -> [Pizzeria] -> Pizzeria
mejorPizzeria pedido pizzerias = 
  foldl1 (mayorSatisfaccion pedido) pizzerias

mayorSatisfaccion :: Pedido -> Pizzeria -> Pizzeria -> Pizzeria
mayorSatisfaccion pedido pizzeria1 pizzeria2  
  | valor pizzeria1 > valor pizzeria2 = pizzeria1
  | otherwise = pizzeria2
  where valor pizzeria = satisfaccionPedido . pizzeria $ pedido

-- 9
laPizzeriaPredilecta = foldl (.) id
laPizzeriaPredilecta' = foldl1 (.)