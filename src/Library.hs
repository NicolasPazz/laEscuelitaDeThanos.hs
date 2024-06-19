module Library where
import PdePreludat
import Test.Hspec.Runner (Config(configPrintSlowItems))

doble :: Number -> Number
doble numero = numero + numero

--Parte A

data Ninja = Ninja {
    nombreNinja :: String,
    herramientas :: [Herramienta],
    jutsus :: [Jutsu],
    rango :: Rango
} deriving (Show)

type Nombre = String
type Herramienta = (Nombre, Number)
type Rango = Number
type Jutsu = Mision -> Mision

obtenerHerramienta :: Herramienta -> Ninja -> Ninja
obtenerHerramienta (nombre, cantidad) ninja
    | (<= 100) . (+ cantidad) . sum . map snd $ herramientas ninja = ninja {herramientas = (nombre, cantidad) : herramientas ninja}
    | otherwise = ninja {herramientas = (nombre, (100 -) . sum . map snd $ herramientas ninja) : herramientas ninja}

usarHerramienta :: Herramienta -> Ninja ->  Ninja
usarHerramienta herramienta ninja  = ninja {herramientas = filter (/= herramienta) $ herramientas ninja}

--Parte B

data Mision = Mision {
    cantidadNinjas :: Number,
    rangoRecomendado :: Rango,
    ninjasEnemigos :: Equipo,
    recompensa :: Herramienta
} deriving (Show)

type Equipo = [Ninja]

esDesafiante :: Equipo -> Mision -> Bool
esDesafiante equipo mision = any ((< rangoRecomendado mision) . rango) equipo && ((>= 2) . length $ ninjasEnemigos mision)

esCopada :: Mision -> Bool
esCopada mision    =    (fst (recompensa mision) == "Bomba de Humo" && snd (recompensa mision) == 3) ||
                        (fst (recompensa mision) == "Shuriken"      && snd (recompensa mision) == 5) ||
                        (fst (recompensa mision) == "Kunai"         && snd (recompensa mision) == 14)

esFactible :: Equipo -> Mision -> Bool
esFactible equipo mision = (not . esDesafiante equipo $ mision) && ((>= cantidadNinjas mision) . length $ equipo)  || ((> 500) . sum . map snd . concatMap herramientas $ equipo)

--

fallarMision :: Mision -> Equipo -> Equipo
fallarMision mision = map (\ninja -> ninja {rango = rango ninja - 2}) . filter ((>= rangoRecomendado mision) . rango)

cumplirMision :: Mision -> Equipo -> Equipo
cumplirMision mision = map (\ninja -> obtenerHerramienta (recompensa mision) ninja {rango = (+ 1) . rango $ ninja})

--

clonesDeSombra :: Number -> Jutsu
clonesDeSombra cantidad mision = mision {cantidadNinjas = min 1 . (+ cantidad) . cantidadNinjas $ mision}

fuerzaDeUnCentenar :: Jutsu
fuerzaDeUnCentenar mision = mision {ninjasEnemigos = filter ((>= 500) . rango) $ ninjasEnemigos mision}

--

misionSimplificada :: Equipo -> Mision -> Mision
misionSimplificada equipo mision = foldl (flip ($)) mision (concatMap jutsus equipo)

ejecutarMision :: Mision -> Equipo -> Bool
ejecutarMision mision equipo = esCopada (misionSimplificada equipo mision) || esFactible equipo (misionSimplificada equipo mision)

--Parte C

granGuerraNinja = Mision 100000 100 infinitosEnemigosZetsu ("Abanico de Madara Uchiha", 1)

crearZetsu :: Number -> Ninja
crearZetsu n = Ninja ("Zetsu " ++ show n) [] [] 600

infinitosEnemigosZetsu :: [Ninja]
infinitosEnemigosZetsu = map crearZetsu [1..]

--a. esDesafiante
--esDesafiante [naruto, sasuke, sakura] granGuerraNinja
--No convergerá, ya que la cantidad de ninjas enemigos es infinita y la función length necesita recorrer todos los elementos de la lista.

--b. esCopada
--esCopada granGuerraNinja
--Converge devolviendo False, ya que no utiliza la lista infitita y la recompensa de la gran guera ninja no es ninguna de las opciones para misiones copadas.

--c. fuerzaDeUnCentenar
--fuerzaDeUnCentenar granGuerraNinja
--No convergerá, ya que la cantidad de ninjas enemigos es infinita y la función filter necesita recorrer todos los elementos de la lista.

