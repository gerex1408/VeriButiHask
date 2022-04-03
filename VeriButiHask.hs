-- Authors : Gerard Lozano & Carla Davesa
-- Desenvolupament d’un programa per a verificar i avaluar partides de Butifarra

data Pal = Oros | Copes | Espases | Bastos  deriving (Eq , Show , Read )

data TipusCarta = Dos | Tres | Quatre | Cinc | Sis | Set | Vuit | Sota | Cavall | Rei | As | Manilla deriving (Eq , Ord , Show , Read )

data Carta =  CartaConcreta TipusCarta Pal deriving (Eq , Show , Read )

type Trumfu =  Pal

instance Ord Carta where
    CartaConcreta x y <= CartaConcreta z t = if y == t then x<=z else error "No es poden comparar"
    CartaConcreta x y >= CartaConcreta z t = if y == t then x>=z else error "No es poden comparar"
    CartaConcreta x y < CartaConcreta z t = if y == t then x<z else error "No es poden comparar"
    CartaConcreta x y > CartaConcreta z t = if y == t then x>z else error "No es poden comparar"


cartes :: [Carta]
cartes = [CartaConcreta Dos Oros, CartaConcreta Tres Oros, CartaConcreta Manilla Bastos, CartaConcreta Cavall Espases]

-- Donada una Carta, ens retorna el seu Pal
palCarta :: Carta -> Pal
palCarta (CartaConcreta x y) = y

-- Donada una Carta, ens retorna el seu Pal
tipusCarta :: Carta -> TipusCarta
tipusCarta (CartaConcreta x y) = x

--Posició d'una carta en una llista de cartes
posicioCarta :: Carta -> [Carta] -> [Integer]
posicioCarta x xs = [p | (p,y) <- zip [0..] xs, x==y]


-- Donada una llista de cartes i un pal, torni les que són del pal donat
cartesPal :: [Carta] -> Pal -> [Carta]
cartesPal ls p = [ CartaConcreta x y | CartaConcreta x y <- ls, p== y ]

-- donada una llista de cartes (en ordre de tirada), i el pal del trumfu, ens indiqui quin és el pal que està guanyant la basa
palGuanyadorBasa :: [Carta] -> Trumfu -> Pal
palGuanyadorBasa ls t = if null (cartesPal ls t) then palCarta (head ls) else t

-- que donada una llista de cartes (en ordre de tirada) i el pal del trumfu, ens retorni la carta guanyadora i la seva posició a la llista.
quiGuanya :: [Carta] -> Trumfu -> (Carta,Integer)
quiGuanya ls t = (cartaGuanyadora, head (posicioCarta cartaGuanyadora ls) ) 
    where
        cartaGuanyadora = maximum (cartesPal ls (palGuanyadorBasa ls t))
