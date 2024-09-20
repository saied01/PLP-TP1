module Proceso (Procesador, AT(Nil,Tern), RoseTree(Rose), Trie(TrieNodo), foldAT, foldRose, foldTrie, procVacio, procId, procCola, procHijosRose, procHijosAT, procRaizTrie, procSubTries, unoxuno, sufijos, inorder, preorder, postorder, preorderRose, hojasRose, ramasRose, caminos, palabras, ifProc,(++!), (.!)) where
import Control.Applicative (Alternative(empty))

import Test.HUnit

-- cd TP1
-- runghc TP1.hs

--Definiciones de tipos

type Procesador a b = a -> [b]


-- Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving Eq
--E.g., at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
--Es es árbol ternario con 1 en la raíz, y con sus tres hijos 2, 3 y 4.

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving Eq
--E.g., rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []] 
--es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
-- E.g., t = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]
-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, b tiene como hijo a d.



-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show = showRoseTree 0
      where
        showRoseTree :: Show a => Int -> RoseTree a -> String
        showRoseTree indent (Rose value children) =
            replicate indent ' ' ++ show value ++ "\n" ++
            concatMap (showRoseTree (indent + 2)) children

instance Show a => Show (AT a) where
    show = showAT 0
      where
        showAT :: Show a => Int -> AT a -> String
        showAT _ Nil = replicate 2 ' ' ++ "Nil"
        showAT indent (Tern value left middle right) =
            replicate indent ' ' ++ show value ++ "\n" ++
            showSubtree (indent + 2) left ++
            showSubtree (indent + 2) middle ++
            showSubtree (indent + 2) right
        
        showSubtree :: Show a => Int -> AT a -> String
        showSubtree indent subtree =
            case subtree of
                Nil -> replicate indent ' ' ++ "Nil\n"
                _   -> showAT indent subtree

instance Show a => Show (Trie a) where
    show = showTrie ""
      where 
        showTrie :: Show a => String -> Trie a -> String
        showTrie indent (TrieNodo maybeValue children) =
            let valueLine = case maybeValue of
                                Nothing -> indent ++ "<vacío>\n"
                                Just v  -> indent ++ "Valor: " ++ show v ++ "\n"
                childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
            in valueLine ++ childrenLines


--Ejercicio 1
procVacio :: Procesador a b
procVacio = (\_ -> [])

procId :: Procesador [a] a
procId = id


procCola :: Procesador [a] a
procCola = (\x -> if null x then [] else tail x)


rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []]
at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
t = TrieNodo (Nothing) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]

procHijosRose :: Procesador (RoseTree a) (RoseTree a)
procHijosRose (Rose _ hijos) = hijos

procHijosAT :: Procesador (AT a) (AT a)
procHijosAT Nil = []
procHijosAT (Tern _ hijo1 hijo2 hijo3) = [hijo1, hijo2, hijo3]

procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie (TrieNodo raiz _) = [raiz]

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries (TrieNodo _ sub) = sub


--Ejercicio 2


foldAT :: (a -> b -> b -> b -> b) -> b -> AT a -> b
foldAT _ z Nil = z
foldAT f z (Tern x i m d) = f x (foldAT f z i) (foldAT f z m) (foldAT f z d)



foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose x hijos) = f x (map ac hijos)
  where ac = foldRose f


foldTrie :: (Maybe a -> [(Char, b)] -> b) -> Trie a -> b
foldTrie f (TrieNodo valor hijos) = f valor (map (\(c, hijo) -> (c, foldTrie f hijo)) hijos)


--Ejercicio 3
unoxuno :: Procesador [a] [a]
unoxuno = foldr (\x ac -> [x] : ac) []


sufijos :: Procesador [a] [a]
sufijos [] = [[]]
sufijos (x:xs) = (x:xs) : sufijos xs


--Ejercicio 4


preorder :: AT a -> [a]
preorder = foldAT (\x izq med der -> [x] ++ izq ++ med ++ der) []

postorder :: AT a -> [a]
postorder Nil = []
postorder (Tern value left middle right) = postorder left ++ postorder middle ++ postorder right ++ [value]

inorder :: AT a -> [a]
inorder Nil = []
inorder (Tern value left middle right) = inorder left ++ inorder middle ++ [value] ++ inorder right


--Ejercicio 5

preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose (\n rec -> if null rec then [n] else n : concat rec)


hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose (\n rec -> if null rec then [n] else concat rec)


ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRose (\n rec -> if null rec then [[n]] else map (n:) (concat rec))


--Ejercicio 6


caminos :: Trie a -> [String]
caminos = foldTrie (\_ hijos -> "" : [v : hijo | (v, hijos') <- hijos, hijo <- hijos'])


--Ejercicio 7

palabras :: Trie a -> [String]
palabras = foldTrie construirCaminos
  where
    construirCaminos :: Maybe a -> [(Char, [String])] -> [String]
    construirCaminos valor hijos =
      let caminosHijos = concatMap (\(v, hijo) -> map (v :) hijo) hijos
      in case valor of
           Just _  -> "" : caminosHijos
           Nothing -> caminosHijos


--Ejercicio 8
-- 8.a)
ifProc :: (a->Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc = \f p1 p2 x -> if f x then p1 x else p2 x

-- 8.b)
(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) = (\f1 f2 -> \x -> f1 x ++ f2 x) 

-- 8.c)
(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) p1 p2 = \x -> concatMap p1 (p2 x)

--Ejercicio 9
-- Se recomienda poner la demostración en un documento aparte, por claridad y prolijidad, y, preferentemente, en algún formato de Markup o Latex, de forma de que su lectura no sea complicada.


{-Tests-}
main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8a" ~: testsEj8a,
  "ejercicio8b" ~: testsEj8b,
  "ejercicio8c" ~: testsEj8c
  ]


testsEj1 = TestList [
  "procVacio" ~: ([] :: [Int]) ~=? procVacio (42 :: Int),
  "procId lista vacía" ~: ([] :: [Int]) ~=? procId ([] :: [Int]),
  "procId lista no vacía" ~: ([1,2,3] :: [Int]) ~=? procId ([1,2,3] :: [Int]),
  "procCola lista vacía" ~: ([] :: [Int]) ~=? procCola ([] :: [Int]),
  "procCola lista no vacía" ~: ([2,3] :: [Int]) ~=? procCola ([1,2,3] :: [Int]),
  "procHijosRose sin hijos" ~: ([] :: [RoseTree Int]) ~=? procHijosRose (Rose (1 :: Int) []),
  "procHijosRose con hijos" ~: ([Rose (2::Int) [], Rose (3::Int) []] :: [RoseTree Int]) ~=? procHijosRose (Rose (1 :: Int) [Rose 2 [], Rose 3 []]),
  "procHijosAT Nil" ~: ([] :: [AT Int]) ~=? procHijosAT (Nil :: AT Int),
  "procHijosAT no Nil" ~: ([Nil, Nil, Nil] :: [AT Int]) ~=? procHijosAT (Tern (1 :: Int) Nil Nil Nil),
  "procRaizTrie con valor" ~: ([Just 'a'] :: [Maybe Char]) ~=? procRaizTrie (TrieNodo (Just 'a') []),
  "procRaizTrie sin valor" ~: ([Nothing] :: [Maybe Char]) ~=? procRaizTrie (TrieNodo Nothing []),
  "procSubTries sin subárboles" ~: ([] :: [(Char, Trie Char)]) ~=? procSubTries (TrieNodo (Nothing :: Maybe Char) []),
  "procSubTries con subárboles" ~: ([('a', TrieNodo Nothing []), ('b', TrieNodo Nothing [])] :: [(Char, Trie Char)]) ~=? 
                                     procSubTries (TrieNodo (Nothing :: Maybe Char) [('a', TrieNodo Nothing []), ('b', TrieNodo Nothing [])])
  ]


-- Combinación de todos los casos de prueba
testsEj2 = TestList [
  "foldAT suma" ~: foldAT (\x i m d -> x + i + m + d) 0 at ~?= 10,
  "foldAT producto" ~: foldAT (\x i m d -> x * i * m * d) 1 at ~?= 24,
  
  "foldRose suma" ~: foldRose (\x hijos -> x + sum hijos) rt ~?= 15,
  "foldRose cuenta nodos" ~: foldRose (\_ hijos -> 1 + sum hijos) rt ~?= 5,
  
  "foldTrie cuenta nodos" ~: foldTrie (\_ hijos -> 1 + sum (map snd hijos)) t ~?= 6,
  "foldTrie valores Just" ~: foldTrie (\valor hijos -> maybe 0 (const 1) valor + sum (map snd hijos)) t ~?= 3 
  ]

-- Definición de tests para `unoxuno` y `sufijos`
testsEj3 = test [
    "unoxuno abc" ~: unoxuno "abc" ~?= ["a", "b", "c"],
    "unoxuno 123" ~: unoxuno [1, 2, 3] ~?= [[1], [2], [3]],
    "unoxuno vacio" ~: unoxuno ([] :: [Int]) ~?= [],

    "sufijos abc" ~: sufijos "abc" ~?= ["abc", "bc", "c", ""],
    "sufijos 123" ~: sufijos [1, 2, 3] ~?= [[1, 2, 3], [2, 3], [3], []],
    "sufijos vacio" ~: sufijos ([] :: [Int]) ~?= [[]] 
  ]


testsEj4 = test [ -- Casos de test para el ejercicio 4
  ""       -- Caso de test 1 - expresión a testear
    ~=? ""                             -- Caso de test 1 - resultado esperado
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  0       -- Caso de test 1 - expresión a testear
    ~=? 0                                       -- Caso de test 1 - resultado esperado
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  False       -- Caso de test 1 - expresión a testear
    ~=? False                                            -- Caso de test 1 - resultado esperado
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]

testsEj8a = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]
testsEj8b = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]
testsEj8c = test [ -- Casos de test para el ejercicio 7
  True         -- Caso de test 1 - expresión a testear
    ~=? True                                          -- Caso de test 1 - resultado esperado
  ]

