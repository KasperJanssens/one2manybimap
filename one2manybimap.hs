#!/usr/bin/env stack
--stack runghc --resolver lts-7.14 --install-ghc --package bimap
import qualified Data.Map as M
import qualified Data.List as L

data One2ManyBimap one many = One2ManyBimap (M.Map one [many]) (M.Map many one)

findOne:: (Ord one) => one -> One2ManyBimap one many -> Maybe [many]
findOne one (One2ManyBimap oneManyMap manyOneMap) =
  M.lookup one oneManyMap

findMany::(Ord many) => many -> One2ManyBimap one many -> Maybe one
findMany many (One2ManyBimap oneManyMap manyOneMap) =
  M.lookup many manyOneMap

add:: (Ord one, Ord many) => one -> many -> One2ManyBimap one many -> One2ManyBimap one many
add one many (One2ManyBimap oneManyMap manyOneMap) =
  let manies = maybe [] id $  M.lookup one oneManyMap in
  let freshManies = manies ++ [many] in
  One2ManyBimap (M.insert one freshManies oneManyMap) (M.insert many one manyOneMap )

remove:: (Ord one, Ord many) => one -> many -> One2ManyBimap one many -> One2ManyBimap one many
remove one many (One2ManyBimap oneManyMap manyOneMap) =
  let freshManyOneMap = M.delete many manyOneMap in
  let freshManies = maybe [] (L.delete many) $  M.lookup one oneManyMap  in
  let freshOneManyMap = M.insert one freshManies oneManyMap in
  One2ManyBimap freshOneManyMap freshManyOneMap
  
move:: (Ord one, Ord many) => one -> one -> many -> One2ManyBimap one many -> One2ManyBimap one many
move oneFrom oneTo many bimap = add oneTo many $ remove oneFrom many bimap

data Gameboard space player item = Gameboard (One2ManyBimap space player) (One2ManyBimap space item)

placePlayer:: (Ord space, Ord player) => space -> player -> Gameboard space player item -> Gameboard space player item
placePlayer space player (Gameboard spacePlayerMap spaceItemMap) =
  Gameboard (add space player spacePlayerMap) spaceItemMap

movePlayer:: (Ord space, Ord player) =>  space -> space -> player -> Gameboard space player item -> Gameboard space player item
movePlayer from to player (Gameboard spacePlayerMap spaceItemMap) =
  Gameboard (move from to player spacePlayerMap) spaceItemMap

findPlayer:: Ord player => player -> Gameboard space player item -> Maybe space
findPlayer player (Gameboard spacePlayerMap spaceItemMap) =
  findMany player spacePlayerMap

getPlayers:: Ord space => space -> Gameboard space player item -> [player]
getPlayers space (Gameboard spacePlayerMap spaceItemMap) =
  maybe [] id $ findOne space spacePlayerMap


main = print "koekoek"
