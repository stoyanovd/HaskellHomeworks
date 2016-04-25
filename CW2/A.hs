module CW2.A where

-- Реализуйте  mapMaybe :: (a -> Maybe b) -> [a] -> [b]
-- которая по сути может выбрасывать элементы из списка.

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe g = ( ((maybe [] return) . g) =<<)


-- find maybeToList =)

-- for test

f :: Integer -> Maybe Integer
f x
    | even x = Just x
    | otherwise = Nothing


e = [2, 3, 4, 5]

w = mapMaybe f e
