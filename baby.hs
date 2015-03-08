app :: Maybe (a -> b) -> Maybe a -> Maybe b
app (Just f) (Just x) = Just (f x)
app _ _ = Nothing