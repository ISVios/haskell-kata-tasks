module  Stream.Internal where
--
--
data Stream a = a :> Stream a
--
infixr :>
--
--
