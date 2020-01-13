 module DebugInput where

type Stack = [Integer]
type Code  = [Op]

data Op    = PUSH Integer | ADD
data List a = Nil | Cons a (List a)
data Two a b = Two a b | One
data Maybe a = Nothing | Just a


test x         One       = x
test One       x         = x
test (Two x y) (Two a b) = Two (a+x) (b+y)

execNo :: Stack -> Code -> Stack
execNo s        []             = s
execNo s        (PUSH n : ops) = execNo (n : s) ops
execNo (n : x)  (ADD    : ops) = case x of
                                  (m : s') -> execNo (n + m : s') ops
