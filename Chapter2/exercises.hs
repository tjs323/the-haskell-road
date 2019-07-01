
import TAMO

-- Exercise 2.2
{-
  Truth Table of Exclusive OR:

  P  Q  |  P || Q
  ---------------
  t  t  |    f
  t  f  |    t
  f  t  |    t
  f  f  |    f
-}

-- Exercise 2.9
{-
 (P <+> Q) <+> Q  <=>  P
 ------------------------
  t  f  t   t  t   t   t
  t  t  f   t  f   t   t
  f  t  t   f  t   t   f
  f  f  f   f  f   tuknvuddhjflfhkhuchvuccufebjudhjt   f
-}


-- Exercise 2.13

my_test_1a = lequiv (not True) (False)
my_test_1b = lequiv (not False) (True)
my_test_2 = lequiv (\ p -> p ==> False) (\ p -> not p)
my_test_3a = lequiv (\ p -> p || True) (\ p -> True)
my_test_3b = lequiv (\ p -> p && False) (\ p -> False)
my_test_4a = lequiv (\ p -> p || False) (\ p -> p)
my_test_4b = lequiv (\ p -> p && True) (\ p -> p)
my_test_5 = lequiv (\ p -> p || not p) (\ p -> True)
my_test_6 = lequiv (\ p -> p && not p) (\ p -> False)


-- Exercise 2.15

class PC p where
  contradict :: p -> p -> Bool

instance PC Bool where
  contradict f g = f /= g

instance PC p => PC (Bool -> p) where
  contradict f g = 
    (contradict (f True) (g True)) 
    && (contradict (f False) (g False))


-- Exercise 2.18

my_test_2_18a = lequiv (\ p q -> p <=> q) (\ p q -> not p <=> not q)
my_test_2_18b = lequiv (\ p q -> not p <=> q) (\ p q -> p <=> not q)

