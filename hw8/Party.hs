module Party where

import Employee

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL empList fun) = GL (emp : empList) (fun + empFun emp)