-- z=7 
-- x=y^2 
-- waxOn =x*5 
-- y=z+8

waxOn = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2
triple x = x * 3
waxOff x = triple x
