let rec leg i j =
    if i <= 2 || j <= 2 || i = j then ()
    else 
      let (p,q) = mult i in
      if p > 1 && q > 1 then (leg p j; leg q j)
      else if 

for i = 0 to 100 do
    for j = 0 to 100 do
        leg i j
    done
done;;