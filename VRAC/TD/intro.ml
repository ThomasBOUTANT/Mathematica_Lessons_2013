let moyenne3 x y z = (x+.y+.z)/.3.;;
let hypotenuse x y = sqrt(x*.x+.y*.y);;
let divise a b = a*(b/a) = b;;
divise 10 3;;
divise 9 3;;
divise 5 15;;
let entier a = float_of_int (int_of_float a) = a;;
entier 12.5;;
entier 19.0000000;;

let factorielle x = 
let a = ref 1 in
for i=1 to x do a:=i * !a done;
print_int !a ;;


print_string "puissance";
let puissance x n = 
let a = ref 1. in
for i=1 to n do a:=!a*.x done;
print_int !a;;

