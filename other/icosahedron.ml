open Graphics;;
open List;;
let (>>) x f = f x;;

let rotz_matrix a = [[cos a; -.sin a; 0.]; [sin a; cos a; 0.]; [0.;0.;1.]];;
let roty_matrix a = [[cos a; 0.; -.sin a]; [0.;1.;0.]; [sin a; 0.; cos a]];;
let rotx_matrix a = [[1.;0.;0.]; [0.; cos a; -.sin a]; [0.; sin a; cos a]];;

let mul_mv m v = map (fun a -> fold_left (+.) 0. (map2 ( *.) a v)) m;;

let rotate x y z v = v >> 
  mul_mv (rotx_matrix x) >> mul_mv (roty_matrix y) >> mul_mv (rotz_matrix z);;

let ha = sqrt (3.) /. 2.;;
let hb = 
  let p = (2. *. ha +. 1.) /. 2. in
  sqrt (p *. (p -. ha) *. (p -. ha) *. (p -. 1.)) /. ha *. 2.;;

let sa = sqrt (ha *. ha -. hb *. hb);;
let sb = sa *. (ha -. sa) /. hb;;

let vertices = [[1.1; 1.44; 0.57];[1.25; 1.44; 0.];[1.5; 1.44; 0.77];[1.75; 1.44; 0.];[1.9; 1.44; 0.57];
                [1.5; 1.66; 0.38];
                [1.1; 1.; 0.29];[1.25; 1.; 0.77];[1.5; 1.; 0.];[1.75; 1.; 0.77];[1.9; 1.; 0.29];
                [1.5; 0.78; 0.38]];;
let faces = [[1;5;3];
             [3;5;4];
             [4;5;2];
             [2;5;0];
             [0;5;1];
             [1;3;8];
             [8;3;10];
             [3;4;10];
             [10;4;9];
             [4;2;9];
             [9;2;7];
             [2;0;7];
             [7;0;6];
             [0;1;6];
             [6;1;8];
             [6;8;11];
             [8;10;11];
             [10;9;11];
             [9;7;11];
             [7;6;11]];;
(*
let vertices = [[0.5; sb; sa]; [-.0.5; sb; sa]; [0.; sb -.hb; 0.]; [0.; sb; sa-.ha]];;
let faces = [[0;1;2];[0;1;3];[0;2;3];[1;2;3]];;
*)
let coord_op f v = match v with
  x::y::_ -> f (int_of_float (x *. 200.) + 300) (int_of_float (y *. 200.) + 300)
| _ -> ();;
let line (b,e) = coord_op moveto b; coord_op lineto e;;

let partition_invisible vert l = 
  let getz l = nth l 2 in
  match fold_left (fun a v -> if getz v < 0. then a+1 else a) 0 vert with
    1 -> partition (fun (a,b) -> getz a < 0. || getz b < 0.) l
  | 2 -> partition (fun (a,b) -> getz a < 0. && getz b < 0.) l
  | _ -> ([], l)
  | _ -> failwith "Impossible";;

let conv f v = f >> map (fun face ->
    let face = map (nth v) face in
    map2 (fun a b -> (a,b)) face (tl face @ [hd face])
  ) >> flatten;;

let draw verts (color_inv, color_vis) =
  let (inv, vis) = conv faces verts >> partition_invisible verts in
  set_color color_inv; iter line inv;
  set_color color_vis; iter line vis;;

let rec uniq l = match l with
  l1::l2::ls when l1=l2 -> uniq (l2::ls)
 |l1::ls -> l1::uniq ls
 |[] -> [];;

let p_line (b1::b2::_,e1::e2::_) = 
  Printf.printf "\\draw (%f,%f) -- (%f,%f);\n" b1 b2 e1 e2;;

open_graph "";;
let rec main_loop x y z =
  if key_pressed () then (
    read_key ();

    let verts = map (rotate x y z) vertices in    
    let (inv,vis) = conv faces verts >> partition_invisible verts in
    let w = List.sort compare (List.map (fun (a,b) -> (min a b, max a b)) (inv @ vis)) >> uniq in
(*
    print_string "inv\n";
    iter p_line inv;
*)
    print_string "vis\n";
    iter p_line w
  )
  else (
    let verts = map (rotate x y z) vertices in
    draw verts (rgb 200 200 200, black);
    for i = 0 to 10000000 do () done;
    draw verts (white, white);
    main_loop (x +. 0.001) (y +. 0.002) (z +. 0.003)
  );;
main_loop 0. 0. 0.;;
close_graph ();;
