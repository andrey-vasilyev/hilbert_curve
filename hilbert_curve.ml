open Graphics;;

(* The curve will be discribed in terms of vectors
    D - down
    U - up 
    L - left
    R - right *)
type vector = L | R | U | D

(* Direction of rotation *)
type direction = CW | CCW

let win_size = 800;;
open_graph (" " ^ string_of_int win_size ^ "x" ^ string_of_int win_size);;

(* Mirroring a curve [D; R; U] -> [D; L; U] *)
let mirror lst =
    List.rev (List.map (fun el -> match el with
                                  | L -> R
                                  | R -> L
                                  | U -> D
                                  | D -> U) lst)

(* Rotating a curve 90 degrees CW or CCW *)
let rotate lst dir =
    match dir with
    | CCW -> List.map (fun el ->
                            match el with
                            | L -> D
                            | R -> U
                            | U -> L
                            | D -> R) lst
    | CW  -> List.map (fun el ->
                            match el with
                            | L -> U
                            | R -> D
                            | U -> R
                            | D -> L) lst

(* Calculating the cure as a list of vectors*)
let rec calc_hilbert n =
    match n with
    | 1 -> [D; R; U]
    | k -> List.append (rotate (mirror (calc_hilbert (k-1))) CCW)
                        (D :: (List.append (calc_hilbert (k-1))
                          (R ::(List.append (calc_hilbert (k-1))
                            (U :: (rotate (mirror (calc_hilbert (k-1))) CW)
                            ))
                          ))
                        )

(* Calculating a starting point for drawing *)
let start_point n =
    let x = (float_of_int win_size /. (2. ** float_of_int (n+1))) in
    (x, float_of_int win_size -. x);;

(* Calculating vector length *)
let vector_length n =
    let (x, y) = start_point n in
    x *. 2.;;

(* Drawing a cure based on the list of vectors *)
let draw_hilbert n vecs =
    let (x, y) = start_point n in
        moveto (int_of_float x) (int_of_float y);
    List.iter (fun el -> match el with
                        | R -> rlineto  (int_of_float (vector_length n)) 0
                        | L -> rlineto  (int_of_float (-.(vector_length n))) 0
                        | U -> rlineto 0  (int_of_float (vector_length n))
                        | D -> rlineto 0  (int_of_float (-.(vector_length n)))) vecs;;

(* Key press handler *)
let rec loop n =
    match read_key () with
    | 'a' when n > 1  -> clear_graph ();
             draw_hilbert (n-1) (calc_hilbert (n-1));
             loop (n-1)
    | 'z' when n < 9 -> clear_graph ();
             draw_hilbert (n+1) (calc_hilbert (n+1));
             loop (n+1)
    | 'q' -> ()
    |  _  -> loop n;;

let k = 2;;

draw_hilbert k (calc_hilbert k);;

loop k
