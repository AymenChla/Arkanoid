open Graphics

module type SimpleIter =
sig
  type 'a t
  val vide : 'a t
  val cons : 'a -> 'a t -> 'a t
  val unfold : ('s -> ('a * 's) option) -> 's -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val append : 'a t -> 'a t -> 'a t                        
  val constant : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type Iter =
sig
  include SimpleIter
  val uncons : 'a t -> ('a * 'a t) option
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end

type 'a flux = Tick of ('a * 'a flux) option Lazy.t;;
module Flux : Iter with type 'a t = 'a flux =
  struct
    type 'a t = 'a flux = Tick of ('a * 'a t) option Lazy.t;;

    let vide = Tick (lazy None);;

    let cons t q = Tick (lazy (Some (t, q)));;

    let uncons (Tick flux) = Lazy.force flux;;
 
    let rec apply f x =
      Tick (lazy (
      match uncons f, uncons x with
      | None         , _             -> None
      | _            , None          -> None
      | Some (tf, qf), Some (tx, qx) -> Some (tf tx, apply qf qx)));;

    let rec unfold f e =
      Tick (lazy (
      match f e with
      | None         -> None
      | Some (t, e') -> Some (t, unfold f e')));;

    let rec filter p flux =
      Tick (lazy (
      match uncons flux with
      | None        -> None
      | Some (t, q) -> if p t then Some (t, filter p q)
                       else uncons (filter p q)));;
    
    let rec append flux1 flux2 =
      Tick (lazy (
      match uncons flux1 with
      | None          -> uncons flux2
      | Some (t1, q1) -> Some (t1, append q1 flux2)));;
    
    let constant c = unfold (fun () -> Some (c, ())) ();;
    (* implantation rapide mais inefficace de map *)
    let map f i = apply (constant f) i;;

    let map2 f i1 i2 = apply (apply (constant f) i1) i2;;
end

module PullIter : Iter with type 'a t = unit -> 'a option =
struct
  type 'a t = unit -> 'a option;;

  let vide = fun () -> None;;

  let cons t q =
    let first = ref true in
    fun () -> if !first then (first := false; Some t) else q ();;

  let uncons i =
    match i () with
    | None   -> None
    | Some v -> Some (v, i);;

  let apply f a = fun () ->
    match f (), a () with
    | None  , _      -> None
    | _     , None   -> None
    | Some f, Some a -> Some (f a);;

  let unfold f s =
    let s = ref s in
    fun () ->
    match f !s with
    | None         -> None
    | Some (v, s') -> (s := s'; Some v);;

  let rec filter p i = fun () ->
    match i () with
    | None   -> None
    | Some v -> if p v then Some v else filter p i ();;

  let append flux1 flux2 =
    fun () -> match flux1 () with
              | None -> flux2 ()
              | Some v -> Some v;;

  let constant c = unfold (fun () -> Some (c, ())) ();;
  (* implantation rapide mais inefficace de map *)
  let map f i = apply (constant f) i;;

  let map2 f i1 i2 = apply (apply (constant f) i1) i2;;
end
  
module PushIter : SimpleIter with type 'a t = ('a -> unit) -> unit =
struct
  type 'a t = ('a -> unit) -> unit;;

  let vide = fun k -> ();;

  let cons t q = fun k -> (k t; q k);;

  let rec unfold f s = fun k ->
    match f s with
    | None         -> ()
    | Some (v, s') -> (k v; unfold f s' k);;

  let filter p i = fun k -> i (fun v -> if p v then k v);;

  let append f1 f2 = fun k -> (f1 k; f2 k);;

  let constant c = unfold (fun () -> Some (c, ())) ();;

  let map f i = fun k -> i (fun v -> k (f v));;
end

module BouncingBall (Init : sig val dt : float val m0 : float val x0 : float val y0 : float val dx0 : float val dy0 : float val mx0 : float val my0 : float end) =
  struct
    let dt2 = (Init.dt, Init.dt);;

    let id = Flux.constant (fun x -> x);;
    let mid = Graphics.open_graph " 400x300";
        Graphics.auto_synchronize false; 
        Flux.constant (fun (_,_) -> let (x,y) = (mouse_pos ()) in (float_of_int x , float_of_int y));;

    let (|+|) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2);;
    let (|*|) (x1, y1) (x2, y2) = (x1 *. x2, y1 *. y2);;

    let integre dt init tr flux =
      let rec acc =
        Tick (lazy (Some (init, Flux.apply tr (Flux.map2 (fun a f -> a |+| (dt2 |*| f)) acc flux))))
      in acc;;
    
    let rB = 5.;;
    let largR = 50.;;
    let longR = 20.;;
    let g = 9.81;;
    let boxx = (5., 395.);;
    let boxy = (5., 295.);;
    let box = (boxx, boxy);;


    let rec rectxy1 (rx,ry,exist,_) =  ((rx-.rB,rx+.largR+.rB),(ry-.rB,ry+.longR+.rB));;
                          
    
    let rec rectxy flRect = 
      match Flux.uncons flRect with 
      | None -> []
      | Some(lRect,_) -> let rec aux lRect = 
                            match lRect with 
                            | [] -> [];
                            | (rx,ry,exist,_)::q -> if exist then ((rx-.rB,rx+.largR+.rB),(ry-.rB,ry+.longR+.rB))::(aux q)
                                                  else aux q
                          in aux lRect;;
      
    let raquette (mx,my) =  ((mx  -. rB ,mx+.80.+. rB ),(10. -. rB ,10. +. 20.+. rB ));;  

    (*let rec findAndSet listrect paire*)
      

    
    (* r = r0 + Integ dr
       dr = dr0 + Integ ddr
       ddr = 0, -g
     *)
    let contact (infx, supx) x dx = (x < infx && dx < 0.) || (x > supx && dx > 0.);;
    let contact_rect_h ((infx,supx),(infy,supy)) (x,y) (dx,dy) m eps = 
        (dx > 0. && y<supy && y>infy && x+.m > infx && x+.m < supx  && x+.m -. infx <= eps)
        || (dx < 0. && y<supy && y>infy && x-.m > infx && x-.m < supx && supx-.x+.m <= eps);;
    
    let contact_rect_v ((infx,supx),(infy,supy)) (x,y) (dx,dy) m eps = 
      (dy > 0. && x<supx && x>infx && y+.m > infy && y+.m < supy && y+.m -.infy <= eps)
      || (dy < 0. && x<supx && x>infx && y-.m > infy && y-.m < supy && supy-.y+.m <= eps);;
    
    let contact_raquette ((infx,supx),(infy,supy)) (x,y) (dx,dy) m eps = 
      (dy < 0. && x<supx && x>infx && y-.m > infy && y-.m < supy && supy-.y+.m <= eps);;
    
    
    let rebond  (boxx, boxy) rect (mx,my) (x, y)  (dx, dy) =
      
      (if (contact boxx x dx)  then -. dx 
        else 
            if List.fold_right (fun pair res -> res || (contact_rect_h pair (x,y) (dx,dy) 1. 2.)) rect false then-.dx 
            else dx
          ),
       (if (contact boxy y dy) then -.dy 
          else 
            if List.fold_right (fun pair res -> res || (contact_rect_v pair (x,y) (dx,dy) 1. 2.)) rect false then-.dy 
            else if contact_raquette (raquette (mx,my)) (x,y) (dx,dy) 1. 2. then -.dy
            else dy)
        ;;

    
    let rec positionMouse = 
      Tick (lazy (Flux.uncons (
      integre dt2 (Init.mx0, Init.my0) mid (Flux.constant (0.,0.))         
        )));;
        
    let rec position flRect =
      Tick (lazy (Flux.uncons (
      integre dt2 (Init.x0, Init.y0) id (vitesse flRect)         
        )))
    and vitesse flRect =
      Tick (lazy (Flux.uncons (
      integre dt2 (Init.dx0, Init.dy0) (Flux.map2 (rebond box (rectxy flRect)) (positionMouse) (position flRect) ) acceleration
        )))
    and acceleration =
      Tick (lazy (Flux.uncons (
      Flux.constant (0., 0.)
        )))
    ;;

    

    let detecterCollision lRect (x,y) (dx,dy) = 
        match lRect with 
        | [] -> lRect
        | l::q ->  let rec aux l acc = 
                      match l with 
                      | [] -> acc 
                      | t::q -> if (contact_rect_h (rectxy1 t) (x,y) (dx,dy) 1. 2.) 
                                || (contact_rect_v (rectxy1 t) (x,y) (dx,dy) 1. 2.) then 
                                aux q acc 
                                else aux q (t::acc)
                    in aux lRect [];;

    let id2 = Flux.constant (fun x -> x);;
    let majBlocks flRect (x,y) (dx,dy) =
         
          match Flux.uncons flRect with 
          | None -> Flux.vide
          | Some(lRect,_) ->  let rec aux  = 
                              let flux = Flux.constant (detecterCollision lRect (x,y) (dx,dy)) in 
                                Tick (lazy (Some 
                                (
                                  lRect, Flux.apply id2 (Flux.map2 (fun a f -> f) aux flux
                                ))))
                              in aux;;


    let blocks flRect =
        match Flux.uncons (position flRect), Flux.uncons (vitesse flRect) with 
        | None , _ 
        | _ , None -> Flux.vide
        | Some((x,y),_), Some((dx,dy),_) -> Tick (lazy (Flux.uncons ( majBlocks flRect (x,y) (dx,dy) )))

    ;;

    let balle flRect = 
                Flux.map2 (fun positionMouse (position,vitesse,blocks) -> (position,vitesse,blocks,positionMouse)) (positionMouse)
                       (Flux.map2 (fun position (vitesse,blocks)-> (position, vitesse, blocks)) (position flRect) 
                                 (Flux.map2 (fun vitesse blocks -> (vitesse, blocks)) (vitesse flRect) (blocks flRect)));;
    


    let rec drawRects lRect =
      match lRect with 
      | [] -> ()
      | (rx,ry,exist,color)::q -> if exist then draw_rect (int_of_float rx) (int_of_float ry) 50 20; set_color color; fill_rect (int_of_float rx) (int_of_float ry) 50 20;
                            drawRects q;;

    
    

    let draw r =
      let ref_r = ref r in
      let ref_handler_alrm = ref Sys.(Signal_handle (fun _ -> ())) in
      let ref_handler_int  = ref Sys.(Signal_handle (fun _ -> ())) in
      let handler_alrm i =
        begin
          match Flux.uncons !ref_r with
          | None                          ->
             begin
               Sys.(set_signal sigalrm !ref_handler_alrm);
               Sys.(set_signal sigint  !ref_handler_int)
             end
          | Some (((x, y), (dx, dy) , lRect, (mx,my)), r') ->
             begin

              

               (*Format.printf "r=(%f, %f); dr = (%f, %f)@." x y dx dy;*)
               clear_graph (); 
               set_color black;
               draw_circle (int_of_float x) (int_of_float y) 5;
               fill_circle (int_of_float x) (int_of_float y) 5;
               drawRects lRect;
              
               
            
               set_color blue;
              let x = fst((mouse_pos ())) in 
                  if x + 80 > 400 then 
                    let x = (400-80) in 
                    draw_rect x 10 80 20;
                    fill_rect x 10 80 20;
                  else if x < 0 then 
                    let x = 0 in 
                    draw_rect x 10 80 20;
                    fill_rect x 10 80 20;
                  else 
                    draw_rect x 10 80 20;
                    fill_rect x 10 80 20;
                    
                      
              

               synchronize ();
               (*ignore (read_line ());*)
               ref_r := r'
             end
        end in
      let handler_int i =
        begin
          ref_r := Flux.vide
        end in
      begin
        
        
        Sys.(ref_handler_alrm := signal sigalrm (Signal_handle handler_alrm));
        Sys.(ref_handler_int  := signal sigint  (Signal_handle handler_int));
        Unix.(setitimer ITIMER_REAL { it_interval = Init.dt; it_value = Init.dt });
      end;;
  end



module BB = BouncingBall (struct let dt = 0.01 let m0 = 10. let x0 = 10. let y0 = 50. let dx0 = 250. let dy0 = 150. let mx0 = 0. let my0 = 0. end);;
let flRect = 
Tick (lazy (Flux.uncons (
                            Flux.constant [(200.,210.,true,yellow);(260.,210.,true,yellow);(200.,150.,true,yellow);(260.,150.,true,green);(100.,150.,true,red)]
              ))) in BB.(draw (balle flRect));;




module Test (F : SimpleIter) =
  struct
    let count n =
      F.unfold (fun i -> if i > n then None else Some (i, i+1)) 1;;
    
    let main consume =
      let t0 = Sys.time () in
      begin
        let nb = consume (F.map (fun x -> x) (F.append (count 5000000) (count 5000000))) in
        let t1 = Sys.time () in
        Format.printf "#elements=%d, time=%f@." nb (t1 -. t0)
        end;;
  end

let _ =
  Format.printf "modèle Flux    : ";
  let module T = Test (Flux) in
  let rec consume n f =
      match Flux.uncons f with
      | None        -> n
      | Some (_, q) -> consume (n+1) q in
  T.main (consume 0);;

let _ =
  Format.printf "modèle PullIter: ";
  let module T = Test (PullIter) in
  let rec consume n f =
      match PullIter.uncons f with
      | None        -> n
      | Some (_, q) -> consume (n+1) q in
  T.main (consume 0);;

let _ =
  Format.printf "modèle PushIter: ";
  let module T = Test (PushIter) in
  let consume f = let r = ref 0 in f (fun _ -> incr r); !r in
  T.main consume;;
