exception End
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

module type Step =
  sig
    val dt : float
  end
  
module Configuration = 
struct
  let largFenetre = 600
  let longFenetre = 300
  let raquetteY = 10
  let largRaquette = 80
  let longRaquette = 20
  let largBrique = 50
  let longBrique = 20

  let rB = 5.
  let largRaquette = 80
  let longRaquette = 20
  let g = 9.81
  let boxx = (rB, float_of_int largFenetre-.rB)
  let boxy = (rB, float_of_int longFenetre-.rB)
  let box = (boxx, boxy)

  let augm = 10.
end

module Drawing (S : Step) =
  struct
    
    include Configuration

    let drawRaquette (rx,ry) = 
      set_color blue;
      let rx = fst((mouse_pos ())) in 
          if rx + largRaquette > largFenetre then 
            let rx = (largFenetre-largRaquette) in 
            draw_rect rx raquetteY largRaquette longRaquette;
            fill_rect rx raquetteY largRaquette longRaquette;
          else if rx < 0 then 
            let rx = 0 in 
            draw_rect rx raquetteY largRaquette longRaquette;
            fill_rect rx raquetteY largRaquette longRaquette;
          else 
            draw_rect rx raquetteY largRaquette longRaquette;
            fill_rect rx raquetteY largRaquette longRaquette;
    ;;

    let rec drawBriques lRect =
      match lRect with 
      | [] -> ()
      | (rx,ry,count,_,score)::q -> if count > 0 then 
                  let color = 
                  (match count with 
                  | 1 -> yellow
                  | 2 -> green
                  | 3 -> red
                  | _ -> raise End)
                  in
                  draw_rect (int_of_float rx) (int_of_float ry) largBrique longBrique; set_color color; fill_rect (int_of_float rx) (int_of_float ry) largBrique longBrique;
        drawBriques q;;
    
    let drawScore score = 
      moveto 10 10;
      set_color black;
      set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
      draw_string ("score: "  ^string_of_int score)
      ;;
    
    let drawTentatives tentatives =
      moveto (largFenetre-50) (longFenetre-50);
      
      set_color red;
      draw_circle (largFenetre-49) (longFenetre-45) 10;
      fill_circle (largFenetre-49) (longFenetre-45) 10;

      set_color white;
      set_font "-*-fixed-medium-r-semicondensed--13-*-*-*-*-*-iso8859-1";
      draw_string  (string_of_int tentatives);
      ;;
      
      module Initialisation =
  struct
    let dt = 0.01
    let masse0 = 10.
    let position0 = (10., 40.)
    let vitesse0 = (70., 70.)
    let briques0 = [(200.,210.,true,yellow,10);(260.,210.,true,yellow,10);(200.,150.,true,yellow,10);(260.,150.,true,green,20);(100.,150.,true,red,50)]
    let score = 0
    let tentatives = 3
  end;;

      let handle_char c = match (Char.code c) with 
      | 27  ->  exit 0
      | _  -> clear_graph ()
      ;;

      let drawGameOver  size   =

          moveto (largFenetre/2-100) (longFenetre/2);
          set_color black;
          set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
          draw_string  "Game Over";
          moveto (largFenetre/2-75) (longFenetre/2-50);
          set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
          draw_string  "Press ESC to Exit";

          synchronize ();
          let s = Graphics.wait_next_event [Graphics.Key_pressed] 
          in if s.Graphics.keypressed then handle_char s.Graphics.key;
  
        ;;

      
      let drawWin size =
        moveto (largFenetre/2-50) (longFenetre/2);
        set_color black;
        set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
        draw_string  "Win";
      ;;
   
      

    let run r =
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
          | Some (((x, y), (dx, dy),briques, (score,tentatives)), r') ->
             begin
               (*Format.printf "r=(%f, %f); dr = (%f, %f)@." x y dx dy;*)
               Graphics.clear_graph ();


               set_color black;
               Graphics.draw_circle (int_of_float x) (int_of_float y) 5;
               fill_circle (int_of_float x) (int_of_float y) 5;

               drawBriques briques;

               let (rx,_) = mouse_pos () in 
               drawRaquette (rx,raquetteY);
              
               drawScore score;
              
               drawTentatives tentatives;
              
               if tentatives = 0 then
                 drawGameOver 0;
                    

               if List.length briques = 0 then drawWin 0;

               Graphics.synchronize ();
               (*ignore (read_line ());*)
               ref_r := r'
             end
        end in
      let handler_int i =
        begin
          ref_r := Flux.vide
        end in
      begin
        
        Graphics.auto_synchronize false;
        Sys.(ref_handler_alrm := signal sigalrm (Signal_handle handler_alrm));
        Sys.(ref_handler_int  := signal sigint  (Signal_handle handler_int));
        Unix.(setitimer ITIMER_REAL { it_interval = S.dt; it_value = S.dt })
      end    

      let positionRaquette = 
        Graphics.open_graph ((" "  ^ string_of_int largFenetre) ^ "x"  ^ (string_of_int longFenetre));
        let (x,_)  = mouse_pos () in
        Tick (lazy (Flux.uncons (
            Flux.constant (float_of_int x, float_of_int raquetteY)
      
        )));;
  end

module type Params =
  sig
    include Step
    val masse0 : float
    val position0 : float * float
    val vitesse0  : float * float
    val briques0 : (float*float*int*Graphics.color*int) list
    val score : int
    val tentatives : int
  end

module BallWithGravity (Init : Params) =
  struct
    
    let dt2 = (Init.dt, Init.dt);;

    let (|+|) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2);;
    let (|*|) (x1, y1) (x2, y2) = (x1 *. x2, y1 *. y2);;

    let integre dt init flux =
      let rec acc =
        Tick (lazy (Some (init, Flux.map2 (fun a f -> a |+| (dt2 |*| f)) acc flux)))
      in acc;;

    let g = 9.81;;
    (* r = r0 + Integ dr
       dr = dr0 + Integ ddr
       ddr = 0, -g
     *)

    let rec position =
      Tick (lazy (Flux.uncons (
      integre dt2 Init.position0 vitesse          
        )))
    and vitesse =
      Tick (lazy (Flux.uncons (
      integre dt2 Init.vitesse0 acceleration
        )))
    and acceleration =
      Tick (lazy (Flux.uncons (
      Flux.constant (0., 0.)
        )))
    ;;

    let briques = 
      Tick (lazy (Flux.uncons (
      Flux.constant Init.briques0
        )))
    ;;
    
    let score = 
      Tick (lazy (Flux.uncons (
      Flux.constant (Init.score,Init.tentatives)
        )))
    ;;
   
    module Draw = Drawing (Init);;
    let etat =  Flux.map2 (fun  score (position,vitesse,blocks)-> (position, vitesse, blocks,score)) (score)
                    (Flux.map2 (fun position (vitesse,blocks)-> (position, vitesse, blocks)) (position) 
                          (Flux.map2 (fun vitesse blocks -> (vitesse, blocks)) (vitesse) (briques)));;

  end

module BouncingBall =
  struct
    
    include Configuration

    let bornesRaquette (rx,ry) =   ((rx  -. rB ,rx+. float_of_int largRaquette +. rB ),(ry -. rB ,ry +. float_of_int longRaquette +. rB ));;  

    let rec bornesBrique (rx,ry,exist,_,_) =  ((rx-.rB,rx+. float_of_int largBrique+.rB),(ry-.rB,ry+. float_of_int longBrique+.rB));; 

    let rec bornesBriques briques = 
      let rec aux briques = 
        match briques with 
        | [] -> [];
        | (rx,ry,exist,_,points)::q -> if exist > 0 then ((rx-.rB,rx+.float_of_int largBrique+.rB),(ry-.rB,ry+.float_of_int longBrique+.rB))::(aux q)
                              else aux q
      in aux briques;;


    let contact_box_h (infx, supx) x dx = (x < infx && dx < 0.) || (x > supx && dx > 0.);;
    let contact_box_v (infy, supy) y dy = (y > supy && dy > 0.);;
    let contact_echec (infy, supy) y dy = (y < infy && dy < 0.);;

    let contact_rect_h ((infx,supx),(infy,supy)) (x,y) (dx,dy) m eps = 
      (dx > 0. && y<supy && y>infy && x+.m > infx && x+.m < supx  && x+.m -. infx <= eps)
      || (dx < 0. && y<supy && y>infy && x-.m > infx && x-.m < supx && supx-.x+.m <= eps);;
  
    let contact_rect_v ((infx,supx),(infy,supy)) (x,y) (dx,dy) m eps = 
      (dy > 0. && x<supx && x>infx && y+.m > infy && y+.m < supy && y+.m -.infy <= eps)
      || (dy < 0. && x<supx && x>infx && y-.m > infy && y-.m < supy && supy-.y+.m <= eps);;

    let contact_raquette ((infx,supx),(infy,supy)) (x,y) (dx,dy) m eps = 
      (dy < 0. && x<supx && x>infx && y-.m > infy && y-.m < supy && supy-.y+.m <= eps);;
    
    
    let rebondBox (boxx, boxy) ((x, y), (dx, dy)) =
        ((if contact_box_h boxx x dx then -. dx else dx),
         (if contact_box_v boxy y dy then -. dy else dy));;
  
    let rebondBrique (boxx, boxy) ((x,y),(dx,dy),briques) =
      let bornes = bornesBriques briques in
      (
        if List.fold_right (fun pair res -> res || (contact_rect_h pair (x,y) (dx,dy) 1. 2.)) bornes false then-.dx 
          else dx
          ),
        (
        if List.fold_right (fun pair res -> res || (contact_rect_v pair (x,y) (dx,dy) 1. 2.)) bornes false then-.dy 
          else dy)
        ;;


    let rebondRaquette  (boxx, boxy) ((x,y),(dx,dy)) = 
        let (rx,_)  = mouse_pos () in 
        (dx),
          (if contact_raquette (bornesRaquette (float_of_int rx,float_of_int raquetteY)) (x,y) (dx,dy) 1. 2. then -.dy
            else dy)
          ;;

    let contact ((x, y),(dx, dy)) = contact_box_h boxx x dx || contact_box_v boxy y dy;;

    let contactEchec ((x,y), (dx,dy)) =  contact_echec boxx y dy;;

    let contactBriques ((x,y),(dx,dy),briques) = 
      let bornes = bornesBriques briques in 
      List.fold_right (fun pair res -> res || (contact_rect_h pair (x,y) (dx,dy) 1. 2.)) bornes false
      || List.fold_right (fun pair res -> res || (contact_rect_v pair (x,y) (dx,dy) 1. 2.)) bornes false;;

    let contactRaquette ((x,y),(dx,dy)) = 
      let (rx,_) = mouse_pos () in 
      contact_raquette (bornesRaquette (float_of_int rx,float_of_int raquetteY)) (x,y) (dx,dy) 1. 2.;;
    
    let updateScore ((x,y),(dx,dy),briques) score =
      match briques with 
      | [] -> score
      | l::q ->  let rec aux l score = 
                    match l with 
                    | [] -> score 
                    | t::q -> if (contact_rect_h (bornesBrique t) (x,y) (dx,dy) 1. 2.) 
                              || (contact_rect_v (bornesBrique t) (x,y) (dx,dy) 1. 2.) then 
                                match t with 
                                | (x,y,existe,coleur,s) -> aux q (score+s)
                              else aux q score
                  in aux briques score;;
    ;;

    let updateBriques ((x,y),(dx,dy),briques) =
        match briques with 
        | [] -> briques
        | l::q ->  let rec aux l acc = 
                      match l with 
                      | [] -> acc 
                      | t::q -> if (contact_rect_h (bornesBrique t) (x,y) (dx,dy) 1. 2.) 
                                || (contact_rect_v (bornesBrique t) (x,y) (dx,dy) 1. 2.) then 
                                    match t with 
                                    | (x,y,1,couleur,points) -> aux q acc
                                    | (x,y,count,couleur,points) -> aux q ((x,y,(count-1),couleur,points)::acc)
                                else aux q (t::acc)
                    in aux briques [];;
      ;;
    

  let until flux contactBox contactBriques contactRaquette contactEchec f1 f2 f3 f4 f5 f6=
    Flux.unfold (fun (init, f) ->
        match Flux.uncons f with
              | None  -> None
              | Some (v, f') -> Some (v, 
                      match v with 
                      | ((x,y),(dx,dy),briques,(score,tentatives))->
                          if init && contactBox ((x,y),(dx,dy)) then (false,f1 v)
                          else if init &&  contactBriques ((x,y),(dx,dy),briques) then (false,f2 v)
                          else if init && contactRaquette ((x,y),(dx,dy)) then (false,f3 v)
                          else if init && contactEchec ((x,y),(dx,dy)) then
                                if tentatives > 1 then (false,f4 v)
                                else (false,f5 v)
                          else if init && List.length briques = 0 then (false,f6 v)
                          else (init,f'))    
    ) (true, flux);;


    let rec etat (module Init : Params) =
      let module BwG = BallWithGravity (Init) in
      Tick (lazy (Flux.uncons (
      until BwG.etat contact contactBriques contactRaquette contactEchec (fun ((x,y),(dx,dy),briques,(score,tentatives)) -> let module Restart =
                                              struct
                                                include Init
                                                let position0 = (x,y)
                                                let vitesse0 = rebondBox box ((x,y),(dx,dy))
                                              end in
                                            etat (module Restart))

                                            (fun ((x,y),(dx,dy),briques,(score,tentatives)) -> let module Restart =
                                              struct
                                                include Init
                                                let position0 = (x,y)
                                                let vitesse0 = let (dx,dy) = rebondBrique box ((x,y),(dx,dy),briques) in ( (if dx > 0. then dx+.augm else dx-.augm) , (if dy > 0. then dy+.augm else dy-.augm))
                                                let briques0 = updateBriques ((x,y),(dx,dy),briques)
                                                let score = updateScore ((x,y),(dx,dy),briques) score
                                              end in
                                            etat (module Restart))

                                            (fun ((x,y),(dx,dy),briques,(score,tentatives)) -> let module Restart =
                                              struct
                                                include Init
                                                let position0 = (x,y)
                                                let score = updateScore ((x,y),(dx,dy),briques) score
                                                let vitesse0 = rebondRaquette box ((x,y),(dx,dy))
                                              end in
                                            etat (module Restart))

                                            (fun ((x,y),(dx,dy),briques,(score,tentatives)) -> let module Restart =
                                              struct
                                                include Init
                                                let position0 = (10., 40.)
                                                let vitesse0 = (150., 200.)
                                                let tentatives = tentatives - 1
                                              end in
                                            etat (module Restart))

                                            (fun ((x,y),(dx,dy),briques,(score,tentatives)) -> let module Restart =
                                              struct
                                                include Init
                                                let position0 = (x,y)
                                                let vitesse0 = (0.,0.)
                                                let tentatives = 0
                                              end in
                                            etat (module Restart))

                                            (fun ((x,y),(dx,dy),briques,(score,tentatives)) -> let module Restart =
                                              struct
                                                include Init
                                                let position0 = (x,y)
                                                let vitesse0 = (0.,0.)
                                              end in
                                            etat (module Restart))
              )))
  end

module Initialisation =
  struct
    let dt = 0.01
    let masse0 = 10.
    let position0 = (10., 40.)
    let vitesse0 = (70., 70.)
    let briques0 = [(200.,210.,1,yellow,10);(260.,210.,1,yellow,10);(200.,150.,1,yellow,10);(260.,150.,2,green,20);(100.,150.,3,red,50)]
    let score = 0
    let tentatives = 1
  end



module Draw = Drawing (Initialisation)

let _  = Draw.run (BouncingBall.etat (module Initialisation));;



