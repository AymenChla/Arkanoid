
(* interfaces utiles pour toute la séance *)

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


(* Exercices 1, 2 et 3 *)

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

(* module de simuation paramétré par les condition initiales: masse, position, vitesse *)
module BouncingBall (Init : sig val m0 : float val x0 : float val y0 : float val dx0 : float val dy0 : float end) =
  struct
    (* fonction d'affichage du flux (position, vitesse) de la balle *)
    (* dans une fenetre rectangulaire 800*600                       *)
    let draw r =
      let rec loop r =
        match Flux.uncons r with
        | None                          -> ()
        | Some (((x, y), (dx, dy)), r') ->
           begin
             (*Format.printf "r=(%f, %f); dr = (%f, %f)@." x y dx dy;*)
             Graphics.clear_graph ();
             Graphics.draw_circle (int_of_float x) (int_of_float y) 5;
             (*ignore (read_line ());*)
             loop r'
           end
      in begin
          Graphics.open_graph " 800x600";
          loop r
        end;;

    (* à compléter *)
  end


(* test de la simulation, à compléter *)
module BB = BouncingBall (struct let m0 = 10. let x0 = 300. let y0 = 400. let dx0 = 15. let dy0 = 5. end);;


(* Exercices 4, 5 et 6 *)

(* module "stub" d'implantation par défaut *)
module Default =
  struct
    let unfold _ _ = assert false
    let filter _ _ = assert false
    let append _ _ = assert false
    let constant _ = assert false
    let map _ _ = assert false
    let uncons _ = assert false
    let apply _ _ = assert false
    let map2 _ _ = assert false
  end

module PullIter : Iter with type 'a t = unit -> 'a option =
struct
  type 'a t = unit -> 'a option;;

  let vide = fun () -> None;;

  let cons t q =
    let first = ref true in
    fun () -> if !first then (first := false; Some t) else q ();;

  (* à compléter en remplaçant le module Default *)
  include Default
end
  
module PushIter : SimpleIter with type 'a t = ('a -> unit) -> unit =
struct
  type 'a t = ('a -> unit) -> unit;;

  let vide = fun k -> ();;

  let cons t q = fun k -> (k t; q k);;

  (* à compléter en remplaçant le module Default *)
  include Default
end

(* module de test des itérateurs *)
module TestIter (F : SimpleIter) =
  struct
    (* un itérateur qui compte de 1 à n *)
    let count n =
      F.unfold (fun i -> if i > n then None else Some (i, i+1)) 1;;
    (* 'main' construit un flux exemple (modifiable) et le consomme *)
    (* en appelant 'consume', qui compte les éléments               *)
    let main (consume : 'a F.t -> int) =
      let t0 = Sys.time () in
      begin
        let nb = consume (F.map (fun x -> x) (F.append (count 5000000) (count 5000000))) in
        let t1 = Sys.time () in
        Format.printf "#elements=%d, time=%f@." nb (t1 -. t0)
        end;;
  end

(* programmes principaux de tests *)
let test_flux () =
  Format.printf "modèle Flux    : ";
  let module T = TestIter (Flux) in
  let rec consume n f =
      match Flux.uncons f with
      | None        -> n
      | Some (_, q) -> consume (n+1) q in
  T.main (consume 0);;

let test_pull () =
  Format.printf "modèle PullIter: ";
  let module T = TestIter (PullIter) in
  let rec consume n f =
      match PullIter.uncons f with
      | None        -> n
      | Some (_, q) -> consume (n+1) q in
  T.main (consume 0);;

let test_push () =
  Format.printf "modèle PushIter: ";
  let module T = TestIter (PushIter) in
  let consume f = let r = ref 0 in f (fun _ -> incr r); !r in
  T.main consume;;
