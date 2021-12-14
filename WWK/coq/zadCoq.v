From Coq Require Import ssreflect ssrbool ssrfun.
Set Implicit Arguments.

Require Import Psatz.
Open Scope nat_scope.

Require Export Coq.Arith.PeanoNat.
(* aby się dobrze pracowało *)
Notation "a | b" := (Nat.divide a b) (at level 70, no associativity).
Require Import Coq.Arith.Wf_nat.

(* PRZECZYTAJ *)
(* dużo lematów *)
Search (_ | _).
(* łądny lemat wiążący mod z | *)
Search (_ | _) (_ mod _).
(* też dużo *)
Search (_ mod _).

(* w Coq jest zdefiniowana pierwszość na liczbach całkowitych. To jest dość nieprzyjemne *)
(* my pracujemy z własną definicją *)
Inductive prime p :=
  prime_intro : 1 < p -> (forall x : nat, 1 < x < p -> ~ x | p) -> prime p.

(* CEL: udowodnić lemat Euklidesa: p | ab -> p | a \/ p | b *)
(* wyżej opisany lemat też jest już udowodniony,
   ale przy pomocy rozszerzonego algorytmu euklidesa - którego Euklides nie znał. To trochę oszustwo.
   Twoim celem jest udowodnienie tego faktu, idąc za dowodem dostępnym tutaj (chodzi o pierwszy prezentowany dowód):
  http://www.sci.brooklyn.cuny.edu/~mate/misc/euclids_lemma.pdf
   
   Pewną trudnością z tym dowodem jest fakt, że ma on zagniżdżone w sobie dwie indukcje i rozumowanie w stylu 'bez straty ogólności'.
*)

Lemma not_mod_not_divide: forall a b : nat, b <> 0 -> a mod b <> 0 <-> ~ b | a.
Proof.
  intros.
  pose proof (Nat.mod_divide a b H).
  destruct H0.
  split; intro; intro.
  pose proof (H1 H3).
  contradiction.
  pose proof (H0 H3).
  contradiction.
Qed.

(* wpierw potrzebujemy lematu: każda liczba ma dzielnik pierwszy *)
(* w tym celu robimy lemat pomocniczy: *)
Lemma primeDivisorHelper : forall n, n>1 -> forall m, m > 1 -> (exists k, prime k /\ k | n) \/ (forall i, 1<i<m -> not (i | n)).
Proof.
  move => n Npos.
  elim.
  right => i Z. lia.
  move => m Z Mbig.
  (* realny początek indukcji jest od 2. Dla mniejszych (tj m=1) teza idzie, bo warunek z prawej jest pusty *)
  case (Nat.eq_decidable (m) 1) => Mrange. subst. right. lia.
  case Z =>  [|Hprime|HnotSmallDiv]. lia. left. apply Hprime.
  (* DOKOŃCZ. Musisz sprawdzić czy m | n, tj czy m mod n = 0. Możesz to zrobić tak jak wyżej *)
  case (Nat.eq_decidable (n mod m) 0); intro.
  assert (m | n). apply Nat.mod_divide; lia.
  left. exists m. split; try done.
  constructor; try lia. intros. intro.
  apply (HnotSmallDiv x H1).
  apply (Nat.divide_trans x m n); done.
  right. intro.
  case (Nat.eq_decidable i m); intros.
  rewrite H0.
  pose proof (@not_mod_not_divide n m).
  destruct H2. lia. apply (H2 H).
  apply HnotSmallDiv. lia.
Qed.

Lemma primeDivisor : forall n, n>1 -> exists m, prime m /\ m | n.
Proof.
  move => n Nrange.
  destruct (@primeDivisorHelper n Nrange (S n)).
  (* dalej idzie szybciutko. DOKOŃCZ *)
  lia. apply H. exists n. split.
  constructor; try lia. intros. apply H. lia.
  apply Nat.divide_refl.
Qed.

(* REGUŁY:
- możesz wybrać jeden z dwóch sposób poniżej, albo pisać jeszcze inaczej
- nie wolno używać gcd, lcm, ani pierwszości z biblioteki standardowej
- wszystkiego innego wolno używać.
- należy formalizować wskazany dowód *)

Lemma divDivMod : forall p x: nat, p <> 0 -> (p | x <-> p | x mod p).
Proof.
  intros.
  rewrite -> (Nat.div_mod x p) at 1; try lia.
  enough (p | p * (x / p)).
  split.
  apply (Nat.divide_add_cancel_r p (p * (x / p)) (x mod p) H0).
  by apply Nat.divide_add_r.
  apply Nat.divide_factor_l.
Qed.

Lemma mulImpDiv : forall a b c: nat, a * b = c -> a | c.
Proof.
  intros. exists b. lia.
Qed.

Lemma primeNonZero : forall p: nat, prime p -> p <> 0.
Proof.
  intros. destruct H. lia.
Qed.

Lemma divMeansLarge : forall a b: nat, a | b -> b <> 0 -> a <= b.
Proof.
  intros. destruct H. destruct x; lia.
Qed.

Lemma divMeansSmall : forall a b c: nat, a >= 1 -> c > 1 -> a = b * c -> 1 <= b < a.
Proof.
  intros. destruct b; destruct c; lia.
Qed.

Lemma mul3 : forall a b c d e: nat, b <> 0 -> a * b * c = d * b * e -> a * c = d * e.
Proof.
  intros.
  rewrite (Nat.mul_comm a b) in H0.
  rewrite (Nat.mul_comm d b) in H0.
  rewrite <- Nat.mul_assoc in H0.
  rewrite <- Nat.mul_assoc in H0.
  apply (Nat.mul_cancel_l (a * c) (d * e) b) in H0; done.
Qed.

(* SPOSÓB 1 *)
(* idziemy zgodnie z tym, jak jest napisane w dowodzie: *)

Theorem EuclidLemma : forall p a b : nat, prime p -> p | a * b -> p | a \/ p | b.
Proof.
  elim / lt_wf_ind => p Ind.
  move => a b Hprime.
  have pneq0 := primeNonZero Hprime.

  without loss : a b Hprime / (a < p /\ b < p). 
  intros.
  case (x (a mod p) (b mod p) Hprime).
  split; try apply Nat.mod_upper_bound; destruct Hprime; try lia.
  rewrite (@divDivMod p (a mod p * (b mod p))); try done.
  rewrite <- Nat.mul_mod; try done.
  rewrite <- divDivMod; done.
  rewrite <- divDivMod; try tauto.
  rewrite <- divDivMod; try tauto.

  case => WLOG1 WLOG2.
  without loss : a b WLOG1 WLOG2 Hprime / (1 <= a /\ 1 <= b).
  
  intros.
  destruct a. left. apply Nat.divide_0_r.
  destruct b. right. apply Nat.divide_0_r.
  apply x; try auto.
  split; lia.

  case => WLOG3 WLOG4.

  intro.
  destruct H as [k].
  have: (1 <= k < p).
  split. lia.
  apply (Nat.mul_lt_mono_pos_l p k p); try lia.
  pose proof (Nat.mul_lt_mono a p b p WLOG1 WLOG2).
  rewrite (Nat.mul_comm p k).
  by rewrite <- H.

  (* Ładujemy indukcję po k *)
  move => kInd.
  move: k kInd a b WLOG1 WLOG2 WLOG3 WLOG4 H.
  elim / lt_wf_ind => k kInd.
  case (Nat.eq_decidable k 0); try lia.
  
  (* Przypadek bazowy k = 1 *)
  case (Nat.eq_decidable k 1).
  intros.
  rewrite H in H1; simpl in H1; rewrite Nat.add_comm in H1; simpl in H1.

  (* Wtedy a * b = p, a skoro 1 <= a < p i 1 <= b < p, to p jest złożona *)
  (* Udowadniany, rozpatrując dwa przypadki: a = 1 i a > 1 *)
  destruct Hprime.
  case (Nat.eq_decidable a 1).
  intro. rewrite H2 in H1; simpl in H1; rewrite Nat.add_comm in H1; simpl in H1. lia.
  intro. cut (1 < a < p); try lia.
  pose proof (@mulImpDiv a b p H1).
  intros. pose proof (n a H4). contradiction.

  (* Teraz przypadek, gdy 2 <= k < p *)
  intros. cut (2 <= k < p); try lia.
  intros. cut (1 < k); try lia.
  intros.

  (* Let q be a prime divisor of k. *)
  destruct (primeDivisor H3) as [q].
  destruct H4. destruct kInd0.
  (* Skoro q | k i k < p, to q < p. *)
  assert (q < p).
    pose proof (divMeansLarge H5 H0). apply (Nat.le_lt_trans q k p H8 H7).

  (* Then q | a * b *)
  destruct H5 as [k'].
  assert (q | a * b).
    rewrite H5 in H1. rewrite (Nat.mul_comm k' q) in H1.
    rewrite <- Nat.mul_assoc in H1. symmetry in H1.
    apply (@mulImpDiv q (k' * p) (a * b) H1).
  
  (* by the minimality assumption on p we have q | a \/ q | b *)
  pose proof (Ind q H8 a b H4 H9).

  (* W równości k' * p = a' * b' mamy k' = k / q (zdefiniowane wyżej) i
     a' = if q | a then a / q else a
     b' = if q | b then b / q else b *)

  (* k' < k, 1 <= k' < p *)  
  destruct H4 as [H4].
  pose proof (@divMeansSmall k k' q H6 H4 H5).
  destruct H11.
  pose proof (Nat.lt_trans k' k p H12 H7).
  assert (1 <= k' < p); try lia.
  assert (q <> 0); try lia.

  destruct H10.
    destruct H10 as [a'].
    pose proof (@divMeansSmall a a' q WLOG3 H4 H10).
    destruct H16.
    assert (a' < p). by apply (Nat.lt_trans a' a p).
    assert (a' * b = k' * p).
      rewrite H10 in H1. rewrite H5 in H1.
      rewrite <- Nat.mul_comm in H1.
      rewrite Nat.mul_comm in H1.
      apply (mul3 _ _ _ _ H15 H1).
    case (kInd k' H12 H14 a' b H18 WLOG2 H16 WLOG4 H19); intros.
    left. rewrite H10. by apply Nat.divide_mul_l.
    by right.

    destruct H10 as [b'].
    pose proof (@divMeansSmall b b' q WLOG4 H4 H10).
    destruct H16.
    assert (b' < p). by apply (Nat.lt_trans b' b p).
    assert (a * b' = k' * p).
      rewrite H10 in H1. rewrite H5 in H1.
      rewrite (Nat.mul_comm b' q) in H1.
      rewrite Nat.mul_assoc in H1.
      apply (mul3 _ _ _ _ H15 H1).
    case (kInd k' H12 H14 a b' WLOG1 H18 WLOG3 H16 H19); intros.
    by left.
    right. rewrite H10. by apply Nat.divide_mul_l.

Qed.