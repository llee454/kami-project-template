(*
  This Kami module represents a device that accepts a sequence
  of bits that represents a binary number and returns a 1 iff the
  number represented by the given sequence is divisible by 3.
*)

Require Import Kami.All.
Require Import List.
Import ListNotations.

Open Scope kami_expr.
Open Scope kami_action.

Definition StateMod0 := 0.
Definition StateMod1 := 1.
Definition StateMod2 := 2.
Definition stateSz := 2.
Definition State := Bit stateSz.
Definition stateReg := "state".

Section ty.
  Variable ty : Kind -> Type.

  Definition transition (state : State @# ty) (b : Bit 1 @# ty) : State @# ty :=
    Switch state Retn State With {
      ($StateMod0 : State @# ty) ::=
        IF b == $0 then $StateMod0 else $StateMod1 : State @# ty;
      ($StateMod1 : State @# ty) ::=
        IF b == $0 then $StateMod2 else $StateMod0 : State @# ty;
      ($StateMod2 : State @# ty) ::=
        IF b == $0 then $StateMod1 else $StateMod2 : State @# ty
    }.
End ty.

Definition mainMod : Mod :=
  Base (MODULE {
    Register stateReg : State <- ConstBit ($StateMod0 : word stateSz) with
    Rule "main" :=
      Read prevState : State <- stateReg;
      Call reset : Bool <- "reset" ();
      Call input : Maybe (Bit 1) <- "getInput" ();
      LET state : State
        <- IF #reset
             then $StateMod0
             else IF #input @% "valid"
                 then transition #prevState (#input @% "data")
                 else #prevState;
      Write stateReg : State <- #state;
      Call "sendOutput" (#state == $StateMod0 : Bool);
      Retv
  }).

Close Scope kami_action.
Close Scope kami_expr.
