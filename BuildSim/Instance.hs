{-# OPTIONS_GHC -cpp -XMagicHash #-}
{- For Hugs, use the option -F"cpp -P -traditional" -}

module Instance where

import qualified Prelude
import qualified Datatypes
import qualified Fin
import qualified LibStruct
import qualified Notations
import qualified Syntax
import qualified Word

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Base
#else
-- HUGS
import qualified IOExts
#endif

#ifdef __GLASGOW_HASKELL__
unsafeCoerce :: a -> b
unsafeCoerce = GHC.Base.unsafeCoerce#
#else
-- HUGS
unsafeCoerce :: a -> b
unsafeCoerce = IOExts.unsafeCoerce
#endif

coq_StateMod0 :: Prelude.Int
coq_StateMod0 =
  0

coq_StateMod1 :: Prelude.Int
coq_StateMod1 =
  (Prelude.succ :: Prelude.Int -> Prelude.Int) 0

coq_StateMod2 :: Prelude.Int
coq_StateMod2 =
  (Prelude.succ :: Prelude.Int -> Prelude.Int)
    ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)

stateSz :: Prelude.Int
stateSz =
  (Prelude.succ :: Prelude.Int -> Prelude.Int)
    ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)

coq_State :: Syntax.Kind
coq_State =
  Syntax.Bit stateSz

stateReg :: Prelude.String
stateReg =
  (:) 's' ((:) 't' ((:) 'a' ((:) 't' ((:) 'e' ([])))))

transition :: (Syntax.Expr a1) -> (Syntax.Expr a1) -> Syntax.Expr a1
transition state b =
  Syntax.unpack coq_State (Syntax.Kor (Syntax.Bit
    (Syntax.size (Syntax.Bit stateSz))) ((:) (Syntax.ITE (Syntax.SyntaxKind
    (Syntax.Bit (Syntax.size (Syntax.Bit stateSz)))) (Syntax.Eq coq_State
    state
    (Prelude.fst ((,) (Syntax.Const (Syntax.Bit stateSz) (Syntax.ConstBit
      stateSz (Word.natToWord stateSz coq_StateMod0))) (Syntax.ITE
      (Syntax.SyntaxKind (Syntax.Bit stateSz)) (Syntax.Eq (Syntax.Bit
      ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)) b (Syntax.Const
      (Syntax.Bit ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0))
      (Syntax.ConstBit ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)
      (Word.natToWord ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0) 0))))
      (Syntax.Const (Syntax.Bit stateSz) (Syntax.ConstBit stateSz
      (Word.natToWord stateSz coq_StateMod0))) (Syntax.Const (Syntax.Bit
      stateSz) (Syntax.ConstBit stateSz
      (Word.natToWord stateSz coq_StateMod1)))))))
    (Syntax.pack (Syntax.Bit stateSz)
      (Prelude.snd ((,) (Syntax.Const (Syntax.Bit stateSz) (Syntax.ConstBit
        stateSz (Word.natToWord stateSz coq_StateMod0))) (Syntax.ITE
        (Syntax.SyntaxKind (Syntax.Bit stateSz)) (Syntax.Eq (Syntax.Bit
        ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)) b (Syntax.Const
        (Syntax.Bit ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0))
        (Syntax.ConstBit ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)
        (Word.natToWord ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0) 0))))
        (Syntax.Const (Syntax.Bit stateSz) (Syntax.ConstBit stateSz
        (Word.natToWord stateSz coq_StateMod0))) (Syntax.Const (Syntax.Bit
        stateSz) (Syntax.ConstBit stateSz
        (Word.natToWord stateSz coq_StateMod1))))))) (Syntax.Const
    (Syntax.Bit (Syntax.size (Syntax.Bit stateSz))) (Syntax.ConstBit
    (Syntax.size (Syntax.Bit stateSz))
    (Word.natToWord (Syntax.size (Syntax.Bit stateSz)) 0)))) ((:) (Syntax.ITE
    (Syntax.SyntaxKind (Syntax.Bit (Syntax.size (Syntax.Bit stateSz))))
    (Syntax.Eq coq_State state
    (Prelude.fst ((,) (Syntax.Const (Syntax.Bit stateSz) (Syntax.ConstBit
      stateSz (Word.natToWord stateSz coq_StateMod1))) (Syntax.ITE
      (Syntax.SyntaxKind (Syntax.Bit stateSz)) (Syntax.Eq (Syntax.Bit
      ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)) b (Syntax.Const
      (Syntax.Bit ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0))
      (Syntax.ConstBit ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)
      (Word.natToWord ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0) 0))))
      (Syntax.Const (Syntax.Bit stateSz) (Syntax.ConstBit stateSz
      (Word.natToWord stateSz coq_StateMod2))) (Syntax.Const (Syntax.Bit
      stateSz) (Syntax.ConstBit stateSz
      (Word.natToWord stateSz coq_StateMod0)))))))
    (Syntax.pack (Syntax.Bit stateSz)
      (Prelude.snd ((,) (Syntax.Const (Syntax.Bit stateSz) (Syntax.ConstBit
        stateSz (Word.natToWord stateSz coq_StateMod1))) (Syntax.ITE
        (Syntax.SyntaxKind (Syntax.Bit stateSz)) (Syntax.Eq (Syntax.Bit
        ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)) b (Syntax.Const
        (Syntax.Bit ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0))
        (Syntax.ConstBit ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)
        (Word.natToWord ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0) 0))))
        (Syntax.Const (Syntax.Bit stateSz) (Syntax.ConstBit stateSz
        (Word.natToWord stateSz coq_StateMod2))) (Syntax.Const (Syntax.Bit
        stateSz) (Syntax.ConstBit stateSz
        (Word.natToWord stateSz coq_StateMod0))))))) (Syntax.Const
    (Syntax.Bit (Syntax.size (Syntax.Bit stateSz))) (Syntax.ConstBit
    (Syntax.size (Syntax.Bit stateSz))
    (Word.natToWord (Syntax.size (Syntax.Bit stateSz)) 0)))) ((:) (Syntax.ITE
    (Syntax.SyntaxKind (Syntax.Bit (Syntax.size (Syntax.Bit stateSz))))
    (Syntax.Eq coq_State state
    (Prelude.fst ((,) (Syntax.Const (Syntax.Bit stateSz) (Syntax.ConstBit
      stateSz (Word.natToWord stateSz coq_StateMod2))) (Syntax.ITE
      (Syntax.SyntaxKind (Syntax.Bit stateSz)) (Syntax.Eq (Syntax.Bit
      ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)) b (Syntax.Const
      (Syntax.Bit ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0))
      (Syntax.ConstBit ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)
      (Word.natToWord ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0) 0))))
      (Syntax.Const (Syntax.Bit stateSz) (Syntax.ConstBit stateSz
      (Word.natToWord stateSz coq_StateMod1))) (Syntax.Const (Syntax.Bit
      stateSz) (Syntax.ConstBit stateSz
      (Word.natToWord stateSz coq_StateMod2)))))))
    (Syntax.pack (Syntax.Bit stateSz)
      (Prelude.snd ((,) (Syntax.Const (Syntax.Bit stateSz) (Syntax.ConstBit
        stateSz (Word.natToWord stateSz coq_StateMod2))) (Syntax.ITE
        (Syntax.SyntaxKind (Syntax.Bit stateSz)) (Syntax.Eq (Syntax.Bit
        ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)) b (Syntax.Const
        (Syntax.Bit ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0))
        (Syntax.ConstBit ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)
        (Word.natToWord ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0) 0))))
        (Syntax.Const (Syntax.Bit stateSz) (Syntax.ConstBit stateSz
        (Word.natToWord stateSz coq_StateMod1))) (Syntax.Const (Syntax.Bit
        stateSz) (Syntax.ConstBit stateSz
        (Word.natToWord stateSz coq_StateMod2))))))) (Syntax.Const
    (Syntax.Bit (Syntax.size (Syntax.Bit stateSz))) (Syntax.ConstBit
    (Syntax.size (Syntax.Bit stateSz))
    (Word.natToWord (Syntax.size (Syntax.Bit stateSz)) 0)))) ([])))))

mainMod :: Syntax.Mod
mainMod =
  Syntax.Base
    (Notations.makeModule
      (Datatypes.app ((:) (Notations.MERegister ((,) stateReg ((,)
        (Syntax.SyntaxKind coq_State) (Prelude.Just
        (Notations.makeConst (Syntax.Bit stateSz) (Syntax.ConstBit stateSz
          (Word.natToWord stateSz coq_StateMod0))))))) ([]))
        (Datatypes.app ((:) (Notations.MERule ((,) ((:) 'm' ((:) 'a' ((:) 'i'
          ((:) 'n' ([]))))) (\_ -> Syntax.ReadReg stateReg (Syntax.SyntaxKind
          coq_State) (\prevState -> Syntax.MCall ((:) 'r' ((:) 'e' ((:) 's'
          ((:) 'e' ((:) 't' ([])))))) ((,) (Syntax.Bit 0) Syntax.Bool)
          (Syntax.Const (Prelude.fst ((,) (Syntax.Bit 0) Syntax.Bool))
          (Syntax.getDefaultConst
            (Prelude.fst ((,) (Syntax.Bit 0) Syntax.Bool)))) (\reset ->
          Syntax.MCall ((:) 'g' ((:) 'e' ((:) 't' ((:) 'I' ((:) 'n' ((:) 'p'
          ((:) 'u' ((:) 't' ([]))))))))) ((,) (Syntax.Bit 0)
          (LibStruct.coq_Maybe (Syntax.Bit
            ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)))) (Syntax.Const
          (Prelude.fst ((,) (Syntax.Bit 0)
            (LibStruct.coq_Maybe (Syntax.Bit
              ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)))))
          (Syntax.getDefaultConst
            (Prelude.fst ((,) (Syntax.Bit 0)
              (LibStruct.coq_Maybe (Syntax.Bit
                ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)))))))
          (\input -> Syntax.LetExpr (Syntax.SyntaxKind coq_State) (Syntax.ITE
          (Syntax.SyntaxKind (Syntax.Bit stateSz)) (Syntax.Var
          (Syntax.SyntaxKind Syntax.Bool) reset) (Syntax.Const (Syntax.Bit
          stateSz) (Syntax.ConstBit stateSz
          (Word.natToWord stateSz coq_StateMod0))) (Syntax.ITE
          (Syntax.SyntaxKind coq_State) (Syntax.ReadStruct
          (Prelude.length ((:) ((,) ((:) 'v' ((:) 'a' ((:) 'l' ((:) 'i' ((:)
            'd' ([])))))) Syntax.Bool) ((:) ((,) ((:) 'd' ((:) 'a' ((:) 't'
            ((:) 'a' ([]))))) (Syntax.Bit
            ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0))) ([])))) (\i ->
          Prelude.snd
            (Fin.nth_Fin ((:) ((,) ((:) 'v' ((:) 'a' ((:) 'l' ((:) 'i' ((:)
              'd' ([])))))) Syntax.Bool) ((:) ((,) ((:) 'd' ((:) 'a' ((:) 't'
              ((:) 'a' ([]))))) (Syntax.Bit
              ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0))) ([]))) i))
          (\j ->
          Prelude.fst
            (Fin.nth_Fin ((:) ((,) ((:) 'v' ((:) 'a' ((:) 'l' ((:) 'i' ((:)
              'd' ([])))))) Syntax.Bool) ((:) ((,) ((:) 'd' ((:) 'a' ((:) 't'
              ((:) 'a' ([]))))) (Syntax.Bit
              ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0))) ([]))) j))
          (Syntax.Var (Syntax.SyntaxKind
          (Prelude.snd ((,) (Syntax.Bit 0)
            (LibStruct.coq_Maybe (Syntax.Bit
              ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)))))) input)
          (unsafeCoerce (Prelude.Left ())))
          (transition (Syntax.Var (Syntax.SyntaxKind coq_State) prevState)
            (Syntax.ReadStruct
            (Prelude.length ((:) ((,) ((:) 'v' ((:) 'a' ((:) 'l' ((:) 'i'
              ((:) 'd' ([])))))) Syntax.Bool) ((:) ((,) ((:) 'd' ((:) 'a'
              ((:) 't' ((:) 'a' ([]))))) (Syntax.Bit
              ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0))) ([]))))
            (\i ->
            Prelude.snd
              (Fin.nth_Fin ((:) ((,) ((:) 'v' ((:) 'a' ((:) 'l' ((:) 'i' ((:)
                'd' ([])))))) Syntax.Bool) ((:) ((,) ((:) 'd' ((:) 'a' ((:)
                't' ((:) 'a' ([]))))) (Syntax.Bit
                ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0))) ([]))) i))
            (\j ->
            Prelude.fst
              (Fin.nth_Fin ((:) ((,) ((:) 'v' ((:) 'a' ((:) 'l' ((:) 'i' ((:)
                'd' ([])))))) Syntax.Bool) ((:) ((,) ((:) 'd' ((:) 'a' ((:)
                't' ((:) 'a' ([]))))) (Syntax.Bit
                ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0))) ([]))) j))
            (Syntax.Var (Syntax.SyntaxKind
            (Prelude.snd ((,) (Syntax.Bit 0)
              (LibStruct.coq_Maybe (Syntax.Bit
                ((Prelude.succ :: Prelude.Int -> Prelude.Int) 0)))))) input)
            (unsafeCoerce (Prelude.Right (Prelude.Left ()))))) (Syntax.Var
          (Syntax.SyntaxKind coq_State) prevState))) (\state ->
          Syntax.WriteReg stateReg (Syntax.SyntaxKind coq_State) (Syntax.Var
          (Syntax.SyntaxKind coq_State) state) (Syntax.MCall ((:) 's' ((:)
          'e' ((:) 'n' ((:) 'd' ((:) 'O' ((:) 'u' ((:) 't' ((:) 'p' ((:) 'u'
          ((:) 't' ([]))))))))))) ((,) Syntax.Bool (Syntax.Bit 0)) (Syntax.Eq
          coq_State (Syntax.Var (Syntax.SyntaxKind coq_State) state)
          (Syntax.Const (Syntax.Bit stateSz) (Syntax.ConstBit stateSz
          (Word.natToWord stateSz coq_StateMod0)))) (\_ -> Syntax.Return
          (Syntax.Const (Syntax.Bit 0)
          (Syntax.getDefaultConst (Syntax.Bit 0)))))))))))) ([])) ([]))))

