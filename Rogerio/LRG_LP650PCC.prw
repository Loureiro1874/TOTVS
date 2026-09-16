#INCLUDE 'PROTHEUS.CH'
#include 'TOTVS.CH'
#INCLUDE 'RWMAKE.CH'
//
/*
   ROTINA DE CONTABILIZAÇÃO DA NOTA F/SCAL DE ENTRADA PARA O PIS COFINS
  Desenvolvido por Rogerio Nunes em 08/2026
  @param  cTipoCtb,C,1 : P- PIS / C-COFINS
*/
User function LP650PCC( cTipoCtb )
   //
   Local cRetLp := 'A CLASSIFICAR'
   Local acNiv1 := Posicione('CTT',1,xFilial('CTT')+SD1->D1_CC,'CTT_XTP')  // FORMULA('LP1')
   //
   //
   IF cTipoCtb == 'P'
      //
      IF SF4->F4_PISCRED='2'
         cRetLp :=  "21401006"
      ELSE
         IF SF4->F4_ESTOQUE='S'
            cRetLp := SB1->B1_CONTA
         ELSEIF acNiv1 =='AGR'
            cRetLp := '72402001'
         ELSEIF acNiv1 == 'PEC'
             cRetLp :='71402001'
         ELSEIF acNiv1 == 'FIL'
             cRetLp :='73302001'
         ELSEIF acNiv1 == 'ADM'
             cRetLp :='61901001'
         ELSE 
             cRetLp := 'CTT PIS<>AGR/PEC/FIL'
         ENDIF    
         //
      ENDIF
      //
   ELSEIF cTipoCtb == 'C' // COFINS 
      //
      //
      IF SF4->F4_PISCRED='2'
         cRetLp :=  "21401002"
      ELSE
         IF SF4->F4_ESTOQUE='S'
            cRetLp := SB1->B1_CONTA
         ELSEIF acNiv1 =='AGR'
            cRetLp := '72402002'
         ELSEIF acNiv1 == 'PEC'
             cRetLp :='71402002'
         ELSEIF acNiv1 == 'FIL'
             cRetLp :='73302002'
         ELSEIF acNiv1 == 'ADM'
             cRetLp :='61901002'
         ELSE 
             cRetLp := 'CTT COFINS<>AGR/PEC/FIL'
         ENDIF    
         //
      ENDIF
      //
   ENDIF
   //
   //PIS --> IIF(SF4->F4_PISCRED='2',"21401006",IIF(SF4->F4_ESTOQUE='S',SB1->B1_CONTA,IIF(FORMULA('LP1') =='AGR','72402001',IIF(FORMULA('LP1')== 'PEC','71402001',IIF(FORMULA('LP1')== 'FIL','73302001',IIF(FORMULA('LP1')== 'ADM','61901001','CTT<>AGR/PEC/FIL') )))))
   //COFINS --> IIF(SF4->F4_PISCRED='2',"21401002",IIF(SF4->F4_ESTOQUE='S',SB1->B1_CONTA,IIF(FORMULA('LP1') =='AGR','72402002',IIF(FORMULA('LP1')== 'PEC','71402002',IIF(FORMULA('LP1')== 'FIL','73302002',IIF(FORMULA('LP1')== 'ADM','61901002','CTT<>AGR/PEC/FIL' ) ))))
Return(cRetLp)
//-----------------------------
