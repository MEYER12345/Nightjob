      *****************************************************************
     h copyright('(C) Copyright 1996 - 2006 King III Solutions, Inc.  +
     h Rel 4.37 2006-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') AUT(*ALL)

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2006 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_9930
      **   Type: ILE RPG Program
      **   Desc: Control vary text line 3030/31
      **
      *****************************************************************
      **
      **  This program will receive and build vary tetxt line.
      **
      **  11/27/12 M Schrimshaw - add company key to invmasz
      *                           invmasl01 has only 'JAR' records
      **  03/10/15 Trish Henley - change prefix on INVMASZ to match
      **                          BEERLY prefix as both files are using
      **                          the same subroutine to fill the parms
      **
      *****************************************************************

     fk_productaif   e           k disk    infds(infds) infsr(k3s_infsr)
     f                                     usropn
      * products by locn, supplier, sub supplier, prod
      *****************************************************************
      *****************************************************************
     D Ctr             S              3  0
     D Cmd             S            512
     D Cmdlen          S             15  5 Inz(512)
     D kcomp           S              3    inz('JAR')
     D kprod           S              6

     D  Myspace        S             10    Dim(13)

     d comp            s                   like(pr_comp)
     d buyr            s                   like(pr_buyr)
     d locn            s                   like(pr_locn)
     d supl            s                   like(pr_supl)
     d suplsub         s                   like(pr_suplsub)
     d prod            s                   like(pr_prod)
     d altsrce         s                   like(pr_altsrce)
      ** History type begin
     d histype         s                   like(pr_altsrce)
      ** History type end
     d statusa         s              5a
     d R6_user         s             10a
     d zz_varytxt      s            130a
     d show1_1st       s             26a   inz('X-------------------------')
     d show1_2nd       s             26a   inz('---------------------- thi')
     d show1_3rd       s             26a   inz('s line is under your contr')
     d show1_4th       s             26a   inz('ol -----------------------')
     d show1_5th       s             26a   inz('-------------------------X')
     d zz_varytx2      s            130a
     d show2_1st       s             26a   inz('X-------------------------')
     d show2_2nd       s             26a   inz('---------------------- thi')
     d show2_3rd       s             26a   inz('s is the 2nd line under yo')
     d show2_4th       s             26a   inz('ur control ---------------')
     d show2_5th       s             26a   inz('-------------------------X')
     d zz_varytx3      s            130a
     d show3_1st       s             26a   inz('X-------------------------')
     d show3_2nd       s             26a   inz('---------------------- thi')
     d show3_3rd       s             26a   inz('s is the 3rd line under yo')
     d show3_4th       s             26a   inz('ur control ---------------')
     d show3_5th       s             26a   inz('-------------------------X')
     d zz_varytx4      s            130a
     d show4_1st       s             26a   inz('X-------------------------')
     d show4_2nd       s             26a   inz('---------------------- I h')
     d show4_3rd       s             26a   inz('ave changed this 4th line ')
     d show4_4th       s             26a   inz('different ----------------')
     d show4_5th       s             26a   inz('-------------------------X')
     dzz_vary          ds
     dzz_vary01                      13    inz('            ')
     dzz_vary02                      13    inz('BtCs         ')
     dzz_vary2         ds
     dzz_vary01a                     13    inz('             ')
     dzz_vary02a                     13    inz('Cst          ')
     dzz_vary3         ds
     dzz_vary01b                     13    inz('             ')
     dzz_vary02b                     13    inz('Cst          ')
     dzz_vary4         ds
     dzz_vary01c                     13    inz('Reg          ')
     dzz_vary02c                     13    inz('             ')
     dpr_num           ds
     dpr_num1                         1  0
     dpr_num2                         1  0
     dpr_num3                         1  0
     dpr_num4                         1  0
     dpr_num5                         1  0
     dpr_num6                         1  0
     dpr_num7                         1  0
     dpr_alp           ds
     dpr_alp1                         1
     dpr_alp2                         1
     dpr_alp3                         1
     dpr_alp4                         1
     dpr_alp5                         1
     dpr_alp6                         1
     dpr_alp7                         1
     dpr_alp8                         1
     dpr_alp9                         1
     d infds           ds
     d status            *status
     dpr_no            ds
     dpr_no1                          1  0
     dpr_no2                          1  0
     dpr_no3                          1  0
     dpr_no4                          1  0
     dpr_no5                          1  0
     dpr_no6                          1  0
     dpr_no7                          1  0
     dpr_no8                          1  0
     dpr_no9                          1  0
     dpr_no10                         1  0
     dpr_no11                         1  0
     dpr_alph          ds
     dpr_alph1                        1
     dpr_alph2                        1
     dpr_alph3                        1
     dpr_alph4                        1
     dpr_alph5                        1
     dpr_alph6                        1
     dpr_alph7                        1
     dpr_alph8                        1
     dpr_alph9                        1
     dpr_alph10                       1
     dpr_alph11                       1
     dpr_alph12                       1
     dpr_alph13                       1
     dpr_alph14                       1
      *****************************************************************
      * parameters passed to program
     c     *entry        plist
     c                   parm                    comp
     c                   parm                    buyr
     c                   parm                    locn
     c                   parm                    supl
     c                   parm                    suplsub
     c                   parm                    prod
     c                   parm                    altsrce
      ** History type begin
     c                   parm                    histype
      ** History type end
     c******             parm                    R6_user
     c                   parm                    zz_varytxt
     c                   parm                    zz_varytx2
     c                   parm                    zz_varytx3
     c                   parm                    zz_varytx4

      * key list for product file
     c     px_key        klist
     c                   kfld                    comp
     c                   kfld                    locn
     c                   kfld                    supl
     c                   kfld                    suplsub
     c                   kfld                    prod

     c                   eval      *in28 = *off
     c                   open      k_producta                           28
     c                   exsr      k3s_infsr
     c                   if        *in28 = *off
     c     px_key        chain(n)  rk_product                         2728
     c                   exsr      k3s_infsr
     c                   if        *in27 = *off and
     c                             *in28 = *off
     c*                  move      pr_trnoord    pr_num
     c*                  exsr      $_pr_numb
     c*                  movel     pr_alp        zz_vary01
     c                   move      pr_altoord    pr_num
     c                   exsr      $_pr_numb
     c*                  movel     pr_alp        zz_vary02
     c                   move      pr_alp        zz_vary02

     c                   eval      zz_varytxt = *blanks
     c                   movel     zz_vary       zz_varytxt

     c*                  eval      zz_varytxt = show1_1st + show1_2nd +
     c*                            show1_3rd + show1_4th + show1_5th

     c                   move      pr_qtybaln    pr_num
     c                   exsr      $_pr_numb
     c                   move      pr_alp        zz_vary02a

     c                   eval      zz_varytx2 = *blanks

     c*                  movel     zz_vary2     zz_varytx2
     c                   eval      zz_varytx2 = zz_vary2
     c*                  eval      zz_varytx2 = show2_1st + show2_2nd +
     c*                            show2_3rd + show2_4th + show2_5th

     c                   move      pr_costreg    pr_no
     c                   exsr      $_pr_no
     c                   eval      zz_vary02c = pr_alph

     c                   eval      zz_varytx3 = *blanks
     c                   eval      zz_varytx3 = zz_vary4

     c*                  eval      zz_varytx3 = show3_1st + show3_2nd +
     c*                            show3_3rd + show3_4th + show3_5th

     c                   eval      zz_varytx4 = *blanks

     c                   eval      zz_varytx4 = show4_1st + show4_2nd +
     c                             show4_3rd + show4_4th + show4_5th

     c                   else
     c                   exsr      k3s_infsr
     c                   endif
     c****               ExSr      Jarboestuff
     c                   endif

     c                   return
      * //////////////////////////////////////////////// Edit PB numbers

      *****************************************************************
     c     $_pr_numb     begsr

     c                   move      pr_num1       pr_alp1
     c                   eval      pr_alp2 = ','
     c                   move      pr_num2       pr_alp3
     c                   move      pr_num3       pr_alp4
     c                   move      pr_num4       pr_alp5
     c                   eval      pr_alp6 = ','
     c                   move      pr_num5       pr_alp7
     c                   move      pr_num6       pr_alp8
     c                   move      pr_num7       pr_alp9
     c                   eval      pr_alph10 = ','
     c                   if        pr_num1 = 0
     c                   eval      pr_alp1 = *blank
     c                   eval      pr_alp2 = *blank
     c                   if        pr_num2 = 0
     c                   eval      pr_alp3 = *blank
     c                   if        pr_num3 = 0
     c                   eval      pr_alp4 = *blank
     c                   if        pr_num4 = 0
     c                   eval      pr_alp5 = *blank
     c                   eval      pr_alp6 = *blank
     c                   if        pr_num5 = 0
     c                   eval      pr_alp7 = *blank
     c                   if        pr_num6 = 0
     c                   eval      pr_alp8 = *blank
     c                   endif
     c                   endif
     c                   endif
     c                   endif
     c                   endif
     c                   endif

     c                   endsr

      *****************************************************************
     c     k3s_infsr     begsr
     c                   if        status > 00000
     c                   if        status = 01215
     c                   eval      *in28 = *off
     c                   endif
     c                   if        status = 00011 or
     c                             status = 00012
     c                   eval      zz_varytxt = *blanks
     c                   eval      zz_varytxt = 'No information found for this +
     c                                          product or supplier  '
     c                   endif
     c                   if        status = 01011 or
     c                             status = 01211 or
     c                             status = 01216 or
     c                             status = 01217 or
     c                             status = 01299
     c                   movel     status        statusa
     c                   eval      zz_varytxt = *blanks
     c                   eval      zz_varytxt = %subst('File error' : 1) + ' ' +
     c                                          statusa + ' ' +
     c                                          'contact your IS department'
     c                   endif
     c                   if        status = 01218
     c                   eval      zz_varytxt = *blanks
     c                   eval      zz_varytxt = 'File error record locked by an+
     c                                          other job, try again '
     c                   endif
     c                   endif
     c                   endsr
     *****************************************************************
     c     $_pr_no       begsr

     c                   move      pr_no1        pr_alph
     c                   eval      pr_alph2 = ','
     c                   move      pr_no2        pr_alph3
     c                   move      pr_no3        pr_alph4
     c                   move      pr_no4        pr_alph5
     c                   eval      pr_alph6 = ','
     c                   move      pr_no5        pr_alph7
     c                   move      pr_no6        pr_alph8
     c                   move      pr_no7        pr_alph9
     c                   eval      pr_alph10 = '.'
     c                   move      pr_no8        pr_alph11
     c                   move      pr_no9        pr_alph12
     c                   move      pr_no10       pr_alph13
     c                   move      pr_no11       pr_alph14
     c                   if        pr_no1 = 0
     c                   eval      pr_alph1 = *blank
     c                   eval      pr_alph2 = *blank
     c                   if        pr_no2 = 0
     c                   eval      pr_alph3 = *blank
     c                   if        pr_no3 = 0
     c                   eval      pr_alph4 = *blank
     c                   if        pr_no4 = 0
     c                   eval      pr_alph5 = *blank
     c                   eval      pr_alph6 = *blank
     c                   if        pr_no6 = 0
     c                   eval      pr_alph7 = *blank
     c                   if        pr_no7 = 0
     c                   eval      pr_alph8 = *blank
     c                   endif
     c                   endif
     c                   endif
     c                   endif
     c                   endif
     c                   endif

     c                   endsr

      *****************************************************************
      *****************************************************************
      ********* week number logic begin *******************************
      **
      **  The section of code below can be used to determine the
      **  week number for products that are 13 four weekly..
      **
      **  You just need to un-comment the statements for use,
      **  and place them appropriately into 9910
      **
      **

     f*_schedpebif   e           k disk
      * schedule period end

     d*week_num        s              2a
     d*week_numbr      s              3a
     d*locn_blank      s                   like(pr_locn)
     d*                                    inz('   ')

      * key list for schedule period end
     c*    se_key        klist
     c*                  kfld                    comp
     c*                  kfld                    locn_blank
     c*                  kfld                    pr_forcint
     c*                  kfld                    lda_sysdat


      ***  need forcast interval from product file...
     c*    px_key        chain(n)  rk_product                         27
     c*                  if        *in27 = *off

      ***    assume weekly products
     c*                  move      pr_forcper    week_num
     c*                  eval      week_numbr = week_num + 'w'

      ***    for 13 four weekly products, determine which week #
     c*                  if        pr_forcint = 13
     c*                  exsr      $_get_lda
     c*    se_key        setgt     rk_schedpe
     c*                  readp     rk_schedpe                             95
     c*                  if        not *in95

     c*                  eval      week_numbr = 'W#1'
     c*    se_ending     adddur    7:*days       se_ending
     c*                  if        lda_sysdat >= se_ending
     c*                  eval      week_numbr = 'W#2'
     c*    se_ending     adddur    7:*days       se_ending
     c*                  if        lda_sysdat >= se_ending
     c*                  eval      week_numbr = 'W#3'
     c*    se_ending     adddur    7:*days       se_ending
     c*                  if        lda_sysdat >= se_ending
     c*                  eval      week_numbr = 'W#4'
     c*                  endif
     c*                  endif
     c*                  endif

     c*                  endif
     c*                  endif
     c*                  endif

      ********* week number logic end   *******************************
      *****************************************************************
      *****************************************************************
     c*    JarboeStuff   Begsr
     c*                  Exsr      @ClearStuff
     c*                  Select
     c*                  When      %Subst(Pr_Prod:1:1) > '0'
     c*                  Exsr      @GetJarboe
     c*                  Other
     c*                  Exsr      @GetIBC
     c*                  EndSl
     c*                  Endsr
     c*****************************************************************
     c*    @GetJarboe    Begsr
     c*    Pr_Prod6      Chain     InvmasL01
     c*                  If        %Found(InvMasL01)
     c*                  eval      kprod = pr_prod6
     c*    imkey         Chain     InvMasZ
     c**     Pr_Prod6      Chain     InvMasZ
     c*                  Exsr      @FillParms
     c*                  EndIf
     c*                  EndSr
     c*****************************************************************
     c*
     c*    @GetIBC       Begsr
     c*    Pr_Prod6      Chain     BeerL
     c*                  If        %Found(BeerL)
     c*    Pr_Prod6      Chain     beerly
     c*                  Exsr      @FillParms
     c*                  EndIf
     c*                  EndSr
     c*****************************************************************
     c*
     c*    @FillParms    Begsr
     c*                  Eval      MySpace(01) = '  Jan  '
     c*                  Eval      MySpace(02) = '  Feb  '
     c*                  Eval      MySpace(03) = '  Mar  '
     c*                  Eval      MySpace(04) = '  Apr  '
     c*                  Eval      MySpace(05) = '  May  '
     c*                  Eval      MySpace(06) = '  Jun  '
     c*                  Eval      MySpace(07) = '  Jul  '
     c*                  Eval      MySpace(08) = '  Aug  '
     c*                  Eval      MySpace(09) = '  Sep  '
     c*                  Eval      MySpace(10) = '  Oct  '
     c*                  Eval      MySpace(11) = '  Nov  '
     c*                  Eval      MySpace(12) = '  Dec  '
     c*                  Eval      MySpace(13) = 'Receipts'
     c*
     c*                  Movea     MySpace       Zz_VaryTxt
     c*
     c*                  Movea     *Blanks       MySpace
     c*
     c*                  Eval      MySpace(01) = %Editc(CurR1:'P')
     c*                  Eval      MySpace(02) = %Editc(CurR2:'P')
     c*                  Eval      MySpace(03) = %Editc(CurR3:'P')
     c*                  Eval      MySpace(04) = %Editc(CurR4:'P')
     c*                  Eval      MySpace(05) = %Editc(CurR5:'P')
     c*                  Eval      MySpace(06) = %Editc(CurR6:'P')
     c*                  Eval      MySpace(07) = %Editc(CurR7:'P')
     c*                  Eval      MySpace(08) = %Editc(CurR8:'P')
     c*                  Eval      MySpace(09) = %Editc(CurR9:'P')
     c*                  Eval      MySpace(10) = %Editc(CurR10:'P')
     c*                  Eval      MySpace(11) = %Editc(CurR11:'P')
     c*                  Eval      MySpace(12) = %Editc(CurR12:'P')
     c*                  Eval      MySpace(13) = %Char(UYear + 2000)
     c*
     c*                  Eval      Ctr = UMonth
     c*                  Dow       Ctr <= 12
     c*                  Eval      MySpace(Ctr) = *Blanks
     c*                  Eval      Ctr = Ctr + 1
     c*                  EndDo
     c*                  Movea     MySpace       Zz_VaryTx2
     c*
     c*                  Eval      MySpace(01) = %Editc(LyR1:'P')
     c*                  Eval      MySpace(02) = %Editc(LyR2:'P')
     c*                  Eval      MySpace(03) = %Editc(LyR3:'P')
     c*                  Eval      MySpace(04) = %Editc(LyR4:'P')
     c*                  Eval      MySpace(05) = %Editc(LyR5:'P')
     c*                  Eval      MySpace(06) = %Editc(LyR6:'P')
     c*                  Eval      MySpace(07) = %Editc(LyR7:'P')
     c*                  Eval      MySpace(08) = %Editc(LyR8:'P')
     c*                  Eval      MySpace(09) = %Editc(LyR9:'P')
     c*                  Eval      MySpace(10) = %Editc(LyR10:'P')
     c*                  Eval      MySpace(11) = %Editc(LyR11:'P')
     c*                  Eval      MySpace(12) = %Editc(LyR12:'P')
     c*                  Eval      MySpace(13) = %Char(UYear + 1999)
     c*                  Movea     MySpace       Zz_VaryTx3
     c*
     c*                  Movea     *Blanks       MySpace
     c*
     c*                  Movea     MySpace       Zz_VaryTx4
     c*
     c*                  EndSr
     c*****************************************************************
     c*    @ClearStuff   Begsr
     c*                  Eval      CurR1  = *zeros
     c*                  Eval      CurR2  = *zeros
     c*                  Eval      CurR3  = *zeros
     c*                  Eval      CurR4  = *zeros
     c*                  Eval      CurR5  = *zeros
     c*                  Eval      CurR6  = *zeros
     c*                  Eval      CurR7  = *zeros
     c*                  Eval      CurR8  = *zeros
     c*                  Eval      CurR9  = *zeros
     c*                  Eval      CurR10 = *zeros
     c*                  Eval      CurR11 = *zeros
     c*                  Eval      CurR12 = *zeros
     c*
     c*                  Eval      CurS1  = *zeros
     c*                  Eval      CurS2  = *zeros
     c*                  Eval      CurS3  = *zeros
     c*                  Eval      CurS4  = *zeros
     c*                  Eval      CurS5  = *zeros
     c*                  Eval      CurS6  = *zeros
     c*                  Eval      CurS7  = *zeros
     c*                  Eval      CurS8  = *zeros
     c*                  Eval      CurS9  = *zeros
     c*                  Eval      CurS10 = *zeros
     c*                  Eval      CurS11 = *zeros
     c*                  Eval      CurS12 = *zeros
     c*                  Eval      LyR1  = *zeros
     c*                  Eval      LyR2  = *zeros
     c*                  Eval      LyR3  = *zeros
     c*                  Eval      LyR4  = *zeros
     c*                  Eval      LyR5  = *zeros
     c*                  Eval      LyR6  = *zeros
     c*                  Eval      LyR7  = *zeros
     c*                  Eval      LyR8  = *zeros
     c*                  Eval      LyR9  = *zeros
     c*                  Eval      LyR10 = *zeros
     c*                  Eval      LyR11 = *zeros
     c*                  Eval      LyR12 = *zeros
     c*
     c*                  Eval      LyS1  = *zeros
     c*                  Eval      LyS2  = *zeros
     c*                  Eval      LyS3  = *zeros
     c*                  Eval      LyS4  = *zeros
     c*                  Eval      LyS5  = *zeros
     c*                  Eval      LyS6  = *zeros
     c*                  Eval      LyS7  = *zeros
     c*                  Eval      LyS8  = *zeros
     c*                  Eval      LyS9  = *zeros
     c*                  Eval      LyS10 = *zeros
     c*                  Eval      LyS11 = *zeros
     c*                  Eval      LyS12 = *zeros
     c*                  EndSr
     c*****************************************************************
     c*    *Inzsr        Begsr
     c*
     c*    imkey         klist
     c*                  kfld                    kcomp
     c*                  kfld                    kprod
     c*
     c*                  Open      Beerl
     c*                  Open      InvMasL01
     c*                  Open      beerly
     c*                  Open      InvMasZ
     c*                  EndSr
     c*****************************************************************
