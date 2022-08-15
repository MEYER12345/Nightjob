      *****************************************************************
     h copyright('(C) Copyright 1996 - 2005 King III Solutions, Inc.  +
     h Rel 4.36 2005-07-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') AUT(*ALL)

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2005 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_9940
      **   Type: ILE RPG Program
      **   Desc: Control vary text line 1040/41
      **
      *****************************************************************
      **
      **  This program will receive and build vary tetxt line.
      **
      **  06/17/10 M Schrimshaw - remove qualifiers
      **  09/23/10 Blue Keeling - add fob
      **  10/07/10 Blue Keeling - Use PostCML
      **  11/29/10 Blue Keeling - Add PostNML
      **  05/07/12 M Schrimshaw - replace invmasl with invmasl01
      **  02/18/13 M Schrimshaw - replace fobweblf with fobwebl01
      **  02/27/13 M Schrimshaw - replace postcml with postcml01
      **  02/27/13 M Schrimshaw - replace postnml with keyed postnm
      **  11/20/13 M Schrimshaw - remove adjusted cost and adjusted fob
      **  10/06/15 Trish Henley - add color(pink) and add distress
      **                          /closeout to K3S text line 4
      **  12/22/15 M Schrimshaw - troubl field cannot look for blank anymore
      *****************************************************************
      **
      *****************************************************************

     fk_productaif   e           k disk    infds(infds) infsr(k3s_infsr)
     f                                     usropn
      * products by locn, supplier, sub supplier, prod

      *Jarboe
     D Pr_Prod         Ds
     D  Pr_Prod6               1      6
     D Ctr             S              3  0
     D Cmd             S            512
     D Cmdlen          S             15  5 Inz(512)
     D Yourspace       s              5  0
     D Clr_Blue        S              1    Inz(X'3A')
     D Clr_Green       S              1    Inz(X'20')
     D Clr_Red         S              1    Inz(X'28')
     D Clr_Pink        S              1    Inz(X'38')
     D Color1          S              1
     D Color2          S              1
     D Color3          S              1
     D FobWord1        S              6
     D FobWord2        S              6
     D Beer_FBamt      S             10
     D Dist_close      S              8

     D  Myspace        S             10    Dim(13)

     d comp            s                   like(pr_comp)
     d buyr            s                   like(pr_buyr)
     d locn            s                   like(pr_locn)
     d supl            s                   like(pr_supl)
     d suplsub         s                   like(pr_suplsub)
     d prod            s                   like(pr_prod)
     d altsrce         s                   like(pr_altsrce)
     d statusa         s              5a
     d R6_user         s             10a
     d zz_varytxt      s            126a
     d show1_1st       s             25a   inz('X------------------------')
     d show1_2nd       s             25a   inz('--------------------- thi')
     d show1_3rd       s             25a   inz('s line is under your cont')
     d show1_4th       s             25a   inz('rol ---------------------')
     d show1_5th       s             26a   inz('-------------------------X')
     d zz_varytx2      s            126a
     d show2_1st       s             25a   inz('X------------------------')
     d show2_2nd       s             25a   inz('--------------------- thi')
     d show2_3rd       s             25a   inz('s is the 2nd line under y')
     d show2_4th       s             25a   inz('our control -------------')
     d show2_5th       s             26a   inz('-------------------------X')
     d zz_varytx3      s            126a
     d show3_1st       s             25a   inz('X------------------------')
     d show3_2nd       s             25a   inz('--------------------- thi')
     d show3_3rd       s             25a   inz('s is the 3rd line under y')
     d show3_4th       s             25a   inz('our control -------------')
     d show3_5th       s             26a   inz('-------------------------X')
     d zz_varytx4      s            126a
     d show4_1st       s             25a   inz('X------------------------')
     d show4_2nd       s             25a   inz('--------------------- I h')
     d show4_3rd       s             25a   inz('ave changed this 4th line')
     d show4_4th       s             25a   inz(' different --------------')
     d show4_5th       s             26a   inz('-------------------------X')
     dzz_vary          ds
     dzz_vary01                      13    inz('             ')
     d*zz_vary02                      13    inz('          ASO')
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
     D status            *status
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
     c*******            parm                    R6_user
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
     c*                   move      pr_trnoord    pr_num
     c*                   exsr      $_pr_numb
     c*                   movel     pr_alp        zz_vary01
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
     C*******            ExSr      Jarboestuff
     c                   endif
     c                   return
      * //////////////////////////////////////////////// Edit PB numbers

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
     c*                  Eval      Zz_VaryTx4 = *blanks
     c*                  Select
     c*                  When      %Subst(Pr_Prod:1:1) > '0'
     c*                  Exsr      @GetJarboe
     c*                  Other
     c*                  Exsr      @GetIBC
     c*                  EndSl
     c*                  Endsr
     c*    @GetJarboe    Begsr
     c*    Pr_Prod6      Chain     InvmasL01
     c*                  If        %Found(InvMasL01)
     c*                  Exsr      @FillParms
     c*                  select
     c*                  when      troubl = 'C'
     c*                  eval      dist_close = 'CLOSEOUT'
     c*                  eval      color3 = clr_pink
     c*                  when      troubl = 'Y'
     c*                  eval      dist_close = 'DISTRESS'
     c*                  eval      color3 = clr_pink
     c*                  other
     c*                  eval      dist_close = *blanks
     c*                  eval      color3 = *blanks
     c*                  endsl
     c*                  EndIf
     c*    Pr_Prod6      Chain     FobWebL01
     c*                  If        Not %Found(FobWebL01)
     c*                  Eval      NmFBamt = *Zeros
     c*                  Eval      NmHdrDsc = *Blanks
     c*                  Eval      FobWord1 = *Blanks
     c*                  Else
     c*                  Eval      FobWord1 = ' Fob='
     c*                  EndIf
     c*                  Select
     c*                  When      NmFbAmt < *zeros
     c*                  Eval      Color1 = Clr_Blue
     c*                  When      NmFbAmt > *zeros
     c*                  Eval      Color1 = Clr_Red
     c*                  Other
     c*                  Eval      Color1 = *Blanks
     c*                  EndSl
     c*
     c*    Pr_Prod6      Chain     FobWebPm
     c*                  If        Not %Found(FobWebPm)
     c*                  Eval      TmFBamt = *Zeros
     c*                  Eval      TmHdrDsc = *Blanks
     c*                  Eval      FobWord2 = *Blanks
     c*                  Else
     c*                  Eval      FobWord2 = ' Fob='
     c*                  EndIf
     c*                  Select
     c*                  When      TmFbAmt < *zeros
     c*                  Eval      Color2 = Clr_Blue
     c*                  When      TmFbAmt > *zeros
     c*                  Eval      Color2 = Clr_Red
     c*                  Other
     c*                  Eval      Color2 = *Blanks
     c*                  EndSl
     c*
     c*                  If        TmFbAmt <> *Zeros
     c*                             or NmFbAmt <> *Zeros
     c*                             or Troubl = 'C'
     c*                             or Troubl = 'Y'
     c*                  Eval      Zz_VaryTx4 = Color2
     c*                             + %Trim(TmHdrDsc)
     c*                             + Fobword2
     c*                             + %Editc(TmFbAmt:'P')
     c*                             + ' *************'
     c*                             + Color1
     c*                             + %Trim(NmHdrDsc)
     c*                             + FobWord1
     c*                             + %Editc(NmFbAmt:'P')
     c*                             + Color3
     c*                             + %Trim(dist_close)
     c*                  Else
     c*                  Eval      Zz_VaryTx4 = *Blanks
     c*                  EndIf
     c*
     c*                  EndSr
     c*
     c*    @GetIBC       Begsr
     c*    Pr_Prod6      Chain     BeerL
     c*    Pr_Prod6      Chain     postcml01
     c*                  If        Not %Found(PostCml01)
     c*                  Eval      OrgFob = *Zeros
     c*                  Eval      HstFb1 = *Zeros
     c*                  EndIf
     c*
     c*    Pr_Prod6      Chain     postnm
     c*                  If        Not %Found(postnm )
     c*                  Eval      NmOrgFob = *Zeros
     c*                  Eval      NmHstFb1 = *Zeros
     c*                  EndIf
     c*
     c*                  Select
     c*                  When      (OrgFob - HstFb1) < *Zeros
     c*                  Eval      FobWord1 = 'Down'
     c*                  Eval      Color1 = Clr_Blue
     c*
     c*                  When      (OrgFob - HstFb1) > *Zeros
     c*                  Eval      Color1 = Clr_Red
     c*                  Eval      FobWord1 = 'Up'
     c*                  Other
     c*                  Eval      FobWord1 = 'No Chg'
     c*                  Eval      Color1 = Clr_Blue
     c*                  endSl
     c*
     c*                  Select
     c*                  When      (NmOrgFob - NmHstFb1) < *Zeros
     c*                  Eval      FobWord2 = 'Down'
     c*                  Eval      Color2 = Clr_Blue
     c*
     c*                  When      (NmOrgFob - NmHstFb1) > *Zeros
     c*                  Eval      Color2 = Clr_Red
     c*                  Eval      FobWord2 = 'Up'
     c*                  Other
     c*                  Eval      FobWord2 = 'No Chg'
     c*                  Eval      Color2 = Clr_Blue
     c*                  endSl
     c*                  Eval      Zz_VaryTx4 = Color1
     c*                               + 'Current: '
     c*                               + %Trim(%EditC((OrgFob - HstFb1):'P'))
     c*                               + ' '
     c*                               + FobWord1
     c*                               + '    '
     c*                               + Color2
     c*                               + 'Next Month: '
     c*                               + %Trim(%EditC((NmOrgFob - NmHstFb1):'P'))
     c*                               + ' '
     c*                               + FobWord2
     c*
     c*                  If        %Found(BeerL)
     c*                  Exsr      @FillParms
     c*                  EndIf
     c*                  EndSr
     c*
     c*    @FillParms    Begsr
     c*                  Eval      MySpace(01) = 'Jan, '
     c*                  Eval      MySpace(02) = 'Feb, '
     c*                  Eval      MySpace(03) = 'Mar, '
     c*                  Eval      MySpace(04) = 'Apr, '
     c*                  Eval      MySpace(05) = 'May, '
     c*                  Eval      MySpace(06) = 'Jun, '
     c*                  Eval      MySpace(07) = 'Jul, '
     c*                  Eval      MySpace(08) = 'Aug, '
     c*                  Eval      MySpace(09) = 'Sep, '
     c*                  Eval      MySpace(10) = 'Oct, '
     c*                  Eval      MySpace(11) = 'Nov, '
     c*                  Eval      MySpace(12) = 'Dec, '
     c*
     c*                  Eval      Ctr = 1
     c*                  Do        12
     c*                  If        Ctr > Umonth
     c*                  Eval      MySpace(Ctr) = %Trim(MySpace(Ctr)) +
     c*                                      %Char(UYear - 1 + 2000)
     c*                  Else
     c*                  Eval      MySpace(Ctr) = %Trim(MySpace(Ctr)) +
     c*                                      %Char(UYear + 2000)
     c*                  EndIf
     c*
     c*                  Eval      Ctr = Ctr + 1
     c*                  EndDo
     c*                  Movea     MySpace       Zz_VaryTxt
     c*
     c*                  Movea     *Blanks       MySpace
     c*
     c*                  If        Btcs < 1
     c*                  Eval      Btcs = 1
     c*                  Endif
     c*                  Eval(h)   YourSpace   = (S1 / btcs)
     c*                  Eval      MySpace(01) = %Editc(YourSpace:'P')
     c*
     c*                  Eval(h)   YourSpace   = (S2 / btcs)
     c*                  Eval      MySpace(02) = %Editc(YourSpace:'P')
     c*
     c*                  Eval(h)   YourSpace   = (S3 / btcs)
     c*                  Eval      MySpace(03) = %Editc(YourSpace:'P')
     c*
     c*                  Eval(h)   YourSpace   = (S4 / btcs)
     c*                  Eval      MySpace(04) = %Editc(YourSpace:'P')
     c*
     c*                  Eval(h)   YourSpace   = (S5 / btcs)
     c*                  Eval      MySpace(05) = %Editc(YourSpace:'P')
     c*
     c*                  Eval(h)   YourSpace   = (S6 / btcs)
     c*                  Eval      MySpace(06) = %Editc(YourSpace:'P')
     c*
     c*                  Eval(h)   YourSpace   = (S7 / btcs)
     c*                  Eval      MySpace(07) = %Editc(YourSpace:'P')
     c*
     c*                  Eval(h)   YourSpace   = (S8 / btcs)
     c*                  Eval      MySpace(08) = %Editc(YourSpace:'P')
     c*
     c*                  Eval(h)   YourSpace   = (S9 / btcs)
     c*                  Eval      MySpace(09) = %Editc(YourSpace:'P')
     c*
     c*                  Eval(h)   YourSpace   = (S10/ btcs)
     c*                  Eval      MySpace(10) = %Editc(YourSpace:'P')
     c*
     c*                  Eval(h)   YourSpace   = (S11/ btcs)
     c*                  Eval      MySpace(11) = %Editc(YourSpace:'P')
     c*
     c*                  Eval(h)   YourSpace   = (S12/ btcs)
     c*                  Eval      MySpace(12) = %Editc(YourSpace:'P')
     c*
     c*                  Eval      MySpace(13) = '  Sales'
     c*                  Movea     MySpace       Zz_VaryTx2
     c*
     c*                  Eval      Zz_VaryTx4 = *blanks
     c*
     c*                  Movea     *Blanks       MySpace
     c*
     c*                  Eval      MySpace(01) = %Editc(R1:'P')
     c*                  Eval      MySpace(02) = %Editc(R2:'P')
     c*                  Eval      MySpace(03) = %Editc(R3:'P')
     c*                  Eval      MySpace(04) = %Editc(R4:'P')
     c*                  Eval      MySpace(05) = %Editc(R5:'P')
     c*                  Eval      MySpace(06) = %Editc(R6:'P')
     c*                  Eval      MySpace(07) = %Editc(R7:'P')
     c*                  Eval      MySpace(08) = %Editc(R8:'P')
     c*                  Eval      MySpace(09) = %Editc(R9:'P')
     c*                  Eval      MySpace(10) = %Editc(R10:'P')
     c*                  Eval      MySpace(11) = %Editc(R11:'P')
     c*                  Eval      MySpace(12) = %Editc(R12:'P')
     c*                  Eval      MySpace(13) = 'Receipts'
     c*                  Movea     MySpace       Zz_VaryTx3
     c*
     c*                  EndSr
     c*    *Inzsr        Begsr
     c*
     c*
     c*                  EVAL      CMD = 'OVRDBF FILE(BEERL) '  +
     c*                                  ' TOFILE(IBC/BEERL)'
     c*                  Call      'QCMDEXC'                            99
     c*                  Parm                    Cmd
     c*                  Parm                    CmdLen
     c*
     c*                  EVAL      CMD = 'OVRDBF FILE(INVMASL) '  +
     c*                                  ' TOFILE(JARBOE/INVMASL)'
     c*                  Call      'QCMDEXC'                            99
     c*                  Parm                    Cmd
     c*                  Parm                    CmdLen
     c*
     c*                  Open      Beerl
     c*                  Open      InvMasL01
     c*                  EndSr
