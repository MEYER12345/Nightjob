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
      *****************************************************************
      **
      *****************************************************************

     fk_productaif   e           k disk    infds(infds) infsr(k3s_infsr)
     f                                     usropn
      * products by locn, supplier, sub supplier, prod

     d comp            s                   like(pr_comp)
     d buyr            s                   like(pr_buyr)
     d locn            s                   like(pr_locn)
     d supl            s                   like(pr_supl)
     d suplsub         s                   like(pr_suplsub)
     d prod            s                   like(pr_prod)
     d altsrce         s                   like(pr_altsrce)
     d statusa         s              5a
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
     d show4_2nd       s             25a   inz('--------------------- thi')
     d show4_3rd       s             25a   inz('s is the 4th line under y')
     d show4_4th       s             25a   inz('our control -------------')
     d show4_5th       s             26a   inz('-------------------------X')
     dzz_vary          ds
     dzz_vary01                      13    inz('          TP ')
     dzz_vary02                      13    inz('          ASO')
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
      * parameters passed to program
     c     *entry        plist
     c                   parm                    comp
     c                   parm                    buyr
     c                   parm                    locn
     c                   parm                    supl
     c                   parm                    suplsub
     c                   parm                    prod
     c                   parm                    altsrce
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
     c                   move      pr_trnoord    pr_num
     c                   exsr      $_pr_numb
     c                   movel     pr_alp        zz_vary01
     c                   move      pr_altoord    pr_num
     c                   exsr      $_pr_numb
     c                   movel     pr_alp        zz_vary02

     c                   eval      zz_varytxt = *blanks
     c                   movel     zz_vary       zz_varytxt

     c                   eval      zz_varytxt = show1_1st + show1_2nd +
     c                             show1_3rd + show1_4th + show1_5th

     c                   eval      zz_varytx2 = *blanks

     c                   eval      zz_varytx2 = show2_1st + show2_2nd +
     c                             show2_3rd + show2_4th + show2_5th

     c                   eval      zz_varytx3 = *blanks

     c                   eval      zz_varytx3 = show3_1st + show3_2nd +
     c                             show3_3rd + show3_4th + show3_5th

     c                   eval      zz_varytx4 = *blanks

     c                   eval      zz_varytx4 = show4_1st + show4_2nd +
     c                             show4_3rd + show4_4th + show4_5th

     c                   else
     c                   exsr      k3s_infsr
     c                   endif
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
