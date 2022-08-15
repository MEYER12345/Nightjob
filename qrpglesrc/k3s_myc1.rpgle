      *****************************************************************
     h copyright('(C) Copyright 1996 - 2003 King III Solutions, Inc.  +
     h Rel 4.32 2003-07-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*YES)
     h option(*nodebugio)

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2003 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_MYC1
      **   Type: ILE RPG Program
      **   Desc: Identify Special Orders and write to K_PRODHLD file
      **
      *****************************************************************
      **
      **  Developed for Myers-Cox, this program will identify Special Order
      **  and then place them into our Hold Out file. These will then be
      **  picked up and coded to be in U1 check.
      **
      *****************************************************************

     fk_intdalydif   e           k disk
     fk_productbif   e           k disk
     fk_prodhld o    e             disk

     d comp            s              1    inz('M')
     d birth           s                   like(ie_birth)
     d end             s                   like(ie_birth)

     d time_stamp      s               z   inz
     d time            s               t   timfmt(*iso)
     d date            s               d   datfmt(*iso)

     d chgtype         s              1
     d program         s             10
     d user            s             10
     d workstn         s             10

      * key list for Daily Sales
     c     ie_key        klist
     c                   kfld                    comp
     c                   kfld                    birth
     c
     c* key list for Product file
     c     pr_key        klist
     c                   kfld                    ie_comp
     c                   kfld                    ie_locn
     c                   kfld                    ie_supl
     c                   kfld                    ie_suplsub
     c                   kfld                    ie_prod
     c
     c     *mdy          move      udate         birth
     c*************      subdur    01:*days      birth
     c     *mdy          move      udate         end
     c                   adddur    14:*days      end

     c     ie_key        setll     rk_intdaly
     c                   dou       %eof(k_intdalyd)
     c     ie_key        reade     rk_intdaly
     c                   if        not %eof(k_intdalyd)

      ***  look for special order criteria, else get out
     c                   if        ie_dlysale = 0 and
     c                             ie_dlyouts > 0
     c                   else
     c                   iter
     c                   endif

     c*** test to see if Special Order product
     c
     c     pr_key        chain     rk_product
     c                   if        %found and
     c                             %subst(pr_desc1:30:11) = 'Special Ord'


     c                   eval      pu_comp    = ie_comp
     c                   eval      pu_locn    = ie_locn
     c                   eval      pu_supl    = ie_supl
     c                   eval      pu_suplsub = ie_suplsub
     c                   eval      pu_suplusr = pr_suplusr
     c                   eval      pu_suplusb = pr_suplusb
     c                   eval      pu_prod    = ie_prod
     c                   eval      pu_birth   = ie_birth
     c                   eval      pu_lastupd = ie_birth
     c                   eval      pu_begin   = ie_birth
     c                   eval      pu_end     = end
     c                   eval      pu_hldqty  = ie_dlyouts
     c                   eval      pu_hldreas = ie_custnam
     c                   move      pu_birth      pu_timestp
     c                   eval      pu_po#     = ie_order#
     c                   eval      pu_source  = 'X'

     c                   write     rk_prodhld
     c
     c                   call      'K3S_M090'
     c                   parm                    time_stamp
     c                   move      time_stamp    time
     c                   move      time_stamp    date
     c
     c                   call      'K3S_3056'
     c                   parm                    pu_comp
     c                   parm      'A'           chgtype
     c                   parm                    date
     c                   parm                    time
     c                   parm      'MYC_KING3'   user
     c                   parm      'K3S_NIGHT'   workstn
     c                   parm      'K3S_MYC1'    program
     c                   parm                    pu_locn
     c                   parm                    pu_supl
     c                   parm                    pu_suplsub
     c                   parm                    pu_begin
     c                   parm                    pu_end
     c                   parm                    pu_hldreas
     c                   parm                    pu_prod
     c                   parm                    pu_hldqty
     c                   parm                    pu_po#
     c
     c                   endif
     c                   endif
     c                   enddo
     c
     c                   eval      *inlr = *on

