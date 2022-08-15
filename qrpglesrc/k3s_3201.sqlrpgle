      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO)
      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2008 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_3021
      **   Type: SQLRPGLE program
      **   Desc: Write K_PRODFOR file PF_CHGTYPE = N records from X records
      **
      *********************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 07/02/2021.
      *   Remarks.
      *
      *********************************************************************
      * ----------------------------------------------------------
     d prodfor_rec   e ds                  ExtName(k_prodfor)
      * ----------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * ----------------------------------------------------------
     d cmsysdate       s               d
     d prdesc1         s             40
      * --------------------------------------------------- prototype
     d K3S_3201        PR                  EXTPGM('K3S_3201')
     d  comp                          1
      * --------------------------------------------------- procedure interface
     d K3S_3201        PI
     d  comp                          1
      * ---------------------------------------------------------- Begin
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endmod;

       exec sql
          select cm_sysdate
             into :cmsysdate   ---host variable to hold date from k_company---
             from k_company
             where cm_comp = :comp ---select k_company row based on parm passed---
             fetch first row only;

       exsr dclpfcursor;  //declare cursor
       exsr opnpfcursor;  //open cursor

       //--------------------------------------------------------- Main Loop
       // main loop using fetch
       dow SQLSTT = SQLStateOk;

        //fetch schedule record to be used for next system date
          exec sql
           fetch next
            from pfcursor
            into :prodfor_rec;

          if SQLState = RowNotFound;
            leave;
          endif;

          if pf_birth = cmsysdate;
             pf_chgtype = 'N';
             exec sql
                select pr_desc1
                   into :prdesc1
                   from k_product
                   where pr_comp = :comp and
                         pr_locn = :pf_locn and
                         pr_supl = :pf_supl and
                         pr_suplsub = :pf_suplsub and
                         pr_prod = :pf_prod
                   fetch first row only;
             if SQLState = SQLStateOk;
                pf_chgdesc = prdesc1;
             endif;

             exec sql
                insert into k_prodfor
                values (:prodfor_rec);

          endif;
       enddo;
       exsr clspfcursor;

       *inlr = *on;

       begsr dclpfcursor;
       exec sql
        declare pfcursor Cursor
         for
         select *  ---select all columns in k_prodfor rows---
         from k_prodfor
         where pf_comp = :comp and
               pf_chgtype = 'X';
       endsr;

       begsr opnpfcursor;
       exec sql
         open pfcursor;
       endsr;

       begsr clspfcursor;
       exec sql
         close pfcursor;
       endsr;
       /end-free
