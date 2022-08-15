      *****************************************************************
     h copyright('(C) Copyright 1996 - 2016 King III Solutions, Inc.  +
     h Rel 5.3  2016-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2016 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_2005
      **   Type: ILE RPG Program
      **   Desc: Clear active products counter at night
      **
      *****************************************************************
      **
      **  This program will simply clear the active products counter
      **  at night for regular suppliers. Program K3S_1500 will reset this
      **  value.
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 06/18/2014.
      **  Remarks. Altered program to utilize SQL select statement to
      **           access K_ COMPANY record. Also, added an SQL cursor
      **           to loop through K_SUPLIER file and update certain
      **           fields if certain criteria apply.
      *****************************************************************
     d td_fxcnxt       s               d                                        date variable
     d today           s               d                                        date variable
     d cmsysdate       s               d                                        date variable
     d one_yr_ago      s               d                                        date variables
     ddatestruc1       ds
     dwrkdate                         6s 0
     d #month                         2s 0 overlay(wrkdate)
     d #day                           2s 0 overlay(wrkdate:3)
     d #year                          2s 0 overlay(wrkdate:5)
      * ----------------------------------------------------  parameter passed prototype
     d/copy k3s_proto
      * ----------------------------------------------------- procedure interface
     d K3S_2005        PI
     d  comp                          1
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * --------------------------------------------------------- Workfields
     d suplier_rec   e ds                  ExtName(k_suplier)
      * --------------------------------------------------------- Workfields
      /free

       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;
       exec sql
         select cm_sysdate
            into :cmsysdate
            from k_company
            where cm_comp = :comp
            fetch first row only;
       If SQLState = SQLStateOk;                     //Successfully found
         today = cmsysdate;
         one_yr_ago = %date();
         one_yr_ago = one_yr_ago - %years(1);
       endif;

       //--------------------------------------------------------- Main Loop
       // main loop using fetch

       exsr dclspcursor;
       exsr opnspcursor;

       dow SQLState = SQLStateOk;
         exec sql
          fetch next
           from spcursor
           into :suplier_rec;
         If SQLState = RowNotFound;
           leave;
         endif;
         if sp_comp = comp AND
            sp_altsrce <> 1;
      //we want to stop prompting 'Order Past Due' if the supplier
      //has not been ordered for over a year, or never ordered at all.
            if sp_ordate < one_yr_ago;
               clear sp_usern1;
            endif;
      //Did this supplier not have any products since last
      //K3S night job? If no active products, clear past due counter.
      //Remember, active products get cleared a few statements down,
      //then rebuilt in K3S_1500
            if sp_actprod = 0;
               clear sp_usern1;
            endif;

            clear sp_actprod;

            //This will ensure that suppliers with no active products
            //get their sequence value reset to 1.
            if sp_soqseq# > 1;
               sp_soqseq# = 1;
            endif;

         //Fix fixed cycle date if less than todays date and
         //new date is equal to todays date
            if sp_fxcfrq > 0 and
               sp_fxcnxt < today;
                  td_fxcnxt = sp_fxcnxt;
                  dow td_fxcnxt < today;
                     select;
                       when sp_fxcfrq = 1;
                          td_fxcnxt += %days(7);
                       when sp_fxcfrq = 2;
                          td_fxcnxt += %days(14);
                       when sp_fxcfrq = 3;
                          td_fxcnxt += %days(21);
                       when sp_fxcfrq = 4;
                          td_fxcnxt += %days(28);
                       when sp_fxcfrq = 5;
                          td_fxcnxt += %days(35);
                       when sp_fxcfrq = 6;
                          td_fxcnxt += %days(42);
                     endsl;
                  enddo;
                  if td_fxcnxt = today;
                     sp_fxcnxt = td_fxcnxt;
                  endif;
            endif;
            exsr updtsupl;
         endif;
       enddo;
       exsr clsspcursor;
       *inlr = *on;

       begsr dclspcursor;
       exec sql
        declare spcursor Cursor
          for
        select *
          from k_suplier
          for update of sp_fxcnxt,
                        sp_soqseq#,
                        sp_actprod,
                        sp_usern1;
       endsr;

       begsr opnspcursor;
       exec sql
        open spcursor;
        if SQLState <> SQLStateOk;
           exsr clsspcursor;
           exec sql
            open spcursor;
        endif;
       endsr;

       begsr clsspcursor;
       exec sql
        close spcursor;
       endsr;

       begsr updtsupl;
       exec sql
        update k_suplier
         Set sp_fxcnxt = :sp_fxcnxt,
             sp_soqseq# = :sp_soqseq#,
             sp_actprod = :sp_actprod
         Where current of spcursor;
       endsr;

      /end-free
