      *****************************************************************
     h copyright('(C) Copyright 1996 - 2014 King III Solutions, Inc.  +
     h Rel 5.1  2014-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2014 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_7140
      **   Type: ILE RPG Program
      **   Desc: Period End Batches - Close out old PEx batches
      **
      *****************************************************************
      **
      **  This program is used to Close out old 'PEx' batches.
      **
      **  For 'PEx' types, only keep current period period alive, checking
      **  forecast interval for 12,13,52.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 08/18/2014.
      *   Remarks. Altered program to use SQL cursor to select
      *            K_Prodseb records with passed company and status 'O'.
      *            Also, utilized SQL select statement to access file
      *            K_Company.
      *****************************************************************
      * -------------------------------------------------- Parameters passed
     d #once           s                   like(pb_comp)
     d PEx_old12       s                   like(pb_birth)
     d PEx_old13       s                   like(pb_birth)
     d PEx_old52       s                   like(pb_birth)
     d cmsysdate       s               d
      * ---------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * ------------------------------------------------------
     d prodseb_rec   e ds                  ExtName(k_prodseb)
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_7140        PI
     d  comp                          1
     d  per12                         1
     d  per13                         1
     d  per52                         1
      * ---------------------------------------------------
      /free

       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;
       //------------------------------------------------------ Once Routine
       //once routine
       if #once   = ' ';
          #once   = 'Y';
          exec sql
           select cm_sysdate
             into :cmsysdate
             from k_company
             where cm_comp = :comp
             fetch first row only;
          PEx_old12 = cmsysdate - %days(01);
          PEx_old13 = cmsysdate - %days(01);
          PEx_old52 = cmsysdate - %days(01);
       endif;

       //-------------------------------------- read through 'O' Open batches

       exsr dclpbcursor;
       exsr opnpbcursor;

       dow SQLState = SQLStateOk;

           exec sql
            fetch next
            from pbcursor
            into :prodseb_rec;

            if SQLState = RowNotFound;
               Leave;
            endif;

       //  PE batches
           if %subst(pb_reqtype:1:2) = 'PE';

             if pb_forcint = 12 and
                pb_birth < PEx_old12 and
                per12 = '1';
                  pb_status = 'C';
                  pb_lastupd = cmsysdate;
                  exsr updtprodseb;
             endif;

             if pb_forcint = 13 and
                pb_birth < PEx_old13 and
                per13 = '1';
                pb_status = 'C';
                pb_lastupd = cmsysdate;
                exsr updtprodseb;
             endif;

             if pb_forcint = 52 and
                pb_birth < PEx_old52 and
                per52 = '1';
                pb_status = 'C';
                pb_lastupd = cmsysdate;
                exsr updtprodseb;
             endif;

           endif;

       enddo;

       exsr clspbcursor;

       *inlr = *on;

       begsr dclpbcursor;
       exec sql
        declare pbcursor Cursor
          for
        select *
          from k_prodseb
          where pb_comp = :comp and
                pb_status = 'O'
          order by pb_comp,
                   pb_status,
                   pb_actbuyr,
                   pb_actuser;
       endsr;

       begsr opnpbcursor;
       exec sql
        open pbcursor;
        if SQLState <> SQLStateOk;
           exsr clspbcursor;
           exec sql
            open pbcursor;
        endif;
       endsr;

       begsr clspbcursor;
       exec sql
        close pbcursor;
       endsr;

       begsr updtprodseb;
       exec sql
       update k_prodseb
        set pb_status = :pb_status,
            pb_lastupd = :pb_lastupd
        where current of pbcursor;

        endsr;
      /end-free
