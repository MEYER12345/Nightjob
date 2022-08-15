      *****************************************************************
     h copyright('(C) Copyright 1996 - 2016 King III Solutions, Inc.  +
     h Rel 5.3  2016-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)
     h BNDDIR('K3S_BNDDIR')
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
      **   Name: K3S_8530
      **   Type: ILE RPG Program
      **   Desc: Update K_INTPROD with transfer supplier information
      **
      *****************************************************************
      **
      **  This program is used to update the k_intprod file by
      **  changing the regular supplier ID to a transfer supplier ID.
      **  This program is part of the night job, and would run before
      **  K3S_9010.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 06/19/2014.
      *   Remarks. Changed this program to utilize an SQL cursor to
      *            loop through file K_INTPROD.  Also, changed it to
      *            use SQL select statements to access K_TRANSPL,
      *            K_TRANCEN, K_TRANCPY, and K_TRANPRD files.  In
      *            addition, added SQL insert statement to add
      *            K_TRANCPY record if one does not already exist.
      *****************************************************************
      * ----------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * ----------------------------------------------------- procedure interface
     d K3S_8530        PI
     d  comp                          1
      * -----------------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * --------------------------------------------------------- Workfields
     d time_stamp      s               z   inz
     d #once           s              1  0                                      once routine
     d w_count         s              5  0                                      count
     d copied          s             10                                         copied date
     d #locn           s                   like(ip_locn)                        location
     d #supl           s                   like(ip_supl)                        supplier
     d #suplsub        s                   like(ip_suplsub)                     supplier sub
     d #prod           s                   like(ip_prod)                        product
     d logtype         s              1
     d user            s             10
     d program         s             10
     d time            s               t   timfmt(*iso)
     d date            s               d   datfmt(*iso)

     d time20          s             20  0
     d time9           s              9  0
     d time3           s              3  0
      * -------------------------------------- Likerec Statements
     d intprod_rec   e ds                  ExtName(k_intprod)
      * ------------------------------------------------------- Once Routine
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       //once routine
       if #once <> 1;
         #once = 1;

       //call module to retrieve timestamp
         callp K3S_Retrieve_Timestamp(time_stamp);

       endif;

       //----------------------------------------------------- Read Products
       exsr dclipcursor;
       exsr opnipcursor;

       //--------------------------------------------------------- Main Loop
       //main loop
       dow SQLState = SQLStateOk;

       //fetch first product interface row
          exec sql
           fetch next
            from ipcursor
            into :intprod_rec;

          if SQLState <> SQLStateOk;
            leave;
          endif;

          #locn = ip_locn;
          #supl = ip_supl;
          #suplsub = ip_suplsub;
          #prod = ip_prod;
       //----------------------------------------------------- supplier test
       enddo;
       //-------------------------------------------------- End of Main Loop

       exsr clsipcursor;

       //finished, set on LR
         *inlr = *on;

       begsr dclipcursor;
       exec sql
        declare ipcursor cursor
         for
         select *
         from k_intprod
         where ip_comp = :comp and
               ip_prod = '720843';
       endsr;

       begsr opnipcursor;
       exec sql
        open ipcursor;
       endsr;

       begsr clsipcursor;
       exec sql
        close ipcursor;
       endsr;
      /end-free
      * ***************************************************** End of program

