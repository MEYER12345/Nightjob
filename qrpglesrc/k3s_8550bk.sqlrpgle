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
      **   Name: K3S_8550
      **   Type: ILE RPG Program
      **   Desc: Transfer supplier copy of history and forecast
      **
      *****************************************************************
      **
      **  This program is used to copy the history and forecast for
      **  products when they get established in a transfer supplier for
      **  the first time. It will call K3S_8540 for products where
      **  the copied date is '0001-01-01'.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 06/19/2014.
      *   Remarks. Changed this program to utilize an SQL cursor to
      *            loop through file K_TRANCPY.  Also, changed it to
      *            use an SQL update statement to update the column
      *            TH_COPIED with the current date. In addition,
      *            deleted the call to K3S_M090 to get the time stamp
      *            from which to pull the current date and replaced
      *            it with the %DATE() RPG IV function.
      *****************************************************************
      * --------------------------------------------------------- Workfields
     d time_stamp      s               z   inz
     d #once           s              1  0                                      once routine
     d multiplier      s              9  4 inz(1)                               history multiplier
     d hist_type       s              1  0 inz(0)                               history type
     d copied          s             10                                         copied date
     d logtype         s              1
     d user            s             10
     d program         s             10
     d time            s               t   timfmt(*iso)
     d date            s               d   datfmt(*iso)

      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_8550        PI
     d  comp                          1
     d  clr_date                       d
      * --------------------------------------------------------- Workfields
     d trancpy_rec   e ds                  ExtName(k_trancpy)
      * ------------------------------------------------------- Once Routine
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                   closqlcsr = *endactgrp;

       //----------------------------------------------------- get timestamp
       //call module to retrieve timestamp
       callp K3S_Retrieve_Timestamp(time_stamp);

       exsr dclthcursor1;
       if SQLState = SQLStateOk;
       //exsr clsthcursor1;
       if SQLState = SQLStateOk;
       exsr opnthcursor1;

       //----------------------------------------- read copy history records
       dow SQLState = SQLStateOk;
       //--------------------------------------------------------- Main Loop
       //main loop

       //transfer system copy history log
       //fetch copy history record

          exec sql
           fetch next
            from thcursor1
            into :trancpy_rec;

          if SQLState = RowNotFound;
            leave;
          endif;

          copied = %char(th_copied:*ISO);

          if copied = '0001-01-01';

       //    copy history and forecast
             callp K3S_8540(th_comp:
                            th_locnto:
                            th_tsupl:
                            th_tsuplsb:
                            th_prod:
                            th_locnfrm:
                            th_rsupl:
                            th_rsuplsb:
                            th_prod:
                            multiplier:
                            hist_type);
             th_copied = %date(%subst(%char(time_stamp):1:10):*ISO);
             exsr updttrancpy;

i            exsr $_log_it;

          endif;
       enddo;
       endif;
       endif;
       exsr clsthcursor1;
       //finished, set on LR
       *inlr = *on;

       begsr dclthcursor1;
       exec sql
        declare thcursor1 Cursor
          for
        select *
          from k_trancpy
          where th_comp = :comp and
                th_copied = :clr_date
          order by th_comp,
                   th_locnfrm,
                   th_locnto,
                   th_prod
          for update of th_copied;

       if SQLState = SQLStateOk;

       endif;
       endsr;

       begsr opnthcursor1;
       exec sql
        open thcursor1;
       endsr;

       begsr clsthcursor1;
       exec sql
        close thcursor1;
       endsr;

       begsr updttrancpy;
       exec sql
        update k_trancpy
         set th_copied = :th_copied
         where current of thcursor1;
       endsr;

       begsr $_log_it;

       logtype = 'C';
       user = 'NIGHT JOB';
       program = 'K3S_8550';
       callp K3S_8557(th_comp:
                      logtype:
                      user:
                      program:
                      time_stamp:
                      th_locnfrm:
                      th_locnto:
                      th_rsupl:
                      th_rsuplsb:
                      th_tsupl:
                      th_tsuplsb:
                      th_prod:
                      th_birth:
                      th_lastupd:
                      th_copied);
       endsr;
      /end-free
       //**************************************************** End of program

