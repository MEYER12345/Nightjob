      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)

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
      **   Name: K3S_8560
      **   Type: ILE RPG Program
      **   Desc: Transfer supplier update copied date
      **
      *****************************************************************
      **
      **  This program is used to update the copied date in both the
      **  k_transpl and k_tranprd files.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 06/19/2014.
      *   Remarks. Changed this program to utilize SQL cursors to
      *            loop through files K_TRANSPL and K_TRANPRD.
      *            In addition, utilized SQL update statements to
      *            update columns TS_COPIED and TP_COPIED.  Also,
      *            deleted the call to K3S_M090 to get the time stamp
      *            from which to pull the current date and replaced
      *            it with the %DATE() RPG IV built-in function.
      *****************************************************************
      * --------------------------------------------------------- Workfields
     d #once           s              1  0                                      once routine
     d copied          s             10                                         copied date
     d curr_date       s               d
     d*
     d/copy k3s_proto
     d*
      * --------------------------------------------------------- Host variables
     d transpl_rec   e ds                  ExtName(k_transpl)
     d tranprd_rec   e ds                  ExtName(k_tranprd)
      * ----------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * ----------------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                   closqlcsr = *endactgrp;

       //Get current date
       curr_date = %date();

       exsr dcltscursor;
       //exsr clstscursor;
       exsr opntscursor;

       //--------------------- read transfer system regular supplier records

       dow SQLState = SQLStateOk;
       //transfer system regular supplier link
       //read transfer supplier record
          exec sql
           fetch next
            from tscursor
            into :transpl_rec;

          if SQLState = RowNotFound;
            leave;
          endif;

       // if copy demand flag is on, and copy date is clear, then
       // update the copied date
          copied = %char(ts_copied:*ISO);
          if ts_cpydemd = 1 AND
             copied = '0001-01-01';
            ts_copied = curr_date;
            exsr updttranspl;
          endif;

       enddo;

       exsr clstscursor;

       //------------------------------------ read transfer product  records

       exsr dcltpcursor;
       //exsr clstpcursor;
       exsr opntpcursor;

       dow SQLState = SQLStateOk;
       //transfer system product link
       //read transfer product record
          exec sql
           fetch next
            from tpcursor
            into :tranprd_rec;

          if SQLState = RowNotFound;
            leave;
          endif;

       // if copy demand flag is on, and copy date is clear, then
       // update the copied date
          copied = %char(tp_copied:*ISO);
          if tp_cpydemd = 1 AND
             copied = '0001-01-01';
               tp_copied = curr_date;
               exsr updttranprd;
          endif;

       enddo;

       exsr clstpcursor;

       //-------------------------------------------------- End of Main Loop

       //finished set on LR
       *inlr = *on;

       //**************************************************** End of program
       begsr opntscursor;
       exec sql
        open tscursor;
        if SQLState <> SQLStateOk;
           exsr clstscursor;
           exec sql
            open tscursor;
        endif;
       endsr;

       begsr opntpcursor;
       exec sql
        open tpcursor;
        if SQLState <> SQLStateOk;
           exsr clstpcursor;
           exec sql
            open tpcursor;
        endif;
       endsr;

       begsr dcltscursor;
       exec sql
        declare tscursor Cursor
          for
        select *
          from k_transpl
          order by ts_comp,
                   ts_locn,
                   ts_supl,
                   ts_suplsub
             for update of ts_copied;
       endsr;

       begsr dcltpcursor;
       exec sql
        declare tpcursor Cursor
          for
        select *
          from k_tranprd
          order by tp_comp,
                   tp_locn,
                   tp_prod
             for update of tp_copied;
       endsr;

       begsr clstscursor;
       exec sql
        close tscursor;
       endsr;

       begsr clstpcursor;
       exec sql
        close tpcursor;
       endsr;

       begsr updttranspl;
       exec sql
        update k_transpl
         Set ts_copied = :ts_copied
         Where current of tscursor;
       endsr;

       begsr updttranprd;
       exec sql
        update k_tranprd
         Set tp_copied = :tp_copied
         Where current of tpcursor;
       endsr;
      /end-free
