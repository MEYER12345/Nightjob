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
      **   Name: K3S_3460
      **   Type: ILE RPG Program
      **   Desc: Product change source of supplier - night batch
      **
      *****************************************************************
      **
      **  This program is used to update the k_intprod file by
      **  changing the regular supplier ID to a different supplier ID.
      **  This program is part of the night job, and would run before
      **  K3S_9010.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 04/14/2014.
      *   Remarks. Changed this program from using the RPG cycle to
      *            using an SQL cursor to loop through file K_PRODSRC.
      *            Added SQL update statements to update K_INTPROD
      *            rather than using RPG CHAIN operation codes.
      *****************************************************************
      **--------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_3460        PI
     d  comp                          1
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d prodsrc_rec   e ds                  ExtName(k_prodsrc)
      * -------------------------------------------------------
     d  pccomp         s              1
     d  pclocn         s              5
     d  pcsupl         s             10
     d  pcsuplsub      s             10
     d  pcprod         s             25
     d  pcnewsupl      s             10
     d  pcnewsub       s             10
      * -------------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       exsr dclpccursor;
       //exsr clspccursor;
       exsr opnpccursor;

       //--------------------------------------------------------- Main Loop
       //main loop
       dow SQLSTT = SQLStateOk;

        //fetch first K_PRODSRC row
          exec sql
           fetch next
            from pccursor
            into :prodsrc_rec;

          If SQLState = RowNotFound;
            leave;
          endif;

          pccomp = pc_comp;
          pclocn = pc_locn;
          pcsupl = pc_supl;
          pcsuplsub = pc_suplsub;
          pcprod = pc_prod;
          pcnewsupl = pc_newsupl;
          pcnewsub = pc_newsub;

          if pc_rectype = 'P';
             exec sql
               Update k_intprod
                 Set ip_supl = :pc_newsupl,
                     ip_suplsub = :pc_newsub
                 Where ip_comp = :pc_comp and
                       ip_locn = :pc_locn and
                       ip_supl = :pc_supl and
                       ip_suplsub = :pc_suplsub and
                       ip_prod = :pc_prod;
             if SQLState = RowNotFound;
               SQLState = SQLStateOk;
             endif;
          endif;

          if pc_rectype = 'S';
             exec sql
               Update k_intprod
                 Set ip_supl = :pc_newsupl,
                     ip_suplsub = :pc_newsub
                 Where ip_comp = :pc_comp and
                       ip_locn = :pc_locn and
                       ip_supl = :pc_supl and
                       ip_suplsub = :pc_suplsub;
             if SQLState = RowNotFound;
               SQLState = SQLStateOk;
             endif;
          endif;

       enddo;

       exsr clspccursor;

       *inlr = *on;

       begsr dclpccursor;
       exec sql
        declare pccursor Cursor
         for
         select *
         from k_prodsrc
         order by pc_comp,
                  pc_locn,
                  pc_rectype,
                  pc_supl,
                  pc_suplsub,
                  pc_prod;
       endsr;

       begsr opnpccursor;
       exec sql
        open pccursor;
        if SQLState <> SQLStateOk;
           exsr clspccursor;
           exec sql
            open pccursor;
        endif;
       endsr;

       begsr clspccursor;
       exec sql
        close pccursor;
       endsr;

      /end-free
        //*************************************************** End of program

