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
      **   Name: K3S_3910
      **   Type: ILE RPG Program
      **   Desc: Add products to K3S_REPLENISH combined supplier
      **         night job
      *****************************************************************
      **
      **  This program is used to create combined supplier orders
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 05/26/2014.
      **  Remarks. Altered program to utilize an SQL cursor to loop
      **           through K_SUPLIER file records and perform the
      **           record selections formerly done by OPNQRYF                                      .
      **           statements in K3S_3910CL. Utilized SQL update
      **           statement to update sp_soqseq# field in K_SUPLIER.
      **
      *****************************************************************
      * ------------------------------------ File information data structure
     d/copy k3s_c010
      * -------------------------------------- Program Status Data Structure
     d/copy k3s_c040
      * -------------------------------------------------------
     d/copy k3s_proto
      * ----------------------------------------------------- procedure interface
     d K3S_3910        PI
     d  comp                          1    const
      * ------------------------------------------------------parameter passed
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d suplier_rec   e ds                  ExtName(k_suplier)
      * ------------------------------------------------------parameter passed prototype
     d*
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       exsr dclspcursor;
       exsr opnspcursor;

       //-------------------------------------------------------- Main Loop
       //main loop
       dow SQLState = SQLStateOk;

       //Fetch K_SUPLIER row from result set
       exec sql
        fetch next
         from spcursor
         into :suplier_rec;

         if SQLState = RowNotFound;
            leave;
         endif;

         exec sql
           update k_suplier
             set sp_soqseq# = 1
             where sp_comp = :sp_comp and
                   sp_locn = :sp_locn and
                   sp_supl = :sp_supl and
                   sp_suplsub = :sp_suplsub;

         callp K3S_3900(sp_comp:
                        sp_locn:
                        sp_supl:
                        sp_suplsub);
       enddo;

       exsr clsspcursor;
       *inlr = *on;

       begsr dclspcursor;
       exec sql
        declare spcursor Cursor
          for
        select *
          from k_suplier
          where sp_comp = :comp and
                sp_altsrce = 2  -----combination supplier
          order by sp_comp,
                   sp_locn,
                   sp_supl,
                   sp_suplsub;
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

      /end-free
