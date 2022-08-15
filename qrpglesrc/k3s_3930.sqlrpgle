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
      **   Name: K3S_3930
      **   Type: ILE RPG Program
      **   Desc: Remove products & orders K3S_REPLENISH combined supplier
      **         night job
      *****************************************************************
      **
      **  This program is used to remove combined supplier orders &
      **  products for re-start
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 05/26/2014.
      **  Remarks. Altered program to utilize an SQL cursor to loop
      **           through K_SUPLIER file records. Also, changed
      **           program to use SQL DELETE statements to delete                                  .
      **           records that meet the desired criteria from the
      **           K_PRODUCT, K_PRODSOQ, and K_SUPLSOQ files.
      **
      *****************************************************************
      * ------------------------------------ File information data structure
     d/copy k3s_c010

      * -------------------------------------- Program Status Data Structure
     d/copy k3s_c040
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_3930        PI
     d  comp                          1    const
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d suplier_rec   e ds                  ExtName(k_suplier)
     d*
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       exsr dclspcursor;
       exsr clsspcursor;
       exsr opnspcursor;

       //-------------------------------------------------------- Main Loop
       //main loop
       dow SQLState = SQLStateOk;

       //Fetch K_SUPLIER row from result set
       //Combined suppliers
          exec sql
           fetch next
            from spcursor
            into :suplier_rec;

          if SQLState = RowNotFound;
             leave;
          endif;

           //Product file
          exec sql
            Delete from k_product
              Where pr_comp = :sp_comp and
                    pr_buyr = :sp_buyr and
                    pr_locn = :sp_locn and
                    pr_supl = :sp_supl and
                    pr_suplsub = :sp_suplsub and
                    pr_altsrce = 2;

           //Product SOQ file
          exec sql
            Delete from k_prodsoq
              Where pq_comp = :sp_comp and
                    pq_buyr = :sp_buyr and
                    pq_locn = :sp_locn and
                    pq_supl = :sp_supl and
                    pq_suplsub = :sp_suplsub and
                    pq_altsrce = 2;

           //Supplier SOQ file
          exec sql
            Delete from k_suplsoq
              Where so_comp = :sp_comp and
                    so_buyr = :sp_buyr and
                    so_locn = :sp_locn and
                    so_supl = :sp_supl and
                    so_suplsub = :sp_suplsub and
                    so_altsrce = 2;

          SQLState = SQLStateOK;

       enddo;

       exsr clsspcursor;
       *inlr = *on;

       begsr dclspcursor;
       exec sql
        declare spcursor Cursor
         for
         select *
         from k_suplier
         where sp_comp = :comp
         order by sp_comp,
                  sp_locn,
                  sp_supl,
                  sp_suplsub;
       endsr;

       begsr opnspcursor;
       exec sql
        open spcursor;
       endsr;

       begsr clsspcursor;
       exec sql
        close spcursor;
       endsr;
      /end-free
