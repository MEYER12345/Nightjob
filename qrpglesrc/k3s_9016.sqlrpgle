      *****************************************************************
     h copyright('(C) Copyright 1996 - 1999 King III Solutions, Inc.  +
     h Rel 4.19 1999-03-30       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') FIXNBR(*ZONED) AUT(*ALL)
      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-1999 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_9016
      **   Type: ILE RPG Program
      **   Desc: Update K_INTPROD where locn/supplier is invalid
      **
      *****************************************************************
      **
      **  This program is used to test the locn/supplier passed from
      **  the host is valid in K3S-Replenish, and if not, then
      **  change the locn/supplier combination to a valid combination.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 06/18/2014.
      *   Remarks. Changed this program to utilize an SQL cursor to
      *            loop through file K_INTPROD.  Also, changed it to
      *            use an SQL select statement to verify locn/suplier
      *            is in the K_SUPLIER file rather than using a CHAIN.
      *****************************************************************
     d intprod_rec   e ds                  ExtName(k_intprod)
      * ------------------------------------------------------parameter passed prototype
     d/copy k3s_proto
      * ----------------------------------------------------- procedure interface
     d K3S_9016        PI
     d  comp                          1
     d  supl                         10
     d  suplsub                      10
      * --------------------------------------------------------- Workfields
     d supplier_ds     ds
     d  sp_comp                       1
     d  sp_supl                      10
     d  sp_suplsub                   10
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                closqlcsr = *endactgrp;

       //product interface records
       exsr dclipcursor;
       //exsr clsipcursor;
       exsr opnipcursor;

       dow SQLSTT = SQLStateOk;
       //loop through all product interface records

          exec sql
           fetch next
            from ipcursor
            into :intprod_rec;

          if SQLState = RowNotFound;
            leave;
          endif;

       // process for this company only
          if ip_comp = comp;

       //suppliers (supl,suplsub)
            exec sql
             select sp_comp,
                    sp_supl,
                    sp_suplsub
             into   :supplier_ds
             from k_suplier
             where  sp_comp = :comp and
                    sp_supl = :ip_supl and
                    sp_suplsub = :ip_suplsub
                    fetch first row only;
       //test for supplier record
             if SQLSTT = RowNotFound;

       // for invalid supplier passed in interface, use substitute supplier
                ip_supl = supl;
                ip_suplsub = suplsub;
                exsr updtiprec;
             endif;
          endif;

       enddo;

       *inlr = *on;

       begsr dclipcursor;
       exec sql
        declare ipcursor Cursor
          for
        select *
          from k_intprod
             for update of ip_supl,
                           ip_suplsub;
       endsr;

       begsr opnipcursor;
       exec sql
        open ipcursor;
        if SQLState <> SQLStateOk;
           exsr clsipcursor;
           exec sql
            open ipcursor;
        endif;
       endsr;

       begsr clsipcursor;
       exec sql
        close ipcursor;
       endsr;

       begsr updtiprec;
       exec sql
        update k_intprod
         Set ip_supl = :ip_supl,
             ip_suplsub = :ip_suplsub
         Where current of ipcursor;
       endsr;
