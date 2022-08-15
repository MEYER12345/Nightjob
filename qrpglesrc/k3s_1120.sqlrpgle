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
      **   Name: K3S_1120
      **   Type: ILE RPG Program
      **   Desc: Suggested order alternate source save orders for week
      **
      *****************************************************************
      **
      **  This program is used to create a copy of all alternate source
      **  orders, until the next alternate source processing night.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 06/20/2014.
      *   Remarks. Changed this program to utilize an SQL cursor to
      *            loop through file K_SUPLSOQ. Also, changed it to
      *            use SQL cursors to loop through the desired
      *            records in K_PRODSOQ. In addition, utilized SQL
      *            insert statements to write records to files
      *            K_SUPLPND and K_PRODPND.
      *****************************************************************
     d  total          s              9p 0                                      total allowed
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d suplsoq_rec   e ds                  ExtName(k_suplsoq)
     d prodsoq_rec   e ds                  ExtName(k_prodsoq)
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_1120        PI
     d  comp                          1
     d*
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                   closqlcsr = *endactgrp;

       exsr dclpqcursor;

       exsr dclsocursor;
       //exsr clssocursor;
       exsr opnsocursor;

       //--------------------------------------------------------- Main Loop
       //main loop
       Dow SQLState = SQLStateOk;

       //fetch supplier suggested order file record
          exec sql
           fetch next
            from socursor
            into :suplsoq_rec;

          if SQLState = RowNotFound;
            leave;
          endif;

          if SQLState = SQLStateOk;

                exsr insert_suplpnd;   //pending order header

       //---------------------------------------------------- start products
       //start with first product for this supplier suggested order
                exsr InzInpSrch;
    ‚   //initialize StmtString
    ‚            exsr intSQLStmt;
    ‚   //prepare statement
    ‚            exsr prepDynSQLStmt;

                if SQLState = SQLStateOk;         //If prepare was successful
    ‚   //open dynamic cursor
                   exsr opnpqcursor;

       //product suggested order
       //Loop through all products in this order
       //read products in this order
                   dow SQLState = SQLStateOk;

                      exec sql
                       fetch next
                        from pqcursor
                        into :prodsoq_rec;

                      if SQLState = RowNotFound;
                        leave;
                      endif;

                      if SQLState = SQLStateOk;
       //bail out to keep from runaway process
                        total += 1;
                        if total > 200000;
                          leave;
                        endif;

       //only process when record read

                        exsr insert_prodpnd;   //pending order detail
                      endif;
                   enddo;
                   exsr clspqcursor;
                else;
                   SQLState = SQLStateOk;
                endif;
          endif;
       enddo;

       exsr clssocursor;

       *inlr = *on;

       begsr dclsocursor;
       exec sql
        declare socursor Cursor
         for
         select *
         from k_suplsoq
         where so_comp = :comp and
               so_altsrce = 1;
       endsr;

       begsr dclpqcursor;
       exec sql
        declare pqcursor Cursor
         for DynSQLStmt;
       endsr;

       begsr opnsocursor;
       exec sql
        open socursor;
        if SQLState <> SQLStateOk;
           exsr clssocursor;
           exec sql
            open socursor;
        endif;
       endsr;

       begsr clssocursor;
       exec sql
        close socursor;
       endsr;

       begsr insert_suplpnd;
         Exec sql
         insert into k_suplpnd
         values (:suplsoq_rec);
       endsr;

       begsr insert_prodpnd;
         Exec sql
         insert into k_prodpnd
         values (:prodsoq_rec);
       endsr;

       begsr PrepDynSQLStmt;
       exec sql
        Prepare DynSqlStmt
          From :StmtString;
       endsr;

       Begsr IntSQLStmt;
       String = *blanks;
       String =   'Select * +
                   From K_Prodsoq +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrch;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'pq_comp = ? and +
                     pq_buyr = ? and +
                     pq_locn = ? and +
                     pq_supl = ? and +
                     pq_suplsub = ? and +
                     pq_soqseq# = ? +
                     Order by pq_comp, +
                              pq_buyr, +
                              pq_locn, +
                              pq_supl, +
                              pq_suplsub, +
                              pq_soqseq#';
       endsr;

       begsr opnpqcursor;
       exec sql
        open pqcursor
          using :so_comp,
                :so_buyr,
                :so_locn,
                :so_supl,
                :so_suplsub,
                :so_soqseq#;
       endsr;

       begsr clspqcursor;
       exec sql
        close pqcursor;
       endsr;

      /end-free
