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
      **   Name: K3S_9160
      **   Type: ILE RPG Program
      **   Desc: Change supplier for alternate source records
      **
      *****************************************************************
      **
      **  This program will change the supplier for alternate source
      **  records during the week. Since alternate source orders are
      **  stored for the week, a change to a product's supplier in the
      **  k_product file, needs to ripple through to the alternate
      **  source orders for the week.
      **
      **  A previously run OPNQRYF will supply only the code '2'
      **  k_logprod records to be processed for today's date.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 06/24/2014.
      *   Remarks. Removed OPNQRYF CL statements pertaining to K3S_9160
      *            from K3S_NITECL and left the call to RPG program
      *            K3S_9160. In K3S_9160 program added code to use an                              .
      *            SQL select statement to select all K_LOGPROD records
      *            where lg_logtype = '2' (change in product supplier),
      *            lg_comp = company parameter, and lg_timestp >=
      *            today_str parameter and <= today_end parameter.
      *            Also, utilized an SQL cursor to loop through the
      *            selected K_LOGPROD records and used dynamic SQL
      *            cursors to loop through the selected records for
      *            K_PRODUCT and K_PRODSOQ. Finally, added SQL update
      *            statements to update K_PRODUCT and K_PRODSOQ to
      *            reflect new supplier.
      *****************************************************************
      * -------------------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
      * -------------------------------------------------------
     d logprod_rec   e ds                  ExtName(k_logprod)
     d product_rec   e ds                  ExtName(k_product)
     d prodsoq_rec   e ds                  ExtName(k_prodsoq)
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * ----------------------------------------------------- procedure interface
     d K3S_9160        PI
     d  comp                          1
     d  log_type                      1
     d  today_str                    26
     d  today_end                    26

      /free
       //-------------------------------------------------------- Begin
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       exsr dclprcursor;
       exsr dclpqcursor;

       exsr dcllgcursor;
       exsr clslgcursor;
       exsr opnlgcursor;

       //-------------------------------------------------------- Main Loop
       //main loop
       dow SQLState = SQLStateOK;

       //fetch product log record
           exec sql
            fetch next
             from lgcursor
             into :logprod_rec;

           if SQLState = RowNotFound;
              leave;
           endif;

       //-------------------------------------- change supplier in k_product

           exsr inzInpSrch;
           exsr intSQLStmt;
           exsr prepDynSQLStmt;
           if SQLState = SQLStateOk;   //Good prep
              exsr opnprcursor;

              dow SQLState = SQLStateOk;

                exec sql
                 fetch next
                  from prcursor
                  into :product_rec;

                if SQLState = RowNotFound;
                   leave;
                endif;
       //only update alternate source records for this location
                if     pr_altsrce = 1;
                       pr_suplusr = lg_suplusr;
                       pr_suplusb = lg_suplusb;
                       pr_suplorg = lg_supl;     //Original supplier
                       pr_suplors = lg_suplsub;  //Original supplier sub
                       exsr updtproduct;
                endif;
              enddo;
              exsr clsprcursor;

           endif;
       //-------------------------------------- change supplier in k_prodsoq

           exsr inzInpSrch2;
           exsr intSQLStmt2;
           exsr prepDynSQLStmt2;
           if SQLState = SQLStateOk;
              exsr opnpqcursor;

              dow SQLState = SQLStateOk;

                exec sql
                 fetch next
                  from pqcursor
                  into :prodsoq_rec;

                if SQLState = RowNotFound;
                   leave;
                endif;
       //only update alternate source records for this location
                if     pq_altsrce = 1;
                       pq_suplusr = lg_suplusr;
                       pq_suplusb = lg_suplusb;
                       pq_suplorg = lg_supl;          //Original supplier
                       pq_suplors = lg_suplsub;       //Original supplier sub
                       pq_suplorn = lg_suplnam;       //Original supplier name
                       exsr updtprodsoq;
                endif;

              enddo;
              exsr clspqcursor;

           endif;
       //-------------------------------------------------------------------

       //read products end of loop
           SQLState= SQLStateOk;
         enddo;

         exsr clslgcursor;
       //-------------------------------------------------- End of Main Loop

       //finished, set on LR
         *inlr = *on;

       //log product transactions - today's records only, for type '2'
       begsr dcllgcursor;
       exec sql
        Declare lgcursor Cursor
         for
         Select *
         From k_logprod
         Where lg_comp = :comp and
               lg_logtype = :log_type and
               char(lg_timestp) >= :today_str and
               char(lg_timestp) <= :today_end;
       endsr;

       begsr opnlgcursor;
       exec sql
        open lgcursor;
        if SQLState <> SQLStateOk;
           exsr clslgcursor;
           exec sql
            open lgcursor;
        endif;
       endsr;

       begsr clslgcursor;
       exec sql
        close lgcursor;
       endsr;

       begsr dclprcursor;
       exec sql
        declare prcursor Cursor
         for DynSQLStmt;
       endsr;

       begsr PrepDynSQLStmt;
       exec sql
        Prepare DynSqlStmt
          From :StmtString;
       endsr;

       Begsr IntSQLStmt;
       String = *blanks;
       String =   'Select * +
                   From K_Product +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrch;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'pr_comp = ? and +
                     pr_prod = ? and +
                     pr_locn = ? +
                     Order by pr_comp, +
                              pr_prod, +
                              pr_locn';
       endsr;

       begsr opnprcursor;
       exec sql
        open prcursor
          using :lg_comp,
                :lg_prod,
                :lg_locn;
       endsr;

       begsr clsprcursor;
       exec sql
        close prcursor;
       endsr;

       begsr updtproduct;
       exec sql
         update k_product
         set pr_suplusr = :pr_suplusr,
             pr_suplusb = :pr_suplusb,
             pr_suplorg = :pr_suplorg,
             pr_suplors = :pr_suplors
         where current of prcursor;
       endsr;

       Begsr IntSQLStmt2;
       String = *blanks;
       String =   'Select * +
                   From K_Prodsoq +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrch2;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'pq_comp = ? and +
                     pq_prod = ? and +
                     pq_locn = ? +
                     Order by pq_comp, +
                              pq_prod, +
                              pq_locn';
       endsr;

       begsr opnpqcursor;
       exec sql
        open pqcursor
          using :lg_comp,
                :lg_prod,
                :lg_locn;
       endsr;

       begsr dclpqcursor;
       exec sql
        declare pqcursor Cursor
         for DynSQLStmt2;
       endsr;

       begsr clspqcursor;
       exec sql
        close pqcursor;
       endsr;

       begsr updtprodsoq;
       exec sql
         update k_prodsoq
         set pq_suplusr = :pq_suplusr,
             pq_suplusb = :pq_suplusb,
             pq_suplorg = :pq_suplorg,
             pq_suplors = :pq_suplors,
             pq_suplorn = :pq_suplorn
         where current of pqcursor;
       endsr;

       begsr PrepDynSQLStmt2;
       exec sql
        Prepare DynSqlStmt2
          From :StmtString;
       endsr;
      /end-free
