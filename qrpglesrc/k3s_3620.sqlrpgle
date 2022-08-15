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
      **   Name: K3S_3620
      **   Type: ILE RPG Program
      **   Desc: Overstocked Products - update with results
      **
      *****************************************************************
      **
      **  This program is used to take the results from K3S_3600 and update
      **  the products file with which locations are overstocked, and the
      **  overstocked values.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 8/14/2014.
      *   Remarks. Changed this program to utilize an SQL cursor to
      *            loop through file K_PRODOVP. Also, changed it to use
      *            an SQL update statement to update K_PRODUCT record
      *            with overstock information. In addition, altered
      *            program to utilize a dynamic SQL cursor so that
      *            when a change in product occurs it will update
      *            all locations where an overstock condition exists.
      *****************************************************************
      * ------------------------------------------------------- work fields
     d #once           s              1  0                                      once routine
     d prod_saved      s                   like(pr_prod)
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_3620        PI
     d  comp                          1
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d prodovp_rec   e ds                  ExtName(k_prodovp)
     d product_rec   e ds                  ExtName(k_product)
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
      * -------------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       exsr dclprcursor;

       exsr dclpvcursor;
       //exsr clspvcursor;
       exsr opnpvcursor;
       //--------------------------------------------------------- Main Loop
       //main loop
       Dow SQLSTT = SQLStateOk;
        //------------------------------------ update product with overstock
        //fetch overstocked products record
         exec sql
          fetch next
           from pvcursor
           into :prodovp_rec;

         if SQLState = RowNotFound;
           leave;
         endif;

        //update product record with overstock information
         Exec sql
          update k_product
          set pr_overunt = :pv_ovrunit,
              pr_overcst = :pv_exccost,
              pr_overcur = :pv_dayscur,
              pr_overmax = :pv_daysmax
          where pr_comp = :pv_comp and
                pr_locn = :pv_locn and
                pr_supl = :pv_supl and
                pr_suplsub = :pv_suplsub and
                pr_prod = :pv_prod;
        //-------------------------------------------------------- Main Loop
        //Change in product, so update all locations that overstock exists
         if pv_prod <> prod_saved;
            prod_saved = pv_prod;

            exsr InzInpSrch;
    ‚   //initialize StmtString
    ‚        exsr IntSQLStmt;
    ‚   //prepare statement
    ‚        exsr PrepDynSQLStmt;

            if SQLState = SQLStateOk;  //If prep was good
    ‚   //open dynamic cursor
               exsr opnprcursor;
        //----------------------------------------------------- product loop
        //main loop
               dow SQLState = SQLStateOk;

                 exec sql
                  fetch next
                   from prcursor
                   into :product_rec;

                 if SQLState = RowNotFound;
                   leave;
                 endif;

        //only update regular source products that overstock exists flag
                 if pr_altsrce <> 1;
                    pr_overflg = 1;
                    exsr updtproduct;
                    SQLState = SQLStateOk;
                 endif;

               enddo;
               exsr clsprcursor;
            endif;
         endif;
         SQLState = SQLStateOk;
       enddo;

       exsr clspvcursor;

       *inlr = *on;

       begsr dclpvcursor;
       exec sql
        declare pvcursor Cursor
         for
         select *
         from k_prodovp
         where pv_comp = :comp
         order by pv_prod;
       endsr;

       begsr opnpvcursor;
       exec sql
        open pvcursor;
        if SQLState <> SQLStateOk;
           exsr clspvcursor;
           exec sql
            open pvcursor;
        endif;
       endsr;

       begsr clspvcursor;
       exec sql
        close pvcursor;
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
                     pr_prod = ? +
                     Order by pr_comp, +
                              pr_prod, +
                              pr_locn  +
                     for update of pr_overflg';
       endsr;

       begsr opnprcursor;
       exec sql
        open prcursor
          using :pv_comp,
                :pv_prod;
       endsr;

       begsr clsprcursor;
       exec sql
        close prcursor;
       endsr;

       begsr updtproduct;
       exec sql
        update k_product
          set pr_overflg = :pr_overflg
          where current of prcursor;
       endsr;

       begsr dclprcursor;
       exec sql
        declare prcursor Cursor
         for DynSQLStmt;
       endsr;
      /end-free

