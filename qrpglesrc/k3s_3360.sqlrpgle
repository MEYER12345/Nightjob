      *****************************************************************
     h copyright('(C) Copyright 1996 - 2016 King III Solutions, Inc.  +
     h Rel 5.3  2016-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)
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
      **   Name: K3S_3360
      **   Type: ILE RPG Program
      **   Desc: Purchase adjustments build K_PRODAPP
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 06/13/2014.
      *   Remarks. Modified program to utilize an SQL cursor to loop
      *            through file K_SUPLSOQ. Also, altered code to use
      *            an SQL select statement to access the desired
      *            K_TABLCOD record and the desired K_PRODUCT records.
      *            In addition, added an SQL insert statement to add
      *            new K_PRODAPP records. Finally, added an SQL cursor
      *            to loop through the desired K_PRODSOQ records.
      *
      *   Programmer. David Meyer.
      *   Date. 02/20/2016.
      *   Remarks. Updated code to Release 5.3. This included adding an
      *            SQL UPDATE statement to increment K_SUPLIER field
      *            sp_usern1 if the K_SUPLIER record meets certain
      *            conditions.
      *****************************************************************
     d any_splits      s              1
     d birth           s             10
     d once            s              1
     d week_end        s              1    inz(*off)
     d weeks           s              7  0
     d daynbr          s              1P 0
     d comp            s              1
     d buyr            s              5
     d tacodeds1       s            100
     d prcostreg       s             11  4
     d prtempory       s              1  0
     d prbirth         s               d
     d one_yr_ago      s               d
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
      * ------------------------------------------------------- Data structures
     d tablcod_rec   e ds                  ExtName(k_tablcod)
     d suplsoq_rec   e ds                  ExtName(k_suplsoq)
     d prodapp_rec   e ds                  ExtName(k_prodapp)
     d prodsoq_rec   e ds                  ExtName(k_prodsoq)
      * -------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * ---------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       exsr dclpqcursor;
       exsr dclsocursor;
       //exsr clssocursor;
       exsr opnsocursor;

       one_yr_ago = %date();
       one_yr_ago = one_yr_ago - %years(1);

       //--------------------------------------------------------- Main Loop
       // fetch rows from supplier suggested order quantity file
       // main loop using fetch
       dow SQLState = SQLStateOk;

         exec sql
           fetch next
            from socursor
            into :suplsoq_rec;

         if SQLState = RowNotFound;
           leave;
         endif;

         //if SQLState = SQLStateOk;
       // Capture days orders left sitting
            if so_altsrce <> 1 and
              so_usera1  <> '0' and
              so_soqtype <> 'CN' and
              so_soqtype <> 'AP';

       // with first valid record, determine if weekend
                 if once = *blank;
                    once = 'Y';
                    weeks = %diff(so_birth:d'1899-12-30':*days);
                    daynbr = %rem(Weeks:7);
       // 0 = Saturday, 1 = Sunday
                    if daynbr = 0 or
                       daynbr = 1;
                       week_end = *on;
                    endif;
                 endif;
                 if week_end = *off;
       // If not a Fixed Cycle supplier, and Delay is 0, ding buyer
                    if (so_fxcfrq =  0   and
                        so_joint  =  0)  or
       // If this is a Fixed Cycle supplier, and we are on or missed Next Order Date,
       // ding buyer
                       (so_fxcfrq > 0    and
                        so_birth >= so_fxcnxt);
                       exec sql
                         update k_suplier
                           set sp_usern1 = sp_usern1 + 1
                               where sp_comp = :so_comp and
                                     sp_locn = :so_locn and
                                     sp_supl = :so_supl and
                                     sp_suplsub = :so_suplsub and
                                     sp_usern1  < 99999 and
                                     sp_ordate  > :one_yr_ago;
                    endif;
                 endif;
            endif;

            if so_altsrce <> 1 and
               so_soqtype = 'AP';
                 exec sql
                 select ta_codeds1
                   into :tacodeds1
                   from k_tablcod
                   where ta_comp = :so_comp and
                         ta_codetyp = 'BUY' and
                         ta_codeval = :so_buyr
                   fetch first row only;
                 if SQLSTT = SQLStateOk;
                    pj_user = tacodeds1;
                 else;
                    pj_user = *blanks;
                 endif;
                 pj_year = %subdt(so_birth:*years);
                 pj_month = %subdt(so_birth:*months);

                 exsr InzInpSrch;
                 exsr IntSQLStmt;
                 exsr PrepDynSQLStmt;

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

                      if pq_altsrce <> 1 and SQLState = SQLStateOk;
           //don't want this test  pq_tempory = 0 and   <- by pass Comb Supl
                        if pq_soqact > 0 or
                           pq_soqsrvc > 0;
                             pj_comp = pq_comp;
                             pj_buyr = pq_buyr;
                             pj_regn = pq_regn;
                             pj_locn = pq_locn;
                             pj_supl = pq_supl;
                             pj_suplsub = pq_suplsub;
                             pj_prod = pq_prod;
                             pj_altsrce = pq_altsrce;
                             pj_soqseq# = pq_soqseq#;
                             pj_po# = so_po#;
                             pj_birth = pq_birth;
                             pj_costord = pq_costord;
                             pj_costdiv = pq_costdiv;

                             exec sql
                               select pr_costreg, pr_tempory, pr_birth
                                 into :prcostreg, :prtempory, :prbirth
                                 from k_product
                                 where pr_comp = :pq_comp and
                                       pr_locn = :pq_locn and
                                       pr_supl = :pq_supl and
                                       pr_suplsub = :pq_suplsub and
                                       pr_prod = :pq_prod
                                 fetch first row only;

                             pj_costreg = prcostreg;
                             pj_soqorig = pq_soqsrvc;

                             SQLState = SQLStateOk;
        //buyer gets credit for these conditions Manual, New, or Discontinue
        //      or Temporary record from options 1,3,4 from 'add to order'
        //      or on the 1st day a product is added to K3S option 2 from 'add to order'
                             if pq_usrstat = 'M' or
                                pq_sysstat = 'N' or
                                pq_sysstat = 'D' or
                                prtempory = 1   or
                                prbirth   = pq_birth;
                                   pj_soqorig = pq_soqact;
                             endif;

                             pj_soqact = pq_soqact;

        //MSS and USD type orders need identification downstream
                             if so_spl1typ = 'MSS' and
                                so_altsrce = 0;
                                  pj_altsrce = 6;
                             endif;
                             if so_spl1typ = 'MSS' and
                                so_altsrce = 2;
                                   pj_altsrce = 7;
                             endif;
                             if so_spl1typ = 'USD' and
                                so_altsrce = 0;
                                  pj_altsrce = 8;
                             endif;
                             if so_spl1typ = 'USD' and
                                so_altsrce = 2;
                                  pj_altsrce = 9;
                             endif;
      //MSS and USD type orders need identification downstream for 'Split 2nd Type'
                             if so_spl2typ = 'MSS' and
                                so_altsrce = 0;
                                   pj_altsrce = 6;
                             endif;
                             if so_spl2typ = 'MSS' and
                                so_altsrce = 2;
                                   pj_altsrce = 7;
                             endif;
                             if so_spl2typ = 'USD' and
                                so_altsrce = 0;
                                   pj_altsrce = 8;
                             endif;
                             if so_spl2typ = 'USD' and
                                so_altsrce = 2;
                                   pj_altsrce = 9;
                             endif;
                             if pj_altsrce > 5;
                                any_splits = *on;
                                  pj_comp = '$';
                             endif;

                             exsr insert_prodapp;
                        endif;
                      endif;
                      SQLState = SQLStateOk;
                    enddo;
                 endif;
                 exsr clspqcursor;
            endif;
         //endif;
         SQLState = SQLStateOk;
       enddo;

       if any_splits = *on;
         eval(r) birth = %char(pj_birth:*ISO);
         callp K3S_3366CL (pq_comp:birth);
       endif;

       exsr clssocursor;

       *inlr = *on;

       begsr dclsocursor;
       exec sql
        declare socursor Cursor
          for
        select *
          from k_suplsoq
          order by so_comp,
                   so_buyr,
                   so_locn,
                   so_supl,
                   so_suplsub,
                   so_soqseq#;
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

       begsr dclpqcursor;
       exec sql
         declare pqcursor Cursor
           for DynSQLStmt;
       endsr;

       begsr insert_prodapp;
         Exec sql
           insert into k_prodapp
           values (:prodapp_rec);
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

       begsr PrepDynSQLStmt;
       exec sql
         Prepare DynSqlStmt
           From :StmtString;
       endsr;

       begsr IntSQLStmt;
       String = *blanks;
       String =   'Select * +
                   From K_Prodsoq +
                   Where ';
       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       begsr InzInpSrch;
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
      /end-free
