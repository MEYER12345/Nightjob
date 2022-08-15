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
      **   Name: K3S_7110
      **   Type: ILE RPG Program
      **   Desc: Get product totals by buyer and forcast interval
      **
      *****************************************************************
      **
      **   An OPNQRYF is run against K_PRODUCT in K3S_7110CL to prepare
      **   the sequence of PR_COMP, PR_BUYR, PR_FORCINT
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 05/18/2014.
      *   Remarks. Removed OPNQRYF CL statements pertaining to K3S_7110
      *            from K3S_7110CL leaving the call to RPG program
      *            K3S_7110 and passing the company code as a parameter.
      *            In K3S_7110 program added code to declare a cursor
      *            to handle selection and sorting of K_PRODUCT records.
      *            Also, changed code to access K_TABLCOD and K_COMPANY
      *            through SQL select statements.
      *****************************************************************
      * -------------------------------------------------- Parameters passed
     d save_buyr       s                   like(r3_buyr)
     d save_comp       s                   like(pr_comp)
     d save_user       s                   like(r3_user)
     d save_locn       s                   like(r3_locn)
     d save_forc       s                   like(r3_forcint)
     d end_date        s               d
     d chkdate         s               d
     d pr_count        s              7  0 inz(0)
     d cmsysdate       s               d
     d tacodeds1       s            100a
     d Prod_Found      s               n
     d #name           s              5
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
     d MsgText         s             50a
     d Msg             s             50a
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d product_rec   e ds                  ExtName(k_product)
     d perdper_rec   e ds                  ExtName(k_perdper)
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * ----------------------------------------------------- procedure interface
     d K3S_7110        PI
     d  comp                          1
      * -------------------------------------------------------
      /free
       exec sql

        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       exsr dclr3cursor;

       exsr dclprcursor;
       exsr opnprcursor;
       //Main loop
       dow SQLState = SQLStateOk;

       //get batch record
         exec sql
          fetch next
            from prcursor
            into :product_rec;

         if SQLState = RowNotFound;
            leave;
         endif;

       //-once routine begin
         if save_buyr = *blanks;
            exec sql
             select cm_sysdate
              into :cmsysdate
              from k_company
              where cm_comp = :pr_comp
              fetch first row only;
            chkdate = %date('0001-01-01':*ISO);
            exec sql
              select ta_codeds1
               into :tacodeds1
               from k_tablcod
               where ta_comp = :pr_comp and
                     ta_codetyp = 'BUY' and
                     ta_codeval = :pr_buyr
               fetch first row only;

            if SQLState = SQLStateOk;
               save_user = tacodeds1;
               save_locn = pr_locn;
               save_buyr = pr_buyr;
               save_forc = pr_forcint;
               save_comp = pr_comp;
             endif;
         endif;
       //-once routine end

       //-accumulate # products
         if pr_buyr = save_buyr and
            pr_locn = save_locn and
            pr_forcint = save_forc;

            pr_count += 1;
         endif;

       //-a break has occured, now go update PEx batches
         if pr_buyr <> save_buyr or
            pr_locn <> save_locn or
            pr_forcint <> save_forc;

            exsr process_perdper;
            exec sql
             select ta_codeds1
              into :tacodeds1
              from k_tablcod
              where ta_comp = :pr_comp and
                    ta_codetyp = 'BUY' and
                    ta_codeval = :pr_buyr
              fetch first row only;

            if SQLState = SQLStateOk;
               save_user = tacodeds1;
               save_buyr = pr_buyr;
               save_locn = pr_locn;
               save_forc = pr_forcint;

               pr_count = 1;
            endif;
       //-end of break
         endif;
         SQLState = SQLStateOk;
       enddo;
       exsr process_perdper;
       exsr clsprcursor;
       //------------------------------------------------------- end program

       *inlr = *on;

       begsr dclprcursor;
       exec sql
        declare prcursor Cursor
         for
         select *
         from k_product
         where pr_comp = :comp and
               pr_altsrce = 0  and
               pr_tempory = 0 and
               pr_deltcnt = 0
         Order by pr_comp,
                  pr_buyr,
                  pr_locn,
                  pr_forcint;
       endsr;

       begsr opnprcursor;
       exec sql
        open prcursor;
        if SQLState <> SQLStateOk;
           exsr clsprcursor;
           exec sql
            open prcursor;
        endif;
       endsr;

       begsr clsprcursor;
       exec sql
        close prcursor;
       endsr;

       begsr opnr3cursor;
       exec sql
        open r3cursor
         using :save_comp,
               :save_user,
               :save_locn,
               :save_buyr,
               :save_forc;
       endsr;

       begsr clsr3cursor;
       exec sql
        close r3cursor;
       endsr;

       begsr dclr3cursor;
       exec sql
        declare r3cursor Cursor
         for DynSQLStmt;
       endsr;

       begsr IntSQLStmt;
       String = *blanks;
       String =   'Select * +
                   From K_Perdper +
                   Where ';
       //
       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       begsr PrepDynSQLStmt;
       exec sql
        Prepare DynSqlStmt
          From :StmtString;
       endsr;

       begsr InzInpSrch;

       InpSrchcnd = *blanks;
       InpSrchCnd = 'r3_comp = ? and +
                     r3_user = ? and +
                     r3_locn = ? and +
                     r3_buyr = ? and +
                     r3_forcint = ? +
                     Order by r3_comp, +
                              r3_user, +
                              r3_locn, +
                              r3_buyr, +
                              r3_forcint, +
                              r3_perdend, +
                              r3_reqtype';
       endsr;

       begsr updtperdper;
       exec sql                   -----row found
        Update k_perdper
         Set r3_totprod = :r3_totprod
         Where current of r3cursor;
       endsr;

       begsr process_perdper;
          if save_buyr <> *blanks;
             exsr InzInpSrch;
    ‚   //Initialize StmtString
  ‚           exsr intSQLStmt;
    ‚   //Prepare dynamic SQL statement
  ‚           exsr prepDynSQLStmt;

             if SQLState = SQLStateOk;   //if prep was good
    ‚   //Open dynamic curosr
                exsr opnr3cursor;

                Dow SQLState = SQLStateOk;
                    exec sql
                     fetch r3cursor
                       into :perdper_rec;

                    if SQLState = RowNotFound;
                       leave;
                    endif;

                    if r3_reqtype >= 'PE1' and
                       r3_reqtype <= 'PE5' and
                       r3_birth = cmsysdate and
                       r3_lastupd = chkdate;

                       r3_totprod = pr_count;
                       exsr updtperdper;
                    endif;
                enddo;
                exsr clsr3cursor;
             endif;
          endif;
       endsr;
      /end-free
