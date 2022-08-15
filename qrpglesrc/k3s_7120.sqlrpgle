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
      **   Name: K3S_7120
      **   Type: ILE RPG Program
      **   Desc: Period end exceptions collect results for previous periods
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 05/18/2014.
      *   Remarks. Replaced RPG looping structure with one utilizing
      *            an SQL cursor in order to read through and update
      *            the selected records from file K_PERDPER. Also,
      *            altered program to use an SQL select statement to
      *            access file K_COMPANY.
      *****************************************************************
      * -------------------------------------------------- Parameters passed
     d start_pnt       s               d   datfmt(*iso)                         batch begin date
     d per1            s               d   datfmt(*iso)                         batch begin date
     d per2            s               d   datfmt(*iso)                         batch begin date
     d per3            s               d   datfmt(*iso)                         batch begin date
     d per4            s               d   datfmt(*iso)                         batch begin date
     d open_batch      s               d   inz(d'0001-01-01') datfmt(*iso)
     d cmsysdate       s               d
      * ------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * ------------------------------------------------------
     d perdper_rec   e ds                  ExtName(k_perdper)
     d prodsed_rec   e ds                  ExtName(k_prodsed)
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
     d MsgText         s             50a
     d Msg             s             50a
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_7120        PI
     d  comp                          1
     d  per12                         1
     d  per13                         1
     d  per52                         1

      /free

       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       exsr dclpdcursor;

        exec sql
         select cm_sysdate
          into :cmsysdate
          from k_company
          where cm_comp = :comp
          fetch first row only;

       exsr dclr3cursor;
       exsr clsr3cursor;
       exsr opnr3cursor;

       //Main loop
       dow SQLState = SQLStateOk;

       //get batch records
         exec sql
          fetch next
           from r3cursor
           into :perdper_rec;

         if SQLState = RowNotFound;
            leave;
         endif;

       // only look at batches from previous periods that are not yet
       // processed, but not today's batches
         if SQLState = SQLStateOk AND
            r3_lastupd = open_batch AND
            r3_perdend <> cmsysdate;

       // make sure its time for this batch to process
               if r3_forcint = 12 and
                  per12      = '1'       OR
                  r3_forcint = 13 and
                  per13      = '1'       OR
                  r3_forcint = 52 and
                  per52      = '1';

       // stamp this batch, he won't be processed again
                  r3_lastupd = cmsysdate;

                  start_pnt = r3_perdend - %days(1);

                  if r3_forcint = 52;
                     per1 = start_pnt + %days(1);
                     per2 = start_pnt + %days(2);
                     per3 = start_pnt + %days(3);
                     per4 = start_pnt + %days(4);
                  else;
                     per1 = start_pnt + %days(7);
                     per2 = start_pnt + %days(14);
                     per3 = start_pnt + %days(21);
                     per4 = start_pnt + %days(28);
                  endif;

                  r3_totper1 = *zeros;
                  r3_totper2 = *zeros;
                  r3_totper3 = *zeros;
                  r3_totper4 = *zeros;
                  r3_totper5 = *zeros;
                  r3_totnot = *zeros;
                  r3_totgone = *zeros;

                  exsr InzInpSrch;
    ‚   //Initialize StmtString
    ‚              exsr intSQLStmt;
    ‚   //Prepare dynamic SQL statement
    ‚              exsr prepDynSQLStmt;

                  if SQLState = SQLStateOk;
    ‚   //Open dynamic cursor
                     exsr opnpdcursor;
       //read k_prodsed records
                     dow SQLState = SQLStateOk;
       //get batch record
                         exec sql
                          fetch pdcursor
                           into :prodsed_rec;

                         if SQLState = RowNotFound;
                            leave;
                         endif;
       //  for all forecast intervals
                         if r3_locn = pd_actlocn;
                            if pd_review = 0;
                               r3_totnot += 1;
                            endif;
                            if pd_review = 8 or
                               pd_review = 9;
                                  r3_totgone += 1;
                            endif;
                         endif;

       //  for weekly
                         if r3_forcint = 52 and r3_locn = pd_actlocn;
                            if pd_review = 1 and
                               pd_lastupd = per1;
                                  r3_totper1 += 1;
                            endif;
                            if pd_review = 1 and
                               pd_lastupd = per2;
                                  r3_totper2 += 1;
                            endif;
                            if pd_review = 1 and
                               pd_lastupd = per3;
                                  r3_totper3 += 1;
                            endif;
                            if pd_review = 1 and
                               pd_lastupd = per4;
                                  r3_totper4 += 1;
                            endif;
                            if pd_review = 1 and
                               pd_lastupd > per4;
                                  r3_totper5 += 1;
                            endif;
                         endif;

       //  for 13 4-weekly or monthly
                         if r3_forcint < 52 and r3_locn = pd_actlocn;
                            if pd_review = 1 and
                               pd_lastupd <= per1;
                                  r3_totper1 += 1;
                            endif;
                            if pd_review = 1 and
                               pd_lastupd > per1 and
                                  pd_lastupd <= per2;
                                     r3_totper2 += 1;
                            endif;
                            if pd_review = 1 and
                               pd_lastupd > per2 and
                                  pd_lastupd <= per3;
                                     r3_totper3 += 1;
                            endif;
                            if pd_review = 1 and
                               pd_lastupd > per3 and
                               pd_lastupd <= per4;
                                  r3_totper4 += 1;
                            endif;
                            if pd_review = 1 and
                               pd_lastupd > per4;
                                  r3_totper5 += 1;
                            endif;
                         endif;
                     enddo;
                     exsr clspdcursor;
                  endif;
       //  read rk_prodsed records finish

                  exsr updtperdper;

               endif;
         endif;

       //   read k_perdper records finish
       enddo;
       //------------------------------------------------------- end program
       exsr clsr3cursor;

       *inlr = *on;

       begsr dclr3cursor;
       exec sql
        declare r3cursor Cursor
          for
        select *
          from k_perdper
          where r3_comp = :comp
          order by r3_comp,
                   r3_user,
                   r3_locn,
                   r3_buyr,
                   r3_forcint,
                   r3_perdend,
                   r3_reqtype
          for update of r3_lastupd,
                        r3_totper1,
                        r3_totper2,
                        r3_totper3,
                        r3_totper4,
                        r3_totper5,
                        r3_totnot,
                        r3_totgone;
       endsr;

       begsr opnr3cursor;
       exec sql
        open r3cursor;
        if SQLState <> SQLStateOk;
           exsr clsr3cursor;
           exec sql
            open r3cursor;
        endif;
       endsr;

       begsr clsr3cursor;
       exec sql
        close r3cursor;
       endsr;

       begsr updtperdper;
       exec sql
       update k_perdper
        set r3_lastupd = :r3_lastupd,
            r3_totper1 = :r3_totper1,
            r3_totper2 = :r3_totper2,
            r3_totper3 = :r3_totper3,
            r3_totper4 = :r3_totper4,
            r3_totper5 = :r3_totper5,
            r3_totnot  = :r3_totnot,
            r3_totgone = :r3_totgone
        where current of r3cursor;
       endsr;

       begsr InzInpSrch;

       InpSrchcnd = *blanks;
       InpSrchCnd = 'pd_comp = ? and +
                     pd_batch = ? +
                     Order by pd_comp, +
                              pd_batch, +
                              pd_actbuyr, +
                              pd_actregn, +
                              pd_actlocn, +
                              pd_actsupl, +
                              pd_actsub, +
                              pd_prodseq, +
                              pd_prod';
       endsr;

       begsr dclpdcursor;
       exec sql
        declare pdcursor Cursor
         for DynSQLStmt;
       endsr;

       begsr IntSQLStmt;
       String = *blanks;
       String =   'Select * +
                   From K_Prodsed +
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

       begsr opnpdcursor;
       exec sql
        open pdcursor
         using :r3_comp,
               :r3_batch;
       endsr;

       begsr clspdcursor;
       exec sql
        close pdcursor;
       endsr;

      /end-free
