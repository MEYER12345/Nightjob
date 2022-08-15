      *****************************************************************
     h copyright('(C) Copyright 1996 - 2009 King III Solutions, Inc.  +
     h Rel 5.04 2009-01-19       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)

      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2009 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_7100
      **   Type: ILE RPG Program
      **   Desc: Create new period end exception records
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 05/17/2014.
      *   Remarks. Changed code to utilize a cursor to loop through file
      *            K_PRODSEB. Also, used dynamic cursors to access files
      *            K_PERDPER and K_PRODSED. In addition, used an SQL
      *            select statement to access K_COMPANY.
      *****************************************************************
      * -------------------------------------------------- Parameters passed
     d*pbforcint#      s              3  0                                      location
      * --------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d cmsysdate       s               d
     d initdate        s               d
      * -------------------------------------------------------
     d prodseb_rec   e ds                  ExtName(k_prodseb)
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
     d K3S_7100        PI
     d  comp                          1
     d  per12                         1
     d  per13                         1
     d  per52                         1
     d  use_date                     10

      /free
       exec sql
       set option commit = *none,
                  datfmt = *iso,
               closqlcsr = *endactgrp;

       exsr dclr3cursor;
       exsr dclpdcursor;

       exec sql
        Select cm_sysdate
         into :cmsysdate
         from k_company
         fetch first row only;

       // if specific date passed, this is not from K3S_NITECL
       // programmer trying to re-build specific batches
        if        use_date <> '0001-01-01';
                  cmsysdate = %date(use_date:*ISO);
        endif;

       exsr dclpbcursor;
       exsr opnpbcursor;
       //-------------------------------------------------------- Main Loop
       //main loop
       //read batch records
       dow SQLState = SQLStateOk;

       //fetch batch record
         exec sql
          fetch next
            from pbcursor
            into :prodseb_rec;

         if SQLState = RowNotFound;
            Leave;
         endif;

         If pb_reqtype >= 'PE1' and
            pb_reqtype <= 'PE5' and
            pb_birth = cmsysdate;
       //**              move      pb_descr      pbforcint#
               exsr InzInpSrch2;
    ‚   //Initialize StmtString
    ‚           exsr intSQLStmt2;
    ‚   //Prepare dynamic SQL statement
    ‚           exsr prepDynSQLStmt2;

               if SQLState = SQLStateOk;
    ‚   //Open dynamic cursor
                  exsr opnpdcursor;

                  Dow SQLState = SQLStateOk;

                     exec sql
                       fetch next
                         from pdcursor
                         into :prodsed_rec;

                     if SQLState = RowNotFound;
                       leave;
                     endif;

                     exsr InzInpSrch;
    ‚   //Initialize StmtString
    ‚                 exsr intSQLStmt;
    ‚   //Prepare dynamic SQL statement
    ‚                 exsr prepDynSQLStmt;

                     if SQLState = SQLStateOk;
    ‚   //Open dynamic curosr
                        exsr opnr3cursor;

                        if SQLState = SQLStateOk;
                           exec sql
                            fetch r3cursor
                              into :perdper_rec;

                             Select;

                               When SQLState = SQLStateOk;
                                If r3_birth = cmsysdate;
                                 exec sql                   -----row found
                                   Update k_perdper
                                     Set r3_tottype = r3_tottype + 1
                                     Where current of r3cursor;
                                endif;

                               When SQLState = RowNotFound;
                                 exsr insertrecd;
                             endsl;
                             exsr clsr3cursor;
                        endif;
                     endif;
                  enddo;
                  exsr clspdcursor;
                  SQLState = SQLStateOk;
               endif;
         endif;
       enddo;
       //------------------------------------------------------- end program
       exsr clspbcursor;

       *inlr = *on;

       begsr dclpbcursor;
       exec sql
        Declare pbcursor Cursor
         for
         Select *
         From k_prodseb
         Where pb_comp = :comp
         Order by pb_comp,
                  pb_status,
                  pb_actbuyr,
                  pb_batch,
                  pb_actregn,
                  pb_actlocn,
                  pb_actsupl,
                  pb_actsub;
       endsr;

       begsr opnpbcursor;
       exec sql
        open pbcursor;
        if SQLState <> SQLStateOk;
           exsr clspbcursor;
           exec sql
            open pbcursor;
        endif;
       endsr;

       begsr clspbcursor;
       exec sql
        close pbcursor;
       endsr;

       begsr opnr3cursor;
       exec sql
        open r3cursor
         using :pb_comp,
               :pb_actuser,
               :pd_actlocn,
               :pb_actbuyr,
               :pb_forcint,
               :pb_birth,
               :pb_reqtype;
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
                     r3_forcint = ? and +
                     r3_perdend = ? and +
                     r3_reqtype = ? +
                     Order by r3_comp, +
                              r3_user, +
                              r3_locn, +
                              r3_buyr, +
                              r3_forcint, +
                              r3_perdend, +
                              r3_reqtype';
       endsr;

       begsr insertrecd;
       exec sql
        set :initdate = date('0001-01-01');
       exec sql                   -----row not found (insert)
        insert into k_perdper
          (r3_comp,
           r3_user,
           r3_locn,
           r3_buyr,
           r3_forcint,
           r3_perdend,
           r3_reqtype,
           r3_birth,
           r3_lastupd,
           r3_tottype,
           r3_batch,
           r3_totprod,
           r3_totper1,
           r3_totper2,
           r3_totper3,
           r3_totper4,
           r3_totper5,
           r3_totnot,
           r3_totgone)
        values (:pb_comp,
                :pb_actuser,
                :pd_actlocn,
                :pb_actbuyr,
                :pb_forcint,
                :pb_birth,
                :pb_reqtype,
                :cmsysdate,
                :initdate,
                1,
                :pb_batch,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0);
       endsr;

       begsr IntSQLStmt2;
       String = *blanks;
       String =   'Select * +
                   From K_Prodsed +
                   Where ';
       //
       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       begsr InzInpSrch2;

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

       begsr opnpdcursor;
       exec sql
        open pdcursor
         using :pb_comp,
               :pb_batch;
       endsr;

       begsr clspdcursor;
       exec sql
        close pdcursor;
       endsr;

       begsr dclpdcursor;
       exec sql
        declare pdcursor Cursor
         for DynSQLStmt2;
       endsr;

       begsr PrepDynSQLStmt2;
       exec sql
        Prepare DynSqlStmt2
          From :StmtString;
       endsr;
      /end-free
