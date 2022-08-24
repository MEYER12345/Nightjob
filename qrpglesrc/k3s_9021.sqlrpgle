      *****************************************************************
     h copyright('(C) Copyright 1996 - 2008 King III Solutions, Inc.  +
     h Rel 5.00 2008-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)
     h BNDDIR('K3S_BNDDIR')
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
      **   Name: K3S_9021
      **   Type: ILE RPG Program
      **   Desc: Nite job - end by setting the processed flag
      **
      *****************************************************************
      **
      **   This program will update the ending time in the schedule
      **   with a timestamp field, and set the processed flag on.
      **
      **   Also, if this is a period ending run, the period ending
      **   record is updated with both the start and ending timestamp.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 04/14/2014.
      *   Remarks. Removed OPYQRYF CL statements pertaining to K3S_9021
      *            from K3S_NITECL and left the call to RPG program
      *            K3S_9021. In K3S_9021 program added code to use an
      *            SQL select statement to select first record where
      *            SY_COMP = passed company parameter and SY_EXCLUDE
      *            = 0 and SY_PROCFLG = 0. Then used an SQL cursor to
      *            read in the first row from the reult set where the
      *            the above criteria applied and to update the
      *            corresponding record in the K_SCHEDDY file.
      *****************************************************************
      * --------------------------------------------------------- Workfields
     d time_stamp      s               z   inz
     d save_date       s               d   datfmt(*iso)
     d procflg_hold    s              1s 0
     d #forcint        s              3  0

      * -------------------------------------------------- Parameters passed
     d/copy k3s_proto
      * -------------------------------------------------- Procedure interface
     d K3S_9021        PI
     d  comp                          1
     d  per_end_12                    1
     d  per_end_13                    1
     d  per_end_52                    1
     d  log_end                      26
     d  sysdate                      10
     d  ending_12                    10
     d  ending_13                    10
     d  ending_52                    10
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
      * -------------------------------------------------------
     d scheddy_rec   e ds                  ExtName(k_scheddy)
     d schedpe_rec   e ds                  ExtName(k_schedpe)
      * -------------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                closqlcsr = *endactgrp;

       exsr dclsecursor_9021;

       exsr dclsycursor_9021;
       exsr opnsycursor_9021;

       If SQLState = SQLStateOk;
        //fetch schedule row to be used for next system date
        //
          exec sql
           fetch next
            from sycursor_9021
            into :scheddy_rec;
       //--------------------------------------------------------- Main Loop
       //main loop
          If SQLState = SQLStateOk;

             sy_procflg = 1;
       //call module to retrieve timestamp
             callp K3S_Retrieve_Timestamp(time_stamp);
             log_end = %char(time_stamp:*iso);
             sy_procend = time_stamp;
             sy_lastupd = sy_sysdate;
             sysdate = %char(sy_sysdate);
       // update schedule

             exsr updscheddy;
             exsr clssycursor_9021;
       //---------------------------------------------- period ending time ?
       // if period ending flag is on, which is set back in program
       // K3S_9010, then we need to update beginning and ending timestamp
       // for appropriate periodicity.
       //
       //---------------------------------------------- forecast interval 12
             if per_end_12 = '1';
                #forcint = 12;
                exsr InzInpSrch;
    �   //initialize StmtString
    �            exsr intSQLStmt;
    �   //prepare statement
    �            exsr prepDynSQLStmt;

                if SQLState = SQLStateOk;         //If prepare was successful
    �   //open dynamic cursor
                   exsr opnsecursor_9021;
       //if we have processed exactly on the period ending date
       //then the record we update is the first record we read
                   if SQLState = SQLStateOk;

                      exec sql
                       fetch next
                       from secursor_9021
                       into :schedpe_rec;

                      if SQLState = SQLStateOk;

       //if we are processing right after the period ending date, then we
       //need to read backwards, to get the prior period ending record.
       //(the reason this could happen would be because of processing across
       // a weekend, or holiday, or any time that certain days have been
       // excluded in the schedule). Most of the period ending dates that
       //happen during a normal work week would use the read above, and
       //not need the readp below.
                         if se_ending <> sy_sysdate;

                            exec sql
                             fetch prior
                              from secursor_9021
                              into :schedpe_rec;
                         endif;

                         if SQLState = SQLStateOk;

                            se_lastupd = sy_sysdate;
                            se_procstr = sy_procstr;
                            se_procend = sy_procend;
                            se_procflg = 1;
                            ending_12 = %char(se_ending);
                            exsr updtschedpe;
                         endif;
                      endif;
                      exsr clssecursor_9021;
                   endif;
                endif;
             endif;
       //---------------------------------------------- forecast interval 13
             if per_end_13 = '1';
                #forcint = 13;
                exsr InzInpSrch;
    �   //initialize StmtString
    �            exsr intSQLStmt;
    �   //prepare statement
    �            exsr prepDynSQLStmt;

                if SQLState = SQLStateOk;         //If prepare was successful
    �   //open dynamic cursor
                   exsr opnsecursor_9021;
       //if we have processed exactly on the period ending date
       //then the record we update is the first record we read
                   if SQLState = SQLStateOk;

                      exec sql
                       fetch next
                       from secursor_9021
                       into :schedpe_rec;

                      if SQLState = SQLStateOk;

       //if we are processing right after the period ending date, then we
       //need to read backwards, to get the prior period ending record.
       //(the reason this could happen would be because of processing across
       // a weekend, or holiday, or any time that certain days have been
       // excluded in the schedule). Most of the period ending dates that
       //happen during a normal work week would use the read above, and
       //not need the readp below.
                         if se_ending <> sy_sysdate;

                            exec sql
                             fetch prior
                              from secursor_9021
                              into :schedpe_rec;
                         endif;

                         if SQLState = SQLStateOk;

                            se_lastupd = sy_sysdate;
                            se_procstr = sy_procstr;
                            se_procend = sy_procend;
                            se_procflg = 1;
                            ending_13 = %char(se_ending);
                            exsr updtschedpe;
                         endif;
                      endif;
                      exsr clssecursor_9021;
                   endif;
                endif;
             endif;
       //---------------------------------------------- forecast interval 52
             if per_end_52 = '1';
                #forcint = 52;
                exsr InzInpSrch;
    �   //initialize StmtString
    �            exsr intSQLStmt;
    �   //prepare statement
    �            exsr prepDynSQLStmt;

                if SQLState = SQLStateOk;         //If prepare was successful
    �   //open dynamic cursor
                   exsr opnsecursor_9021;
       //if we have processed exactly on the period ending date
       //then the record we update is the first record we read
                   if SQLState = SQLStateOk;

                      exec sql
                       fetch next
                       from secursor_9021
                       into :schedpe_rec;

                      if SQLState = SQLStateOk;

       //if we are processing right after the period ending date, then we
       //need to read backwards, to get the prior period ending record.
       //(the reason this could happen would be because of processing across
       // a weekend, or holiday, or any time that certain days have been
       // excluded in the schedule). Most of the period ending dates that
       //happen during a normal work week would use the read above, and
       //not need the readp below.
                         if se_ending <> sy_sysdate;

                            exec sql
                             fetch prior
                              from secursor_9021
                              into :schedpe_rec;
                         endif;

                         if SQLState = SQLStateOk;

                            se_lastupd = sy_sysdate;
                            se_procstr = sy_procstr;
                            se_procend = sy_procend;
                            se_procflg = 1;
                            ending_52 = %char(se_ending);
                            exsr updtschedpe;
                         endif;
                      endif;
                      exsr clssecursor_9021;
                   endif;
                endif;
             endif;
       //-------------------------------------------------------------------
          endif;

       endif;

       //finished, set on LR
       *inlr = *on;

       begsr dclsycursor_9021;
       exec sql
        declare sycursor_9021 Cursor
         for
         select *
         from k_scheddy
         where sy_comp = :comp and
               sy_exclude = 0 and
               sy_procflg = 0
         order by sy_comp,
                  sy_sysdate
         for update of sy_procflg,
                       sy_procend,
                       sy_lastupd;
       endsr;

       begsr opnsycursor_9021;
       exec sql
        open sycursor_9021;
        if SQLState <> SQLStateOk;
           exsr clssycursor_9021;
           exec sql
            open sycursor_9021;
        endif;
       endsr;

       begsr updscheddy;
       exec sql
        update k_scheddy
         Set sy_procflg = :sy_procflg,
             sy_procend = :sy_procend,
             sy_lastupd = :sy_lastupd
         where current of sycursor_9021;
       endsr;

       begsr clssycursor_9021;
       exec sql
        close sycursor_9021;
       endsr;

       Begsr IntSQLStmt;
       String = *blanks;
       String =   'Select * +
                   From K_Schedpe +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrch;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'se_comp = ? and +
                     se_forcint = ? and +
                     se_ending >= ? +
                     Order by se_comp, +
                              se_forcint, +
                              se_ending +
                     for update of se_lastupd, +
                                   se_procstr, +
                                   se_procend, +
                                   se_procflg';
       endsr;

       begsr opnsecursor_9021;
       exec sql
        open secursor_9021
          using :comp,
                :#forcint,
                :sy_sysdate;
       endsr;

       begsr PrepDynSQLStmt;
       exec sql
        Prepare DynSqlStmt
          From :StmtString;
       endsr;

       begsr dclsecursor_9021;
       exec sql
        declare secursor_9021 dynamic scroll Cursor
         for DynSQLStmt;
       endsr;

       begsr clssecursor_9021;
       exec sql
        close secursor_9021;
       endsr;

       begsr updtschedpe;
       exec sql
        update k_schedpe
          set se_lastupd = :se_lastupd,
              se_procstr = :se_procstr,
              se_procend = :se_procend,
              se_procflg = :se_procflg
          where current of secursor_9021;
       endsr;
      /end-free
      * ***************************************************** End of program
