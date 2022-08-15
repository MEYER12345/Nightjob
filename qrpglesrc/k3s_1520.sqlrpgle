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
      **   Name: K3S_1520
      **   Type: ILE RPG Program
      **   Desc: Suggested order Grand totals build (batch) Night job
      **
      *****************************************************************
      **
      **  This program is used to display a list of batches for a
      **  buy group. The user can selectively pick different sets of
      **  batches by entering any mixture of components, like: region,
      **  location, supplier, batch type, batch status, begin date,
      **  end date, and user.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 10/01/2014.
      *   Remarks. Altered program to use SQL Select statement to access
      *            K_Company file.  Added SQL cursor to loop through
      *            K_Suplsoq and a dynamic SQL cursor to access
      *            file K_Buyrsot.
      *****************************************************************
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_1520        PI
     d  comp                          1
      * ---------------------------------------------------
     d xx_comp         s                   like(bt_comp)
     d cmsysdate       s               d
     d*
     d dmcomp          s                   like(so_comp)
     d dmbuyr          s                   like(so_buyr)
     d dmlocn          s                   like(so_locn)
     d dmsupl          s                   like(so_supl)
     d dmsuplsub       s                   like(so_suplsub)
     d dmsoqseq#       s                   like(so_soqseq#)
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
      * -------------------------------------------------------
     d suplsoq_rec   e ds                  ExtName(k_suplsoq)
     d buyrsot_rec   e ds                  ExtName(k_buyrsot)
      * -------------------------------------------------------
      /free
       exec sql

        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       //------------------------------------------------------ Once Routine
       exec sql
        Select cm_sysdate
          Into :cmsysdate
          From k_company
          Where cm_comp = :comp
          Fetch first row only;

       exsr dclbtcursor;

       exsr dclsocursor;
       //exsr clssocursor;
       exsr opnsocursor;

       exsr $_bld_rec;
       //-------------------------------------------------- End of Main Loop

       //finished, set on LR
       *inlr = *on;

       //**************************************************** End of program
       //////////////////////////////////////////// Build subfile of batches

       begsr $_bld_rec;
       //-------------------------------------------------------- Main Loop
       //main loop
       dow SQLState = SQLStateOK;
       //read thru batch records until end of file, or one page filled, or
       //    until new buy group read (if buy group selected)

          exec sql
           fetch next
            from socursor
            into :suplsoq_rec;

          if SQLState = RowNotFound;
             leave;
          endif;

          if so_comp = comp and
             so_actunet > 0;

             exsr InzInpSrch;
    ‚   //initialize StmtString
    ‚         exsr intSQLStmt;
    ‚   //prepare statement
    ‚         exsr prepDynSQLStmt;

             if SQLState = SQLStateOk; //if prep was successful

                exsr opnbtcursor;

                   if SQLState = SQLStateOk;
                      exec sql
                       fetch next
                       from btcursor
                       into :buyrsot_rec;

                      if so_soqtype = 'FC' and SQLState = SQLStateOk;
                           bt_duefc#n += 1;
                           bt_duefc#r += 1;
                           bt_duefcvn += so_actunet;
                           bt_duefcvr += so_actunet;
                      endif;
                      if so_soqtype = 'FC' and SQLState = RowNotFound;
                           exsr initzero;
                           bt_duefc#n = 1;
                           bt_duefc#r = 1;
                           bt_duefcvn = so_actunet;
                           bt_duefcvr = so_actunet;
                      endif;
                      if so_soqtype = 'ND' and SQLState = SQLStateOk;
                           bt_duend#n += 1;
                           bt_duend#r += 1;
                           bt_duendvn += so_actunet;
                           bt_duendvr += so_actunet;
                      endif;
                      if so_soqtype = 'ND' and SQLState = RowNotFound;
                           exsr initzero;
                           bt_duend#n = 1;
                           bt_duend#r = 1;
                           bt_duendvn = so_actunet;
                           bt_duendvr = so_actunet;
                      endif;
                      if so_soqtype = 'FB' and SQLState = SQLStateOk;
                           bt_duefb#n += 1;
                           bt_duefb#r += 1;
                           bt_duefbvn += so_actunet;
                           bt_duefbvr += so_actunet;
                      endif;
                      if so_soqtype = 'FB' and SQLState = RowNotFound;
                           exsr initzero;
                           bt_duefb#n = 1;
                           bt_duefb#r = 1;
                           bt_duefbvn = so_actunet;
                           bt_duefbvr = so_actunet;
                      endif;
                      if so_soqtype = 'RE' and SQLState = SQLStateOk;
                           bt_duerm#n += 1;
                           bt_duerm#r += 1;
                           bt_duermvn += so_actunet;
                           bt_duermvr += so_actunet;
                      endif;
                      if so_soqtype = 'RE' and SQLState = RowNotFound;
                           exsr initzero;
                           bt_duerm#n = 1;
                           bt_duerm#r = 1;
                           bt_duermvn = so_actunet;
                           bt_duermvr = so_actunet;
                      endif;
                      if so_soqtype <> 'FC' and
                         so_soqtype <> 'ND' and
                         so_soqtype <> 'FB' and
                         so_soqtype <> 'RE' and SQLState = SQLStateOk;
                           bt_notdu#n += 1;
                           bt_notdu#r += 1;
                           bt_notduvn += so_actunet;
                           bt_notduvr += so_actunet;
                      endif;
                      if so_soqtype <> 'FC' and
                         so_soqtype <> 'ND' and
                         so_soqtype <> 'FB' and
                         so_soqtype <> 'RE' and SQLState = RowNotFound;
                           exsr initzero;
                           bt_notdu#n = 1;
                           bt_notdu#r = 1;
                           bt_notduvn = so_actunet;
                           bt_notduvr = so_actunet;
                      endif;
                      bt_aprov# = 0;
                      bt_aprovv = 0;
                      bt_cancl# = 0;
                      bt_canclv = 0;

                     if SQLState = SQLStateOk;
                        exsr updtbuyrsot;
                     else;
                        bt_comp = comp;
                        bt_buyr = so_buyr;
                        bt_locn = so_locn;
                        bt_birth = cmsysdate;
                        bt_lastupd = cmsysdate;
                        bt_sysdate = cmsysdate;
                        exsr insertbuyrsot;
                     endif;
                     clear bt_duefc#n;
                     clear bt_duefc#r;
                     clear bt_duend#n;
                     clear bt_duend#r;
                     clear bt_duefb#n;
                     clear bt_duefb#r;
                     clear bt_duerm#n;
                     clear bt_duerm#r;
                     clear bt_notdu#n;
                     clear bt_notdu#r;
                     clear bt_duefcvn;
                     clear bt_duefcvr;
                     clear bt_duendvn;
                     clear bt_duendvr;
                     clear bt_duefbvn;
                     clear bt_duefbvr;
                     clear bt_duermvn;
                     clear bt_duermvr;
                     clear bt_notduvn;
                     clear bt_notduvr;
                   endif;
                   exsr clsbtcursor;
             endif;
          endif;
       enddo;

       exsr clssocursor;

       endsr;

       begsr dclsocursor;
       exec sql
        declare socursor Cursor
         for
         select *
         from k_suplsoq
         where so_comp = :comp
         Order by so_comp,
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

       Begsr IntSQLStmt;
       String = *blanks;
       String =   'Select * +
                   From K_Buyrsot +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrch;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'bt_comp = ? and +
                     bt_buyr = ? and +
                     bt_locn = ? and +
                     bt_sysdate = ? +
                     Order by bt_comp, +
                              bt_buyr, +
                              bt_locn, +
                              bt_sysdate';

       endsr;

       begsr opnbtcursor;
       exec sql
        open btcursor
          using :comp,
                :so_buyr,
                :so_locn,
                :cmsysdate;
       endsr;

       begsr PrepDynSQLStmt;
       exec sql
        Prepare DynSqlStmt
          From :StmtString;
       endsr;

       begsr dclbtcursor;
       exec sql
        declare btcursor dynamic Cursor
         for DynSQLStmt;
       endsr;

       begsr clsbtcursor;
       exec sql
        close btcursor;
       endsr;

       begsr updtbuyrsot;
       exec sql
        update k_buyrsot
          set bt_duefc#n = :bt_duefc#n,
              bt_duefc#r = :bt_duefc#r,
              bt_duefcvn = :bt_duefcvn,
              bt_duefcvr = :bt_duefcvr,
              bt_duend#n = :bt_duend#n,
              bt_duend#r = :bt_duend#r,
              bt_duendvn = :bt_duendvn,
              bt_duendvr = :bt_duendvr,
              bt_duefb#n = :bt_duefb#n,
              bt_duefb#r = :bt_duefb#r,
              bt_duefbvn = :bt_duefbvn,
              bt_duefbvr = :bt_duefbvr,
              bt_duerm#n = :bt_duerm#n,
              bt_duerm#r = :bt_duerm#r,
              bt_duermvn = :bt_duermvn,
              bt_duermvr = :bt_duermvr,
              bt_notdu#n = :bt_notdu#n,
              bt_notdu#r = :bt_notdu#r,
              bt_notduvn = :bt_notduvn,
              bt_notduvr = :bt_notduvr,
              bt_aprov# = :bt_aprov#,
              bt_aprovv = :bt_aprovv,
              bt_cancl# = :bt_cancl#,
              bt_canclv = :bt_canclv
          where current of btcursor;
       endsr;

       begsr insertbuyrsot;
         Exec sql
         insert into k_buyrsot
         values (:buyrsot_rec);
       endsr;


       begsr initzero;

       clear bt_duefc#n;
       clear bt_duefc#r;
       clear bt_duend#n;
       clear bt_duend#r;
       clear bt_duefb#n;
       clear bt_duefb#r;
       clear bt_duerm#n;
       clear bt_duerm#r;
       clear bt_notdu#n;
       clear bt_notdu#r;
       clear bt_duefcvn;
       clear bt_duefcvr;
       clear bt_duendvn;
       clear bt_duendvr;
       clear bt_duefbvn;
       clear bt_duefbvr;
       clear bt_duermvn;
       clear bt_duermvr;
       clear bt_notduvn;
       clear bt_notduvr;

       endsr;
      /end-free

