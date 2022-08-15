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
      **   Name: K3S_9046
      **   Type: ILE RPG Program
      **   Desc: Clear first time user sign on for today flags
      **
      *****************************************************************
      **
      **   The record types to be cleared are:
      **
      **      UDW - User date warning
      **      UPL - User product log
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 06/01/2014.
      **  Remarks. Altered program to utilize a dynamic SQL cursor to
      **           loop thru K_TABLCOD file twice. The first time it
      **           updates records with ta_codeval = 'UDW' and the
      **           second time it will update records with ta_codeval
      **           of 'UPL'. Used a parameter marker for the code
      **           value in building the dynamic select statement as
      **           that value will change from the first loop to the
      **           second.
      *****************************************************************
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
     d codetyp         s             20a   inz
      * -------------------------------------------------------
     d tablcod_rec   e ds                  ExtName(k_tablcod)
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_9046        PI
     d  comp                          1    const
     d*
      /free
       //------------------------------------------------------- UDW records
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;

       //declare dynamic cursor
       exsr dcltacursor;
       // clear 'User date warning' records
       codetyp = 'UDW';
       exsr InzInpSrch;
    ‚   //initialize StmtString
    ‚   exsr intSQLStmt;
    ‚   //prepare statement
    ‚   exsr prepDynSQLStmt;
       if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
          exsr opntacursor;
       //set to supplier
          Dow SQLState = SQLStateOk;
             exec sql
              fetch next
              from tacursor
              into :tablcod_rec;

              if SQLState = RowNotFound;
                leave;
              endif;

              exsr updttablcod;
          enddo;

          exsr clstacursor;

       endif;
       //------------------------------------------------------- UPL records
       // clear 'User product log'  records
       codetyp = 'UPL';
       exsr InzInpSrch;
    ‚   //initialize StmtString
    ‚   exsr intSQLStmt;
    ‚   //prepare statement
    ‚   exsr prepDynSQLStmt;
       if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
          exsr opntacursor;
       //set to supplier
          Dow SQLState = SQLStateOk;
             exec sql
              fetch next
              from tacursor
              into :tablcod_rec;

              if SQLState = RowNotFound;
                leave;
              endif;

              exsr updttablcod;
          enddo;

          exsr clstacursor;
       endif;

       *inlr = *on;

       begsr opntacursor;
       exec sql
        open tacursor
          using :comp,
                :codetyp;
       endsr;

       begsr clstacursor;
       exec sql
        close tacursor;
       endsr;

       //Syntactically verify SQL statement
       begsr PrepDynSQLStmt;
       exec sql
        Prepare DynSqlStmt
          From :StmtString;
       endsr;

       begsr IntSQLStmt;
       String = *blanks;
       String =  'Select * +
                  From K_Tablcod +
                  Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       begsr InzInpSrch;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'ta_comp = ? and +
                     ta_codetyp = ? +
                     order by ta_comp, +
                              ta_codetyp +
                     for update of ta_flag1';
       endsr;

       begsr dcltacursor;
       exec sql
        declare tacursor Cursor
         for DynSQLStmt;
       endsr;

       begsr updttablcod;
       exec sql
         update k_tablcod
           set ta_flag1 = 0
           where current of tacursor;
       endsr;
      /end-free
