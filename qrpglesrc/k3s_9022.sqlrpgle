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
      **   Name: K3S_9022
      **   Type: ILE RPG Program
      **   Desc: Nite job - final 'begin & end' time stamp
      **
      *****************************************************************
      **
      **   This program will update the beginning and ending schedule
      **   with a timestamp field, and total #'s of records processed
      **   for each of the log types. This program is also the very last
      **   program to run in the night program K3S_NITECL.
      **
      **   Also, if this is a period ending run, the period ending
      **   record is updated with a final 'begin & end' timestamp.
      **
      *****************************************************************
      **                                                               *
      **   Change ID  Change Date Change Description                   *
      **   ---------  ----------  -------------------------------------*
      **     k4MON    01-04-2019  Inserting Copy Books to address      *
      **                          the calling of AR_MONITOR.           *
      **                                                               *
      *****************************************************************
      **
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 06/11/2014.
      **  Remarks. Altered program to utilize SQL select statement to
      **           access K_SCHEDDY record.  Also, changed it to
      **           use SQL update statements to update the proper
      **           fields in K_NITELOG and K_SCHEDPE.
      **
      *****************************************************************
      * --------------------------------- Local data area *LDA definitions
     d/COPY K3S_C035
      * --------------------------------------------------------- Workfields
     d procstr         s               z
     d procend         s               z
     d nltimestp       s               z
     d tacodeds1       s            100
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d scheddy_rec   e ds                  ExtName(k_scheddy)
      *--------------------------------------------------------
     d wk_codetyp      s              3    inz('LIB')                           Access Code
     d wk_codeval      s             20    inz('K3S_DTA')                       Access Value
     d wk_library      s             25    inz                                  Library
     d*wk_jobid        s             25    inz('Night Job')                     Job ID
     d wk_jobid        s             25    inz('Night Job Complete')            Job ID
     d wk_process      s              1    inz('1')                             Process Type
      * --------------------- Parameters passed Dashboard Update
     d AR_MONITOR      PR                  extpgm('AR_MONITOR')
     d  Library                      25                                               Library
     d  JobId                        25                                                Job Id
     d  Process                       1                                                 Process
      * -------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * -------------------------------------------------- Procedure interface
     d K3S_9022        PI
     d  comp                          1
     d  per_end_12                    1
     d  per_end_13                    1
     d  per_end_52                    1
     d  sysdate                      10
     d  ending12                     10
     d  ending13                     10
     d  ending52                     10
     d  series                        7  0

      /free
       //------------------------------------------------------ once routine
       exec sql
         set option commit = *none,
                    datfmt = *iso,
                 closqlcsr = *endactgrp;
       // get today's schedule record
       exec sql
         select *
           into :scheddy_rec
           from k_scheddy
           where sy_comp = :comp and
                 sy_sysdate = date(:sysdate)
           fetch first row only;
       if SQLState = SQLStateOk;
       //get begin timestamp
          exec sql
            select nl_timestp
              into :nltimestp
              from k_nitelog
              where nl_comp = :comp and
                    nl_logtype = 'B' and
                    nl_series = :series
              fetch first row only;
       //Night job 'B' begin timestamp, 'F' final timestamp
          if SQLState = SQLStateOk;
             procstr = nltimestp;

       // get ending timestamp
             exec sql
               select nl_timestp
                 into :nltimestp
                 from k_nitelog
                 where nl_comp = :comp and
                       nl_logtype = 'F' and
                       nl_series = :series;
             if SQLState = SQLStateOk;
                procend = nltimestp;

       // update today's schedule record
                sy_procstr = procstr;
                sy_procend = procend;

       //retrieve local data area
                in *dtaara;

                sy_recproc = lda_recprc;
                sy_rectyp1 = lda_rctyp1;
                sy_rectyp2 = lda_rctyp2;
                sy_rectyp3 = lda_rctyp3;
                sy_rectyp4 = lda_rctyp4;
                sy_rectyp5 = lda_rctyp5;
                sy_rectyp6 = lda_rctyp6;
                sy_rectyp7 = lda_rctyp7;
                sy_rectyp8 = lda_rctyp8;
                sy_rectyp9 = lda_rctyp9;
                sy_rectypx = lda_rctypx;

       //     update local data area *lda
                out *dtaara;

                exsr update_scheddy;

       //---------------------------------------------- period ending time ?
       // if period ending flag is on, which is set back in program
       // K3S_9010, then we need to update begin & end timestamps
       // for appropriate periodicity.

       //---------------------------------------------- forecast interval 12
                if       per_end_12 = '1';
       //update schedule period end record
                         exec sql
                           update k_schedpe
                             set se_procstr = :procstr,
                                 se_procend = :procend
                             where se_comp = :comp and
                                   se_forcint = 12 and
                                   se_ending = date(:ending12);

                endif;

       //---------------------------------------------- forecast interval 13
                if       per_end_13 = '1';
       //update schedule period end record
                         exec sql
                           update k_schedpe
                             set se_procstr = :procstr,
                                 se_procend = :procend
                             where se_comp = :comp and
                                   se_forcint = 13 and
                                   se_ending = date(:ending13);

                endif;

        //--------------------------------------------- forecast interval 52
                if       per_end_52 = '1';
       //update schedule period end record
                         exec sql
                           update k_schedpe
                             set se_procstr = :procstr,
                                 se_procend = :procend
                             where se_comp = :comp and
                                   se_forcint = 52 and
                                   se_ending = date(:ending52);
                endif;

        //------------------------------------------------------------------

             endif;
          endif;
       endif;
       //---------------------------------------------------------------------
       // Invoke AR_MONITOR.
       //---------------------------------------------------------------------
       exsr $_call_mon;

       // finished, set on LR
       *inlr = *on;

       begsr update_scheddy;
       exec sql
        update k_scheddy
          set sy_procstr = :sy_procstr,
              sy_procend = :sy_procend,
              sy_recproc = :sy_recproc,
              sy_rectyp1 = :sy_rectyp1,
              sy_rectyp2 = :sy_rectyp2,
              sy_rectyp3 = :sy_rectyp3,
              sy_rectyp4 = :sy_rectyp4,
              sy_rectyp5 = :sy_rectyp5,
              sy_rectyp6 = :sy_rectyp6,
              sy_rectyp7 = :sy_rectyp7,
              sy_rectyp8 = :sy_rectyp8,
              sy_rectyp9 = :sy_rectyp9,
              sy_rectypx = :sy_rectypx
          where sy_comp = :comp and
                sy_sysdate = date(:sysdate);
       endsr;

       //-----------------------------------------------------------------
       //-------- Get Tablcod and invoke AR_MONITOR
       //-----------------------------------------------------------------
       begsr $_call_mon;

       exec sql
       select ta_codeds1
        into :tacodeds1
         from k_tablcod
          where ta_comp = :comp and
                ta_codetyp = :wk_codetyp and
                ta_codeval = :wk_codeval
                fetch first row only;
       if SQLState = SQLStateOk;
          clear wk_library;
          %subst(wk_library:1:10) = %subst(tacodeds1:1:10);

          callp AR_MONITOR(wk_library:
                           wk_jobid:
                           wk_process);
       endif;

       endsr;
      /end-free
       //***************************************************** End of program
