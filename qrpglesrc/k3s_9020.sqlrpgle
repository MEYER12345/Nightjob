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
      **   Name: K3S_9020
      **   Type: ILE RPG Program
      **   Desc: Nite job - begin by getting the next schedule date
      **
      *****************************************************************
      **
      **  This program is used to update the location records with the
      **  next schedule date. Also, a test is made to see if a period end
      **  has occured, and if so, update the current period values.
      **  The program also passes the next schedule date as a parm,
      **  back out to the CL process.
      **
      **  The company system date is also updated.
      **
      **  A test is made to see if today is the Alternate Source processing
      **  day, and if so, the correct value is sent back to the CL process.
      **
      **      alt_src = '1' this is alternate source processing day
      **              = '0' this is not alternate source processing day
      **
      **  This program was originally designed to handle multiple
      **  schedule records (1 for each location), but changed to be
      **  more of a company specific schedule. Therefore, the location
      **  fields have been taken out of the daily schedule and PE schedule
      **  files.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 04/14/2014.
      *   Remarks. Removed OPYQRYF CL statements pertaining to K3S_9020
      *            from K3S_NITECL and left the call to RPG program
      *            K3S_9020 In K3S_9020 program added code to use an                               .
      *            SQL select statement to select first record where
      *            SY_COMP = passed company parameter and SY_EXCLUDE
      *            = 0 and SY_PROCFLG = 0. Then used an SQL cursor to
      *            read in the first row from the reult set where the
      *            the above criteria applied and to update the
      *            corresponding record in the K_SCHEDDY file.
      *
      *****************************************************************
      * --------------------------------------------------------- Workfields
     d time_stamp      s               z   inz
     d save_date       s               d   datfmt(*iso)
     d msgt            s             40
     d #forcint        s              3  0
      * --------------------------------------------------- parameter passed prototype
     d/copy 'k3s_proto'
      * ----------------------------------------------------- procedure interface
     d K3S_9020        PI
     d  comp                          1
     d  alt_src                       1
     d  log_start                    26
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
      * -------------------------------------------------------
     d schedpe_rec   e ds                  ExtName(k_schedpe)
     d scheddy_rec   e ds                  ExtName(k_scheddy)
     d locatns_rec   e ds                  ExtName(k_locatns)
      * -------------------------------------------------------
      /free
       exec sql
        set option commit = *none,
                closqlcsr = *endactgrp;

       exsr dclsecursor_9020;

       exsr dclsycursor_9020;
       //exsr clssycursor_9020;
       exsr opnsycursor_9020;


       If SQLState = SQLStateOk;

       //fetch schedule record to be used for next system date
          exec sql
           fetch next
            from sycursor_9020
            into :scheddy_rec;

          If SQLState = SQLStateOk;

        //save date for company record
             save_date = sy_sysdate;

        //is today alternate source processing day?  1=yes, 0=no
             if sy_procalt = 1;
               alt_src = '1';
             else;
               alt_src = '0';
             endif;

           //call subprocedure to retrieve time stamp
             callp K3S_Retrieve_Timestamp(time_stamp);
             sy_procstr = time_stamp;
             log_start = %char(time_stamp:*iso);
             sy_lastupd = sy_sysdate;

             exsr updscheddy;
          endif;

        //--------------------------------------------------- read locations
          exsr dcllccursor_9020;
          //exsr clslccursor_9020;
          exsr opnlccursor_9020;

       //--------------------------------------------------------- Main Loop
       // main loop
          dow SQLState = SQLStateOk;

       //read first location record
            exec sql
              fetch next
              from lccursor_9020
              into :locatns_rec;

            if SQLState = RowNotFound;
              leave;
            endif;

       //if location is set to be processed (process night jo
       //and this is the same company, continue

            if lc_process = 1 AND lc_comp = comp;

       //read schedule period end records, determine which periods used

        //--------------------------------------------- forecast interval 12
        //  is forecast interval 12 being used?
                #forcint = 12;
                exsr InzInpSrch;
    �   //initialize StmtString
    �            exsr intSQLStmt;
    �   //prepare statement
    �            exsr prepDynSQLStmt;

                if SQLState = SQLStateOk;         //If prepare was successful
    �   //open dynamic cursor
                   exsr opnsecursor_9020;
          //is forecast interval 12 being used?
                   if SQLState = SQLStateOk;

                      exec sql
                       fetch next
                        from secursor_9020
                        into :schedpe_rec;

                      if SQLState = SQLStateOk;
          //today is period end, so read next record for current year & perd
                         if se_ending = sy_sysdate;

                            exec sql
                             fetch next
                             from secursor_9020
                             into :schedpe_rec;

                         endif;
                         if SQLState = SQLStateOk;
                            lc_year12 = se_forcyr;
                            lc_perd12 = se_forcper;
                         endif;
                      endif;
                      exsr clssecursor_9020;
                   endif;
                endif;
            //----------------------------------------- forecast interval 13
            //is forecast interval 13 being used?
                #forcint = 13;
                exsr InzInpSrch;
    �   //initialize StmtString
    �            exsr intSQLStmt;
    �   //prepare statement
    �            exsr prepDynSQLStmt;

                if SQLState = SQLStateOk;         //If prepare was successful
    �   //open dynamic cursor
                   exsr opnsecursor_9020;
          //is forecast interval 13 being used?
                   if SQLState = SQLStateOk;

                      exec sql
                       fetch next
                        from secursor_9020
                        into :schedpe_rec;

                      if SQLState = SQLStateOk;
          //today is period end, so read next record for current year & perd
                         if se_ending = sy_sysdate;

                            exec sql
                             fetch next
                             from secursor_9020
                             into :schedpe_rec;

                         endif;
                         if SQLState = SQLStateOk;
                            lc_year13 = se_forcyr;
                            lc_perd13 = se_forcper;
                         endif;
                      endif;
                      exsr clssecursor_9020;
                   endif;
                endif;

          //------------------------------------------- forecast interval 52
          //is forecast interval 52 being used?
                #forcint = 52;
                exsr InzInpSrch;
    �   //initialize StmtString
    �            exsr intSQLStmt;
    �   //prepare statement
    �            exsr prepDynSQLStmt;

                if SQLState = SQLStateOk;         //If prepare was successful
    �   //open dynamic cursor
                   exsr opnsecursor_9020;
          //is forecast interval 52 being used?
                   if SQLState = SQLStateOk;

                      exec sql
                       fetch next
                        from secursor_9020
                        into :schedpe_rec;

                      if SQLState = SQLStateOk;
          //today is period end, so read next record for current year & perd
                         if se_ending = sy_sysdate;

                            exec sql
                             fetch next
                             from secursor_9020
                             into :schedpe_rec;

                         endif;
                         if SQLState = SQLStateOk;
                            lc_year52 = se_forcyr;
                            lc_perd52 = se_forcper;
                         endif;
                      endif;
                      exsr clssecursor_9020;
                   endif;
                endif;
          //----------------------------------------------------------------

         //update location
                lc_sysdate = sy_sysdate;
                lc_lastupd = sy_sysdate;
                exsr updlocatns;

            endif;

          //read location records end of loop
          enddo;

          exsr clslccursor_9020;
          exsr clssycursor_9020;


       //-------------------------------------------------- End of Main Loop

       //update company file with next schedule date
          exec sql
             Update k_company
             set cm_sysdate = :save_date,
                 cm_lastupd = :save_date
             where cm_comp = :comp;

       endif;
       //finished, set on LR
       *inlr = *on;

       begsr dclsycursor_9020;
       exec sql
        declare sycursor_9020 Cursor
         for
         select *
         from k_scheddy
         where sy_comp = :comp and
               sy_exclude = 0 and
               sy_procflg = 0
         order by sy_comp,
                  sy_sysdate
         for update of sy_procstr,
                       sy_lastupd;
       endsr;

       begsr opnsycursor_9020;
       exec sql
        open sycursor_9020;
        if SQLState <> SQLStateOk;
           exsr clssycursor_9020;
           exec sql
            open sycursor_9020;
        endif;
       endsr;

       begsr updscheddy;
       exec sql
        update k_scheddy
         Set sy_procstr = :sy_procstr,
             sy_lastupd = :sy_lastupd
         where current of sycursor_9020;
       endsr;

       begsr clssycursor_9020;
       exec sql
        close sycursor_9020;
       endsr;

       begsr dcllccursor_9020;
       exec sql
        declare lccursor_9020 cursor
         for
         select *
         from k_locatns
         order by lc_comp,
                  lc_locn
         for update of lc_year12,
                       lc_perd12,
                       lc_year13,
                       lc_perd13,
                       lc_year52,
                       lc_perd52,
                       lc_sysdate,
                       lc_lastupd;
       endsr;

       begsr opnlccursor_9020;
       exec sql
        open lccursor_9020;
       endsr;

       begsr updlocatns;
       exec sql
        update k_locatns
         Set lc_year12 = :lc_year12,
             lc_perd12 = :lc_perd12,
             lc_year13 = :lc_year13,
             lc_perd13 = :lc_perd13,
             lc_year52 = :lc_year52,
             lc_perd52 = :lc_perd52,
             lc_sysdate = :lc_sysdate,
             lc_lastupd = :lc_lastupd
        where current of lccursor_9020;
       endsr;

       begsr clslccursor_9020;
       exec sql
        close lccursor_9020;
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
                              se_ending';
       endsr;

       begsr opnsecursor_9020;
       exec sql
        open secursor_9020
          using :lc_comp,
                :#forcint,
                :sy_sysdate;
       endsr;

       begsr PrepDynSQLStmt;
       exec sql
        Prepare DynSqlStmt
          From :StmtString;
       endsr;

       begsr dclsecursor_9020;
       exec sql
        declare secursor_9020 Cursor
         for DynSQLStmt;
       endsr;

       begsr clssecursor_9020;
       exec sql
        close secursor_9020;
       endsr;
      /end-free
       //**************************************************** End of program
