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
      **   Name: K3S_9031
      **   Type: ILE RPG Program
      **   Desc: Load *LDA with user specific information for RMVUSR
      **
      *****************************************************************
      **
      **  This program is used to load the *LDA with user criteria
      **  and would be in the first series of programs to run when
      **  a user signs on.
      **
      *****************************************************************
      **
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 05/28/2014.
      **  Remarks. Altered program to utilize embedded SQL statements
      **           to access K_USERPRF, K_COMPANY, and K_TABLCOD
      **           records instead of using native RPGLE CHAIN
      **           statements.
      **
      *****************************************************************
      * ------------------------------------------------ convert date fields
     d date_work       s             10    inz(*blanks)

      * ---------------------------------------------------- Local Data Area
     d/copy k3s_c030

      * -------------------------------------- Program Status Data Structure
     d/copy k3s_c040

      * ----------------------------------- D-specs for common workfields
     d/copy k3s_c270
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_9031        PI
     d  nj_user                      10
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d userprf_rec   e ds                  ExtName(k_userprf)
     d company_rec   e ds                  ExtName(k_company)
     d tablcod_rec   e ds                  ExtName(k_tablcod)
      * -------------------------------------------------------
        ///////////////////////////////////////////////// get data area *lda

      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                   closqlcsr = *endactgrp;

       //retrieve local data area *lda
         in *dtaara;

       //////////////////////////////////////////////// get user id criteria

        //get user record
         exec sql
           select *
             into :userprf_rec
             from k_userprf
             where us_user = :nj_user
             fetch first row only;
        /////////////////////////////////////////////////// get company info

        //get company record
         exec sql
           select *
             into :company_rec
             from k_company
             where cm_comp = :us_comp
             fetch first row only;

         ////////////////////////////////// get user domain and manager info

        //get table code record for 'USR'
           exec sql
             select *
               into :tablcod_rec
               from k_tablcod
               where ta_comp = :us_comp and
                     ta_codetyp = 'USR' and
                     ta_codeval = :nj_user
                     fetch first row only;
        //////////////////////////////////////////////// prime user lda area

         //prime user id area

         //user id area
           lda_userid = nj_user;

         //company id
           lda_comp  = us_comp;

         //company code
           lda_compcd = cm_compcod;

         //buy group
           lda_buyr   = us_buyr;

         //location
           lda_locn   = us_locn;

         //user prefered date format
           lda_usrdat = us_datfmt;

         //user prefered time format
           lda_usrtim = us_timfmt;

         //user prefered time adjustment in hours
           lda_usradj = us_timadjs;

         //today's K3S-Replenish system date in *ISO format
           lda_sysdat = cm_sysdate;

         //   today's AS/400 system date in *ISO format
         //call subprocedure to retrieve time stamp
           callp K3S_Retrieve_Timestamp(time_stamp);
           lda_cmpdat = %date(%subst(%char(time_stamp):1:10):*iso);

         //company directed display of product ID and description
           lda_trmprd = cm_trmprod;

         //user preference for alarm
           lda_alarm  = us_alarm;

         //number of days past end date, that deals expire
           lda_dealex = cm_dealexp;

         //user preference for display of deal allowances $,D,%
           lda_dealal = us_dealal;

         //user preference for display of deal price increases $,D,%
           lda_dealpi = us_dealpi;

         //user preference for entry of dates as six digits (flag  1,0)
           lda_date6  = us_datesix;

         //user preference for 'if error, display window' (flag  1,0)
           lda_window = us_ewindow;

         //user preference for order summary view
           lda_orview = us_ordview;

         //user preference for order summary mode
           lda_ormode = us_ordmode;

         //user preference for product summary view
           lda_prview = us_prdview;

         //user preference for selected products review batches dflt view
           lda_slview = us_selview;

         //user preference for product summary option
           lda_proptn = us_prdoptn;

         //user preference for product history type display
           lda_hisdsp = us_hisdspl;

         //user preference for product tracking signal > display
           lda_trcksg = us_tracksg;

         //user preference for unit selected for orders display, 1 - 6
           lda_untdsp = us_unitdsp;

         //user preference for select check for order detail
           lda_selchk = us_selchek;

         //user preference for reverse image flag on SOQ over-ride
           lda_prdsum = us_prodsum;

         //user preference for SOQ <> buy multiple flag in product detl
           lda_mltwrn = us_multwrn;

         //user preference for alternate source availability view
           lda_altvu  = us_altview;

         //user preference for # days to blink F=19 Log
           lda_logwrn = us_logwarn;

         //user preference for # days to expire notes
           lda_exnote = us_expnote;

         //user domain
           lda_domain = ta_codeds2;

         //manager flag (1,0)
           lda_managr = ta_flag2;

         //night job severe error flag
           lda_severe = 0;

         //user preference for F3 window
           lda_f3_wrn = us_f3_warn;

         //user preference for cursor posistion in k3s_1010
           lda_cr1010 = us_cur1010;

         //user preference for cost/divisor change warning k3s_1040
           lda_cstwrn = us_cstwarn;

         //user preference for po arrival date
           lda_arvdat  = us_arvdate;

         //user preference for automatic rounding to buy multiple
           lda_autrnd  = us_autornd;

         //user preference for automatic freeze window
           lda_autfrz  = us_autofrz;

         //user preference for automatic freeze date
           lda_frzdat  = us_dftlfrz;

         //company supplier in product notes flag
           lda_noteky  = cm_notekey;

        ////////////////////////////////////////////// update data area *lda

        //    update local data area *lda
           out *dtaara;
        //////////////////////////////////////////////////////// end program

        //    end program
           eval      *inlr = *on;
      /end-free
