     *****************************************************************
     h copyright('(C) Copyright 1996 - 2016 King III Solutions, Inc.  +
     h Rel 5.3  2016-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h OPTION(*NODEBUGIO:*SRCSTMT)
     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5')
     h BNDDIR('K3S_BNDDIR')
      *****************************************************************
      **                                                          `
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2016 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_1510
      **   Type: ILE RPG Program
      **   Desc: Allocate extra days to Supplier Suggested Orders
      **
      *****************************************************************
      **
      **  This program can perform three functions from K3S_1500:
      **
      **     1) Allocate extra days to entire supplier order while
      **        trying to reach a discount bracket (minimum), or
      **        keeping order from going over a limit (maximum).
      **
      **     2) While in forward buy check mode, this program will
      **        simply calculate replenishment needs for products without
      **        extra days, or add extra days for products that have them.
      **
      **     3) While in alternate source mode, order replenishment amount
      **        plus extra based upon alternate source advantage, or
      **        restricted quantity amount.
      **
      **  This program can perform two functions from K3S_1020:
      **
      **     1) Allocate extra days to entire supplier order while
      **        trying to reach a discount bracket (minimum), or
      **        keeping order from going over a limit (maximum).
      **
      **     2) If rebuild option 0, then order replenishment amount
      **        plus extra days passed.
      **
      **  Parameters passed to this program will specify the supplier
      **  order being re-built.
      **
      *****************************************************************
      **
      **  Indicator usage
      **
      **      record formats
      **  11  rk_prodsoq
      **  12  rk_suplsoq
      **  13  rk_locatns
      **
      *****************************************************************
      **  Maintenance Log.
      **  Programmer. David Meyer.
      **  Date. 07/11/2014.
      **  Remarks. Converted this program to free format RPG.
      **
      **  Programmer. David Meyer
      **  Date: 06/24/2016.
      **  Remarks. Converted subroutines $_manual, $_calc_SOQ, $_usage,
      **    and $_manual_R into subprocedures K3S_Calc_Manual,
      **    K3S_Calc_SOQ, K3S_Calc_Usage, and K3S_Manual_R respectively.
      *****************************************************************

     f*_prodsoqauf   e           k disk
      * product suggested orders

     f*_productauf   e           k disk
      * products by locn, supl, suplsub, prod

     f*_suplsoqauf   e           k disk
      * supplier suggested orders

     f*_locatnsaif   e           k disk
      * locations

     f*k_prodsezaif   e           k disk
      * product seasonal profiles

     f*_tablcodaif   e           k disk                                         table codes
      * table file

     f*_suplvocauf a e           k disk                                         table codes
      * supplier variable order cycle

     f*_prodwkdauf a e           k disk                                         table codes
      * product weekly distribution

     f*_intaltreuf a e           k disk                                         table codes
      * interface records for alternate source system

      * ---------------------------------- calculate usage (days into units)
     d*copy k3s_c100

      * --------------------------------- calculate suggested order quantity
     d*copy k3s_c120

      * ------------------------------------ calculate manual min/max values
     d*copy k3s_c130

      * ---------------------------------------------- Alt source flag (1,0)
      *   Regular order = 0
      *   Alternate source order = 1
      *alt_sour        s              1  0

      * ------------------------------------------------------ Variable days

     d varydays        s              3  0 dim(10)
     d varywork        s              9  3

      * ---------------------------------------- Accumulative bracket arrays

     d reg             s             15  4 dim(10)
     d net             s             15  4 dim(10)
     d wgt             s             15  3 dim(10)
     d vol             s             15  3 dim(10)
     d pqt             s             15  4 dim(10)
     d oth             s             15  4 dim(10)
     d un7             s             15  4 dim(10)
     d un8             s             15  4 dim(10)
     d un9             s             15  4 dim(10)

      * --------------------------------------------------------- Workfields
     d minday          s              9  3
     d maxday          s              9  3
     d minunit         s              7  0
     d maxunit         s              7  0
     d mnwork          s              7  0
     d mxwork          s              7  0
     d delay           s              1  0
     d dealtest        s              1
     d final           s              1
     d extension       s             15  4
     d type            s              1  0
     d daywork         s              3  0
     d x               s              3  0
     d z               s              3  0
     d i               s              3  0
     d j               s              3  0
     d attempts        s              3  0
     d achieved        s              1
     d days            s              1
     d precision       s              3  1
     d testvalue       s             15  4
     d savsoqseq#      s                   like(pq_soqseq#)
     d soqactw1        s                   like(pq_soqact)
     d soqactw2        s                   like(pq_soqact)
     d soqactsv        s                   like(pq_soqact)
     d convpak         s                   like(pq_convpak)
     d convpak1        s                   like(pq_convpak)
     d convpak2        s                   like(pq_convpak)
     d convpak3        s                   like(pq_convpak)
     d chksoq          s                   like(so_chksoq)
     d chk6mon         s                   like(so_chk6mon)
     d chkover         s                   like(so_chkover)
     d chkback         s                   like(so_chkback)
     d least_1         s              1
     d excl_buy        s              1  0                                      Exclude buying logic
     d recalc_tst      s                   like(so_recalc)
     d vary_orcyc      s              1  0                                      Variable Order Cycle
     D Weeks           S              7  0
     D DayNbr          S              1P 0
     d average_0       s              1  0                                      Average 0 allowed
     d must_recv       s              1  0                                      Must receive 1st
     d not_recvd       s               d   inz(d'0001-01-01') datfmt(*iso)
     d mxtest          s              7
     d u1sttest        s             10
     d first12         s               d
     d first13         s               d
     d first52         s               d
     d today           s               d
     d*
     d taflag1         s              1  0
     d taflag2         s              1  0
     d prodsez_found   s               n
     d product_found   s               n
     d locn_hold       s              5
     d supl_hold       s             10
     d suplsub_hold    s             10

     d @q_maxunit      s              7  0
     d @q_minunit      s              7  0
     d @q_qtybaln      s              7  0
     d @q_minqty       s              7  0
     d @q_buymult      s              7  0
     d @q_convpak      s              7  0
     d @q_convpkp      s              3  1
     d @q_soq          s              7  0

     d  @m_altsour     s              1  0
     d  @m_maminid     s              3  0
     d  @m_mamaxid     s              3  0
     d  @m_maminiu     s              7  0
     d  @m_mamaxiu     s              7  0
     d  @m_qtybaln     s              7  0
     d  @m_chkopnt     s              1  0
     d  @m_sochk       s              5  0
     d  @m_seasonl     s             10
     d  @m_forcast     s              9  2
     d  @m_forcper     s              3  0
     d  @m_forcint     s              3  0
     d  @m_longtrm     s              3  2
     d  @m_minday      s              9  3
     d  @m_maxday      s              9  3
     d  @m_minunit     s              7  0
     d  @m_maxunit     s              7  0
     d  @m_mnwork      s              7  0
     d  @m_mxwork      s              7  0
     d  @m_str_day     s              1  0
     d  @m_str_unt     s              1  0

     d  @u_comp        s              1
     d  @u_seasonl     s             10
     d  @u_days        s             11  3
     d  @u_forcast     s              9  2
     d  @u_forcper     s              3  0
     d  @u_forcint     s              3  0
     d  @u_longtrm     s              3  2
     d  @u_dowmap      s              1  0
     d  @u_dowper1     s              5  2
     d  @u_dowper2     s              5  2
     d  @u_dowper3     s              5  2
     d  @u_dowper4     s              5  2
     d  @u_dowper5     s              5  2
     d  @u_dowper6     s              5  2
     d  @u_dowper7     s              5  2
     d  @u_sstper1     s              5  2
     d  @u_sstper2     s              5  2
     d  @u_sstper3     s              5  2
     d  @u_sstper4     s              5  2
     d  @u_sstper5     s              5  2
     d  @u_sstper6     s              5  2
     d  @u_sstper7     s              5  2
     d  @u_diff12      s              3  0
     d  @u_diff13      s              3  0
     d  @u_diff52      s              3  0
     d  @u_use         s             15  3
     d  @u_Ds_flag     s              1  0                                      Calculate results?
     d  @u_DayNbr      s              1  0                                      Today's day #
     d  @u_Ds_days     s              2  0                                      Display days
     d  @u_Ds_wdis     s              5  2 dim(12)                              Weekly Dist %
     d  @u_Ds_dlyu     s             15  4 dim(12)                              Daily usage
     d  @u_Ds_spct     s              5  2 dim(12)                              Safety Stock %
     d  @u_Ds_susg     s             15  4 dim(12)                              Safety Stock usage
     d  @u_Ds_accu     s             15  4 dim(12)                              Accumulated usage
     d  @u_Ds_prno     s              3  0 dim(12)                              Profile factor
     d  @u_Ds_fctr     s              5  2 dim(12)
     d  @u_Ds_warn     s              1  0                                      Today's day #

      * work fields used in subprocedure

      *   profile array
     d @u_ixa          ds
     d  pzfactr01                     5s 2
     d  pzfactr02                     5s 2
     d  pzfactr03                     5s 2
     d  pzfactr04                     5s 2
     d  pzfactr05                     5s 2
     d  pzfactr06                     5s 2
     d  pzfactr07                     5s 2
     d  pzfactr08                     5s 2
     d  pzfactr09                     5s 2
     d  pzfactr10                     5s 2
     d  pzfactr11                     5s 2
     d  pzfactr12                     5s 2
     d  pzfactr13                     5s 2
     d  pzfactr14                     5s 2
     d  pzfactr15                     5s 2
     d  pzfactr16                     5s 2
     d  pzfactr17                     5s 2
     d  pzfactr18                     5s 2
     d  pzfactr19                     5s 2
     d  pzfactr20                     5s 2
     d  pzfactr21                     5s 2
     d  pzfactr22                     5s 2
     d  pzfactr23                     5s 2
     d  pzfactr24                     5s 2
     d  pzfactr25                     5s 2
     d  pzfactr26                     5s 2
     d  pzfactr27                     5s 2
     d  pzfactr28                     5s 2
     d  pzfactr29                     5s 2
     d  pzfactr30                     5s 2
     d  pzfactr31                     5s 2
     d  pzfactr32                     5s 2
     d  pzfactr33                     5s 2
     d  pzfactr34                     5s 2
     d  pzfactr35                     5s 2
     d  pzfactr36                     5s 2
     d  pzfactr37                     5s 2
     d  pzfactr38                     5s 2
     d  pzfactr39                     5s 2
     d  pzfactr40                     5s 2
     d  pzfactr41                     5s 2
     d  pzfactr42                     5s 2
     d  pzfactr43                     5s 2
     d  pzfactr44                     5s 2
     d  pzfactr45                     5s 2
     d  pzfactr46                     5s 2
     d  pzfactr47                     5s 2
     d  pzfactr48                     5s 2
     d  pzfactr49                     5s 2
     d  pzfactr50                     5s 2
     d  pzfactr51                     5s 2
     d  pzfactr52                     5s 2
     d @u_ix                          5s 2 dim(52)
     d                                     overlay(@u_ixa)

     d ManMinMaxR      s              1  0                                      manual min/max rev

      *** car count logic work fields
     d CarLogic        s              1  0                                      car logic used?
     d CarCount        s              7  0                                      number cars
     d CarRemain       s              7  0                                      remainder
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            400a   inz
      * -------------------------------------------------------
     d suplsoq_rec   e ds                  ExtName(k_suplsoq)
     d product_rec   e ds                  ExtName(k_product)
     d locatns_rec   e ds                  ExtName(k_locatns)
     d prodwkd_rec   e ds                  ExtName(k_prodwkd)
     d suplvoc_rec   e ds                  ExtName(k_suplvoc)
     d prodsoq_rec   e ds                  ExtName(k_prodsoq)
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
      * parameters passed to program
     d K3S_1510        PI
     d  program                      10
     d  alt_sour                      1  0
     d  comp                          1
     d  buyr                          5
     d  locn                          5
     d  supl                         10
     d  suplsub                      10
     d  soqseq#                       5  0
     d  fbuy_check                    1  0
     d  extra_days                    5  2
     d  keepqty                       1

      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;
       //--------------------------------------------------- Prime key lists
       exsr dclpqcursor;
       exsr dclprcursor;

       //prime key lists
       locn_hold = locn;
       supl_hold = supl;
       suplsub_hold = suplsub;

       //-------------------------------------------- initialize work fields

       clear daywork;
       clear attempts;
       clear recalc_tst;
       achieved = *off;
       final    = *off;

       //------------------------------------ get Manual Min/Max information

       // save Straight Manual Min/Max Days and Units processing flags
       //      @m_str_day = 1 means use straight days (flag 1)
       //      @m_str_day = 0 means do not use straight days (flag 1)
       //      @m_str_unt = 1 means use straight units (flag 2)
       //      @m_str_unt = 0 means do not use straight units (flag 2)

       exec sql
         select ta_flag1, ta_flag2
           into :taflag1, :taflag2
           from k_tablcod
           where ta_comp = :comp and
                 ta_codetyp = 'APP' and
                 ta_codeval = 'MIN_MAX   STRAIGHT=1'
           fetch first row only;
       if SQLState = SQLStateOk;
          @m_str_day = taflag1;
          @m_str_unt = taflag2;
       else;
          @m_str_day = 0;
          @m_str_unt = 0;
       endif;

       // calculate Manual Min/Max Days and Units when PR_FORCAST = 0
       //      average_0 = 1 means to process min/max even when PR_FORCAST = 0
       //      average_0 = 0 means do not process min/max  when PR_FORCAST = 0
       average_0 = 0;
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
           where ta_comp = :comp and
                 ta_codetyp = 'APP' and
                 ta_codeval = 'MIN_MAX   AVERAGE=0 '
           fetch first row only;
       if SQLState = SQLStateOk and taflag1 = 1;
          average_0 = 1;
       endif;

       // Must receive product before suggested quantity generated,
       // added logic to test quantity on order for that product
       //      must_recv = 1 means feature is active, product must be received
       //      must_recv = 0 means this feature is not active
       must_recv = 0;
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
           where ta_comp = :comp and
                 ta_codetyp = 'APP' and
                 ta_codeval = 'K3S_1500  MUST_RECV '
           fetch first row only;
       if SQLState = SQLStateOk and taflag1 = 1;
          must_recv = 1;
       endif;

       // car count logic
       //      CarLogic = 1 means car count logic being used
       //      CarLogic = 0 means car count logic not being used
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
           where ta_comp = :comp and
                 ta_codetyp = 'APP' and
                 ta_codeval = 'K3S_1500  CAR_COUNT '
           fetch first row only;
       if SQLState = SQLStateOk and taflag1 = 1;
          CarLogic = 1;
       else;
          CarLogic = 0;
       endif;

       // Manual Min/Max sequence reversed ?
       //      ManMinMaxR = 1 means yes, calculate Days 1st, Units 2nd
       //      ManMinMaxR = 0 means no, leave it at Units 1st, Days 2nd
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
           where ta_comp = :comp and
                 ta_codetyp = 'APP' and
                 ta_codeval = 'K3S_C131  REVERSE   '
           fetch first row only;
       if SQLState = SQLStateOk and taflag1 = 1;
          ManMinMaxR = 1;
       else;
          ManMinMaxR = 0;
       endif;

       // ------------------------------------- get Exclude Buying Until flag
       // Exclude Buying Until logic to look at 'Back out' quantity?
       //      excl_buy = 1 means use 'back out' quantity in logic
       //    excl_buy = 0 means do not use 'back out' qty in logic
       exec sql
         select ta_flag1
           into :taflag1
           from k_tablcod
           where ta_comp = :comp and
                 ta_codetyp = 'APP' and
                 ta_codeval = 'EXCL_BUY  BACK_OUT  '
           fetch first row only;
       if SQLState = SQLStateOk;
          excl_buy = taflag1;
       else;
          excl_buy = 0;
       endif;

       // --------------------------------------------- alternate source order

       // if this is an alternate source order, then set the fbuy_check flag
       //    on, so that this program will not try and allocate up to a
       //    minimum, but simply buy extra days already generated earlier.
       //
       if alt_sour   = 1;
          fbuy_check = 1;
       endif;

       //---------------------------------------------------------- Location

       // get location
       exec sql
         select *
           into :locatns_rec
           from k_locatns
           where lc_comp = :comp and
                 lc_locn = :locn
           fetch first row only;
       if SQLState = SQLStateOk;

       //*** determine day of week for variable order cycle
          Weeks = %diff(lc_sysdate:d'1899-12-30':*days);
          Daynbr = %rem(Weeks:7);
          if DayNbr = 0;
             DayNbr = 7;
          endif;

       // Convert alpha field to date
          today = lc_sysdate;

       // retrieve first day of next period
       //   do this only once, for all 3 forecasting period types
          callp K3S_M160(comp:
                         today:
                         first12:
                         first13:
                         first52);

       // calculate number of days from today until first day next period
       // convert date to alpha
          u1sttest = %char(first12:*iso);
          if u1sttest <> '0001-01-01';
             @u_diff12 = %diff(first12:today:*days);
          endif;

          if u1sttest <> '0001-01-01';
             @u_diff13 = %diff(first13:today:*days);
          endif;

          u1sttest = %char(first52:*iso);
          if u1sttest <> '0001-01-01';
             @u_diff52 = %diff(first52:today:*days);
          endif;

       endif;

       // ------------------------------------------- Supplier suggested order

       // get supplier suggested order
       exec sql
         select *
           into :suplsoq_rec
           from k_suplsoq
           where so_comp = :comp and
                 so_buyr = :buyr and
                 so_locn = :locn and
                 so_supl = :supl and
                 so_suplsub = :suplsub and
                 so_soqseq# = :soqseq#
           fetch first row only;

       if SQLState = SQLStateOk;
       endif;

       // ---------------------------------------------- Varialble Order Cycle

       // --------  variable order cycle section -- begin
       // check to see if supplier has variable order cycle record
       vary_orcyc = 0;
       @u_dowmap  = 0;

       exec sql
          select *
            into :suplvoc_rec
            from k_suplvoc
            where si_comp = :comp and
                  si_locn = :locn and
                  si_supl = :supl and
                  si_suplsub = :suplsub
            fetch first row only;

       if SQLState = SQLStateOk;
          vary_orcyc = 1;
       endif;

       // --------  variable order cycle section -- end

       // -------------------------------------------------- One time routines

       //     build vary days with initial values of 0,1,2,3,4,5,6,7,8,9 days
       For i = 1 to 10;
           varydays(i) = daywork;
           daywork += 1;
       endfor;

       //     set precision
       if so_precise = 1;
          precision  = .1;
       else;
          precision  = 1.0;
       endif;

       //     for parts type of environment, allocate 10 days at a time
       if so_precise = 2;
          precision  = 10.0;
       endif;

       //     if we came from 'K3S_1210' for extra days on entire order,
       //     then set precission and rebuild option
       if program = 'K3S_1210';
          precision  = 1.0;
          so_rebldop = 0;
       endif;

       // ---------------------------------------------------------- Main line

       //     if supplier discount bracket being used, and this is not a
       //     forward buy check supplier order, then allocate days to meet
       //     supplier discount bracket.
       if (so_rebldop > 0) and
          (fbuy_check = 0);
          exsr $_allocate;
       endif;

       //     provide final answer to allocation attempt
       exsr $_answer;

       // return
       return;

       ///////////////////////////////////////////////////////////// Allocate

       // allocation of days to meet supplier brackets

       begsr $_allocate;

       //  Process through loop until either bracket acheived, or 50 attempts.
       //      Since each attempt includes 10 variations of days,
       //      all 50 attempts would be equal to 500 variations of days.
       //         If precision = 1.0, then variations are from 0 to 499  days.
       //         If precision =  .1, then variations are from 0 to 49.9 days.

       //    begin attempts
       dou (achieved = *on) or (attempts = 50);

       //    increment attempts
           attempts += 1;

       //    clear accumulation fields
           clear reg;
           clear net;
           clear wgt;
           clear vol;
           clear pqt;
           clear oth;
           clear un7;
           clear un8;
           clear un9;

           clear chksoq;
           clear chk6mon;
           clear chkover;
           clear chkback;

           exsr InzInpSrchpq;
    ‚   //initialize StmtString
    ‚       exsr IntSQLStmtpq;
    ‚   //prepare statement
    ‚       exsr PrepDynSQLStmtpq;
       //if good prep
           if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
              exsr opnpqcursor;
       // --------------------------------------------------------------------
       // loop through all products in this order
              Dow SQLState = SQLStateOk;
       //read products in order
                 exec sql
                   fetch next
                     from pqcursor
                     into :prodsoq_rec;

                 if SQLState = RowNotFound;
                   leave;
                 endif;

                 if so_altsrce = 2;
                    locn_hold = pq_cmblocn;
                    supl_hold = pq_cmbsupl;
                    suplsub_hold = pq_cmbsub;
                 endif;

                 exec sql
                   select *
                     into :product_rec
                     from k_product
                     where pr_comp = :comp and
                           pr_locn = :locn_hold and
                           pr_supl = :supl_hold and
                           pr_suplsub = :suplsub_hold and
                           pr_prod = :pq_prod
                     fetch first row only;

                 exsr $_calculat;

                 SQLState = SQLStateOk;
              enddo;

              exsr clspqcursor;
       // --------------------------------------------------------------------

       // test if supplier bracket reached
       //         achieved set on if it is reached
              exsr $_testbrkt;

       // increment next set of day variations
              if achieved = *off;
                 varydays += 10;
              endif;

           endif;
       // continue loop until achieved = on, or 50 attempts made
       enddo;

       // set failed flag to 1, failed to reach supplier bracket
       if achieved = *off and attempts = 50;
          recalc_tst = 1;
       endif;

       endsr;

       ///////////////////////////////////////////////////////////// Calculate

       // calculate suggested order quantities for 10 different day variations

       begsr $_calculat;

       // variable order cycle does exist, determine correct value
       if vary_orcyc = 1;
       //    D=Daily
          if si_rectype = 'D';
       //***       save off Weekly Distribution values
             @u_Ds_flag = 0;
             @u_DayNbr  = DayNbr;
             @u_dowmap  = si_dowmap;
             @u_dowper1 = si_dowper1;
             @u_dowper2 = si_dowper2;
             @u_dowper3 = si_dowper3;
             @u_dowper4 = si_dowper4;
             @u_dowper5 = si_dowper5;
             @u_dowper6 = si_dowper6;
             @u_dowper7 = si_dowper7;
             @u_sstper1 = si_sstper1;
             @u_sstper2 = si_sstper2;
             @u_sstper3 = si_sstper3;
             @u_sstper4 = si_sstper4;
             @u_sstper5 = si_sstper5;
             @u_sstper6 = si_sstper6;
             @u_sstper7 = si_sstper7;
       //***       check if there is a Product Weekly Distribution record

             exec sql
                select *
                  into :prodwkd_rec
                  from k_prodwkd
                  where pm_comp = :pr_comp and
                        pm_locn = :pr_locn and
                        pm_supl = :pr_supl and
                        pm_suplsub = :pr_suplsub and
                        pm_prod = :pr_prod
                  fetch first row only;

             if SQLState = SQLStateOk;
                @u_dowper1 = pm_dowper1;
                @u_dowper2 = pm_dowper2;
                @u_dowper3 = pm_dowper3;
                @u_dowper4 = pm_dowper4;
                @u_dowper5 = pm_dowper5;
                @u_dowper6 = pm_dowper6;
                @u_dowper7 = pm_dowper7;
                @u_sstper1 = pm_sstper1;
                @u_sstper2 = pm_sstper2;
                @u_sstper3 = pm_sstper3;
                @u_sstper4 = pm_sstper4;
                @u_sstper5 = pm_sstper5;
                @u_sstper6 = pm_sstper6;
                @u_sstper7 = pm_sstper7;

       //***  no Product specific safety stock, default to Supplier
                if pm_sstper1 = 0 and
                   pm_sstper2 = 0 and
                   pm_sstper3 = 0 and
                   pm_sstper4 = 0 and
                   pm_sstper5 = 0 and
                   pm_sstper6 = 0 and
                   pm_sstper7 = 0;

                   @u_sstper1 = si_sstper1;
                   @u_sstper2 = si_sstper2;
                   @u_sstper3 = si_sstper3;
                   @u_sstper4 = si_sstper4;
                   @u_sstper5 = si_sstper5;
                   @u_sstper6 = si_sstper6;
                   @u_sstper7 = si_sstper7;
                endif;

             endif;
          endif;

       endif;

       // get seasonal profile information

       //     get profile factors, if product contains seasonal profile ID

       prodsez_found = *off;
       if pq_seasonl <> *blanks;
          exec sql
            select pz_factr01, pz_factr02, pz_factr03,
                   pz_factr04, pz_factr05, pz_factr06,
                   pz_factr07, pz_factr08, pz_factr09,
                   pz_factr10, pz_factr11, pz_factr12,
                   pz_factr13, pz_factr14, pz_factr15,
                   pz_factr16, pz_factr17, pz_factr18,
                   pz_factr19, pz_factr20, pz_factr21,
                   pz_factr22, pz_factr23, pz_factr24,
                   pz_factr25, pz_factr26, pz_factr27,
                   pz_factr28, pz_factr29, pz_factr30,
                   pz_factr31, pz_factr32, pz_factr33,
                   pz_factr34, pz_factr35, pz_factr36,
                   pz_factr37, pz_factr38, pz_factr39,
                   pz_factr40, pz_factr41, pz_factr42,
                   pz_factr43, pz_factr44, pz_factr45,
                   pz_factr46, pz_factr47, pz_factr48,
                   pz_factr49, pz_factr50, pz_factr51,
                   pz_factr52
             into :pzfactr01, :pzfactr02, :pzfactr03,
                  :pzfactr04, :pzfactr05, :pzfactr06,
                  :pzfactr07, :pzfactr08, :pzfactr09,
                  :pzfactr10, :pzfactr11, :pzfactr12,
                  :pzfactr13, :pzfactr14, :pzfactr15,
                  :pzfactr16, :pzfactr17, :pzfactr18,
                  :pzfactr19, :pzfactr20, :pzfactr21,
                  :pzfactr22, :pzfactr23, :pzfactr24,
                  :pzfactr25, :pzfactr26, :pzfactr27,
                  :pzfactr28, :pzfactr29, :pzfactr30,
                  :pzfactr31, :pzfactr32, :pzfactr33,
                  :pzfactr34, :pzfactr35, :pzfactr36,
                  :pzfactr37, :pzfactr38, :pzfactr39,
                  :pzfactr40, :pzfactr41, :pzfactr42,
                  :pzfactr43, :pzfactr44, :pzfactr45,
                  :pzfactr46, :pzfactr47, :pzfactr48,
                  :pzfactr49, :pzfactr50, :pzfactr51,
                  :pzfactr52
             from k_prodsez
             where pz_comp = :comp and
                   pz_seasonl = :pq_seasonl
             fetch first row only;
             if SQLState = SQLStateOk;
                prodsez_found = *on;
             endif;
       endif;

       //   set factors to 1.00 if:

       //        1) profile not found for this product, or
       //        2) no profile for this product, and long term trend <> 1.00
       //        3) no profile for this product, and Weekly Distribution ON

       if ((pq_seasonl <> *blanks) and
            not prodsez_found)
               or
          ((pq_seasonl = *blanks) and
          (pq_longtrm <> 1.00))
               or
          ((pq_seasonl = *blanks) and
          (@u_dowmap = 1));

          pzfactr01 = 1.00;
          pzfactr02 = 1.00;
          pzfactr03 = 1.00;
          pzfactr04 = 1.00;
          pzfactr05 = 1.00;
          pzfactr06 = 1.00;
          pzfactr07 = 1.00;
          pzfactr08 = 1.00;
          pzfactr09 = 1.00;
          pzfactr10 = 1.00;
          pzfactr11 = 1.00;
          pzfactr12 = 1.00;
          pzfactr13 = 1.00;
          pzfactr14 = 1.00;
          pzfactr15 = 1.00;
          pzfactr16 = 1.00;
          pzfactr17 = 1.00;
          pzfactr18 = 1.00;
          pzfactr19 = 1.00;
          pzfactr20 = 1.00;
          pzfactr21 = 1.00;
          pzfactr22 = 1.00;
          pzfactr23 = 1.00;
          pzfactr24 = 1.00;
          pzfactr25 = 1.00;
          pzfactr26 = 1.00;
          pzfactr27 = 1.00;
          pzfactr28 = 1.00;
          pzfactr29 = 1.00;
          pzfactr30 = 1.00;
          pzfactr31 = 1.00;
          pzfactr32 = 1.00;
          pzfactr33 = 1.00;
          pzfactr34 = 1.00;
          pzfactr35 = 1.00;
          pzfactr36 = 1.00;
          pzfactr37 = 1.00;
          pzfactr38 = 1.00;
          pzfactr39 = 1.00;
          pzfactr40 = 1.00;
          pzfactr41 = 1.00;
          pzfactr42 = 1.00;
          pzfactr43 = 1.00;
          pzfactr44 = 1.00;
          pzfactr45 = 1.00;
          pzfactr46 = 1.00;
          pzfactr47 = 1.00;
          pzfactr48 = 1.00;
          pzfactr49 = 1.00;
          pzfactr50 = 1.00;
          pzfactr51 = 1.00;
          pzfactr52 = 1.00;
       endif;

       @u_comp    = pq_comp;
       @u_seasonl = pq_seasonl;
       @u_forcast = pq_forcast;
       @u_forcper = pq_forcper;
       @u_forcint = pq_forcint;
       //@u_sysdate = %char(lc_sysdate:*iso);
       @u_longtrm = pq_longtrm;

       //   set i to 0
       i = 0;

       //   do 10 different day variations for product
       dou i = 10;

       //   increment by 1
          i += 1;

        //  set varywork field (days work field from 0-49.9, or 0-499)
          varywork = precision * varydays(i);

       //   if this is a forward buy check process, or if buyer is trying to
       //      add extra days to order interactively, then set j = 1, so that
       //      varydays = 0
          if fbuy_check = 1 OR
             program = 'K3S_1210' OR
             (program = 'K3S_1020' AND so_rebldop = 0);

             j = 1;
          endif;

       //   if this is final time, set varywork from answer discovered

       //      assumption is that criteria is minimum

          if final = *on;

             varywork = precision * varydays(j);

       //      if criteria is maximum, then we will need to back off
       //           one iteration of (j) value. the trick is that
       //           varydays built 10 days at a time, so you must test
       //           if we are at very first iteration ever. Otherwise,
       //           just back up one iteration.

             if so_cur1typ = 1 AND
                so_cur1lev > 0 AND
                alt_sour   = 0 AND
                fbuy_check = 0;

       //           already over maximum before allocating, can't go any
       //           lower, so stop
                if j = 1 AND varydays(j) = 0;
                   varywork = 0;
                else;
       //           back off one day
                   varywork = precision * (varydays(j) - 1);
                endif;

             endif;

          endif;

          if keepqty = '0';
             pq_soqovrd = 0;
          endif;
       //   clear product suggested order quantity, if no manual over-ride,
       //         and this is not a restricted quantity product
          if pq_soqovrd = 0 and pq_restflg = 0;
             clear pq_soqact;
          endif;

          clear pq_opointu;
          clear pq_opointd;
          clear pq_oruptou;
          clear pq_oruptod;
          clear pq_soqpcnt;

       //   select the category of product to work with
          select;

       //  if suggested order quantity has been manually over-ridden, use it
             when pq_soqovrd = 1;

       //  new and manual products, do nothing
             when (pq_sysstat = 'N') or (pq_usrstat = 'M');
                i = 10;

       //  products with no cost, do nothing
             when pq_costeac = 0;
       //***********       eval      i = 10

       //  No cost products start ----------------
       // product must have cost for calculations
       //    only exception would be if buyer uses Manual Min/Max in UNITS
       //                         or if buyer uses Manual Min/Max in DAYS

       //  only execute if a manual minimum UNITS is used
       //              or if a manual minimum DAYS  is used
                  if pq_maminiu > 0 OR
                     pq_maminid > 0;

                     @m_seasonl = pq_seasonl;
                     @m_forcast = pq_forcast;
                     @m_forcper = pq_forcper;
                     @m_forcint = pq_forcint;
                     @m_longtrm = pq_longtrm;

                     @m_altsour = alt_sour;
                     @m_maminiu = pq_maminiu;
                     @m_mamaxiu = pq_mamaxiu;
                     @m_maminid = pq_maminid;
                     @m_mamaxid = pq_mamaxid;
                     @m_qtybaln = pq_qtybaln;
                     @m_chkopnt = pq_chkopnt;
                     @m_sochk   = so_chkopnt;

                     @m_minday  = 0;
                     @m_maxday  = 0;
                     @m_minunit = pq_maminiu;
                     @m_maxunit = pq_mamaxiu;
                     @m_mnwork  = 0;
                     @m_mxwork  = 0;

                     if pq_maminid > 0;
                        @m_maminid = 0;
                        @m_mamaxid = 0;
                        eval(h)   @m_maminiu = pq_maminid *
                                               ((pq_forcast * pq_forcint)/364);
                        eval(h)   @m_mamaxiu = pq_mamaxid *
                                               ((pq_forcast * pq_forcint)/364);
                     endif;

       // call subroutine to calculate manual minimum or maximum values
                     callp K3S_Calc_Manual(@m_altsour:
                                           @m_maminid:
                                           @m_mamaxid:
                                           @m_maminiu:
                                           @m_mamaxiu:
                                           @m_qtybaln:
                                           @m_chkopnt:
                                           @m_sochk:
                                           @m_seasonl:
                                           @m_forcast:
                                           @m_forcper:
                                           @m_forcint:
                                           @m_longtrm:
                                           @m_minday:
                                           @m_maxday:
                                           @m_minunit:
                                           @m_maxunit:
                                           @m_mnwork:
                                           @m_mxwork:
                                           @m_str_day:
                                           @m_str_unt:
                                           @u_comp:
                                           @u_days:
                                           @u_dowmap:
                                           @u_dowper1:
                                           @u_dowper2:
                                           @u_dowper3:
                                           @u_dowper4:
                                           @u_dowper5:
                                           @u_dowper6:
                                           @u_dowper7:
                                           @u_sstper1:
                                           @u_sstper2:
                                           @u_sstper3:
                                           @u_sstper4:
                                           @u_sstper5:
                                           @u_sstper6:
                                           @u_sstper7:
                                           @u_diff12:
                                           @u_diff13:
                                           @u_diff52:
                                           @u_use:
                                           @u_Ds_flag:
                                           @u_DayNbr:
                                           @u_ix:
                                           @u_DS_Days:
                                           @u_DS_wdis:
                                           @u_DS_dlyu:
                                           @u_DS_spct:
                                           @u_DS_susg:
                                           @u_DS_accu:
                                           @u_DS_prno:
                                           @u_DS_fctr:
                                           @u_DS_warn);
       //            exsr $_manual;

                     minday     = @m_minday;
                     maxday     = @m_maxday;
                     minunit    = @m_minunit;
                     maxunit    = @m_maxunit;
                     mnwork     = @m_mnwork;
                     mxwork     = @m_mxwork;
                     pq_chkopnt = @m_chkopnt;
                     so_chkopnt = @m_sochk;

       //  if maximum units less than minimum units, make equal
                     if maxunit < minunit;
                        maxunit = minunit;
                        mxwork  = mnwork;
                     endif;

       //  save data into fields for later update
                     pq_opointu = minunit;
                     pq_oruptou = maxunit;

                     clear pq_soqact;

       // calculate order quantity if balance falls below order point
                     if pq_qtybaln < pq_opointu;

                        @q_maxunit = pq_oruptou;
                        @q_minunit = pq_opointu;
                        @q_qtybaln = pq_qtybaln;
                        @q_minqty  = pq_minqty;
                        @q_buymult = pq_buymult;
                        @q_convpak = pq_convpak;
                        @q_convpkp = pq_convpkp;

                        callp K3S_Calc_SOQ(@q_maxunit:
                                           @q_minunit:
                                           @q_qtybaln:
                                           @q_minqty:
                                           @q_buymult:
                                           @q_convpak:
                                           @q_convpkp:
                                           @q_soq);
       //               exsr $_calc_SOQ;

                        pq_soqact = @q_soq;
                     endif;

                  endif;
       //  No cost products end ------------------

       //  new and manual products, do nothing      <move up before costeac=0>
       //*********when      (pq_sysstat = 'N') or (pq_usrstat = 'M')
       //*********eval      i = 10

       //discontinued products, or forecast = 0 with negative balance
             when (pq_sysstat = 'D') or ((pq_forcast = 0)
                                     and (pq_qtybaln < 0));

       //     if discontinued product, then flag must be on that allows
       //          purchase of discontinued products, OR
       //          this is not a discontinued product, so continue
                 if (pq_sysstat = 'D' AND lc_purdisc = 1) OR
                     pq_sysstat <> 'D';

       //     if negative balance, purchase it
                     if pq_qtybaln < 0;
                        pq_soqact  = pq_qtybaln * -1;

       //          round to buymult
                        if (pq_sysstat = 'D' AND lc_rnddisc > 0) OR
                            pq_sysstat <> 'D';

                            soqactw1 = pq_soqact / pq_buymult;
                            soqactw2 = %rem(pq_soqact:pq_buymult);

                            if soqactw1 > 0;
                               least_1  = *on;
                            else;
                               least_1  = *off;
                            endif;

                            if soqactw2 > 0;
                               soqactw1 += 1;

                               if least_1 = *on  AND  lc_rnddisc = 2  AND
                                  pq_sysstat = 'D';

                                  eval(h)soqactw1 = pq_soqact /
                                                    pq_buymult;
                               endif;

                               pq_soqact = pq_buymult * soqactw1;
                            endif;

       //   purchase minimum quantity test
                            if pq_soqact < pq_minqty;
                               pq_soqact = pq_minqty;
                            endif;

                        endif;
                     endif;
                 endif;

       //  products with no forecast, and quantity >= 0, do nothing
       //       unless we allow it
             when (pq_forcast = 0) and (pq_qtybaln >= 0)
                                   and (average_0   = 0);
                i = 10;

       //  this section includes all other products not selected above,
       //       which would be all normal products where the soq was not
       //       manually over-riden.
             other;

       //  determine order point in days and units

       //      order point level in days
                 minday = varywork + pq_sstimef +
                                     pq_leadtm  +
                                     so_add_day +
                                     pq_add_day +
                                     pq_fbxdays;

       //      for rebuild level 0, and buyer trying to add extra days

       //      interactively, then add extra days
                 if (program = 'K3S_1020' AND so_rebldop = 0);
                    minday = minday + so_orcycle + extra_days;
                 endif;

                 if program = 'K3S_1210';
                    minday = minday + so_orcycle + pq_selfbxd;
                 else;
                    pq_selfbxd = 0;
                 endif;

       //      for alternate source orders, use regular source order cycle
                 if so_altsrce = 1;
                    so_orcycle = pq_orcycle;
                 endif;

       //      include order cycle for forward buy check phase
                 if fbuy_check = 1;
                    minday = minday + so_orcycle;
                 endif;

                 clear @u_use;
                 @u_days = minday;

       //      order point level in units
                 callp K3S_Calc_Usage(@u_comp:
                                      @u_seasonl:
                                      @u_days:
                                      @u_forcast:
                                      @u_forcper:
                                      @u_forcint:
                                      @u_longtrm:
                                      @u_dowmap:
                                      @u_dowper1:
                                      @u_dowper2:
                                      @u_dowper3:
                                      @u_dowper4:
                                      @u_dowper5:
                                      @u_dowper6:
                                      @u_dowper7:
                                      @u_sstper1:
                                      @u_sstper2:
                                      @u_sstper3:
                                      @u_sstper4:
                                      @u_sstper5:
                                      @u_sstper6:
                                      @u_sstper7:
                                      @u_diff12:
                                      @u_diff13:
                                      @u_diff52:
                                      @u_use:
                                      @u_Ds_flag:
                                      @u_DayNbr:
                                      @u_ix:
                                      @u_Ds_days:
                                      @u_Ds_wdis:
                                      @u_Ds_dlyu:
                                      @u_Ds_spct:
                                      @u_Ds_susg:
                                      @u_Ds_accu:
                                      @u_Ds_prno:
                                      @u_Ds_fctr:
                                      @u_Ds_warn);
       //              exsr $_usage;

                 eval(h) minunit = @u_use;

       //     if usage exists, then unit must be at least 1
                 if (minunit = 0) and (@u_use > 0);
                    minunit = 1;
                 endif;

       //      order up to level in days
                 maxday =  pq_sstimef +
                           pq_leadtm  +
                           pq_intrval;

       // test minday and maxday for large values
                 if minday > 999;
                    minday = 999;
                 endif;
                 if maxday > 999;
                    maxday = 999;
                 endif;

       // -------------------------------------- building max units  start

       //     performing F16= from K3S_1020 with rebuild option 0
                 if program = 'K3S_1020' AND so_rebldop = 0;

                    clear @u_use;
                    @u_days = maxday;

       //      order up to level in units

                    callp K3S_Calc_Usage(@u_comp:
                                         @u_seasonl:
                                         @u_days:
                                         @u_forcast:
                                         @u_forcper:
                                         @u_forcint:
                                         @u_longtrm:
                                         @u_dowmap:
                                         @u_dowper1:
                                         @u_dowper2:
                                         @u_dowper3:
                                         @u_dowper4:
                                         @u_dowper5:
                                         @u_dowper6:
                                         @u_dowper7:
                                         @u_sstper1:
                                         @u_sstper2:
                                         @u_sstper3:
                                         @u_sstper4:
                                         @u_sstper5:
                                         @u_sstper6:
                                         @u_sstper7:
                                         @u_diff12:
                                         @u_diff13:
                                         @u_diff52:
                                         @u_use:
                                         @u_Ds_flag:
                                         @u_DayNbr:
                                         @u_ix:
                                         @u_Ds_days:
                                         @u_Ds_wdis:
                                         @u_Ds_dlyu:
                                         @u_Ds_spct:
                                         @u_Ds_susg:
                                         @u_Ds_accu:
                                         @u_Ds_prno:
                                         @u_Ds_fctr:
                                         @u_Ds_warn);
       //           exsr $_usage;

                    eval(h) maxunit = @u_use;

       //     if usage exists, then unit must be at least 1
                    if (maxunit = 0) and (@u_use > 0);
                       maxunit = 1;
                    endif;

       //     all other situations
                 else;

       //  if order up to level <= order point in days, OR
       //  we are in de-allocation mode (varywork less than order cycle),
       //  and we are not in Forward Buy check mode,
       //    then make order point = order up to level
                    if ((maxday <= minday) OR
                        (varywork < so_orcycle)) and
                        (fbuy_check = 0);

                        maxday = minday;
                        maxunit = minunit;

       //     calculate order up to level in units
                    else;

                        clear @u_use;
                        @u_days = maxday;

       //      order up to level in units

                        callp K3S_Calc_Usage(@u_comp:
                                             @u_seasonl:
                                             @u_days:
                                             @u_forcast:
                                             @u_forcper:
                                             @u_forcint:
                                             @u_longtrm:
                                             @u_dowmap:
                                             @u_dowper1:
                                             @u_dowper2:
                                             @u_dowper3:
                                             @u_dowper4:
                                             @u_dowper5:
                                             @u_dowper6:
                                             @u_dowper7:
                                             @u_sstper1:
                                             @u_sstper2:
                                             @u_sstper3:
                                             @u_sstper4:
                                             @u_sstper5:
                                             @u_sstper6:
                                             @u_sstper7:
                                             @u_diff12:
                                             @u_diff13:
                                             @u_diff52:
                                             @u_use:
                                             @u_Ds_flag:
                                             @u_DayNbr:
                                             @u_ix:
                                             @u_Ds_days:
                                             @u_Ds_wdis:
                                             @u_Ds_dlyu:
                                             @u_Ds_spct:
                                             @u_Ds_susg:
                                             @u_Ds_accu:
                                             @u_Ds_prno:
                                             @u_Ds_fctr:
                                             @u_Ds_warn);
       //               exsr $_usage;

                        eval(h) maxunit = @u_use;

       //     if usage exists, then unit must be at least 1
                        if (maxunit = 0) and (@u_use > 0);
                           maxunit = 1;
                        endif;

                    endif;

                 endif;

       // -------------------------------------- building max units  end

       // set min and max days into work fields
                 eval(h) mnwork = minday;
                 eval(h) mxwork = maxday;

       // ------------------------ logic for manual minimum and maximum values

       //    only execute if a manual minimum or maximum exists, AND
       //         product has a forecast
                 if (((pq_maminiu > 0) or (pq_maminid > 0) or
                      (pq_mamaxiu > 0) or (pq_mamaxid > 0))
                       AND       pr_forcast > 0)
                            OR
       //    only execute if a manual minimum or maximum exists, AND
       //         product does not have a forecast, but we allow it
                    (((pq_maminiu > 0) or (pq_maminid > 0) or
                      (pq_mamaxiu > 0) or (pq_mamaxid > 0))
                       AND       pr_forcast = 0
                       AND       average_0  = 1);

                    @m_seasonl = pq_seasonl;
                    @m_forcast = pq_forcast;
                    @m_forcper = pq_forcper;
                    @m_forcint = pq_forcint;
                    @m_longtrm = pq_longtrm;

                    @m_altsour = alt_sour;
                    @m_maminiu = pq_maminiu;
                    @m_mamaxiu = pq_mamaxiu;
                    @m_maminid = pq_maminid;
                    @m_mamaxid = pq_mamaxid;
                    @m_qtybaln = pq_qtybaln;
                    @m_chkopnt = pq_chkopnt;
                    @m_sochk   = so_chkopnt;

                    @m_minday  = minday;
                    @m_maxday  = maxday;
                    @m_minunit = minunit;
                    @m_maxunit = maxunit;
                    @m_mnwork  = mnwork;
                    @m_mxwork  = mxwork;

       // call subroutine to calculate manual minimum or maximum values

                    if ManMinMaxR = 0;
       // sequence will be Units 1st, Days 2nd
                       callp K3S_Calc_Manual(@m_altsour:
                                             @m_maminid:
                                             @m_mamaxid:
                                             @m_maminiu:
                                             @m_mamaxiu:
                                             @m_qtybaln:
                                             @m_chkopnt:
                                             @m_sochk:
                                             @m_seasonl:
                                             @m_forcast:
                                             @m_forcper:
                                             @m_forcint:
                                             @m_longtrm:
                                             @m_minday:
                                             @m_maxday:
                                             @m_minunit:
                                             @m_maxunit:
                                             @m_mnwork:
                                             @m_mxwork:
                                             @m_str_day:
                                             @m_str_unt:
                                             @u_comp:
                                             @u_days:
                                             @u_dowmap:
                                             @u_dowper1:
                                             @u_dowper2:
                                             @u_dowper3:
                                             @u_dowper4:
                                             @u_dowper5:
                                             @u_dowper6:
                                             @u_dowper7:
                                             @u_sstper1:
                                             @u_sstper2:
                                             @u_sstper3:
                                             @u_sstper4:
                                             @u_sstper5:
                                             @u_sstper6:
                                             @u_sstper7:
                                             @u_diff12:
                                             @u_diff13:
                                             @u_diff52:
                                             @u_use:
                                             @u_Ds_flag:
                                             @u_DayNbr:
                                             @u_ix:
                                             @u_DS_Days:
                                             @u_DS_wdis:
                                             @u_DS_dlyu:
                                             @u_DS_spct:
                                             @u_DS_susg:
                                             @u_DS_accu:
                                             @u_DS_prno:
                                             @u_DS_fctr:
                                             @u_DS_warn);
       //           exsr $_manual;
                    else;
       // sequence will be Days 1st, Units 2nd
                       callp K3S_Calc_Manual_R(@m_altsour:
                                               @m_maminid:
                                               @m_mamaxid:
                                               @m_maminiu:
                                               @m_mamaxiu:
                                               @m_qtybaln:
                                               @m_chkopnt:
                                               @m_sochk:
                                               @m_seasonl:
                                               @m_forcast:
                                               @m_forcper:
                                               @m_forcint:
                                               @m_longtrm:
                                               @m_minday:
                                               @m_maxday:
                                               @m_minunit:
                                               @m_maxunit:
                                               @m_mnwork:
                                               @m_mxwork:
                                               @m_str_day:
                                               @m_str_unt:
                                               @u_comp:
                                               @u_days:
                                               @u_dowmap:
                                               @u_dowper1:
                                               @u_dowper2:
                                               @u_dowper3:
                                               @u_dowper4:
                                               @u_dowper5:
                                               @u_dowper6:
                                               @u_dowper7:
                                               @u_sstper1:
                                               @u_sstper2:
                                               @u_sstper3:
                                               @u_sstper4:
                                               @u_sstper5:
                                               @u_sstper6:
                                               @u_sstper7:
                                               @u_diff12:
                                               @u_diff13:
                                               @u_diff52:
                                               @u_use:
                                               @u_Ds_flag:
                                               @u_DayNbr:
                                               @u_ix:
                                               @u_DS_Days:
                                               @u_DS_wdis:
                                               @u_DS_dlyu:
                                               @u_DS_spct:
                                               @u_DS_susg:
                                               @u_DS_accu:
                                               @u_DS_prno:
                                               @u_DS_fctr:
                                               @u_DS_warn);
                    endif;

                    minday     = @m_minday;
                    maxday     = @m_maxday;
                    minunit    = @m_minunit;
                    maxunit    = @m_maxunit;
                    mnwork     = @m_mnwork;
                    mxwork     = @m_mxwork;
                    pq_chkopnt = @m_chkopnt;
                    so_chkopnt = @m_sochk;

                 endif;

       // --------------------------------------------------------------------

       //    all products
                 if maxunit < minunit;
                    maxunit = minunit;
                    mxwork  = mnwork;
                 endif;

                 pq_opointu = minunit;
                 pq_oruptou = maxunit;

                 pq_opointd = mnwork;

       //  filter maximum order up to days to 999
                 pq_oruptox = mxwork;
                 if mxwork > 999;
                    mxtest = %editc(mxwork:'X');
                    pq_oruptod = %dec(%subst(mxtest:5:3):3:0);
                 else;
                    pq_oruptod = mxwork;
                 endif;

       // calculate order quantity if balance falls below order point
       // and this is not a restricted product
                 if pq_qtybaln < pq_opointu and
                    pq_restflg = 0;

       // car count logic
                    if CarLogic     = 1 and
                       pr_contflg   = 1 and
                       pr_maminiu   > 1 and
                       pq_oruptou   > pr_maminiu;

                       CarCount     = %div(pq_oruptou:pr_maminiu);
                       CarRemain    = %rem(pq_oruptou:pr_maminiu);
                       if CarRemain > 0;
                          CarCount  += 1;
                       endif;
                       pq_oruptou   = CarCount * pr_maminiu;
                    endif;

       // call subroutine to calculate suggested order quantity
                    clear pq_soqact;

                    @q_maxunit = pq_oruptou;
                    @q_minunit = pq_opointu;
                    @q_qtybaln = pq_qtybaln;
                    @q_minqty  = pq_minqty;
                    @q_buymult = pq_buymult;
                    @q_convpak = pq_convpak;
                    @q_convpkp = pq_convpkp;

                    callp K3S_Calc_SOQ(@q_maxunit:
                                       @q_minunit:
                                       @q_qtybaln:
                                       @q_minqty:
                                       @q_buymult:
                                       @q_convpak:
                                       @q_convpkp:
                                       @q_soq);
       //           exsr $_calc_SOQ;

                    pq_soqact = @q_soq;

                 endif;

          endsl;

       // --------------------------------------------------- alternate source
       //
       // if this is an alternate source order, and
       //    product is not restircted: pq_restflg=0, and
       //    product is not low price:  pq_fbxdays=0, then
       //    clear soq
       //
          if alt_sour   = 1 and
             pq_restflg = 0 and
             pq_fbxdays = 0;

             clear pq_soqact;
          endif;

       // if this is an alternate source order, and
       //    product has suggested quantity to order,
       //    then determine if line extension meets minimum requirement.
       //    if not, log such condition with LCV of '12'
       //
          if alt_sour   = 1 and
             pq_soqact  > 0 and
             pq_soqact * pq_costeac < lc_altmin$;

             clear pq_soqact;

             exec sql
               update k_intaltr
                 set ia_loadcod = 12
                 where ia_comp = :pr_comp and
                       ia_locn = :pr_locn and
                       ia_suplalt = :pr_supl and
                       ia_suplals = :pr_suplsub and
                       ia_prod = :pr_prod;

          endif;

       // --------------------------------------------------- Must receive 1st
       // soq exists for product, BUT
       //    customer does not want products suggested prior to receiving
       //    unless nothing has yet been ordered before
          if pq_soqact  > 0 and
             must_recv  = 1 and
             pr_lstrcvd = not_recvd and
             pr_qtyoord > 0;

             clear pq_soqact;
          endif;

       // ----------------------------------------------- Exclude buying until

       // soq exists for product, BUT
       //     we want to Exclude Buying Until a specific date in the future,
       //     (for auto parts, as long as no back orders exist)
          if pq_soqact > 0;
             if pr_excuntl > lc_sysdate;
                if excl_buy = 1;
                   if pr_qtyback = 0;
                      clear pq_soqact;
                   endif;
                else;
                   if keepqty = '0' or
                      (keepqty = '1' and pq_soqovrd = 0);
                      clear pq_soqact;
                   endif;
                endif;
             endif;
          endif;

       // -------------------------------------------------- accumulate totals

       // accumulate values if suggested order quantity exists

       // 4 checks must be re-determined
          clear pq_chksoq;
          clear pq_chk6mon;
          clear pq_chkover;
          clear pq_chkback;

          if pq_soqact > 0;

       //   dollars regular
             if (reg(i) + (pq_soqact * pr_costeac)) <
                                      999999999;
                reg(i) = reg(i) +
       //**************(pq_soqact * pq_costeac)
                      (pq_soqact * pr_costeac);
  06         else;
  06            reg(i) = 999999999;
E06€          endif;

       //   dollars net
             if (net(i) + (pq_soqact * pq_costeac)) <
                                      999999999;
                net(i) = net(i) +
                         (pq_soqact * pq_costeac);
  06         else;
  06            net(i) = 999999999;
E06€          endif;

       //   weight
             if pq_weightd > 0;
                if (wgt(i) + (pq_soqact *
                   (pq_weight / pq_weightd))) <
                                999999999;
                   wgt(i) = wgt(i) + (pq_soqact *
                            (pq_weight / pq_weightd));
  06            else;
  06               wgt(i) = 999999999;
                endif;
             endif;

       //   volume
             if pq_volumed > 0;
                if (vol(i) + (pq_soqact *
                   (pq_volume / pq_volumed))) <
                                999999999;
                   vol(i) = vol(i) + (pq_soqact *
                            (pq_volume / pq_volumed));
  06            else;
  06               vol(i) = 999999999;
                endif;
             endif;

       //   purchase increment
             if pq_purincr > 0;
                if (pqt(i) +
                    (pq_soqact / pq_purincr)) <
                                 999999999;
                    pqt(i) = pqt(i) +
                            (pq_soqact / pq_purincr);
  06            else;
  06               pqt(i) = 999999999;
                endif;
             endif;

       //   other
             if (oth(i) +
                 (pq_soqact * pq_disothr)) <
                              999999999;
                oth(i) = oth(i) +
                         (pq_soqact * pq_disothr);
  06         else;
  06            oth(i) = 999999999;
             endif;

       //   unit 7
             if (un7(i) +
                (pq_soqact * pr_disunt7)) <
                             999999999;
                un7(i) = un7(i) +
                         (pq_soqact * pr_disunt7);
  06         else;
  06            un7(i) = 999999999;
             endif;

       //   unit 8
             if (un8(i) +
                 (pq_soqact * pr_disunt8)) <
                              999999999;
                 un8(i) = un8(i) +
                          (pq_soqact * pr_disunt8);
  06         else;
  06             un8(i) = 999999999;
             endif;

       //   unit 9
             if (un9(i) +
                 (pq_soqact * pr_disunt9)) <
                              999999999;
                 un9(i) = un9(i) +
                          (pq_soqact * pr_disunt9);
  06         else;
  06             un9(i) = 999999999;
             endif;

       //   save line extension

             if pq_soqact * pq_costeac < 9999999;
                pq_dolrext = pq_soqact * pq_costeac;
             else;
                pq_dolrext = 9999999;
             endif;

       // -------------------------------------------------- save original soq
       // save original suggested order quantity, if we got here from
       // order build process, as opposed to interactive rebuilding, and
       // this is the final pass.
       //*****  this logic was too early and not all products would reach it
       //*****  b/c of the 'IF PQ_SOQACT > 0' line on 1188
       //****               if        program = 'K3S_1500' AND final = *on
       //****               eval      pq_soqsrvc = pq_soqact
       //****               eval      pr_soqnite = pq_soqact
       //****               endif

       //****               if        program = 'K3S_1020' AND
       //****                         final = *on AND
       //****                         pq_soqovrd = 0
       //****               eval      pq_soqsrvc = pq_soqact
       //****               endif

       //--------------------------------------------------- Product checks

       // soq exists for product
             pq_chksoq = 1;
             chksoq += 1;

       // buying more than 6 months check
             if pq_soqact > (pq_forcast * pq_forcint * .5);
                pq_chk6mon = 1;
                chk6mon += 1;
             endif;

       // back order for product, and suggested order exists
             if (pq_qtyback > 0) and (pq_soqact > 0);
                pq_chkback = 1;
                chkback += 1;
             endif;

       // overstocked check
             if (pq_soqact > 0) and (pq_overflg = 1)
                                and (pq_overunt = 0);
                pq_chkover = 1;
                chkover += 1;
             endif;

          endif;

       //    only process loop once at final time
          if  final = *on;
              i = 10;
          endif;

       enddo;

       // -------------------------------------------------- save original soq
       // save original suggested order quantity, if we got here from
       // order build process, as opposed to interactive rebuilding, and
       // this is the final pass.
       if program = 'K3S_1500' AND final = *on;
          pq_soqsrvc = pq_soqact;
          pr_soqnite = pq_soqact;
       endif;

       if program = 'K3S_1020' AND
          final = *on AND
          pq_soqovrd = 0;

          pq_soqsrvc = pq_soqact;
       endif;

       endsr;

       /////////////////////////////////////////////////////// Test brackets

       begsr $_testbrkt;

       j = 0;

       dou (achieved = *on) or (j=10);

          j += 1;

          exsr $_getvalue;

       //    for minimum criteria, stop when value is met
          if so_cur1typ = 0;
             if testvalue >= so_cur1val;
                achieved = *on;
             endif;

          else;

       //    for maximum criteria, go past level, and then final will
       //    back up to last valid value
             if testvalue > so_cur1val;
                achieved = *on;
             endif;

          endif;

       enddo;

       endsr;

       /////////////////////////////////////////////////////////// Get value

       begsr $_getvalue;

       select;

          when so_cur1unt = 1;
               testvalue  = reg(j);

          when so_cur1unt = 2;
               testvalue  = net(j);

          when so_cur1unt = 3;
               testvalue  = wgt(j);

          when so_cur1unt = 4;
               testvalue  = vol(j);

          when so_cur1unt = 5;
               testvalue  = pqt(j);

          when so_cur1unt = 6;
              testvalue  = oth(j);

          when so_cur1unt = 7;
               testvalue  = un7(j);

          when so_cur1unt = 8;
               testvalue  = un8(j);

          when so_cur1unt = 9;
               testvalue  = un9(j);

       endsl;

       endsr;

       ////////////////////////////////////////////////////////////// Answer

       begsr $_answer;


       //  this field used to control processes in subroutine $_calculat
       final = *on;

       //    if order reaches bracket amount, or this is forward buy check,
       //        or extra days being added directly from k3s_1020,
       //        or extra days being added directly from k3s_1210,
       //        then process supplier order
       if (achieved = *on) OR
          (fbuy_check = 1) OR
          (program = 'K3S_1210') OR
          (program = 'K3S_1020' AND so_rebldop = 0);

       //    clear accumulation fields
          clear reg;
          clear net;
          clear wgt;
          clear vol;
          clear pqt;
          clear oth;
          clear un7;
          clear un8;
          clear un9;

          clear chksoq;
          clear chk6mon;
          clear chkover;
          clear chkback;

          locn_hold = locn;
          supl_hold = supl;
          suplsub_hold = suplsub;

          exsr InzInpSrchpq;
    ‚   //initialize StmtString
    ‚     exsr IntSQLStmtpq;
    ‚   //prepare statement
    ‚     exsr PrepDynSQLStmtpq;
       //if good prep
          if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
             exsr opnpqcursor;
       // start with first product for this supplier suggested order

       // --------------------------------------------------------------------
       // loop through all products in this order

             Dow SQLState = SQLStateOk;
       //read products in order
                exec sql
                 fetch next
                   from pqcursor
                   into :prodsoq_rec;

                if SQLState = RowNotFound;
                   leave;
                endif;

       // read product file information

                if so_altsrce = 2;
                   locn_hold = pq_cmblocn;
                   supl_hold = pq_cmbsupl;
                   suplsub_hold = pq_cmbsub;
                endif;

                exsr InzInpSrchpr;
    ‚   //initialize StmtString
    ‚           exsr IntSQLStmtpr;
    ‚   //prepare PrepDynSQLStmtprPrepDynSQLStmtprstatement
    ‚           exsr PrepDynSQLStmtpr;
       //if good prep
                product_found = *off;
                if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                   exsr opnprcursor;
                   if SQLState = SQLStateOk;
       //read products in order
                      exec sql
                        fetch next
                          from prcursor
                          into :product_rec;

                      if SQLState = SQLStateOk;
                         product_found = *on;
                      endif;
                   endif;
                endif;
       // calculate suggested order quantities for 10 different day variations
                exsr $_calculat;

                exec sql
                   update k_prodsoq
                     set pq_soqovrd = :pq_soqovrd,
                         pq_soqact  = :pq_soqact,
                         pq_opointu = :pq_opointu,
                         pq_opointd = :pq_opointd,
                         pq_oruptou = :pq_oruptou,
                         pq_oruptod = :pq_oruptod,
                         pq_oruptox = :pq_oruptox,
                         pq_soqsrvc = :pq_soqsrvc,
                         pq_dolrext = :pq_dolrext,
                         pq_soqpcnt = :pq_soqpcnt,
                         pq_chkopnt = :pq_chkopnt,
                         pq_selfbxd = :pq_selfbxd,
                         pq_chksoq  = :pq_chksoq,
                         pq_chk6mon = :pq_chk6mon,
                         pq_chkover = :pq_chkover,
                         pq_chkback = :pq_chkback
                     where current of pqcursor;

                if product_found;
                   exec sql
                     update k_product
                       set pr_soqnite = :pr_soqnite
                       where current of prcursor;
                endif;
                exsr clsprcursor;

                SQLstate = SQLStateOk;
             enddo;
             exsr clspqcursor;
       // --------------------------------------------------------------------

             exsr dclsocursor;
             exsr opnsocursor;

       //  get supplier suggested order
             exec sql
              fetch next
                from socursor
                into :suplsoq_rec;

             if SQLState = SQLStateOk;

       //  save accumulated values
                so_actureg = reg(1);
                so_actunet = net(1);
                so_actuwgt = wgt(1);
                so_actuvol = vol(1);
                so_actupqt = pqt(1);
                so_actuoth = oth(1);
                so_actuun7 = un7(1);
                so_actuun8 = un8(1);
                so_actuun9 = un9(1);

       //  for alternate source orders, set replenishment total to actual
                if alt_sour = 1;
                   so_replreg = reg(1);
                   so_replnet = net(1);
                   so_replwgt = wgt(1);
                   so_replvol = vol(1);
                   so_replpqt = pqt(1);
                   so_reploth = oth(1);
                   so_replun7 = un7(1);
                   so_replun8 = un8(1);
                   so_replun9 = un9(1);
                endif;

       //  for alternate source orders, add 'savings' to 'Reg cost'
                if alt_sour = 1;
                   so_replreg += so_reploth;
                   so_actureg += so_actuoth;
                endif;

                so_chksoq  = chksoq;
                so_chk6mon = chk6mon;
                so_chkover = chkover;
                so_chkback = chkback;

       //  calculate extra days added to order
                so_jointex = varywork - so_orcycle;

       //      for rebuild level 0, and
       //      buyer trying to add extra days interactively, add extra days
                if program = 'K3S_1020' AND so_rebldop = 0;
                   so_jointex = extra_days;
                endif;

       //     if we came from 'K3S_1210' for extra days on entire order,
       //     then set extra days and rebuild option
                if program = 'K3S_1210';
                   so_jointex = extra_days;
                   so_rebldop = 0;
                endif;

       //  mark suggested order as forward buy type
       //       but not when using the auto po system
                if fbuy_check = 1 AND so_autopo = 0;
                   so_soqtype = 'FB';
                   so_jointex = 0;
                endif;

       //  force auto PO orders, with forward bought products, and have
       //       negative extra days, to contain 0 joint extra days
                if fbuy_check = 1 AND so_autopo = 1
                                  AND so_jointex < 0;
                   so_jointex = 0;
                endif;

                so_recalc = 0;

                exec sql
                  update k_suplsoq
                    set so_actureg = :so_actureg,
                        so_actunet = :so_actunet,
                        so_actuwgt = :so_actuwgt,
                        so_actuvol = :so_actuvol,
                        so_actupqt = :so_actupqt,
                        so_actuoth = :so_actuoth,
                        so_actuun7 = :so_actuun7,
                        so_actuun8 = :so_actuun8,
                        so_actuun9 = :so_actuun9,
                        so_replreg = :so_replreg,
                        so_replnet = :so_replnet,
                        so_replwgt = :so_replwgt,
                        so_replvol = :so_replvol,
                        so_replpqt = :so_replpqt,
                        so_reploth = :so_reploth,
                        so_replun7 = :so_replun7,
                        so_replun8 = :so_replun8,
                        so_replun9 = :so_replun9,
                        so_chksoq = :so_chksoq,
                        so_chk6mon = :so_chk6mon,
                        so_chkover = :so_chkover,
                        so_chkback = :so_chkback,
                        so_jointex = :so_jointex,
                        so_rebldop = :so_rebldop,
                        so_soqtype = :so_soqtype,
                        so_recalc = :so_recalc
                    where current of socursor;
             endif;
             exsr clssocursor;
         endif;
       else;

       //    if order fails to reach supplier bracket, then mark as failed
          if achieved = *off and recalc_tst = 1;
             exec sql
               update k_suplsoq
                  set so_recalc = 1
                  where so_comp = :comp and
                        so_buyr = :buyr and
                        so_locn = :locn and
                        so_supl = :supl and
                        so_suplsub = :suplsub and
                        so_soqseq# = :soqseq#;
          endif;

       endif;
       endsr;

       begsr dclpqcursor;
       exec sql
        declare pqcursor Cursor
         for DynSQLStmtpq;
       endsr;

       begsr PrepDynSQLStmtpq;
       exec sql
        Prepare DynSqlStmtpq
          From :StmtString;
       endsr;

       Begsr IntSQLStmtpq;
       String = *blanks;
       String =   'Select * +
                   From K_Prodsoq +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrchpq;
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
                              pq_soqseq#, +
                              pq_prodseq +
                     For update of pq_soqovrd, +
                                   pq_soqact, +
                                   pq_opointu, +
                                   pq_opointd, +
                                   pq_oruptou, +
                                   pq_oruptod, +
                                   pq_oruptox, +
                                   pq_soqsrvc, +
                                   pq_dolrext, +
                                   pq_soqpcnt, +
                                   pq_chkopnt, +
                                   pq_selfbxd, +
                                   pq_chksoq, +
                                   pq_chk6mon, +
                                   pq_chkover, +
                                   pq_chkback';

       endsr;


       begsr opnpqcursor;
       exec sql
        open pqcursor
          using :comp,
                :buyr,
                :locn,
                :supl,
                :suplsub,
                :soqseq#;
       endsr;

       begsr clspqcursor;
       exec sql
        close pqcursor;
       endsr;

       begsr dclprcursor;
       exec sql
        declare prcursor Cursor
         for DynSQLStmtpr;
       endsr;

       begsr PrepDynSQLStmtpr;
       exec sql
        Prepare DynSqlStmtpr
          From :StmtString;
       endsr;

       Begsr IntSQLStmtpr;
       String = *blanks;
       String =   'Select * +
                   From K_Product +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrchpr;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'pr_comp = ? and +
                     pr_locn = ? and +
                     pr_supl = ? and +
                     pr_suplsub = ? and +
                     pr_prod = ? +
                     Order by pr_comp, +
                              pr_locn, +
                              pr_supl, +
                              pr_suplsub, +
                              pr_prod +
                     For update of pr_soqnite';
       endsr;

       begsr opnprcursor;
       exec sql
        open prcursor
          using :comp,
                :locn_hold,
                :supl_hold,
                :suplsub_hold,
                :pq_prod;
       endsr;

       begsr clsprcursor;
       exec sql
        close prcursor;
       endsr;

       begsr dclsocursor;
       exec sql
        declare socursor Cursor
          for
        select *
          from k_suplsoq
          where so_comp = :comp and
                so_buyr = :buyr and
                so_locn = :locn and
                so_supl = :supl and
                so_suplsub = :suplsub and
                so_soqseq# = :soqseq#
          order by so_comp,
                   so_buyr,
                   so_locn,
                   so_supl,
                   so_suplsub,
                   so_soqseq#
          for update of so_actureg,
                        so_actunet,
                        so_actuwgt,
                        so_actuvol,
                        so_actupqt,
                        so_actuoth,
                        so_actuun7,
                        so_actuun8,
                        so_actuun9,
                        so_replreg,
                        so_replnet,
                        so_replwgt,
                        so_replvol,
                        so_replpqt,
                        so_reploth,
                        so_replun7,
                        so_replun8,
                        so_replun9,
                        so_chksoq,
                        so_chk6mon,
                        so_chkover,
                        so_chkback,
                        so_jointex,
                        so_rebldop,
                        so_soqtype,
                        so_recalc;
       endsr;

       begsr opnsocursor;
       exec sql
        open socursor;
       endsr;

       begsr clssocursor;
       exec sql
        close socursor;
       endsr;
      /end-free

      * ---------------------------------- calculate usage (days into units)
     c*copy k3s_c101

      * --------------------------------- calculate suggested order quantity
     c*copy k3s_c121

      * ------------------------------------ calculate manual min/max values
     c*copy k3s_c131

