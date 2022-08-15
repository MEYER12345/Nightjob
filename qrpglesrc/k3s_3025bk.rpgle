      *****************************************************************
     h copyright('(C) Copyright 1996 - 2016 King III Solutions, Inc.  +
     h Rel 5.3  2016-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')

     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)
     h BNDDIR('K3S_BNDDIR')
      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2016 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_3025
      **   Type: ILE RPG Program
      **   Desc: Product SOQ simulator
      **
      *****************************************************************
      **
      **  This program is used to calculate the suggested order quantity
      **  to reach a given service level objective for one product only.
      **
      *****************************************************************
     fK3S_3025fmcf   e             workstn infds(infds)


     fk_prodsoqbif   e           k disk
      * selected products batches

     fk_suplsoqaif   e           k disk
      * selected products batches

     fk_productaif   e           k disk
      * products

     fk_suplieraif   e           k disk
      * suppliers

     fk_locatnsaif   e           k disk
      * locations

     fk_prodsezaif   e           k disk
      * product seasonal profiles

     fk_tablcodaif   e           k disk                                         table codes
      * table file

     fk_suplvocauf a e           k disk                                         deal summary info
      * supplier variable order cycle

     fk_prodwkdauf a e           k disk                                         table codes
      * product weekly distribution

      * ------------------------------------ File information data structure
     d/copy k3s_c010

      * ------------------------------------------- Function key definitions
     d/copy k3s_c020

      * ---------------------------------------------------- Local Data Area
     d/copy k3s_c030

      * -------------------------------------- Program Status Data Structure
     d/copy k3s_c040

      * ---------------------------------- Message logic for screen programs
     d/copy k3s_c050

      * ---------------------------------- calculate safety stock components
     d*copy k3s_c090

      * ---------------------------------- calculate usage (days into units)
     d*copy k3s_c100

      * ----------------------------------------- calculations with GK table
     d/copy k3s_c110

      * --------------------------------- calculate suggested order quantity
     d*copy k3s_c120

      * ------------------------------------ calculate manual min/max values
     d*copy k3s_c130

      * ---------------------------------- Display headings for history
     d/copy k3s_c140

      * ----------------------------------- D-specs for common workfields
     d*copy k3s_c270

      * --------------------------------------------------------- Workfields
     d once            s              1
     d least_1         s              1
     d minday          s              9  3
     d maxday          s              9  3
     d minunit         s              7  0
     d maxunit         s              7  0
     d mnwork          s              7  0
     d mxwork          s              7  0
     d extension       s             15  4
     d onetime         s                   like(pq_comp)
     d zz_usrdate      s             10
     d soqactw1        s                   like(pq_soqact)
     d soqactw2        s                   like(pq_soqact)
     d soqactsv        s                   like(pq_soqact)
     d convpak         s                   like(pr_convpak)
     d convpak1        s                   like(pr_convpak)
     d convpak2        s                   like(pr_convpak)
     d convpak3        s                   like(pr_convpak)
     D Weeks           S              7  0
     D DayNbr          S              1P 0
     d max_to_min      s              1  0                                      Force Max to Min flg
     d xx_delay        s              7
     d mxtest          s              7
     d char_date       s             10
     d not_six         s              1p 0 inz(0)
     d errors          s              1
     d y               s              3  0
     d j               s              3  0
     d u1sttest        s             10
     d first12         s               d
     d first13         s               d
     d first52         s               d
     d today           s               d

     d @s_repcary      s              3  3
     d @s_linecst      s              5  2
     d @s_orcycle      s              3  0
     d @s_forcint      s              3  0
     d @s_buymult      s              7  0
     d @s_minqty       s              7  0
     d @s_costeac      s             11  4
     d @s_forcast      s              9  2
     d @s_fordevp      s              3  3
     d @s_service      s              3  3
     d @s_leadtm       s              3  0
     d @s_leadtmv      s              3  3
     d @s_formeth      s              1  0
     d @s_sstimef      s              9  3
     d @s_otimfac      s              3  0
     d @s_devtime      s              7  3
     d @s_intrval      s              5  0
     d meandevary      s              5  4 dim(71)                              mean dev runtime ary
     d strddevary      s              5  4 dim(37)                              std dev runtime ary

     d @q_maxunit      s              7  0
     d @q_minunit      s              7  0
     d @q_qtybaln      s              7  0
     d @q_minqty       s              7  0
     d @q_buymult      s              7  0
     d @q_convpak      s              7  0
     d @q_convpkp      s              3  1
     d @q_soq          s              7  0

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
     d  @u_Ds_fctr     s              5  2 dim(12)                              Profile factor
     d  @u_Ds_warn     s              1  0                                      Calculate results?

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

      *   profile array
     d                 ds
     d @u_ixa                  1    260
     d  pz_factr01                         overlay(@u_ixa:01)
     d  pz_factr02                         overlay(@u_ixa:06)
     d  pz_factr03                         overlay(@u_ixa:11)
     d  pz_factr04                         overlay(@u_ixa:16)
     d  pz_factr05                         overlay(@u_ixa:21)
     d  pz_factr06                         overlay(@u_ixa:26)
     d  pz_factr07                         overlay(@u_ixa:31)
     d  pz_factr08                         overlay(@u_ixa:36)
     d  pz_factr09                         overlay(@u_ixa:41)
     d  pz_factr10                         overlay(@u_ixa:46)
     d  pz_factr11                         overlay(@u_ixa:51)
     d  pz_factr12                         overlay(@u_ixa:56)
     d  pz_factr13                         overlay(@u_ixa:61)
     d  pz_factr14                         overlay(@u_ixa:66)
     d  pz_factr15                         overlay(@u_ixa:71)
     d  pz_factr16                         overlay(@u_ixa:76)
     d  pz_factr17                         overlay(@u_ixa:81)
     d  pz_factr18                         overlay(@u_ixa:86)
     d  pz_factr19                         overlay(@u_ixa:91)
     d  pz_factr20                         overlay(@u_ixa:96)
     d  pz_factr21                         overlay(@u_ixa:101)
     d  pz_factr22                         overlay(@u_ixa:106)
     d  pz_factr23                         overlay(@u_ixa:111)
     d  pz_factr24                         overlay(@u_ixa:116)
     d  pz_factr25                         overlay(@u_ixa:121)
     d  pz_factr26                         overlay(@u_ixa:126)
     d  pz_factr27                         overlay(@u_ixa:131)
     d  pz_factr28                         overlay(@u_ixa:136)
     d  pz_factr29                         overlay(@u_ixa:141)
     d  pz_factr30                         overlay(@u_ixa:146)
     d  pz_factr31                         overlay(@u_ixa:151)
     d  pz_factr32                         overlay(@u_ixa:156)
     d  pz_factr33                         overlay(@u_ixa:161)
     d  pz_factr34                         overlay(@u_ixa:166)
     d  pz_factr35                         overlay(@u_ixa:171)
     d  pz_factr36                         overlay(@u_ixa:176)
     d  pz_factr37                         overlay(@u_ixa:181)
     d  pz_factr38                         overlay(@u_ixa:186)
     d  pz_factr39                         overlay(@u_ixa:191)
     d  pz_factr40                         overlay(@u_ixa:196)
     d  pz_factr41                         overlay(@u_ixa:201)
     d  pz_factr42                         overlay(@u_ixa:206)
     d  pz_factr43                         overlay(@u_ixa:211)
     d  pz_factr44                         overlay(@u_ixa:216)
     d  pz_factr45                         overlay(@u_ixa:221)
     d  pz_factr46                         overlay(@u_ixa:226)
     d  pz_factr47                         overlay(@u_ixa:231)
     d  pz_factr48                         overlay(@u_ixa:236)
     d  pz_factr49                         overlay(@u_ixa:241)
     d  pz_factr50                         overlay(@u_ixa:246)
     d  pz_factr51                         overlay(@u_ixa:251)
     d  pz_factr52                         overlay(@u_ixa:256)
     d @u_ix                          5s 2 dim(52)
     d                                     overlay(@u_ixa)

     d ManMinMaxR      s              1  0                                      manual min/max rev

      *** car count logic work fields
     d CarLogic        s              1  0                                      car logic used?
     d CarCount        s              7  0                                      number cars
     d CarRemain       s              7  0                                      remainder

      * --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto

      * parameters passed to program
     d K3S_3025        PI
     d  comp                          1
     d  locn                          5
     d  buyr                          5
     d  supl                         10
     d  suplsub                      10
     d  prod                         25
     d  soqseq#                       5  0
     d  soq                           7  0
     d  @return                       3  0
     d  modedsp                       8
      * ---------------------------------------------------------- likerec statements
      * key list for selected products review - batch header
     d prodsoqb_key    DS                  likerec(rk_prodsoq:*key)
      * key list for product file
     d producta_key    DS                  likerec(rk_product:*key)
      * key list for product weekly distribution
     d prodwkda_key    DS                  likerec(rk_prodwkd:*key)
      * key list for selected products review - batch header
     d suplsoqa_key    DS                  likerec(rk_suplsoq:*key)

      /free
       // ------------------------------------- get Manual Min/Max information

       // save Straight Manual Min/Max Days and Units processing flags
       //      @m_str_day = 1 means use straight days (flag 1)
       //      @m_str_day = 0 means do not use straight days (flag 1)
       //      @m_str_unt = 1 means use straight units (flag 2)
       //      @m_str_unt = 0 means do not use straight units (flag 2)
       ta_codetyp = 'APP';
       ta_codeval = 'MIN_MAX   STRAIGHT=1';
       chain (comp:ta_codetyp:ta_codeval) k_tablcoda;
       if %found(k_tablcoda);
          @m_str_day = ta_flag1;
          @m_str_unt = ta_flag2;
       else;
          @m_str_day = 0;
          @m_str_unt = 0;
       endif;

       // car count logic
       //      CarLogic = 1 means car count logic being used
       //      CarLogic = 0 means car count logic not being used
       ta_comp    = pr_comp;
       ta_codetyp = 'APP';
       ta_codeval = 'K3S_1500  CAR_COUNT ';
       chain (ta_comp:ta_codetyp:ta_codeval) k_tablcoda;
       if %found(k_tablcoda) and ta_flag1 = 1;
          CarLogic = 1;
       else;
          CarLogic = 0;
       endif;

       // Manual Min/Max sequence reversed ?
       //      ManMinMaxR = 1 means yes, calculate Days 1st, Units 2nd
       //      ManMinMaxR = 0 means no, leave it at Units 1st, Days 2nd
       ta_comp    = pr_comp;
       ta_codetyp = 'APP';
       ta_codeval = 'K3S_C131  REVERSE   ';
       chain (ta_comp:ta_codetyp:ta_codeval) k_tablcoda;
       if %found(k_tablcoda) and ta_flag1 = 1;
          ManMinMaxR = 1;
       else;
          ManMinMaxR = 0;
       endif;

       // Force calculated Maximum to calculated Minimum
       //      max_to_min = 1 means do set maximum to minimum
       //      max_to_min = 0 means to use normal EOQ logic
       ta_comp    = pr_comp;
       ta_codetyp = 'APP';
       ta_codeval = 'K3S_1500  MAX_TO_MIN';
       chain (ta_comp:ta_codetyp:ta_codeval) k_tablcoda;
       if %found(k_tablcoda);
          max_to_min = ta_flag1;
       else;
          max_to_min = 0;
       endif;

       // ------------------------------------------------------ Read Product

       exsr $_loadarrays;

       msgpgq = '*';
       exsr $_get_lda;
       zz_program = psds_progm;
       if once = ' ';

       // call module to obtain user update authority flag
          @usr_updat = 0;
          callp K3S_9050(zz_program:
                         @usr_updat);
          *in94 = (@usr_updat = 0);
          *in45 = *on;
          once  = 'Y';
          if *in94 = *on;
             *in45 = *off;
          endif;
       endif;
       prodsoqb_key.pq_comp = comp;
       prodsoqb_key.pq_buyr = buyr;
       prodsoqb_key.pq_locn = locn;
       prodsoqb_key.pq_supl = supl;
       prodsoqb_key.pq_suplsub = suplsub;
       prodsoqb_key.pq_soqseq# = soqseq#;
       prodsoqb_key.pq_prod = prod;
       chain %kds(prodsoqb_key) k_prodsoqb;

       producta_key.pr_comp = comp;
       producta_key.pr_locn = locn;
       producta_key.pr_supl = supl;
       producta_key.pr_suplsub = suplsub;
       producta_key.pr_prod = prod;
       chain %kds(producta_key) k_producta;

       suplsoqa_key.so_comp = comp;
       suplsoqa_key.so_buyr = buyr;
       suplsoqa_key.so_locn = locn;
       suplsoqa_key.so_supl = supl;
       suplsoqa_key.so_suplsub = suplsub;
       suplsoqa_key.so_soqseq# = soqseq#;
       chain %kds(suplsoqa_key) k_suplsoqa;

       chain (comp:locn:supl:suplsub) k_supliera;

       chain (comp:locn) k_locatnsa;

       chain(n) (comp:locn:supl:suplsub) k_suplvoca;
       if %found(k_suplvoca);
          if si_dowmap = 1 and
             pq_oruptou > 0;

             *in20 = *on;
          else;
             *in20 = *off;
          endif;
          *in34 = *on;
          zz_voctype = si_rectype;

       //  --------  variable order cycle section -- begin
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
          callp K3S_M160(pr_comp:
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

          if daynbr = 1;
             day1 = ' 1 Sunday   ';
             day2 = ' 2 Monday   ';
             day3 = ' 3 Tuesday  ';
             day4 = ' 4 Wednesday';
             day5 = ' 5 Thursday ';
             day6 = ' 6 Friday   ';
             day7 = ' 7 Saturday ';
             day8 = ' 8 Sunday   ';
             day9 = ' 9 Monday   ';
             day10 = '10 Tuesday  ';
             day11 = '11 Wednesday';
             day12 = '12 Thursday ';
          endif;
          if daynbr = 2;
             day7 = ' 7 Sunday   ';
             day8 = ' 8 Monday   ';
             day9 = ' 9 Tuesday  ';
             day10 = '10 Wednesday';
             day11 = '11 Thursday ';
             day12 = '12 Friday   ';
             day1 = ' 1 Monday   ';
             day2 = ' 2 Tuesday  ';
             day3 = ' 3 Wednesday';
             day4 = ' 4 Thursday ';
             day5 = ' 5 Friday   ';
             day6 = ' 6 Saturday ';
          endif;
          if daynbr = 3;
             day6 = ' 6 Sunday   ';
             day7 = ' 7 Monday   ';
             day8 = ' 8 Tuesday  ';
             day9 = ' 9 Wednesday';
             day10 = '10 Thursday ';
             day11 = '11 Friday   ';
             day12 = '12 Saturday ';
             day1 = ' 1 Tuesday  ';
             day2 = ' 2 Wednesday';
             day3 = ' 3 Thursday ';
             day4 = ' 4 Friday   ';
             day5 = ' 5 Saturday ';
          endif;
          if daynbr = 4;
             day5 = ' 5 Sunday   ';
             day6 = ' 6 Monday   ';
             day7 = ' 7 Tuesday  ';
             day8 = ' 8 Wednesday';
             day9 = ' 9 Thursday ';
             day10 = '10 Friday   ';
             day11 = '11 Saturday ';
             day12 = '12 Sunday   ';
             day1 = ' 1 Wednesday';
             day2 = ' 2 Thursday ';
             day3 = ' 3 Friday   ';
             day4 = ' 4 Saturday ';
          endif;
          if daynbr = 5;
             day4 = ' 4 Sunday   ';
             day5 = ' 5 Monday   ';
             day6 = ' 6 Tuesday  ';
             day7 = ' 7 Wednesday';
             day8 = ' 8 Thursday ';
             day9 = ' 9 Friday   ';
             day10 = '10 Saturday ';
             day11 = '11 Sunday   ';
             day12 = '12 Monday   ';
             day1 = ' 1 Thursday ';
             day2 = ' 2 Friday   ';
             day3 = ' 3 Saturday ';
          endif;
          if daynbr = 6;
             day3 = ' 3 Sunday   ';
             day4 = ' 4 Monday   ';
             day5 = ' 5 Tuesday  ';
             day6 = ' 6 Wednesday';
             day7 = ' 7 Thursday ';
             day8 = ' 8 Friday   ';
             day9 = ' 9 Saturday ';
             day10 = '10 Sunday   ';
             day11 = '11 Monday   ';
             day12 = '12 Tuesday  ';
             day1 = ' 1 Friday   ';
             day2 = ' 2 Saturday ';
          endif;
          if daynbr = 7;
             day2 = ' 2 Sunday   ';
             day3 = ' 3 Monday   ';
             day4 = ' 4 Tuesday  ';
             day5 = ' 5 Wednesday';
             day6 = ' 6 Thursday ';
             day7 = ' 7 Friday   ';
             day8 = ' 8 Saturday ';
             day9 = ' 9 Sunday   ';
             day10 = '10 Monday   ';
             day11 = '11 Tuesday  ';
             day12 = '12 Wednesday';
             day1 = ' 1 Saturday ';
          endif;
       //    D=Daily
          if si_rectype = 'D';
       //***       save off Weekly Distribution values
             @u_Ds_flag = 1;
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

             prodwkda_key.pm_comp = comp;
             prodwkda_key.pm_locn = locn;
             prodwkda_key.pm_supl = supl;
             prodwkda_key.pm_suplsub = suplsub;
             prodwkda_key.pm_prod = prod;
             chain(n) %kds(prodwkda_key) k_prodwkda;

             if %found(k_prodwkda);
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
       // --------  variable order cycle section -- end

       else;
          *in34 = *off;
          zz_voctype = *blanks;
          @u_dowmap  = 0;
       endif;


       // calclate 'Other days' for screen presentation
       zz_othdays = pq_opointd -
                    so_orcycle -
                    pq_leadtm  -
                    pq_sstfday;
       if pq_sysstat = 'D' or
          pq_sysstat = 'N' or
          pq_forcast = 0 or
       // pq_maminid > 0 or
       // pq_mamaxid > 0 or
          pq_maminiu > 0 or
          pq_mamaxiu > 0 or
          zz_othdays < 0 or
          pq_usrstat = 'M';

          zz_othdays = 0;
       endif;


       // include customer back orders from night job interface file, and
       //         hold out quantity and transfer pending quantity
       pq_qtybsum =    pq_qtyback +
                       pq_qtyhold +
                       pq_qtypend;

       // include promotional quantity, if within dates
       if (pr_prombeg <= lc_sysdate) AND
          (pr_promend >= lc_sysdate);

          pq_qtybsum += pr_promqty;
       endif;

       // --------------------------------------------------- Process products
       dow (key_press <> f12_key) and
           (key_press <> f05_key);

           exsr $_edt_scrn;
           if errors = *off;
       // process product record

       // calculate balance
              pq_qtybaln = pq_qtyohnd
                         + pq_qtyoord
                         - pq_qtybsum;

              clear pq_soqact;

       // --------------------------------------------- Major categories begin
       // select the category of product to work with
              select;

       //                                           ----------- New products
       // new products
                 when pr_sysstat = 'N';

       //                                             -------- Manual products
       // manual products accumulation
                 when pr_usrstat = 'M';

       //                                             -- Discontinued products
       // discontinued products accumulation
                 when pr_sysstat = 'D';

       //   if balance is negative, purchase the negative amount
                    if pr_qtybaln < 0 AND lc_purdisc = 1;

       //       make value positive
                       pq_soqact  = pr_qtybaln * -1;

       //   if regular order (not alternate source order), get cost
                       if pq_altsrce <> 1;
                          pq_costord = pr_costreg;

       //      see if flag is set for rounding of the balance
       //            values:  0  do not round
       //                     1  round up to cover total SOQ
       //                     2  round up or down
                           if lc_rnddisc > 0;
                              soqactw1 = pq_soqact / pr_buymult;
                              soqactw2 = %rem(pq_soqact:pr_buymult);

                              if soqactw1 > 0;
                                 least_1  = *on;
                              else;
                                 least_1  = *off;
                              endif;

                              if soqactw2 > 0;
                                 soqactw1 += 1;

                                 if least_1 = *on  AND  lc_rnddisc = 2;
                                    eval(h)   soqactw1 = pq_soqact / pr_buymult;
                                 endif;

                                 pq_soqact = pr_buymult * soqactw1;
                              endif;

       //   purchase minimum quantity test
                              if pq_soqact < pr_minqty;
                                 pq_soqact = pr_minqty;
                              endif;

                           endif;
                         endif;
                       endif;

       //                                             ------- No cost products
       // product must have cost for calculations
                 when pr_costeac = 0;

       //  No cost products start ----------------
       //    only exception would be if buyer uses Manual Min/Max in UNITS

       //    only execute if a manual minimum UNITS is used
                    if pr_maminiu > 0;

                       @m_seasonl = pr_seasonl;
                       @m_forcast = pr_forcast;
                       @m_forcper = pr_forcper;
                       @m_forcint = pr_forcint;
                       @m_longtrm = pr_longtrm;

                       @m_altsour = 0;
                       @m_maminiu = pr_maminiu;
                       @m_mamaxiu = pr_mamaxiu;
                       @m_maminid = pr_maminid;
                       @m_mamaxid = pr_mamaxid;
                       @m_qtybaln = pr_qtybaln;
                       @m_chkopnt = pq_chkopnt;

                       @m_minday  = 0;
                       @m_maxday  = 0;
                       @m_minunit = pr_maminiu;
                       @m_maxunit = pr_mamaxiu;
                       @m_mnwork  = 0;
                       @m_mxwork  = 0;

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
       //              exsr $_manual;

                       minday     = @m_minday;
                       maxday     = @m_maxday;
                       minunit    = @m_minunit;
                       maxunit    = @m_maxunit;
                       mnwork     = @m_mnwork;
                       mxwork     = @m_mxwork;
                       pq_chkopnt = @m_chkopnt;

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
                          @q_minqty  = pr_minqty;
                          @q_buymult = pr_buymult;
                          @q_convpak = pr_convpak;
                          @q_convpkp = pr_convpkp;

                          callp K3S_Calc_SOQ(@q_maxunit:
                                             @q_minunit:
                                             @q_qtybaln:
                                             @q_minqty:
                                             @q_buymult:
                                             @q_convpak:
                                             @q_convpkp:
                                             @q_soq);
       //                 exsr $_calc_SOQ;

                          pq_soqact = @q_soq;
                       endif;

                    endif;
       //  No cost products end ------------------

       //                                              --- All normal products
       //  This section includes all other products not selected above,
       //       which would be all normal products to be calculated.
                 other;

                    clear @s_sstimef;
                    clear @s_otimfac;
                    clear @s_devtime;
                    clear @s_intrval;

                    @s_repcary = lc_repcary * .01;
                    @s_linecst = pq_linecst;
                    @s_orcycle = so_orcycle;
                    @s_forcint = pr_forcint;
                    @s_buymult = pr_buymult;
                    @s_minqty  = pr_minqty;
                    @s_costeac = pr_costeac;
                    @s_forcast = pr_forcast;
                    @s_fordevp = pr_fordevp * .01;
                    @s_service = pr_service * .01;
                    @s_leadtm  = pq_leadtm;
                    @s_leadtmv = pq_leadtmv * .01;
                    @s_formeth = pr_formeth;

       // call subroutine to calculate safety stock
                    callp K3S_Calc_Safety(@s_repcary:
                                          @s_linecst:
                                          @s_orcycle:
                                          @s_forcint:
                                          @s_buymult:
                                          @s_minqty:
                                          @s_costeac:
                                          @s_forcast:
                                          @s_fordevp:
                                          @s_service:
                                          @s_leadtm:
                                          @s_leadtmv:
                                          @s_formeth:
                                          @s_sstimef:
                                          @s_otimfac:
                                          @s_devtime:
                                          @s_intrval:
                                          meandevary:
                                          strddevary);
       //           exsr $_safety;

                    pr_sstimef = @s_sstimef;
                    pr_otimfac = @s_otimfac;
                    pr_devtimf = @s_devtime;
                    pr_intrval = @s_intrval;

       // if using Weekly Distribution logic,
       //    don't use regular Safety Stock Days logic (use SS Days of 0),
       //    only buy Order Cycle and Lead Time but nothing additional
       //      because of economics
                    if @u_dowmap  = 1;
                       @s_sstimef = 0;
                       pr_sstimef = 0;
                       @s_intrval = 0;
                       pr_intrval = 0;
                    endif;

       // get seasonal profile information

       //     get profile factors, if product contains seasonal profile ID

                    if pr_seasonl <> *blanks;
                       pz_seasonl = pr_seasonl;
                       chain (comp:pr_seasonl) k_prodseza;
                    endif;

       //     set factors to 1.00 if:

       //          1) profile not found for this product, or
       //          2) no profile for this product, and long term trend <> 1.00
       //          3) no profile for this product, and Weekly Distribution ON

                    if ((pr_seasonl <> *blanks) and
                       not %found(k_prodseza))
                           or
                       ((pr_seasonl = *blanks) and
                        (pr_longtrm <> 1.00))
                           or
                       ((pr_seasonl = *blanks) and
                        (@u_dowmap = 1));

                       pz_factr01 = 1.00;
                       pz_factr02 = 1.00;
                       pz_factr03 = 1.00;
                       pz_factr04 = 1.00;
                       pz_factr05 = 1.00;
                       pz_factr06 = 1.00;
                       pz_factr07 = 1.00;
                       pz_factr08 = 1.00;
                       pz_factr09 = 1.00;
                       pz_factr10 = 1.00;
                       pz_factr11 = 1.00;
                       pz_factr12 = 1.00;
                       pz_factr13 = 1.00;
                       pz_factr14 = 1.00;
                       pz_factr15 = 1.00;
                       pz_factr16 = 1.00;
                       pz_factr17 = 1.00;
                       pz_factr18 = 1.00;
                       pz_factr19 = 1.00;
                       pz_factr20 = 1.00;
                       pz_factr21 = 1.00;
                       pz_factr22 = 1.00;
                       pz_factr23 = 1.00;
                       pz_factr24 = 1.00;
                       pz_factr25 = 1.00;
                       pz_factr26 = 1.00;
                       pz_factr27 = 1.00;
                       pz_factr28 = 1.00;
                       pz_factr29 = 1.00;
                       pz_factr30 = 1.00;
                       pz_factr31 = 1.00;
                       pz_factr32 = 1.00;
                       pz_factr33 = 1.00;
                       pz_factr34 = 1.00;
                       pz_factr35 = 1.00;
                       pz_factr36 = 1.00;
                       pz_factr37 = 1.00;
                       pz_factr38 = 1.00;
                       pz_factr39 = 1.00;
                       pz_factr40 = 1.00;
                       pz_factr41 = 1.00;
                       pz_factr42 = 1.00;
                       pz_factr43 = 1.00;
                       pz_factr44 = 1.00;
                       pz_factr45 = 1.00;
                       pz_factr46 = 1.00;
                       pz_factr47 = 1.00;
                       pz_factr48 = 1.00;
                       pz_factr49 = 1.00;
                       pz_factr50 = 1.00;
                       pz_factr51 = 1.00;
                       pz_factr52 = 1.00;
                    endif;

                    @u_comp    = pr_comp;
                    @u_seasonl = pr_seasonl;
                    @u_forcast = pr_forcast;
                    @u_forcper = pr_forcper;
                    @u_forcint = pr_forcint;
       //** calculate weekly average for display in F20=Results window
                    select;
                       when pr_forcint = 52;
                            @u_weekly  = pr_forcast;
                       when pr_forcint = 12;
                            @u_weekly  = (pr_forcast * 12) / 52;
                       when pr_forcint = 13;
                            @u_weekly  = (pr_forcast * 13) / 52;
                    endsl;

                    @u_longtrm = pr_longtrm;
       //---------------------------------------------- calculate min/max days
       // calculate minimum and maximum values
                    minday = pr_sstimef +
                             pq_leadtm  +
                             so_orcycle;

                    maxday = pr_sstimef +
                             pq_leadtm  +
                             pr_intrval;

       // force calculated maximum days to minimum days
                    if max_to_min = 1;
                       maxday = minday;
                    endif;

       // include other days for minimum
                    minday += zz_othdays;

       //--------------------------------------------- calculate min/max units
                    clear @u_use;
                    @u_days = minday;

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
                                         @U_Ds_prno:
                                         @U_Ds_fctr:
                                         @U_Ds_warn);
       //           exsr $_usage;

                    eval(h) minunit = @u_use;

       //     if usage exists, then unit must be at least 1
                    if (minunit = 0) and (@u_use > 0);
                       minunit = 1;
                    endif;

       //     if order up to level <= order point in days, then make both
       //     days and units equal to minimum days and units
                    if (maxday <= minday);
                       maxday = minday;
                       maxunit = minunit;

       //     calculate order up to level in units
                    else;

                       clear @u_use;
                       @u_days = maxday;


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
                                            @U_Ds_warn);
       //              exsr $_usage;

                       eval(h) maxunit = @u_use;

       //     if usage exists, then unit must be at least 1
                       if (maxunit = 0) and (@u_use > 0);
                          maxunit = 1;
                       endif;

                    endif;

       // set min and max days into work fields
                    eval(h) mnwork = minday;
                    eval(h) mxwork = maxday;

       // ------------------------ logic for manual minimum and maximum values

       //    only execute if a manual minimum or maximum exists, AND
       //         product has a forecast
                    if ((pr_maminiu > 0) or (pr_maminid > 0) or
                        (pr_mamaxiu > 0) or (pr_mamaxid > 0))
                         AND       pr_forcast > 0;

                         @m_seasonl = pr_seasonl;
                         @m_forcast = pr_forcast;
                         @m_forcper = pr_forcper;
                         @m_forcint = pr_forcint;
                         @m_longtrm = pr_longtrm;

                         @m_altsour = pq_altsrce;
                         @m_maminiu = pr_maminiu;
                         @m_mamaxiu = pr_mamaxiu;
                         @m_maminid = pr_maminid;
                         @m_mamaxid = pr_mamaxid;
                         @m_qtybaln = pq_qtybaln;
                         @m_chkopnt = pq_chkopnt;
                         @m_sochk   = pq_chkopnt;

                         @m_minday  = minday;
                         @m_maxday  = maxday;
                         @m_minunit = minunit;
                         @m_maxunit = maxunit;
                         @m_mnwork  = mnwork;
                         @m_mxwork  = mxwork;

       // call subroutine to calculate manual minimum or maximum values
                         if ManMinMaxR = 0;
       //      sequence will be Units 1st, Days 2nd
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
                         else;
       //      sequence will be Days 1st, Units 2nd
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

                    endif;

       // ------------------------------------------------------- all products
       //  if maximum units less than minimum units, make equal
                    if maxunit < minunit;
                       maxunit = minunit;
                       mxwork  = mnwork;
                    endif;

       //  save data into fields for later update
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

                    pq_intrval = pr_intrval;
                    pq_sstimef = pr_sstimef;

       //      save safety stock days into a 3.0 field for screen presentation
                    eval(h) pq_sstfday = pr_sstimef;

       // car count logic
                    if CarLogic     = 1 and
                       pr_carcoun   = 1 and
                       pr_maminiu   > 1 and
                       pq_oruptou   > pr_maminiu;

                       CarCount     = %div(pq_oruptou:pr_maminiu);
                       CarRemain    = %rem(pq_oruptou:pr_maminiu);
                       if CarRemain > 0;
                          CarCount  += 1;
                       endif;
                       pq_opointu = CarCount * pr_maminiu;
                       pq_oruptou = CarCount * pr_maminiu;
                    endif;

       // ------------------------------------------- calculate order quantity

       // calculate order quantity if balance falls below order point
                    if pq_qtybaln < pq_opointu;

       // call subroutine to calculate suggested order quantity
                       clear pq_soqact;

                       @q_maxunit = pq_oruptou;
                       @q_minunit = pq_opointu;
                       @q_qtybaln = pq_qtybaln;
                       @q_minqty  = pr_minqty;
                       @q_buymult = pr_buymult;
                       @q_convpak = pr_convpak;
                       @q_convpkp = pr_convpkp;


                       callp K3S_Calc_SOQ(@q_maxunit:
                                          @q_minunit:
                                          @q_qtybaln:
                                          @q_minqty:
                                          @q_buymult:
                                          @q_convpak:
                                          @q_convpkp:
                                          @q_soq);
       //              exsr $_calc_SOQ;

                       pq_soqact = @q_soq;

                       zz_total4 = pr_costreg * pq_soqact;
                       zz_total3 = pq_soqact * pq_costord /
                                    pq_costdiv;
                    endif;

              endsl;
           endif;
       // ----------------------------------------------------- Product checks
       // if errors exist, and user set for alarm, sound alarm
           if (errors = *on) and
              (lda_alarm = 1);

              *in98 = *on;
           else;
              *in98 = *off;
           endif;

           if key_press = f20_key;
              day1p = @u_ds_prno(1);
              day1f = @u_ds_fctr(1);
              day1w = @u_ds_wdis(1);
              day1s = @u_ds_spct(1);
              if @u_ds_dlyu(1) > 999999.9999;
                 day1d = 999999.9999;
              else;
                 day1d = @u_ds_dlyu(1);
              endif;
              if @u_ds_susg(1) > 999999.9999;
                 day1ds = 999999.9999;
              else;
                 day1ds = @u_ds_susg(1);
              endif;
              if @u_ds_accu(1) > 9999999.9999;
                 day1a = 9999999.9999;
              else;
                 day1a = @u_ds_accu(1);
              endif;
       //*** calculate day's total
              day1t = day1d + day1ds;
              day2p = @u_ds_prno(2);
              day2f = @u_ds_fctr(2);
              day2w = @u_ds_wdis(2);
              day2s = @u_ds_spct(2);
              if @u_ds_dlyu(2) > 999999.9999;
                 day2d = 999999.9999;
              else;
                 day2d = @u_ds_dlyu(2);
              endif;
              if @u_ds_susg(2) > 999999.9999;
                 day2ds = 999999.9999;
              else;
                 day2ds = @u_ds_susg(2);
              endif;
              if @u_ds_accu(2) > 9999999.9999;
                 day2a = 9999999.9999;
              else;
                 day2a = @u_ds_accu(2);
              endif;
       //*** calculate day's total
              day2t = day2d + day2ds;
              day3p = @u_ds_prno(3);
              day3f = @u_ds_fctr(3);
              day3w = @u_ds_wdis(3);
              day3s = @u_ds_spct(3);
              if @u_ds_dlyu(3) > 999999.9999;
                 day3d = 999999.9999;
              else;
                 day3d = @u_ds_dlyu(3);
              endif;
              if @u_ds_susg(3) > 999999.9999;
                 day3ds = 999999.9999;
              else;
                 day3ds = @u_ds_susg(3);
              endif;
              if @u_ds_accu(3) > 9999999.9999;
                 day3a = 9999999.9999;
              else;
                 day3a = @u_ds_accu(3);
              endif;
       //**** calculate day's total
              day3t = day3d + day3ds;
              day4p = @u_ds_prno(4);
              day4f = @u_ds_fctr(4);
              day4w = @u_ds_wdis(4);
              day4s = @u_ds_spct(4);
              if @u_ds_dlyu(4) > 999999.9999;
                 day4d = 999999.9999;
              else;
                 day4d = @u_ds_dlyu(4);
              endif;
              if @u_ds_susg(4) > 999999.9999;
                 day4ds = 999999.9999;
              else;
                 day4ds = @u_ds_susg(4);
              endif;
              if @u_ds_accu(4) > 9999999.9999;
                 day4a = 9999999.9999;
              else;
                 day4a = @u_ds_accu(4);
              endif;
       //*** calculate day's total
              day4t = day4d + day4ds;
              day5p = @u_ds_prno(5);
              day5f = @u_ds_fctr(5);
              day5w = @u_ds_wdis(5);
              day5s = @u_ds_spct(5);
              if @u_ds_dlyu(5) > 999999.9999;
                 day5d = 999999.9999;
              else;
                 day5d = @u_ds_dlyu(5);
              endif;
              if @u_ds_susg(5) > 999999.9999;
                 day5ds = 999999.9999;
              else;
                 day5ds = @u_ds_susg(5);
              endif;
              if @u_ds_accu(5) > 9999999.9999;
                 day5a = 9999999.9999;
              else;
                 day5a = @u_ds_accu(5);
              endif;
       //*** calculate day's total
              day5t = day5d + day5ds;
              day6p = @u_ds_prno(6);
              day6f = @u_ds_fctr(6);
              day6w = @u_ds_wdis(6);
              day6s = @u_ds_spct(6);
              if @u_ds_dlyu(6) > 999999.9999;
                 day6d = 999999.9999;
              else;
                 day6d = @u_ds_dlyu(6);
              endif;
              if @u_ds_susg(6) > 999999.9999;
                 day6ds = 999999.9999;
              else;
                 day6ds = @u_ds_susg(6);
              endif;
              if @u_ds_accu(6) > 9999999.9999;
                 day6a = 9999999.9999;
              else;
                 day6a = @u_ds_accu(6);
              endif;
       //*** calculate day's total
              day6t = day6d + day6ds;
              day7p = @u_ds_prno(7);
              day7f = @u_ds_fctr(7);
              day7w = @u_ds_wdis(7);
              day7s = @u_ds_spct(7);
              if @u_ds_dlyu(7) > 999999.9999;
                 day7d = 999999.9999;
              else;
                 day7d = @u_ds_dlyu(7);
              endif;
              if @u_ds_susg(7) > 999999.9999;
                 day7ds = 999999.9999;
              else;
                 day7ds = @u_ds_susg(7);
              endif;
              if @u_ds_accu(7) > 9999999.9999;
                 day7a = 9999999.9999;
              else;
                 day7a = @u_ds_accu(7);
              endif;
       //*** calculate day's total
              day7t = day7d + day7ds;
              day8p = @u_ds_prno(8);
              day8f = @u_ds_fctr(8);
              day8w = @u_ds_wdis(8);
              day8s = @u_ds_spct(8);
              if @u_ds_dlyu(8) > 999999.9999;
                 day8d = 999999.9999;
              else;
                 day8d = @u_ds_dlyu(8);
              endif;
              if @u_ds_susg(8) > 999999.9999;
                 day8ds = 999999.9999;
              else;
                 day8ds = @u_ds_susg(8);
              endif;
              if @u_ds_accu(8) > 9999999.9999;
                 day8a = 9999999.9999;
              else;
                 day8a = @u_ds_accu(8);
              endif;
       //*** calculate day's total
              day8t = day8d + day8ds;
              day9p = @u_ds_prno(9);
              day9f = @u_ds_fctr(9);
              day9w = @u_ds_wdis(9);
              day9s = @u_ds_spct(9);
              if @u_ds_dlyu(9) > 999999.9999;
                 day9d = 999999.9999;
              else;
                 day9d = @u_ds_dlyu(9);
              endif;
              if @u_ds_susg(9) > 999999.9999;
                 day9ds = 999999.9999;
              else;
                 day9ds = @u_ds_susg(9);
              endif;
              if @u_ds_accu(9) > 9999999.9999;
                 day9a = 9999999.9999;
              else;
                 day9a = @u_ds_accu(9);
              endif;
       //*** calculate day's total
              day9t = day9d + day9ds;
              day10p = @u_ds_prno(10);
              day10f = @u_ds_fctr(10);
              day10w = @u_ds_wdis(10);
              day10s = @u_ds_spct(10);
              if @u_ds_dlyu(10) > 999999.9999;
                 day10d = 999999.9999;
              else;
                 day10d = @u_ds_dlyu(10);
              endif;
              if @u_ds_susg(10) > 999999.9999;
                 day10ds = 999999.9999;
              else;
                 day10ds = @u_ds_susg(10);
              endif;
              if @u_ds_accu(10) > 9999999.9999;
                 day10a = 9999999.9999;
              else;
                 day10a = @u_ds_accu(10);
              endif;
       //*** calculate day's total
              day10t = day10d + day10ds;
              day11p = @u_ds_prno(11);
              day11f = @u_ds_fctr(11);
              day11w = @u_ds_wdis(11);
              day11s = @u_ds_spct(11);
              if @u_ds_dlyu(11) > 999999.9999;
                 day11d = 999999.9999;
              else;
                 day11d = @u_ds_dlyu(11);
              endif;
              if @u_ds_susg(11) > 999999.9999;
                 day11ds = 999999.9999;
              else;
                 day11ds = @u_ds_susg(11);
              endif;
              if @u_ds_accu(11) > 9999999.9999;
                 day11a = 9999999.9999;
              else;
                 day11a = @u_ds_accu(11);
              endif;
       //*** calculate day's total
              day11t = day11d + day11ds;
              day12p = @u_ds_prno(12);
              day12f = @u_ds_fctr(12);
              day12w = @u_ds_wdis(12);
              day12s = @u_ds_spct(12);
              if @u_ds_dlyu(12) > 999999.9999;
                 day12d = 999999.9999;
              else;
                 day12d = @u_ds_dlyu(12);
              endif;
              if @u_ds_susg(12) > 999999.9999;
                 day12ds = 999999.9999;
              else;
                 day12ds = @u_ds_susg(12);
              endif;
              if @u_ds_accu(12) > 9999999.9999;
                 day12a = 9999999.9999;
              else;
                 day12a = @u_ds_accu(12);
              endif;
       //*** calculate day's total
              day12t = day12d + day12ds;
              *in57 = *off;
              *in58 = *off;
              *in59 = *off;
              *in60 = *off;
              *in61 = *off;
              *in62 = *off;
              *in63 = *off;
              *in64 = *off;
              *in65 = *off;
              *in66 = *off;
              *in67 = *off;
              *in68 = *off;
              *in69 = *off;
              if @u_ds_days = 12;
                 *in57 = *on;
                 *in58 = *on;
                 *in59 = *on;
                 *in61 = *on;
                 *in62 = *on;
                 *in63 = *on;
                 *in64 = *on;
                 *in65 = *on;
                 *in66 = *on;
                 *in67 = *on;
                 *in68 = *on;
                 *in69 = *on;
              endif ;
              if @u_ds_days = 11;
                 *in57 = *on;
                 *in58 = *on;
                 *in61 = *on;
                 *in62 = *on;
                 *in63 = *on;
                 *in64 = *on;
                 *in65 = *on;
                 *in66 = *on;
                 *in67 = *on;
                 *in68 = *on;
                 *in69 = *on;
              endif;
              if @u_ds_days = 10;
                 *in57 = *on;
                 *in61 = *on;
                 *in62 = *on;
                 *in63 = *on;
                 *in64 = *on;
                 *in65 = *on;
                 *in66 = *on;
                 *in67 = *on;
                 *in68 = *on;
                 *in69 = *on;
              endif;
              if @u_ds_days = 9;
                 *in61 = *on;
                 *in62 = *on;
                 *in63 = *on;
                 *in64 = *on;
                 *in65 = *on;
                 *in66 = *on;
                 *in67 = *on;
                 *in68 = *on;
                 *in69 = *on;
              endif;
              if @u_ds_days = 8;
                 *in61 = *on;
                 *in62 = *on;
                 *in63 = *on;
                 *in64 = *on;
                 *in65 = *on;
                 *in66 = *on;
                 *in67 = *on;
                 *in68 = *on;
              endif;
              if @u_ds_days = 7;
                 *in61 = *on;
                 *in62 = *on;
                 *in63 = *on;
                 *in64 = *on;
                 *in65 = *on;
                 *in66 = *on;
                 *in67 = *on;
              endif;
              if @u_ds_days = 6;
                 *in61 = *on;
                 *in62 = *on;
                 *in63 = *on;
                 *in64 = *on;
                 *in65 = *on;
                 *in66 = *on;
              endif;
              if @u_ds_days = 5;
                 *in61 = *on;
                 *in62 = *on;
                 *in63 = *on;
                 *in64 = *on;
                 *in65 = *on;
              endif;
              if @u_ds_days = 4;
                 *in61 = *on;
                 *in62 = *on;
                 *in63 = *on;
                 *in64 = *on;
              endif;
              if @u_ds_days = 3;
                 *in61 = *on;
                 *in62 = *on;
                 *in63 = *on;
              endif;
              if @u_ds_days = 2;
                 *in61 = *on;
                 *in62 = *on;
              endif;
              if @u_ds_days = 1;
                 *in61 = *on;
              endif;
              if @u_ds_warn = 1;
                 *in60 = *on;
              endif;
       //***           *in21 = *on
       //***           *in22 = *off
              dou  key_press = f12_key;
                   write k3_ctl_msg;
                   exfmt k3_3025_2r;
       //***       if key_press = f20_key and
       //***          *in21 = *on;
       //***          *in21 = *off;
       //***          *in22 = *on;
       //***       else;
       //***          if key_press = f20_key and
       //***             *in22 = *on;
       //***             *in21 = *on;
       //***             *in22 = *off;
       //***          endif;
       //***       endif;
              enddo;
              if key_press = f12_key;
                 key_press = enter_key;
                 zz_suplmsk = *blanks;
       // call module to convert date to user specification
                 callp K3S_Convert_ISO_Date(pr_birth:
                                            lda_usrdat:
                                            not_six:
                                            char_date);
                 zz_birth = %triml(char_date);

                 if suplsub    = *blanks;
                    zz_suplmsk = %trimr(supl) + ' ' +
                                 sp_name;
                 else;
                    zz_suplmsk = %trimr(supl) + ' ' +
                                 suplsub + ' ' +
                                 sp_name;
                 endif;

                 xx_delay = %editc(pq_delay:'X');
                 zz_delay = %dec(%subst(xx_delay:7:1):1:0);
                 if pq_delay < 0;
                    zz_delay *= -1;
                    *in78 = *on;
                 else;
                    *in78 = *off;
                 endif;

                 *in40 = *off;
                 *in41 = *off;
                 *in42 = *off;
                 *in43 = *off;

                 if pq_chksoq  = 1;
                    *in40 = *on;
                 endif;
                 if pq_chkopnt = 1;
                    *in41 = *on;
                 endif;
                 if pq_chkserv = 1;
                    *in42 = *on;
                 endif;

                 if pq_chkover = 1;
                    *in43 = *on;
                 endif;
                 callp K3S_Convert_ISO_Date(pr_lstordr:
                                            lda_usrdat:
                                            not_six:
                                            char_date);

                 zz_lstordr = %triml(char_date);
                 if so_altsrce = 2;
                    setll *hival k_supliera;
                    setll (lda_comp:pr_cmblocn:pr_cmbsupl:pr_cmbsub)
                           k_supliera;
                    reade (lda_comp:pr_cmblocn:pr_cmbsupl:pr_cmbsub)
                           k_supliera;

                    if not %eof(k_supliera);
                       zz_cmbname = sp_name;
                    else;
                       zz_cmbname = 'Unknown supplier  ';
                    endif;
                 else;
                    *in33 = *off;
                 endif;

                 chain (comp:locn:supl:suplsub) k_supliera;

                 zz_modedsp = modedsp;

                 if so_soqtype = 'AP';
                    *in79 = *on;
                 else;
                    *in79 = *off;
                 endif;

       // get time formatted
                 callp K3S_Retrieve_Date_Time(lda_usrdat:
                                              lda_usrtim:
                                              lda_usradj:
                                              zz_usrdate:
                                              zz_usrtime);

                 write k3_3025_3r;
              endif;
           endif;

           write k3_ctl_msg;
           exfmt k3_3025_1r;
           if key_press = f05_key;
              soq = pq_soqact;
              @return = 5;
           endif;

       // read products end of loop
       enddo;

       // --------------------------------------------------- End of Main Loop

       // finished, set on LR
       *inlr = *on;

       // ***************************************************** End of program
       ////////////////////////////////////////////////////// Edit flags

       begsr $_edt_scrn;

       *in70 = *off;
       *in71 = *off;
       *in72 = *off;
       *in73 = *off;
       *in74 = *off;
       *in75 = *off;
       *in76 = *off;
       *in77 = *off;
       *in78 = *off;
       *in79 = *off;
       *in80 = *off;
       *in81 = *off;
       *in82 = *off;
       *in83 = *off;
       *in84 = *off;
       // edit screen fields
       errors    = *off;

       if errors = *off;
          if pr_forcast < 0;
             errors = *on;
             if (errors = *on) and
                (lda_alarm = 1);

                *in98 = *on;
             else;
                *in98 = *off;
             endif;
             @msg_id    = 'K3_9999';
             @msg_text = *blanks;
             @msg_text = 'Forecsat must be >= 0     ';
             *in77 = *on;
             exsr $_add_msg;
             exsr $_pass_msg;
          else;
             errors = *off;
             *in77 = *off;
          endif;
       endif;

       if errors = *off;
          if pr_seasonl <> *blanks;
             chain (comp:pr_seasonl) k_prodseza;
             if not %found(k_prodseza);
                errors = *on;
                if (errors = *on) and
                   (lda_alarm = 1);

                   *in98 = *on;
                else;
                   *in98 = *off;
                endif;
                @msg_id    = 'K3_9999';
                @msg_text = *blanks;
                @msg_text = 'Profile does not exist    ';
                *in81 = *on;
                exsr $_add_msg;
                exsr $_pass_msg;
             else;
                errors = *off;
                *in81 = *off;
             endif;
          endif;
       endif;

       if errors = *off;
          if pq_leadtm  < 1;
             errors = *on;
             if (errors = *on) and
                (lda_alarm = 1);

                *in98 = *on;
             else;
                *in98 = *off;
             endif;
             @msg_id    = 'K3_9999';
             @msg_text = *blanks;
             @msg_text = 'Lead time must be >= 1    ';
             *in72 = *on;
             exsr $_add_msg;
             exsr $_pass_msg;
          else;
             errors = *off;
             *in72 = *off;
          endif;
       endif;

       if errors = *off;
          if pq_leadtmv < 0;
             errors = *on;
             if (errors = *on) and
                (lda_alarm = 1);

                *in98 = *on;
             else;
                *in98 = *off;
             endif;
             @msg_id    = 'K3_9999';
             @msg_text = *blanks;
             @msg_text = 'Variance must be >= 0     ';
             *in73 = *on;
             exsr $_add_msg;
             exsr $_pass_msg;
          else;
             errors = *off;
             *in73 = *off;
          endif;
       endif;

       if errors = *off;
          if zz_othdays < 0;
             errors = *on;
             if (errors = *on) and
                (lda_alarm = 1);

                *in98 = *on;
             else;
                *in98 = *off;
             endif;
             @msg_id    = 'K3_9999';
             @msg_text = *blanks;
             @msg_text = 'Days must be >= 0         ';
             *in75 = *on;
             exsr $_add_msg;
             exsr $_pass_msg;
          else;
             errors = *off;
             *in75 = *off;
          endif;
       endif;

       if errors = *off;
          if pq_linecst < .01;
             errors = *on;
             if (errors = *on) and
                (lda_alarm = 1);

                *in98 = *on;
             else;
                *in98 = *off;
             endif;
             @msg_id    = 'K3_9999';
             @msg_text = *blanks;
             @msg_text = 'Must be >= .01            ';
             *in71 = *on;
             exsr $_add_msg;
             exsr $_pass_msg;
          else;
             errors = *off;
             *in71 = *off;
          endif;
       endif;


       if errors = *off;
          if pr_service < 50.0;
             errors = *on;
             if (errors = *on) and
                (lda_alarm = 1);

                *in98 = *on;
             else;
                *in98 = *off;
             endif;
             @msg_id    = 'K3_9999';
             @msg_text = *blanks;
             @msg_text = 'Must be >= 50.0           ';
             *in76 = *on;
             exsr $_add_msg;
             exsr $_pass_msg;
          else;
             errors = *off;
             *in76 = *off;
          endif;
       endif;


       if errors = *off;
          if pr_buymult < 0;
             errors = *on;
             if (errors = *on) and
                (lda_alarm = 1);

                *in98 = *on;
             else;
                *in98 = *off;
             endif;
             @msg_id   = 'K3_9999';
             @msg_text = *blanks;
             @msg_text = 'Must be >= 0              ';
             *in79 = *on;
             exsr $_add_msg;
             exsr $_pass_msg;
          else;
             errors = *off;
             *in79 = *off;
          endif;
       endif;

       if errors = *off;
          if pr_minqty  < 0;
             errors = *on;
             if (errors = *on) and
                (lda_alarm = 1);

                *in98 = *on;
             else;
                *in98 = *off;
             endif;
             @msg_id   = 'K3_9999';
             @msg_text = *blanks;
             @msg_text = 'Must be >= 0              ';
             *in80 = *on;
             exsr $_add_msg;
             exsr $_pass_msg;
          else;
             errors = *off;
             *in80 = *off;
          endif;
       endif;

       if errors = *off;
          if pr_convpak < 0;
             errors = *on;
             if (errors = *on) and
                (lda_alarm = 1);

                *in98 = *on;
             else;
                *in98 = *off;
             endif;
             @msg_id    = 'K3_9999';
             @msg_text = *blanks;
             @msg_text = 'Must be >= 0              ';
             *in82 = *on;
             exsr $_add_msg;
             exsr $_pass_msg;
          else;
             errors = *off;
             *in82 = *off;
          endif;
       endif;

       if errors = *off;
          if pr_convpkp < 10.0;
             errors = *on;
             if (errors = *on) and
                (lda_alarm = 1);

                *in98 = *on;
             else;
                *in98 = *off;
             endif;
             @msg_id    = 'K3_9999';
             @msg_text = *blanks;
             @msg_text = 'Must be >= 10.0 %         ';
             *in83 = *on;
             exsr $_add_msg;
             exsr $_pass_msg;
          else;
             errors = *off;
             *in83 = *off;
          endif;
       endif;

       If errors = *off;
          if (lc_repcary < 10.0) or
             (lc_repcary > 60.0);

             *in70 = *on;
             errors = *on;
             @msg_id   = 'K3_9999';
             @msg_text = *blanks;
             @msg_text = 'Entry must be 10.0 thru 60.0   ';
             exsr $_add_msg;
             exsr $_pass_msg;
          else;
             *in70 = *off;
             errors = *off;
          endif;
       endif;

       If errors = *off;
          if (so_orcycle < 1) or
             (so_orcycle > 999);

             *in74 = *on;
             errors = *on;
             @msg_id   = 'K3_9999';
             @msg_text = *blanks;
             @msg_text = 'Entry must be 1 thru 999       ';
             exsr $_add_msg;
             exsr $_pass_msg;
          else;
             *in74 = *off;
             errors = *off;
          endif;
       endif;

       if errors = *off;
          if (pr_longtrm < .95) or
             (pr_longtrm > 1.10);
             errors = *on;
             if (errors = *on) and
                (lda_alarm = 1);

                *in98 = *on;
             else;
                *in98 = *off;
             endif;
             @msg_id    = 'K3_9999';
             @msg_text = *blanks;
             @msg_text = 'Must be between .95 & 1.10 ';
             *in84 = *on;
             exsr $_add_msg;
             exsr $_pass_msg;
          else;
             errors = *off;
             *in84 = *off;
          endif;
       endif;



       If errors = *off;
          exsr $_add_msg;
          exsr $_pass_msg;
       endif ;


       endsr;

       begsr $_loadarrays;

       for y = 1 to %elem(@g_gk);
            eval meandevary(y) = @g_gk(y);
       endfor;

       for y = 1 to %elem(@g_gk_s);
            eval strddevary(y) = @g_gk_s(y);
       endfor;
       endsr;
      /end-free


      * ////////////////////////////////////////////////////////////////////
      * ---------------------------------- Message logic for screen programs
     c/copy k3s_c051

      * -----------------------------------------Cursor position subroutine
     c/copy k3s_c080

      * -----------------------------------------get lda subroutine
     c/copy k3s_c031

      * ---------------------------------- calculate safety stock components
     c*copy k3s_c091

      * ---------------------------------- calculate usage (days into units)
     c*copy k3s_c101

      * --------------------------------- calculate suggested order quantity
     c*copy k3s_c121

      * ------------------------------------ calculate manual min/max values
     c*copy k3s_c131

      * ----------------------------------------- calculations with GK table
     c/copy k3s_c111
