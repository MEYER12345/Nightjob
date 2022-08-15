
      *****************************************************************
     h copyright('(C) Copyright 1996 - 2014 King III Solutions, Inc.  +
     h Rel 5.1  2014-01-01       Program Property of King III Solutions, Inc. +
     h All rights reserved              +
     h K3S_Replenish (R) is a Registered Trade Mark of King III Solutions Inc.')


     h DFTACTGRP(*NO) ACTGRP('K3S_ACTG_5') OPTION(*NODEBUGIO)
     h BNDDIR('K3S_BNDDIR')
      *****************************************************************
      **
      **   K3S-Replenish (R) - Inventory REPLENISHment System
      **   Copyright (C) 1996-2014 by King III Solutions, Inc.
      **   Program property of King III Solutions, Inc.
      **   All rights reserved.
      **   K3S_Replenish (R) is a Registered Trade Mark of
      **   King III Solutions Inc.
      **
      *****************************************************************
      **
      **   Name: K3S_3600
      **   Type: ILE RPG Program
      **   Desc: Select overstocked products - calculations
      **
      *****************************************************************
      **
      **   The purpose of this program is to build a batch of overstocked
      **   products based upon specific tests. The output file can then be
      **   used for multiple purposes, such as an 'Overstock Report',
      **   flagging products for a 'Selected products review' batches, and
      **   flagging products for Select check OV (overstocked).
      **
      *****************************************************************
      **
      **  This program reads products from file k_product that were
      **  assembled using OPNQRYF. This OPNQRYF set should only contain
      **  data for one company at a time. Multiple locations can be
      **  processed during the same batch. For performance, make sure
      **  sequence is in company, location, supplier, sub supplier seq.
      **
      *****************************************************************
      *   Maintenance Log.
      *   Programmer. David Meyer.
      *   Date. 07/31/2016.
      *   Remarks. Removed OPYQRYF CL statements pertaining to K3S_3600
      *            from K3S_3600CL and left the call to RPG program
      *            K3S_3600. In K3S_3600 added code to use an SQL
      *            cursor to loop thru K_PRODUCT. Replaced CHAIN
      *            operation codes for other files with SQL select
      *            statements. Also, had to change parameter list on
      *            procedure interface and prototype to include company
      *            as OPNQRYF was selecting it before. Now SQL cursor
      *            declaration will select company. In addition,
      *            declared a dynamic SQL cursor with selection
      *            criteria to filter records. Used this cursor to
      *            loop through the result set and total up hold out
      *            quantities.
      *****************************************************************
      * --------------------------------------------------------- Workfields
     d bypass          s              1a
     d #once           s              1
     d locn            s                   like(pr_locn)
     d birth_test      s                   like(pr_birth)
     d qtyhold         s                   like(pr_qtyhold)
     d max_days        s              7p 3
     d max_units       s              7p 0
     d days_suply      s             11p 0
     d days_curnt      s              5p 0
     d days_maxpt      s              5p 0
     d found           s              1
     d high_time       s              5p 0
     d low_time        s              5p 0
     d time            s              5p 0
     d over_units      s              7p 0
     d over_invst      s             15p 3
     d safesum         s                   like(pr_safesum)
     d product_ok      s              1  0
     d days            s              5p 0
     d useprdlnk       s              1                                         returned date frmtd
     d vary_orcyc      s              1  0                                      Variable Order Cycle
     D Weeks           S              7  0
     D DayNbr          S              1P 0
     d last_recvd      s              1  0                                      Variable Order Cycle
     d no_lstrcvd      s               d   inz(d'0001-01-01') datfmt(*iso)
     d buy_mult        s              1  0                                      Buy Multiple
     d min_qty         s              1  0                                      Minimum quantity
     d min_units       s              1  0                                      Manual min units
     d average_0       s              1  0                                      Average 0 allowed
     d on_order        s              1  0                                      On order excluded
     d ref_birth       s              1  0                                      Ref Birth include
     d ref_days        s              5p 0
     d ref_test        s                   like(pr_birth)
     d all_D_over      s              1  0                                      All 'D' ovrstock?
     d y               s              3  0
     d #do_once        s              1  0
     d first12         s               d
     d first13         s               d
     d first52         s               d
     d today           s               d
     d u1sttest        s             10
     d prodsez_found   s               n
     d rrn             s             10i 0
      * -------------------------------------------------------
     d tanumber1       s              5  0
     d taflag1         s              1  0
     d p1deltcnt       s              1  0
     d prodhld_count   s              5  0
      * -------------------------------------------------------
     d SQLStateOk      c                   Const('00000')
     d RowNotFound     c                   Const('02000')
      * -------------------------------------------------------
     d product_rec   e ds                  ExtName(k_product)
     d locatns_rec   e ds                  ExtName(k_locatns)
     d suplier_rec   e ds                  ExtName(k_suplier)
     d prodlnk_rec   e ds                  ExtName(k_prodlnk)
     d suplvoc_rec   e ds                  ExtName(k_suplvoc)
     d prodwkd_rec   e ds                  ExtName(k_prodwkd)
     d prodovp_rec   e ds                  ExtName(k_prodovp)
     d prodhld_rec   e ds                  ExtName(k_prodhld)
      * -------------------------------------------------------
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
     d  @u_Ds_warn     s              1  0

      * work fields used in subprocedure

      *   profile array
     d
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

      * ----------------------------------------- Supplier for Control Break
     d
     d supl            ds
     d  #prlocn                       5
     d  #prsupl                      10
     d  #prsuplsub                   10
     d suplsaved       s                   like(supl)

      * -------------------------------------- Hold out quantity work fields
     d hold_locn       s                   like(pr_cmblocn)
     d hold_supl       s                   like(pr_cmbsupl)
     d hold_sub        s                   like(pr_cmbsub)
      * -------------------------------------------------------
     d StmtString      s          32000a   varying
     d                                     inz
     d String          s             40a   inz
     d InpSrchCnd      s            300a   inz
      * ---------------------------------- calculate safety stock components
     d*copy k3s_c090

      * ---------------------------------- calculate usage (days into units)
     d*copy k3s_c100

      * ----------------------------------------- calculations with GK table
     d/copy k3s_c110

     d* --------------------------------------------------- parameter passed prototype
     d/copy k3s_proto
      * --------------------------------------------------- procedure interface
     d K3S_3600        PI
     d  comp                          1    Const                                buy group ID
     d  sgp1in                       10    Const                                buy group ID
     d  sgp2in                       10    Const                                buy group ID
     d  sgp3in                       10    Const                                buy group ID
     d  sgp4in                       10    Const                                buy group ID
     d  sgp5in                       10    Const                                buy group ID

      /free
       exec sql
        set option commit = *none,
                   datfmt = *iso,
                closqlcsr = *endactgrp;
       // ------------------------------------------------------ Read Products
       //declare dynamic cursor
       exsr dclpucursor;
       exsr dclprcursor;
       //exsr clsprcursor;
       exsr opnprcursor;

       // ---------------------------------------------------------- Main Loop
       // main loop
       dow SQLState = SQLStateOk;

       //fetch product row
          exec sql
           fetch next
            from prcursor
            into :product_rec;

          if SQLState = RowNotFound;
            leave;
          endif;

       // ------------------------------------------------------ once routine
          if #once <> '1';
             #once = '1';

             exsr $_loadarrays;
        //initialize do once flag for retrieving first day of next period
             #do_once = 0;

       // save # of days before overstock calculations apply to a product
             exec sql
               select ta_number1
                 into :tanumber1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'K3S_3600  BIRTH_TEST'
                 fetch first row only;
             if SQLState = SQLStateOk;
                days = tanumber1;
             else;
                days = 90;
             endif;
             if days < 0;
                days = 0;
             endif;

       // use 'last received date' to filter new products from report?
             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'K3S_3600  LAST_RECVD'
                 fetch first row only;
             last_recvd = 0;
             if SQLState = SQLStateOk AND taflag1 = 1;
                last_recvd = 1;
             endif;

       // use 'buy multiple' to filter very large pack size on slow movers
             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'K3S_3600  PR_BUYMULT'
                 fetch first row only;
             buy_mult = 0;
             if SQLState = SQLStateOk AND taflag1 = 1;
                buy_mult = 1;
             endif;

       // use 'minimum qty' to filter products where buyer has no choice
             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'K3S_3600  PR_MINQTY '
                 fetch first row only;
             min_qty = 0;
             if SQLState = SQLStateOk AND taflag1 = 1;
                min_qty = 1;
             endif;

       // use 'manual minimum units' to filter products
             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'K3S_3600  PR_MAMINIU'
                 fetch first row only;
             min_units = 0;
             if SQLState = SQLStateOk AND taflag1 = 1;
                min_units = 1;
             endif;

       // allow Manual Minimum Units calculation when PR_FORCAST = 0
       //      average_0 = 1 means to find excess quantity when PR_FORCAST = 0
       //      average_0 = 0 means do not find excess quanty if PR_FORCAST = 0
             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'MIN_MAX   AVERAGE=0 '
                 fetch first row only;
             average_0 = 0;
             if SQLState = SQLStateOk AND taflag1 = 1;
                average_0 = 1;
             endif;

       // Exclude quantity on order from balance calculation
       //      on_order  = 1 means exclude on order from balance calculation
       //      on_order  = 0 means do not exclude on order from balance
             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'K3S_3600  PR_QTYOORD'
                 fetch first row only;
             on_order  = 0;
             if SQLState = SQLStateOk AND taflag1 = 1;
                on_order  = 1;
             endif;

       // Reference Birth logic
       //      ref_birth = 1 means include in logic
       //      ref_birth = 0 means do not include in logic
             exec sql
               select ta_flag1, ta_number1
                 into :taflag1, :tanumber1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'K3S_3600  PR_RFBIRTH'
                 fetch first row only;
             ref_birth = 0;
             if SQLState = SQLStateOk AND taflag1 = 1;
                ref_birth = 1;
                if tanumber1 < 1000;
                    ref_days  = tanumber1;
                endif;
             endif;

       // All inventory for Discontinued as Overstock?
       // All_D_over = 1 means Yes, assume all inventory for 'D' is overstock
       // All_D_over = 0 means No, do not assume all inventory for 'D' is overstock
            exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'K3S_3600  ALL_D_OVER'
                 fetch first row only;
            all_D_over = 0;
            if SQLState = SQLStateOk AND taflag1 = 1;
               all_D_over= 1;
            endif;

       // get po processing type
             exec sql
               select ta_flag1
                 into :taflag1
                 from k_tablcod
                 where ta_comp = :pr_comp and
                       ta_codetyp = 'APP' and
                       ta_codeval = 'K_PRODLNK PROCESSING'
                 fetch first row only;
             useprdlnk = *off;
             if SQLState = SQLStateOk;
                if taflag1 = 1;
                   useprdlnk = *on;
                else;
                   useprdlnk = *off;
                endif;
             else;
                useprdlnk = *off;
             endif;

          endif;

       // ----------------------------------------------------- Location break

       //  change in location
          if pr_locn <> locn;
             locn    = pr_locn;

       //  get location
             exec sql
               select *
                 into :locatns_rec
                 from k_locatns
                 where lc_comp = :pr_comp and
                       lc_locn = :pr_locn
                 fetch first row only;

             if SQLState = SQLStateOk;

       //    calculate date for testing
                birth_test = lc_sysdate - %days(days);
                ref_test = lc_sysdate - %days(ref_days);

       // *** determine day of week for variable order cycle
                Weeks = %diff(lc_sysdate:d'1899-12-30':*days);
                DayNbr = %rem(Weeks:7);
                if DayNbr = 0;
                   DayNbr = 7;
                endif;

               if #do_once <> 1;
                  #do_once = 1;

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

               endif;
             endif;

          endif;

       // ----------------------------------------------------- Supplier break

       //  change in supplier ID
          #prlocn = pr_locn;
          #prsupl = pr_supl;
          #prsuplsub = pr_suplsub;

          if supl <> suplsaved;

       // save new supplier ID
             suplsaved = supl;

       // get supplier record

             exec sql
               select *
                 into :suplier_rec
                 from k_suplier
                 where sp_comp = :pr_comp and
                       sp_locn = :pr_locn and
                       sp_supl = :pr_supl and
                       sp_suplsub = :pr_suplsub
                 fetch first row only;

             if SQLState = SQLStateOk;

       // --------  variable order cycle section -- begin
       // check to see if supplier has variable order cycle record
                vary_orcyc = 0;
                @u_dowmap  = 0;

                exec sql
                  select *
                    into :suplvoc_rec
                    from k_suplvoc
                    where si_comp = :pr_comp and
                          si_locn = :pr_locn and
                          si_supl = :pr_supl and
                          si_suplsub = :pr_suplsub
                    fetch first row only;
                if SQLState = SQLStateOk;
                   vary_orcyc = 1;
                endif;

       // variable order cycle does exist, determine correct value
                if vary_orcyc = 1;
       //    D=Daily
                   if si_rectype = 'D';
       //          save off Weekly Distribution values
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
       //          check if there is a Product Weekly Distribution record

                      exec sql
                        select *
                          into :prodwkd_rec
                          from k_prodwkd
                          where pm_comp = :pr_comp and
                                pm_locn = :pr_locn and
                                pm_supl = :pr_supl and
                                pm_suplsub = :pr_suplsub and
                                pm_prod    = :pr_prod
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
                      endif;
                   endif;

                endif;
       // --------  variable order cycle section -- end

             endif;


          endif;

       // ------------------------------------------------- selection criteria

       //  assume all products to be taken
          product_ok = 1;

       //  only process discontinued products if approved for this location
          if pr_sysstat = 'D'  AND
             lc_ovrdisc = 0;

             product_ok = 0;
          endif;

       //  only process manual products if approved for this location
          if pr_usrstat = 'M'  AND
             lc_ovrmanl = 0;

             product_ok = 0;
          endif;

       //  are we testing 'last receive date' to filter for new products?
       //      must also ensure that we don't filter products where they
       //      were overstocked prior to going live with K3S
          if last_recvd = 1 AND
             pr_lstrcvd = no_lstrcvd AND
             pr_qtyohnd = 0;

             product_ok = 0;
          endif;

       //  Reference Birth testing area
       //      if this area is on, then product must have had inventory
       //      for a while before we call it overstock
          if ref_birth  = 1;
       //          never taken inventory yet
             if pr_rfbirth = no_lstrcvd;
                product_ok = 0;
             endif;
       //          too recent with inventory
             if pr_rfbirth <> no_lstrcvd and
                pr_rfbirth >= ref_test;

                product_ok = 0;
             endif;
          endif;

          if sgp1in <> *blanks or
             sgp2in <> *blanks or
             sgp3in <> *blanks or
             sgp4in <> *blanks or
             sgp5in <> *blanks;
             if sgp1in <> *blanks and
                sp_group1 <> sgp1in or
                sgp2in <> *blanks and
                sp_group2 <> sgp2in or
                sgp3in <> *blanks and
                sp_group3 <> sgp3in or
                sgp4in <> *blanks and
                sp_group4 <> sgp4in or
                sgp5in <> *blanks and
                sp_group5 <> sgp5in;

                product_ok = 0;
             endif;
          endif;

       //  only take products that are ok from tests above (disc and manual)
          if product_ok = 1;

       //  only process products where:
       //         birth date older than 90 days ago, and
       //         forward buy date older than today, and
       //         product not linked.

             if pr_birth   < birth_test and
                pr_fbuydat < lc_sysdate;

                if useprdlnk = *on;
                   *in55 = *on;
                   *in56 = *on;
                   *in57 = *off;
                   exec sql
                     select *
                       into :prodlnk_rec
                       from k_prodlnk
                       where pk_comp = :pr_comp and
                             pk_type = 'D' and
                             pk_frmlocn = :pr_locn and
                             pk_frmsupl = :pr_supl and
                             pk_frmsub  = :pr_suplsub and
                             pk_frmprod = :pr_prod
                       fetch first row only;

                   if SQLState = SQLStateOk;
                      *in55 = *off;
                      exec sql
                        select pr_deltcnt
                          into :p1deltcnt
                          from k_product
                          where pr_comp = :pk_comp and
                                pr_locn = :pk_to_locn and
                                pr_supl = :pk_to_supl and
                                pr_suplsub = :pk_to_sub and
                                pr_prod = :pk_to_prod
                          fetch first row only;
       //If product linked then bypass it.
                      if SQLState = SQLStateOk;
                         *in56 = *off;
                         if p1deltcnt = 0;
                            *in57 = *on;
                         else;
                            *in57 = *off;
                         endif;
                      else;
                         *in57 = *off;
                      endif;
                   endif;

                   if *in55 = *off and
                      *in56 = *off and
                      *in57 = *on;

                      bypass = 'Y';
                   else;
                      bypass = ' ';
                   endif;
                else;
                   bypass = ' ';
                endif;

                if bypass = ' ';
       // ----------------------------------------------------- Hold out begin
       // --- start develop hold out quantity
                   qtyhold = 0;

       //       generate key for testing
       //          combined supplier
                   if sp_altsrce = 2;
                      hold_locn  = pr_cmblocn;
                      hold_supl  = pr_cmbsupl;
                      hold_sub   = pr_cmbsub;

       //          regular supplier
                   else;
                      hold_locn  = pr_locn;
                      hold_supl  = pr_supl;
                      hold_sub   = pr_suplsub;
                   endif;

       //       first test, does hold out quantity exist?
                   exec sql
                     select count(*)
                       into :prodhld_count
                       from k_prodhld
                       where pu_comp = :pr_comp and
                             pu_locn = :hold_locn and
                             pu_supl = :hold_supl and
                             pu_suplsub = :hold_sub and
                             pu_prod = :pr_prod;

       //      if yes, then see if in window
                   if prodhld_count > 0;

                      exsr InzInpSrch;
    ‚   //initialize StmtString
                      exsr IntSQLStmt;
    ‚   //prepare statement
    ‚                  exsr PrepDynSQLStmt;

                      if SQLState = SQLStateOk;
    ‚   //open dynamic cursor
                         exsr opnpucursor;

                         dow SQLState = SQLStateOk;

                            exec sql
                              fetch next
                                 from pucursor
                                 into :prodhld_rec;

                            if SQLState = SQLStateOk;

       //        if in window, accumlate hold out quantity
                               if pu_begin  <=  lc_sysdate AND
                                  pu_end    >=  lc_sysdate;

                                  qtyhold += pu_hldqty;
                               endif;
                            endif;
                         enddo;
                         exsr clspucursor;
                      endif;
                   endif;

       // --- end   develop hold out quantity
       // ----------------------------------------------------- Hold out end

       //  calculate current quantity balance to be used during this session

                   pr_qtybaln = pr_qtyohnd
                              + pr_qtyoord
                              - pr_qtyback
                              - qtyhold
                              - pr_qtypend;

       // back out promotional quantity, if within dates
                   if (pr_prombeg <= lc_sysdate) AND
                      (pr_promend >= lc_sysdate);

                      pr_qtybaln -=  pr_promqty;
                   endif;

       // back out on order quantity, if exclusion is on
                   if on_order = 1;
                      pr_qtybaln -= pr_qtyoord;
                   endif;

       //  only process products where:
       //         product is seasonal, or
       //         product is not seasonal, a quantity balance exists,
       //             and quantity balance >= forecast

                   if (pr_seasonl <> *blanks)    or
                      (pr_seasonl = *blanks and
                      pr_qtybaln > *zeros  and
                      pr_qtybaln >= pr_forcast);

       //  if no forecast exists for product, then
       //     set current days inventory to maximum, and
       //     set over stocked units to balance, and
       //     skip time calculations

                      if pr_forcast = 0;
                         days_curnt = 999;
                         over_units = pr_qtybaln;

       //  if we allow determination of excess when no forcast exists,
       //     and buyer using manual minimum in units, then calculate it
                         if average_0 = 1 and pr_maminiu > 0;
                            over_units = pr_qtybaln - pr_maminiu;
                            if over_units < 0;
                               over_units = 0;
                            endif;
                         endif;

                         exsr skip_calc;
                      else;
       // If forecast exists for product, and
       // it is a 'D' Discontinued product, and
       // customer wants to say everything is overstock,
       // then set days to maximum, and treat all as overstock
                         if pr_forcast > 0 and
                            pr_sysstat = 'D' and
                            all_D_over = 1;
                               days_curnt = 999;
                               over_units = pr_qtybaln;
                               exsr skip_calc;
                         else;
       //  initialize work fields
                            clear max_days;
                            clear max_units;
                            clear days_suply;
                            clear days_curnt;
                            clear days_maxpt;
                            clear found;
                            clear high_time;
                            clear low_time;
                            clear time;
                            clear over_units;
                            clear over_invst;

       //   if product lead time less than supplier lead time, use supplier lt
                            if pr_leadtm < sp_leadtmo AND lc_ltmuse = 0;
                               pr_leadtm = sp_leadtmo;
                            endif;

       // calculate safety stock sum of elements
                            safesum = lc_repcary +
                                      pr_linecst +
                                      sp_orcycle +
                                      pr_forcint +
                                      pr_minqty  +
                                      pr_buymult +
                                      pr_costeac +
                                      pr_forcast +
                                      pr_fordevp +
                                      pr_service +
                                      pr_leadtm  +
                                      pr_leadtmv +
                                      pr_formeth;

       // if sum of elements has changed, then calculate safety stock
                            if pr_safesum <> safesum;

       //  get safety stock and order time factor

                               clear @s_sstimef;
                               clear @s_otimfac;
                               clear @s_devtime;
                               clear @s_intrval;

                               @s_repcary = lc_repcary * .01;
                               @s_linecst = pr_linecst;
                               @s_orcycle = sp_orcycle;
                               @s_forcint = pr_forcint;
                               @s_buymult = pr_buymult;
                               @s_minqty  = pr_minqty;
                               @s_costeac = pr_costeac;
                               @s_forcast = pr_forcast;
                               @s_fordevp = pr_fordevp * .01;
                               @s_service = pr_service * .01;
                               @s_leadtm  = pr_leadtm;
                               @s_leadtmv = pr_leadtmv * .01;
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
       //                      exsr $_safety;

                            else;

       // no change to safety stock components, so use values from k_product
                               @s_sstimef = pr_sstimef;
                               @s_otimfac = pr_otimfac;

                            endif;

       // calculate maximum days:
       //    2 times safety stock, plus lead time, plus order time factor
                            max_days   = (2 * @s_sstimef) +
                                         pr_leadtm + @s_otimfac;
                            eval(h) days_maxpt = max_days;

       // calculate maximum units:

       //     get profile factors, if product contains seasonal profile ID

                            prodsez_found = *off;
                            if pr_seasonl <> *blanks;
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
                                   into :pzfactr01, :pzfactr02,: pzfactr03,
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
                                   where pz_comp = :pr_comp and
                                         pz_seasonl = :pr_seasonl
                                   fetch first row only;
                                   if SQLState = SQLStateOk;
                                      prodsez_found = *on;
                                   endif;
                            endif;

       //     set factors to 1.00 if:

       //          1) profile not found for this product, or
       //          2) no profile for this product, and long term trend <> 1.00
       //          3) no profile for this product, and Weekly Distribution ON

                            if ((pr_seasonl <> *blanks) and
                               not prodsez_found)
                                    or
                               ((pr_seasonl = *blanks) and
                                (pr_longtrm <> 1.00))
                                    or
                               ((pr_seasonl = *blanks) and
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

                            @u_comp    = pr_comp;
                            @u_seasonl = pr_seasonl;
                            @u_forcast = pr_forcast;
                            @u_forcper = pr_forcper;
                            @u_forcint = pr_forcint;
                            @u_longtrm = pr_longtrm;

                            clear @u_use;
                            @u_days = max_days;

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
       //                   exsr $_usage;

       // allow for large buy multiple
                           if buy_mult = 1 and
                              pr_buymult > @u_use;

                              @u_use = pr_buymult;
                           endif;

       //  see if minimum quantity can be a problem
                           if min_qty = 1 and
                              pr_minqty > @u_use;

                              @u_use = pr_minqty;
                           endif;

       //  see if manual minimum units is to be considered
                           if min_units = 1 and
                              pr_maminiu > @u_use;

                              @u_use = pr_maminiu;
                           endif;

                           eval(h) max_units = @u_use;

       //  quantity balance test
                           if pr_qtybaln >= @u_use + 1;

       //  check zero balance
                              if pr_qtybaln = 0;
                                 time = 0;
                                 exsr over_calc;
                                 exsr skip_calc;
                              else;
       //  calculate number of days supply by:
       //        balance / (annual usage / 364)
                                 eval(h) days_suply = pr_qtybaln /
                                           ((pr_forcint * pr_forcast) /
                                           364);
       //  less than a half days supply, based upon above calculation
                                 if days_suply = 0;
                                    time = 0;
                                    exsr over_calc;
                                    exsr skip_calc;
                                 else;

       //  more than 3 years worth
                                    if days_suply > 999;
                                       time = 999;
                                       exsr over_calc;
                                       exsr skip_calc;
                                    else;

       //  if product is not seasonal, then no reason to iterate to find
       //     days supply. simple routine above is sufficient.
                                       if pr_seasonl = *blanks;
                                          time = days_suply;
                                       else;

       //  however, if product is seasonal, then you must iterate to find
       //     out how many days supply the balance quantity is.

       //     find high point
                                         clear @u_use;
                                         high_time = days_suply + 1;

                                         dow pr_qtybaln > @u_use AND
                                             high_time <> 99999;

                                             clear @u_use;
                                             @u_days = high_time;

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
       //                                    exsr $_usage;

                                             if pr_qtybaln > @u_use;

                                                if (high_time * 2) > 99999;
                                                   high_time = 99999;
                                                else;
                                                   high_time = high_time * 2;
                                                endif;

                                             endif;

                                         enddo;

       //  tons of it, could be slow mover with profile containing lots of
       //  0.00's
                                         if high_time = 99999;
                                            time = 999;
                                            max_units = 0;
                                            exsr over_calc;
                                            exsr skip_calc;
                                         else;

       //     set low point
                                            low_time  = max_days;
                                            clear time;

       //     iterate to find days
                                            found = *off;

                                            dou found = *on;

                                               eval(h) time  = (high_time +
                                                               low_time) * .5;

                                               found = (time = high_time) or
                                                       (time = low_time);

                                               if found = *off;
                                                  clear @u_use ;
                                                  @u_days = time;

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
       //                                         exsr $_usage;

                                                  if pr_qtybaln > @u_use;
                                                     low_time  = time;
                                                  else;
                                                     high_time = time;
                                                  endif;

                                               endif;

                                            enddo;
       //high_time = 99999
                                         endif;

       //pr_seasonl = *blanks
                                       endif;
                                       exsr over_calc;
                                       exsr skip_calc;
       //days_suply > 999
                                    endif;
       //days_suply = 0
                                 endif;

       //quantity balance = 0
                              endif;
       // quantity balance test
                           endif;
       // pr_forcast > 0 and pr_sysstat = 'D' and all_D_over = 1
                         endif;
       // pr_forcast = 0
                      endif;
       // profile, quantity balance, forecast test
                   endif;

       // bypass test
                endif;

       // date test
             endif;

       // product ok
          endif;

          SQLState = SQLStateOk;
       enddo;
       exsr clsprcursor;

       // finished, set on LR
       *inlr = *on;

       begsr over_calc;

       days_curnt = time;

       over_units = pr_qtybaln - max_units;

       endsr;

       begsr skip_calc;

       eval(h) over_invst = over_units * pr_costeac *
                            (lc_ovrcary * .01);


       //   does product reach thresh hold for days and dollars?
       //        if so, then it is overstocked, so take it

       if (((days_curnt - days_maxpt) > lc_ovrdays)
                  and
          ((over_invst / 364) *
           (days_curnt - days_maxpt)) > lc_ovrdolr)
                  OR
       //          if we allow determination of excess when no forcast exists,
       //          and buyer using manual minimum in units, then calculate it
           (average_0 = 1 and pr_maminiu > 0
                          and over_units > 0);
           pv_comp    = pr_comp;
           pv_buyr    = sp_buyr;
           pv_locn    = pr_locn;
           pv_supl    = pr_supl;
           pv_suplsub = pr_suplsub;
           pv_name    = sp_name;
           pv_prod    = pr_prod;
           pv_prodseq = pr_prodseq;
           pv_desc1   = pr_desc1;
           pv_birth   = lc_sysdate ;
           pv_usrstat = pr_usrstat;
           pv_sysstat = pr_sysstat;
           pv_qtyohnd = pr_qtyohnd;
           pv_qtyoord = pr_qtyoord;
           pv_qtybaln = pr_qtybaln;
           pv_costeac = pr_costeac;
           pv_ndc_upc = pr_ndc_upc;
           if (over_invst / 364) *
              (days_curnt - days_maxpt) <= 9999999;

              eval(h) pv_exccost = (over_invst / 364) *
                                   (days_curnt - days_maxpt);
           else;
              pv_exccost = 9999999;
           endif;
           pv_ovrunit = over_units;
           pv_dayscur = days_curnt;
           pv_daysmax = days_maxpt;

           exsr insert_prodovp;

       endif;

       endsr;

       begsr $_loadarrays;

       for y = 1 to %elem(@g_gk);
            eval meandevary(y) = @g_gk(y);
       endfor;

       for y = 1 to %elem(@g_gk_s);
            eval strddevary(y) = @g_gk_s(y);
       endfor;
       endsr;

       begsr insert_prodovp;

       exec sql
         insert into k_prodovp
           values (:prodovp_rec);

       endsr;

       begsr dclprcursor;

       exec sql
        declare prcursor Cursor
         for
         select *
         from k_product a
         where pr_comp = :comp and
               pr_altsrce = 0 and   --regular source products
               pr_deltcnt = 0       --products with no delete count started
         order by pr_locn,
                  pr_supl,
                  pr_suplsub,
                  rrn(a);           --relative record number

       endsr;

       begsr opnprcursor;
       exec sql
        open prcursor;
        if SQLState <> SQLStateOk;
           exsr clsprcursor;
           exec sql
            open prcursor;
        endif;
       endsr;

       begsr clsprcursor;
       exec sql
        close prcursor;
       endsr;

       Begsr IntSQLStmt;
       String = *blanks;
       String =   'Select * +
                   From K_Prodhld +
                   Where ';

       StmtString = *blanks;
       StmtString = %trim(String) + ' ' +
                    %trim(InpSrchCnd);
       endsr;

       Begsr InzInpSrch;
       InpSrchCnd = *blanks;
       InpSrchCnd = 'pu_comp = ? and +
                     pu_locn = ? and +
                     pu_supl = ? and +
                     pu_suplsub = ? and +
                     pu_prod = ? and +
                     pu_end >= ? +
                     Order by pu_comp, +
                              pu_locn, +
                              pu_supl, +
                              pu_suplsub, +
                              pu_prod, +
                              pu_end';
       endsr;

       begsr dclpucursor;
       exec sql
        declare pucursor Cursor
         for DynSQLStmt;
       endsr;

       begsr PrepDynSQLStmt;
       exec sql
        Prepare DynSqlStmt
          From :StmtString;
       endsr;

       begsr opnpucursor;
       exec sql
        open pucursor
          using :pr_comp,
                :hold_locn,
                :hold_supl,
                :hold_sub,
                :pr_prod,
                :lc_sysdate;
       endsr;

       begsr clspucursor;
       exec sql
        close pucursor;
       endsr;

      /end-free

      * ---------------------------------- calculate safety stock components
     c*copy k3s_c091

      * ---------------------------------- calculate usage (days into units)
     c*copy k3s_c101

      * ----------------------------------------- calculations with GK table
     c/copy k3s_c111
